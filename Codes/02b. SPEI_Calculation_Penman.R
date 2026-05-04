# =============================================================================
# 02b. SPEI Calculation — Penman-Monteith PET Method
# =============================================================================
# Analysis Plan Reference: Section 1.2 — Calculating SPEI using ERA5 data
#                          PET Method (b): Penman-Monteith FAO-56
#
# This script implements Steps 2–7 of Section 1.2 using pre-calculated PET:
#   Step 2: Water balance (WB = Precip - PET) using Penman-Monteith PET
#   Step 3: Decide accumulation periods (1, 3, 6, 12 months)
#   Step 4: Aggregate water balance over accumulation window
#   Step 5: Fit log-logistic distribution per pixel per month
#   Step 6: Calculate cumulative probability
#   Step 7: Transform to standard normal (SPEI z-score)
#
# Unlike Script 02a (Thornthwaite), PET is NOT calculated here.
# PET comes from ERA5-Land resampled files prepared in Scripts 03–05.
# ERA5-Land PET uses Penman-Monteith FAO-56 — more data-intensive but
# physically rigorous, particularly in energy-limited climates.
#
# Standalone script — does not depend on Scripts 01 or 02a.
#
# Prerequisites:
#   - Script 05 completed: clipped PET NC files in PET_DIR
#   - Databricks cluster accessible (for ERA5 precipitation data)
#
# Output: spei_penman_results.rds — one row per pixel per month,
#         columns: x, y, year, month, spei1, spei3, spei6, spei12
# =============================================================================


# -- Libraries ----------------------------------------------------------------
library(sparklyr)
library(dplyr)
library(dbplyr)
library(terra)
library(SPEI)


# =============================================================================
# USER SETTINGS — edit this section only
# =============================================================================

# Clipped PET files from Script 05 — must be resampled to ERA5 0.25 deg grid
pet_dir <- "/Volumes/projects/gwsc_drought/drought_outputs/pet_clipped/"

# Databricks catalog — must match Scripts 01/02a
project_folder <- "xx_drought_index"
catalog_name   <- "projects"

# Year range for PET files (must match YEARS used in Script 03)
pet_year_start <- 1970
pet_year_end   <- 2023

# =============================================================================


# -- Databricks connection ----------------------------------------------------
sc <- spark_connect(method = "databricks")


# -- Step 2a: Load clipped Penman-Monteith PET from NC files ------------------
# PET loaded first — pixel coords used to filter ERA5 in Spark (avoids OOM).
# PET prepared in Scripts 03–05: downloaded from ERA5-Land, converted to
# mm/month, resampled from 0.10 deg → 0.25 deg, clipped to ERA5 extent.
# All NC files in pet_dir are loaded and combined into one long-format table.
pet_files <- list.files(pet_dir, pattern = "1970_2023.*\\.nc$", full.names = TRUE)

if (length(pet_files) == 0) {
  stop(sprintf("No PET NC files found in: %s\nRun Scripts 03-05 first.", pet_dir))
}

cat(sprintf("Loading %d PET file(s)...\n", length(pet_files)))

load_pet_file <- function(nc_path, year_start, year_end) {
  r      <- rast(nc_path)
  # Assign clean date-based layer names
  dates  <- seq(as.Date(paste0(year_start, "-01-01")),
                by = "month", length.out = nlyr(r))
  names(r) <- format(dates, "pet_%Y_%m")

  df_wide <- as.data.frame(r, xy = TRUE, na.rm = TRUE)

  # Pivot wide → long (base R)
  pet_cols <- grep("^pet_", names(df_wide), value = TRUE)
  df_long  <- reshape(
    df_wide,
    varying   = pet_cols,
    v.names   = "pet_mm",
    timevar   = "layer_name",
    times     = pet_cols,
    direction = "long"
  )
  df_long$year       <- as.integer(substr(df_long$layer_name, 5, 8))
  df_long$month      <- as.integer(substr(df_long$layer_name, 10, 11))
  df_long$pet_mm     <- round(df_long$pet_mm, 2)
  df_long$id         <- NULL
  df_long$layer_name <- NULL

  df_long[order(df_long$x, df_long$y, df_long$year, df_long$month),
          c("x", "y", "year", "month", "pet_mm")]
}

pet_list <- lapply(pet_files, load_pet_file,
                   year_start = pet_year_start,
                   year_end   = pet_year_end)
pet_r    <- do.call(rbind, pet_list)

message(sprintf("PET loaded: %s rows, %d pixels",
                format(nrow(pet_r), big.mark = ","),
                n_distinct(paste(pet_r$x, pet_r$y))))


# -- Step 1: Load ERA5 monthly precipitation from Databricks ------------------
# Analysis Plan Section 1.2 Step 1: ERA5 daily table aggregated to monthly.
# Filter in Spark to only pixels present in PET files before collect() —
# avoids pulling all ~10993 global pixels into R (OOM fix).
pet_r$x <- round(pet_r$x, 4)
pet_r$y <- round(pet_r$y, 4)

pet_pixels_sdf <- copy_to(
  sc,
  unique(pet_r[, c("x", "y")]),
  name      = "pet_pixels",
  overwrite = TRUE
)

era5_normals <- tbl(sc, in_catalog(catalog_name, project_folder,
                                   "drought_index_dailysummary"))

COUNTRY_FILTER <- c("Argentina", "Benin", "Thailand", "Greece", "Mexico", "South Africa")

monthly_spark <- era5_normals |>
  filter(country %in% COUNTRY_FILTER) |>
  group_by(x, y, country, year, month) |>
  summarise(
    monthly_precip = sum(precip, na.rm = TRUE),
    n_days_missing = sum(as.integer(is.na(precip))),
    .groups        = "drop"
  ) |>
  filter(n_days_missing < 5) |>
  inner_join(pet_pixels_sdf, by = c("x", "y")) |>
  arrange(x, y, year, month)

monthly_r <- monthly_spark |> collect()

message(sprintf("ERA5 precip loaded: %s rows, %d pixels",
                format(nrow(monthly_r), big.mark = ","),
                n_distinct(paste(monthly_r$x, monthly_r$y))))


# -- Step 2b: Join PET to ERA5 precipitation on x, y, year, month ------------
# Analysis Plan Section 1.2 Step 2: water balance = precip - PET.
# Coordinates already rounded to 4dp in Step 1 — join directly.
monthly_r$x <- round(monthly_r$x, 4)
monthly_r$y <- round(monthly_r$y, 4)

monthly_pet <- merge(monthly_r, pet_r, by = c("x", "y", "year", "month"))

pct_matched <- round(100 * nrow(monthly_pet) / nrow(monthly_r), 1)
message(sprintf("Join result: %s / %s rows matched (%.1f%%)",
                format(nrow(monthly_pet), big.mark = ","),
                format(nrow(monthly_r),   big.mark = ","),
                pct_matched))

if (pct_matched < 90) {
  warning("Less than 90% of ERA5 rows matched PET. Check coordinate alignment.")
}

# Water balance: negative = moisture deficit (drought conditions)
monthly_pet$water_balance <- monthly_pet$monthly_precip - monthly_pet$pet_mm


# -- Steps 5–7: SPEI calculator for a single pixel ---------------------------
# Analysis Plan Section 1.2 Steps 5–7:
#   Step 5: Fit log-logistic distribution (handles negative WB; Gamma cannot)
#   Step 6: Cumulative probability from log-logistic
#   Step 7: Transform to standard normal z-score (SPEI)
#
# SPEI::spei() handles all three steps internally.
# Same minimum data guard as Script 02a: max(scale*4, 24) months.
safe_spei_penman <- function(wb_vec, scale = 3) {
  tryCatch({
    min_obs <- max(scale * 4, 24)
    if (sum(!is.na(wb_vec)) < min_obs) {
      return(rep(NA_real_, length(wb_vec)))
    }
    result <- as.numeric(spei(wb_vec, scale = scale, na.rm = TRUE)$fitted)
    result[is.infinite(result)] <- NA_real_
    return(result)
  }, error = function(e) {
    rep(NA_real_, length(wb_vec))
  })
}


# -- Steps 3 + 4: Apply SPEI at all four accumulation periods ----------------
# Analysis Plan Section 1.2 Steps 3–4: four accumulation periods (1, 3, 6, 12 months).
#
# SPEI-1  → single-month WB anomaly; flash drought onset
# SPEI-3  → 3-month rolling WB; seasonal drought
# SPEI-6  → 6-month rolling WB; medium-term drought
# SPEI-12 → 12-month rolling WB; long-term/hydrological drought
#
# Data MUST be chronologically sorted before grouping.
spei_penman_results <- monthly_pet |>
  arrange(x, y, year, month) |>
  group_by(x, y) |>
  mutate(
    spei1  = safe_spei_penman(water_balance, scale = 1),
    spei3  = safe_spei_penman(water_balance, scale = 3),
    spei6  = safe_spei_penman(water_balance, scale = 6),
    spei12 = safe_spei_penman(water_balance, scale = 12)
  ) |>
  ungroup()


# -- Sanity check -------------------------------------------------------------
# All inf counts should be 0. All mean values should be ~0 (standardised).
spei_penman_results |>
  summarise(
    total        = n(),
    inf_spei1    = sum(is.infinite(spei1),  na.rm = TRUE),
    inf_spei3    = sum(is.infinite(spei3),  na.rm = TRUE),
    inf_spei6    = sum(is.infinite(spei6),  na.rm = TRUE),
    inf_spei12   = sum(is.infinite(spei12), na.rm = TRUE),
    na_spei1     = sum(is.na(spei1)),
    na_spei3     = sum(is.na(spei3)),
    na_spei6     = sum(is.na(spei6)),
    na_spei12    = sum(is.na(spei12)),
    pct_na_spei1 = round(mean(is.na(spei1))  * 100, 1),
    pct_na_spei3 = round(mean(is.na(spei3))  * 100, 1),
    mean_spei1   = round(mean(spei1,  na.rm = TRUE), 3),
    mean_spei3   = round(mean(spei3,  na.rm = TRUE), 3),
    mean_spei6   = round(mean(spei6,  na.rm = TRUE), 3),
    mean_spei12  = round(mean(spei12, na.rm = TRUE), 3)
  )

message("SPEI (Penman-Monteith) calculation complete!")


# -- Save output --------------------------------------------------------------
# spei_penman_results.rds feeds into drought detection alongside
# spi_results.rds and spei_results.rds (Thornthwaite).
VOLUME_PATH <- "/Volumes/projects/gwsc_drought/drought_outputs"
saveRDS(spei_penman_results, file.path(VOLUME_PATH, "spei_penman_results.rds"))
