# =============================================================================
# 01_APP_Index_Calculation.R
# GWSC Drought Monitor — ERA5 Index Calculation
# =============================================================================
# Pulls ERA5 daily data from Databricks for APP_COUNTRY, aggregates to monthly,
# and computes the selected drought index (SPI, SPEI-Thornthwaite, or
# SPEI-Penman-Monteith) at the selected accumulation scale.
#
# Called by: 00_APP_MASTER.Rmd (parameters already set in environment)
#
# Requires in environment (set by MASTER):
#   APP_COUNTRY   — character, country name
#   APP_INDEX     — "SPI" | "SPEI-T" | "SPEI-P"
#   APP_SCALE     — integer, accumulation period (3, 6, or 12)
#   REF_START     — integer, first year of reference period
#   OBS_END       — integer, last year of observation period
#   .index_col    — internal column name (e.g. "spi6", "spei3", "spei6_pm")
#   USE_SPARK_APPLY — logical, TRUE = distributed spark_apply (experimental),
#                     FALSE = collect to driver then R loop (default, proven)
#
# Produces in environment:
#   index_monthly — data frame, one row per pixel × month
#                   columns: x, y, country, year, month, <.index_col>
# =============================================================================

library(sparklyr)
library(dplyr)
library(dbplyr)
library(SPEI)

# Switch: set TRUE to test spark_apply distributed path (data eng team suggestion)
# Set FALSE (default) for proven collect-then-R-loop path
USE_SPARK_APPLY <- FALSE


# -- Databricks connection ----------------------------------------------------
sc <- spark_connect(method = "databricks")


# -- ERA5 data source (global silver — monthly pre-aggregated, 0.25 deg, 1950-2024) --
# Column mapping vs Phase 1 table:
#   longitude -> x  |  latitude -> y  |  sum_tp_mm -> monthly_precip
#   mean_t2m_c -> mean_temp_monthly   |  year/month extracted from time column
era5 <- tbl(sc, in_catalog("era5-daily-data", "silver_dev",
  "aer_era5_silver_approximate_1950_to_present_dev_interpolation_res5"))


# -- SPEI-Penman: check PET files exist before pulling data -------------------
if (APP_INDEX == "SPEI-P") {
  library(terra)
  pet_dir <- "/Volumes/projects/gwsc_drought/drought_outputs/pet_clipped/"
  pet_files <- list.files(pet_dir, pattern = "\\.nc$", full.names = TRUE)
  if (length(pet_files) == 0) {
    stop(sprintf(
      "SPEI-P selected but no PET NC files found in:\n  %s\nRun ERA5-Land PET preprocessing first (Scripts 03-05 of Phase 1).",
      pet_dir
    ))
  }
  cat(sprintf("Found %d PET file(s) in %s\n", length(pet_files), pet_dir))
}


# -- Pull monthly data from global silver -------------------------------------
# Global silver is already monthly-aggregated — no daily rollup needed.
# Rename columns to match downstream scripts (x, y, monthly_precip, mean_temp_monthly).
monthly_spark <- era5 %>%
  filter(
    country %in% !!APP_COUNTRY,
    year(time) >= !!REF_START,
    year(time) <= !!OBS_END
  ) %>%
  select(
    x              = longitude,
    y              = latitude,
    country,
    time,
    monthly_precip    = sum_tp_mm,
    mean_temp_monthly = mean_t2m_c
  ) %>%
  mutate(
    year  = year(time),
    month = month(time)
  ) %>%
  select(-time) %>%
  arrange(x, y, year, month)

cat(sprintf("Monthly data pulled for %s (%d-%d).\n",
            APP_COUNTRY, REF_START, OBS_END))


# =============================================================================
# PATH A — spark_apply: distributed index fitting (experimental)
# Each Spark executor receives one pixel group and runs SPEI in R.
# Requires SPEI package installed on every worker node.
# Advantage: no collect() bottleneck — parallelises across all worker cores.
# Risk: R executor spawn overhead; SPEI must be on workers; flaky on some
#       Databricks runtimes. Test with small run before relying on this.
# =============================================================================

if (USE_SPARK_APPLY) {

  cat("Using spark_apply (distributed) path...\n")

  # Capture APP_SCALE into function scope — spark_apply closures need explicit capture
  .scale <- APP_SCALE
  .index <- APP_INDEX

  index_sdf <- spark_apply(
    monthly_spark,
    function(df) {
      library(SPEI)

      df <- df[order(df$year, df$month), ]

      safe_spi <- function(v, scale) {
        tryCatch({
          if (sum(!is.na(v)) < max(scale * 4, 24)) return(rep(NA_real_, length(v)))
          as.numeric(SPEI::spi(v, scale = scale, na.rm = TRUE)$fitted)
        }, error = function(e) rep(NA_real_, length(v)))
      }

      safe_spei_t <- function(precip, temp, scale, lat) {
        tryCatch({
          if (sum(!is.na(precip) & !is.na(temp)) < max(scale * 4, 24))
            return(rep(NA_real_, length(precip)))
          if (mean(temp, na.rm = TRUE) > 100) temp <- temp - 273.15
          pet <- SPEI::thornthwaite(temp, lat = lat)
          wb  <- precip - pet
          as.numeric(SPEI::spei(wb, scale = scale, na.rm = TRUE)$fitted)
        }, error = function(e) rep(NA_real_, length(precip)))
      }

      if (.index == "SPI") {
        df$index_value <- safe_spi(df$monthly_precip, .scale)
      } else if (.index == "SPEI-T") {
        df$index_value <- safe_spei_t(df$monthly_precip, df$mean_temp_monthly,
                                       .scale, lat = df$y[1])
      }
      # SPEI-P not supported in spark_apply path (PET NC files not on workers)

      df[, c("x", "y", "country", "year", "month", "index_value")]
    },
    group_by = c("x", "y", "country"),
    packages = "SPEI",
    context  = list(.scale = .scale, .index = .index)
  )

  cat("Collecting spark_apply results...\n")
  index_monthly <- index_sdf %>%
    collect() %>%
    rename(!!.index_col := index_value)

  spark_disconnect(sc)
  cat(sprintf("spark_apply complete. %s rows.\n",
              format(nrow(index_monthly), big.mark = ",")))


# =============================================================================
# PATH B — collect then R loop: proven, single-driver (default)
# =============================================================================

} else {

  cat("Using collect-then-R path (default)...\n")
  monthly_r <- monthly_spark %>% collect()
  spark_disconnect(sc)
  cat(sprintf("  %s rows, %d unique pixels collected.\n",
              format(nrow(monthly_r), big.mark = ","),
              length(unique(paste(monthly_r$x, monthly_r$y))))
  )
  # Ensure year/month are integer (Spark may return numeric)
  monthly_r$year  <- as.integer(monthly_r$year)
  monthly_r$month <- as.integer(monthly_r$month)


  # -- Helper functions -------------------------------------------------------
  safe_spi <- function(precip_vec, scale) {
    tryCatch({
      min_obs <- max(scale * 4, 24)
      if (sum(!is.na(precip_vec)) < min_obs) return(rep(NA_real_, length(precip_vec)))
      as.numeric(spi(precip_vec, scale = scale, na.rm = TRUE)$fitted)
    }, error = function(e) rep(NA_real_, length(precip_vec)))
  }

  safe_spei_t <- function(precip_vec, temp_vec, scale, lat) {
    tryCatch({
      min_obs <- max(scale * 4, 24)
      valid   <- !is.na(precip_vec) & !is.na(temp_vec)
      if (sum(valid) < min_obs) return(rep(NA_real_, length(precip_vec)))
      if (mean(temp_vec[valid], na.rm = TRUE) > 100) temp_vec <- temp_vec - 273.15
      pet_vec <- thornthwaite(temp_vec, lat = lat)
      wb      <- precip_vec - pet_vec
      as.numeric(spei(wb, scale = scale, na.rm = TRUE)$fitted)
    }, error = function(e) rep(NA_real_, length(precip_vec)))
  }

  load_pet_monthly <- function(pet_dir, year_start, year_end, pixels_df) {
    pet_files <- list.files(pet_dir, pattern = "\\.nc$", full.names = TRUE)
    pet_list <- lapply(pet_files, function(f) {
      r     <- rast(f)
      dates <- seq(as.Date(paste0(year_start, "-01-01")),
                   by = "month", length.out = nlyr(r))
      names(r) <- format(dates, "pet_%Y_%m")
      pts    <- vect(pixels_df, geom = c("x", "y"), crs = "EPSG:4326")
      vals   <- terra::extract(r, pts, ID = FALSE)
      result <- cbind(pixels_df[, c("x", "y")], vals)
      tidyr::pivot_longer(result, -c(x, y),
                          names_to = "pet_key", values_to = "pet_mm") %>%
        dplyr::mutate(
          year  = as.integer(sub("pet_(\\d{4})_.*",       "\\1", pet_key)),
          month = as.integer(sub("pet_\\d{4}_(\\d{2})", "\\1", pet_key))
        ) %>%
        dplyr::select(x, y, year, month, pet_mm)
    })
    do.call(rbind, pet_list)
  }

  safe_spei_p <- function(precip_vec, pet_vec, scale) {
    tryCatch({
      min_obs <- max(scale * 4, 24)
      valid   <- !is.na(precip_vec) & !is.na(pet_vec)
      if (sum(valid) < min_obs) return(rep(NA_real_, length(precip_vec)))
      wb <- precip_vec - pet_vec
      as.numeric(spei(wb, scale = scale, na.rm = TRUE)$fitted)
    }, error = function(e) rep(NA_real_, length(precip_vec)))
  }


  # -- Apply index per pixel --------------------------------------------------
  cat(sprintf("Computing %s-%d for all pixels...\n", APP_INDEX, APP_SCALE))

  if (APP_INDEX == "SPI") {

    index_monthly <- monthly_r %>%
      arrange(x, y, year, month) %>%
      group_by(x, y, country) %>%
      mutate(!!.index_col := safe_spi(monthly_precip, scale = APP_SCALE)) %>%
      ungroup() %>%
      select(x, y, country, year, month, all_of(.index_col))

  } else if (APP_INDEX == "SPEI-T") {

    index_monthly <- monthly_r %>%
      arrange(x, y, year, month) %>%
      group_by(x, y, country) %>%
      mutate(!!.index_col := safe_spei_t(monthly_precip, mean_temp_monthly,
                                          scale = APP_SCALE, lat = first(y))) %>%
      ungroup() %>%
      select(x, y, country, year, month, all_of(.index_col))

  } else if (APP_INDEX == "SPEI-P") {

    pixels_df <- unique(monthly_r[, c("x", "y")])
    pet_data  <- load_pet_monthly(pet_dir, REF_START, OBS_END, pixels_df)
    monthly_r <- monthly_r %>%
      left_join(pet_data, by = c("x", "y", "year", "month"))

    col_name <- paste0("spei", APP_SCALE, "_pm")
    index_monthly <- monthly_r %>%
      arrange(x, y, year, month) %>%
      group_by(x, y, country) %>%
      mutate(!!col_name := safe_spei_p(monthly_precip, pet_mm, scale = APP_SCALE)) %>%
      ungroup() %>%
      select(x, y, country, year, month, all_of(col_name))

    .index_col <- col_name
  }

  cat(sprintf("Index calculation complete. %s rows.\n",
              format(nrow(index_monthly), big.mark = ",")))
}
