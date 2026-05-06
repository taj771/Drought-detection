# =============================================================================
# 02. SPEI Calculation — Thornthwaite PET Method
# =============================================================================
# Analysis Plan Reference: Section 1.2 — Calculating SPEI using ERA5 data
#                          PET Method (a): Thornthwaite
#
# This script implements Steps 1–7 of Section 1.2:
#   Step 1: Monthly precipitation + temperature from ERA5 via Databricks
#   Step 2: Calculate water balance (WB = Precip - PET)
#   Step 3: Decide accumulation periods (1, 3, 6, 12 months)
#   Step 4: Aggregate water balance over accumulation window
#   Step 5: Fit log-logistic distribution per pixel per month
#   Step 6: Calculate cumulative probability
#   Step 7: Transform to standard normal (SPEI z-score)
#
# Standalone script — does not depend on Script 01.
# Both scripts pull ERA5 independently and save separate outputs.
#
# Output: spei_results.rds — one row per pixel per month with SPEI-1/3/6/12
# =============================================================================


# -- Libraries ----------------------------------------------------------------
library(sparklyr)
library(dplyr)
library(dbplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(SPEI)


# -- Databricks connection ----------------------------------------------------
sc <- spark_connect(method = "databricks")


# -- Data source --------------------------------------------------------------
# Analysis Plan Section 1.2 Step 1: same ERA5 daily table as Script 01.
# Requires both precip and mean_temp (used for Thornthwaite PET).
project_folder <- "xx_drought_index"

era5_normals <- tbl(sc, in_catalog("projects", project_folder, "drought_index_dailysummary"))


# -- Data check (run once to verify) ------------------------------------------
era5_normals %>%
  summarise(
    total_rows  = n(),
    n_pixels    = n_distinct(paste(x, y)),
    n_years     = n_distinct(year),
    date_start  = min(time),
    date_end    = max(time)
  ) %>%
  collect()

era5_normals %>%
  head(100) %>%
  collect()


# -- Daily → monthly aggregation (runs in Spark) ------------------------------
# Analysis Plan Section 1.2 Step 1: aggregate daily precip and temperature to monthly.
# mean_temp_monthly is required for Thornthwaite PET calculation (Step 2).
# NOTE: Verify ERA5 mean_temp units. ERA5 raw = Kelvin; convert to °C if needed
#       (thornthwaite() requires °C). Check with: range(monthly_r$mean_temp_monthly)
#       If values ~273–310, subtract 273.15 in the safe_spei() function below.
COUNTRY_FILTER <- c("Argentina", "Benin", "Thailand", "Greece", "Mexico", "South Africa")

monthly_spark <- era5_normals %>%
  filter(country %in% COUNTRY_FILTER) %>%
  group_by(x, y, country, year, month) %>%
  summarise(
    monthly_precip    = sum(precip, na.rm = TRUE),
    mean_temp_monthly = mean(mean_temp, na.rm = TRUE),
    n_days_observed   = n(),
    n_days_missing    = sum(as.integer(is.na(precip))),
    .groups = "drop"
  ) %>%
  filter(n_days_missing < 5) %>%
  arrange(x, y, year, month)

monthly_spark %>%
  summarise(
    n_rows   = n(),
    n_pixels = n_distinct(paste(x, y))
  ) %>%
  collect()

monthly_r <- monthly_spark %>% collect()


# -- Step 2 + 5–7: SPEI calculator for a single pixel ------------------------
# Analysis Plan Section 1.2 Steps 2, 5, 6, 7:
#   Step 2: Water balance = Precip - PET (Thornthwaite)
#   Step 5: Fit log-logistic distribution (handles negative WB; Gamma cannot)
#   Step 6: Cumulative probability from log-logistic
#   Step 7: Transform to standard normal z-score (SPEI)
#
# Thornthwaite PET requires monthly mean temperature (°C) and latitude.
# SPEI::spei() handles Steps 5–7 internally via log-logistic distribution.
#
# Minimum data guard: same logic as Script 01 — max(scale*4, 24) months.
# Inf values replaced with NA (can occur at extreme WB tails).
safe_spei <- function(precip_vec, temp_vec, lat, scale = 3) {
  tryCatch({
    min_obs <- max(scale * 4, 24)
    if (sum(!is.na(precip_vec)) < min_obs) {
      return(rep(NA_real_, length(precip_vec)))
    }

    # Step 2a: Thornthwaite PET (°C required).
    # ERA5 mean_temp is stored in Kelvin in the Databricks catalog.
    # Detect by checking median: Kelvin values cluster ~273-310, Celsius ~0-37.
    temp_c <- if (median(temp_vec, na.rm = TRUE) > 100) temp_vec - 273.15 else temp_vec
    pet <- as.numeric(thornthwaite(temp_c, lat = lat))

    # Step 2b: Water balance
    water_balance <- precip_vec - pet

    # Steps 5–7: log-logistic fit → cumulative prob → z-score
    result <- as.numeric(spei(water_balance, scale = scale, na.rm = TRUE)$fitted)
    result[is.infinite(result)] <- NA_real_

    return(result)

  }, error = function(e) {
    message("SPEI failed for lat=", lat, " : ", e$message)
    return(rep(NA_real_, length(precip_vec)))
  })
}


# -- Step 3 + 4: Apply SPEI at all four accumulation periods ------------------
# Analysis Plan Section 1.2 Steps 3–4: four accumulation periods (1, 3, 6, 12 months).
#
# SPEI-1  → single-month water balance anomaly; flash drought onset
# SPEI-3  → 3-month rolling WB; seasonal drought
# SPEI-6  → 6-month rolling WB; medium-term drought
# SPEI-12 → 12-month rolling WB; long-term/hydrological drought
#
# lat = first(y): each pixel group uses its own latitude for Thornthwaite PET.
# Data MUST be chronologically sorted before grouping.
spei_results <- monthly_r %>%
  arrange(x, y, year, month) %>%
  group_by(x, y, country) %>%
  mutate(
    spei1  = safe_spei(monthly_precip, mean_temp_monthly, lat = first(y), scale = 1),
    spei3  = safe_spei(monthly_precip, mean_temp_monthly, lat = first(y), scale = 3),
    spei6  = safe_spei(monthly_precip, mean_temp_monthly, lat = first(y), scale = 6),
    spei12 = safe_spei(monthly_precip, mean_temp_monthly, lat = first(y), scale = 12)
  ) %>%
  ungroup()


# -- Sanity check -------------------------------------------------------------
# All inf counts should be 0. All mean values should be ~0 (standardised).
spei_results %>%
  summarise(
    total         = n(),
    inf_spei1     = sum(is.infinite(spei1),  na.rm = TRUE),
    inf_spei3     = sum(is.infinite(spei3),  na.rm = TRUE),
    inf_spei6     = sum(is.infinite(spei6),  na.rm = TRUE),
    inf_spei12    = sum(is.infinite(spei12), na.rm = TRUE),
    na_spei1      = sum(is.na(spei1)),
    na_spei3      = sum(is.na(spei3)),
    na_spei6      = sum(is.na(spei6)),
    na_spei12     = sum(is.na(spei12)),
    pct_na_spei1  = round(mean(is.na(spei1))  * 100, 1),
    pct_na_spei3  = round(mean(is.na(spei3))  * 100, 1),
    mean_spei1    = round(mean(spei1,  na.rm = TRUE), 3),
    mean_spei3    = round(mean(spei3,  na.rm = TRUE), 3),
    mean_spei6    = round(mean(spei6,  na.rm = TRUE), 3),
    mean_spei12   = round(mean(spei12, na.rm = TRUE), 3)
  )

message("SPEI (Thornthwaite) calculation complete!")


# -- Save output --------------------------------------------------------------
# spei_thornthwaite_results.rds feeds into Script 03 alongside spi_results.rds and spei_penman_results.rds.
VOLUME_PATH <- "/Volumes/projects/gwsc_drought/drought_outputs"
saveRDS(spei_results, file.path(VOLUME_PATH, "spei_thornthwaite_results.rds"))
