# =============================================================================
# 01. SPI Calculation
# =============================================================================
# Analysis Plan Reference: Section 1.1 — Calculating SPI using ERA5 data
#
# This script implements Steps 1–6 of Section 1.1:
#   Step 1: Collect raw monthly precipitation from ERA5 via Databricks
#   Step 2: Decide accumulation periods (1, 3, 6, 12 months)
#   Step 3: Aggregate daily → monthly precipitation
#   Step 4: Fit Gamma distribution per pixel per month
#   Step 5: Calculate cumulative probability
#   Step 6: Transform to standard normal (SPI z-score)
#
# Output: spi_results.rds — one row per pixel per month with SPI-1/3/6/12
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
# Connects to the Databricks cluster where ERA5 data is stored.
# Cluster ID read from environment variable (set in .Renviron or Databricks env).
sc <- spark_connect(method = "databricks")


# -- Data source --------------------------------------------------------------
# Analysis Plan Section 1.1 Step 1: ERA5 daily data table on Databricks.
# Table contains: x, y (pixel coords), time, year, month, precip, mean_temp.
# Data range: 1973–2023. Reference period for fitting: 1970–1999 (set in Step 4).
project_folder <- "xx_drought_index"

era5_normals <- tbl(sc, in_catalog("projects", project_folder, "drought_index_dailysummary"))


# -- Data check (run once to verify) ------------------------------------------
# Confirms spatial extent, temporal range, and row count before pulling to R.
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


# -- Step 3: Daily → monthly aggregation (runs in Spark) ----------------------
# Analysis Plan Section 1.1 Step 3: Sum daily precipitation within each month per pixel.
# All computation stays in Spark — avoids pulling raw daily data into R memory.
# Months with 5+ missing days excluded as a data quality guard.
monthly_spark <- era5_normals %>%
  group_by(x, y, country, year, month) %>%
  summarise(
    monthly_precip    = sum(precip, na.rm = TRUE),
    mean_temp_monthly = mean(mean_temp, na.rm = TRUE),  # retained for SPEI (Script 02)
    n_days_observed   = n(),
    n_days_missing    = sum(as.integer(is.na(precip))),
    .groups = "drop"
  ) %>%
  filter(n_days_missing < 5) %>%
  arrange(x, y, year, month)

# Verify monthly size before pulling to R
monthly_spark %>%
  summarise(
    n_rows   = n(),
    n_pixels = n_distinct(paste(x, y))
  ) %>%
  collect()


# -- Collect monthly data into R ----------------------------------------------
# After aggregation in Spark, pull the smaller monthly table into R memory
# for the pixel-by-pixel SPI fitting (SPEI package runs in R, not Spark).
monthly_r <- monthly_spark %>% collect()


# -- Step 4–6: SPI calculator for a single pixel ------------------------------
# Analysis Plan Section 1.1 Steps 4–6:
#   Step 4: Fit Gamma distribution to historical aggregated precipitation
#   Step 5: Calculate cumulative probability from the fitted Gamma
#   Step 6: Transform cumulative probability to standard normal z-score (SPI)
#
# The SPEI::spi() function handles all three steps internally.
# Each pixel receives its own Gamma fit — a desert pixel and a rainforest pixel
# are each compared to their own historical normal, not a global average.
#
# Minimum data check: need at least (scale × 4) months of valid data,
# minimum 24 months, to ensure the Gamma fit is statistically reliable.
safe_spi <- function(precip_vec, scale = 3) {
  tryCatch({
    min_obs <- max(scale * 4, 24)
    if (sum(!is.na(precip_vec)) < min_obs) {
      return(rep(NA_real_, length(precip_vec)))
    }
    as.numeric(spi(precip_vec, scale = scale, na.rm = TRUE)$fitted)
  }, error = function(e) {
    rep(NA_real_, length(precip_vec))
  })
}


# -- Step 2 + 4–6: Apply SPI at all four accumulation periods -----------------
# Analysis Plan Section 1.1 Step 2: four accumulation periods (1, 3, 6, 12 months).
#
# SPI-1  → single-month anomaly; captures rapid onset drought
# SPI-3  → 3-month rolling sum; seasonal/short-term drought
# SPI-6  → 6-month rolling sum; medium-term drought
# SPI-12 → 12-month rolling sum; long-term/hydrological drought
#
# Data MUST be chronologically sorted before grouping — wrong order produces
# incorrect rolling sums and entirely wrong SPI values.
spi_results <- monthly_r %>%
  arrange(x, y, year, month) %>%
  group_by(x, y) %>%
  mutate(
    spi1  = safe_spi(monthly_precip, scale = 1),
    spi3  = safe_spi(monthly_precip, scale = 3),
    spi6  = safe_spi(monthly_precip, scale = 6),
    spi12 = safe_spi(monthly_precip, scale = 12)
  ) %>%
  ungroup()

message("SPI calculation complete!")
head(spi_results)


# -- Save output --------------------------------------------------------------
# spi_results.rds is the input to Script 03 (drought detection).
# One row per pixel per month; columns: x, y, year, month, spi1, spi3, spi6, spi12.
VOLUME_PATH <- "/Volumes/projects/gwsc_drought/drought_outputs"
saveRDS(spi_results, file.path(VOLUME_PATH, "spi_results.rds"))
