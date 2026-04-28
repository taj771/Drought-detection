#______Calculation of SPI — Split Reference Period_________________________

# Load necessary libraries -------------
library(here)
library(sparklyr)
library(dplyr)
library(dbplyr)
library(ggplot2)
library(colorspace)
library(tidyterra)
library(terra)
library(ggpubr)
library(tidyr)
library(stringr)
library(lubridate)
library(modelsummary)
library(SPEI)

# create a spark connection -----------
sc <- spark_connect(
  cluster_id = Sys.getenv("DATABRICKS_CLUSTER_ID"),
  method = "databricks_connect"
)

# Define project variables -----------------------------
project.folder <- 'xx_drought_index'

era5_normals <- tbl(sc, in_catalog('projects', project.folder,
                                   'drought_index_dailysummary'))

# ── Define reference and evaluation periods ───────────────────────────────────
CALIB_START <- 1975
CALIB_END   <- 1999   # ← reference period end
EVAL_START  <- 2000   # ← evaluation period start
EVAL_END    <- 2023

cat(sprintf("Reference period : %d-%d\n", CALIB_START, CALIB_END))
cat(sprintf("Evaluation period: %d-%d\n", EVAL_START,  EVAL_END))

# Check size BEFORE collecting anything
era5_normals %>%
  summarise(
    total_rows  = n(),
    n_pixels    = n_distinct(paste(x, y)),
    n_years     = n_distinct(year),
    date_start  = min(time),
    date_end    = max(time)
  ) %>%
  collect()

# Aggregate daily to monthly
monthly_spark <- era5_normals %>%
 # filter(country == "Benin") %>%          # change countries
  group_by(x, y, year, month) %>%
  summarise(
    monthly_precip    = sum(precip,    na.rm = TRUE),
    mean_temp_monthly = mean(mean_temp, na.rm = TRUE),
    n_days_observed   = n(),
    n_days_missing    = sum(as.integer(is.na(precip))),
    .groups = 'drop'
  ) %>%
  filter(n_days_missing < 5) %>%
  arrange(x, y, year, month)

# Bring into R memory
monthly_r <- monthly_spark %>% collect()

# =============================================================================
# STEP 2: Define SPI calculator with split reference period
#
# What changed from original:
#   ORIGINAL: fits Gamma on FULL time series 1975-2023
#             every value measured against itself
#             tautological for period comparison
#
#   NEW:      fits Gamma on 1975-1999 ONLY (reference)
#             applies those fixed parameters to 1975-2023
#             2000-2023 values measured against 1975-1999 normal
#             genuine climate signal preserved
#
# How it works:
#   1. Mask out 2000-2023 (set to NA)
#   2. Fit Gamma on 1975-1999 only
#   3. Extract shape and scale parameters
#   4. Apply those parameters to full 1975-2023 series
#   5. Every month converted to z-score using 1975-1999 yardstick
# =============================================================================

safe_spi_split <- function(precip_vec, years_vec,
                           scale     = 3,
                           calib_end = 1999) {
  tryCatch({
    
    # ── Minimum observations check ──────────────────────────────────────────
    # Same as original but applied to calibration period only
    min_obs    <- max(scale * 4, 24)
    calib_idx  <- years_vec <= calib_end
    
    if (sum(!is.na(precip_vec[calib_idx])) < min_obs) {
      return(rep(NA_real_, length(precip_vec)))
    }
    
    # ── Step A — mask evaluation period ─────────────────────────────────────
    # Hide 2000-2023 from the Gamma fitter
    # Only 1975-1999 data visible for distribution fitting
    precip_calib <- precip_vec
    precip_calib[!calib_idx] <- NA
    
    # ── Step B — fit Gamma on 1975-1999 ONLY ────────────────────────────────
    # spi() sees only calibration period data
    # Fits Gamma distribution per calendar month
    # e.g. all Jan+Feb+Mar sums 1975-1999 → one Gamma
    #      all Apr+May+Jun sums 1975-1999 → another Gamma
    # Extracts shape and scale parameters
    fit <- spi(
      precip_calib,
      scale        = scale,
      na.rm        = TRUE,
      fit          = "ub-pwm",
      distribution = "Gamma"
    )
    
    calib_params <- fit$coefficients
    
    # ── Step C — apply 1975-1999 parameters to full 1975-2023 series ────────
    # Now feed the FULL precipitation series
    # but use the 1975-1999 Gamma parameters as the yardstick
    # 
    # For each month spi() asks:
    #   "Where does this 3-month sum fall
    #    in the 1975-1999 Gamma distribution?"
    # Then converts that position to a z-score
    #
    # 1975-1999 months → measured against 1975-1999 normal → ~N(0,1)
    # 2000-2023 months → ALSO measured against 1975-1999 normal
    #                    if drier → more negative SPI → real signal
    out <- spi(
      precip_vec,          # ← full 1975-2023 series
      scale        = scale,
      na.rm        = TRUE,
      fit          = "ub-pwm",
      distribution = "Gamma",
      params       = calib_params   # ← fixed 1975-1999 parameters
    )
    
    as.numeric(out$fitted)
    
  }, error = function(e) {
    return(rep(NA_real_, length(precip_vec)))
  })
}

# =============================================================================
# STEP 3: Apply split SPI to every pixel independently
#
# Same logic as original:
#   Each pixel gets its OWN Gamma fitted on ITS OWN 1975-1999 data
#   A pixel in Benin and a pixel in Thailand
#   both compared to THEIR OWN 1975-1999 normal
#
# Key difference from original:
#   Original: safe_spi(monthly_precip, scale = 3)
#             fits on full 1975-2023
#
#   New:      safe_spi_split(monthly_precip, year, scale = 3)
#             fits on 1975-1999 only
#             applies to full 1975-2023
# =============================================================================

spi_results <- monthly_r %>%
  
  # CRITICAL: must be in chronological order
  arrange(x, y, year, month) %>%
  
  # Process each pixel independently
  group_by(x, y) %>%
  
  mutate(
    # Pass year vector so function knows which months are calibration
    # calib_end = 1999 means 1975-1999 used to fit Gamma
    spi1  = safe_spi_split(monthly_precip, year,
                           scale = 1,  calib_end = CALIB_END),
    spi3  = safe_spi_split(monthly_precip, year,
                           scale = 3,  calib_end = CALIB_END),
    spi6  = safe_spi_split(monthly_precip, year,
                           scale = 6,  calib_end = CALIB_END),
    spi12 = safe_spi_split(monthly_precip, year,
                           scale = 12, calib_end = CALIB_END)
  ) %>%
  
  ungroup()

message("Split reference period SPI calculation complete!")

# =============================================================================
# STEP 4: Validate the split worked correctly
#
# Reference period 1975-1999 should be ~N(0,1)
# ~50% below 0, ~30.9% below -0.5, ~15.9% below -1.0
#
# Evaluation period 2000-2023 can differ
# Any deviation = real climate signal
# =============================================================================

cat("\n=== VALIDATION — exceedance rates by period ===\n\n")

spi_results %>%
  mutate(
    period = ifelse(year <= CALIB_END,
                    sprintf("%d-%d reference", CALIB_START, CALIB_END),
                    sprintf("%d-%d evaluation", EVAL_START, EVAL_END))
  ) %>%
  group_by(period) %>%
  summarise(
    pct_spi1_below_0    = round(100 * mean(spi1 < 0,    na.rm = TRUE), 1),
    pct_spi1_below_neg05 = round(100 * mean(spi1 < -0.5, na.rm = TRUE), 1),
    pct_spi1_below_neg1  = round(100 * mean(spi1 < -1.0, na.rm = TRUE), 1),
    mean_spi1            = round(mean(spi1, na.rm = TRUE), 3),
    sd_spi1              = round(sd(spi1,   na.rm = TRUE), 3),
    .groups = "drop"
  ) %>%
  print()

cat("\nExpected for reference period:\n")
cat("  pct below 0    ≈ 50.0%\n")
cat("  pct below -0.5 ≈ 30.9%\n")
cat("  pct below -1.0 ≈ 15.9%\n")
cat("  mean           ≈ 0.000\n")
cat("  sd             ≈ 1.000\n\n")
cat("Evaluation period deviation = real climate signal\n\n")

# =============================================================================
# STEP 5: Save results
# Same format as original spi_results
# Drop-in replacement — all downstream code unchanged
# =============================================================================

head(spi_results)
saveRDS(spi_results, "./Data/spi_results_split.rds")
message("Saved: spi_results_split.rds")