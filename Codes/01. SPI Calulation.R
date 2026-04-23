#______Calculation of SPI_________________________


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

# create a spark connection -----------
sc <- spark_connect(
  cluster_id = Sys.getenv("DATABRICKS_CLUSTER_ID"),
  method = "databricks_connect"
)



# Define project variables -----------------------------
project.folder= 'xx_drought_index'

era5_normals <- tbl(sc, in_catalog('projects', project.folder, 'drought_index_dailysummary'))



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

# Check a sample
era5_normals %>%
  head(100) %>%
  collect()


# Aggregate daily to monthly - all happens in Spark (fast)
monthly_spark <- era5_normals %>%
  group_by(x, y, year, month) %>% # Creates groups for each unique combination of:- `x, y` = one specific pixel location- `year, month` = one specific calendar month
  summarise(
    monthly_precip   = sum(precip, na.rm = TRUE), # `monthly_precip = sum(precip)` → Adds up all daily precipitation in that month
    mean_temp_monthly = mean(mean_temp, na.rm = TRUE), # `mean_temp_monthly = mean(mean_temp)` → Average temperature for the month no direct use for now but probably whren ET calculating there should be use
    n_days_observed  = n(),                                     # How many days recorded
    n_days_missing   = sum(as.integer(is.na(precip))),          # Spark-safe NA count
    .groups = 'drop'
  ) %>%
  # Filter out months with too many missing days
  filter(n_days_missing < 5) %>%          # Allow max 5 missing days per month, Removes months with too many missing days,  Ensures SPI is calculated on reliable data probably with ER5 and CMIP there wont be missing values but add as a sanity check 
  arrange(x, y, year, month) # Data must be in chronological order for each pixel

# Check size of monthly data before collecting
# Counts total rows and unique pixels BEFORE pulling data to R - Helps to estimate memory needs- `n_rows` = total monthly observations - `n_pixels` = number of unique locations

monthly_spark %>%
  summarise(
    n_rows   = n(),
    n_pixels = n_distinct(paste(x, y))
  ) %>%
  collect()


library(SPEI)

# -----------------------------------------------------------
# STEP 1: Bring monthly precipitation data into R memory
# Each row = one pixel (x,y) at a specific year/month
# Example row: x=10, y=20, year=2021, month=3, monthly_precip=30mm
# -----------------------------------------------------------
monthly_r <- monthly_spark %>% collect()


# -------------------------------------------------------------
# STEP 2: Define the SPI calculator for a single pixel
#
# What this function receives (precip_vec):
#   A full time series of monthly precipitation for ONE pixel
#   e.g. [12.3, 0.0, 5.1, 44.2, 80.1, ...] from 1990 to 2021
#
# What this function returns:
#   Same length vector but with SPI z-scores instead of mm values
#   e.g. [-0.5, -1.2, 0.3, 1.1, -2.1, ...]
#
# What SPI actually does internally (e.g. scale = 3):
#   1. Rolls a 3-month window:  Jan+Feb+Mar, Feb+Mar+Apr, ...
#   2. Groups by calendar month: collects all Jan+Feb+Mar sums (1990-2020)
#   3. Fits a Gamma distribution to those historical sums
#   4. Asks: where does the CURRENT year's sum fall in that history?
#   5. Converts that position to a z-score (SPI value)
#
#   Example:
#   Historical Jan+Feb+Mar (1990-2020) → Gamma distribution fitted
#   2021 Jan+Feb+Mar = 30mm → falls in far left tail → SPI3 = -2.1 (severe drought)
# --------------------------------------------------------------
safe_spi <- function(precip_vec, scale = 3) {
  tryCatch({
    
    # check: Do we have enough history to fit a reliable Gamma? 
    # For scale=3  → need at least 12 valid months (3 * 4)
    # For scale=6  → need at least 24 valid months (6 * 4)
    # For scale=12 → need at least 48 valid months (12 * 4)
    # Without enough data, the Gamma fit is statistically meaningless
    min_obs <- max(scale * 4, 24)
    if (sum(!is.na(precip_vec)) < min_obs) {
      return(rep(NA_real_, length(precip_vec)))
    }
    
    # ── Core calculation ──
    # spi() does all the heavy lifting:
    #   - rolls the accumulation window (e.g. 3-month sums)
    #   - fits Gamma distribution from historical data
    #   - transforms each value to a z-score
    # $fitted extracts just the z-scores
    # as.numeric() strips any extra formatting from SPEI package
    as.numeric(spi(precip_vec, scale = scale, na.rm = TRUE)$fitted)
    
  }, error = function(e) {
    # If anything goes wrong (e.g. all zeros, weird distribution),
    # return NAs instead of crashing the whole pipeline
    return(rep(NA_real_, length(precip_vec)))
  })
}


# --------------------------------------------------------------
# STEP 3: Apply SPI to every pixel independently
#
# Logic:
#   - Each pixel (x,y) has its OWN precipitation history
#   - So each pixel gets its OWN Gamma distribution
#   - A pixel in a desert and a pixel in a rainforest
#     are both compared to THEIR OWN normal, not a global average
#
# Three drought timescales calculated:
#   spi3  → 3-month  window → captures seasonal/short-term drought
#   spi6  → 6-month  window → captures medium-term drought
#   spi12 → 12-month window → captures long-term/hydrological drought
#
# SPI value interpretation:
#   > +2.0  → Extremely wet
#     +1.0  → Moderately wet
#      0.0  → Normal
#     -1.0  → Moderately dry
#   < -2.0  → Extremely dry (severe drought)
# ----------------------------------------------------

spi_results <- monthly_r %>%
  
  # CRITICAL: time series MUST be in chronological order
  # Wrong order = wrong rolling sums = completely wrong SPI values
  arrange(x, y, year, month) %>%
  
  # Process each pixel independently
  # pixel (x=10,y=20) gets its own Gamma, pixel (x=10,y=21) gets its own Gamma
  group_by(x, y) %>%
  
  mutate(
    # Each safe_spi() call receives the full chronological precip history
    # for this pixel and returns a z-score for every single month
    spi1  = safe_spi(monthly_precip, scale = 1),   # single-month anomaly
    spi3  = safe_spi(monthly_precip, scale = 3),   # Jan+Feb+Mar vs historical Jan+Feb+Mar
    spi6  = safe_spi(monthly_precip, scale = 6),   # 6-month window
    spi12 = safe_spi(monthly_precip, scale = 12)   # 12-month window
  ) %>%
  
  ungroup()

message("SPI calculation complete!")
head(spi_results)


saveRDS(spi_results, "./Data/spi_results.rds")












