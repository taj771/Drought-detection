#------------Calculation of SPEI------------------
# Here there are few ways to calualate the PET and we use 
# Example country: Argentina


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


# --------------------------------------------------------------
# STEP 2: Define the SPEI calculator for a single pixel
#
# What changes vs SPI:
#   - First compute PET using Thornthwaite (needs temp + latitude)
#   - Then compute water balance = precip - PET
#   - Feed water balance into spei() instead of raw precip
#   - SPEI uses log-logistic distribution internally (handles negatives)
#     because water balance CAN be negative (hot dry months)
#     Gamma cannot handle negatives — that is why distribution changes
# --------------------------------------------------------------

# SPEI calculation using Thornthwaite PET method
# Builds on your existing spi_results which already has:
#   x, y, year, month, monthly_precip, mean_temp_monthly
# --------------------------------------------------------------

library(SPEI)
library(dplyr)

# ── SPEI calculator for one pixel ────────────────────────────────────────────
safe_spei <- function(precip_vec, temp_vec, lat, scale = 3) {
  tryCatch({
    
    # Minimum data check — need enough history to fit distribution
    # SPI-3  needs at least 3 × 4 = 12 valid months
    # SPI-6  needs at least 6 × 4 = 24 valid months
    # SPI-12 needs at least 12 × 4 = 48 valid months
    if (sum(!is.na(precip_vec)) < (scale * 4)) {
      return(rep(NA_real_, length(precip_vec)))
    }
    
    # Step 1: Compute PET via Thornthwaite
    #   - needs absolute temperature in °C
    #   - needs latitude in degrees (negative = southern hemisphere)
    #   - returns PET in mm/month
    pet <- as.numeric(thornthwaite(temp_vec, lat = lat))
    
    # Step 2: Water balance = precip - PET
    #   - positive → more rain than demand → wet
    #   - negative → more demand than rain → drought stress
    #   - CAN be negative — that is why SPEI uses log-logistic not Gamma
    water_balance <- precip_vec - pet
    
    # Step 3: SPEI from water balance
    # Pipeline inside spei():
    #   Input vector
    #   ↓ Rolling sum over scale months
    #   ↓ Fit log-logistic distribution per calendar month
    #     (log-logistic used because water balance can be negative,
    #      Gamma cannot handle negative values)
    #   ↓ Convert to cumulative probability
    #   ↓ qnorm() → z-score output (mean=0, sd=1)
    result <- as.numeric(spei(water_balance, scale = scale, na.rm = TRUE)$fitted)  # ← assigned to result
    
    # Step 4: Replace Inf / -Inf with NA
    #   These occur when extreme water balance values get cumulative
    #   probability of exactly 0 or 1, making qnorm() return ±Inf
    #   Not physically meaningful — treat as missing
    result[is.infinite(result)] <- NA_real_
    
    return(result)
    
  }, error = function(e) {
    message("SPEI failed for lat=", lat, " : ", e$message)
    return(rep(NA_real_, length(precip_vec)))
  })
}

# ── Apply SPEI to every pixel independently ───────────────────────────────────
spei_results <- spi_results %>%
  
  # Chronological order is critical — wrong order = wrong rolling sums
  arrange(x, y, year, month) %>%
  
  # Each pixel gets its own PET and its own water balance distribution
  group_by(x, y) %>%
  
  mutate(
    spei3  = safe_spei(monthly_precip, mean_temp_monthly, lat = first(y), scale = 3),
    spei6  = safe_spei(monthly_precip, mean_temp_monthly, lat = first(y), scale = 6),
    spei12 = safe_spei(monthly_precip, mean_temp_monthly, lat = first(y), scale = 12)
  ) %>%
  
  ungroup()

# ── Sanity check ──────────────────────────────────────────────────────────────
spei_results %>%
  summarise(
    total        = n(),
    inf_spei3    = sum(is.infinite(spei3),  na.rm = TRUE),   # should be 0
    inf_spei6    = sum(is.infinite(spei6),  na.rm = TRUE),   # should be 0
    inf_spei12   = sum(is.infinite(spei12), na.rm = TRUE),   # should be 0
    na_spei3     = sum(is.na(spei3)),
    na_spei6     = sum(is.na(spei6)),
    na_spei12    = sum(is.na(spei12)),
    pct_na_spei3 = round(mean(is.na(spei3)) * 100, 1),
    mean_spei3   = round(mean(spei3,  na.rm = TRUE), 3),     # should be ~0
    mean_spei6   = round(mean(spei6,  na.rm = TRUE), 3),     # should be ~0
    mean_spei12  = round(mean(spei12, na.rm = TRUE), 3)      # should be ~0
  )

message("SPEI calculation complete!")
saveRDS(spei_results, "./Data/spei_results.rds")