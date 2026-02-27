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
  group_by(x, y, year, month) %>%
  summarise(
    monthly_precip    = sum(precip, na.rm = TRUE),
    mean_temp_monthly = mean(mean_temp, na.rm = TRUE),
    n_days_observed   = n(),
    n_days_missing    = sum(as.integer(is.na(precip))),
    .groups = 'drop'
  ) %>%
  filter(n_days_missing < 5) %>%
  arrange(x, y, year, month)

# Check size of monthly data before collecting
monthly_spark %>%
  summarise(
    n_rows   = n(),
    n_pixels = n_distinct(paste(x, y))
  ) %>%
  collect()


library(SPEI)

# -----------------------------------------------------------
# STEP 1: Bring monthly precipitation data into R memory
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

library(SPEI)
library(dplyr)

# ── SPEI calculator for one pixel ────────────────────────────────────────────
safe_spei <- function(precip_vec, temp_vec, lat, scale = 3) {
  tryCatch({
    
    # ── CHANGED: replaced (scale * 4) with max(scale * 4, 24) ───────────────
    # Previously: if (sum(!is.na(precip_vec)) < (scale * 4))
    # Problem:    for scale=1 this only required 4 months — too few for a
    #             reliable log-logistic fit, leading to unstable SPEI-1 values
    # Fix:        enforce a minimum of 24 months regardless of scale,
    #             while keeping the scale*4 rule for larger scales (e.g. scale=12 → 48)
    min_obs <- max(scale * 4, 24)
    if (sum(!is.na(precip_vec)) < min_obs) {
      return(rep(NA_real_, length(precip_vec)))
    }
    # ── END CHANGE ────────────────────────────────────────────────────────────
    
    # Step 1: Compute PET via Thornthwaite
    pet <- as.numeric(thornthwaite(temp_vec, lat = lat))
    
    # Step 2: Water balance = precip - PET
    water_balance <- precip_vec - pet
    
    # Step 3: SPEI from water balance
    result <- as.numeric(spei(water_balance, scale = scale, na.rm = TRUE)$fitted)
    
    # Step 4: Replace Inf / -Inf with NA
    result[is.infinite(result)] <- NA_real_
    
    return(result)
    
  }, error = function(e) {
    message("SPEI failed for lat=", lat, " : ", e$message)
    return(rep(NA_real_, length(precip_vec)))
  })
}

# ── Apply SPEI to every pixel independently ──────────────────────────────────
spei_results <- spi_results %>%
  
  arrange(x, y, year, month) %>%
  
  group_by(x, y) %>%
  
  mutate(
    # ── ADDED: spei1 ─────────────────────────────────────────────────────────
    # scale=1: each month's water balance compared directly to all historical
    # values for that same calendar month via log-logistic distribution
    # Noisier than spei3/6/12 — best for detecting flash drought onset
    spei1  = safe_spei(monthly_precip, mean_temp_monthly, lat = first(y), scale = 1), 
    # ── END ADDITION ──────────────────────────────────────────────────────────
    spei3  = safe_spei(monthly_precip, mean_temp_monthly, lat = first(y), scale = 3),
    spei6  = safe_spei(monthly_precip, mean_temp_monthly, lat = first(y), scale = 6),
    spei12 = safe_spei(monthly_precip, mean_temp_monthly, lat = first(y), scale = 12)
  ) %>%
  
  ungroup()

# ── Sanity check ──────────────────────────────────────────────────────────────
spei_results %>%
  summarise(
    total         = n(),
    # ── ADDED: spei1 sanity checks ───────────────────────────────────────────
    inf_spei1     = sum(is.infinite(spei1),  na.rm = TRUE),   # should be 0   
    na_spei1      = sum(is.na(spei1)),                                        
    pct_na_spei1  = round(mean(is.na(spei1))  * 100, 1),                      
    mean_spei1    = round(mean(spei1,  na.rm = TRUE), 3),     # should be ~0  
    # ── END ADDITION ──────────────────────────────────────────────────────────
    inf_spei3     = sum(is.infinite(spei3),  na.rm = TRUE),   # should be 0
    inf_spei6     = sum(is.infinite(spei6),  na.rm = TRUE),   # should be 0
    inf_spei12    = sum(is.infinite(spei12), na.rm = TRUE),   # should be 0
    na_spei3      = sum(is.na(spei3)),
    na_spei6      = sum(is.na(spei6)),
    na_spei12     = sum(is.na(spei12)),
    pct_na_spei3  = round(mean(is.na(spei3))  * 100, 1),
    mean_spei3    = round(mean(spei3,  na.rm = TRUE), 3),     # should be ~0
    mean_spei6    = round(mean(spei6,  na.rm = TRUE), 3),     # should be ~0
    mean_spei12   = round(mean(spei12, na.rm = TRUE), 3)      # should be ~0
  )

message("SPEI calculation complete!")
saveRDS(spei_results, "./Data/spei_results.rds")