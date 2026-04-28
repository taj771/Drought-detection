# =============================================================================
# CHUNK 1 — ERA5 REFERENCE PERIOD — FINAL CLEAN VERSION
# =============================================================================

library(sparklyr)
library(dplyr)
library(dbplyr)
library(tidyr)
library(lubridate)
library(SPEI)
library(purrr)

# ── Connect ───────────────────────────────────────────────────────────────────
sc <- spark_connect(
  cluster_id = Sys.getenv("DATABRICKS_CLUSTER_ID"),
  method     = "databricks_connect"
)

CALIB_START <- 1975
CALIB_END   <- 2010
PROJECT     <- "xx_drought_index"
cat(sprintf("Reference period: %d-%d\n", CALIB_START, CALIB_END))

# ── Load and aggregate ERA5 ───────────────────────────────────────────────────
era5_raw <- tbl(sc, in_catalog("projects", PROJECT,
                               "drought_index_dailysummary"))

monthly_spark <- era5_raw %>%
 # filter(country == "Benin") %>%          # ← remove for full run
  group_by(x, y, year, month) %>%
  summarise(
    monthly_precip    = sum(precip,     na.rm = TRUE),
    mean_temp_monthly = mean(mean_temp, na.rm = TRUE),
    n_days_observed   = n(),
    n_days_missing    = sum(as.integer(is.na(precip))),
    .groups = "drop"
  ) %>%
  filter(n_days_missing < 5) %>%
  arrange(x, y, year, month)

monthly_r <- monthly_spark %>% collect()

cat(sprintf("Rows      : %d\n", nrow(monthly_r)))
cat(sprintf("Pixels    : %d\n", n_distinct(paste(monthly_r$x, monthly_r$y))))
cat(sprintf("Years     : %d to %d\n", min(monthly_r$year), max(monthly_r$year)))

# =============================================================================
# FUNCTION 1 — extract Gamma parameters from ERA5 calibration period
# =============================================================================

extract_gamma_params <- function(pixel_df, calib_end = 2010) {
  
  px      <- pixel_df$x[1]
  py      <- pixel_df$y[1]
  country <- if ("country" %in% names(pixel_df)) pixel_df$country[1] else NA_character_
  
  pixel_df   <- pixel_df %>% arrange(year, month)
  precip_vec <- pixel_df$monthly_precip
  years_vec  <- pixel_df$year
  calib_idx  <- years_vec <= calib_end
  
  results <- list()
  
  for (sc_val in c(1, 3, 6)) {
    
    min_obs <- max(sc_val * 4, 24)
    if (sum(!is.na(precip_vec[calib_idx])) < min_obs) next
    
    precip_calib             <- precip_vec
    precip_calib[!calib_idx] <- NA
    
    tryCatch(
      {
        fit <- spi(
          precip_calib,
          scale        = sc_val,
          na.rm        = TRUE,
          fit          = "ub-pwm",
          distribution = "Gamma"
        )
        
        coef_array <- fit$coefficients   # 2 x 1 x 12 array
        alpha_vals <- coef_array["alpha", 1, ]
        beta_vals  <- coef_array["beta",  1, ]
        
        params <- data.frame(
          alpha          = as.numeric(alpha_vals),
          beta           = as.numeric(beta_vals),
          calendar_month = 1:12,
          spi_scale      = sc_val,
          x              = px,
          y              = py,
          country        = country
        )
        
        results[[length(results) + 1]] <- params
      },
      error = function(e) {
        cat(sprintf("Error x=%.3f y=%.3f scale=%d: %s\n",
                    px, py, sc_val, e$message))
        NULL
      }
    )
  }
  
  if (length(results) == 0) return(data.frame())
  do.call(rbind, results)
}

# =============================================================================
# FUNCTION 2 — apply ERA5 Gamma parameters to any precipitation series
# =============================================================================

apply_gamma_params <- function(pixel_df, params_all) {
  
  px <- pixel_df$x[1]
  py <- pixel_df$y[1]
  
  pixel_df   <- pixel_df %>% arrange(year, month)
  precip_vec <- pixel_df$monthly_precip
  years_vec  <- pixel_df$year
  
  pixel_params <- params_all %>% filter(x == px, y == py)
  
  if (nrow(pixel_params) == 0) {
    pixel_df$spi1 <- NA_real_
    pixel_df$spi3 <- NA_real_
    pixel_df$spi6 <- NA_real_
    return(pixel_df)
  }
  
  for (sc_val in c(1, 3, 6)) {
    
    col_name    <- paste0("spi", sc_val)
    min_obs     <- max(sc_val * 4, 24)
    warmup_rows <- sc_val - 1
    
    if (sum(!is.na(precip_vec)) < min_obs) {
      pixel_df[[col_name]] <- NA_real_
      next
    }
    
    scale_params <- pixel_params %>%
      filter(spi_scale == sc_val) %>%
      arrange(calendar_month) %>%
      select(alpha, beta)
    
    if (nrow(scale_params) == 0) {
      pixel_df[[col_name]] <- NA_real_
      next
    }
    
    coef_array           <- array(NA, dim = c(2, 1, 12))
    coef_array[1, 1, ]   <- scale_params$alpha
    coef_array[2, 1, ]   <- scale_params$beta
    dimnames(coef_array) <- list(c("alpha", "beta"), NULL, NULL)
    
    tryCatch(
      {
        out <- spi(
          precip_vec,
          scale        = sc_val,
          na.rm        = TRUE,
          fit          = "ub-pwm",
          distribution = "Gamma",
          params       = coef_array
        )
        
        spi_vals <- as.numeric(out$fitted)
        
        # warmup months → NA
        if (warmup_rows > 0) spi_vals[1:warmup_rows] <- NA_real_
        
        # zero precip -Inf → -3.09
        spi_vals[is.infinite(spi_vals) & spi_vals < 0] <- -3.09
        spi_vals[is.infinite(spi_vals) & spi_vals > 0] <-  3.09
        
        # NaN → NA
        spi_vals[is.nan(spi_vals)] <- NA_real_
        
        pixel_df[[col_name]] <- spi_vals
      },
      error = function(e) {
        cat(sprintf("  Error x=%.3f y=%.3f scale=%d: %s\n",
                    px, py, sc_val, e$message))
        pixel_df[[col_name]] <<- NA_real_
      }
    )
  }
  
  return(pixel_df)
}

# =============================================================================
# STEP 1 — Extract Gamma parameters from ERA5 calibration period
# =============================================================================

pixels <- monthly_r %>% select(x, y) %>% distinct()
cat(sprintf("\nExtracting Gamma params for %d pixels...\n\n", nrow(pixels)))

era5_gamma_params <- map_dfr(seq_len(nrow(pixels)), function(i) {
  
  if (i %% 50 == 0) cat(sprintf("  Pixel %d / %d\n", i, nrow(pixels)))
  
  pixel_ts <- monthly_r %>%
    filter(x == pixels$x[i], y == pixels$y[i]) %>%
    arrange(year, month)
  
  extract_gamma_params(pixel_ts, calib_end = CALIB_END)
})

cat(sprintf("\nParams extracted : %d rows\n", nrow(era5_gamma_params)))
cat(sprintf("Pixels covered   : %d\n",
            n_distinct(paste(era5_gamma_params$x, era5_gamma_params$y))))

era5_gamma_params %>%
  group_by(spi_scale) %>%
  summarise(
    n_rows     = n(),
    n_pixels   = n_distinct(paste(x, y)),
    alpha_mean = round(mean(alpha, na.rm = TRUE), 3),
    beta_mean  = round(mean(beta,  na.rm = TRUE), 3),
    .groups    = "drop"
  ) %>%
  print()

saveRDS(era5_gamma_params, "./Data/era5_gamma_params_1975_2010.rds")
message("Saved: era5_gamma_params_1975_2010.rds")

# =============================================================================
# STEP 2 — Apply ERA5 parameters to full ERA5 series 1975-2023
# =============================================================================

cat("\nApplying ERA5 params to full ERA5 series...\n\n")

spi_era5 <- map_dfr(seq_len(nrow(pixels)), function(i) {
  
  if (i %% 50 == 0) cat(sprintf("  Pixel %d / %d\n", i, nrow(pixels)))
  
  pixel_ts <- monthly_r %>%
    filter(x == pixels$x[i], y == pixels$y[i]) %>%
    arrange(year, month)
  
  apply_gamma_params(pixel_ts, era5_gamma_params)
  
}) %>%
  mutate(
    date   = as.Date(sprintf("%d-%02d-01", year, month)),
    period = ifelse(
      year <= CALIB_END,
      sprintf("%d-%d reference",  CALIB_START, CALIB_END),
      sprintf("%d-%d evaluation", CALIB_END + 1, max(year))
    )
  )

cat(sprintf("\nSPI calculated : %d rows\n", nrow(spi_era5)))

# =============================================================================
# STEP 3 — Validate ERA5 SPI
# =============================================================================

cat("\n=== VALIDATION — ERA5 SPI ===\n\n")
cat("Key metrics: mean ≈ 0 and sd ≈ 1 confirm correct standardisation\n\n")

spi_era5 %>%
  group_by(period) %>%
  summarise(
    n_months        = n(),
    pct_below_0     = round(100 * mean(spi1 < 0,    na.rm = TRUE), 1),
    pct_below_neg05 = round(100 * mean(spi1 < -0.5, na.rm = TRUE), 1),
    pct_below_neg1  = round(100 * mean(spi1 < -1.0, na.rm = TRUE), 1),
    mean_spi1       = round(mean(spi1, na.rm = TRUE), 3),
    sd_spi1         = round(sd(spi1,   na.rm = TRUE), 3),
    pct_na          = round(100 * mean(is.na(spi1)), 1),
    .groups = "drop"
  ) %>%
  print()

cat("\nValidation criteria:\n")
cat("  mean_spi ≈ 0.000  ← most important\n")
cat("  sd_spi   ≈ 1.000  ← most important\n")
cat("  pct_na   ≈ 0%     ← no missing values\n")
cat("  pct_below_0 ~ 41-43% for Benin\n")
cat("  (lower than 50% due to right-skewed\n")
cat("   precipitation distribution)\n\n")

# =============================================================================
# STEP 4 — Save ERA5 results
# =============================================================================

saveRDS(spi_era5, "./Data/spi_era5_with_reference.rds")
message("Saved: spi_era5_with_reference.rds")
message("Chunk 1 complete")

# =============================================================================
# CHUNK 2 — APPLY ERA5 PARAMETERS TO CMIP DATA