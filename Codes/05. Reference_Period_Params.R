# =============================================================================
# 05. Reference Period Parameters — Freeze ERA5 Distribution Params
# =============================================================================
# Analysis Plan Reference: Phase 2 Section 2.1
#
# Extracts and freezes distribution parameters fitted on ERA5 reference period
# (1973–2002), then applies them to the full ERA5 series. Frozen params are
# used in Phase 3 to apply identical standardisation to CMIP data.
# Note: ERA5 data available from 1973; reference period 1973–2002 = 30 years
# (WMO minimum). Comparison period: 2003–2023.
#
# SPI  — Gamma distribution (α, β per pixel × scale × calendar month)
# SPEI — Log-logistic distribution (ξ, α, κ per pixel × scale × calendar month)
#
# Inputs:
#   - ERA5 monthly data via Databricks catalog
#     catalog: projects | schema: gwsc_drought | table: era5_monthly
#
# Outputs (written to Unity Catalog Volume):
#   - /Volumes/projects/gwsc_drought/drought_outputs/era5_gamma_params_ref1973_2002.rds
#   - /Volumes/projects/gwsc_drought/drought_outputs/era5_loglogistic_params_ref1973_2002.rds
#   - /Volumes/projects/gwsc_drought/drought_outputs/spi_era5_reference_period.rds
#   - /Volumes/projects/gwsc_drought/drought_outputs/spei_era5_reference_period.rds
# =============================================================================


# -- Libraries ----------------------------------------------------------------
library(sparklyr)
library(dplyr)
library(dbplyr)
library(tidyr)
library(lubridate)
library(SPEI)
library(purrr)


# -- Configuration ------------------------------------------------------------
CALIB_START <- 1973
CALIB_END   <- 2002
CATALOG     <- "projects"
SCHEMA      <- "xx_drought_index"    # same schema as scripts 01/02a/02b
TABLE       <- "drought_index_dailysummary"  # daily table — aggregate below
VOLUME_PATH <- "/Volumes/projects/gwsc_drought/drought_outputs"  # Unity Catalog Volume

cat(sprintf("Reference period: %d–%d\n", CALIB_START, CALIB_END))
cat(sprintf("Output path     : %s\n", VOLUME_PATH))
dir.create(VOLUME_PATH, recursive = TRUE, showWarnings = FALSE)


# -- Connect to Databricks (running on-cluster — use 'databricks' method) ----
sc <- spark_connect(method = "databricks")


# -- Load and aggregate ERA5 monthly data -------------------------------------
# Same daily table as scripts 01/02a/02b. Filter to 6 study countries,
# aggregate precip to monthly. pet_monthly = NA (not in daily table — SPEI
# log-logistic params require water balance; script uses precip only for gamma).
COUNTRY_FILTER <- c("Argentina", "Benin", "Thailand",
                    "Greece", "Mexico", "South Africa")

era5_raw <- tbl(sc, in_catalog(CATALOG, SCHEMA, TABLE))

monthly_r <- era5_raw %>%
  filter(country %in% COUNTRY_FILTER) %>%
  group_by(x, y, country, year, month) %>%
  summarise(
    monthly_precip = sum(precip,           na.rm = TRUE),
    n_days_missing = sum(as.integer(is.na(precip))),
    .groups        = "drop"
  ) %>%
  filter(n_days_missing < 5) %>%
  collect()

monthly_r <- monthly_r %>% arrange(x, y, year, month)

cat(sprintf("Rows   : %d\n", nrow(monthly_r)))
cat(sprintf("Pixels : %d\n", n_distinct(paste(monthly_r$x, monthly_r$y))))
cat(sprintf("Years  : %d to %d\n", min(monthly_r$year), max(monthly_r$year)))



# =============================================================================
# FUNCTION — extract_gamma_params
# Fits Gamma α/β per pixel × scale × calendar month on calibration period only.
# Returns data.frame with one row per (pixel, scale, month).
# =============================================================================

extract_gamma_params <- function(pixel_df, calib_end = CALIB_END) {

  px      <- pixel_df$x[1]
  py      <- pixel_df$y[1]
  country <- if ("country" %in% names(pixel_df)) pixel_df$country[1] else NA_character_

  pixel_df   <- pixel_df %>% arrange(year, month)
  precip_vec <- pixel_df$monthly_precip
  years_vec  <- pixel_df$year
  calib_idx  <- years_vec >= CALIB_START & years_vec <= calib_end

  results <- list()

  for (sc_val in c(1, 3, 6, 12)) {

    min_obs <- max(sc_val * 4, 24)
    if (sum(!is.na(precip_vec[calib_idx])) < min_obs) next

    precip_calib             <- precip_vec
    precip_calib[!calib_idx] <- NA

    tryCatch({
      fit <- spi(
        precip_calib,
        scale        = sc_val,
        na.rm        = TRUE,
        fit          = "ub-pwm",
        distribution = "Gamma"
      )

      coef_array <- fit$coefficients   # 2 × 1 × 12 array
      results[[length(results) + 1]] <- data.frame(
        alpha          = as.numeric(coef_array["alpha", 1, ]),
        beta           = as.numeric(coef_array["beta",  1, ]),
        calendar_month = 1:12,
        spi_scale      = sc_val,
        x              = px,
        y              = py,
        country        = country
      )
    }, error = function(e) {
      cat(sprintf("  Gamma error x=%.3f y=%.3f scale=%d: %s\n", px, py, sc_val, e$message))
    })
  }

  if (length(results) == 0) return(data.frame())
  do.call(rbind, results)
}


# =============================================================================
# FUNCTION — extract_loglogistic_params
# Fits log-logistic ξ/α/κ per pixel × scale × calendar month (SPEI).
# Returns data.frame with one row per (pixel, scale, month).
# =============================================================================

extract_loglogistic_params <- function(pixel_df, calib_end = CALIB_END) {

  px      <- pixel_df$x[1]
  py      <- pixel_df$y[1]
  country <- if ("country" %in% names(pixel_df)) pixel_df$country[1] else NA_character_

  pixel_df <- pixel_df %>% arrange(year, month)
  years_vec <- pixel_df$year
  calib_idx <- years_vec >= CALIB_START & years_vec <= calib_end

  # SPEI = P - PET (climatic water balance)
  if (!"pet_monthly" %in% names(pixel_df)) {
    cat(sprintf("  No pet_monthly column for x=%.3f y=%.3f — skipping SPEI params\n", px, py))
    return(data.frame())
  }

  wb_vec <- pixel_df$monthly_precip - pixel_df$pet_monthly

  results <- list()

  for (sc_val in c(1, 3, 6, 12)) {

    min_obs <- max(sc_val * 4, 24)
    if (sum(!is.na(wb_vec[calib_idx])) < min_obs) next

    wb_calib             <- wb_vec
    wb_calib[!calib_idx] <- NA

    tryCatch({
      fit <- spei(
        wb_calib,
        scale        = sc_val,
        na.rm        = TRUE,
        fit          = "ub-pwm",
        distribution = "log-Logistic"
      )

      coef_array <- fit$coefficients   # 3 × 1 × 12 array (xi, alpha, kappa)
      results[[length(results) + 1]] <- data.frame(
        xi             = as.numeric(coef_array["xi",    1, ]),
        alpha          = as.numeric(coef_array["alpha", 1, ]),
        kappa          = as.numeric(coef_array["kappa", 1, ]),
        calendar_month = 1:12,
        spei_scale     = sc_val,
        x              = px,
        y              = py,
        country        = country
      )
    }, error = function(e) {
      cat(sprintf("  LogLogistic error x=%.3f y=%.3f scale=%d: %s\n", px, py, sc_val, e$message))
    })
  }

  if (length(results) == 0) return(data.frame())
  do.call(rbind, results)
}


# =============================================================================
# FUNCTION — apply_gamma_params
# Applies frozen Gamma params to full precipitation series (any period).
# =============================================================================

apply_gamma_params <- function(pixel_df, params_all) {

  px <- pixel_df$x[1]
  py <- pixel_df$y[1]

  pixel_df     <- pixel_df %>% arrange(year, month)
  precip_vec   <- pixel_df$monthly_precip
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

    if (sum(!is.na(precip_vec)) < min_obs) {
      pixel_df[[col_name]] <- NA_real_
      next
    }

    scale_params <- pixel_params %>%
      filter(spi_scale == sc_val) %>%
      arrange(calendar_month)

    if (nrow(scale_params) < 12) {
      pixel_df[[col_name]] <- NA_real_
      next
    }

    coef_array           <- array(NA, dim = c(2, 1, 12))
    coef_array[1, 1, ]   <- scale_params$alpha
    coef_array[2, 1, ]   <- scale_params$beta
    dimnames(coef_array) <- list(c("alpha", "beta"), NULL, NULL)

    tryCatch({
      out      <- spi(precip_vec, scale = sc_val, na.rm = TRUE,
                      fit = "ub-pwm", distribution = "Gamma",
                      params = coef_array)
      spi_vals <- as.numeric(out$fitted)

      if (sc_val > 1) spi_vals[1:(sc_val - 1)] <- NA_real_
      spi_vals[is.infinite(spi_vals) & spi_vals < 0] <- -3.09
      spi_vals[is.infinite(spi_vals) & spi_vals > 0] <-  3.09
      spi_vals[is.nan(spi_vals)] <- NA_real_

      pixel_df[[col_name]] <- spi_vals
    }, error = function(e) {
      cat(sprintf("  Apply Gamma error x=%.3f y=%.3f scale=%d: %s\n",
                  px, py, sc_val, e$message))
      pixel_df[[col_name]] <- NA_real_
    })
  }

  return(pixel_df)
}


# =============================================================================
# FUNCTION — apply_loglogistic_params
# Applies frozen log-logistic params to full water-balance series (any period).
# =============================================================================

apply_loglogistic_params <- function(pixel_df, params_all) {

  px <- pixel_df$x[1]
  py <- pixel_df$y[1]

  pixel_df     <- pixel_df %>% arrange(year, month)
  pixel_params <- params_all %>% filter(x == px, y == py)

  if (!"pet_monthly" %in% names(pixel_df)) {
    pixel_df$spei1 <- NA_real_
    pixel_df$spei3 <- NA_real_
    pixel_df$spei6 <- NA_real_
    return(pixel_df)
  }

  wb_vec <- pixel_df$monthly_precip - pixel_df$pet_monthly

  if (nrow(pixel_params) == 0) {
    pixel_df$spei1 <- NA_real_
    pixel_df$spei3 <- NA_real_
    pixel_df$spei6 <- NA_real_
    return(pixel_df)
  }

  for (sc_val in c(1, 3, 6)) {

    col_name    <- paste0("spei", sc_val)
    min_obs     <- max(sc_val * 4, 24)

    if (sum(!is.na(wb_vec)) < min_obs) {
      pixel_df[[col_name]] <- NA_real_
      next
    }

    scale_params <- pixel_params %>%
      filter(spei_scale == sc_val) %>%
      arrange(calendar_month)

    if (nrow(scale_params) < 12) {
      pixel_df[[col_name]] <- NA_real_
      next
    }

    coef_array           <- array(NA, dim = c(3, 1, 12))
    coef_array[1, 1, ]   <- scale_params$xi
    coef_array[2, 1, ]   <- scale_params$alpha
    coef_array[3, 1, ]   <- scale_params$kappa
    dimnames(coef_array) <- list(c("xi", "alpha", "kappa"), NULL, NULL)

    tryCatch({
      out       <- spei(wb_vec, scale = sc_val, na.rm = TRUE,
                        fit = "ub-pwm", distribution = "log-Logistic",
                        params = coef_array)
      spei_vals <- as.numeric(out$fitted)

      if (sc_val > 1) spei_vals[1:(sc_val - 1)] <- NA_real_
      spei_vals[is.infinite(spei_vals) & spei_vals < 0] <- -3.09
      spei_vals[is.infinite(spei_vals) & spei_vals > 0] <-  3.09
      spei_vals[is.nan(spei_vals)] <- NA_real_

      pixel_df[[col_name]] <- spei_vals
    }, error = function(e) {
      cat(sprintf("  Apply LogLogistic error x=%.3f y=%.3f scale=%d: %s\n",
                  px, py, sc_val, e$message))
      pixel_df[[col_name]] <- NA_real_
    })
  }

  return(pixel_df)
}


# =============================================================================
# STEP 1 — Extract Gamma params (SPI) from reference period 1970–1999
# =============================================================================

pixels <- monthly_r %>% select(x, y) %>% distinct()
cat(sprintf("\nExtracting Gamma params for %d pixels...\n", nrow(pixels)))

era5_gamma_params <- map_dfr(seq_len(nrow(pixels)), function(i) {
  if (i %% 50 == 0) cat(sprintf("  Pixel %d / %d\n", i, nrow(pixels)))
  pixel_ts <- monthly_r %>%
    filter(x == pixels$x[i], y == pixels$y[i]) %>%
    arrange(year, month)
  extract_gamma_params(pixel_ts, calib_end = CALIB_END)
})

cat(sprintf("Gamma params: %d rows | %d pixels covered\n",
            nrow(era5_gamma_params),
            n_distinct(paste(era5_gamma_params$x, era5_gamma_params$y))))

saveRDS(era5_gamma_params, file.path(VOLUME_PATH, "era5_gamma_params_ref1973_2002.rds"))
message("Saved: era5_gamma_params_ref1973_2002.rds")


# =============================================================================
# STEP 2 — Extract log-logistic params (SPEI) from reference period 1970–1999
# =============================================================================

cat(sprintf("\nExtracting log-logistic params for %d pixels...\n", nrow(pixels)))

era5_loglogistic_params <- map_dfr(seq_len(nrow(pixels)), function(i) {
  if (i %% 50 == 0) cat(sprintf("  Pixel %d / %d\n", i, nrow(pixels)))
  pixel_ts <- monthly_r %>%
    filter(x == pixels$x[i], y == pixels$y[i]) %>%
    arrange(year, month)
  extract_loglogistic_params(pixel_ts, calib_end = CALIB_END)
})

cat(sprintf("Log-logistic params: %d rows | %d pixels covered\n",
            nrow(era5_loglogistic_params),
            n_distinct(paste(era5_loglogistic_params$x, era5_loglogistic_params$y))))

saveRDS(era5_loglogistic_params, file.path(VOLUME_PATH, "era5_loglogistic_params_ref1973_2002.rds"))
message("Saved: era5_loglogistic_params_ref1973_2002.rds")


# =============================================================================
# STEP 3 — Apply frozen Gamma params to full ERA5 series
# =============================================================================
# Per-pixel disk-write pattern (same as script 03) — avoids OOM from
# map_dfr accumulating 10,993 pixel results simultaneously in RAM.
# Each pixel written to spi_tmp/px_<hash>.rds, combined after loop.
# =============================================================================

SPI_TMP <- file.path(VOLUME_PATH, "spi_era5_tmp")
dir.create(SPI_TMP, showWarnings = FALSE, recursive = TRUE)

cat(sprintf("\nApplying frozen Gamma params to full ERA5 series (%d pixels)...\n",
            nrow(pixels)))

for (i in seq_len(nrow(pixels))) {
  px_key  <- sprintf("px_%s_%s", gsub("\\.", "p", pixels$x[i]),
                                  gsub("\\.", "p", pixels$y[i]))
  out_f   <- file.path(SPI_TMP, paste0(px_key, ".rds"))
  if (file.exists(out_f)) next   # skip-if-exists for resume safety

  pixel_ts <- monthly_r[monthly_r$x == pixels$x[i] &
                           monthly_r$y == pixels$y[i], ]
  pixel_ts <- pixel_ts[order(pixel_ts$year, pixel_ts$month), ]

  result <- tryCatch(
    apply_gamma_params(pixel_ts, era5_gamma_params),
    error = function(e) {
      cat(sprintf("  Gamma apply error px %d: %s\n", i, e$message))
      NULL
    }
  )

  if (!is.null(result) && nrow(result) > 0) {
    saveRDS(result, out_f)
  }

  if (i %% 500 == 0) {
    done <- length(list.files(SPI_TMP, pattern = "\\.rds$"))
    cat(sprintf("  SPI apply: %d / %d pixels written\n", done, nrow(pixels)))
    gc(verbose = FALSE)
  }
}

cat("  Combining SPI pixel files...\n")
spi_files <- list.files(SPI_TMP, pattern = "\\.rds$", full.names = TRUE)
spi_era5  <- do.call(rbind, lapply(spi_files, readRDS))
spi_era5$date <- as.Date(sprintf("%d-%02d-01", spi_era5$year, spi_era5$month))
spi_era5$period <- ifelse(spi_era5$year <= CALIB_END,
                           sprintf("%d-%d reference", CALIB_START, CALIB_END),
                           sprintf("%d+ evaluation",  CALIB_END + 1))

saveRDS(spi_era5, file.path(VOLUME_PATH, "spi_era5_reference_period.rds"))
message("Saved: spi_era5_reference_period.rds")

# Clean up tmp files
unlink(SPI_TMP, recursive = TRUE)


# =============================================================================
# STEP 4 — Apply frozen log-logistic params to full ERA5 series
# =============================================================================
# Note: pet_monthly is NA in ERA5 daily table — SPEI water balance (P - PET)
# will be NA for all pixels. apply_loglogistic_params() handles this gracefully
# (returns data.frame with spei1/3/6 = NA). Step runs for completeness and to
# produce the output file structure needed in Phase 3 when PET data is available.
# =============================================================================

SPEI_TMP <- file.path(VOLUME_PATH, "spei_era5_tmp")
dir.create(SPEI_TMP, showWarnings = FALSE, recursive = TRUE)

cat(sprintf("\nApplying frozen log-logistic params to full ERA5 series (%d pixels)...\n",
            nrow(pixels)))

for (i in seq_len(nrow(pixels))) {
  px_key  <- sprintf("px_%s_%s", gsub("\\.", "p", pixels$x[i]),
                                  gsub("\\.", "p", pixels$y[i]))
  out_f   <- file.path(SPEI_TMP, paste0(px_key, ".rds"))
  if (file.exists(out_f)) next

  pixel_ts <- monthly_r[monthly_r$x == pixels$x[i] &
                            monthly_r$y == pixels$y[i], ]
  pixel_ts <- pixel_ts[order(pixel_ts$year, pixel_ts$month), ]

  result <- tryCatch(
    apply_loglogistic_params(pixel_ts, era5_loglogistic_params),
    error = function(e) {
      cat(sprintf("  LogLogistic apply error px %d: %s\n", i, e$message))
      NULL
    }
  )

  if (!is.null(result) && nrow(result) > 0) {
    saveRDS(result, out_f)
  }

  if (i %% 500 == 0) {
    done <- length(list.files(SPEI_TMP, pattern = "\\.rds$"))
    cat(sprintf("  SPEI apply: %d / %d pixels written\n", done, nrow(pixels)))
    gc(verbose = FALSE)
  }
}

cat("  Combining SPEI pixel files...\n")
spei_files <- list.files(SPEI_TMP, pattern = "\\.rds$", full.names = TRUE)
spei_era5  <- do.call(rbind, lapply(spei_files, readRDS))
spei_era5$date <- as.Date(sprintf("%d-%02d-01", spei_era5$year, spei_era5$month))
spei_era5$period <- ifelse(spei_era5$year <= CALIB_END,
                            sprintf("%d-%d reference", CALIB_START, CALIB_END),
                            sprintf("%d+ evaluation",  CALIB_END + 1))

saveRDS(spei_era5, file.path(VOLUME_PATH, "spei_era5_reference_period.rds"))
message("Saved: spei_era5_reference_period.rds")

unlink(SPEI_TMP, recursive = TRUE)


# =============================================================================
# STEP 5 — Validate: mean ≈ 0, sd ≈ 1 in reference period
# =============================================================================

cat("\n=== VALIDATION — reference period only ===\n")
cat("Expected: mean ≈ 0, sd ≈ 1 (correct standardisation)\n\n")

spi_era5 %>%
  filter(year >= CALIB_START, year <= CALIB_END) %>%
  summarise(
    n          = n(),
    mean_spi1  = round(mean(spi1, na.rm = TRUE), 3),
    sd_spi1    = round(sd(spi1,   na.rm = TRUE), 3),
    mean_spi3  = round(mean(spi3, na.rm = TRUE), 3),
    sd_spi3    = round(sd(spi3,   na.rm = TRUE), 3),
    mean_spi6  = round(mean(spi6, na.rm = TRUE), 3),
    sd_spi6    = round(sd(spi6,   na.rm = TRUE), 3),
    pct_na     = round(100 * mean(is.na(spi1)), 1)
  ) %>%
  print()

message("Script 05 complete — reference period params frozen and applied.")
message("Outputs: era5_gamma_params_ref1973_2002.rds | era5_loglogistic_params_ref1973_2002.rds")
message("         spi_era5_reference_period.rds | spei_era5_reference_period.rds")
