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

  for (sc_val in c(1, 3, 6, 12)) {

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

  for (sc_val in c(1, 3, 6, 12)) {

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
# STEP 2 — Extract log-logistic params (SPEI) from reference period 1973–2002
# =============================================================================
# SPEI log-logistic params require water balance (P - PET). ERA5 daily table
# has no PET column. Water balance is loaded from:
#   spei_thornthwaite_results.rds (Script 02a) — Thornthwaite PET
#   spei_penman_results.rds       (Script 02b) — Penman-Monteith PET
# Both carry a water_balance column (P - PET per pixel per month).
# Params are fit separately per PET method and saved in one combined file
# with a pet_method column ("thornthwaite" | "penman").
# =============================================================================

spei_thorn <- tryCatch(
  readRDS(file.path(VOLUME_PATH, "spei_thornthwaite_results.rds")),
  error = function(e) { message("WARNING: spei_thornthwaite_results.rds not found — skipping Thornthwaite params"); NULL }
)
spei_penman <- tryCatch(
  readRDS(file.path(VOLUME_PATH, "spei_penman_results.rds")),
  error = function(e) { message("WARNING: spei_penman_results.rds not found — skipping Penman params"); NULL }
)

wb_sources <- list()
if (!is.null(spei_thorn)  && "water_balance" %in% names(spei_thorn))
  wb_sources[["thornthwaite"]] <- spei_thorn  %>% select(x, y, country, year, month, monthly_precip, water_balance)
if (!is.null(spei_penman) && "water_balance" %in% names(spei_penman))
  wb_sources[["penman"]]       <- spei_penman %>% select(x, y, country, year, month, monthly_precip, water_balance)

if (length(wb_sources) == 0) {
  message("WARNING: no water_balance source available — skipping log-logistic param extraction")
  era5_loglogistic_params <- data.frame()
} else {

  extract_ll_from_wb <- function(wb_df, pet_method_label) {
    wb_df <- wb_df %>% arrange(x, y, year, month)
    px_list <- wb_df %>% select(x, y) %>% distinct()
    cat(sprintf("  Fitting log-logistic params [%s] for %d pixels...\n",
                pet_method_label, nrow(px_list)))

    map_dfr(seq_len(nrow(px_list)), function(i) {
      if (i %% 50 == 0) cat(sprintf("    Pixel %d / %d\n", i, nrow(px_list)))
      pixel_ts <- wb_df %>%
        filter(x == px_list$x[i], y == px_list$y[i]) %>%
        arrange(year, month)

      # Rename water_balance → pet_monthly so extract_loglogistic_params() works unchanged
      pixel_ts$pet_monthly <- pixel_ts$monthly_precip - pixel_ts$water_balance

      result <- extract_loglogistic_params(pixel_ts, calib_end = CALIB_END)
      if (nrow(result) > 0) result$pet_method <- pet_method_label
      result
    })
  }

  ll_parts <- mapply(extract_ll_from_wb, wb_sources, names(wb_sources),
                     SIMPLIFY = FALSE)
  era5_loglogistic_params <- dplyr::bind_rows(ll_parts)
}

cat(sprintf("Log-logistic params: %d rows | %d pixels covered\n",
            nrow(era5_loglogistic_params),
            if (nrow(era5_loglogistic_params) > 0)
              n_distinct(paste(era5_loglogistic_params$x, era5_loglogistic_params$y))
            else 0L))

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
if (dir.exists(SPI_TMP)) unlink(SPI_TMP, recursive = TRUE)
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
spi_era5  <- dplyr::bind_rows(lapply(spi_files, readRDS))
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
# Water balance sourced from scripts 02a/02b outputs (same wb_sources built
# in Step 2). One output file produced per PET method, then combined.
# pixels with no matching WB source get spei1/3/6 = NA.
# =============================================================================

apply_ll_from_wb_source <- function(wb_df, pet_method_label, ll_params_all) {

  ll_params <- ll_params_all[ll_params_all$pet_method == pet_method_label, ]
  if (nrow(ll_params) == 0) {
    message(sprintf("  No log-logistic params for pet_method=%s — skipping apply", pet_method_label))
    return(data.frame())
  }

  wb_df  <- wb_df %>% arrange(x, y, year, month)
  px_list <- wb_df %>% select(x, y) %>% distinct()

  SPEI_TMP_M <- file.path(VOLUME_PATH, paste0("spei_era5_tmp_", pet_method_label))
  if (dir.exists(SPEI_TMP_M)) unlink(SPEI_TMP_M, recursive = TRUE)
  dir.create(SPEI_TMP_M, showWarnings = FALSE, recursive = TRUE)

  cat(sprintf("\nApplying log-logistic params [%s] to full ERA5 series (%d pixels)...\n",
              pet_method_label, nrow(px_list)))

  for (i in seq_len(nrow(px_list))) {
    px_key <- sprintf("px_%s_%s", gsub("\\.", "p", px_list$x[i]),
                                   gsub("\\.", "p", px_list$y[i]))
    out_f  <- file.path(SPEI_TMP_M, paste0(px_key, ".rds"))
    if (file.exists(out_f)) next

    pixel_ts <- wb_df[wb_df$x == px_list$x[i] & wb_df$y == px_list$y[i], ]
    pixel_ts <- pixel_ts[order(pixel_ts$year, pixel_ts$month), ]
    # Reconstruct pet_monthly so apply_loglogistic_params() can compute wb
    pixel_ts$pet_monthly <- pixel_ts$monthly_precip - pixel_ts$water_balance

    result <- tryCatch(
      apply_loglogistic_params(pixel_ts, ll_params),
      error = function(e) {
        cat(sprintf("  LogLogistic apply error px %d [%s]: %s\n",
                    i, pet_method_label, e$message))
        NULL
      }
    )

    if (!is.null(result) && nrow(result) > 0) {
      result$pet_method <- pet_method_label
      saveRDS(result, out_f)
    }

    if (i %% 500 == 0) {
      done <- length(list.files(SPEI_TMP_M, pattern = "\\.rds$"))
      cat(sprintf("  SPEI apply [%s]: %d / %d pixels written\n",
                  pet_method_label, done, nrow(px_list)))
      gc(verbose = FALSE)
    }
  }

  cat(sprintf("  Combining SPEI pixel files [%s]...\n", pet_method_label))
  spei_files <- list.files(SPEI_TMP_M, pattern = "\\.rds$", full.names = TRUE)
  if (length(spei_files) == 0) return(data.frame())
  result_df  <- dplyr::bind_rows(lapply(spei_files, readRDS))
  unlink(SPEI_TMP_M, recursive = TRUE)
  result_df
}

if (length(wb_sources) == 0 || nrow(era5_loglogistic_params) == 0) {
  message("WARNING: no WB source or log-logistic params — spei_era5_reference_period.rds will have NA SPEI")
  spei_era5 <- monthly_r
  spei_era5$spei1 <- NA_real_
  spei_era5$spei3 <- NA_real_
  spei_era5$spei6 <- NA_real_
  spei_era5$pet_method <- NA_character_
} else {
  spei_parts <- mapply(apply_ll_from_wb_source,
                       wb_sources, names(wb_sources),
                       MoreArgs = list(ll_params_all = era5_loglogistic_params),
                       SIMPLIFY = FALSE)
  spei_era5 <- dplyr::bind_rows(spei_parts)
}

spei_era5$date   <- as.Date(sprintf("%d-%02d-01", spei_era5$year, spei_era5$month))
spei_era5$period <- ifelse(spei_era5$year <= CALIB_END,
                            sprintf("%d-%d reference", CALIB_START, CALIB_END),
                            sprintf("%d+ evaluation",  CALIB_END + 1))

saveRDS(spei_era5, file.path(VOLUME_PATH, "spei_era5_reference_period.rds"))
message("Saved: spei_era5_reference_period.rds")


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
