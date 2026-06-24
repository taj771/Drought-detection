# =============================================================================
# 03_APP_Output.R
# GWSC Drought Monitor — Summary Statistics & CSV Output
# =============================================================================
# Aggregates drought characteristics per period (reference / observation),
# computes % change, and writes a single-row-per-config CSV.
#
# Called by: 00_APP_MASTER.Rmd
#
# Requires in environment:
#   events_ref, events_obs — from 02_APP_Detection.R
#   APP_COUNTRY, APP_CONFIG_ID, APP_INDEX, APP_SCALE, APP_THRESHOLD
#   REF_START, REF_END, OBS_START, OBS_END
#   OUTPUT_DIR
#
# Output CSV columns:
#   country, config_id, index, scale, threshold,
#   ref_period, obs_period,
#   ref_freq, ref_duration, ref_severity, ref_pct_pixels,
#   obs_freq, obs_duration, obs_severity, obs_pct_pixels,
#   pct_change_freq, pct_change_duration, pct_change_severity, pct_change_pct_pixels,
#   run_date
# =============================================================================

library(dplyr)


# -- Pixel counts per period --------------------------------------------------
# Need total pixel count to compute pct_pixels_affected.
# Use index_monthly (present in environment from script 01) as the pixel universe.
n_pixels <- length(unique(paste(index_monthly$x, index_monthly$y)))


# -- Summarise events per period ----------------------------------------------
summarise_period <- function(events_df, yr_start, yr_end, n_px) {

  if (is.null(events_df) || nrow(events_df) == 0) {
    return(data.frame(
      freq        = 0,
      duration    = NA_real_,
      severity    = NA_real_,
      pct_pixels  = 0
    ))
  }

  n_years <- yr_end - yr_start + 1

  # Frequency: mean events per pixel per year
  events_per_pixel <- events_df %>%
    group_by(x, y) %>%
    summarise(n_events = n(), .groups = "drop")

  freq <- sum(events_per_pixel$n_events) / (n_px * n_years)

  # Duration and severity: mean across all events (exclude Inf/-Inf from bad pixels)
  duration <- mean(events_df$duration_months[is.finite(events_df$duration_months)], na.rm = TRUE)
  severity <- mean(events_df$peak_index[is.finite(events_df$peak_index)],           na.rm = TRUE)

  # Pixels affected at least once in the period
  n_pixels_affected <- length(unique(paste(events_df$x, events_df$y)))
  pct_pixels        <- round(100 * n_pixels_affected / n_px, 2)

  data.frame(
    freq       = round(freq,     4),
    duration   = round(duration, 2),
    severity   = round(severity, 4),
    pct_pixels = pct_pixels
  )
}

cat("Summarising reference period...\n")
s_ref <- summarise_period(events_ref, REF_START, REF_END, n_pixels)

cat("Summarising observation period...\n")
s_obs <- summarise_period(events_obs, OBS_START, OBS_END, n_pixels)


# -- % change -----------------------------------------------------------------
pct_change <- function(ref_val, obs_val, flip_sign = FALSE) {
  if (is.na(ref_val) || ref_val == 0) return(NA_real_)
  ch <- round((obs_val - ref_val) / abs(ref_val) * 100, 1)
  if (flip_sign) -ch else ch
}

# severity is negative (lower = worse); flip so positive = worsening
pct_freq     <- pct_change(s_ref$freq,       s_obs$freq)
pct_duration <- pct_change(s_ref$duration,   s_obs$duration)
pct_severity <- pct_change(s_ref$severity,   s_obs$severity, flip_sign = TRUE)
pct_pixels   <- pct_change(s_ref$pct_pixels, s_obs$pct_pixels)


# -- Build output row ---------------------------------------------------------
output_row <- data.frame(
  country               = APP_COUNTRY,
  config_id             = APP_CONFIG_ID,
  index                 = APP_INDEX,
  scale                 = APP_SCALE,
  threshold             = APP_THRESHOLD,
  ref_period            = paste0(REF_START, "-", REF_END),
  obs_period            = paste0(OBS_START, "-", OBS_END),
  ref_freq              = s_ref$freq,
  ref_duration          = s_ref$duration,
  ref_severity          = s_ref$severity,
  ref_pct_pixels        = s_ref$pct_pixels,
  obs_freq              = s_obs$freq,
  obs_duration          = s_obs$duration,
  obs_severity          = s_obs$severity,
  obs_pct_pixels        = s_obs$pct_pixels,
  pct_change_freq       = pct_freq,
  pct_change_duration   = pct_duration,
  pct_change_severity   = pct_severity,
  pct_change_pct_pixels = pct_pixels,
  run_date              = Sys.Date(),
  stringsAsFactors      = FALSE
)


# -- Print summary to console -------------------------------------------------
cat("\n=== GWSC Drought Monitor — Results ===\n")
cat(sprintf("Country   : %s\n",       APP_COUNTRY))
cat(sprintf("Config    : %s\n",       APP_CONFIG_ID))
cat(sprintf("Threshold : %.2f\n",     APP_THRESHOLD))
cat(sprintf("Ref period: %d–%d\n",   REF_START, REF_END))
cat(sprintf("Obs period: %d–%d\n\n", OBS_START, OBS_END))

cat(sprintf("%-22s %10s %10s %10s\n", "Characteristic", "Ref", "Obs", "% Change"))
cat(strrep("-", 55), "\n")
cat(sprintf("%-22s %10.4f %10.4f %10.1f%%\n",
            "Freq (events/px/yr)", s_ref$freq,       s_obs$freq,       pct_freq))
cat(sprintf("%-22s %10.2f %10.2f %10.1f%%\n",
            "Duration (months)",   s_ref$duration,   s_obs$duration,   pct_duration))
cat(sprintf("%-22s %10.4f %10.4f %10.1f%%\n",
            "Peak severity",       s_ref$severity,   s_obs$severity,   pct_severity))
cat(sprintf("%-22s %10.1f%% %10.1f%% %10.1f%%\n",
            "% Pixels affected",  s_ref$pct_pixels, s_obs$pct_pixels, pct_pixels))


# -- Write CSV ----------------------------------------------------------------
dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)

out_file <- file.path(
  OUTPUT_DIR,
  sprintf("drought_monitor_%s_%s_%s.csv",
          gsub(" ", "_", APP_COUNTRY),
          APP_CONFIG_ID,
          format(Sys.Date(), "%Y%m%d"))
)

write.csv(output_row, out_file, row.names = FALSE)
cat(sprintf("\nOutput written: %s\n", out_file))
