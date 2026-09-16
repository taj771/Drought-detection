# =============================================================================
# 00_config.R — shared config + event rule for the Q4 threshold-frequency deck
# =============================================================================
# Q4 question: why does drought EVENT FREQUENCY not fall monotonically as the
# threshold gets stricter? (It bumps up at -0.5 for SPEI, not for SPI.)
#
# Single source of truth for paths, thresholds, observation window, the demo
# pixel, and the run-length event-counting rule. Sourced by 01/02/10.
# =============================================================================

Q4 <- list(
  Y0       = 2003,                     # observation window (matches char deck)
  Y1       = 2023,                     #   and the other Q3 witnesses
  THR      = c(0.0, -0.5, -1.0, -1.5), # thresholds swept
  SCALES   = c(1, 3, 6, 12),           # accumulation periods (months)
  MINDUR   = 1,                        # min event length (months)
  DEMO_X   = 2.625,                    # demonstrative Benin pixel (SPEI bump)
  DEMO_Y   = 7.875,
  # local parquet (already downloaded — no cluster needed)
  EVENTS   = "Codes/_archive/_data/events.parquet",   # detected events (all thr)
  SPEIT    = "Codes/_archive/_data/speit.parquet",    # monthly SPEI-Thornthwaite
  SPI      = "Codes/_archive/_data/spi_results.parquet", # monthly SPI
  OUT      = "Codes/threshold_freq/out",
  FIGS     = "Codes/threshold_freq/out/figs"
)
Q4$NYR <- Q4$Y1 - Q4$Y0 + 1            # 21 years

# -- Event-counting rule ------------------------------------------------------
# A month is "below" if value < thr. Consecutive below-months form ONE event
# (run-length). Any month that rises to/above thr splits the run. NaN never
# counts as below and breaks a run. No inter-event gap-merge. This is the exact
# rule used by v2_pipeline/python/03_v2_Drought_Detection_pandas_udf.py and
# nostd_freq/00_event_rule.R. (It was also app_monitor/02_APP_Detection.R, now
# archived — v2 script 03 is the canonical implementation.)
count_events <- function(value, thr, mindur = Q4$MINDUR) {
  below <- !is.na(value) & (value < thr)
  r <- rle(below)
  sum(r$values & r$lengths >= mindur)
}

# Total below-threshold months (the monotonic quantity, for contrast).
below_months <- function(value, thr) {
  sum(!is.na(value) & (value < thr))
}
