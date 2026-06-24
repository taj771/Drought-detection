# =============================================================================
# 02_APP_Detection.R
# GWSC Drought Monitor — Drought Event Detection
# =============================================================================
# Detects drought events from index_monthly using APP_THRESHOLD.
# Runs detection separately for reference and observation periods.
# Uses the same vectorised threshold algorithm as Phase 1 script 03.
#
# Called by: 00_APP_MASTER.Rmd
#
# Requires in environment:
#   index_monthly — output of 01_APP_Index_Calculation.R
#   .index_col    — column name of the index (e.g. "spi6")
#   APP_THRESHOLD — onset threshold (e.g. -0.5)
#   REF_START, REF_END, OBS_START, OBS_END
#   APP_CONFIG_ID
#
# Produces in environment:
#   events_ref — drought events in reference period
#   events_obs — drought events in observation period
#   Both data frames have columns:
#     x, y, country, period, config_id, drought_id,
#     start_date, end_date, duration_months,
#     peak_index, total_severity
# =============================================================================

library(dplyr)
library(lubridate)


# -- Detection function (vectorised threshold method) -------------------------
# Identical algorithm to Phase 1 script 03: rle-based, no per-month loop.
detect_droughts_threshold <- function(index_vec, dates, start_thr) {

  dates <- as.Date(dates)
  valid_idx <- which(!is.na(index_vec) & !is.nan(index_vec))
  if (length(valid_idx) < 3) return(data.frame())
  iv <- index_vec[valid_idx]
  dt <- dates[valid_idx]

  below <- iv < start_thr
  if (!any(below)) return(data.frame())

  rl         <- rle(below)
  run_ends   <- cumsum(rl$lengths)
  run_starts <- c(1L, run_ends[-length(run_ends)] + 1L)
  event_runs <- which(rl$values & rl$lengths >= 1L)
  if (length(event_runs) == 0L) return(data.frame())

  events_list <- lapply(seq_along(event_runs), function(e_i) {
    r    <- event_runs[e_i]
    si   <- run_starts[r]
    ei   <- run_ends[r]
    ev_v <- iv[si:ei]
    ev_d <- dt[si:ei]
    data.frame(
      drought_id      = e_i,
      start_date      = ev_d[1L],
      end_date        = ev_d[length(ev_d)],
      duration_months = length(ev_d),
      peak_index      = if (length(ev_v) > 0) min(ev_v, na.rm = TRUE) else NA_real_,
      total_severity  = if (length(ev_v) > 0) sum(ev_v, na.rm = TRUE) else NA_real_,
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, events_list)
}


# -- Run detection per period × pixel -----------------------------------------
run_detection <- function(df, period_label, yr_start, yr_end) {

  period_data <- df %>%
    filter(year >= yr_start, year <= yr_end) %>%
    mutate(date = as.Date(sprintf("%04d-%02d-01", as.integer(year), as.integer(month)))) %>%
    arrange(x, y, date)

  pixels <- unique(period_data[, c("x", "y")])
  cat(sprintf("  Period %s: %d pixels, %d rows\n",
              period_label, nrow(pixels), nrow(period_data)))

  pixel_list <- split(period_data, paste(period_data$x, period_data$y, sep = "_"))

  events_list <- lapply(pixel_list, function(px_df) {
    if (nrow(px_df) < 6) return(NULL)

    index_vec <- px_df[[.index_col]]
    if (all(is.na(index_vec))) return(NULL)

    ev <- tryCatch(
      detect_droughts_threshold(index_vec, px_df$date, APP_THRESHOLD),
      error = function(e) NULL
    )
    if (is.null(ev) || nrow(ev) == 0) return(NULL)

    ev$x       <- px_df$x[1]
    ev$y       <- px_df$y[1]
    ev$country <- if ("country" %in% names(px_df)) px_df$country[1] else APP_COUNTRY
    ev
  })

  nonempty <- Filter(Negate(is.null), events_list)
  if (length(nonempty) == 0) {
    warning(sprintf("No drought events detected in %s period.", period_label))
    return(data.frame())
  }

  result <- do.call(rbind, nonempty)
  result$period    <- period_label
  result$config_id <- APP_CONFIG_ID
  rownames(result) <- NULL
  result
}


# -- Detect in both periods ---------------------------------------------------
cat("Running detection — reference period...\n")
events_ref <- run_detection(index_monthly, "reference", REF_START, REF_END)
cat(sprintf("  -> %s events\n", format(nrow(events_ref), big.mark = ",")))

cat("Running detection — observation period...\n")
events_obs <- run_detection(index_monthly, "observation", OBS_START, OBS_END)
cat(sprintf("  -> %s events\n", format(nrow(events_obs), big.mark = ",")))
