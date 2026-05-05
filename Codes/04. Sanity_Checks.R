# =============================================================================
# 04. Sanity Checks — Drought Detection Algorithm Verification
# =============================================================================
# Analysis Plan Reference: Phase 1 1.3 — Algorithm sanity checks (5 checks)
#
# Check 1: Visual event inspection
#   Random sample of detected events plotted as time series with threshold line,
#   onset/end markers. Confirms algorithm reads correct onset/termination months.
#
# Check 2: Extreme duration check
#   Longest 1% of events inspected. Flags potential infinite-duration bugs or
#   genuine multi-year droughts. Each case printed for manual review.
#
# Check 3: Known drought cross-check
#   Algorithm output for documented drought years verified against expected
#   spatial extent and severity. Failure = threshold or SPI/SPEI problem.
#
# Check 4: Zero-output pixel check
#   Pixels with zero detected events mapped. Should match consistently wet
#   or low-variability areas. Many zero-output pixels in drought-prone areas
#   indicates silent pipeline failure.
#
# Check 5: Symmetric threshold test
#   Run detection with start_thr == end_thr (threshold method).
#   Check for infinite events (SPI never recovers) or zero-duration events.
#
# Depends on:
#   - ./Data/all_drought_events.rds   (Script 03 output)
#   - ./Data/spi_results.rds          (Script 01 output)
#
# Outputs:
#   - ./Figures/sanity_check1_events_sample.png
#   - ./Figures/sanity_check2_extreme_durations.csv
#   - ./Figures/sanity_check3_known_droughts.png
#   - ./Figures/sanity_check4_zero_pixels_map.png
#   - ./Data/sanity_check5_symmetric_test.rds
# =============================================================================


# -- Libraries ----------------------------------------------------------------
if (!requireNamespace("patchwork", quietly = TRUE)) install.packages("patchwork")
if (!requireNamespace("rnaturalearth", quietly = TRUE)) install.packages("rnaturalearth")
if (!requireNamespace("rnaturalearthdata", quietly = TRUE)) install.packages("rnaturalearthdata")
library(dplyr)
library(ggplot2)
library(purrr)
library(patchwork)
library(lubridate)
library(terra)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)


# -- Paths --------------------------------------------------------------------
VOLUME_PATH  <- "/Volumes/projects/gwsc_drought/drought_outputs"
FIGURES_PATH <- file.path(VOLUME_PATH, "Figures")


# -- Load data ----------------------------------------------------------------
message("Loading detection outputs...")

events   <- readRDS(file.path(VOLUME_PATH, "all_drought_events.rds"))
spi_data <- readRDS(file.path(VOLUME_PATH, "spi_results.rds"))

dir.create(FIGURES_PATH, showWarnings = FALSE, recursive = TRUE)

events$start_date <- as.Date(events$start_date)
events$end_date   <- as.Date(events$end_date)


# =============================================================================
# CHECK 1: Visual event inspection
# =============================================================================
# Sample 6 random events from output. Plot pixel time series with threshold,
# onset/end markers. One plot per event.
# =============================================================================
message("\n=== Check 1: Visual event inspection ===")

set.seed(42)
sample_events <- events |>
  filter(!is.na(x), !is.na(y), !is.na(config_id)) |>
  slice_sample(n = min(6, nrow(events)))

plots <- purrr::map(seq_len(nrow(sample_events)), function(i) {

  ev    <- sample_events[i, ]
  px    <- ev$x
  py    <- ev$y
  idx   <- ev$index
  thr   <- ev$start_thr
  cfgid <- ev$config_id

  # Get pixel time series for this index
  pixel_ts <- spi_data[spi_data$x == px & spi_data$y == py, ]
  pixel_ts  <- pixel_ts[order(pixel_ts$year, pixel_ts$month), ]

  if (!idx %in% names(pixel_ts)) {
    message(sprintf("  Skipping event %d — index '%s' not in spi_data", i, idx))
    return(NULL)
  }

  pixel_ts$date      <- as.Date(paste(pixel_ts$year, pixel_ts$month, "01", sep = "-"))
  pixel_ts$index_val <- pixel_ts[[idx]]

  # Show ±12 months around event
  window_start <- ev$start_date %m-% months(12)
  window_end   <- ev$end_date   %m+% months(12)
  plot_ts <- pixel_ts[pixel_ts$date >= window_start &
                        pixel_ts$date <= window_end, ]

  if (nrow(plot_ts) < 3) return(NULL)

  ggplot(plot_ts, aes(x = date, y = index_val)) +
    geom_col(aes(fill = index_val < thr), show.legend = FALSE) +
    scale_fill_manual(values = c("FALSE" = "steelblue", "TRUE" = "firebrick")) +
    geom_hline(yintercept = thr, linetype = "dashed", colour = "red", linewidth = 0.8) +
    geom_vline(xintercept = as.numeric(ev$start_date),
               colour = "darkred", linetype = "solid", linewidth = 0.8) +
    geom_vline(xintercept = as.numeric(ev$end_date),
               colour = "darkred", linetype = "dotted", linewidth = 0.8) +
    labs(
      title = sprintf("Config: %s | Pixel: (%.3f, %.3f)", cfgid, px, py),
      subtitle = sprintf("Onset: %s | End: %s | Duration: %d months | Severity: %s",
                         ev$start_date, ev$end_date,
                         ev$duration_months, ev$severity_class),
      x = NULL, y = idx
    ) +
    theme_minimal(base_size = 10)
})

plots <- Filter(Negate(is.null), plots)

if (length(plots) > 0) {
  p_combined <- patchwork::wrap_plots(plots, ncol = 2)
  ggplot2::ggsave(file.path(FIGURES_PATH, "sanity_check1_events_sample.png"),
                  p_combined, width = 14, height = 10, dpi = 150)
  message(sprintf("  Saved %d event plots -> %s/sanity_check1_events_sample.png",
                  length(plots), FIGURES_PATH))
} else {
  message("  No plots generated — check index column names match spi_data.")
}


# =============================================================================
# CHECK 2: Extreme duration check
# =============================================================================
# Top 1% longest events. Print for manual review. Save to CSV.
# =============================================================================
message("\n=== Check 2: Extreme duration check ===")

duration_99 <- quantile(events$duration_months, 0.99, na.rm = TRUE)
extreme_events <- events |>
  filter(duration_months >= duration_99) |>
  arrange(desc(duration_months)) |>
  select(config_id, index, method, start_thr, x, y,
         start_date, end_date, duration_months, peak_index, severity_class)

message(sprintf("  99th percentile duration: %d months", duration_99))
message(sprintf("  Events at/above 99th pct: %d", nrow(extreme_events)))

if ("country" %in% names(events)) {
  extreme_events <- merge(
    extreme_events,
    unique(events[, c("x", "y", "country")]),
    by = c("x", "y"), all.x = TRUE
  )
}

write.csv(extreme_events,
          file.path(FIGURES_PATH, "sanity_check2_extreme_durations.csv"),
          row.names = FALSE)
message("  Saved -> sanity_check2_extreme_durations.csv")
message("  Review this file: very long events (>24 months) should be inspected manually.")
print(head(extreme_events, 10))


# =============================================================================
# CHECK 3: Known drought cross-check
# =============================================================================
# Check known severe drought years in study countries.
# Expected: events detected with meaningful spatial coverage.
# Failure: zero or very low coverage in known drought years/countries.
# =============================================================================
message("\n=== Check 3: Known drought cross-check ===")

# Well-documented drought years per country (from literature / GWSC records)
known_droughts <- data.frame(
  country    = c("Thailand", "Argentina", "South Africa", "Benin", "Mexico", "Greece"),
  drought_yr = c(1979,        1995,         1992,           1983,    1982,     1989),
  stringsAsFactors = FALSE
)

if (!"country" %in% names(events) || !"event_year" %in% names(events)) {
  message("  'country' or 'event_year' column missing — skipping Check 3.")
} else {
  check3 <- purrr::map_dfr(seq_len(nrow(known_droughts)), function(i) {
    ctry <- known_droughts$country[i]
    yr   <- known_droughts$drought_yr[i]

    ev_yr <- events[events$country == ctry & events$event_year == yr &
                      events$index == "spi6" & events$method == "threshold" &
                      events$start_thr == -1.0, ]

    n_pixels_total <- length(unique(paste(
      events$x[events$country == ctry], events$y[events$country == ctry])))

    n_pixels_drought <- length(unique(paste(ev_yr$x, ev_yr$y)))

    data.frame(
      country          = ctry,
      known_drought_yr = yr,
      n_events         = nrow(ev_yr),
      pixels_affected  = n_pixels_drought,
      total_pixels     = n_pixels_total,
      pct_affected     = round(100 * n_pixels_drought / max(n_pixels_total, 1), 1),
      pass             = n_pixels_drought > 0
    )
  })

  print(check3)
  failures <- check3[!check3$pass, ]
  if (nrow(failures) > 0) {
    message(sprintf("  WARNING: %d known drought years not detected:", nrow(failures)))
    print(failures[, c("country", "known_drought_yr")])
  } else {
    message("  All known drought years detected. Check 3 PASSED.")
  }
}


# =============================================================================
# CHECK 4: Zero-output pixel check
# =============================================================================
# Map pixels with zero events detected (any config).
# These should be wet/stable regions. Many zeros in dry regions = pipeline error.
# =============================================================================
message("\n=== Check 4: Zero-output pixel check ===")

all_pixels <- unique(spi_data[, c("x", "y")])
active_pixels <- unique(events[, c("x", "y")])

zero_pixels <- merge(all_pixels, active_pixels,
                     by = c("x", "y"), all.x = TRUE)
zero_pixels$has_event <- paste(zero_pixels$x, zero_pixels$y) %in%
  paste(active_pixels$x, active_pixels$y)

n_zero  <- sum(!zero_pixels$has_event)
n_total <- nrow(zero_pixels)
pct_zero <- round(100 * n_zero / n_total, 1)

message(sprintf("  Total pixels : %d", n_total))
message(sprintf("  Zero-event   : %d (%.1f%%)", n_zero, pct_zero))

if (pct_zero > 50) {
  message("  WARNING: >50%% of pixels have zero events — possible silent failure.")
} else {
  message("  Zero-pixel rate looks reasonable. Check 4 PASSED.")
}

# Map zero pixels
world <- ne_countries(scale = "medium", returnclass = "sf")

p_zero <- ggplot() +
  geom_sf(data = world, fill = "grey90", colour = "white", linewidth = 0.2) +
  geom_point(data = zero_pixels[!zero_pixels$has_event, ],
             aes(x = x, y = y), colour = "firebrick", size = 0.3, alpha = 0.5) +
  geom_point(data = zero_pixels[zero_pixels$has_event, ],
             aes(x = x, y = y), colour = "steelblue", size = 0.3, alpha = 0.2) +
  labs(title = "Check 4: Zero-event pixels (red) vs pixels with events (blue)",
       subtitle = sprintf("%d of %d pixels (%.1f%%) have zero detected events",
                          n_zero, n_total, pct_zero),
       x = "Longitude", y = "Latitude") +
  coord_sf(expand = FALSE) +
  theme_minimal(base_size = 11)

ggsave(file.path(FIGURES_PATH, "sanity_check4_zero_pixels_map.png"),
       p_zero, width = 12, height = 6, dpi = 150)
message("  Saved -> sanity_check4_zero_pixels_map.png")


# =============================================================================
# CHECK 5: Symmetric threshold test
# =============================================================================
# Re-run detection on one index/pixel with start_thr = end_thr (-1.0).
# Check: no infinite-duration events, no zero-duration events.
# Using threshold method only (cumulative end condition is different).
# =============================================================================
message("\n=== Check 5: Symmetric threshold test ===")

# detect_droughts() copied from script 03 — sourcing script 03 directly would
# re-run the entire 75-config detection loop and overwrite outputs.
detect_droughts <- function(index_vec, dates, method, start_thr, min_duration) {
  if (length(dates) != length(index_vec))
    stop("'dates' and 'index_vec' must be same length")
  dates     <- as.Date(dates)
  valid_idx <- which(!is.na(index_vec) & !is.nan(index_vec))
  if (length(valid_idx) < min_duration) return(data.frame())
  iv <- index_vec[valid_idx]
  dt <- dates[valid_idx]
  n  <- length(iv)
  if (method == "threshold") {
    below      <- iv < start_thr
    if (!any(below)) return(data.frame())
    rl         <- rle(below)
    run_ends   <- cumsum(rl$lengths)
    run_starts <- c(1L, run_ends[-length(run_ends)] + 1L)
    event_runs <- which(rl$values & rl$lengths >= min_duration)
    if (length(event_runs) == 0L) return(data.frame())
    events_list <- lapply(seq_along(event_runs), function(e_i) {
      r  <- event_runs[e_i]; si <- run_starts[r]; ei <- run_ends[r]
      ev_v <- iv[si:ei]; ev_d <- dt[si:ei]; cs <- cumsum(ev_v)
      data.frame(drought_id = e_i, start_date = ev_d[1L],
                 end_date = ev_d[length(ev_d)],
                 duration_months = length(ev_d), entry_index = ev_v[1L],
                 peak_index = min(ev_v), peak_date = ev_d[which.min(ev_v)],
                 cum_deficit_neg = sum(ev_v[ev_v < 0]),
                 total_severity = sum(ev_v), min_cum_deficit = min(cs),
                 stringsAsFactors = FALSE)
    })
    events <- do.call(rbind, events_list)
  } else {
    events_list <- list(); drought_id <- 0L; i <- 1L
    while (i <= n) {
      if (iv[i] >= start_thr) { i <- i + 1L; next }
      cs <- cumsum(iv[i:n]); r <- which(cs >= 0L)[1L]
      if (is.na(r)) {
        ev_v <- iv[i:n]; ev_d <- dt[i:n]; i <- n + 1L
      } else {
        ev_end <- i + r - 2L
        if (ev_end < i) { i <- i + r; next }
        ev_v <- iv[i:ev_end]; ev_d <- dt[i:ev_end]; i <- i + r
      }
      if (length(ev_v) == 0L) next
      drought_id <- drought_id + 1L; cs_ev <- cumsum(ev_v)
      events_list[[drought_id]] <- data.frame(
        drought_id = drought_id, start_date = ev_d[1L],
        end_date = ev_d[length(ev_d)], duration_months = length(ev_d),
        entry_index = ev_v[1L], peak_index = min(ev_v),
        peak_date = ev_d[which.min(ev_v)],
        cum_deficit_neg = sum(ev_v[ev_v < 0]), total_severity = sum(ev_v),
        min_cum_deficit = min(cs_ev), stringsAsFactors = FALSE)
    }
    if (drought_id == 0L) return(data.frame())
    events <- do.call(rbind, events_list)
  }
  events$severity_class <- dplyr::case_when(
    events$peak_index >= -1.0                              ~ "Mild",
    events$peak_index >= -1.5 & events$peak_index < -1.0 ~ "Moderate",
    events$peak_index >= -2.0 & events$peak_index < -1.5 ~ "Severe",
    events$peak_index <  -2.0                             ~ "Extreme"
  )
  return(events)
}

# Use one pixel — first available in spi_data with enough data
test_pixel <- spi_data[spi_data$x == spi_data$x[1] & spi_data$y == spi_data$y[1], ]
test_pixel <- test_pixel[order(test_pixel$year, test_pixel$month), ]
test_pixel$date <- as.Date(paste(test_pixel$year, test_pixel$month, "01", sep = "-"))

sym_events <- detect_droughts(
  index_vec    = test_pixel$spi6,
  dates        = test_pixel$date,
  method       = "threshold",
  start_thr    = -1.0,
  min_duration = 1
)

if (nrow(sym_events) == 0) {
  message("  No events detected in test pixel — try different pixel if spi6 all NA.")
} else {
  zero_dur    <- sum(sym_events$duration_months == 0, na.rm = TRUE)
  max_dur     <- max(sym_events$duration_months, na.rm = TRUE)
  n_events    <- nrow(sym_events)

  message(sprintf("  Events detected     : %d", n_events))
  message(sprintf("  Zero-duration events: %d  (should be 0)", zero_dur))
  message(sprintf("  Max duration        : %d months", max_dur))

  if (zero_dur > 0) {
    message("  WARNING: Zero-duration events found — termination logic bug.")
  } else if (max_dur > 120) {
    message("  WARNING: Events >10 years detected — check termination condition.")
  } else {
    message("  Check 5 PASSED — no zero-duration or infinite events.")
  }

  saveRDS(sym_events, file.path(VOLUME_PATH, "sanity_check5_symmetric_test.rds"))
  message("  Saved -> sanity_check5_symmetric_test.rds")
}


message("\n=== All 5 sanity checks complete ===")
message("Review outputs in ./Figures/ and ./Data/ before proceeding to Analysis 1.")
