# =============================================================================
# 04. Sanity Checks — Drought Detection Algorithm Verification
# =============================================================================
# Analysis Plan Reference: Phase 1 1.3 — Algorithm sanity checks (5 checks)
#
# OOM fix: streams through cfg_*.rds one file at a time instead of loading
# all_drought_events.rds (1500 MB, OOMs on i3.xlarge). Each cfg file is ~20 MB.
# Peak RAM = one cfg file + spi_results + small accumulators.
#
# Checks:
#   1. Visual event inspection — sample events plotted as time series
#   2. Extreme duration check  — top 1% durations saved to CSV
#   3. Known drought cross-check — documented drought years verified
#   4. Zero-output pixel check  — pixels with no events mapped
#   5. Symmetric threshold test — inline detect_droughts() on one pixel
#
# Inputs (VOLUME_PATH):
#   all_drought_events.parquet — all detection events (Script 03_v2), split
#                                by config_id in-script
#   spi_results.parquet        — SPI time series (Script 01_v2)
#
# Outputs (VOLUME_PATH/Figures):
#   sanity_check1_events_sample.png
#   sanity_check2_extreme_durations.csv
#   sanity_check4_zero_pixels_map.png
#   sanity_check5_symmetric_test.rds
# =============================================================================


# -- Libraries ----------------------------------------------------------------
# terra 1.7-78 is pinned in the cluster init script (latest fails on the image's
# old Rcpp). rnaturalearth, however, only installs reliably on the running driver
# — at boot the init Rscript dies before reaching it. So bootstrap it here, on
# the driver, where it provably builds (terra + units are already present).
if (!requireNamespace("rnaturalearth", quietly = TRUE)) {
  install.packages("rnaturalearth",
    repos = "https://packagemanager.posit.co/cran/__linux__/noble/latest")
}

# sf before terra — prevents namespace masking crash on Databricks
library(dplyr)
library(ggplot2)
library(purrr)
library(patchwork)
library(lubridate)
library(sf)
library(terra)
library(rnaturalearth)
library(rnaturalearthdata)
library(arrow)   # read v2 .parquet inputs


# -- Paths --------------------------------------------------------------------
# v2 pipeline writes .parquet to drought_outputs/v2.
VOLUME_PATH   <- "/Volumes/projects/gwsc_drought/drought_outputs/v2"
FIGURES_PATH  <- file.path(VOLUME_PATH, "Figures")
LOG_PATH      <- file.path(VOLUME_PATH, "script04_error.txt")

dir.create(FIGURES_PATH, showWarnings = FALSE, recursive = TRUE)


# -- Helpers ------------------------------------------------------------------
.log <- function(msg) {
  line <- paste0("[", Sys.time(), "] ", msg, "\n")
  tryCatch(cat(line, file = LOG_PATH, append = TRUE), error = function(e) NULL)
  message(trimws(msg))
}

.log_check_error <- function(check_name, e) {
  .log(paste0(check_name, " FAILED: ", conditionMessage(e)))
}

# Write startup marker — if this exists after a crash, crash was in data load
writeLines(paste0("[", Sys.time(), "] Script 04 started"), LOG_PATH)


# -- Discover detection output ------------------------------------------------
# v2 distributed detection (03_v2) writes a SINGLE all_drought_events.parquet
# (all configs), not per-config cfg_*.parquet files — the cfg streaming pattern
# was a v1 OOM workaround. We read the one parquet and split it by config_id so
# the per-config streaming loop below is unchanged. KEEP_COLS is pushed down to
# arrow so only needed columns are materialised (keeps peak RAM low).
EVENTS_PARQUET <- file.path(VOLUME_PATH, "all_drought_events.parquet")
if (!file.exists(EVENTS_PARQUET))
  stop("all_drought_events.parquet not found — script 03_v2 may not have run.")


# -- Load SPI data (needed for Check 1 and Check 5) --------------------------
.log("Loading spi_results.parquet...")
spi_data <- as.data.frame(arrow::read_parquet(file.path(VOLUME_PATH, "spi_results.parquet")))
.log(sprintf("spi_data: %d rows x %d cols", nrow(spi_data), ncol(spi_data)))


# =============================================================================
# SINGLE STREAMING PASS — collect everything needed for checks 1–4
# in one loop through the 75 cfg files. Peak RAM = one cfg + spi_data.
# =============================================================================

KEEP_COLS <- c("x", "y", "country", "config_id", "index", "method",
               "start_thr", "start_date", "end_date", "duration_months",
               "peak_index", "severity_class", "event_year")

# Accumulators
sample_pool      <- list()   # Check 1: small pool of candidate events
extreme_list     <- list()   # Check 2: events above running 99th pct
check3_list      <- list()   # Check 3: known-drought year results
active_pixel_set <- character(0)  # Check 4: keys of pixels with >=1 event
all_pixel_set    <- character(0)  # Check 4: all pixel keys (from spi_data)
n_total_events   <- 0L

# Known droughts for Check 3
known_droughts <- data.frame(
  country    = c("Thailand", "Argentina", "South Africa", "Benin", "Mexico", "Greece"),
  drought_yr = c(1979,        1995,         1992,           1983,    1982,     1989),
  stringsAsFactors = FALSE
)

# All pixel keys from spi_data (computed once, small)
all_pixel_set <- unique(paste(spi_data$x, spi_data$y))
.log(sprintf("Total SPI pixels: %d", length(all_pixel_set)))

.log("Reading all_drought_events.parquet (column pushdown)...")

# Read once, only the columns the checks need. Then split by config_id so the
# loop streams one config at a time (peak RAM = one config chunk + spi_data).
events_tbl  <- arrow::read_parquet(EVENTS_PARQUET, as_data_frame = FALSE)
present_cols <- KEEP_COLS[KEEP_COLS %in% names(events_tbl)]
events_df   <- as.data.frame(dplyr::select(events_tbl, dplyr::all_of(present_cols)))
rm(events_tbl); gc(verbose = FALSE)

if (!"config_id" %in% names(events_df))
  stop("all_drought_events.parquet missing config_id column.")

cfg_splits <- split(events_df, events_df$config_id)
rm(events_df); gc(verbose = FALSE)
.log(sprintf("Found %d configs in all_drought_events.parquet", length(cfg_splits)))

set.seed(42)

for (cfg in cfg_splits) {

  if (is.null(cfg) || nrow(cfg) == 0) next

  # Subset to needed columns only (already pushed down, but keep for safety)
  present <- KEEP_COLS[KEEP_COLS %in% names(cfg)]
  cfg     <- cfg[, present, drop = FALSE]

  n_total_events <- n_total_events + nrow(cfg)

  # Check 1: reservoir sample — keep up to 2 events per cfg file
  if (nrow(cfg) > 0) {
    take <- cfg[sample(nrow(cfg), min(2, nrow(cfg))), , drop = FALSE]
    sample_pool[[length(sample_pool) + 1]] <- take
  }

  # Check 2: accumulate rows at/above 99th pct duration per file
  if ("duration_months" %in% names(cfg)) {
    q99 <- quantile(cfg$duration_months, 0.99, na.rm = TRUE)
    extreme_list[[length(extreme_list) + 1]] <- cfg[cfg$duration_months >= q99, , drop = FALSE]
  }

  # Check 3: filter to spi6 / threshold / -1.0, known drought years
  if (all(c("country", "event_year", "index", "method", "start_thr") %in% names(cfg))) {
    hits <- cfg[
      !is.na(cfg$index)       & cfg$index       == "spi6"      &
      !is.na(cfg$method)      & cfg$method       == "threshold" &
      !is.na(cfg$start_thr)   & cfg$start_thr    == -1.0       &
      !is.na(cfg$country)     &
      !is.na(cfg$event_year)  &
      paste(cfg$country, cfg$event_year) %in%
        paste(known_droughts$country, known_droughts$drought_yr), ]
    if (nrow(hits) > 0)
      check3_list[[length(check3_list) + 1]] <- hits[, c("x", "y", "country", "event_year"), drop = FALSE]
  }

  # Check 4: add pixel keys from this cfg to active set
  if (all(c("x", "y") %in% names(cfg)))
    active_pixel_set <- union(active_pixel_set, paste(cfg$x, cfg$y))

  rm(cfg); gc(verbose = FALSE)
}

.log(sprintf("Pass complete. Total events across all cfgs: %d", n_total_events))
.log(sprintf("Active pixels (>=1 event): %d of %d", length(active_pixel_set), length(all_pixel_set)))


# =============================================================================
# CHECK 1: Visual event inspection
# =============================================================================
message("\n=== Check 1: Visual event inspection ===")

tryCatch({

  # Flatten reservoir sample to max 6 events
  pool <- do.call(rbind, sample_pool)
  pool <- pool[!is.na(pool$x) & !is.na(pool$y) & !is.na(pool$config_id), ]
  sample_events <- pool[sample(nrow(pool), min(6, nrow(pool))), ]

  if ("start_date" %in% names(sample_events))
    sample_events$start_date <- as.Date(sample_events$start_date)
  if ("end_date" %in% names(sample_events))
    sample_events$end_date <- as.Date(sample_events$end_date)

  plots <- purrr::map(seq_len(nrow(sample_events)), function(i) {
    ev    <- sample_events[i, ]
    px    <- ev$x; py <- ev$y; idx <- ev$index
    thr   <- ev$start_thr; cfgid <- ev$config_id

    pixel_ts <- spi_data[spi_data$x == px & spi_data$y == py, ]
    pixel_ts  <- pixel_ts[order(pixel_ts$year, pixel_ts$month), ]

    if (!idx %in% names(pixel_ts)) {
      message(sprintf("  Skipping event %d — index '%s' not in spi_data", i, idx))
      return(NULL)
    }

    pixel_ts$date      <- as.Date(paste(pixel_ts$year, pixel_ts$month, "01", sep = "-"))
    pixel_ts$index_val <- pixel_ts[[idx]]

    window_start <- ev$start_date %m-% months(12)
    window_end   <- ev$end_date   %m+% months(12)
    plot_ts <- pixel_ts[pixel_ts$date >= window_start & pixel_ts$date <= window_end, ]
    if (nrow(plot_ts) < 3) return(NULL)

    ggplot(plot_ts, aes(x = date, y = index_val)) +
      geom_col(aes(fill = index_val < thr), show.legend = FALSE) +
      scale_fill_manual(values = c("FALSE" = "steelblue", "TRUE" = "firebrick")) +
      geom_hline(yintercept = thr, linetype = "dashed", colour = "red", linewidth = 0.8) +
      geom_vline(xintercept = as.numeric(ev$start_date),
                 colour = "darkred", linetype = "solid", linewidth = 0.8) +
      geom_vline(xintercept = as.numeric(ev$end_date),
                 colour = "darkred", linetype = "dotted", linewidth = 0.8) +
      labs(title = sprintf("Config: %s | Pixel: (%.3f, %.3f)", cfgid, px, py),
           subtitle = sprintf("Onset: %s | End: %s | Dur: %d mo | Sev: %s",
                              ev$start_date, ev$end_date,
                              ev$duration_months, ev$severity_class),
           x = NULL, y = idx) +
      theme_minimal(base_size = 10)
  })

  plots <- Filter(Negate(is.null), plots)
  if (length(plots) > 0) {
    p_combined <- patchwork::wrap_plots(plots, ncol = 2)
    ggplot2::ggsave(file.path(FIGURES_PATH, "sanity_check1_events_sample.png"),
                    p_combined, width = 14, height = 10, dpi = 150)
    .log(sprintf("Check 1: saved %d event plots", length(plots)))
  } else {
    .log("Check 1: no plots generated — check index column names match spi_data")
  }

}, error = function(e) .log_check_error("Check 1", e))


# =============================================================================
# CHECK 2: Extreme duration check
# =============================================================================
message("\n=== Check 2: Extreme duration check ===")

tryCatch({

  extreme_events <- do.call(rbind, extreme_list)
  extreme_events <- extreme_events[order(-extreme_events$duration_months), ]

  .log(sprintf("Check 2: %d extreme-duration events (top 1%% per config)", nrow(extreme_events)))

  write.csv(extreme_events,
            file.path(FIGURES_PATH, "sanity_check2_extreme_durations.csv"),
            row.names = FALSE)
  .log("Check 2: saved sanity_check2_extreme_durations.csv")
  .log("  Review: events >24 months should be inspected manually")
  print(head(extreme_events[, intersect(
    c("config_id", "index", "start_thr", "x", "y",
      "start_date", "end_date", "duration_months", "peak_index", "country"),
    names(extreme_events))], 10))

}, error = function(e) .log_check_error("Check 2", e))


# =============================================================================
# CHECK 3: Known drought cross-check
# =============================================================================
message("\n=== Check 3: Known drought cross-check ===")

tryCatch({

  if (length(check3_list) == 0) {
    .log("Check 3: no hits collected — country/event_year columns may be missing from cfg files")
  } else {
    hits_all <- do.call(rbind, check3_list)

    check3 <- purrr::map_dfr(seq_len(nrow(known_droughts)), function(i) {
      ctry <- known_droughts$country[i]
      yr   <- known_droughts$drought_yr[i]
      ev_yr <- hits_all[!is.na(hits_all$country) & hits_all$country == ctry &
                          !is.na(hits_all$event_year) & hits_all$event_year == yr, ]
      n_pixels_drought <- nrow(unique(ev_yr[, c("x", "y"), drop = FALSE]))
      data.frame(country = ctry, known_drought_yr = yr,
                 pixels_affected = n_pixels_drought, pass = n_pixels_drought > 0)
    })

    print(check3)
    failures <- check3[!check3$pass, ]
    if (nrow(failures) > 0) {
      .log(sprintf("Check 3: WARNING — %d known drought years not detected", nrow(failures)))
    } else {
      .log("Check 3: PASSED — all known drought years detected")
    }
  }

}, error = function(e) .log_check_error("Check 3", e))


# =============================================================================
# CHECK 4: Zero-output pixel check
# =============================================================================
message("\n=== Check 4: Zero-output pixel check ===")

tryCatch({

  n_zero   <- sum(!all_pixel_set %in% active_pixel_set)
  n_total  <- length(all_pixel_set)
  pct_zero <- round(100 * n_zero / n_total, 1)

  .log(sprintf("Check 4: total pixels=%d | zero-event=%d (%.1f%%)",
               n_total, n_zero, pct_zero))

  if (pct_zero > 50) {
    .log("Check 4: WARNING — >50% of pixels have zero events — possible silent failure")
  } else {
    .log("Check 4: PASSED — zero-pixel rate looks reasonable")
  }

  # Build coordinate data frame for map
  spi_coords <- unique(spi_data[, c("x", "y"), drop = FALSE])
  spi_coords$has_event <- paste(spi_coords$x, spi_coords$y) %in% active_pixel_set

  world <- ne_countries(scale = "medium", returnclass = "sf")

  p_zero <- ggplot() +
    geom_sf(data = world, fill = "grey90", colour = "white", linewidth = 0.2) +
    geom_point(data = spi_coords[!spi_coords$has_event, ],
               aes(x = x, y = y), colour = "firebrick", size = 0.3, alpha = 0.5) +
    geom_point(data = spi_coords[spi_coords$has_event, ],
               aes(x = x, y = y), colour = "steelblue", size = 0.3, alpha = 0.2) +
    labs(title = "Check 4: Zero-event pixels (red) vs pixels with events (blue)",
         subtitle = sprintf("%d of %d pixels (%.1f%%) have zero detected events",
                            n_zero, n_total, pct_zero),
         x = "Longitude", y = "Latitude") +
    coord_sf(expand = FALSE) +
    theme_minimal(base_size = 11)

  ggsave(file.path(FIGURES_PATH, "sanity_check4_zero_pixels_map.png"),
         p_zero, width = 12, height = 6, dpi = 150)
  .log("Check 4: saved sanity_check4_zero_pixels_map.png")

}, error = function(e) .log_check_error("Check 4", e))


# =============================================================================
# CHECK 5: Symmetric threshold test
# =============================================================================
message("\n=== Check 5: Symmetric threshold test ===")

tryCatch({

  # detect_droughts() copied from script 03 — sourcing script 03 would re-run
  # the full 75-config loop and overwrite all outputs.
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
      events_df <- do.call(rbind, events_list)
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
      events_df <- do.call(rbind, events_list)
    }
    events_df$severity_class <- dplyr::case_when(
      events_df$peak_index >= -1.0                                  ~ "Mild",
      events_df$peak_index >= -1.5 & events_df$peak_index < -1.0   ~ "Moderate",
      events_df$peak_index >= -2.0 & events_df$peak_index < -1.5   ~ "Severe",
      events_df$peak_index <  -2.0                                  ~ "Extreme"
    )
    return(events_df)
  }

  # Find first pixel with non-NA spi6 values
  test_px <- NULL
  px_list <- unique(spi_data[, c("x", "y"), drop = FALSE])
  for (k in seq_len(min(20, nrow(px_list)))) {
    tp <- spi_data[spi_data$x == px_list$x[k] & spi_data$y == px_list$y[k], ]
    if (sum(!is.na(tp$spi6)) >= 12) { test_px <- tp; break }
  }

  if (is.null(test_px)) {
    .log("Check 5: no pixel with sufficient spi6 data found — skipping")
  } else {
    test_px <- test_px[order(test_px$year, test_px$month), ]
    test_px$date <- as.Date(paste(test_px$year, test_px$month, "01", sep = "-"))

    sym_events <- detect_droughts(
      index_vec    = test_px$spi6,
      dates        = test_px$date,
      method       = "threshold",
      start_thr    = -1.0,
      min_duration = 1
    )

    if (nrow(sym_events) == 0) {
      .log("Check 5: no events detected in test pixel")
    } else {
      zero_dur <- sum(sym_events$duration_months == 0, na.rm = TRUE)
      max_dur  <- max(sym_events$duration_months, na.rm = TRUE)
      .log(sprintf("Check 5: n_events=%d | zero_dur=%d | max_dur=%d months",
                   nrow(sym_events), zero_dur, max_dur))
      if (zero_dur > 0) {
        .log("Check 5: WARNING — zero-duration events found — termination logic bug")
      } else if (max_dur > 120) {
        .log("Check 5: WARNING — events >10 years found — check termination condition")
      } else {
        .log("Check 5: PASSED — no zero-duration or infinite events")
      }
      saveRDS(sym_events, file.path(VOLUME_PATH, "sanity_check5_symmetric_test.rds"))
      .log("Check 5: saved sanity_check5_symmetric_test.rds")
    }
  }

}, error = function(e) .log_check_error("Check 5", e))


.log("Script 04 complete — all checks done")
message(sprintf("Error log: %s", LOG_PATH))
