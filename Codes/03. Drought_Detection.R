# =============================================================================
# 06. Drought Detection — Pixel-Level, All Configurations
# =============================================================================
# Analysis Plan Reference: Section 1.3 — Detecting drought events under
#   different definitions and thresholds of SPI and SPEI
#
# This script implements Steps 1–4 of Section 1.3:
#
#   Step 1: Load and prepare SPI/SPEI data
#           Load the pixel-level monthly SPI and SPEI time series produced
#           by Scripts 01, 02a, and 02b. Each row is one pixel × one month,
#           with columns spi1, spi3, spi6, spi12, spei1, spei3, spei6, spei12.
#
#   Step 2: Set drought detection rules (configuration block)
#           Define what constitutes a drought using three parameters:
#             - Index:         which SPI/SPEI column to use
#             - Method:        "threshold" — drought ends when index recovers
#                              above the start threshold
#                              "cumulative" — drought ends when cumulative sum
#                              of index values since onset returns to >= 0,
#                              allowing persistence through temporarily
#                              above-normal months. Applicable to scale-1 only.
#             - Start threshold: how low the index must drop to trigger onset
#             - Min duration:  minimum consecutive months below threshold
#                              before onset is confirmed (scale-1 only)
#           All combinations are generated automatically from the ranges set
#           in the config block below. The 45 configurations in Appendix A1
#           of the analysis plan are reproduced exactly by the default settings.
#
#   Step 3: Detect drought events at every grid cell
#           For each configuration, every pixel's time series is scanned
#           month by month. Two questions are asked per month:
#             Q1 — Is a drought starting? (index drops below start threshold,
#                  and stays below for min_duration consecutive months)
#             Q2 — When does it end? (threshold method: first month back above
#                  start threshold; cumulative method: first month where
#                  cumulative sum since onset returns to >= 0)
#           Each pixel is processed independently so its own historical
#           distribution defines what is "normal" for that location.
#
#   Step 4: Record drought events and characteristics
#           For each confirmed event, the following are recorded:
#             - Start and end dates
#             - Duration in months
#             - Entry SPI/SPEI value (value at onset month)
#             - Peak SPI/SPEI value (worst single month of the event)
#             - Cumulative SPI/SPEI deficit (sum of all negative values)
#             - Total severity (sum of all index values during event)
#             - Severity class: Mild / Moderate / Severe / Extreme
#
# Depends on:
#   - ./Data/spi_results.rds         (Script 01 output)
#   - ./Data/spei_thornthwaite_results.rds  (Script 02a output — Thornthwaite)
#   - ./Data/spei_penman_results.rds (Script 02b output — Penman-Monteith)
#     Note: if Penman results not yet available, script runs on SPI +
#     Thornthwaite SPEI only.
#
# Output:
#   - ./Data/all_drought_events.rds           — full event list, all configs
#   - ./Data/drought_summary_yearly.csv       — yearly summary per country
#   - ./Data/drought_annual_summary.csv       — one row per config per country
#   - ./Data/config_comparison_by_country.csv — config comparison table
# =============================================================================


# -- Libraries ----------------------------------------------------------------
library(dplyr)
library(lubridate)
library(parallel)


# =============================================================================
# USER SETTINGS — edit this section only
# =============================================================================

# -- Data paths ---------------------------------------------------------------
VOLUME_PATH            <- "/Volumes/projects/gwsc_drought/drought_outputs"
spi_path               <- file.path(VOLUME_PATH, "spi_results.rds")
spei_thornthwaite_path <- file.path(VOLUME_PATH, "spei_thornthwaite_results.rds")
spei_penman_path       <- file.path(VOLUME_PATH, "spei_penman_results.rds")   # set NULL if not ready

# -- Country filter -----------------------------------------------------------
# List countries to include. Set to NULL to use ALL available pixels.
# Country names must match the 'country' column in the RDS files.
COUNTRY_FILTER <- c("Argentina", "Benin", "Thailand", "Greece", "Mexico", "South Africa")
# COUNTRY_FILTER <- NULL   # ← uncomment to run on all available pixels

# -- Year range filter --------------------------------------------------------
# Full period: reference + comparison + buffer to 2023.
YEAR_FROM <- 1970
YEAR_TO   <- 2023

# Reference period for frozen params (WMO 30-year minimum; ERA5 starts 1973).
REF_START <- 1973
REF_END   <- 2002

# -- Configuration ranges -----------------------------------------------------
# These define the 75 configurations in Appendix A1 of the analysis plan.
# Edit these vectors to add/remove configurations.
#
# INDICES: which index columns to detect drought on.
#   SPI scales:  spi1, spi3, spi6, spi12
#   SPEI (Thornthwaite): spei1, spei3, spei6, spei12
#   SPEI (Penman):       spei1_pm, spei3_pm, spei6_pm, spei12_pm
#     (Penman columns only present if spei_penman_path is not NULL)
#
# METHODS: two detection methods (analysis plan Section 1.3 Step 2):
#   "threshold"  — drought ends when index recovers above start threshold
#   "cumulative" — drought ends when cumulative sum >= 0
#                  NOTE: cumulative method only valid for scale-1 indices
#                  (spi1, spei1, spei1_pm). Applied automatically below.
#
# START_THRESHOLDS: onset threshold values.
#    0.0 = Any negative anomaly entry
#   -0.25 = Near-normal drought entry
#   -0.5 = Mild drought entry
#   -1.0 = Moderate drought entry
#   -1.5 = Severe drought entry
#
# MIN_DURATION: minimum consecutive months below threshold before onset
#   confirmed. Only applies to scale-1 indices. Fixed at 1 for all others.

INDICES          <- c("spi1", "spi3", "spi6", "spi12",
                      "spei1", "spei3", "spei6", "spei12")
METHODS          <- c("threshold", "cumulative")
START_THRESHOLDS <- c(0.0, -0.25, -0.5, -1.0, -1.5)
MIN_DURATION     <- 1   # fixed at 1 per analysis plan (Appendix A1)

# -- Parallelism ---------------------------------------------------------------
# Configs processed simultaneously. Each worker inherits pixel_list via fork
# (copy-on-write). N_CORES=1 = sequential (safe); N_CORES=2 = 2x faster but
# risks OOM if pixel_list is large (previous runs OOMed with pixel-level
# mclapply — config-level is safer but test with 1 first).
N_CORES <- 1L

# =============================================================================


# -- Step 1: Load and prepare SPI/SPEI data -----------------------------------
# Analysis Plan Section 1.3 Step 1: Load pixel-level monthly SPI/SPEI values.
# Join SPI and SPEI tables on x, y, year, month.
# Optionally filter by country and year range.
message("=== Step 1: Loading index data ===\n")

spi_data  <- readRDS(spi_path)
spei_data <- readRDS(spei_thornthwaite_path)

# Join SPI and Thornthwaite SPEI on pixel coordinates + date
index_data <- merge(
  spi_data,
  spei_data[, c("x", "y", "year", "month", "spei1", "spei3", "spei6", "spei12")],
  by = c("x", "y", "year", "month"),
  all.x = TRUE
)

# Optionally join Penman SPEI if available
if (!is.null(spei_penman_path) && file.exists(spei_penman_path)) {
  spei_pm <- readRDS(spei_penman_path)
  # Rename Penman columns to avoid clash with Thornthwaite columns
  names(spei_pm)[names(spei_pm) == "spei1"]  <- "spei1_pm"
  names(spei_pm)[names(spei_pm) == "spei3"]  <- "spei3_pm"
  names(spei_pm)[names(spei_pm) == "spei6"]  <- "spei6_pm"
  names(spei_pm)[names(spei_pm) == "spei12"] <- "spei12_pm"

  index_data <- merge(
    index_data,
    spei_pm[, c("x", "y", "year", "month",
                "spei1_pm", "spei3_pm", "spei6_pm", "spei12_pm")],
    by = c("x", "y", "year", "month"),
    all.x = TRUE
  )

  # Add Penman indices to detection list
  INDICES <- c(INDICES, "spei1_pm", "spei3_pm", "spei6_pm", "spei12_pm")
  message("Penman-Monteith SPEI loaded and joined.")
} else {
  message("Penman SPEI not found — running on SPI + Thornthwaite SPEI only.")
}

# Filter to only indices that actually exist as columns
INDICES <- intersect(INDICES, names(index_data))

# Apply year filter
index_data <- index_data[index_data$year >= YEAR_FROM &
                           index_data$year <= YEAR_TO, ]

# Apply country filter (NULL = use all)
if (!is.null(COUNTRY_FILTER) && "country" %in% names(index_data)) {
  index_data <- index_data[index_data$country %in% COUNTRY_FILTER, ]
  message(sprintf("Country filter applied: %s", paste(COUNTRY_FILTER, collapse = ", ")))
} else {
  message("No country filter — using all available pixels.")
}

index_data$date <- as.Date(paste(index_data$year, index_data$month, "01", sep = "-"))
index_data      <- index_data[order(index_data$x, index_data$y, index_data$date), ]

cat(sprintf("Rows loaded  : %s\n", format(nrow(index_data), big.mark = ",")))
cat(sprintf("Unique pixels: %s\n",
            format(length(unique(paste(index_data$x, index_data$y))), big.mark = ",")))
cat(sprintf("Year range   : %d to %d\n", YEAR_FROM, YEAR_TO))
cat(sprintf("Indices      : %s\n\n", paste(INDICES, collapse = ", ")))


# -- Step 2: Build configuration table ----------------------------------------
# Analysis Plan Section 1.3 Step 2: all combinations of index × method × threshold.
# Scale-1 indices (spi1, spei1, spei1_pm) run both methods.
# Scale-3/6/12 indices run threshold method only (accumulation window already
# provides temporal persistence — cumulative not applicable).
message("=== Step 2: Building configuration table ===\n")

scale1_indices  <- intersect(c("spi1", "spei1", "spei1_pm"), INDICES)
longer_indices  <- setdiff(INDICES, scale1_indices)

# Scale-1: both methods
configs_scale1 <- expand.grid(
  index        = scale1_indices,
  method       = METHODS,
  start_thr    = START_THRESHOLDS,
  min_duration = MIN_DURATION,
  stringsAsFactors = FALSE
)

# Scale-3/6/12: threshold method only
configs_longer <- expand.grid(
  index        = longer_indices,
  method       = "threshold",
  start_thr    = START_THRESHOLDS,
  min_duration = 1,
  stringsAsFactors = FALSE
)

config_table <- rbind(configs_scale1, configs_longer)

# Auto-generate Config ID matching Appendix A1 naming convention:
# [Index prefix][Scale]-[Method code]-[Threshold x 100, 3-digit]
# e.g. S1-T-000, S1-T-025, ET3-C-050, EP6-T-100, S1-T-150
config_table$config_id <- sprintf(
  "%s-%s-%03d",
  # Index prefix
  dplyr::case_when(
    config_table$index == "spi1"     ~ "S1",
    config_table$index == "spi3"     ~ "S3",
    config_table$index == "spi6"     ~ "S6",
    config_table$index == "spi12"    ~ "S12",
    config_table$index == "spei1"    ~ "ET1",
    config_table$index == "spei3"    ~ "ET3",
    config_table$index == "spei6"    ~ "ET6",
    config_table$index == "spei12"   ~ "ET12",
    config_table$index == "spei1_pm" ~ "EP1",
    config_table$index == "spei3_pm" ~ "EP3",
    config_table$index == "spei6_pm" ~ "EP6",
    config_table$index == "spei12_pm"~ "EP12",
    TRUE ~ config_table$index
  ),
  # Method code: T = threshold, C = cumulative
  ifelse(config_table$method == "threshold", "T", "C"),
  # Threshold × 100 (absolute value), 3-digit zero-padded
  # e.g. 0.0→000, -0.25→025, -0.5→050, -1.0→100, -1.5→150
  round(abs(config_table$start_thr) * 100)
)

config_table <- config_table[order(config_table$index,
                                   config_table$method,
                                   config_table$start_thr), ]

cat(sprintf("Total configurations: %d\n\n", nrow(config_table)))
print(config_table[, c("config_id", "index", "method", "start_thr", "min_duration")],
      row.names = FALSE)


# -- Step 3: Drought detection function ---------------------------------------
# Analysis Plan Section 1.3 Step 3: scan each pixel's time series month by month.
#
# Algorithm logic:
#   - Scan forward through the time series.
#   - ONSET: confirmed when min_duration consecutive months all fall below
#     start_thr. The start date is the first of those months.
#   - END (threshold method): first month where index >= start_thr after onset.
#     That recovery month is NOT counted as part of the drought.
#   - END (cumulative method): cumulative sum of index values since onset is
#     tracked. Drought ends (and recovery month excluded) once cum_sum >= 0.
#     This allows a drought to persist through a brief wet month if the
#     preceding deficit has not yet been repaid.
#   - After each event closes, scanning continues from the month after the
#     recovery month.
#
# Returns a data frame of events (one row per event) with characteristics
# defined in analysis plan Section 1.3 Step 4.
detect_droughts <- function(index_vec, dates, method, start_thr, min_duration) {

  if (length(dates) != length(index_vec))
    stop("'dates' and 'index_vec' must be same length")

  dates <- as.Date(dates)

  # Strip NAs once upfront — avoids per-iteration is.na checks in the loop.
  valid_idx <- which(!is.na(index_vec) & !is.nan(index_vec))
  if (length(valid_idx) < min_duration) return(data.frame())
  iv <- index_vec[valid_idx]
  dt <- dates[valid_idx]
  n  <- length(iv)

  if (method == "threshold") {

    # -- Fully vectorised threshold method using run-length encoding -----------
    # Each contiguous run of months all below start_thr (length >= min_duration)
    # is one drought event.  rle() replaces the month-by-month while/for scan.
    below <- iv < start_thr
    if (!any(below)) return(data.frame())

    rl         <- rle(below)
    run_ends   <- cumsum(rl$lengths)
    run_starts <- c(1L, run_ends[-length(run_ends)] + 1L)
    event_runs <- which(rl$values & rl$lengths >= min_duration)
    if (length(event_runs) == 0L) return(data.frame())

    events_list <- lapply(seq_along(event_runs), function(e_i) {
      r    <- event_runs[e_i]
      si   <- run_starts[r]
      ei   <- run_ends[r]
      ev_v <- iv[si:ei]
      ev_d <- dt[si:ei]
      cs   <- cumsum(ev_v)
      data.frame(
        drought_id      = e_i,
        start_date      = ev_d[1L],
        end_date        = ev_d[length(ev_d)],
        duration_months = length(ev_d),
        entry_index     = ev_v[1L],
        peak_index      = min(ev_v),
        peak_date       = ev_d[which.min(ev_v)],
        cum_deficit_neg = sum(ev_v[ev_v < 0]),
        total_severity  = sum(ev_v),
        min_cum_deficit = min(cs),
        stringsAsFactors = FALSE
      )
    })
    events <- do.call(rbind, events_list)

  } else {

    # -- Semi-vectorised cumulative method ------------------------------------
    # Outer while advances to each onset; inner recovery uses cumsum() instead
    # of a per-month for loop — avoids O(n^2) iteration for long events.
    events_list <- list()
    drought_id  <- 0L
    i           <- 1L

    while (i <= n) {
      if (iv[i] >= start_thr) { i <- i + 1L; next }

      # cumsum from onset: first position where cs >= 0 is the recovery month.
      cs <- cumsum(iv[i:n])
      r  <- which(cs >= 0L)[1L]

      if (is.na(r)) {
        ev_v  <- iv[i:n];     ev_d  <- dt[i:n]
        i     <- n + 1L
      } else {
        ev_end <- i + r - 2L   # last month IN the event (recovery month excluded)
        if (ev_end < i) { i <- i + r; next }
        ev_v  <- iv[i:ev_end]; ev_d  <- dt[i:ev_end]
        i     <- i + r
      }

      if (length(ev_v) == 0L) next

      drought_id <- drought_id + 1L
      cs_ev <- cumsum(ev_v)
      events_list[[drought_id]] <- data.frame(
        drought_id      = drought_id,
        start_date      = ev_d[1L],
        end_date        = ev_d[length(ev_d)],
        duration_months = length(ev_d),
        entry_index     = ev_v[1L],
        peak_index      = min(ev_v),
        peak_date       = ev_d[which.min(ev_v)],
        cum_deficit_neg = sum(ev_v[ev_v < 0]),
        total_severity  = sum(ev_v),
        min_cum_deficit = min(cs_ev),
        stringsAsFactors = FALSE
      )
    }

    if (drought_id == 0L) return(data.frame())
    events <- do.call(rbind, events_list)
  }

  # Severity class based on peak index value (analysis plan Section 1.3 Step 4)
  events$severity_class <- dplyr::case_when(
    events$peak_index >= -1.0                              ~ "Mild",
    events$peak_index >= -1.5 & events$peak_index < -1.0 ~ "Moderate",
    events$peak_index >= -2.0 & events$peak_index < -1.5 ~ "Severe",
    events$peak_index <  -2.0                             ~ "Extreme"
  )

  return(events)
}


# -- Step 3 (continued): Loop all configs × all pixels -----------------------
# Analysis Plan Section 1.3 Step 3: process every pixel independently.
# Each pixel uses its own SPI/SPEI time series (already standardised to its
# own local historical distribution in Scripts 01/02a/02b).
message("\n=== Step 3: Running detection — all configs × all pixels ===\n")

pixels <- unique(index_data[, c("x", "y")])
pixels <- pixels[order(pixels$x, pixels$y), ]
total_pixels <- nrow(pixels)

# Pre-split index_data by pixel key once — avoids re-scanning full data frame
# for every config × pixel combination (critical for performance).
message("Pre-splitting index data by pixel...")
index_data$pixel_key <- paste(index_data$x, index_data$y, sep = "_")
pixel_list <- split(index_data, index_data$pixel_key)
pixel_list <- lapply(pixel_list, function(d) d[order(d$date), ])
message(sprintf("  %d unique pixels cached.\n", length(pixel_list)))

# Preserve pixels_per_country lookup before releasing index_data.
# index_data and pixel_list hold identical rows — keeping both doubles RAM.
if ("country" %in% names(index_data)) {
  pixels_per_country_pre <- unique(index_data[!is.na(index_data$country),
                                               c("x", "y", "country")])
}
rm(index_data)
gc()

overall_start <- Sys.time()

# Write each config result to disk immediately to avoid accumulating all 75
# configs in RAM simultaneously (causes OOM on driver node).
tmp_dir <- file.path(VOLUME_PATH, "detection_tmp")
dir.create(tmp_dir, showWarnings = FALSE, recursive = TRUE)
message(sprintf("Temp dir for config chunks: %s\n", tmp_dir))

parallel::mclapply(seq_len(nrow(config_table)), function(cfg_i) {

  cfg <- config_table[cfg_i, ]

  chunk_file <- file.path(tmp_dir, paste0("cfg_", cfg$config_id, ".rds"))
  if (file.exists(chunk_file)) {
    cat(sprintf("\n[%d/%d] SKIP (exists): %s\n", cfg_i, nrow(config_table), cfg$config_id))
    return(invisible(NULL))
  }

  cat(sprintf(
    "\n[%d/%d] Config: %s | index=%s | method=%s | thr=%.2f\n",
    cfg_i, nrow(config_table),
    cfg$config_id, cfg$index, cfg$method, cfg$start_thr
  ))

  # lapply + Filter + do.call(rbind) — faster than purrr::map_dfr for large
  # pixel lists (no per-row overhead, direct list pre-allocation).
  pixel_events_list <- lapply(seq_len(total_pixels), function(i) {

    px <- pixels$x[i]
    py <- pixels$y[i]

    pixel_ts <- pixel_list[[paste(px, py, sep = "_")]]
    if (is.null(pixel_ts) || nrow(pixel_ts) < 12) return(NULL)

    index_vec <- pixel_ts[[cfg$index]]
    if (is.null(index_vec) || all(is.na(index_vec))) return(NULL)

    events <- tryCatch(
      detect_droughts(
        index_vec    = index_vec,
        dates        = pixel_ts$date,
        method       = cfg$method,
        start_thr    = cfg$start_thr,
        min_duration = cfg$min_duration
      ),
      error = function(e) NULL
    )

    if (is.null(events) || nrow(events) == 0) return(NULL)

    events$x            <- px
    events$y            <- py
    events$config_id    <- cfg$config_id
    events$index        <- cfg$index
    events$method       <- cfg$method
    events$start_thr    <- cfg$start_thr
    events$min_duration <- cfg$min_duration

    if ("country" %in% names(pixel_ts)) {
      events$country <- pixel_ts$country[1]
    }

    events
  })

  nonempty <- Filter(Negate(is.null), pixel_events_list)
  pixel_events <- if (length(nonempty) > 0) do.call(rbind, nonempty) else data.frame()
  rm(pixel_events_list, nonempty)

  cat(sprintf("  -> %s events detected\n",
              format(nrow(pixel_events), big.mark = ",")))

  saveRDS(pixel_events, chunk_file)
  invisible(NULL)

}, mc.cores = N_CORES)

# -- Assemble chunks + compute summaries one config at a time -----------------
# Unity Catalog Volumes do NOT support append writes — fwrite(append=TRUE) removed.
# Summaries computed per-config inside loop (small RAM).
# After loop: single rbindlist pass for all_drought_events (summaries already freed).
if (!requireNamespace("data.table", quietly = TRUE)) install.packages("data.table")
library(data.table)

message("\nCombining config chunks and computing summaries (one config at a time)...")
chunk_files <- list.files(tmp_dir, pattern = "^cfg_.*\\.rds$", full.names = TRUE)

if (exists("pixels_per_country_pre")) {
  ppc <- as.data.table(pixels_per_country_pre)
} else {
  ppc <- NULL
}

yearly_list      <- vector("list", length(chunk_files))
pixel_freq_list  <- vector("list", length(chunk_files))
n_total_events   <- 0L

for (i in seq_along(chunk_files)) {
  chunk <- readRDS(chunk_files[[i]])

  if (is.null(chunk) || nrow(chunk) == 0L) {
    yearly_list[[i]] <- NULL
    next
  }

  setDT(chunk)

  if (is.null(ppc) && "country" %in% names(chunk)) {
    ppc <- unique(chunk[!is.na(country), .(x, y, country)])
  }

  n_total_events <- n_total_events + nrow(chunk)

  # Fallback: join country from ppc if chunk lacks column (handles stale cfg_ files).
  if (!"country" %in% names(chunk) && !is.null(ppc)) {
    chunk <- merge(chunk, ppc, by = c("x", "y"), all.x = TRUE)
  }

  if ("country" %in% names(chunk) && any(!is.na(chunk$country))) {
    chunk <- chunk[!is.infinite(peak_index) & !is.na(country)]
    chunk[, event_year := as.integer(format(as.Date(start_date), "%Y"))]

    px_yr <- chunk[, .(
      pixel_n_events      = .N,
      pixel_mean_duration = mean(duration_months, na.rm = TRUE),
      pixel_mean_peak     = mean(peak_index,      na.rm = TRUE),
      pixel_mean_severity = mean(total_severity,  na.rm = TRUE),
      pixel_n_mild        = sum(severity_class == "Mild",     na.rm = TRUE),
      pixel_n_moderate    = sum(severity_class == "Moderate", na.rm = TRUE),
      pixel_n_severe      = sum(severity_class == "Severe",   na.rm = TRUE),
      pixel_n_extreme     = sum(severity_class == "Extreme",  na.rm = TRUE)
    ), keyby = .(config_id, index, method, start_thr, min_duration,
                 country, event_year, x, y)]

    cy <- px_yr[, .(
      pixels_with_drought   = .N,
      mean_events_per_pixel = round(mean(pixel_n_events,      na.rm = TRUE), 1),
      mean_duration_months  = round(mean(pixel_mean_duration, na.rm = TRUE), 1),
      mean_peak_index       = round(mean(pixel_mean_peak,     na.rm = TRUE), 2),
      mean_total_severity   = round(mean(pixel_mean_severity, na.rm = TRUE), 2)
    ), keyby = .(config_id, index, method, start_thr, min_duration,
                 country, event_year)]

    yearly_list[[i]] <- cy

    # Pixel-level freq in reference period — for Script 06 Level 3 spatial tests.
    # Sum events per pixel × config across ref years only (small accumulator).
    px_ref <- px_yr[event_year >= REF_START & event_year <= REF_END,
                    .(ref_n_events = sum(pixel_n_events, na.rm = TRUE)),
                    keyby = .(config_id, country, x, y)]
    pixel_freq_list[[i]] <- if (nrow(px_ref) > 0) px_ref else NULL

  } else {
    yearly_list[[i]]     <- NULL
    pixel_freq_list[[i]] <- NULL
  }

  rm(chunk, px_yr)
  if (i %% 10L == 0L) gc()
}

elapsed <- round(difftime(Sys.time(), overall_start, units = "mins"), 1)
message(sprintf("\n=== Summary pass complete in %.1f minutes ===", as.numeric(elapsed)))
message(sprintf("Total events across all configs: %s\n", format(n_total_events, big.mark = ",")))

# -- Build summaries from small per-config slabs ------------------------------
non_null_yearly <- Filter(Negate(is.null), yearly_list)
rm(yearly_list); gc()

if (length(non_null_yearly) > 0 && !is.null(ppc)) {

  drought_summary_yearly <- rbindlist(non_null_yearly, use.names = TRUE, fill = TRUE)
  rm(non_null_yearly); gc()

  ppc_count <- if (is.data.table(ppc)) {
    ppc[, .(total_pixels_country = .N), by = country]
  } else {
    dt <- as.data.table(aggregate(x ~ country, data = as.data.frame(ppc), FUN = length))
    setnames(dt, c("country", "total_pixels_country"))
    dt
  }

  drought_summary_yearly <- merge(drought_summary_yearly, ppc_count,
                                  by = "country", all.x = TRUE)
  drought_summary_yearly[, pct_pixels_affected := round(
    100 * pixels_with_drought / total_pixels_country, 1)]
  drought_summary_yearly[, total_pixels_country := NULL]
  setorder(drought_summary_yearly, config_id, country, event_year)

  n_years <- drought_summary_yearly[, uniqueN(event_year)]

  drought_annual_summary <- drought_summary_yearly[, .(
    annual_freq_events   = round(sum(mean_events_per_pixel, na.rm = TRUE) / n_years, 2),
    annual_pct_pixels    = round(sum(pct_pixels_affected,   na.rm = TRUE) / n_years, 1),
    annual_mean_duration = round(mean(mean_duration_months, na.rm = TRUE), 1),
    annual_mean_peak     = round(mean(mean_peak_index,      na.rm = TRUE), 2),
    annual_mean_severity = round(mean(mean_total_severity,  na.rm = TRUE), 2)
  ), keyby = .(config_id, index, method, start_thr, min_duration, country)]
  setorder(drought_annual_summary, country, index, method, start_thr)

} else {
  message("No 'country' column in any chunk — skipping country-level summaries.")
  drought_summary_yearly <- data.table()
  drought_annual_summary <- data.table()
  rm(non_null_yearly); gc()
}

# -- Assemble all_drought_events (single rbindlist after summaries freed) -----
# Summaries done and yearly_list freed. Peak RAM here = ~1.5x total events size.
message("\nAssembling all_drought_events from cfg_*.rds files...")
all_events_list <- lapply(chunk_files, function(f) {
  x <- readRDS(f)
  if (is.null(x) || nrow(x) == 0L) return(NULL)
  x
})
all_drought_events <- rbindlist(Filter(Negate(is.null), all_events_list),
                                use.names = TRUE, fill = TRUE)
rm(all_events_list); gc()

# -- Save outputs -------------------------------------------------------------
message("\n=== Saving outputs ===\n")

saveRDS(all_drought_events, file.path(VOLUME_PATH, "all_drought_events.rds"))
rm(all_drought_events); gc()
message("  all_drought_events.rds           — full event list, all configs")

if (nrow(drought_summary_yearly) > 0) {
  saveRDS(drought_summary_yearly,
          file.path(VOLUME_PATH, "drought_summary_yearly_ref.rds"))
  message("  drought_summary_yearly_ref.rds   — yearly summary per country per config (ERA5 reference)")
}

if (nrow(drought_annual_summary) > 0) {
  # Named _ref: ERA5 historical period = reference for scripts 06/07 Friedman analysis.
  # drought_annual_summary_comp.rds will be produced in Phase 3 from CMIP6 runs.
  saveRDS(drought_annual_summary,
          file.path(VOLUME_PATH, "drought_annual_summary_ref.rds"))
  message("  drought_annual_summary_ref.rds   — one row per config per country (ERA5 reference)")
}

# Pixel-level frequency in reference period — used by Script 06 Level 3 spatial tests.
non_null_pf <- Filter(Negate(is.null), pixel_freq_list)
rm(pixel_freq_list); gc()
if (length(non_null_pf) > 0) {
  pixel_freq_ref <- rbindlist(non_null_pf, use.names = TRUE, fill = TRUE)
  rm(non_null_pf)
  # Sum across configs that appear in multiple chunks (shouldn't happen but guard)
  pixel_freq_ref <- pixel_freq_ref[, .(ref_n_events = sum(ref_n_events, na.rm = TRUE)),
                                    keyby = .(config_id, country, x, y)]
  saveRDS(pixel_freq_ref, file.path(VOLUME_PATH, "pixel_freq_ref.rds"))
  message("  pixel_freq_ref.rds               — per-pixel event count in ref period (Script 06 Level 3)")
  rm(pixel_freq_ref); gc()
} else {
  message("  pixel_freq_ref.rds               — SKIPPED (no country column in chunks)")
}

message("\n=== Script 03 complete ===")
message("Next: Script 07 (sanity checks and visual inspection of detected events)")
