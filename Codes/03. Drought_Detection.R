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
library(purrr)
library(lubridate)


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
# Full period: reference (1970–1999) + comparison (2000–2020) + buffer to 2023.
YEAR_FROM <- 1970
YEAR_TO   <- 2023

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

  dates  <- as.Date(dates)
  n      <- length(index_vec)

  event_id   <- rep(NA_integer_, n)
  cum_track  <- rep(NA_real_,    n)
  drought_id <- 0L
  i          <- 1L

  while (i <= n) {

    val <- index_vec[i]
    if (is.na(val) || is.nan(val)) { i <- i + 1L; next }

    # -- Check min_duration consecutive months all below start_thr ------------
    window       <- index_vec[i:min(i + min_duration - 1L, n)]
    window       <- window[!is.na(window) & !is.nan(window)]
    onset_confirmed <- (length(window) == min_duration) &&
                       all(window < start_thr)

    if (!onset_confirmed) { i <- i + 1L; next }

    # -- Drought onset confirmed ----------------------------------------------
    drought_id  <- drought_id + 1L
    cum_deficit <- 0
    months_in_event <- 0L

    for (k in i:n) {

      val_k <- index_vec[k]
      if (is.na(val_k) || is.nan(val_k)) next

      months_in_event <- months_in_event + 1L

      if (method == "threshold") {
        # Recovery: first month index >= start_thr (after at least 1 month in event)
        if (months_in_event > 1L && val_k >= start_thr) {
          i <- k + 1L
          break
        }
        event_id[k]  <- drought_id
        cum_deficit  <- cum_deficit + val_k
        cum_track[k] <- cum_deficit

      } else if (method == "cumulative") {
        # Accumulate before checking recovery
        cum_deficit <- cum_deficit + val_k
        # Recovery: cumulative sum returns to >= 0 (wet month repaid deficit)
        # Recovery month not tagged — it is a positive contribution, not drought
        if (cum_deficit >= 0) {
          i <- k + 1L
          break
        }
        event_id[k]  <- drought_id
        cum_track[k] <- cum_deficit
      }

      if (k == n) { i <- n + 1L }
    }
  }

  if (drought_id == 0L) return(data.frame())

  # -- Step 4: Record event characteristics -----------------------------------
  # Analysis Plan Section 1.3 Step 4: extract characteristics per event.
  monthly_tagged <- data.frame(
    event_date  = dates,
    index_val   = index_vec,
    event_id    = event_id,
    cum_deficit = cum_track,
    stringsAsFactors = FALSE
  )
  monthly_tagged <- monthly_tagged[!is.na(monthly_tagged$event_id), ]

  events <- monthly_tagged |>
    (\(d) {
      do.call(rbind, lapply(split(d, d$event_id), function(ev) {
        data.frame(
          drought_id      = ev$event_id[1],
          start_date      = min(ev$event_date),
          end_date        = max(ev$event_date),
          duration_months = nrow(ev),
          entry_index     = ev$index_val[which.min(ev$event_date)],
          peak_index      = min(ev$index_val,          na.rm = TRUE),
          peak_date       = ev$event_date[which.min(ev$index_val)],
          cum_deficit_neg = sum(ev$index_val[ev$index_val < 0], na.rm = TRUE),
          total_severity  = sum(ev$index_val,           na.rm = TRUE),
          min_cum_deficit = min(ev$cum_deficit,         na.rm = TRUE),
          stringsAsFactors = FALSE
        )
      }))
    })()

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

overall_start <- Sys.time()

config_results <- purrr::map(seq_len(nrow(config_table)), function(cfg_i) {

  cfg <- config_table[cfg_i, ]

  message(sprintf(
    "\n[%d/%d] Config: %s | index=%s | method=%s | thr=%.2f | min_dur=%d",
    cfg_i, nrow(config_table),
    cfg$config_id, cfg$index, cfg$method, cfg$start_thr, cfg$min_duration
  ))

  pixel_events <- purrr::map_dfr(seq_len(total_pixels), function(i) {

    px <- pixels$x[i]
    py <- pixels$y[i]

    pixel_ts <- pixel_list[[paste(px, py, sep = "_")]]
    if (is.null(pixel_ts)) return(data.frame())
    if (nrow(pixel_ts) < 12) return(data.frame())

    index_vec <- pixel_ts[[cfg$index]]
    if (all(is.na(index_vec))) return(data.frame())

    events <- tryCatch(
      detect_droughts(
        index_vec    = index_vec,
        dates        = pixel_ts$date,
        method       = cfg$method,
        start_thr    = cfg$start_thr,
        min_duration = cfg$min_duration
      ),
      error = function(e) data.frame()
    )

    if (nrow(events) == 0) return(data.frame())

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

    return(events)
  })

  message(sprintf("  -> %s events detected",
                  format(nrow(pixel_events), big.mark = ",")))

  return(pixel_events)
})

all_drought_events <- dplyr::bind_rows(config_results)

elapsed <- round(difftime(Sys.time(), overall_start, units = "mins"), 1)
message(sprintf("\n=== Detection complete in %.1f minutes ===", as.numeric(elapsed)))
message(sprintf("Total events across all configs: %s\n",
                format(nrow(all_drought_events), big.mark = ",")))


# -- Summaries ----------------------------------------------------------------
# Yearly summary: mean characteristics per country per config per year.
# Annual summary: collapse yearly → one row per country per config.
# Config comparison: one row per config per country for comparing configurations.

if ("country" %in% names(all_drought_events)) {

  pixels_per_country <- unique(index_data[!is.na(index_data$country),
                                          c("x", "y", "country")]) |>
    (\(d) aggregate(x ~ country, data = d, FUN = length))() |>
    setNames(c("country", "total_pixels_country"))

  all_drought_events <- all_drought_events[
    !is.infinite(all_drought_events$peak_index) &
      !is.na(all_drought_events$country), ]

  all_drought_events$event_year <- as.integer(format(
    as.Date(all_drought_events$start_date), "%Y"))

  # -- Yearly summary ---------------------------------------------------------
  drought_summary_yearly <- all_drought_events |>
    group_by(config_id, index, method, start_thr, min_duration,
             country, event_year, x, y) |>
    summarise(
      pixel_n_events      = n(),
      pixel_mean_duration = mean(duration_months, na.rm = TRUE),
      pixel_mean_peak     = mean(peak_index,      na.rm = TRUE),
      pixel_mean_severity = mean(total_severity,  na.rm = TRUE),
      pixel_n_mild        = sum(severity_class == "Mild",     na.rm = TRUE),
      pixel_n_moderate    = sum(severity_class == "Moderate", na.rm = TRUE),
      pixel_n_severe      = sum(severity_class == "Severe",   na.rm = TRUE),
      pixel_n_extreme     = sum(severity_class == "Extreme",  na.rm = TRUE),
      .groups = "drop"
    ) |>
    group_by(config_id, index, method, start_thr, min_duration,
             country, event_year) |>
    summarise(
      pixels_with_drought   = n(),
      mean_events_per_pixel = round(mean(pixel_n_events,      na.rm = TRUE), 1),
      mean_duration_months  = round(mean(pixel_mean_duration, na.rm = TRUE), 1),
      mean_peak_index       = round(mean(pixel_mean_peak,     na.rm = TRUE), 2),
      mean_total_severity   = round(mean(pixel_mean_severity, na.rm = TRUE), 2),
      .groups = "drop"
    ) |>
    merge(pixels_per_country, by = "country") |>
    transform(pct_pixels_affected = round(
      100 * pixels_with_drought / total_pixels_country, 1)) |>
    (\(d) d[, !names(d) %in% "total_pixels_country"])()|>
    (\(d) d[order(d$config_id, d$country, d$event_year), ])()

  # -- Annual summary ---------------------------------------------------------
  n_years <- length(unique(all_drought_events$event_year))

  drought_annual_summary <- drought_summary_yearly |>
    group_by(config_id, index, method, start_thr, min_duration, country) |>
    summarise(
      annual_freq_events   = round(sum(mean_events_per_pixel, na.rm = TRUE) / n_years, 2),
      annual_pct_pixels    = round(sum(pct_pixels_affected,   na.rm = TRUE) / n_years, 1),
      annual_mean_duration = round(mean(mean_duration_months, na.rm = TRUE), 1),
      annual_mean_peak     = round(mean(mean_peak_index,      na.rm = TRUE), 2),
      annual_mean_severity = round(mean(mean_total_severity,  na.rm = TRUE), 2),
      .groups = "drop"
    ) |>
    (\(d) d[order(d$country, d$index, d$method, d$start_thr), ])()

} else {
  message("No 'country' column — skipping country-level summaries.")
  drought_summary_yearly <- data.frame()
  drought_annual_summary <- data.frame()
}


# -- Save outputs -------------------------------------------------------------
message("\n=== Saving outputs ===\n")

saveRDS(all_drought_events, file.path(VOLUME_PATH, "all_drought_events.rds"))
message("  all_drought_events.rds           — full event list, all configs")

if (nrow(drought_summary_yearly) > 0) {
  write.csv(drought_summary_yearly, file.path(VOLUME_PATH, "drought_summary_yearly.csv"),
            row.names = FALSE)
  message("  drought_summary_yearly.csv       — yearly summary per country per config")
}

if (nrow(drought_annual_summary) > 0) {
  write.csv(drought_annual_summary, file.path(VOLUME_PATH, "drought_annual_summary.csv"),
            row.names = FALSE)
  message("  drought_annual_summary.csv       — one row per config per country")
}

message("\n=== Script 06 complete ===")
message("Next: Script 07 (sanity checks and visual inspection of detected events)")
