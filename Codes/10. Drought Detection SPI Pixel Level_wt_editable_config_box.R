###############################################################################
# DROUGHT DETECTION — PIXEL-BY-PIXEL WITH MULTIPLE CONFIGURATIONS
# Runs all config combinations and saves results in one table
# Each row tagged with its config specs for easy identification
###############################################################################

library(dplyr)
library(ggplot2)
library(lubridate)
library(patchwork)
library(SPEI)
library(tidyr)
library(purrr)

# =============================================================================
# STEP 1: Connect to Databricks and load SPI data
# =============================================================================

library(sparklyr)
library(dbplyr)

sc <- spark_connect(
  cluster_id = Sys.getenv("DATABRICKS_CLUSTER_ID"),
  method     = "databricks_connect"
)

project.folder <- "xx_drought_index"

spi <- tbl(sc, in_catalog("projects", project.folder, "spi_results"))

# Quick check of what is in the table
cat("=== DATA CHECK ===\n")
glimpse(spi)

# =============================================================================
# STEP 2: Pull data locally and prepare pixel time series
# =============================================================================

message("Pulling SPI data from Databricks...")

pixel_data <- spi %>%
  arrange(x, y, year, month) %>%
  collect() %>%
  mutate(
    month = as.integer(month),
    year  = as.integer(year),
    date  = as.Date(paste(year, month, "01", sep = "-"))
  ) %>%
  filter(between(year, 1975, 2023)) %>% # chnage year as needed 
  arrange(x, y, date)%>%
  filter(country %in% c("Argentina","Benin", "Thailand", "Greece", "Mexico", "South Africa"))  # Filter countries

cat(sprintf("Rows pulled  : %s\n",
            format(nrow(pixel_data), big.mark = ",")))
cat(sprintf("Unique pixels: %s\n",
            format(n_distinct(paste(pixel_data$x, pixel_data$y)),
                   big.mark = ",")))
cat(sprintf("Year range   : %d to %d\n",
            min(pixel_data$year), max(pixel_data$year)))
cat(sprintf("Columns      : %s\n\n",
            paste(names(pixel_data), collapse = ", ")))

pixels <- pixel_data %>%
  select(x, y) %>%
  distinct() %>%
  arrange(x, y)

total_pixels <- nrow(pixels)
cat(sprintf("Total pixels to process: %s\n\n",
            format(total_pixels, big.mark = ",")))


# =============================================================================
# STEP 3: CONFIGURATION RANGES
# ─────────────────────────────────────────────────────────────────────────────
# Define ranges and breaks for each parameter
# All combinations will be generated automatically
#
# SPI_SCALES    : which SPI columns to include
# METHODS       : which detection methods to include
# START_THR_SEQ : range of start thresholds  — seq(from, to, by)
# MIN_DUR_SEQ   : range of min_duration values — seq(from, to, by)
#
# To exclude any scale or method just remove it from the vector
# To test a single value set from = to  e.g. seq(-0.5, -0.5, by = 0.5)
# =============================================================================

# =============================================================================
# UPDATED CONFIGURATION TABLE
# =============================================================================

INDICES <- c("spi1",  "spi3",  "spi6",
             "spei1", "spei3", "spei6")

METHODS <- c("threshold_entry",   # ends at entry SPI value
             "threshold_peak",    # ends at rolling peak SPI value
             "cumulative")        # ends when cumulative sum >= 0

START_THR_SEQ    <- c(-0.0000000001, -0.5, -1.0)
MIN_DUR_SPI1_SEQ <- seq(1, 1, by = 1)

# ── Scale-1 indices: duration varies ─────────────────────────────────────────
configs_scale1 <- expand.grid(
  index        = c("spi1", "spei1"),
  method       = METHODS,
  start_thr    = START_THR_SEQ,
  min_duration = MIN_DUR_SPI1_SEQ,
  stringsAsFactors = FALSE
)

# ── Longer scales: duration fixed at 1 ───────────────────────────────────────
configs_longer <- expand.grid(
  index        = c("spi3", "spi6", "spei3", "spei6"),
  method       = METHODS,
  start_thr    = START_THR_SEQ,
  min_duration = 1,
  stringsAsFactors = FALSE
)

config_table <- bind_rows(configs_scale1, configs_longer) %>%
  as_tibble() %>%
  mutate(
    # end_thr not needed in config anymore — handled dynamically in function
    # but kept as reference label for output tables
    end_thr_description = case_when(
      method == "threshold_entry" ~ "above entry SPI",
      method == "threshold_peak"  ~ "above rolling peak SPI",
      method == "cumulative"      ~ "cumulative sum >= 0"
    ),
    
    # Auto-generate config name
    config_name = sprintf(
      "%s_%s_thr%s_dur%d",
      toupper(index),
      case_when(
        method == "threshold_entry" ~ "thr_ent",
        method == "threshold_peak"  ~ "thr_pek",
        method == "cumulative"      ~ "cum"
      ),
      gsub("-", "n",
           gsub("\\.", "",
                sprintf("%.1f", abs(start_thr)))),
      min_duration
    )
  ) %>%
  # Remove cumulative with duration > 2
  filter(!(method == "cumulative" & min_duration > 2)) %>%
  arrange(index, method, start_thr, min_duration)

configs_to_run <- config_table

cat("=== CONFIGURATIONS GENERATED ===\n\n")
configs_to_run %>%
  select(config_name, index, method,
         start_thr, end_thr_description,
         min_duration) %>%
  print(n = Inf, width = Inf)

cat(sprintf("\nTotal configurations: %d\n\n", nrow(configs_to_run)))

# ── Verify duration rule ──────────────────────────────────────────────────────
cat("=== DURATION PER INDEX ===\n\n")
configs_to_run %>%
  group_by(index) %>%
  summarise(
    durations   = paste(sort(unique(min_duration)), collapse = ", "),
    n_configs   = n(),
    .groups     = "drop"
  ) %>%
  print()


#============================================================================
# For easy check up lets filter only few definition specs

configs_to_run <- configs_to_run %>%
  filter(
    index  %in% c("spi1", "spi3", "spi6") &
      method %in% c("cumulative", "threshold_entry")
  )

# Verify what was kept
cat(sprintf("Configs remaining: %d\n\n", nrow(configs_to_run)))
configs_to_run %>%
  select(config_name, index, method, start_thr, min_duration) %>%
  print(n = Inf)
# =============================================================================
# UPDATED detect_droughts FUNCTION
# Three methods:
#   threshold_entry → ends when SPI recovers above first month value
#   threshold_peak  → ends when SPI recovers above rolling peak value
#   cumulative      → ends when cumulative sum from first month >= 0
# =============================================================================
detect_droughts <- function(spi_vec, dates, config) {
  
  if (is.null(dates))
    stop("'dates' is NULL")
  if (length(dates) != length(spi_vec))
    stop("'dates' and 'spi_vec' must be same length")
  
  dates     <- as.Date(dates)
  method    <- config$method
  start_thr <- config$start_threshold
  min_dur   <- config$min_duration
  n         <- length(spi_vec)
  
  # ── Determine end threshold logic upfront ───────────────────────────────────
  # Fixed threshold (-0.5, -1.0): end = start + 0.0000000001 (fixed)
  # Any negative (start ≈ 0)    : end = dynamic (based on entry value)
  is_any_negative <- abs(start_thr) < 0.001
  
  event_id   <- rep(NA_integer_, n)
  cum_track  <- rep(NA_real_,    n)
  drought_id <- 0
  i          <- 1
  
  while (i <= n) {
    
    val <- spi_vec[i]
    if (is.na(val) || is.nan(val)) { i <- i + 1; next }
    
    # ── Check min_dur consecutive months below start_thr ──────────────────────
    window       <- spi_vec[i:min(i + min_dur - 1, n)]
    window       <- window[!is.na(window) & !is.nan(window)]
    all_negative <- (length(window) == min_dur) && all(window < start_thr)
    
    if (!all_negative) { i <- i + 1; next }
    
    # ── Drought confirmed ──────────────────────────────────────────────────────
    drought_id  <- drought_id + 1
    cum_deficit <- 0
    entry_val   <- spi_vec[i]
    peak_val    <- Inf
    months_in_event <- 0
    
    # ── Set end threshold based on which case we are in ───────────────────────
    if (is_any_negative) {
      # Dynamic — ends when SPI recovers above entry value
      end_thr_fixed <- NULL
    } else {
      # Fixed — ends when SPI crosses back above start threshold
      end_thr_fixed <- start_thr + 0.0000000001
    }
    
    for (k in i:n) {
      
      val_k <- spi_vec[k]
      if (is.na(val_k) || is.nan(val_k)) next
      
      months_in_event <- months_in_event + 1
      
      if (method == "threshold_entry") {
        
        # ── Determine end condition for this method ──────────────────────────
        end_val <- if (is_any_negative) entry_val else end_thr_fixed
        
        if (months_in_event > 1 & val_k >= end_val) {
          i <- k + 1
          break
        }
        event_id[k]  <- drought_id
        cum_track[k] <- cum_deficit + val_k
        cum_deficit  <- cum_deficit + val_k
        
      } else if (method == "threshold_peak") {
        
        # ── Update peak before checking end ─────────────────────────────────
        if (val_k < peak_val) peak_val <- val_k
        
        # ── Determine end condition for this method ──────────────────────────
        # For fixed thresholds → end when above start_thr
        # For any negative     → end when above rolling peak
        if (is_any_negative) {
          end_val <- peak_val
        } else {
          end_val <- end_thr_fixed
        }
        
        if (months_in_event > 1 & val_k >= end_val) {
          i <- k + 1
          break
        }
        event_id[k]  <- drought_id
        cum_track[k] <- cum_deficit + val_k
        cum_deficit  <- cum_deficit + val_k
        
      } else if (method == "cumulative") {
        
        cum_deficit <- cum_deficit + val_k
        
        # ── Check end BEFORE tagging ──────────────────────────────────────
        # Recovery month (cum_deficit >= 0) is NOT tagged as drought
        # because it is a wet month that repaid the deficit
        if (cum_deficit >= 0) {
          i <- k + 1
          break
        }
        
        # ── Only tag months still in deficit ─────────────────────────────
        event_id[k]  <- drought_id
        cum_track[k] <- cum_deficit
      }
      
      if (k == n) { i <- n + 1 }
    }
  }
  
  if (drought_id == 0) return(tibble())
  
  monthly_track <- tibble(
    event_date  = dates,
    spi         = spi_vec,
    event_id    = event_id,
    cum_deficit = cum_track
  ) %>%
    filter(!is.na(event_id))
  
  events <- monthly_track %>%
    group_by(drought_id = event_id) %>%
    summarise(
      start_date      = min(event_date),
      end_date        = max(event_date),
      duration_months = n(),
      entry_spi       = spi[which.min(event_date)],
      peak_spi        = min(spi,         na.rm = TRUE),
      cum_spi_neg     = sum(spi[spi < 0], na.rm = TRUE),
      peak_date       = event_date[which.min(spi)],
      total_severity  = sum(spi,         na.rm = TRUE),
      min_cum_deficit = min(cum_deficit, na.rm = TRUE),
      .groups         = "drop"
    ) %>%
    mutate(
      severity_class = case_when(
        peak_spi >= -1.0                    ~ "Mild",
        peak_spi >= -1.5 & peak_spi < -1.0 ~ "Moderate",
        peak_spi >= -2.0 & peak_spi < -1.5 ~ "Severe",
        peak_spi <  -2.0                    ~ "Extreme"
      )
    )
  
  return(events)
}

# =============================================================================
# STEP 5: Loop through all configurations
# For each config, loop through all pixels and collect events
# Tag every row with config specs so results are identifiable
# =============================================================================

message("\n=== Running all configurations ===\n")
overall_start <- Sys.time()

all_drought_events <- map_dfr(seq_len(nrow(configs_to_run)), function(cfg_i) {
  
  cfg <- configs_to_run[cfg_i, ]
  
  message(sprintf(
    "\n[Config %d/%d] %s | index=%s | method=%s | start=%.4f | min_dur=%d",
    cfg_i, nrow(configs_to_run),
    cfg$config_name,
    cfg$index,          # ← was cfg$spi_scale
    cfg$method,
    cfg$start_thr,
    cfg$min_duration
  ))
  
  tick <- max(1, floor(total_pixels / 10))
  
  pixel_events <- map_dfr(seq_len(total_pixels), function(i) {
    
    px <- pixels$x[i]
    py <- pixels$y[i]
    
    if (i %% tick == 0 || i == total_pixels) {
      message(sprintf("  Pixel %d / %d  (%.0f%%)",
                      i, total_pixels, 100 * i / total_pixels))
    }
    
    pixel_ts <- pixel_data %>%
      filter(x == px, y == py) %>%
      arrange(date)
    
    if (nrow(pixel_ts) < 12) return(tibble())
    
    spi_vec <- pixel_ts[[cfg$index]]   # ← was cfg$spi_scale
    
    dates   <- pixel_ts$date
    
    if (all(is.na(spi_vec))) return(tibble())
    
    events <- tryCatch(
      detect_droughts(
        spi_vec = spi_vec,
        dates   = dates,
        config  = list(
          method          = cfg$method,
          start_threshold = cfg$start_thr,
          # ← end_threshold removed — handled dynamically in function
          min_duration    = cfg$min_duration
        )
      ),
      error = function(e) tibble()
    )
    
    if (nrow(events) == 0) return(tibble())
    
    # Tag with pixel coordinates AND all config specs
    events %>%
      mutate(
        x            = px,
        y            = py,
        config_name  = cfg$config_name,
        index        = cfg$index,        # ← was spi_scale
        method       = cfg$method,
        start_thr    = cfg$start_thr,
        min_duration = cfg$min_duration
      )
  })
  
  message(sprintf("  → %s events detected",
                  format(nrow(pixel_events), big.mark = ",")))
  
  return(pixel_events)
})

elapsed <- round(difftime(Sys.time(), overall_start, units = "mins"), 1)
message(sprintf("\n=== All configs complete in %.1f minutes ===",
                as.numeric(elapsed)))
message(sprintf("Total events across all configs: %s\n",
                format(nrow(all_drought_events), big.mark = ",")))
# =============================================================================
# STEP 6: Add country labels then yearly summary by country
# =============================================================================

message("=== Adding country labels ===\n")

# Get unique x, y, country from pixel_data
pixels_with_country <- pixel_data %>%
  select(x, y, country) %>%
  distinct()

# Join country onto all_drought_events
all_drought_events <- all_drought_events %>%
  left_join(pixels_with_country, by = c("x", "y"))

cat("Country check:\n")
print(table(all_drought_events$country, useNA = "always"))

# =============================================================================
# STEP 6: Yearly summary — mean across pixels within each country
# =============================================================================

# =============================================================================
# STEP 6A: Create pixels_per_country lookup
# Count total pixels per country — used as denominator for pct calculations
# =============================================================================

pixels_per_country <- pixel_data %>%
  select(x, y, country) %>%
  distinct() %>%
  filter(!is.na(country)) %>%
  group_by(country) %>%
  summarise(total_pixels_country = n(), .groups = "drop")

cat("=== PIXELS PER COUNTRY ===\n\n")
print(pixels_per_country)

drought_summary_yearly <- all_drought_events %>%
  filter(!if_any(everything(), is.infinite)) %>%
  filter(!is.na(country)) %>%
  mutate(event_year = year(start_date)) %>%
  
  # ── First summarise to pixel level ────────────────────────────────────────
  group_by(
    config_name, index, method,    # ← spi_scale → index
    start_thr, min_duration,
    country, event_year, x, y
  ) %>%
  summarise(
    pixel_n_events      = n(),
    pixel_mean_duration = mean(duration_months,  na.rm = TRUE),
    pixel_mean_peak_spi = mean(peak_spi,         na.rm = TRUE),
    pixel_mean_cum_spi_neg = mean(cum_spi_neg, na.rm = TRUE),
    pixel_mean_severity = mean(total_severity,   na.rm = TRUE),
    pixel_n_mild        = sum(severity_class == "Mild",     na.rm = TRUE),
    pixel_n_moderate    = sum(severity_class == "Moderate", na.rm = TRUE),
    pixel_n_severe      = sum(severity_class == "Severe",   na.rm = TRUE),
    pixel_n_extreme     = sum(severity_class == "Extreme",  na.rm = TRUE),
    .groups = "drop"
  ) %>%
  
  # ── Then average across pixels within each country ────────────────────────
  group_by(
    config_name, index, method,    # ← spi_scale → index
    start_thr, min_duration,
    country, event_year
  ) %>%
  summarise(
    pixels_with_drought   = n(),
    mean_events_per_pixel = round(mean(pixel_n_events,      na.rm = TRUE), 1),
    mean_duration_months  = round(mean(pixel_mean_duration, na.rm = TRUE), 1),
    mean_peak_spi         = round(mean(pixel_mean_peak_spi, na.rm = TRUE), 2),
    
    mean_cum_spi_neg      = round(mean(pixel_mean_cum_spi_neg, na.rm = TRUE) ,2),
    
    mean_total_severity   = round(mean(pixel_mean_severity, na.rm = TRUE), 2),
    mean_n_mild           = round(mean(pixel_n_mild,        na.rm = TRUE), 1),
    mean_n_moderate       = round(mean(pixel_n_moderate,    na.rm = TRUE), 1),
    mean_n_severe         = round(mean(pixel_n_severe,      na.rm = TRUE), 1),
    mean_n_extreme        = round(mean(pixel_n_extreme,     na.rm = TRUE), 1),
    .groups = "drop"
  ) %>%
  left_join(pixels_per_country, by = "country") %>%
  mutate(
    pct_pixels_affected = round(
      100 * pixels_with_drought / total_pixels_country, 1)
  ) %>%
  select(-total_pixels_country) %>%
  arrange(config_name, country, event_year)

cat("=== YEARLY SUMMARY BY COUNTRY — PREVIEW ===\n\n")
drought_summary_yearly %>%
  filter(config_name == configs_to_run$config_name[1]) %>%
  print(n = Inf, width = Inf)


# =============================================================================
# STEP 6B: Annual frequency and means — collapse yearly summary across years
# One row per country per config
# =============================================================================

n_years <- n_distinct(year(all_drought_events$start_date))

drought_annual_summary <- drought_summary_yearly %>%
  group_by(
    config_name, index, method,
    start_thr, min_duration,
    country
  ) %>%
  summarise(
    # ── Annual frequency — mean events per pixel per year ──────────────────
    # sum across all years then divide by total years
    # includes zero-event years in denominator
    annual_freq_events      = round(sum(mean_events_per_pixel,  na.rm = TRUE) / n_years, 2),
    annual_pct_pixels       = round(sum(pct_pixels_affected,    na.rm = TRUE) / n_years, 1),
    
    # ── Annual means — average each metric across years ────────────────────
    annual_mean_duration    = round(mean(mean_duration_months,  na.rm = TRUE), 1),
    annual_mean_peak_spi    = round(mean(mean_peak_spi,         na.rm = TRUE), 2),
    annual_mean_cum_spi_neg = round(mean(mean_cum_spi_neg,      na.rm = TRUE), 2),
    annual_mean_severity    = round(mean(mean_total_severity,   na.rm = TRUE), 2),
    annual_mean_mild        = round(mean(mean_n_mild,           na.rm = TRUE), 1),
    annual_mean_moderate    = round(mean(mean_n_moderate,       na.rm = TRUE), 1),
    annual_mean_severe      = round(mean(mean_n_severe,         na.rm = TRUE), 1),
    annual_mean_extreme     = round(mean(mean_n_extreme,        na.rm = TRUE), 1),
    .groups = "drop"
  ) %>%
  arrange(country, index, method, start_thr, min_duration)

cat("=== ANNUAL FREQUENCY AND MEANS ===\n\n")
cat(sprintf("Averaged over %d years\n\n", n_years))
drought_annual_summary %>%
  filter(country == "Benin") %>%
  print(width = Inf)



# =============================================================================
# STEP 7: Overall summary — mean across pixels per config per country
# =============================================================================

config_comparison <- all_drought_events %>%
  filter(!if_any(everything(), is.infinite)) %>%
  filter(!is.na(country)) %>%
  
  # ── First summarise to pixel level ────────────────────────────────────────
  group_by(
    config_name, index, method,    # ← spi_scale → index
    start_thr, min_duration,
    country, x, y
  ) %>%
  summarise(
    pixel_n_events      = n(),
    pixel_mean_duration = mean(duration_months, na.rm = TRUE),
    pixel_mean_peak_spi = mean(peak_spi,        na.rm = TRUE),
    pixel_mean_severity = mean(total_severity,  na.rm = TRUE),
    pixel_pct_mild      = 100 * mean(severity_class == "Mild",     na.rm = TRUE),
    pixel_pct_moderate  = 100 * mean(severity_class == "Moderate", na.rm = TRUE),
    pixel_pct_severe    = 100 * mean(severity_class == "Severe",   na.rm = TRUE),
    pixel_pct_extreme   = 100 * mean(severity_class == "Extreme",  na.rm = TRUE),
    .groups = "drop"
  ) %>%
  
  # ── Then average across pixels within each country ────────────────────────
  group_by(
    config_name, index, method,    # ← spi_scale → index
    start_thr, min_duration, country
  ) %>%
  summarise(
    pixels_with_drought     = n(),
    mean_events_per_pixel   = round(mean(pixel_n_events,      na.rm = TRUE), 1),
    mean_duration_months    = round(mean(pixel_mean_duration, na.rm = TRUE), 1),
    mean_peak_spi           = round(mean(pixel_mean_peak_spi, na.rm = TRUE), 2),
    mean_total_severity     = round(mean(pixel_mean_severity, na.rm = TRUE), 2),
    mean_pct_mild           = round(mean(pixel_pct_mild,      na.rm = TRUE), 1),
    mean_pct_moderate       = round(mean(pixel_pct_moderate,  na.rm = TRUE), 1),
    mean_pct_severe         = round(mean(pixel_pct_severe,    na.rm = TRUE), 1),
    mean_pct_extreme        = round(mean(pixel_pct_extreme,   na.rm = TRUE), 1),
    .groups = "drop"
  ) %>%
  left_join(pixels_per_country, by = "country") %>%
  mutate(
    pct_pixels_with_drought = round(
      100 * pixels_with_drought / total_pixels_country, 1)
  ) %>%
  select(-total_pixels_country) %>%
  arrange(country, index, start_thr, min_duration)   # ← spi_scale → index

cat("=== CONFIG COMPARISON BY COUNTRY — PREVIEW ===\n\n")
config_comparison %>%
  filter(country == "Argentina") %>%
  print(width = Inf)

# =============================================================================
# STEP 8: Save outputs
# =============================================================================

saveRDS(all_drought_events,
        "./Data/all_drought_events_allconfigs.rds")

#write.csv(all_drought_events,
#          "./Data/all_drought_events_allconfigs.csv",
#          row.names = FALSE)



write.csv(drought_annual_summary,
          "./Data/drought_annual_summary.csv",
          row.names = FALSE)


#______________________________________________________________________________

write.csv(drought_summary_yearly,
          "./Data/drought_summary_yearly_bycountry.csv",
          row.names = FALSE)

write.csv(config_comparison,
          "./Data/config_comparison_bycountry.csv",
          row.names = FALSE)

message("\n=== All outputs saved ===")
message("  all_drought_events_allconfigs.rds     — full event list")
message("  drought_summary_yearly_bycountry.csv  — yearly summary per country per config")
message("  config_comparison_bycountry.csv       — one row per config per country")

