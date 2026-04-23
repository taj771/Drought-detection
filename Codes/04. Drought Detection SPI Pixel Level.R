###############################################################################
# DROUGHT DETECTION — PIXEL-BY-PIXEL (no country averaging)
# Runs detect_droughts() for every grid cell independently
# Indices: SPI1, SPI3, SPI6, SPI12
# Methods: threshold | cumulative
###############################################################################

library(dplyr)
library(ggplot2)
library(lubridate)
library(patchwork)
library(SPEI)
library(tidyr)
library(purrr)


# ─────────────────────────────────────────────────────────────────────────────
# STEP 1: Load data
# ─────────────────────────────────────────────────────────────────────────────

spi_results <- readRDS("./Data/spi_results.rds")

# Expected columns: x, y, year, month, spi1, spi3, spi6, spi12
# One row per pixel per month

# Quick check
cat("=== DATA CHECK ===\n")
cat(sprintf("Rows         : %s\n", format(nrow(spi_results), big.mark = ",")))
cat(sprintf("Unique pixels: %s\n", format(n_distinct(paste(spi_results$x, spi_results$y)), big.mark = ",")))
cat(sprintf("Year range   : %d to %d\n", min(spi_results$year), max(spi_results$year)))
cat(sprintf("Columns      : %s\n\n", paste(names(spi_results), collapse = ", ")))

# ─────────────────────────────────────────────────────────────────────────────
# STEP 2: Prepare pixel time series
# Add date column, filter year range, sort
# ─────────────────────────────────────────────────────────────────────────────

pixel_data <- spi_results %>%
  mutate(
    month = as.integer(month),
    year  = as.integer(year),
    date  = as.Date(paste(year, month, "01", sep = "-"))
  ) %>%
  filter(between(year, 2000, 2025)) %>%
  arrange(x, y, date)

cat(sprintf("Pixels after year filter: %s\n",
            format(n_distinct(paste(pixel_data$x, pixel_data$y)),
                   big.mark = ",")))

# ─────────────────────────────────────────────────────────────────────────────
# STEP 3: Get unique pixel coordinates
# ─────────────────────────────────────────────────────────────────────────────

pixels <- pixel_data %>%
  select(x, y) %>%
  distinct() %>%
  arrange(x, y)

cat(sprintf("Total pixels to process: %s\n\n", format(nrow(pixels), big.mark = ",")))

# ─────────────────────────────────────────────────────────────────────────────
# STEP 4: Configure drought detection — ONLY change this block

# method:
#   "threshold"  → drought ends when SPI rises back above end_threshold
#   "cumulative" → drought ends when cumulative SPI sum is repaid to >= 0
#                  (allows drought to continue through above-normal months)
#
# start_threshold:
#   0    = any negative SPI triggers (most sensitive)
#  -0.5  = mild dryness onset
#  -1.0  = WMO moderate drought
#
# end_threshold:
#   used only for method = "threshold"
#   e.g. -0.5 means SPI must recover to >= -0.5 to end the drought
#
# min_duration:
#   minimum number of CONSECUTIVE months below start_threshold
#   required before a drought is declared
#   e.g. 2 = need two back-to-back negative months to confirm drought start
# ─────────────────────────────────────────────────────────────────────────────

drought_config <- list(
  method          = "threshold",
  start_threshold = -0.5,
  end_threshold   = -0.4999999999999,
  min_duration    = 2
)

# ─────────────────────────────────────────────────────────────────────────────
# STEP 5: Drought detection function 
# ─────────────────────────────────────────────────────────────────────────────

detect_droughts <- function(spi_vec, dates, config) {
  
  if (is.null(dates))                   stop("'dates' is NULL")
  if (length(dates) != length(spi_vec)) stop("'dates' and 'spi_vec' must be same length")
  
  dates     <- as.Date(dates)
  method    <- config$method
  start_thr <- config$start_threshold
  end_thr   <- config$end_threshold
  min_dur   <- config$min_duration
  n         <- length(spi_vec)
  
  event_id   <- rep(NA_integer_, n)
  cum_track  <- rep(NA_real_,    n)
  drought_id <- 0
  i          <- 1
  
  while (i <= n) {
    
    val <- spi_vec[i]
    if (is.na(val) || is.nan(val)) { i <- i + 1; next }
    
    window       <- spi_vec[i:min(i + min_dur - 1, n)]
    window       <- window[!is.na(window) & !is.nan(window)]
    all_negative <- (length(window) == min_dur) && all(window < start_thr)
    
    if (!all_negative) { i <- i + 1; next }
    
    drought_id  <- drought_id + 1
    cum_deficit <- 0
    
    for (k in i:n) {
      val_k <- spi_vec[k]
      if (is.na(val_k) || is.nan(val_k)) next
      
      if (method == "threshold") {
        if (val_k >= end_thr) { i <- k + 1; break }
        cum_deficit  <- cum_deficit + val_k
        event_id[k]  <- drought_id
        cum_track[k] <- cum_deficit
        
      } else if (method == "cumulative") {
        cum_deficit  <- cum_deficit + val_k
        event_id[k]  <- drought_id
        cum_track[k] <- cum_deficit
        if (cum_deficit >= 0) { i <- k + 1; break }
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
  ) %>% filter(!is.na(event_id))
  
  events <- monthly_track %>%
    group_by(drought_id = event_id) %>%
    summarise(
      start_date      = min(event_date),
      end_date        = max(event_date),
      duration_months = n(),
      peak_spi        = min(spi,          na.rm = TRUE),
      peak_date       = event_date[which.min(spi)],
      total_severity  = sum(spi,          na.rm = TRUE),
      min_cum_deficit = min(cum_deficit,  na.rm = TRUE),
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

# ─────────────────────────────────────────────────────────────────────────────
# STEP 6: Run drought detection pixel by pixel across all SPI scales
#
# For each pixel (x, y):
#   - extract its full time series for spi1, spi3, spi6, spi12
#   - run detect_droughts() on each scale
#   - tag results with x, y, spi_scale
#   - collect all events into one long dataframe
#
# Output: all_drought_events
#   One row per drought event per pixel per SPI scale
# ─────────────────────────────────────────────────────────────────────────────

message("\n=== Running pixel-by-pixel drought detection ===\n")
message(sprintf("Method: %s | Start: %.1f | End: %.10f | Min duration: %d months\n",
                drought_config$method,
                drought_config$start_threshold,
                drought_config$end_threshold,
                drought_config$min_duration))

spi_scales <- c("spi1", "spi3", "spi6", "spi12")

# Progress counter
total_pixels <- nrow(pixels)
tick         <- max(1, floor(total_pixels / 20))  # print progress every 5%

all_drought_events <- map_dfr(seq_len(total_pixels), function(i) {
  
  px <- pixels$x[i]
  py <- pixels$y[i]
  
  # Progress message
  if (i %% tick == 0 || i == total_pixels) {
    message(sprintf("  Processing pixel %d / %d  (%.0f%%)",
                    i, total_pixels, 100 * i / total_pixels))
  }
  
  # Extract time series for this pixel
  pixel_ts <- pixel_data %>%
    filter(x == px, y == py) %>%
    arrange(date)
  
  # Skip pixels with too many NAs
  if (nrow(pixel_ts) < 12) return(tibble())
  
  # Run detection for each SPI scale
  map_dfr(spi_scales, function(scale) {
    
    spi_vec <- pixel_ts[[scale]]
    dates   <- pixel_ts$date
    
    # Skip if all NA
    if (all(is.na(spi_vec))) return(tibble())
    
    events <- tryCatch(
      detect_droughts(spi_vec, dates, drought_config),
      error = function(e) tibble()
    )
    
    if (nrow(events) == 0) return(tibble())
    
    # Tag with pixel coordinates and scale
    events %>%
      mutate(
        x         = px,
        y         = py,
        spi_scale = toupper(scale)
      )
  })
})

message(sprintf("\nTotal drought events detected across all pixels: %s\n",
                format(nrow(all_drought_events), big.mark = ",")))

# ─────────────────────────────────────────────────────────────────────────────
# STEP 7: Summary table by YEAR and SPI scale
# For each year — how many droughts, how long, how severe
# Only counts events that STARTED in that year
# ─────────────────────────────────────────────────────────────────────────────

message("=== Building yearly summary table ===\n")

drought_summary_yearly <- all_drought_events %>%
  filter(!if_any(everything(), is.infinite)) %>%
  
  # Use the year the drought STARTED
  mutate(event_year = year(start_date)) %>%
  
  group_by(event_year, spi_scale) %>%
  summarise(
    
    # How many drought events started this year across all pixels
    total_events            = n(),
    
    # How many unique pixels had a drought start this year
    pixels_with_drought     = n_distinct(paste(x, y)),
    
    # What % of total pixels had drought this year
    pct_pixels_affected     = round(100 * n_distinct(paste(x, y)) /
                                      total_pixels, 1),
    
    # Duration stats
    mean_duration_months    = round(mean(duration_months,   na.rm = TRUE), 1),
    median_duration_months  = round(median(duration_months, na.rm = TRUE), 1),
    max_duration_months     = max(duration_months,          na.rm = TRUE),
    
    # Severity stats
    mean_peak_spi           = round(mean(peak_spi,          na.rm = TRUE), 2),
    min_peak_spi            = round(min(peak_spi,           na.rm = TRUE), 2),
    mean_total_severity     = round(mean(total_severity,    na.rm = TRUE), 2),
    
    # Severity class counts
    n_mild                  = sum(severity_class == "Mild",     na.rm = TRUE),
    n_moderate              = sum(severity_class == "Moderate", na.rm = TRUE),
    n_severe                = sum(severity_class == "Severe",   na.rm = TRUE),
    n_extreme               = sum(severity_class == "Extreme",  na.rm = TRUE),
    
    .groups = "drop"
  ) %>%
  arrange(spi_scale, event_year)

cat("=== YEARLY DROUGHT SUMMARY — ALL PIXELS ===\n\n")
print(drought_summary_yearly, n = 30, width = Inf)

# ─────────────────────────────────────────────────────────────────────────────
# Quick preview — show SPI3 only so output is readable
# ─────────────────────────────────────────────────────────────────────────────

cat("\n=== PREVIEW: SPI3 yearly summary ===\n\n")
drought_summary_yearly %>%
  filter(spi_scale == "SPI3") %>%
  select(event_year, total_events, pixels_with_drought,
         pct_pixels_affected, mean_duration_months,
         mean_peak_spi, n_moderate, n_severe, n_extreme) %>%
  print(n = Inf, width = Inf)

# ─────────────────────────────────────────────────────────────────────────────
# Plot: events per year per SPI scale
# ─────────────────────────────────────────────────────────────────────────────

p_yearly_events <- ggplot(
  drought_summary_yearly,
  aes(x = event_year, y = total_events, colour = spi_scale,
      group = spi_scale)
) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.8) +
  scale_colour_manual(
    values = c(SPI1 = "#5C9BD6", SPI3 = "#E67E22",
               SPI6 = "#27AE60", SPI12 = "#8E44AD"),
    name = "SPI scale"
  ) +
  labs(
    title    = "Total drought events detected per year — all pixels",
    subtitle = sprintf("Method: %s | Start threshold: %.1f | Min duration: %d months",
                       drought_config$method,
                       drought_config$start_threshold,
                       drought_config$min_duration),
    x = "Year",
    y = "Total drought events (all pixels)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title    = element_text(face = "bold"),
    plot.subtitle = element_text(size = 9, colour = "grey40"),
    legend.position = "top"
  )

p_yearly_pct <- ggplot(
  drought_summary_yearly,
  aes(x = event_year, y = pct_pixels_affected, colour = spi_scale,
      group = spi_scale)
) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.8) +
  scale_colour_manual(
    values = c(SPI1 = "#5C9BD6", SPI3 = "#E67E22",
               SPI6 = "#27AE60", SPI12 = "#8E44AD"),
    name = "SPI scale"
  ) +
  labs(
    title    = "% of pixels affected by drought per year",
    x        = "Year",
    y        = "% pixels with drought"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title      = element_text(face = "bold"),
    legend.position = "top"
  )

p_yearly_duration <- ggplot(
  drought_summary_yearly,
  aes(x = event_year, y = mean_duration_months, colour = spi_scale,
      group = spi_scale)
) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.8) +
  scale_colour_manual(
    values = c(SPI1 = "#5C9BD6", SPI3 = "#E67E22",
               SPI6 = "#27AE60", SPI12 = "#8E44AD"),
    name = "SPI scale"
  ) +
  labs(
    title    = "Mean drought duration per year (months)",
    x        = "Year",
    y        = "Mean duration (months)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title      = element_text(face = "bold"),
    legend.position = "top"
  )

p_yearly_events / p_yearly_pct / p_yearly_duration +
  plot_annotation(
    title = "Yearly drought characteristics — pixel-by-pixel detection",
    theme = theme(plot.title = element_text(face = "bold", size = 14))
  )

ggsave("./Figures/drought_yearly_summary.png",
       width = 12, height = 12, dpi = 300)

# ─────────────────────────────────────────────────────────────────────────────
# Save yearly summary
# ─────────────────────────────────────────────────────────────────────────────

write.csv(drought_summary_yearly,
          "./Data/drought_summary_yearly_pixelbypixel.csv",
          row.names = FALSE)

message("Yearly summary saved to: ./Data/drought_summary_yearly_pixelbypixel.csv")

# ─────────────────────────────────────────────────────────────────────────────
# STEP 8: Severity breakdown table
# Shows count and % of events in each severity class per SPI scale
# ─────────────────────────────────────────────────────────────────────────────

severity_summary <- all_drought_events %>%
  filter(!if_any(everything(), is.infinite)) %>%
  count(spi_scale, severity_class) %>%
  group_by(spi_scale) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  ungroup() %>%
  pivot_wider(
    names_from  = severity_class,
    values_from = c(n, pct),
    values_fill = 0
  ) %>%
  arrange(spi_scale)

cat("\n=== SEVERITY BREAKDOWN ===\n\n")
print(severity_summary, width = Inf)

# ─────────────────────────────────────────────────────────────────────────────
# STEP 9: Spatial summary — drought frequency per pixel
# How many drought events occurred at each pixel for each SPI scale
# Useful for mapping drought hotspots
# ─────────────────────────────────────────────────────────────────────────────

pixel_drought_freq <- all_drought_events %>%
  filter(!if_any(everything(), is.infinite)) %>%
  group_by(x, y, spi_scale) %>%
  summarise(
    n_events        = n(),
    mean_duration   = round(mean(duration_months, na.rm = TRUE), 1),
    mean_peak_spi   = round(mean(peak_spi,         na.rm = TRUE), 2),
    total_months_in_drought = sum(duration_months, na.rm = TRUE),
    .groups = "drop"
  )

cat("\n=== PIXEL DROUGHT FREQUENCY (first 10 rows) ===\n\n")
print(head(pixel_drought_freq, 10))

# ─────────────────────────────────────────────────────────────────────────────
# STEP 10: Plot spatial drought frequency maps
# One map per SPI scale showing number of events per pixel
# ─────────────────────────────────────────────────────────────────────────────

plot_drought_frequency <- function(pixel_freq, scale_name) {
  
  df <- pixel_freq %>% filter(spi_scale == scale_name)
  
  ggplot(df, aes(x = x, y = y, fill = n_events)) +
    geom_tile() +
    scale_fill_distiller(palette = "YlOrRd", direction = 1,
                         name = "No. of\nevents") +
    coord_equal() +
    labs(
      title    = paste(scale_name, "— Drought event frequency per pixel"),
      subtitle = sprintf("Method: %s | Start: %.1f | Min duration: %d months",
                         drought_config$method,
                         drought_config$start_threshold,
                         drought_config$min_duration),
      x = "Longitude",
      y = "Latitude"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title    = element_text(face = "bold"),
      legend.position = "right"
    )
}

p_freq_spi1  <- plot_drought_frequency(pixel_drought_freq, "SPI1")
p_freq_spi3  <- plot_drought_frequency(pixel_drought_freq, "SPI3")
p_freq_spi6  <- plot_drought_frequency(pixel_drought_freq, "SPI6")
p_freq_spi12 <- plot_drought_frequency(pixel_drought_freq, "SPI12")

(p_freq_spi1 + p_freq_spi3) / (p_freq_spi6 + p_freq_spi12) +
  plot_annotation(
    title    = "Spatial Drought Frequency — Pixel-by-Pixel Detection",
    subtitle = sprintf("Total pixels: %s | Period: 2000–2025",
                       format(total_pixels, big.mark = ",")),
    theme = theme(
      plot.title    = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(size = 10, color = "grey40")
    )
  )

ggsave("./Figures/drought_frequency_maps_pixelbypixel.png",
       width = 14, height = 10, dpi = 300)

# ─────────────────────────────────────────────────────────────────────────────
# STEP 11: Save outputs
# ─────────────────────────────────────────────────────────────────────────────

# All individual pixel-level events
saveRDS(all_drought_events,   "./Data/all_drought_events_pixelbypixel.rds")
write.csv(all_drought_events, "./Data/all_drought_events_pixelbypixel.csv",
          row.names = FALSE)

# Summary tables
write.csv(drought_summary,    "./Data/drought_summary_pixelbypixel.csv",
          row.names = FALSE)
write.csv(severity_summary,   "./Data/severity_summary_pixelbypixel.csv",
          row.names = FALSE)
write.csv(pixel_drought_freq, "./Data/pixel_drought_frequency.csv",
          row.names = FALSE)

message("\n=== All outputs saved ===")
message("  all_drought_events_pixelbypixel.rds  — full event list per pixel")
message("  drought_summary_pixelbypixel.csv     — summary across all pixels")
message("  severity_summary_pixelbypixel.csv    — severity breakdown")
message("  pixel_drought_frequency.csv          — events per pixel per scale")
message("  drought_frequency_maps_pixelbypixel.png — spatial maps")