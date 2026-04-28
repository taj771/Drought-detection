###############################################################################
# SPI-3 + 3 consecutive months  vs  SPI-6 direct
# Testing Kate's argument across ALL pixels
# Full pipeline including function definition
###############################################################################

library(sparklyr)
library(dplyr)
library(ggplot2)
library(patchwork)
library(purrr)
library(tidyr)
library(lubridate)

# =============================================================================
# STEP 1 — Connect to Databricks
# =============================================================================

sc <- spark_connect(
  cluster_id = Sys.getenv("DATABRICKS_CLUSTER_ID"),
  method     = "databricks_connect"
)

project.folder <- "xx_drought_index"

spi <- tbl(sc, in_catalog("projects", project.folder, "spi_results"))

# =============================================================================
# STEP 2 — Pull data locally
# =============================================================================

message("Pulling SPI data from Databricks...")

pixel_data <- spi %>%
  arrange(x, y, year, month) %>%
  collect() %>%
  mutate(
    date  = as.Date(paste(year, month, "01", sep = "-")),
    month = as.integer(month),
    year  = as.integer(year)
  )

cat(sprintf("Rows pulled  : %s\n",
            format(nrow(pixel_data), big.mark = ",")))
cat(sprintf("Unique pixels: %s\n",
            format(n_distinct(paste(pixel_data$x, pixel_data$y)),
                   big.mark = ",")))
cat(sprintf("Columns      : %s\n\n",
            paste(names(pixel_data), collapse = ", ")))

# Get unique pixel list
pixels <- pixel_data %>%
  select(x, y) %>%
  distinct() %>%
  arrange(x, y)

total_pixels <- nrow(pixels)
cat(sprintf("Total pixels to process: %d\n\n", total_pixels))

# =============================================================================
# STEP 3 — Define detect_droughts function
# =============================================================================

detect_droughts <- function(spi_vec, dates, config) {
  
  if (is.null(dates))
    stop("'dates' is NULL — check column name")
  if (length(dates) != length(spi_vec))
    stop("'dates' and 'spi_vec' must be same length")
  
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
    
    # Check min_dur consecutive months all below start threshold
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
  ) %>%
    filter(!is.na(event_id))
  
  events <- monthly_track %>%
    group_by(drought_id = event_id) %>%
    summarise(
      start_date      = min(event_date),
      end_date        = max(event_date),
      duration_months = n(),
      peak_spi        = min(spi,         na.rm = TRUE),
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

# Confirm function loaded
cat(sprintf("detect_droughts loaded: %s\n\n", exists("detect_droughts")))

# =============================================================================
# STEP 4 — Verify function works on one pixel before running all
# =============================================================================

message("=== STEP 4: Verifying function on one pixel ===\n")

px_test <- pixels$x[1]
py_test <- pixels$y[1]

test_ts <- pixel_data %>%
  filter(x == px_test, y == py_test) %>%
  arrange(date)

cat(sprintf("Test pixel   : x=%.4f  y=%.4f\n", px_test, py_test))
cat(sprintf("Rows         : %d\n", nrow(test_ts)))
cat(sprintf("Date range   : %s to %s\n\n",
            min(test_ts$date), max(test_ts$date)))

# Test with min_duration = 1, 2, 3 to understand the data
for (min_dur in c(1, 2, 3)) {
  events <- detect_droughts(
    spi_vec = test_ts$spi3,
    dates   = test_ts$date,
    config  = list(
      method          = "threshold",
      start_threshold = -0.5,
      end_threshold   = -0.4999,
      min_duration    = min_dur
    )
  )
  cat(sprintf(
    "SPI-3 | min_duration=%d | events detected: %d\n",
    min_dur, nrow(events)
  ))
}

cat("\n")

# Test SPI-6 as well
for (min_dur in c(1, 2, 3)) {
  events <- detect_droughts(
    spi_vec = test_ts$spi6,
    dates   = test_ts$date,
    config  = list(
      method          = "threshold",
      start_threshold = -0.5,
      end_threshold   = -0.4999,
      min_duration    = min_dur
    )
  )
  cat(sprintf(
    "SPI-6 | min_duration=%d | events detected: %d\n",
    min_dur, nrow(events)
  ))
}

# =============================================================================
# STEP 5 — Define the two definitions to compare
#
# Definition A: SPI-3 + 3 consecutive months below threshold
#               Kate's argument: this ≈ SPI-6 directly
#
# Definition B: SPI-6 with simple threshold, no consecutive rule
#               The direct simpler equivalent Kate is suggesting
# =============================================================================

config_A <- list(
  label           = "SPI3_3month_rule",
  spi_col         = "spi3",
  method          = "threshold",
  start_threshold = -0.5,
  end_threshold   = -0.4999,
  min_duration    = 3
)

config_B <- list(
  label           = "SPI6_direct",
  spi_col         = "spi6",
  method          = "threshold",
  start_threshold = -0.5,
  end_threshold   = -0.4999,
  min_duration    = 1
)

# =============================================================================
# STEP 6 — Run both definitions across all pixels
# =============================================================================

message("\n=== STEP 6: Running comparison across all pixels ===\n")
message(sprintf(
  "Definition A: %s | Definition B: %s\n",
  config_A$label, config_B$label
))

tick <- max(1, floor(total_pixels / 20))

all_results <- map_dfr(seq_len(total_pixels), function(i) {
  
  px <- pixels$x[i]
  py <- pixels$y[i]
  
  if (i %% tick == 0 || i == total_pixels) {
    message(sprintf("  Pixel %d / %d  (%.0f%%)",
                    i, total_pixels, 100 * i / total_pixels))
  }
  
  # Extract time series for this pixel
  pixel_ts <- pixel_data %>%
    filter(x == px, y == py) %>%
    arrange(date)
  
  if (nrow(pixel_ts) < 12) return(tibble())
  
  # Run both definitions
  map_dfr(list(config_A, config_B), function(cfg) {
    
    spi_vec <- pixel_ts[[cfg$spi_col]]
    dates   <- pixel_ts$date
    
    if (all(is.na(spi_vec))) return(tibble())
    
    events <- tryCatch(
      detect_droughts(
        spi_vec = spi_vec,
        dates   = dates,
        config  = list(
          method          = cfg$method,
          start_threshold = cfg$start_threshold,
          end_threshold   = cfg$end_threshold,
          min_duration    = cfg$min_duration
        )
      ),
      error = function(e) tibble()
    )
    
    # Return per-pixel per-definition summary
    if (nrow(events) == 0) {
      tibble(
        x                    = px,
        y                    = py,
        definition           = cfg$label,
        n_events             = 0L,
        total_drought_months = 0L,
        mean_duration        = NA_real_,
        mean_peak_spi        = NA_real_,
        n_mild               = 0L,
        n_moderate           = 0L,
        n_severe             = 0L,
        n_extreme            = 0L
      )
    } else {
      events %>%
        filter(!if_any(everything(), is.infinite)) %>%
        summarise(
          n_events             = n(),
          total_drought_months = sum(duration_months,  na.rm = TRUE),
          mean_duration        = round(mean(duration_months, na.rm = TRUE), 1),
          mean_peak_spi        = round(mean(peak_spi,   na.rm = TRUE), 2),
          n_mild               = sum(severity_class == "Mild",     na.rm = TRUE),
          n_moderate           = sum(severity_class == "Moderate", na.rm = TRUE),
          n_severe             = sum(severity_class == "Severe",   na.rm = TRUE),
          n_extreme            = sum(severity_class == "Extreme",  na.rm = TRUE)
        ) %>%
        mutate(
          x          = px,
          y          = py,
          definition = cfg$label
        )
    }
  })
})

message(sprintf("\nResults rows: %s\n",
                format(nrow(all_results), big.mark = ",")))

# =============================================================================
# STEP 7 — Overall summary across all pixels
# =============================================================================

message("=== STEP 7: Overall summary ===\n")

overall_summary <- all_results %>%
  filter(!if_any(everything(), is.infinite)) %>%
  group_by(definition) %>%
  summarise(
    total_events            = sum(n_events,             na.rm = TRUE),
    total_drought_months    = sum(total_drought_months, na.rm = TRUE),
    mean_events_per_pixel   = round(mean(n_events,      na.rm = TRUE), 2),
    mean_duration_months    = round(mean(mean_duration, na.rm = TRUE), 1),
    mean_peak_spi           = round(mean(mean_peak_spi, na.rm = TRUE), 2),
    pixels_with_drought     = sum(n_events > 0),
    pct_pixels_with_drought = round(100 * sum(n_events > 0) /
                                      total_pixels, 1),
    .groups = "drop"
  )

cat("=== OVERALL SUMMARY ===\n\n")
print(overall_summary, width = Inf)

# =============================================================================
# STEP 8 — Pivot wide and calculate agreement per pixel
# =============================================================================

results_wide <- all_results %>%
  pivot_wider(
    id_cols     = c(x, y),
    names_from  = definition,
    values_from = c(n_events, total_drought_months,
                    mean_duration, mean_peak_spi,
                    n_mild, n_moderate, n_severe, n_extreme)
  ) %>%
  rename_with(~ gsub(" ", "_", .x)) %>%
  mutate(
    diff_n_events = n_events_SPI3_3month_rule - n_events_SPI6_direct,
    diff_months   = total_drought_months_SPI3_3month_rule -
      total_drought_months_SPI6_direct,
    event_agree   = abs(diff_n_events) <= 1,
    month_agree   = abs(diff_months)   <= 3,
    fully_agree   = event_agree & month_agree
  )

# =============================================================================
# STEP 9 — Agreement statistics
# =============================================================================

cat("\n=== AGREEMENT BETWEEN DEFINITIONS ===\n\n")

agreement_summary <- results_wide %>%
  summarise(
    total_pixels          = n(),
    pct_fully_agree       = round(100 * mean(fully_agree,   na.rm = TRUE), 1),
    pct_event_agree       = round(100 * mean(event_agree,   na.rm = TRUE), 1),
    pct_month_agree       = round(100 * mean(month_agree,   na.rm = TRUE), 1),
    mean_diff_events      = round(mean(diff_n_events,       na.rm = TRUE), 2),
    mean_diff_months      = round(mean(diff_months,         na.rm = TRUE), 2),
    pixels_A_more_events  = sum(diff_n_events > 0, na.rm = TRUE),
    pixels_B_more_events  = sum(diff_n_events < 0, na.rm = TRUE),
    pixels_equal          = sum(diff_n_events == 0, na.rm = TRUE)
  )

print(agreement_summary, width = Inf)

cat(sprintf(
  "\n  %.1f%% of pixels agree within 1 event\n",
  agreement_summary$pct_fully_agree
))
cat(sprintf(
  "  SPI-3+rule detects more events in %d pixels\n",
  agreement_summary$pixels_A_more_events
))
cat(sprintf(
  "  SPI-6 direct detects more events in %d pixels\n",
  agreement_summary$pixels_B_more_events
))
cat(sprintf(
  "  Both detect equal events in %d pixels\n\n",
  agreement_summary$pixels_equal
))

if (agreement_summary$pct_fully_agree > 85) {
  cat("CONCLUSION: Definitions are largely equivalent\n")
  cat("Kate's argument is supported — consider using SPI-6 directly\n")
} else {
  cat("CONCLUSION: Definitions give meaningfully different results\n")
  cat("Consecutive rule adds genuine value beyond SPI-6\n")
}

# =============================================================================
# STEP 10 — Wet month interruption check across all pixels
# =============================================================================

message("\n=== STEP 10: Wet month interruption check ===\n")

interruption_counts <- map_dfr(seq_len(total_pixels), function(i) {
  
  px <- pixels$x[i]
  py <- pixels$y[i]
  
  pixel_ts <- pixel_data %>%
    filter(x == px, y == py) %>%
    arrange(date)
  
  if (nrow(pixel_ts) < 6) return(tibble())
  
  n_interrupts <- pixel_ts %>%
    mutate(
      below    = spi3 < -0.5,
      prev1    = lag(below,  1),
      prev2    = lag(below,  2),
      above    = !below,
      next1    = lead(below, 1),
      is_inter = !is.na(prev2) & !is.na(prev1) &
        !is.na(next1) &
        prev2 & prev1 & above & next1
    ) %>%
    summarise(n_interruptions = sum(is_inter, na.rm = TRUE))
  
  tibble(
    x               = px,
    y               = py,
    n_interruptions = n_interrupts$n_interruptions
  )
})

cat(sprintf(
  "Pixels with at least one wet month interruption: %d / %d  (%.1f%%)\n",
  sum(interruption_counts$n_interruptions > 0),
  total_pixels,
  100 * mean(interruption_counts$n_interruptions > 0)
))
cat(sprintf(
  "Total interruptions across all pixels          : %d\n\n",
  sum(interruption_counts$n_interruptions)
))

# =============================================================================
# STEP 11 — Plots
# =============================================================================

message("=== STEP 11: Generating plots ===\n")

# Plot A — Histogram of event counts per definition
p_events <- ggplot(
  all_results %>% filter(!if_any(everything(), is.infinite)),
  aes(x = n_events, fill = definition)
) +
  geom_histogram(bins = 20, alpha = 0.6,
                 position = "identity", colour = "white") +
  scale_fill_manual(
    values = c(
      "SPI3_3month_rule" = "#5C9BD6",
      "SPI6_direct"      = "#E67E22"
    ),
    labels = c(
      "SPI3_3month_rule" = "SPI-3 + 3 month rule",
      "SPI6_direct"      = "SPI-6 direct"
    ),
    name = NULL
  ) +
  labs(
    title    = "Drought events per pixel — both definitions",
   # subtitle = sprintf(
   #  "Agreement: %.1f%% of pixels within 1 event",
   #   agreement_summary$pct_fully_agree
   # ),
    x = "Number of drought events",
    y = "Number of pixels"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title      = element_text(face = "bold"),
    legend.position = "top"
  )

# Plot B — Scatter: A vs B per pixel
p_scatter <- ggplot(
  results_wide,
  aes(
    x      = n_events_SPI6_direct,
    y      = n_events_SPI3_3month_rule,
    colour = fully_agree
  )
) +
  geom_point(alpha = 0.4, size = 1.5) +
  geom_abline(slope = 1, intercept = 0,
              colour = "firebrick", linetype = "dashed") +
  scale_colour_manual(
    values = c("TRUE" = "#27AE60", "FALSE" = "#E74C3C"),
    labels = c("TRUE" = "Agree", "FALSE" = "Disagree"),
    name   = NULL
  ) +
  labs(
    title    = "Event count per pixel — A vs B",
    subtitle = "Points on dashed line = perfect agreement",
    x        = "SPI-6 direct — n events",
    y        = "SPI-3 + 3 month rule — n events"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title      = element_text(face = "bold"),
    legend.position = "top"
  )

# Plot C — Spatial map of difference
p_map <- ggplot(
  results_wide,
  aes(x = x, y = y, fill = diff_n_events)
) +
  geom_tile() +
  scale_fill_gradient2(
    low      = "#2196F3",
    mid      = "white",
    high     = "#E53935",
    midpoint = 0,
    name     = "Difference\n(A minus B)"
  ) +
  coord_equal() +
  labs(
    title    = "Spatial map — event count difference (A minus B)",
    subtitle = "Blue = SPI-6 detects more | Red = SPI-3+rule detects more | White = agree",
    x        = "Longitude",
    y        = "Latitude"
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"))

# Combine all plots
(p_events + p_scatter) / p_map +
  plot_annotation(
    title    = "Kate's question: SPI-3 + 3 consecutive months  vs  SPI-6 direct",
    subtitle = sprintf(
      "Total pixels: %d  |  Agreement: %.1f%%  |  Mean event difference: %.2f per pixel",
      total_pixels,
      agreement_summary$pct_fully_agree,
      agreement_summary$mean_diff_events
    ),
    theme = theme(
      plot.title    = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(size = 10, colour = "grey40")
    )
  )

ggsave("./Figures/spi3_rule_vs_spi6_comparison.png",
       width = 14, height = 12, dpi = 300)

# =============================================================================
# STEP 12 — Save all outputs
# =============================================================================

write.csv(overall_summary,
          "./Data/comparison_overall_summary.csv",
          row.names = FALSE)

write.csv(all_results,
          "./Data/comparison_all_pixels_long.csv",
          row.names = FALSE)

write.csv(results_wide,
          "./Data/comparison_all_pixels_wide.csv",
          row.names = FALSE)

write.csv(agreement_summary,
          "./Data/comparison_agreement_summary.csv",
          row.names = FALSE)

write.csv(interruption_counts,
          "./Data/wet_month_interruptions_all_pixels.csv",
          row.names = FALSE)

message("\n=== All done ===")
message("Outputs saved:")
message("  comparison_overall_summary.csv         — overall by definition")
message("  comparison_all_pixels_long.csv         — per pixel per definition")
message("  comparison_all_pixels_wide.csv         — per pixel side by side")
message("  comparison_agreement_summary.csv       — agreement statistics")
message("  wet_month_interruptions_all_pixels.csv — interruption counts")
message("  spi3_rule_vs_spi6_comparison.png       — plots")





### Country disaggregate plots 



# ── Get unique x, y, country from pixel_data ────────────────────────────────

pixels_with_country <- pixel_data %>%
  select(x, y, country) %>%
  distinct()

cat("Unique pixels with country:\n")
print(table(pixels_with_country$country, useNA = "always"))
cat(sprintf("\nTotal unique pixels: %d\n\n", nrow(pixels_with_country)))

# ── Join onto all_results and results_wide ───────────────────────────────────

all_results <- all_results %>%
  left_join(pixels_with_country, by = c("x", "y"))

results_wide <- results_wide %>%
  left_join(pixels_with_country, by = c("x", "y"))

# ── Verify ───────────────────────────────────────────────────────────────────

cat("Country column in all_results:\n")
print(table(all_results$country, useNA = "always"))

cat("\nCountry column in results_wide:\n")
print(table(results_wide$country, useNA = "always"))

n_na <- sum(is.na(all_results$country))
cat(sprintf("\nRows with NA country: %d (%.1f%%)\n",
            n_na, 100 * n_na / nrow(all_results)))

# ── Country level agreement summary ─────────────────────────────────────────
country_agreement <- results_wide %>%
  group_by(country) %>%
  summarise(
    total_pixels        = n(),
    pct_fully_agree     = round(100 * mean(fully_agree,   na.rm = TRUE), 1),
    mean_diff_events    = round(mean(diff_n_events,       na.rm = TRUE), 2),
    mean_diff_months    = round(mean(diff_months,         na.rm = TRUE), 2),
    pixels_A_more       = sum(diff_n_events > 0, na.rm = TRUE),
    pixels_B_more       = sum(diff_n_events < 0, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(mean_diff_events))

cat("\n=== AGREEMENT BY COUNTRY ===\n\n")
print(country_agreement, width = Inf)

# =============================================================================
# PLOT A — Histogram of event counts per definition faceted by country
# =============================================================================

p_events <- ggplot(
  all_results %>%
    filter(!if_any(everything(), is.infinite)) %>%
    filter(!is.na(country)),
  aes(x = n_events, fill = definition)
) +
  geom_histogram(bins = 20, alpha = 0.6,
                 position = "identity", colour = "white") +
  scale_fill_manual(
    values = c(
      "SPI3_3month_rule" = "#5C9BD6",
      "SPI6_direct"      = "#E67E22"
    ),
    labels = c(
      "SPI3_3month_rule" = "SPI-3 + 3 month rule",
      "SPI6_direct"      = "SPI-6 direct"
    ),
    name = NULL
  ) +
  facet_wrap(~ country, scales = "free_y", ncol = 3) +
  labs(
    title = "Drought events per pixel — both definitions by country",
    x     = "Number of drought events",
    y     = "Number of pixels"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title      = element_text(face = "bold"),
    legend.position = "top",
    strip.text      = element_text(face = "bold", size = 10)
  )

# =============================================================================
# PLOT B — Scatter: A vs B per pixel faceted by country
# =============================================================================

p_scatter <- ggplot(
  results_wide %>% filter(!is.na(country)),
  aes(
    x      = n_events_SPI6_direct,
    y      = n_events_SPI3_3month_rule,
    colour = fully_agree
  )
) +
  geom_point(alpha = 0.4, size = 1.2) +
  geom_abline(slope = 1, intercept = 0,
              colour = "firebrick", linetype = "dashed") +
  scale_colour_manual(
    values = c("TRUE" = "#27AE60", "FALSE" = "#E74C3C"),
    labels = c("TRUE" = "Agree", "FALSE" = "Disagree"),
    name   = NULL
  ) +
  facet_wrap(~ country, scales = "free", ncol = 3) +
  labs(
    title    = "Event count per pixel — A vs B by country",
    subtitle = "Points on dashed line = perfect agreement",
    x        = "SPI-6 direct — n events",
    y        = "SPI-3 + 3 month rule — n events"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title      = element_text(face = "bold"),
    legend.position = "top",
    strip.text      = element_text(face = "bold", size = 10)
  )

# =============================================================================
# PLOT C — Spatial maps of difference per country
# One map per country showing where definitions agree/disagree
# =============================================================================

# Get country list for looping
country_list <- sort(unique(results_wide$country[!is.na(results_wide$country)]))

# Build one spatial map per country
spatial_plots <- lapply(country_list, function(cn) {
  
  df <- results_wide %>% filter(country == cn)
  
  # Agreement stats for subtitle
  pct_agree  <- round(100 * mean(df$fully_agree, na.rm = TRUE), 1)
  mean_diff  <- round(mean(df$diff_n_events, na.rm = TRUE), 1)
  
  ggplot(df, aes(x = x, y = y, fill = diff_n_events)) +
    geom_tile() +
    scale_fill_gradient2(
      low      = "#2196F3",
      mid      = "white",
      high     = "#E53935",
      midpoint = 0,
      name     = "Diff\n(A-B)"
    ) +
    coord_equal() +
    labs(
      title    = cn,
      subtitle = sprintf("Agreement: %.1f%%  |  Mean diff: %.1f events",
                         pct_agree, mean_diff),
      x = "Longitude",
      y = "Latitude"
    ) +
    theme_minimal(base_size = 10) +
    theme(
      plot.title    = element_text(face = "bold", size = 10),
      plot.subtitle = element_text(size = 8, colour = "grey40"),
      legend.position = "right",
      legend.key.size = unit(0.4, "cm"),
      legend.text     = element_text(size = 7)
    )
})

# Combine spatial maps using patchwork
spatial_combined <- wrap_plots(spatial_plots, ncol = 3) +
  plot_annotation(
    title    = "Spatial difference maps by country (A minus B)",
    subtitle = "Blue = SPI-6 detects more | Red = SPI-3+rule detects more | White = agree",
    theme    = theme(
      plot.title    = element_text(face = "bold", size = 13),
      plot.subtitle = element_text(size = 9, colour = "grey40")
    )
  )

# =============================================================================
# PLOT D — Country level summary bar chart
# Mean events per pixel per country per definition
# =============================================================================

country_mean_events <- all_results %>%
  filter(!if_any(everything(), is.infinite)) %>%
  filter(!is.na(country)) %>%
  group_by(country, definition) %>%
  summarise(
    mean_events   = round(mean(n_events,      na.rm = TRUE), 1),
    mean_peak_spi = round(mean(mean_peak_spi, na.rm = TRUE), 2),
    mean_duration = round(mean(mean_duration, na.rm = TRUE), 1),
    .groups = "drop"
  )

p_country_bar <- ggplot(
  country_mean_events,
  aes(x = reorder(country, mean_events),
      y = mean_events,
      fill = definition)
) +
  geom_col(position = "dodge", width = 0.7, colour = "white") +
  geom_text(
    aes(label = mean_events),
    position = position_dodge(width = 0.7),
    hjust    = -0.2,
    size     = 3
  ) +
  scale_fill_manual(
    values = c(
      "SPI3_3month_rule" = "#5C9BD6",
      "SPI6_direct"      = "#E67E22"
    ),
    labels = c(
      "SPI3_3month_rule" = "SPI-3 + 3 month rule",
      "SPI6_direct"      = "SPI-6 direct"
    ),
    name = NULL
  ) +
  coord_flip() +
  labs(
    title    = "Mean drought events per pixel by country",
    subtitle = "Shows which countries have the largest difference between definitions",
    x        = NULL,
    y        = "Mean events per pixel"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title      = element_text(face = "bold"),
    legend.position = "top",
    panel.grid.major.y = element_blank()
  )

# Peak SPI by country
p_country_spi <- ggplot(
  country_mean_events,
  aes(x = reorder(country, mean_peak_spi),
      y = mean_peak_spi,
      fill = definition)
) +
  geom_col(position = "dodge", width = 0.7, colour = "white") +
  scale_fill_manual(
    values = c(
      "SPI3_3month_rule" = "#5C9BD6",
      "SPI6_direct"      = "#E67E22"
    ),
    labels = c(
      "SPI3_3month_rule" = "SPI-3 + 3 month rule",
      "SPI6_direct"      = "SPI-6 direct"
    ),
    name = NULL
  ) +
  coord_flip() +
  labs(
    title    = "Mean peak SPI by country",
    subtitle = "More negative = more severe events detected",
    x        = NULL,
    y        = "Mean peak SPI"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title         = element_text(face = "bold"),
    legend.position    = "top",
    panel.grid.major.y = element_blank()
  )

# =============================================================================
# Save all plots
# =============================================================================

# Histogram by country
ggsave("./Figures/histogram_by_country.png",
       plot  = p_events,
       width = 14, height = 10, dpi = 300)

# Scatter by country
ggsave("./Figures/scatter_by_country.png",
       plot  = p_scatter,
       width = 14, height = 10, dpi = 300)

# Spatial maps by country
ggsave("./Figures/spatial_maps_by_country.png",
       plot  = spatial_combined,
       width = 16, height = 12, dpi = 300)

# Country bar charts
ggsave("./Figures/country_summary_bars.png",
       plot  = (p_country_bar / p_country_spi),
       width = 10, height = 10, dpi = 300)

message("All country-level plots saved.")