# =============================================================================
# PERIOD COMPARISON — drought frequency and characteristics
# Period 1: 2000-2010   Period 2: 2011-2021
# =============================================================================

library(dplyr)
library(ggplot2)
library(tidyr)
library(patchwork)

# ── Define periods ────────────────────────────────────────────────────────────
PERIOD_1_START <- 1980
PERIOD_1_END   <- 2000
PERIOD_2_START <- 2001
PERIOD_2_END   <- 2021
N_YEARS        <- 20

# ── Assign each event to a period ────────────────────────────────────────────
events_with_period <- all_drought_events %>%
  filter(!if_any(everything(), is.infinite)) %>%
  filter(!is.na(country)) %>%
  mutate(
    event_year = year(start_date),
    period     = case_when(
      event_year >= PERIOD_1_START & event_year <= PERIOD_1_END ~ "2000-2010",
      event_year >= PERIOD_2_START & event_year <= PERIOD_2_END ~ "2011-2021",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(period))

cat(sprintf("Events in Period 1 (2000-2010): %d\n",
            sum(events_with_period$period == "2000-2010")))
cat(sprintf("Events in Period 2 (2011-2021): %d\n\n",
            sum(events_with_period$period == "2011-2021")))

# =============================================================================
# STEP 1 — Period summary at pixel level then country level
# =============================================================================

period_comparison <- events_with_period %>%
  
  # ── Stage 1 — pixel level ──────────────────────────────────────────────────
  group_by(
    config_name, index, method,
    start_thr, min_duration,
    country, period, x, y
  ) %>%
  summarise(
    pixel_n_events      = n(),
    pixel_mean_duration = mean(duration_months,  na.rm = TRUE),
    pixel_mean_peak_spi = mean(peak_spi,         na.rm = TRUE),
    pixel_mean_severity = mean(total_severity,   na.rm = TRUE),
    pixel_mean_cum_neg  = mean(min_cum_deficit,  na.rm = TRUE),
    .groups = "drop"
  ) %>%
  
  # ── Stage 2 — country level ────────────────────────────────────────────────
  group_by(
    config_name, index, method,
    start_thr, min_duration,
    country, period
  ) %>%
  summarise(
    pixels_with_drought  = n(),
    
    # Annual frequency — mean events per pixel divided by N_YEARS
    annual_freq_events   = round(mean(pixel_n_events,      na.rm = TRUE) /
                                   N_YEARS, 2),
    
    mean_duration_months = round(mean(pixel_mean_duration, na.rm = TRUE), 1),
    mean_peak_spi        = round(mean(pixel_mean_peak_spi, na.rm = TRUE), 2),
    mean_total_severity  = round(mean(pixel_mean_severity, na.rm = TRUE), 2),
    mean_cum_neg         = round(mean(pixel_mean_cum_neg,  na.rm = TRUE), 2),
    .groups = "drop"
  ) %>%
  left_join(pixels_per_country, by = "country") %>%
  mutate(
    pct_pixels_affected = round(
      100 * pixels_with_drought / total_pixels_country, 1)
  ) %>%
  select(-total_pixels_country)

cat("=== PERIOD COMPARISON — PREVIEW ===\n\n")
period_comparison %>%
  filter(country == "Benin",
         index   == "spi1",
         method  == "cumulative") %>%
  print(width = Inf)

# =============================================================================
# STEP 2 — Compute change between periods
# =============================================================================

period_change <- period_comparison %>%
  select(config_name, index, method, start_thr,
         min_duration, country, period,
         annual_freq_events, mean_duration_months,
         mean_peak_spi, mean_total_severity,
         pct_pixels_affected) %>%
  pivot_wider(
    names_from  = period,
    values_from = c(annual_freq_events, mean_duration_months,
                    mean_peak_spi, mean_total_severity,
                    pct_pixels_affected)
  ) %>%
  mutate(
    # Absolute change
    change_freq     = `annual_freq_events_2011-2021`   -
      `annual_freq_events_2000-2010`,
    change_duration = `mean_duration_months_2011-2021` -
      `mean_duration_months_2000-2010`,
    change_severity = `mean_total_severity_2011-2021`  -
      `mean_total_severity_2000-2010`,
    change_pct_pix  = `pct_pixels_affected_2011-2021`  -
      `pct_pixels_affected_2000-2010`,
    
    # Relative change %
    pct_change_freq = round(
      100 * change_freq / `annual_freq_events_2000-2010`, 1)
  )

cat("\n=== PERIOD CHANGE — PREVIEW ===\n\n")
period_change %>%
  filter(country == "Benin",
         index   == "spi1",
         method  == "cumulative") %>%
  select(config_name, country,
         `annual_freq_events_2000-2010`,
         `annual_freq_events_2011-2021`,
         change_freq, pct_change_freq) %>%
  print(width = Inf)

# =============================================================================
# STEP 3 — SPI monthly exceedance rates
# Key argument: threshold exceedance is stable, event frequency is not
# =============================================================================

spi_exceedance <- pixel_data %>%
  filter(!is.na(country)) %>%
  mutate(
    year   = year(date),
    period = case_when(
      year >= PERIOD_1_START & year <= PERIOD_1_END ~ "2000-2010",
      year >= PERIOD_2_START & year <= PERIOD_2_END ~ "2011-2021",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(period)) %>%
  group_by(country, period) %>%
  summarise(
    n_months        = n(),
    pct_below_0     = round(100 * mean(spi1 < 0,    na.rm = TRUE), 1),
    pct_below_neg05 = round(100 * mean(spi1 < -0.5, na.rm = TRUE), 1),
    pct_below_neg1  = round(100 * mean(spi1 < -1.0, na.rm = TRUE), 1),
    .groups = "drop"
  )

cat("\n=== SPI MONTHLY EXCEEDANCE RATES BY PERIOD ===\n")
cat("(should be stable — SPI is standardised)\n\n")
print(spi_exceedance, n = Inf)

# =============================================================================
# STEP 4 — Plots: one per country
# Left  — grouped bar chart: period 1 vs period 2 per config
# Right — SPI exceedance rates: stable across periods
# =============================================================================

countries <- sort(unique(period_comparison$country))

for (cntry in countries) {
  
  cat(sprintf("\nBuilding period comparison plot: %s\n", cntry))
  
  df_plot <- period_comparison %>%
    filter(country == cntry) %>%
    arrange(index, method, start_thr) %>%
    mutate(
      x_pos  = as.integer(factor(config_name,
                                 levels = unique(config_name))),
      period = factor(period, levels = c("2000-2010", "2011-2021"))
    )
  
  n_configs <- n_distinct(df_plot$config_name)
  
  # ── Grouped bar chart — annual frequency ─────────────────────────────────
  p_freq <- ggplot(
    df_plot,
    aes(x = x_pos, y = mean_duration_months, # annual_freq_events,
        fill = period)          # ← removed alpha = index
  ) +
    geom_col(
      position = position_dodge(width = 0.75),
      width    = 0.7,
      colour   = "white"
    ) +
    geom_text(
      aes(label = sprintf("%.2f", annual_freq_events)),
      position = position_dodge(width = 0.75),
      angle    = 90,        # ← rotate 90 degrees
      hjust    = -0.1,      # ← push label above bar
      vjust = -0.4, size = 2.8, colour = "grey30"
    ) +
    scale_fill_manual(
      values = c("2000-2010" = "#5C9BD6",
                 "2011-2021" = "#E67E22"),
      name = "Period"
    ) +
    # ← removed scale_alpha_manual entirely
    scale_x_continuous(
      limits = c(0.5, n_configs + 0.5),
      expand = c(0, 0),
      breaks = 1:n_configs
    ) +
    scale_y_continuous(
      expand = expansion(mult = c(0, 0.2))
    ) +
    labs(x = NULL, y = "Annual events per pixel") +
    theme_minimal(base_size = 11) +
    theme(
      axis.text.x        = element_blank(),
      axis.ticks.x       = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank(),
      legend.position    = "top",
      plot.margin        = margin(10, 10, 0, 10)
    )
  # ── Parameter grid ────────────────────────────────────────────────────────
  config_ref <- df_plot %>%
    select(x_pos, config_name, index, method, start_thr) %>%
    distinct() %>%
    arrange(x_pos)
  
  param_long <- bind_rows(
    config_ref %>% transmute(x_pos, param = "SPI",
                             value = gsub("spi|spei", "", index)),
    config_ref %>% transmute(x_pos, param = "Method",
                             value = case_when(
                               method == "cumulative"      ~ "C",
                               method == "threshold_entry" ~ "E",
                               method == "threshold_peak"  ~ "P"
                             )),
    config_ref %>% transmute(x_pos, param = "Threshold",
                             value = sprintf("%.1f", start_thr))
  ) %>%
    mutate(param = factor(param,
                          levels = rev(c("SPI", "Method", "Threshold"))))
  
  p_grid <- ggplot(param_long, aes(x = x_pos, y = param)) +
    geom_tile(fill = "grey97", colour = "grey88",
              linewidth = 0.4, width = 0.88, height = 0.88) +
    geom_text(aes(label = value), colour = "grey20",
              size = 3, fontface = "bold") +
    geom_hline(yintercept = 1.5, colour = "grey70",
               linewidth = 0.4, linetype = "dashed") +
    scale_x_continuous(limits = c(0.5, n_configs + 0.5),
                       expand = c(0, 0)) +
    scale_y_discrete(expand = c(0.1, 0.1)) +
    labs(x = NULL, y = NULL,
         caption = "Method: C = Cumulative   E = Threshold entry   P = Threshold peak") +
    theme_minimal(base_size = 10) +
    theme(
      axis.text.x   = element_blank(),
      axis.ticks    = element_blank(),
      axis.text.y   = element_text(colour = "grey40", size = 9,
                                   face = "bold", hjust = 1),
      panel.grid    = element_blank(),
      plot.caption  = element_text(colour = "grey50", size = 8,
                                   hjust = 0),
      plot.margin   = margin(0, 10, 5, 10)
    )
  
  # ── SPI exceedance panel ──────────────────────────────────────────────────
  exc_df <- spi_exceedance %>%
    filter(country == cntry) %>%
    pivot_longer(
      cols      = starts_with("pct_"),
      names_to  = "threshold",
      values_to = "pct"
    ) %>%
    mutate(
      threshold = case_when(
        threshold == "pct_below_0"     ~ "SPI < 0",
        threshold == "pct_below_neg05" ~ "SPI < -0.5",
        threshold == "pct_below_neg1"  ~ "SPI < -1.0"
      ),
      threshold = factor(threshold,
                         levels = c("SPI < 0",
                                    "SPI < -0.5",
                                    "SPI < -1.0")),
      period = factor(period, levels = c("2000-2010", "2011-2021"))
    )
  
  p_exceedance <- ggplot(
    exc_df,
    aes(x = threshold, y = pct, fill = period)
  ) +
    geom_col(
      position = position_dodge(width = 0.6),
      width    = 0.5,
      colour   = "white"
    ) +
    geom_text(
      aes(label = sprintf("%.1f%%", pct)),
      position = position_dodge(width = 0.6),
      vjust = -0.4, size = 3
    ) +
    scale_fill_manual(
      values = c("2000-2010" = "#5C9BD6",
                 "2011-2021" = "#E67E22"),
      name = "Period"
    ) +
    scale_y_continuous(
      limits = c(0, 65),
      expand = expansion(mult = c(0, 0.1)),
      labels = function(x) paste0(x, "%")
    ) +
    labs(
      title    = "SPI monthly exceedance rates",
      subtitle = paste0(
        "Rates stable across periods — SPI is standardised\n",
        "Event frequency changes due to clustering and recovery"
      ),
      x = NULL,
      y = "% of months"
    ) +
    theme_minimal(base_size = 10) +
    theme(
      plot.title         = element_text(face = "bold", size = 10),
      plot.subtitle      = element_text(size = 8, colour = "grey40",
                                        lineheight = 1.3),
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank(),
      legend.position    = "top",
      plot.margin        = margin(10, 10, 5, 10)
    )
  
  # ── Combine all panels ────────────────────────────────────────────────────
  p_final <- (p_freq / p_grid) +
    plot_layout(widths = c(2.5, 1)) +
    plot_annotation(
      title    = sprintf("Period comparison — %s", cntry),
      subtitle = sprintf(
        paste0("Left: annual event frequency per pixel by config",
               " | Right: SPI monthly exceedance rates\n",
               "%d–%d vs %d–%d (%d years each)"),
        PERIOD_1_START, PERIOD_1_END,
        PERIOD_2_START, PERIOD_2_END,
        N_YEARS
      ),
      theme = theme(
        plot.title    = element_text(face = "bold", size = 13),
        plot.subtitle = element_text(size = 9, colour = "grey40",
                                     lineheight = 1.4)
      )
    )
  
  ggsave(
    sprintf("./Figures/period_comparison_%s.png",
            gsub(" ", "_", cntry)),
    plot   = p_final,
    width  = max(14, n_configs * 0.6 + 5),
    height = 9,
    dpi    = 200
  )
  
  cat(sprintf("  Saved: period_comparison_%s.png\n",
              gsub(" ", "_", cntry)))
}

message("\nAll period comparison plots saved.")