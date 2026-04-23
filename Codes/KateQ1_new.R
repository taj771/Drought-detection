# =============================================================================
# MULTI-INDEX COMPARISON
# definition vs raw month events across SPI1, SPI3, SPI6
# =============================================================================

TARGET_METHOD  <- "threshold_entry"
TARGET_THR     <- -0.5
TARGET_DUR     <- 1
TARGET_INDICES <- c("spi1", "spi3", "spi6")

multi_index_summary <- map_dfr(TARGET_INDICES, function(idx) {
  
  all_drought_events %>%
    filter(!if_any(everything(), is.infinite)) %>%
    filter(!is.na(country)) %>%
    filter(
      index        == idx,
      method       == TARGET_METHOD,
      abs(start_thr - TARGET_THR) < 0.001,
      min_duration == TARGET_DUR
    ) %>%
    mutate(
      event_year        = year(start_date),
      definition_events = 1,
      raw_month_events  = duration_months  # ← same logic for all indices
    ) %>%
    
    # ── Stage 1 — pixel level per year ─────────────────────────────────────
    group_by(index, country, event_year, x, y) %>%
    summarise(
      pixel_def_events  = sum(definition_events),
      pixel_raw_events  = sum(raw_month_events),
      pixel_dur_def     = mean(duration_months, na.rm = TRUE),
      pixel_peak_def    = mean(peak_spi,        na.rm = TRUE),
      .groups = "drop"
    ) %>%
    
    # ── Stage 2 — country level per year ───────────────────────────────────
    group_by(index, country, event_year) %>%
    summarise(
      n_pixels          = n(),
      def_mean_events   = round(mean(pixel_def_events, na.rm = TRUE), 2),
      def_mean_duration = round(mean(pixel_dur_def,    na.rm = TRUE), 1),
      def_mean_peak_spi = round(mean(pixel_peak_def,   na.rm = TRUE), 2),
      raw_mean_events   = round(mean(pixel_raw_events, na.rm = TRUE), 2),
      raw_mean_duration = 1,
      raw_mean_peak_spi = round(mean(pixel_peak_def,   na.rm = TRUE), 2),
      ratio             = round(
        mean(pixel_raw_events, na.rm = TRUE) /
          mean(pixel_def_events, na.rm = TRUE), 2),
      .groups = "drop"
    )
})

# ── Overall mean across all years per index per country ───────────────────────
multi_index_overall <- multi_index_summary %>%
  group_by(index, country) %>%
  summarise(
    def_mean_events   = round(mean(def_mean_events,   na.rm = TRUE), 2),
    def_mean_duration = round(mean(def_mean_duration, na.rm = TRUE), 1),
    def_mean_peak_spi = round(mean(def_mean_peak_spi, na.rm = TRUE), 2),
    raw_mean_events   = round(mean(raw_mean_events,   na.rm = TRUE), 2),
    raw_mean_duration = 1,
    raw_mean_peak_spi = round(mean(raw_mean_peak_spi, na.rm = TRUE), 2),
    mean_ratio        = round(mean(ratio,             na.rm = TRUE), 2),
    .groups = "drop"
  ) %>%
  mutate(
    index = toupper(index)
  ) %>%
  arrange(country, index)

cat("=== MULTI-INDEX COMPARISON ===\n\n")
cat(sprintf("Method: %s | Threshold: %.1f | Min dur: %d\n\n",
            TARGET_METHOD, TARGET_THR, TARGET_DUR))

cat("--- Definition-based vs Raw month-based ---\n\n")

multi_index_overall %>%
  select(country, index,
         def_mean_events, def_mean_duration, def_mean_peak_spi,
         raw_mean_events, raw_mean_duration, raw_mean_peak_spi,
         mean_ratio) %>%
  print(width = Inf)





# =============================================================================
# OPTION A — three metric panels, definition vs raw, across SPI1/SPI3/SPI6
# =============================================================================

library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)

textcol <- "grey40"

# ── Reshape to long format for plotting ───────────────────────────────────────
plot_long <- multi_index_overall %>%
  select(country, index,
         def_mean_events, raw_mean_events,
         def_mean_duration, raw_mean_duration,
         def_mean_peak_spi, raw_mean_peak_spi) %>%
  pivot_longer(
    cols      = -c(country, index),
    names_to  = "series",
    values_to = "value"
  ) %>%
  mutate(
    approach = ifelse(grepl("^def_", series), "Definition", "Raw month"),
    metric   = case_when(
      grepl("events",   series) ~ "Mean events\nper pixel per year",
      grepl("duration", series) ~ "Mean duration\n(months)",
      grepl("peak_spi", series) ~ "Mean peak SPI"
    ),
    index    = factor(toupper(index),
                      levels = c("SPI1", "SPI3", "SPI6")),
    approach = factor(approach,
                      levels = c("Definition", "Raw month")),
    # For peak SPI use absolute value for easier reading
    value    = ifelse(grepl("peak", series), abs(value), value)
  )

# ── Colours ───────────────────────────────────────────────────────────────────
approach_cols <- c("Definition" = "#378ADD",
                   "Raw month"  = "#EF9F27")

# ── One plot per metric ───────────────────────────────────────────────────────
make_metric_plot <- function(metric_label, y_label,
                             free_y = TRUE) {
  
  df <- plot_long %>%
    filter(metric == metric_label)
  
  ggplot(df, aes(x = index, y = value,
                 fill = approach)) +
    geom_col(
      position = position_dodge(width = 0.7),
      width    = 0.65,
      colour   = "white"
    ) +
    geom_text(
      aes(label = sprintf("%.2f", value)),
      position = position_dodge(width = 0.7),
      vjust    = -0.4,
      size     = 2.8,
      colour   = "grey30"
    ) +
    scale_fill_manual(values = approach_cols, name = NULL) +
    scale_y_continuous(
      expand = expansion(mult = c(0, 0.2)),
      breaks = scales::pretty_breaks(n = 5)
    ) +
    facet_wrap(~ country, nrow = 1) +
    labs(
      title = metric_label,
      x     = NULL,
      y     = y_label
    ) +
    theme_minimal(base_size = 10) +
    theme(
      plot.title         = element_text(face = "bold", size = 11,
                                        colour = textcol),
      axis.text.x        = element_text(size = 9, colour = textcol),
      axis.text.y        = element_text(size = 8, colour = textcol),
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank(),
      strip.text         = element_text(face = "bold", size = 9),
      legend.position    = "none",
      plot.margin        = margin(8, 8, 4, 8)
    )
}

p1 <- make_metric_plot("Mean events\nper pixel per year",
                       "events / pixel / yr")
p2 <- make_metric_plot("Mean duration\n(months)",
                       "months")
p3 <- make_metric_plot("Mean peak SPI",
                       "mean |peak SPI|")

# ── Shared legend ─────────────────────────────────────────────────────────────
legend_plot <- ggplot(
  data.frame(approach = c("Definition", "Raw month"),
             x = 1, y = 1),
  aes(x = x, y = y, fill = approach)
) +
  geom_col() +
  scale_fill_manual(values = approach_cols, name = "Approach") +
  theme_void() +
  theme(
    legend.position  = "top",
    legend.direction = "horizontal",
    legend.text      = element_text(size = 10, colour = textcol),
    legend.title     = element_text(size = 10, colour = textcol)
  )

shared_legend <- cowplot::get_legend(legend_plot)

# ── Combine ───────────────────────────────────────────────────────────────────
p_final <- p1 | p2 | p3 +
  plot_layout(guides = "collect") +
  plot_annotation(
    title    = "Definition-based vs raw month counting",
    subtitle = sprintf(
      "Method: %s | Threshold: %.1f | Indices: SPI-1, SPI-3, SPI-6",
      TARGET_METHOD, TARGET_THR
    ),
    caption  = paste0(
      "Definition groups consecutive months into one event. ",
      "Raw treats each month independently (duration always = 1). ",
      "Ratio = mean definition duration."
    ),
    theme = theme(
      plot.title    = element_text(face = "bold", size = 13,
                                   colour = textcol),
      plot.subtitle = element_text(size = 9, colour = "grey50"),
      plot.caption  = element_text(size = 8, colour = "grey50",
                                   hjust = 0)
    )
  ) &
  theme(legend.position = "top")

ggsave(
  "./Figures/def_vs_raw_multiindex.png",
  plot   = p_final,
  width  = 14,
  height = 5,
  dpi    = 200
)

message("Saved: def_vs_raw_multiindex.png")