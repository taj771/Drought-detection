# =============================================================================
# BAR PLOTS WITH PARAMETER GRID — multiple metrics
# One plot per country per metric
# =============================================================================

library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)
library(stringr)

textcol  <- "grey40"
idx_cols <- c(
  spi1  = "#378ADD", spi3  = "#EF9F27", spi6  = "#1D9E75",
  spei1 = "#8E44AD", spei3 = "#E74C3C", spei6 = "#16A085"
)

method_colours  <- c(
  cumulative      = "#534AB7",
  threshold_entry = "#993C1D",
  threshold_peak  = "#085041"
)
method_linetypes <- c(
  cumulative      = "dashed",
  threshold_entry = "dotdash",
  threshold_peak  = "dotted"
)
method_short <- c(
  cumulative      = "Cumulative",
  threshold_entry = "Thr entry",
  threshold_peak  = "Thr peak"
)

# ── Define metrics to plot ────────────────────────────────────────────────────
metrics <- list(
  list(
    col      = "annual_freq_events",
    label    = "Mean annual events per pixel",
    filename = "annual_freq"
  ),
  list(
    col      = "annual_mean_severity",
    label    = "Mean annual total severity (SPI sum)",
    filename = "annual_severity"
  ),
  list(
    col      = "annual_mean_cum_spi_neg",
    label    = "Mean annual cumulative negative SPI",
    filename = "annual_cum_spi_neg"
  )
)

countries <- sort(unique(drought_annual_summary$country[
  !is.na(drought_annual_summary$country)]))

# ── Loop over countries and metrics ──────────────────────────────────────────
for (cntry in countries) {
  for (metric in metrics) {
    
    cat(sprintf("\nBuilding: %s | %s\n", cntry, metric$col))
    
    df <- drought_annual_summary %>%
      filter(country == cntry) %>%
      arrange(index, method, start_thr, min_duration) %>%
      mutate(
        x        = row_number(),
        metric_val = .data[[metric$col]]
      )
    
    if (nrow(df) == 0 || all(is.na(df$metric_val))) next
    
    n            <- nrow(df)
    overall_mean <- mean(df$metric_val, na.rm = TRUE)
    
    # ── Group means per method ──────────────────────────────────────────────
    method_means <- df %>%
      group_by(method) %>%
      summarise(
        group_mean = mean(metric_val, na.rm = TRUE),
        x_min      = min(x) - 0.4,
        x_max      = max(x) + 0.4,
        .groups    = "drop"
      )
    
    # ── Bar chart panel ─────────────────────────────────────────────────────
    p_bars <- ggplot(df, aes(x = x, y = metric_val, fill = index)) +
      geom_col(width = 0.75, colour = "white") +
      geom_text(
        aes(label = sprintf("%.2f", metric_val)),
        vjust  = -0.4, size = 3, colour = "grey30"
      ) +
      
      # Group mean lines — one per method
     # geom_segment(
     #   data = method_means,
     #   aes(x      = x_min, xend   = x_max,
     #       y      = group_mean, yend = group_mean,
     #       colour = method, linetype = method),
    #    linewidth   = 0.8,
    #    inherit.aes = FALSE
    #  ) +
      
      # Group mean labels
   #   geom_text(
    #    data = method_means,
   #     aes(x      = x_max + 0.1,
    #        y      = group_mean,
    #        label  = sprintf("%s: %.2f",
    #                         method_short[method],
    #                         group_mean),
    #        colour = method),
    #    hjust       = 0,
    #    vjust       = -0.4,
    #    size        = 2.8,
    #    inherit.aes = FALSE
    #  ) +
      
      scale_fill_manual(values  = idx_cols,        name = "Index") +
      scale_colour_manual(values  = method_colours,  name = "Method mean") +
      scale_linetype_manual(values = method_linetypes, name = "Method mean") +
      scale_x_continuous(
        limits = c(0.5, n + 0.5),
        expand = c(0, 0),
        breaks = 1:n
      ) +
      scale_y_continuous(
        expand = expansion(mult = c(0, 0.25)),
        breaks = scales::pretty_breaks(n = 5)
      ) +
      labs(x = NULL, y = metric$label) +
      theme_minimal(base_size = 11) +
      theme(
        axis.text.x        = element_blank(),
        axis.ticks.x       = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor   = element_blank(),
        legend.position    = "top",
        legend.direction   = "horizontal",
        plot.margin        = margin(10, 130, 0, 10)
      )
    
    # ── Parameter grid panel ────────────────────────────────────────────────
    param_long <- bind_rows(
      df %>% transmute(x, param = "SPI",
                       value = gsub("spi|spei", "", index),
                       grp   = "index"),
      df %>% transmute(x, param = "Method",
                       value = method_short[method],
                       grp   = "method"),
      df %>% transmute(x, param = "Threshold",
                       value = sprintf("%.1f", start_thr),
                       grp   = "thr")
    ) %>%
      mutate(param = factor(param,
                            levels = rev(c("SPI","Method","Threshold"))))%>%
      mutate(value = ifelse(value == "Cumulative", "c", value))%>%
      mutate(value = ifelse(value == "Thr entry", "E", value))
    
    
    p_grid <- ggplot(param_long, aes(x = x, y = param)) +
      geom_tile(
        fill = "grey97", colour = "grey88",
        linewidth = 0.4, width = 0.88, height = 0.88
      ) +
      geom_text(aes(label = value), colour = "grey20", size = 3) +
      geom_hline(yintercept = 1.5,
                 colour = "grey70", linewidth = 0.4,
                 linetype = "dashed") +
      scale_x_continuous(
        limits = c(0.5, n + 0.5),
        expand = c(0, 0),
        breaks = 1:n
      ) +
      scale_y_discrete(expand = c(0.1, 0.1)) +
      labs(x = NULL, y = NULL) +
      theme_minimal(base_size = 10) +
      theme(
        axis.text.x  = element_blank(),
        axis.ticks   = element_blank(),
        axis.text.y  = element_text(colour = textcol, size = 9,
                                    face = "bold", hjust = 1),
        panel.grid   = element_blank(),
        plot.margin  = margin(0, 130, 0, 10)
      )
    
    # ── Method notes panel ──────────────────────────────────────────────────
    note_text <- paste0(
      "Cumulative: drought persists until cumulative SPI since onset >= 0.  ",
      "Thr entry: ends when SPI recovers above the fixed starting threshold.  ",
      "Thr peak: ends when SPI recovers above the most negative value reached."
    )
    
    p_note <- ggplot() +
      annotate("text",
               x = 0, y = 0.5,
               label      = str_wrap(note_text, width = 120),
               hjust      = 0, vjust = 0.5,
               size       = 3, colour = "grey50",
               lineheight = 1.4) +
      scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
      scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
      theme_void() +
      theme(plot.margin = margin(4, 10, 4, 10))
    
    # ── Combine ─────────────────────────────────────────────────────────────
    p_final <- p_bars / p_grid / p_note +
      plot_layout(heights = c(3, 1.2, 0.5)) +
      plot_annotation(
        title    = sprintf("%s — %s", metric$label, cntry),
        subtitle = sprintf("%d configurations | bars coloured by index", n),
        theme    = theme(
          plot.title    = element_text(face = "bold", size = 13,
                                       colour = textcol),
          plot.subtitle = element_text(size = 9, colour = "grey50")
        )
      )
    
    ggsave(
      sprintf("./Figures/%s_%s.png",
              metric$filename,
              gsub(" ", "_", cntry)),
      plot   = p_final,
      width  = max(10, n * 0.55),
      height = 8,
      dpi    = 200
    )
    
    cat(sprintf("  Saved: %s_%s.png\n",
                metric$filename,
                gsub(" ", "_", cntry)))
  }
}

message("\nAll metric plots saved.")