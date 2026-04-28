# =============================================================================
# MULTI-COUNTRY DROUGHT TIME PLOT
# Shows ALL detected events across ALL pixels per country
# Summarised as % pixels affected per month
# =============================================================================

TARGET_INDEX  <- "spi3"
TARGET_METHOD <- "threshold_entry"
TARGET_THR    <- -0.5
#TARGET_DUR    <- 1

MONTHS_PER_PAGE <- 24
PLOT_WIDTH      <- 16
PLOT_HEIGHT     <- 3.5

severity_fills <- c(
  "Mild"     = "#FFF9C4",
  "Moderate" = "#FFCC80",
  "Severe"   = "#EF9A9A",
  "Extreme"  = "#CE93D8"
)




all_drought_events <- readRDS( "./Data/all_drought_events_allconfigs.rds")%>%
  filter(!(index %in% c("spi3", "spi6") & method == "cumulative"))



# ── Step 1 — expand all events to month level ─────────────────────────────────
# One row per drought month per pixel
all_drought_months <- all_drought_events %>%
  filter(!if_any(everything(), is.infinite)) %>%
  filter(!is.na(country)) %>%
  filter(
    index        == TARGET_INDEX,
    method       == TARGET_METHOD,
    abs(start_thr - TARGET_THR) < 0.001
 #   min_duration == TARGET_DUR
  ) %>%
  rowwise() %>%
  mutate(date = list(seq(start_date, end_date, by = "month"))) %>%
  unnest(date) %>%
  select(date, x, y, country, severity_class,
         peak_spi, duration_months)

# ── Step 2 — monthly country summary ─────────────────────────────────────────
# For each month — what % of pixels were in drought
# and what was the dominant severity

monthly_summary <- all_drought_months %>%
  group_by(country, date) %>%
  summarise(
    n_drought_pixels = n_distinct(paste(x, y)),
    mean_peak_spi    = round(mean(peak_spi, na.rm = TRUE), 2),
    # Dominant severity — most common class that month
    dominant_severity = names(sort(table(severity_class),
                                   decreasing = TRUE))[1],
    .groups = "drop"
  ) %>%
  left_join(pixels_per_country, by = "country") %>%
  mutate(
    pct_pixels_drought = round(
      100 * n_drought_pixels / total_pixels_country, 1),
    dominant_severity = factor(dominant_severity,
                               levels = c("Mild", "Moderate",
                                          "Severe", "Extreme"))
  )

# ── Step 3 — set shared x axis ───────────────────────────────────────────────
x_min <- min(monthly_summary$date) %m-% months(1)
x_max <- max(monthly_summary$date) %m+% months(1)

n_months <- interval(x_min, x_max) %/% months(1)
n_pages  <- ceiling(n_months / MONTHS_PER_PAGE)
countries <- sort(unique(monthly_summary$country))

cat(sprintf("Countries : %d\n", length(countries)))
cat(sprintf("Months    : %d → %d pages\n\n", n_months, n_pages))

# ── Step 4 — loop over pages ──────────────────────────────────────────────────
for (page_i in seq_len(n_pages)) {
  
  chunk_start <- x_min %m+% months((page_i - 1) * MONTHS_PER_PAGE)
  chunk_end   <- min(x_max,
                     x_min %m+% months(page_i * MONTHS_PER_PAGE))
  
  cat(sprintf("Page %d/%d: %s to %s\n",
              page_i, n_pages,
              format(chunk_start, "%b %Y"),
              format(chunk_end,   "%b %Y")))
  
  plot_list <- list()
  
  for (cntry in countries) {
    
    df <- monthly_summary %>%
      filter(
        country == cntry,
        date >= chunk_start,
        date <= chunk_end
      )
    
    if (nrow(df) == 0) next
    
    p <- ggplot(df, aes(x = date, y = pct_pixels_drought)) +
      
      # ── Coloured bars by dominant severity ─────────────────────────────
      geom_col(
        aes(fill = dominant_severity),
        width = 28,   # days — roughly one month
        colour = "white",
        linewidth = 0.2
      ) +
      
      scale_fill_manual(
        values   = severity_fills,
        name     = "Dominant severity",
        drop     = FALSE,
        na.value = "grey90"
      ) +
      
      # ── Reference line at 50% ───────────────────────────────────────────
      geom_hline(yintercept = 50,
                 colour = "grey60", linewidth = 0.4,
                 linetype = "dashed") +
      
      # ── Fixed x axis ────────────────────────────────────────────────────
      scale_x_date(
        date_breaks = "1 month",
        date_labels = "%b\n%Y",
        limits      = c(chunk_start, chunk_end),
        expand      = c(0.01, 0)
      ) +
      
      scale_y_continuous(
        limits = c(0, 100),
        breaks = c(0, 25, 50, 75, 100),
        labels = function(x) paste0(x, "%"),
        expand = expansion(mult = c(0, 0.05))
      ) +
      
      labs(
        title = sprintf("%s", cntry),
        x     = NULL,
        y     = "% pixels\nin drought"
      ) +
      
      theme_minimal(base_size = 9) +
      theme(
        plot.title         = element_text(face = "bold", size = 10,
                                          colour = "grey20"),
        axis.text.x        = element_text(angle = 90, hjust = 1,
                                          vjust = 0.5, size = 6.5),
        axis.text.y        = element_text(size = 8),
        axis.ticks.x       = element_line(colour = "grey70",
                                          linewidth = 0.3),
        panel.grid.major.x = element_line(colour = "grey92",
                                          linewidth = 0.3),
        panel.grid.minor   = element_blank(),
        legend.position    = "none"
      )
    
    plot_list[[cntry]] <- p
  }
  
  if (length(plot_list) == 0) next
  
  # ── Stack country plots ───────────────────────────────────────────────────
  p_combined <- wrap_plots(plot_list, ncol = 1) +
    plot_annotation(
      title   = sprintf(
        "%s | %s | thr=%.1f | %s to %s   [page %d / %d]",
        toupper(TARGET_INDEX), TARGET_METHOD, TARGET_THR,
        format(chunk_start, "%b %Y"),
        format(chunk_end,   "%b %Y"),
        page_i, n_pages
      ),
      caption = paste0(
        "Bar height = % of country pixels in drought that month  |  ",
        "Colour = dominant severity class  |  ",
        "Dashed line = 50% of pixels"
      ),
      theme = theme(
        plot.title   = element_text(face = "bold", size = 12),
        plot.caption = element_text(size = 8, colour = "grey50",
                                    hjust = 0)
      )
    )
  
  ggsave(
    sprintf("./Figures/country_drought_timeline_%s_p%02d.png",
            TARGET_INDEX, page_i),
    plot   = p_combined,
    width  = PLOT_WIDTH,
    height = length(plot_list) * PLOT_HEIGHT + 0.8,
    dpi    = 150
  )
  
  cat(sprintf("  Saved: country_drought_timeline_%s_p%02d.png\n",
              TARGET_INDEX, page_i))
}

message(sprintf("\n=== Done — %d pages saved ===", n_pages))