# =============================================================================
# TARGETED PIXEL CHECK
# Pull all events for a specific pixel and config and plot
# =============================================================================

# ── Set your target pixel and config here ────────────────────────────────────
TARGET_X      <- 2.625
TARGET_Y      <- 9.875
TARGET_INDEX  <- "spi1"
TARGET_METHOD <- "threshold_entry"
TARGET_THR    <- -0.0000000001
TARGET_DUR    <- 1

# ── Pull all events for this pixel and config ─────────────────────────────────
all_pixel_events <- all_drought_events %>%
  filter(
    x            == TARGET_X,
    y            == TARGET_Y,
    index        == TARGET_INDEX,
    method       == TARGET_METHOD,
    abs(start_thr - TARGET_THR) < 0.001,
    min_duration == TARGET_DUR
  ) %>%
  arrange(start_date)

cat(sprintf("=== ALL EVENTS at pixel (%.3f, %.3f) ===\n\n",
            TARGET_X, TARGET_Y))
cat(sprintf("Config: %s | %s | thr=%.4f | dur=%d\n\n",
            TARGET_INDEX, TARGET_METHOD,
            TARGET_THR, TARGET_DUR))
cat(sprintf("Total events detected: %d\n\n",
            nrow(all_pixel_events)))

all_pixel_events %>%
  select(start_date, end_date, duration_months,
         entry_spi, peak_spi, severity_class) %>%
  print(n = Inf)

# ── Pull full time series ─────────────────────────────────────────────────────
ts_start <- min(all_pixel_events$start_date) %m-% months(3)
ts_end   <- max(all_pixel_events$end_date)   %m+% months(3)

ts <- pixel_data %>%
  filter(x == TARGET_X, y == TARGET_Y) %>%
  arrange(date) %>%
  select(date, value = all_of(TARGET_INDEX)) %>%
  filter(date >= ts_start & date <= ts_end) %>%
  mutate(below_thr = value < TARGET_THR)

cat(sprintf("\nTime series: %s to %s (%d months)\n\n",
            ts_start, ts_end, nrow(ts)))
print(ts, n = Inf)

# ── Build event bands ─────────────────────────────────────────────────────────
event_bands <- all_pixel_events %>%
  mutate(
    xmin  = start_date,
    xmax  = end_date + days(29),
    label = sprintf("E%d\n%s", drought_id,
                    format(start_date, "%b %y"))
  )

# ── Plot ──────────────────────────────────────────────────────────────────────
p <- ggplot(ts, aes(x = date, y = value)) +
  
  # All event bands
  geom_rect(
    data        = event_bands,
    aes(xmin = xmin, xmax = xmax,
        ymin = -Inf, ymax = Inf),
    inherit.aes = FALSE,
    fill        = "#FAEEDA",
    alpha       = 0.6
  ) +
  
  # Reference lines
  geom_hline(yintercept = 0,
             colour = "grey60", linewidth = 0.4) +
  geom_hline(yintercept = TARGET_THR,
             colour = "#E53935", linewidth = 0.5,
             linetype = "dashed") +
  
  # Time series line and points
  geom_line(colour = "grey50", linewidth = 0.7) +
  geom_point(colour = "grey50", size = 2,
             shape = 21, fill = "white", stroke = 0.8) +
  
  # Value labels
  geom_text(
    aes(label = sprintf("%.2f", value)),
    colour = "grey30", size = 2.5,
    vjust  = -0.8, hjust = 0.5
  ) +
  
  # Highlight months inside events
#  geom_line(
#    data      = ts %>%
#      inner_join(
#        all_pixel_events %>%
#          select(start_date, end_date) %>%
#          rowwise() %>%
#          mutate(date = list(seq(start_date, end_date,
 #                                by = "month"))) %>%
 #         unnest(date),
#        by = "date"
 #     ),
#    colour    = "#E67E22",
#    linewidth = 1.0
#  ) +
  geom_point(
    data = ts %>%
      inner_join(
        all_pixel_events %>%
          select(start_date, end_date) %>%
          rowwise() %>%
          mutate(date = list(seq(start_date, end_date,
                                 by = "month"))) %>%
          unnest(date),
        by = "date"
      ),
    colour = "#E67E22",
    size   = 3,
    shape  = 19
  ) +
 # geom_text(
 #   data = ts %>%
  #    inner_join(
  #      all_pixel_events %>%
   #       select(start_date, end_date) %>%
   #       rowwise() %>%
  #        mutate(date = list(seq(start_date, end_date,
  #                               by = "month"))) %>%
  #        unnest(date),
  #      by = "date"
  #    ),
  #  aes(label = sprintf("%.2f", value)),
  #  colour   = "#E67E22",
  #  size     = 2.8,
  #  vjust    = -0.9,
  #  fontface = "bold"
 # ) +
  
  # Event ID labels
  geom_label(
    data = event_bands,
    aes(x     = xmin + (xmax - xmin) / 2,
        y     = Inf,
        label = label),
    inherit.aes = FALSE,
    vjust       = 1.1,
    size        = 2.5,
    colour      = "#633806",
    fill        = "white",
    label.size  = 0.2
  ) +
  
  # Monthly x-axis
  scale_x_date(
    date_breaks = "1 month",
    date_labels = "%b\n%Y",
    expand      = c(0.02, 0)
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0.15, 0.25))
  ) +
  
  labs(
    title    = sprintf(
      "All events | %s | %s | thr=%.4f | dur=%d | (%.3f, %.3f)",
      toupper(TARGET_INDEX), TARGET_METHOD,
      TARGET_THR, TARGET_DUR,
      TARGET_X, TARGET_Y
    ),
    subtitle = sprintf(
      "Total events: %d | Period: %s to %s",
      nrow(all_pixel_events),
      format(ts_start, "%b %Y"),
      format(ts_end,   "%b %Y")
    ),
    x = NULL,
    y = toupper(TARGET_INDEX)
  ) +
  
  theme_minimal(base_size = 10) +
  theme(
    plot.title         = element_text(face = "bold", size = 11),
    plot.subtitle      = element_text(size = 9, colour = "grey40"),
    axis.text.x        = element_text(angle = 90, hjust = 1,
                                      vjust = 0.5, size = 7.5),
    axis.ticks.x       = element_line(colour = "grey70",
                                      linewidth = 0.3),
    panel.grid.major.x = element_line(colour = "grey92",
                                      linewidth = 0.3),
    panel.grid.minor   = element_blank()
  )

print(p)

ggsave(
  sprintf("./Figures/pixel_check_%.3f_%.3f_%s_%s.png",
          TARGET_X, TARGET_Y,
          TARGET_INDEX, TARGET_METHOD),
  plot   = p,
  width  = max(14, as.numeric(ts_end - ts_start) / 30 * 0.6),
  height = 6,
  dpi    = 150
)

message("Plot saved.")