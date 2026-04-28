# =============================================================================
# SANITY CHECK — random single month events
# =============================================================================

n_pixels        <- 5
MONTHS_PER_PAGE <- 12
PLOT_WIDTH      <- 16
PLOT_HEIGHT     <- 7

severity_fills <- c(
  "Mild"     = "#FFF9C4",
  "Moderate" = "#FFCC80",
  "Severe"   = "#EF9A9A",
  "Extreme"  = "#CE93D8"
)

# ── Pick n random pixels that have at least one single month event ────────────
random_picks <- all_drought_events %>%
  filter(!if_any(everything(), is.infinite)) %>%
  filter(!is.na(country)) %>%
  filter(duration_months == 1) %>%
  select(x, y, country, config_name, index,
         method, start_thr, min_duration) %>%
  distinct() %>%
  slice_sample(n = min(n_pixels, nrow(.)))

cat(sprintf("=== PLOTTING %d RANDOM SINGLE-MONTH EVENT PIXELS ===\n\n",
            nrow(random_picks)))

for (pick_i in seq_len(nrow(random_picks))) {
  
  # ── These were missing from your version ─────────────────────────────────
  rp  <- random_picks[pick_i, ]
  idx <- rp$index
  
  cat(sprintf("\n--- Pick %d / %d ---\n", pick_i, nrow(random_picks)))
  cat(sprintf("Pixel  : x=%.3f  y=%.3f | %s\n", rp$x, rp$y, rp$country))
  cat(sprintf("Config : %s\n", rp$config_name))
  
  # ── Pull all events for this pixel + config ───────────────────────────────
  all_pixel_events <- all_drought_events %>%
    filter(
      x           == rp$x,
      y           == rp$y,
      config_name == rp$config_name
    ) %>%
    arrange(start_date) %>%
    mutate(
      is_single = duration_months == 1,
      label     = sprintf("E%d\n%s\n%d mo%s",
                          drought_id,
                          format(start_date, "%b %y"),
                          duration_months,
                          ifelse(duration_months == 1, "\n★", ""))
    )
  
  cat(sprintf("Total events       : %d\n", nrow(all_pixel_events)))
  cat(sprintf("Single month events: %d (%.1f%%)\n",
              sum(all_pixel_events$is_single),
              100 * mean(all_pixel_events$is_single)))
  
  # ── Full time series ───────────────────────────────────────────────────────
  ts_start <- min(all_pixel_events$start_date) %m-% months(3)
  ts_end   <- max(all_pixel_events$end_date)   %m+% months(3)
  
  ts_full <- pixel_data %>%
    filter(x == rp$x, y == rp$y) %>%
    arrange(date) %>%
    select(date, value = all_of(idx)) %>%
    filter(date >= ts_start & date <= ts_end)
  
  # ── Mark event months ──────────────────────────────────────────────────────
  event_months <- all_pixel_events %>%
    rowwise() %>%
    mutate(date = list(seq(start_date, end_date, by = "month"))) %>%
    unnest(date) %>%
    select(date, drought_id, severity_class, is_single)
  
  # ── Build event bands ──────────────────────────────────────────────────────
  event_bands <- all_pixel_events %>%
    mutate(
      xmin = start_date,
      xmax = end_date + days(29)
    )
  
  # ── Window centred around first single month event ─────────────────────────
  first_single <- all_pixel_events %>%
    filter(is_single) %>%
    slice_min(start_date, n = 1)
  
  chunk_start <- max(ts_start,
                     first_single$start_date %m-% months(6))
  chunk_end   <- min(ts_end,
                     chunk_start %m+% months(MONTHS_PER_PAGE))
  
  ts_chunk <- ts_full %>%
    filter(date >= chunk_start & date <= chunk_end) %>%
    left_join(event_months, by = "date") %>%
    mutate(in_event = !is.na(drought_id))
  
  event_bands_chunk <- event_bands %>%
    filter(xmin <= chunk_end & xmax >= chunk_start) %>%
    mutate(
      xmin_clip = pmax(xmin, chunk_start),
      xmax_clip = pmin(xmax, chunk_end + days(29)),
      label_x   = xmin_clip + (xmax_clip - xmin_clip) / 2
    )
  
  if (nrow(ts_chunk) == 0) {
    cat("  No data in window — skipping\n")
    next
  }
  
  # ── Plot ───────────────────────────────────────────────────────────────────
  p <- ggplot(ts_chunk, aes(x = date, y = value)) +
    
    geom_rect(
      data        = event_bands_chunk,
      aes(xmin = xmin_clip, xmax = xmax_clip,
          ymin = -Inf, ymax = Inf,
          fill = severity_class),
      inherit.aes = FALSE,
      alpha       = 0.4
    ) +
    
    geom_rect(
      data = event_bands_chunk %>% filter(is_single),
      aes(xmin = xmin_clip, xmax = xmax_clip,
          ymin = -Inf, ymax = Inf),
      inherit.aes = FALSE,
      fill        = NA,
      colour      = "#E53935",
      linewidth   = 1.0,
      linetype    = "solid"
    ) +
    
    scale_fill_manual(values = severity_fills, name = "Severity") +
    
    geom_hline(yintercept = 0,
               colour = "grey60", linewidth = 0.4) +
    geom_hline(yintercept = rp$start_thr,
               colour = "#E53935", linewidth = 0.5,
               linetype = "dashed") +
    
    geom_line(colour = "grey40", linewidth = 0.6) +
    geom_point(colour = "grey40", size = 1.8,
               shape = 21, fill = "white", stroke = 0.8) +
    geom_text(
      aes(label = sprintf("%.2f", value)),
      colour = "grey30", size = 2.3,
      vjust = -0.8, hjust = 0.5
    ) +
    
    geom_label(
      data = event_bands_chunk %>%
        filter(label_x >= chunk_start & label_x <= chunk_end),
      aes(x      = label_x,
          y      = Inf,
          label  = label,
          colour = ifelse(is_single, "#A32D2D", "grey20")),
      inherit.aes = FALSE,
      vjust      = 1.1, size = 2.3,
      fill       = "white",
      label.size = 0.2,
      lineheight = 0.9
    ) +
    scale_colour_identity() +
    
    scale_x_date(
      date_breaks = "1 month",
      date_labels = "%b\n%Y",
      limits      = c(chunk_start, chunk_end),
      expand      = c(0.02, 0)
    ) +
    scale_y_continuous(
      expand = expansion(mult = c(0.15, 0.28))
    ) +
    
    labs(
      title = sprintf(
        "Pick %d | %s | %s | lon=%.3f lat=%.3f | %s",
        pick_i, toupper(idx), rp$config_name,
        rp$x, rp$y, rp$country
      ),
      subtitle = sprintf(
        "%s to %s | Method: %s | Thr: %.4f | Total events: %d | Single month: %d (%.1f%%) | ★ = single month",
        format(chunk_start, "%b %Y"),
        format(chunk_end,   "%b %Y"),
        rp$method, rp$start_thr,
        nrow(all_pixel_events),
        sum(all_pixel_events$is_single),
        100 * mean(all_pixel_events$is_single)
      ),
      x    = NULL,
      y    = toupper(idx),
      fill = "Severity"
    ) +
    
    theme_minimal(base_size = 10) +
    theme(
      plot.title         = element_text(face = "bold", size = 11),
      plot.subtitle      = element_text(size = 8, colour = "grey40"),
      axis.text.x        = element_text(angle = 90, hjust = 1,
                                        vjust = 0.5, size = 7),
      axis.ticks.x       = element_line(colour = "grey70",
                                        linewidth = 0.3),
      panel.grid.major.x = element_line(colour = "grey92",
                                        linewidth = 0.3),
      panel.grid.minor   = element_blank(),
      legend.position    = "bottom",
      legend.direction   = "horizontal"
    )
  
  print(p)
  
  ggsave(
    sprintf("./Figures/single_month_check_%02d_%s_%.3f_%.3f.png",
            pick_i, rp$config_name, rp$x, rp$y),
    plot   = p,
    width  = PLOT_WIDTH,
    height = PLOT_HEIGHT,
    dpi    = 150
  )
  
  cat(sprintf("Saved: single_month_check_%02d_%s\n",
              pick_i, rp$config_name))
}

message(sprintf("\n=== Done — %d images saved ===", nrow(random_picks)))