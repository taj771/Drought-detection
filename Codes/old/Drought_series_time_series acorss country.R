# =============================================================================
# MULTI-COUNTRY DROUGHT TIME COMPARISON
# One plot per country, same x-axis time frame
# Same random pixel+config pick style as before
# =============================================================================

TARGET_INDEX  <- "spi3"
TARGET_METHOD <- "threshold_entry"
TARGET_THR    <- -0.5
#TARGET_DUR    <- 1

severity_fills <- c(
  "Mild"     = "#FFF9C4",
  "Moderate" = "#FFCC80",
  "Severe"   = "#EF9A9A",
  "Extreme"  = "#CE93D8"
)

# ── Pick one random pixel per country ────────────────────────────────────────
set.seed(42)

random_picks <- all_drought_events %>%
  filter(!if_any(everything(), is.infinite)) %>%
  filter(!is.na(country)) %>%
  filter(
    index        == TARGET_INDEX,
    method       == TARGET_METHOD,
    abs(start_thr - TARGET_THR) < 0.001
    #min_duration == TARGET_DUR
  ) %>%
  select(x, y, country, config_name, index,
         method, start_thr, min_duration) %>%
  distinct() %>%
  group_by(country) %>%
  slice_sample(n = 1) %>%           # ← one random pixel per country
  ungroup()

cat(sprintf("Countries: %d\n", nrow(random_picks)))
print(random_picks %>% select(country, x, y, config_name))

# ── Get all events for each picked pixel ─────────────────────────────────────
all_picks_events <- map_dfr(seq_len(nrow(random_picks)), function(i) {
  rp <- random_picks[i, ]
  all_drought_events %>%
    filter(
      x           == rp$x,
      y           == rp$y,
      config_name == rp$config_name
    ) %>%
    mutate(
      country     = rp$country,
      is_single   = duration_months == 1,
      label       = sprintf("E%d\n%s\n%d mo",
                            drought_id,
                            format(start_date, "%b %y"),
                            duration_months)
    )
})

# ── Shared x axis — full study period ────────────────────────────────────────
x_min <- min(all_picks_events$start_date) %m-% months(3)
x_max <- max(all_picks_events$end_date)   %m+% months(3)

MONTHS_PER_PAGE <- 24   # ← months per page
PLOT_WIDTH      <- 16
PLOT_HEIGHT     <- 3.5  # ← shorter per country since stacked

n_months <- interval(x_min, x_max) %/% months(1)
n_pages  <- ceiling(n_months / MONTHS_PER_PAGE)
countries <- sort(unique(random_picks$country))

cat(sprintf("\nTotal months: %d → %d pages\n", n_months, n_pages))

# ── Loop over pages ───────────────────────────────────────────────────────────
for (page_i in seq_len(n_pages)) {
  
  chunk_start <- x_min %m+% months((page_i - 1) * MONTHS_PER_PAGE)
  chunk_end   <- min(x_max,
                     x_min %m+% months(page_i * MONTHS_PER_PAGE))
  
  cat(sprintf("\nPage %d/%d: %s to %s\n",
              page_i, n_pages,
              format(chunk_start, "%b %Y"),
              format(chunk_end,   "%b %Y")))
  
  plot_list <- list()
  
  for (cntry in countries) {
    
    rp <- random_picks %>% filter(country == cntry)
    idx <- rp$index
    
    # ── Time series for this pixel ──────────────────────────────────────────
    ts <- pixel_data %>%
      filter(x == rp$x, y == rp$y) %>%
      arrange(date) %>%
      select(date, value = all_of(idx)) %>%
      filter(date >= chunk_start & date <= chunk_end)
    
    if (nrow(ts) == 0) next
    
    # ── Events for this pixel ───────────────────────────────────────────────
    px_events <- all_picks_events %>%
      filter(country == cntry)
    
    event_months <- px_events %>%
      rowwise() %>%
      mutate(date = list(seq(start_date, end_date, by = "month"))) %>%
      unnest(date) %>%
      select(date, drought_id, severity_class, is_single)
    
    ts <- ts %>%
      left_join(event_months, by = "date") %>%
      mutate(in_event = !is.na(drought_id))
    
    event_bands <- px_events %>%
      filter(start_date <= chunk_end & end_date >= chunk_start) %>%
      mutate(
        xmin_clip = pmax(start_date,        chunk_start),
        xmax_clip = pmin(end_date + days(29), chunk_end),
        label_x   = xmin_clip + (xmax_clip - xmin_clip) / 2
      )
    
    # ── Build plot for this country ─────────────────────────────────────────
    p <- ggplot(ts, aes(x = date, y = value)) +
      
      # Event shading
      geom_rect(
        data        = event_bands,
        aes(xmin = xmin_clip, xmax = xmax_clip,
            ymin = -Inf, ymax = Inf,
            fill = severity_class),
        inherit.aes = FALSE,
        alpha       = 0.4
      ) +
      
      # Single month red border
      geom_rect(
        data = event_bands %>% filter(is_single),
        aes(xmin = xmin_clip, xmax = xmax_clip,
            ymin = -Inf, ymax = Inf),
        inherit.aes = FALSE,
        fill        = NA,
        colour      = "#E53935",
        linewidth   = 0.8,
        linetype    = "solid"
      ) +
      
      scale_fill_manual(values = severity_fills,
                        name   = "Severity") +
      
      geom_hline(yintercept = 0,
                 colour = "grey60", linewidth = 0.4) +
      geom_hline(yintercept = rp$start_thr,
                 colour = "#E53935", linewidth = 0.5,
                 linetype = "dashed") +
      
      geom_line(colour = "grey40", linewidth = 0.6) +
      geom_point(colour = "grey40", size = 1.5,
                 shape = 21, fill = "white", stroke = 0.7) +
      geom_text(
        aes(label = sprintf("%.2f", value)),
        colour = "grey30", size = 2.0,
        vjust = -0.8, hjust = 0.5
      ) +
      
      # Event labels
      geom_label(
        data = event_bands %>%
          filter(label_x >= chunk_start & label_x <= chunk_end),
        aes(x      = label_x,
            y      = Inf,
            label  = label,
            colour = ifelse(is_single, "#A32D2D", "grey20")),
        inherit.aes = FALSE,
        vjust = 1.1, size = 2.0,
        fill = "white", label.size = 0.2,
        lineheight = 0.9
      ) +
      scale_colour_identity() +
      
      # ── Fixed x axis — same for all countries ────────────────────────────
      scale_x_date(
        date_breaks = "1 month",
        date_labels = "%b\n%Y",
        limits      = c(chunk_start, chunk_end),
        expand      = c(0.01, 0)
      ) +
      scale_y_continuous(
        expand = expansion(mult = c(0.15, 0.28))
      ) +
      
      labs(
        title = sprintf("%s | lon=%.3f lat=%.3f | %d events",
                        cntry, rp$x, rp$y,
                        nrow(px_events)),
        x    = NULL,
        y    = toupper(idx)
      ) +
      
      theme_minimal(base_size = 9) +
      theme(
        plot.title         = element_text(face = "bold", size = 10,
                                          colour = "grey20"),
        axis.text.x        = element_text(angle = 90, hjust = 1,
                                          vjust = 0.5, size = 6.5),
        axis.ticks.x       = element_line(colour = "grey70",
                                          linewidth = 0.3),
        panel.grid.major.x = element_line(colour = "grey92",
                                          linewidth = 0.3),
        panel.grid.minor   = element_blank(),
        legend.position    = "none"   # ← shared legend added separately
      )
    
    plot_list[[cntry]] <- p
  }
  
  # ── Stack all country plots vertically using patchwork ─────────────────────
  if (length(plot_list) == 0) next
  
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
        "Each panel = one randomly selected pixel per country  |  ",
        "Shading = detected drought severity  |  ",
        "Red border = single-month event  |  ",
        "Red dashed line = start threshold"
      ),
      theme = theme(
        plot.title   = element_text(face = "bold", size = 12),
        plot.caption = element_text(size = 8, colour = "grey50",
                                    hjust = 0)
      )
    )
  
  # ── Shared severity legend ────────────────────────────────────────────────
  legend_p <- ggplot(
    data.frame(
      severity = names(severity_fills),
      x = 1, y = 1
    ),
    aes(x = x, y = y, fill = severity)
  ) +
    geom_col() +
    scale_fill_manual(
      values = severity_fills,
      name   = "Severity",
      guide  = guide_legend(direction = "horizontal",
                            nrow = 1)
    ) +
    theme_void() +
    theme(legend.position = "top")
  
  shared_legend <- cowplot::get_legend(legend_p)
  
  p_final <- cowplot::plot_grid(
    shared_legend,
    p_combined,
    ncol       = 1,
    rel_heights = c(0.05, 1)
  )
  
  ggsave(
    sprintf("./Figures/multi_country_drought_%s_p%02d.png",
            TARGET_INDEX, page_i),
    plot   = p_final,
    width  = PLOT_WIDTH,
    height = length(plot_list) * PLOT_HEIGHT + 0.5,
    dpi    = 150
  )
  
  cat(sprintf("  Saved: multi_country_drought_%s_p%02d.png\n",
              TARGET_INDEX, page_i))
}

message(sprintf("\n=== Done — %d pages saved ===", n_pages))