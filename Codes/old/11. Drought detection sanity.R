# =============================================================================
# SANITY CHECK — Random event spot check
# Pick a random detected event, pull its pixel time series,
# and plot to verify detection logic matches the SPI values
# =============================================================================

library(ggplot2)
library(dplyr)
library(lubridate)

# =============================================================================
# STEP 1 — Pick a random event to check
# You can also manually set the seed or pick a specific event
# =============================================================================

set.seed(42)   # change seed to get a different random event

# Sample one random event from all detected events
random_event <- all_drought_events %>%
  filter(!if_any(everything(), is.infinite)) %>%
  slice_sample(n = 1)

cat("=== SAMPLED EVENT ===\n\n")
cat(sprintf("Config      : %s\n",   random_event$config_name))
cat(sprintf("Index       : %s\n",   random_event$index))
cat(sprintf("Method      : %s\n",   random_event$method))
cat(sprintf("Start thr   : %.4f\n", random_event$start_thr))
cat(sprintf("Min duration: %d\n",   random_event$min_duration))
cat(sprintf("Country     : %s\n",   random_event$country))
cat(sprintf("Pixel       : x=%.4f  y=%.4f\n",
            random_event$x, random_event$y))
cat(sprintf("Event start : %s\n",
            format(random_event$start_date, "%b %Y")))
cat(sprintf("Event end   : %s\n",
            format(random_event$end_date, "%b %Y")))
cat(sprintf("Duration    : %d months\n", random_event$duration_months))
cat(sprintf("Entry SPI   : %.3f\n",  random_event$entry_spi))
cat(sprintf("Peak SPI    : %.3f\n",  random_event$peak_spi))
cat(sprintf("Severity    : %s\n",    random_event$severity_class))
cat(sprintf("Total sev   : %.3f\n",  random_event$total_severity))

# =============================================================================
# STEP 2 — Pull full time series for that pixel and index
# =============================================================================

px <- random_event$x
py <- random_event$y
idx <- random_event$index

pixel_ts <- pixel_data %>%
  filter(x == px, y == py) %>%
  arrange(date) %>%
  select(date, value = all_of(idx))

cat(sprintf("\nTime series pulled: %d months (%s to %s)\n",
            nrow(pixel_ts),
            min(pixel_ts$date),
            max(pixel_ts$date)))

# =============================================================================
# STEP 3 — Mark which months belong to the detected event
# =============================================================================

event_start <- random_event$start_date
event_end   <- random_event$end_date
entry_val   <- random_event$entry_spi
peak_val    <- random_event$peak_spi
start_thr   <- random_event$start_thr
method      <- random_event$method

# Add context window — show 12 months before and after event
window_start <- event_start %m-% months(12)
window_end   <- event_end   %m+% months(12)

plot_ts <- pixel_ts %>%
  filter(date >= window_start & date <= window_end) %>%
  mutate(
    in_event     = date >= event_start & date <= event_end,
    is_start     = date == event_start,
    is_end       = date == event_end,
    is_peak      = date == random_event$peak_date
  )

# =============================================================================
# STEP 4 — Build verification plot
# =============================================================================

# Dynamic subtitle based on method
subtitle_text <- case_when(
  method == "threshold_entry" ~ sprintf(
    "Method: threshold_entry | Start thr: %.2f | Entry SPI: %.3f | End when SPI > %.3f",
    start_thr, entry_val, entry_val),
  method == "threshold_peak"  ~ sprintf(
    "Method: threshold_peak  | Start thr: %.2f | Entry SPI: %.3f | Peak SPI: %.3f",
    start_thr, entry_val, peak_val),
  method == "cumulative"      ~ sprintf(
    "Method: cumulative | Start thr: %.2f | Entry SPI: %.3f | Ends when cum sum >= 0",
    start_thr, entry_val)
)

p_check <- ggplot(plot_ts, aes(x = date, y = value)) +
  
  # ── Shade the detected event period ────────────────────────────────────────
  annotate("rect",
           xmin  = event_start, xmax = event_end + days(15),
           ymin  = -Inf,        ymax = Inf,
           fill  = "#FAEEDA",   alpha = 0.5) +
  
  # ── Zero reference line ─────────────────────────────────────────────────────
  geom_hline(yintercept = 0,
             colour = "grey60", linewidth = 0.4) +
  
  # ── Start threshold line ────────────────────────────────────────────────────
  geom_hline(yintercept = start_thr,
             colour    = "#E53935",
             linewidth = 0.5,
             linetype  = "dashed") +
  
  # ── Entry SPI line (threshold_entry end condition) ──────────────────────────
  {if (method == "threshold_entry")
    geom_hline(yintercept = entry_val,
               colour    = "#1565C0",
               linewidth = 0.5,
               linetype  = "dotted")
  } +
  
  # ── Peak SPI line (threshold_peak end condition) ────────────────────────────
  {if (method == "threshold_peak")
    geom_hline(yintercept = peak_val,
               colour    = "#6A1B9A",
               linewidth = 0.5,
               linetype  = "dotted")
  } +
  
  # ── Full SPI time series ────────────────────────────────────────────────────
  geom_line(colour = "grey50", linewidth = 0.6) +
  
  # ── Highlight months inside the event ───────────────────────────────────────
  geom_line(
    data    = plot_ts %>% filter(in_event),
    colour  = "#E67E22",
    linewidth = 1.0
  ) +
  
  geom_point(
    data   = plot_ts %>% filter(in_event),
    colour = "#E67E22",
    size   = 2.5
  ) +
  
  # ── Mark start month ────────────────────────────────────────────────────────
  geom_point(
    data   = plot_ts %>% filter(is_start),
    colour = "#27AE60",
    size   = 4,
    shape  = 17   # triangle
  ) +
  
  # ── Mark end month ───────────────────────────────────────────────────────────
  geom_point(
    data   = plot_ts %>% filter(is_end),
    colour = "#E53935",
    size   = 4,
    shape  = 15   # square
  ) +
  
  # ── Mark peak month ──────────────────────────────────────────────────────────
  geom_point(
    data   = plot_ts %>% filter(is_peak),
    colour = "#6A1B9A",
    size   = 4,
    shape  = 18   # diamond
  ) +
  
  # ── Labels on key months ─────────────────────────────────────────────────────
  geom_label(
    data  = plot_ts %>% filter(is_start),
    aes(label = sprintf("Start\n%.3f", value)),
    vjust = -0.5, size = 3,
    colour = "#27AE60", fill = "white",
    label.size = 0.3
  ) +
  geom_label(
    data  = plot_ts %>% filter(is_peak),
    aes(label = sprintf("Peak\n%.3f", value)),
    vjust =  1.5, size = 3,
    colour = "#6A1B9A", fill = "white",
    label.size = 0.3
  ) +
  geom_label(
    data  = plot_ts %>% filter(is_end),
    aes(label = sprintf("End\n%.3f", value)),
    vjust = -0.5, size = 3,
    colour = "#E53935", fill = "white",
    label.size = 0.3
  ) +
  
  scale_x_date(date_breaks = "3 months",
               date_labels = "%b %Y",
               expand = c(0.02, 0)) +
  scale_y_continuous(breaks = seq(-3, 3, by = 0.5)) +
  
  labs(
    title    = sprintf(
      "Sanity check — %s | Pixel (%.3f, %.3f) | %s",
      toupper(idx), px, py,
      random_event$country
    ),
    subtitle = subtitle_text,
    x        = NULL,
    y        = sprintf("%s value", toupper(idx)),
    caption  = sprintf(
      "Event: %s to %s (%d months) | Severity: %s | Seed: 42",
      format(event_start, "%b %Y"),
      format(event_end,   "%b %Y"),
      random_event$duration_months,
      random_event$severity_class
    )
  ) +
  
  theme_minimal(base_size = 11) +
  theme(
    plot.title    = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(size = 9, colour = "grey40"),
    plot.caption  = element_text(size = 8, colour = "grey50"),
    axis.text.x   = element_text(angle = 30, hjust = 1, size = 8),
    panel.grid.minor = element_blank()
  )

print(p_check)

# =============================================================================
# STEP 5 — Cumulative sum panel (for cumulative method)
# Shows how the deficit accumulated and recovered
# =============================================================================

if (method == "cumulative") {
  
  cum_ts <- plot_ts %>%
    filter(in_event) %>%
    mutate(cum_sum = cumsum(value))
  
  p_cum <- ggplot(cum_ts, aes(x = date, y = cum_sum)) +
    geom_hline(yintercept = 0,
               colour = "#27AE60", linewidth = 0.6,
               linetype = "dashed") +
    geom_line(colour = "#185FA5", linewidth = 1.0) +
    geom_point(colour = "#185FA5", size = 2.5) +
    geom_label(
      data  = cum_ts %>% slice_tail(n = 1),
      aes(label = sprintf("End\ncum=%.3f", cum_sum)),
      vjust = -0.5, size = 3,
      colour = "#E53935", fill = "white",
      label.size = 0.3
    ) +
    scale_x_date(date_breaks = "1 month",
                 date_labels = "%b %Y") +
    labs(
      title    = "Cumulative SPI deficit during event",
      subtitle = "Drought ends when cumulative sum returns to >= 0",
      x        = NULL,
      y        = "Cumulative SPI sum"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title    = element_text(face = "bold", size = 11),
      plot.subtitle = element_text(size = 9, colour = "grey40"),
      axis.text.x   = element_text(angle = 30, hjust = 1, size = 8),
      panel.grid.minor = element_blank()
    )
  
  print(p_cum)
}

# =============================================================================
# STEP 6 — Print the raw numbers for manual verification
# =============================================================================

cat("\n=== RAW TIME SERIES DURING EVENT (manual check) ===\n\n")

plot_ts %>%
  filter(in_event) %>%
  mutate(
    cum_sum       = cumsum(value),
    above_entry   = value > entry_val,
    above_peak    = value > peak_val,
    above_zero    = value > 0
  ) %>%
  select(date, value, cum_sum,
         above_entry, above_peak, above_zero) %>%
  print(n = Inf)

# =============================================================================
# STEP 7 — Run multiple checks easily
# Change n_checks to spot check more events
# =============================================================================

# full series randlyly picke pixel 

# =============================================================================
# STEP 7 — Pick n random config + pixel combinations
# Plot ALL events for each pixel
# =============================================================================

# =============================================================================
# STEP 7 — Pick n random config + pixel combinations
# Plot ALL events for each pixel
# =============================================================================

#set.seed(40)   # change for different random picks
n_pixels <- 5  # ← change this to plot more or fewer pixels

# ── Pick n random config + pixel combinations ─────────────────────────────────
random_picks <- all_drought_events %>%
  filter(start_date >= "2001-01-01",
         end_date <= "2001-12-31")%>%
  filter(!if_any(everything(), is.infinite)) %>%
  select(x, y, country, config_name, index,
         method, start_thr, min_duration) %>%
  distinct() %>%
  slice_sample(n = min(n_pixels, nrow(.)))

cat(sprintf("=== PLOTTING %d RANDOM CONFIG + PIXEL COMBINATIONS ===\n\n",
            nrow(random_picks)))

# ── Severity colour map ───────────────────────────────────────────────────────
severity_fills <- c(
  "Mild"     = "#FFF9C4",
  "Moderate" = "#FFCC80",
  "Severe"   = "#EF9A9A",
  "Extreme"  = "#CE93D8"
)

# ── Loop over each random pick ────────────────────────────────────────────────
for (pick_i in seq_len(nrow(random_picks))) {
  
  # ── Settings ──────────────────────────────────────────────────────────────────
  n_pixels        <- 2
  MONTHS_PER_PAGE <- 12
  PLOT_WIDTH      <- 16
  PLOT_HEIGHT     <- 7
  
  # ── Pick n random config + pixel combinations ─────────────────────────────────
  random_picks <- all_drought_events %>%
    filter(year(start_date) == 2001) %>%
    filter(!if_any(everything(), is.infinite)) %>%
    select(x, y, country, config_name, index,
           method, start_thr, min_duration) %>%
    distinct() %>%
    slice_sample(n = min(n_pixels, nrow(.)))
  
  cat(sprintf("=== PLOTTING %d RANDOM CONFIG + PIXEL COMBINATIONS ===\n\n",
              nrow(random_picks)))
  
  severity_fills <- c(
    "Mild"     = "#FFF9C4",
    "Moderate" = "#FFCC80",
    "Severe"   = "#EF9A9A",
    "Extreme"  = "#CE93D8"
  )
  
  # ── PASTE THE CHUNKED LOOP HERE ───────────────────────────────────────────────
  for (pick_i in seq_len(nrow(random_picks))) {
    
    rp  <- random_picks[pick_i, ]
    idx <- rp$index
    
    all_pixel_events <- all_drought_events %>%
      filter(
        x           == rp$x,
        y           == rp$y,
        config_name == rp$config_name
      ) %>%
      arrange(start_date)
    
    if (nrow(all_pixel_events) == 0) next
    
    cat(sprintf("\n--- Pick %d / %d ---\n", pick_i, nrow(random_picks)))
    cat(sprintf("Pixel  : x=%.3f  y=%.3f | %s\n", rp$x, rp$y, rp$country))
    cat(sprintf("Config : %s\n", rp$config_name))
    cat(sprintf("Total events: %d\n", nrow(all_pixel_events)))
    
    all_pixel_events %>%
      select(start_date, end_date, duration_months,
             entry_spi, peak_spi, severity_class) %>%
      print(n = Inf)
    
    ts_start <- min(all_pixel_events$start_date) %m-% months(3)
    ts_end   <- max(all_pixel_events$end_date)   %m+% months(3)
    
    ts_full <- pixel_data %>%
      filter(x == rp$x, y == rp$y) %>%
      arrange(date) %>%
      select(date, value = all_of(idx)) %>%
      filter(date >= ts_start & date <= ts_end)
    
    event_months <- all_pixel_events %>%
      rowwise() %>%
      mutate(date = list(seq(start_date, end_date, by = "month"))) %>%
      unnest(date) %>%
      select(date, drought_id, severity_class)
    
    event_bands <- all_pixel_events %>%
      mutate(
        xmin  = start_date,
        xmax  = end_date + days(29),
        label = sprintf("E%d\n%s\n%d mo",
                        drought_id,
                        format(start_date, "%b %y"),
                        duration_months)
      )
    
    n_months <- nrow(ts_full)
    n_pages  <- ceiling(n_months / MONTHS_PER_PAGE)
    
    cat(sprintf("%d months → %d pages\n", n_months, n_pages))
    
    for (page_i in seq_len(n_pages)) {
      
      idx_start   <- (page_i - 1) * MONTHS_PER_PAGE + 1
      idx_end     <- min(page_i * MONTHS_PER_PAGE, n_months)
      chunk_start <- ts_full$date[idx_start]
      chunk_end   <- ts_full$date[idx_end]
      
      ts_chunk <- ts_full %>%
        filter(date >= chunk_start & date <= chunk_end) %>%
        left_join(event_months, by = "date") %>%
        mutate(in_event = !is.na(drought_id))
      
      event_bands_chunk <- event_bands %>%
        filter(xmin <= chunk_end & xmax >= chunk_start) %>%
        mutate(
          xmin_clip = pmax(xmin, chunk_start),
          xmax_clip = pmin(xmax, chunk_end + days(29))
        )
      
      if (nrow(ts_chunk) == 0) next
      
      p <- ggplot(ts_chunk, aes(x = date, y = value)) +
        
        geom_rect(
          data        = event_bands_chunk,
          aes(xmin = xmin_clip, xmax = xmax_clip,
              ymin = -Inf, ymax = Inf,
              fill = severity_class),
          inherit.aes = FALSE,
          alpha       = 0.4
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
            mutate(label_x = xmin_clip + (xmax_clip - xmin_clip) / 2) %>%
            filter(label_x >= chunk_start & label_x <= chunk_end),
          aes(x = label_x, y = Inf, label = label),
          inherit.aes = FALSE,
          vjust = 1.1, size = 2.3,
          colour = "grey20", fill = "white",
          label.size = 0.2, lineheight = 0.9
        ) +
        
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
            "Pick %d | %s | %s | lon=%.3f lat=%.3f | %s   [page %d / %d]",
            pick_i, toupper(idx), rp$config_name,
            rp$x, rp$y, rp$country, page_i, n_pages
          ),
          subtitle = sprintf(
            "%s to %s | Method: %s | Thr: %.4f | Min dur: %d | Total events: %d",
            format(chunk_start, "%b %Y"),
            format(chunk_end,   "%b %Y"),
            rp$method, rp$start_thr,
            rp$min_duration, nrow(all_pixel_events)
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
        sprintf("./Figures/full_series_%02d_%s_%.3f_%.3f_p%02d.png",
                pick_i, rp$config_name, rp$x, rp$y, page_i),
        plot   = p,
        width  = PLOT_WIDTH,
        height = PLOT_HEIGHT,
        dpi    = 150
      )
      
      cat(sprintf("  Page %d/%d saved\n", page_i, n_pages))
    }
  }
  
  message(sprintf("\n=== Done — %d picks saved ===", nrow(random_picks)))
}

message(sprintf("\n=== Done — %d plots saved ===", nrow(random_picks)))




# Only event wise 
n_checks <- 5

cat(sprintf("Total events available: %d\n",
            nrow(all_drought_events %>%
                   filter(!if_any(everything(), is.infinite)))))

random_events <- all_drought_events %>%
  filter(!if_any(everything(), is.infinite)) %>%
  slice_sample(n = min(n_checks, nrow(.)))

cat(sprintf("Plotting %d events\n\n", nrow(random_events)))

for (row_i in seq_len(nrow(random_events))) {
  
  ev  <- random_events[row_i, ]
  idx <- ev$index
  
  ts <- pixel_data %>%
    filter(x == ev$x, y == ev$y) %>%
    arrange(date) %>%
    select(date, value = all_of(idx)) %>%
    filter(date >= ev$start_date %m-% months(6) &
             date <= ev$end_date   %m+% months(6)) %>%
    mutate(in_event = date >= ev$start_date & date <= ev$end_date)
  
  p <- ggplot(ts, aes(x = date, y = value)) +
    
    # ── Event shading ──────────────────────────────────────────────────────────
    annotate("rect",
             xmin = ev$start_date, xmax = ev$end_date,
             ymin = -Inf, ymax = Inf,
             fill = "#FAEEDA", alpha = 0.5) +
    
    # ── Reference lines ────────────────────────────────────────────────────────
    geom_hline(yintercept = 0,
               colour = "grey60", linewidth = 0.4) +
    geom_hline(yintercept = ev$start_thr,
               colour = "#E53935", linewidth = 0.5,
               linetype = "dashed") +
    
    # ── Full time series line + points ─────────────────────────────────────────
    geom_line(colour = "grey50", linewidth = 0.6) +
    geom_point(colour = "grey50", size = 1.8, shape = 21,
               fill = "white", stroke = 0.8) +
    
    # ── Value labels for ALL points ────────────────────────────────────────────
    geom_text(
      aes(label = sprintf("%.2f", value)),
      colour   = "grey40",
      size     = 2.5,
      vjust    = -0.8,
      hjust    = 0.5,
      fontface = "plain"
    ) +
    
    # ── Event months — line, points and labels ─────────────────────────────────
    geom_line(
      data      = ts %>% filter(in_event),
      colour    = "#E67E22",
      linewidth = 1.0
    ) +
    geom_point(
      data   = ts %>% filter(in_event),
      colour = "#E67E22",
      size   = 3,
      shape  = 19
    ) +
   # geom_text(
    #  data     = ts %>% filter(in_event),
    #  aes(label = sprintf("%.2f", value)),
    #  colour   = "#E67E22",
    #  size     = 2.8,
    #  vjust    = -0.9,
    #  hjust    = 0.5,
    #  fontface = "bold"
   # ) +
    
    # ── Monthly x-axis ─────────────────────────────────────────────────────────
    scale_x_date(
      date_breaks  = "1 month",      # every month
      date_labels  = "%b\n%Y",       # month on top, year below
      expand       = c(0.03, 0)
    ) +
    
    scale_y_continuous(
      expand = expansion(mult = c(0.15, 0.2))  # extra space for labels
    ) +
    
    labs(
      title    = sprintf("Check %d | %s | %s | %s | (%.3f, %.3f)",
                         row_i, ev$config_name, ev$country,
                         toupper(idx), ev$x, ev$y),
      subtitle = sprintf("%s to %s | %d months | %s | peak SPI: %.2f",
                         format(ev$start_date, "%b %Y"),
                         format(ev$end_date,   "%b %Y"),
                         ev$duration_months,
                         ev$severity_class,
                         ev$peak_spi),
      x = NULL,
      y = toupper(idx)
    ) +
    
    theme_minimal(base_size = 10) +
    theme(
      plot.title       = element_text(face = "bold", size = 10),
      plot.subtitle    = element_text(size = 8, colour = "grey40"),
      axis.text.x      = element_text(angle = 90, hjust = 1,
                                      vjust = 0.5, size = 7.5),
      axis.ticks.x     = element_line(colour = "grey70", linewidth = 0.3),
      panel.grid.major.x = element_line(colour = "grey92",
                                        linewidth = 0.3),
      panel.grid.minor = element_blank()
    )
  
  print(p)
  
  ggsave(
    sprintf("./Figures/sanity_check_%02d_%s_%s.png",
            row_i, ev$config_name, ev$country),
    plot   = p,
    width  = max(12, nrow(ts) * 0.5),  # wider for monthly labels
    height = 6,
    dpi    = 150
  )
  
  cat(sprintf("Check %d: %s | %s | %s to %s | peak=%.2f\n",
              row_i, ev$config_name, ev$country,
              format(ev$start_date, "%b %Y"),
              format(ev$end_date,   "%b %Y"),
              ev$peak_spi))
}





# Pull the exact time series for this pixel
pixel_data %>%
  filter(x == 1.125, y == 10.125) %>%
  arrange(date) %>%
  filter(date >= as.Date("2000-01-01") &
  date <= as.Date("2000-08-01")) %>%
  select(date, spi1) %>%
  mutate(
  below_zero  = spi1 < 0,
  cum_sum     = cumsum(ifelse(spi1 < 0, spi1, 0))
  ) %>%
  print()