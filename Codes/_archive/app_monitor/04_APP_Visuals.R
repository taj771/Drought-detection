# =============================================================================
# 04_APP_Visuals.R
# GWSC Drought Monitor — Graphical + Map Outputs
# =============================================================================
# Builds four figures for the daily monitor and writes them as PNGs to
# OUTPUT_DIR/figures/<Country>/. Called by 00_APP_MASTER.Rmd AFTER 03_APP_Output.
#
# Requires in environment (from scripts 01-03):
#   index_monthly  — per-pixel monthly index (x, y, year, month, <.index_col>)
#   events_ref     — drought events, reference period (x, y, start_date,
#                    end_date, duration_months, peak_index, total_severity)
#   events_obs     — drought events, observation period (same columns)
#   boundary       — sf/sfc country polygon (from 01)
#   app_summary_table — long ref/obs/%change table (from 03)
#   APP_COUNTRY, APP_INDEX, APP_SCALE, APP_THRESHOLD, .index_col
#   REF_START/REF_END, OBS_START/OBS_END, OUTPUT_DIR
#
# Figures:
#   1. fig1_hotspot_map.png      — per-pixel event count (obs period), clipped
#   2. fig2_timeseries.png       — monthly mean index + drought threshold line
#   3. fig3_refobs_bars.png      — 4 characteristics, reference vs observation
#   4. fig4_event_timeline.png   — events as bars on a time axis (top pixels)
# =============================================================================

library(dplyr)
library(ggplot2)
library(sf)

FIG_DIR <- file.path(OUTPUT_DIR, "figures", gsub(" ", "_", APP_COUNTRY))
dir.create(FIG_DIR, showWarnings = FALSE, recursive = TRUE)

.save <- function(p, name, w = 8, h = 6) {
  f <- file.path(FIG_DIR, name)
  ggsave(f, p, width = w, height = h, dpi = 150, bg = "white")
  cat(sprintf("  wrote %s\n", f))
}

.title <- sprintf("%s — %s-%d, threshold %.1f",
                  APP_COUNTRY, APP_INDEX, APP_SCALE, APP_THRESHOLD)


# -- Figure 1: drought hotspot map (observation period) -----------------------
# Per-pixel count of drought events in the observation period, drawn over the
# country polygon. Shows WHERE droughts concentrate.
cat("Figure 1: hotspot map...\n")
tryCatch({
  if (!is.null(events_obs) && nrow(events_obs) > 0) {
    hot <- events_obs %>%
      group_by(x, y) %>%
      summarise(n_events = n(), .groups = "drop")

    bnd_sf <- sf::st_sf(geometry = sf::st_sfc(boundary, crs = 4326))

    p1 <- ggplot() +
      geom_sf(data = bnd_sf, fill = "grey95", colour = "grey50", linewidth = 0.3) +
      geom_tile(data = hot, aes(x = x, y = y, fill = n_events),
                width = 0.25, height = 0.25, alpha = 0.9) +
      scale_fill_viridis_c(option = "inferno", direction = -1,
                           name = "Events") +
      coord_sf(expand = FALSE) +
      labs(title = paste0("Drought hotspots (", OBS_START, "-", OBS_END, ")"),
           subtitle = .title, x = NULL, y = NULL) +
      theme_minimal()
    .save(p1, "fig1_hotspot_map.png")
  } else {
    cat("  skipped — no observation-period events\n")
  }
}, error = function(e) cat("  fig1 error:", conditionMessage(e), "\n"))


# -- Figure 2: time-series of monthly mean index ------------------------------
# Country-mean of the selected index by month, with the drought threshold line.
# Shows WHEN droughts occurred and the ref vs obs split.
cat("Figure 2: time-series...\n")
tryCatch({
  ts <- index_monthly %>%
    mutate(date = as.Date(sprintf("%04d-%02d-01",
                                  as.integer(year), as.integer(month)))) %>%
    group_by(date) %>%
    summarise(mean_index = mean(.data[[.index_col]], na.rm = TRUE),
              .groups = "drop") %>%
    arrange(date)

  p2 <- ggplot(ts, aes(x = date, y = mean_index)) +
    geom_line(colour = "steelblue", linewidth = 0.4) +
    geom_hline(yintercept = APP_THRESHOLD, colour = "red", linetype = "dashed") +
    geom_vline(xintercept = as.Date(sprintf("%d-01-01", OBS_START)),
               colour = "grey40", linetype = "dotted") +
    labs(title = "Monthly mean drought index",
         subtitle = paste0(.title,
                           "  (red = threshold, dotted = ref/obs split)"),
         x = NULL, y = paste0(APP_INDEX, "-", APP_SCALE)) +
    theme_minimal()
  .save(p2, "fig2_timeseries.png", w = 10, h = 5)
}, error = function(e) cat("  fig2 error:", conditionMessage(e), "\n"))


# -- Figure 3: reference vs observation bar chart -----------------------------
# The four characteristics from the summary table, ref vs obs side-by-side.
cat("Figure 3: ref-vs-obs bars...\n")
tryCatch({
  bars <- app_summary_table %>%
    tidyr::pivot_longer(c("reference", "observation"),
                        names_to = "period", values_to = "value") %>%
    mutate(period = factor(period, levels = c("reference", "observation"),
                           labels = c(paste0("Ref ", REF_START, "-", REF_END),
                                      paste0("Obs ", OBS_START, "-", OBS_END))))

  p3 <- ggplot(bars, aes(x = period, y = value, fill = period)) +
    geom_col(width = 0.6) +
    facet_wrap(~ characteristic, scales = "free_y") +
    scale_fill_manual(values = c("grey60", "firebrick"), guide = "none") +
    labs(title = "Drought characteristics: reference vs observation",
         subtitle = .title, x = NULL, y = NULL) +
    theme_minimal()
  .save(p3, "fig3_refobs_bars.png", w = 9, h = 6)
}, error = function(e) cat("  fig3 error:", conditionMessage(e), "\n"))


# -- Figure 4: event timeline (top pixels by event count) ---------------------
# Drought events as horizontal bars on a time axis, for the pixels with the
# most events. Shows duration + temporal clustering.
cat("Figure 4: event timeline...\n")
tryCatch({
  all_events <- bind_rows(
    if (!is.null(events_ref)) mutate(events_ref, period = "ref") else NULL,
    if (!is.null(events_obs)) mutate(events_obs, period = "obs") else NULL
  )
  if (!is.null(all_events) && nrow(all_events) > 0) {
    top_px <- all_events %>%
      count(x, y, sort = TRUE) %>%
      head(15) %>%
      mutate(pixel = sprintf("(%.2f, %.2f)", x, y))

    tl <- all_events %>%
      inner_join(top_px[, c("x", "y", "pixel")], by = c("x", "y")) %>%
      mutate(start_date = as.Date(start_date), end_date = as.Date(end_date))

    p4 <- ggplot(tl) +
      geom_segment(aes(x = start_date, xend = end_date,
                       y = pixel, yend = pixel, colour = peak_index),
                   linewidth = 3) +
      scale_colour_viridis_c(option = "inferno", name = "Peak\nindex") +
      labs(title = "Drought event timeline (top 15 pixels)",
           subtitle = .title, x = NULL, y = NULL) +
      theme_minimal()
    .save(p4, "fig4_event_timeline.png", w = 10, h = 6)
  } else {
    cat("  skipped — no events\n")
  }
}, error = function(e) cat("  fig4 error:", conditionMessage(e), "\n"))

cat(sprintf("\nFigures written to: %s\n", FIG_DIR))
