# =============================================================================
# 08. Appendix A1 Figure — Visual Event Inspection
# =============================================================================
# Analysis Plan Reference: Phase 1 Section 1.3 Step 4, Sanity Check 1
#
# Produces the Appendix A1 figure: SPI-6, threshold-entry method, thr=0.0,
# for the reference pixel Thailand 100.375°E / 14.625°N.
#
# Shows monthly SPI-6 time series with detected drought events shaded,
# onset/termination markers, and threshold line.
#
# Input  : spi_era5_reference_period.rds  (Script 05 output)
#          all_drought_events.rds          (Script 03 output)
# Output : Figures/appendix_a1_event_inspection.png
# =============================================================================


# -- Libraries ----------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(lubridate)
library(arrow)   # read v2 .parquet inputs


# -- Configuration ------------------------------------------------------------
# v2 pipeline writes .parquet to drought_outputs/v2.
VOLUME_PATH  <- "/Volumes/projects/gwsc_drought/drought_outputs/v2"
FIGURES_PATH <- file.path(VOLUME_PATH, "Figures")

# Benin pixel inside the clipped boundary (post polygon-clip; y=6.375 was a
# bbox-corner cell removed by the clip fix).
TARGET_X     <- 0.875
TARGET_Y     <- 10.375
TARGET_INDEX <- "spi6"
TARGET_METHOD <- "threshold"
TARGET_THR   <- 0.0
TARGET_CONFID <- "S6-T-000"  # config_id for spi6, threshold, thr=0.0

dir.create(FIGURES_PATH, showWarnings = FALSE, recursive = TRUE)


# -- Load data ----------------------------------------------------------------
# Use spi_results.rds (script 01 output) — has spi6, smaller than spi_era5 file.
message("Loading SPI data...")
spi_data <- as.data.frame(arrow::read_parquet(file.path(VOLUME_PATH, "spi_results.parquet")))

# v2 detection writes a single all_drought_events.parquet (not per-config cfg
# files). Filter to the target config with arrow predicate pushdown so only that
# config's rows are materialised — avoids OOM from the full event table.
message("Loading drought events for target config...")
events_parquet <- file.path(VOLUME_PATH, "all_drought_events.parquet")
if (!file.exists(events_parquet)) {
  stop(sprintf("Events file not found: %s", events_parquet))
}
events <- arrow::read_parquet(events_parquet) %>%
  dplyr::filter(config_id == TARGET_CONFID) %>%
  as.data.frame()
if (nrow(events) == 0) {
  stop(sprintf("No events for config_id %s in all_drought_events.parquet", TARGET_CONFID))
}


# -- Extract pixel time series ------------------------------------------------
pixel_ts <- spi_data %>%
  filter(abs(x - TARGET_X) < 0.01, abs(y - TARGET_Y) < 0.01) %>%
  arrange(year, month) %>%
  mutate(date = as.Date(sprintf("%d-%02d-01", year, month)))

if (nrow(pixel_ts) == 0) {
  stop(sprintf("No data for pixel x=%.3f y=%.3f. Check coordinates.", TARGET_X, TARGET_Y))
}

if (!TARGET_INDEX %in% names(pixel_ts)) {
  stop(sprintf("Column '%s' not found. Available: %s",
               TARGET_INDEX, paste(names(pixel_ts), collapse = ", ")))
}

cat(sprintf("Pixel: x=%.3f, y=%.3f  |  %d months (%d to %d)\n",
            TARGET_X, TARGET_Y, nrow(pixel_ts),
            min(pixel_ts$year), max(pixel_ts$year)))


# -- Extract events for this pixel + config -----------------------------------
pixel_events <- events %>%
  filter(
    abs(x - TARGET_X) < 0.01,
    abs(y - TARGET_Y) < 0.01,
    config_id == TARGET_CONFID
  ) %>%
  mutate(
    start_date = as.Date(start_date),
    end_date   = as.Date(end_date)
  )

cat(sprintf("Drought events (config %s): %d\n", TARGET_CONFID, nrow(pixel_events)))


# -- Build shading rectangles for drought periods -----------------------------
event_rects <- pixel_events %>%
  mutate(
    xmin = start_date,
    xmax = end_date + days(31),   # extend to end of last month
    severity_class = factor(severity_class,
                            levels = c("Mild", "Moderate", "Severe", "Extreme"))
  )

severity_colours <- c(
  "Mild"     = "#FDE68A",
  "Moderate" = "#FDBA74",
  "Severe"   = "#EA580C",
  "Extreme"  = "#991B1B"
)


# -- Build plot ---------------------------------------------------------------
index_vals <- pixel_ts %>%
  select(date, value = all_of(TARGET_INDEX)) %>%
  filter(!is.na(value))

y_min <- min(c(index_vals$value, TARGET_THR - 0.3), na.rm = TRUE)
y_max <- max(c(index_vals$value, 0.5),              na.rm = TRUE)

p <- ggplot() +

  # Drought event shading
  geom_rect(
    data = event_rects,
    aes(xmin = xmin, xmax = xmax,
        ymin = -Inf, ymax = Inf,
        fill = severity_class),
    alpha = 0.35
  ) +
  scale_fill_manual(values = severity_colours, name = "Severity class",
                    drop = FALSE) +

  # Zero reference line
  geom_hline(yintercept = 0, colour = "grey50", linewidth = 0.4, linetype = "dotted") +

  # Threshold line
  geom_hline(yintercept = TARGET_THR, colour = "#DC2626",
             linewidth = 0.6, linetype = "dashed") +

  # SPI-6 time series
  geom_line(data = index_vals,
            aes(x = date, y = value),
            colour = "#1D4ED8", linewidth = 0.5) +

  # Onset markers
  geom_point(data = pixel_events,
             aes(x = as.Date(start_date), y = TARGET_THR),
             colour = "#16A34A", shape = 25, size = 2.5, fill = "#16A34A") +

  # Termination markers
  geom_point(data = pixel_events,
             aes(x = as.Date(end_date), y = TARGET_THR),
             colour = "#DC2626", shape = 24, size = 2.5, fill = "#DC2626") +

  labs(
    title    = "Appendix A1 — Visual Event Inspection",
    subtitle = sprintf(
      "SPI-6 | Threshold method | Entry threshold = %.1f\nPixel: %.3f°E / %.3f°N (Thailand)",
      TARGET_THR, TARGET_X, TARGET_Y
    ),
    x = NULL,
    y = "SPI-6",
    caption = paste(
      "Shading: detected drought events (by severity class).",
      "▼ Onset  ▲ Termination  — — Threshold",
      sep = "\n"
    )
  ) +

  scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
  scale_y_continuous(breaks = seq(floor(y_min), ceiling(y_max), by = 0.5)) +
  coord_cartesian(ylim = c(y_min, y_max)) +

  theme_bw(base_size = 11) +
  theme(
    plot.title    = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(size = 9, colour = "grey30"),
    plot.caption  = element_text(size = 7, colour = "grey40"),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )


# -- Save figure --------------------------------------------------------------
out_path <- file.path(FIGURES_PATH, "appendix_a1_event_inspection.png")
ggsave(out_path, plot = p, width = 12, height = 5, dpi = 150)

message(sprintf("Figure saved: %s", out_path))
cat(sprintf("\nEvents plotted:\n"))
pixel_events %>%
  select(start_date, end_date, duration_months, peak_index, severity_class) %>%
  arrange(start_date) %>%
  print(row.names = FALSE)

message("Script 08 complete.")
