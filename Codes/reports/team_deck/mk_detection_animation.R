# =============================================================================
# mk_detection_animation.R — Slide 4 time-lapse: how we detect drought events
# =============================================================================
# One real Benin pixel (x=0.875, y=10.375), SPI-3, threshold -0.5, 2003-2023.
# Builds a frame-by-frame animation: a cursor sweeps left->right along the SPI-3
# series; when the line drops below -0.5 a drought STARTS, when it recovers the
# event ENDS and is shaded by severity, its characteristics pop as a callout, and
# a running "Droughts found: N" counter + a rolling table of the most recent
# events grow beside the plot.
#
# Detection rule (faithful to pipeline): a run of consecutive months with
# SPI-3 < -0.5, length >= min_duration (=1). Per-event stats mirror
# detect_droughts() in v1_R_single_cluster/03. Drought_Detection.R.
#
# Outputs (in this folder):
#   detection_benin.gif      — the animation (guaranteed)
#   detection_benin.mp4      — same, if av/ffmpeg available (best-effort)
#   detection_poster.png     — final frame, embedded in the beamer deck
# =============================================================================

suppressMessages({
  library(arrow); library(dplyr); library(ggplot2); library(lubridate)
  library(grid); library(gridExtra)
})

# -- Config -------------------------------------------------------------------
WD        <- "c:/Users/tajayalath/Box/R - Projects - Tharaka/Drought-indices"
setwd(WD)
DATA      <- "Codes/_archive/_data/spi_results.parquet"
OUTDIR    <- "Codes/reports/team_deck"
FRAMEDIR  <- file.path(OUTDIR, "frames")
TARGET_X  <- 0.875
TARGET_Y  <- 10.375
SCALE_COL <- "spi3"
THR       <- -0.5
MIN_DUR   <- 1
Y0        <- 2003
Y1        <- 2023
FPS       <- 12          # animation speed
STEP      <- 1           # cursor step in months (1 = every month)
N_TABLE   <- 4           # rolling table shows this many most-recent events

dir.create(FRAMEDIR, showWarnings = FALSE, recursive = TRUE)
unlink(file.path(FRAMEDIR, "frame_*.png"))

# -- Load + slice pixel -------------------------------------------------------
message("Loading SPI data...")
d <- as.data.frame(arrow::read_parquet(DATA))
px <- d %>%
  filter(abs(x - TARGET_X) < 0.01, abs(y - TARGET_Y) < 0.01,
         year >= Y0, year <= Y1) %>%
  arrange(year, month) %>%
  mutate(date = as.Date(sprintf("%d-%02d-01", year, month)),
         val  = .data[[SCALE_COL]])
stopifnot(nrow(px) > 0)
message(sprintf("Pixel %.3f/%.3f: %d months (%d-%d)",
                TARGET_X, TARGET_Y, nrow(px), Y0, Y1))

# -- Detect events (run-length rule) ------------------------------------------
below <- px$val < THR; below[is.na(below)] <- FALSE
r <- rle(below)
ends   <- cumsum(r$lengths)
starts <- c(1L, head(ends, -1) + 1L)
ev_idx <- which(r$values & r$lengths >= MIN_DUR)

severity_of <- function(peak) {
  if (peak >= -1.0) "Mild"
  else if (peak >= -1.5) "Moderate"
  else if (peak >= -2.0) "Severe"
  else "Extreme"
}

events <- lapply(ev_idx, function(i) {
  s <- starts[i]; e <- ends[i]
  seg <- px$val[s:e]
  pk_local <- which.min(seg)
  data.frame(
    start_date      = px$date[s],
    end_date        = px$date[e],
    duration_months = as.integer(e - s + 1L),
    peak_index      = round(min(seg), 2),
    peak_date       = px$date[s + pk_local - 1L],
    severity_class  = severity_of(min(seg)),
    stringsAsFactors = FALSE
  )
}) %>% bind_rows() %>% arrange(start_date)
events$id <- seq_len(nrow(events))
message(sprintf("Detected %d events (min_dur=%d).", nrow(events), MIN_DUR))

severity_colours <- c(Mild="#FDE68A", Moderate="#FDBA74",
                      Severe="#EA580C", Extreme="#991B1B")
events$severity_class <- factor(events$severity_class,
                                levels = names(severity_colours))

# axis limits (fixed across all frames so the plot doesn't jump)
y_min <- min(c(px$val, THR - 0.3), na.rm = TRUE)
y_max <- max(c(px$val, 0.5),       na.rm = TRUE)
x_min <- min(px$date); x_max <- max(px$date)

# -- Frame builder ------------------------------------------------------------
# For a given cursor date, draw the plot revealed up to the cursor plus every
# event that has finished (end_date <= cursor).
make_frame <- function(cursor) {
  seen  <- px %>% filter(date <= cursor)                       # revealed series
  done  <- events %>% filter(end_date <= cursor)               # finished events
  # a drought currently in progress? (started, not yet ended)
  active <- events %>% filter(start_date <= cursor & end_date > cursor)

  # split revealed line into above/below-threshold for red colouring
  seen <- seen %>% mutate(below = val < THR)

  rects <- done %>%
    mutate(xmin = start_date, xmax = end_date + days(31))

  p <- ggplot() +
    # finished-event shading, by severity
    { if (nrow(rects))
        geom_rect(data = rects,
                  aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf,
                      fill = severity_class), alpha = 0.35) } +
    scale_fill_manual(values = severity_colours, name = "Severity",
                      drop = FALSE) +
    # in-progress drought: light grey pending band
    { if (nrow(active))
        geom_rect(data = active,
                  aes(xmin = start_date, xmax = cursor + days(31)),
                  ymin = -Inf, ymax = Inf, fill = "grey70", alpha = 0.25) } +
    geom_hline(yintercept = 0,   colour = "grey55", linewidth = 0.4, linetype = "dotted") +
    geom_hline(yintercept = THR, colour = "#DC2626", linewidth = 0.7, linetype = "dashed") +
    annotate("text", x = x_min, y = THR + 0.12, hjust = 0, size = 3,
             colour = "#DC2626", label = sprintf("threshold = %.1f", THR)) +
    # SPI-3 line (blue), then red overlay for below-threshold points
    geom_line(data = seen, aes(x = date, y = val),
              colour = "#1D4ED8", linewidth = 0.6) +
    geom_point(data = filter(seen, below),
               aes(x = date, y = val), colour = "#DC2626", size = 0.9) +
    # onset / termination markers for finished events
    { if (nrow(done)) list(
        geom_point(data = done, aes(x = start_date, y = THR),
                   shape = 25, size = 2.4, colour = "#16A34A", fill = "#16A34A"),
        geom_point(data = done, aes(x = end_date, y = THR),
                   shape = 24, size = 2.4, colour = "#DC2626", fill = "#DC2626")) } +
    # cursor line
    geom_vline(xintercept = as.numeric(cursor), colour = "grey30",
               linewidth = 0.5) +
    # callout on the most-recently finished event
    { if (nrow(done)) {
        last <- done %>% slice_max(end_date, n = 1)
        lbl <- sprintf("%d mo | peak %.1f | %s",
                       last$duration_months, last$peak_index,
                       as.character(last$severity_class))
        annotate("label",
                 x = last$start_date + (last$end_date - last$start_date)/2,
                 y = y_min + 0.35, label = lbl, size = 2.9,
                 fill = "white", label.size = 0.3, colour = "grey15")
      } } +
    scale_x_date(limits = c(x_min, x_max + days(31)),
                 date_breaks = "3 years", date_labels = "%Y") +
    coord_cartesian(ylim = c(y_min, y_max)) +
    labs(title = "Detecting drought on one Benin pixel  (SPI-3)",
         subtitle = sprintf("%s  —  droughts found: %d",
                            format(cursor, "%b %Y"), nrow(done)),
         x = NULL, y = "SPI-3") +
    theme_bw(base_size = 12) +
    theme(plot.title = element_text(face = "bold", size = 13),
          plot.subtitle = element_text(size = 11, colour = "grey25"),
          legend.position = "bottom",
          panel.grid.minor = element_blank())

  # rolling table of the most recent finished events
  if (nrow(done)) {
    tbl <- done %>% arrange(desc(end_date)) %>% head(N_TABLE) %>%
      transmute(`#` = id,
                Start = format(start_date, "%Y-%m"),
                `Dur (mo)` = duration_months,
                Peak = sprintf("%.1f", peak_index),
                Class = as.character(severity_class))
    tt <- ttheme_minimal(base_size = 9,
                         colhead = list(fg_params = list(fontface = "bold")))
    tg <- tableGrob(tbl, rows = NULL, theme = tt)
    title_g <- textGrob(sprintf("Drought log  (last %d of %d)",
                                min(N_TABLE, nrow(done)), nrow(done)),
                        gp = gpar(fontface = "bold", fontsize = 10))
    right <- arrangeGrob(title_g, tg, heights = unit.c(unit(1.4, "lines"),
                                                       unit(1, "null")))
  } else {
    right <- textGrob("Drought log\n(none yet)",
                      gp = gpar(fontface = "italic", fontsize = 10,
                                col = "grey50"))
  }

  arrangeGrob(p, right, ncol = 2, widths = c(3.2, 1))
}

# -- Render frames ------------------------------------------------------------
cursors <- px$date[seq(1, nrow(px), by = STEP)]
if (tail(cursors, 1) != tail(px$date, 1)) cursors <- c(cursors, tail(px$date, 1))
# hold final frame a beat (repeat last cursor a few times)
cursors <- c(cursors, rep(tail(cursors, 1), FPS))

message(sprintf("Rendering %d frames...", length(cursors)))
frame_files <- character(length(cursors))
for (k in seq_along(cursors)) {
  fp <- file.path(FRAMEDIR, sprintf("frame_%03d.png", k))
  png(fp, width = 1280, height = 620, res = 110)
  grid.draw(make_frame(cursors[k]))
  dev.off()
  frame_files[k] <- fp
  if (k %% 25 == 0) message(sprintf("  %d/%d", k, length(cursors)))
}

# -- Poster (final frame) -----------------------------------------------------
file.copy(tail(frame_files, 1), file.path(OUTDIR, "detection_poster.png"),
          overwrite = TRUE)
message("Poster saved: detection_poster.png")

# -- Stitch GIF (gifski) ------------------------------------------------------
if (requireNamespace("gifski", quietly = TRUE)) {
  gifski::gifski(frame_files,
                 gif_file = file.path(OUTDIR, "detection_benin.gif"),
                 width = 1280, height = 620, delay = 1/FPS, loop = TRUE)
  message("GIF saved: detection_benin.gif")
} else message("gifski not available — GIF skipped.")

# -- Stitch MP4 (av, best-effort) ---------------------------------------------
mp4 <- tryCatch({
  if (requireNamespace("av", quietly = TRUE)) {
    av::av_encode_video(frame_files,
                        output = file.path(OUTDIR, "detection_benin.mp4"),
                        framerate = FPS)
    "detection_benin.mp4"
  } else NA_character_
}, error = function(e) { message("MP4 encode failed: ", conditionMessage(e)); NA_character_ })
if (!is.na(mp4)) message("MP4 saved: ", mp4)

# -- Event list to console (for spot-check) -----------------------------------
cat("\n--- Detected events ---\n")
print(events %>% select(id, start_date, end_date, duration_months,
                        peak_index, severity_class), row.names = FALSE)
message("\nmk_detection_animation.R complete.")
