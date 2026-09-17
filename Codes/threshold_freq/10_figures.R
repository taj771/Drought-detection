# =============================================================================
# 10_figures.R — Q4 threshold-frequency figures (R / ggplot, matches Q3 style)
# =============================================================================
# Builds:
#   split_demo_speit3.png  — 4-panel: same SPEI-3 series cut at each threshold,
#                            below-runs shaded + numbered, count vs below-months
#   split_demo_spi3.png    — same for SPI-3 (contrast: monotonic, no bump)
#   freq_duration_mirror.png — median freq (bump) vs median duration (dip) by thr
#
# All inputs are LOCAL parquet — no Databricks cluster needed.
# =============================================================================

source("Codes/threshold_freq/00_config.R")
suppressMessages({ library(arrow); library(dplyr); library(ggplot2); library(tidyr) })
dir.create(Q4$FIGS, showWarnings = FALSE, recursive = TRUE)

# -- load one pixel's monthly series ------------------------------------------
load_pixel <- function(path, col) {
  d <- as.data.frame(read_parquet(path))
  # tolerate column-name variants
  if (!col %in% names(d)) stop(sprintf("column %s not in %s", col, path))
  d |>
    filter(x == Q4$DEMO_X, y == Q4$DEMO_Y,
           year >= Q4$Y0, year <= Q4$Y1) |>
    arrange(year, month) |>
    mutate(date = as.Date(sprintf("%d-%02d-01", year, month))) |>
    transmute(date, val = .data[[col]])
}

# -- shaded below-runs for one threshold --------------------------------------
runs_below <- function(df, thr) {
  b <- !is.na(df$val) & (df$val < thr)
  r <- rle(b)
  ends   <- cumsum(r$lengths)
  starts <- c(1L, head(ends, -1) + 1L)
  idx <- which(r$values)
  if (!length(idx)) return(NULL)
  data.frame(
    event = seq_along(idx),
    xmin  = df$date[starts[idx]],
    xmax  = df$date[ends[idx]]
  )
}

# -- 4-panel split-demo figure ------------------------------------------------
split_demo <- function(df, label, out_png) {
  panels <- lapply(Q4$THR, function(thr) {
    rb <- runs_below(df, thr)
    n_ev <- if (is.null(rb)) 0L else nrow(rb)
    n_mo <- below_months(df$val, thr)
    list(thr = thr, rb = rb,
         title = sprintf("thr = %+.1f :  %d events  |  %d below-months",
                         thr, n_ev, n_mo))
  })

  # long frame of the series repeated per panel
  ser <- do.call(rbind, lapply(panels, function(p)
    transform(df, panel = p$title)))
  ser$panel <- factor(ser$panel, levels = sapply(panels, `[[`, "title"))

  ytop <- max(df$val, na.rm = TRUE)   # label near the top of every panel
  shade <- do.call(rbind, lapply(panels, function(p) {
    if (is.null(p$rb)) return(NULL)
    transform(p$rb, panel = p$title, ylab = ytop)
  }))
  if (!is.null(shade)) shade$panel <- factor(shade$panel, levels = levels(ser$panel))

  # Drought MONTHS (points on the line, only where value < thr) so each event
  # has a visible marker and the number clearly tags a drought spell, not a gap.
  dmon <- do.call(rbind, lapply(panels, function(p) {
    b <- !is.na(df$val) & (df$val < p$thr)
    if (!any(b)) return(NULL)
    transform(df[b, ], panel = p$title)
  }))
  if (!is.null(dmon)) dmon$panel <- factor(dmon$panel, levels = levels(ser$panel))
  thr_line <- do.call(rbind, lapply(panels, function(p)
    data.frame(panel = p$title, thr = p$thr)))
  thr_line$panel <- factor(thr_line$panel, levels = levels(ser$panel))

  p <- ggplot(ser, aes(date, val)) +
    { if (!is.null(shade)) geom_rect(data = shade,
        aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
        inherit.aes = FALSE, fill = "#f2a15a", alpha = 0.35) } +
    geom_hline(data = thr_line, aes(yintercept = thr),
               linetype = "dashed", colour = "#c0392b", linewidth = 0.5) +
    geom_line(linewidth = 0.35, colour = "grey15") +
    { if (!is.null(dmon)) geom_point(data = dmon, aes(date, val),
        inherit.aes = FALSE, colour = "#c0392b", size = 0.7) } +
    { if (!is.null(shade)) geom_text(data = shade,
        aes(x = xmin + (xmax - xmin)/2, y = ylab, label = event),
        inherit.aes = FALSE, size = 2.3, fontface = "bold",
        colour = "#7b3f00", vjust = -0.3) } +
    facet_wrap(~panel, ncol = 1, scales = "fixed") +
    scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +
    labs(title = sprintf("Threshold split / event-fragmentation demo — %s", label),
         subtitle = sprintf("pixel x=%.3f, y=%.3f (Benin, %d-%d)",
                            Q4$DEMO_X, Q4$DEMO_Y, Q4$Y0, Q4$Y1),
         x = "Time (monthly)", y = label) +
    theme_bw(base_size = 9) +
    theme(strip.text = element_text(face = "bold", hjust = 0),
          panel.grid.minor = element_blank())

  ggsave(out_png, p, width = 11, height = 7, dpi = 150)
  cat("Saved:", out_png, "\n")
}

speit3 <- load_pixel(Q4$SPEIT, "spei3")
spi3   <- load_pixel(Q4$SPI,   "spi3")
split_demo(speit3, "SPEI-3 (Thornthwaite)", file.path(Q4$FIGS, "split_demo_speit3.png"))
split_demo(spi3,   "SPI-3",                 file.path(Q4$FIGS, "split_demo_spi3.png"))

# -- freq / duration mirror (aggregate, from sweep.parquet if present) --------
sweep_f <- file.path(Q4$OUT, "sweep.parquet")
if (file.exists(sweep_f)) {
  sw <- as.data.frame(read_parquet(sweep_f)) |> filter(scale == 3)
  long <- sw |>
    select(fam, start_thr, med_freq, med_mean_dur) |>
    pivot_longer(c(med_freq, med_mean_dur), names_to = "metric", values_to = "v") |>
    mutate(metric = recode(metric, med_freq = "Frequency (events/yr)",
                                   med_mean_dur = "Mean duration (months)"))
  pm <- ggplot(long, aes(factor(start_thr, levels = sort(unique(start_thr))),
                         v, colour = fam, group = fam)) +
    geom_line() + geom_point() +
    facet_wrap(~metric, scales = "free_y") +
    labs(title = "Frequency bumps up where duration dips down (scale 3)",
         x = "Threshold", y = NULL, colour = "Index") +
    theme_bw(base_size = 10)
  ggsave(file.path(Q4$FIGS, "freq_duration_mirror.png"), pm,
         width = 9, height = 4, dpi = 150)
  cat("Saved:", file.path(Q4$FIGS, "freq_duration_mirror.png"), "\n")
}
