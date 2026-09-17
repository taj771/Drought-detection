# =============================================================================
# 11_box_windows.R — Q5 box plots: the same frequency told on three windows
# =============================================================================
# The team's question, in the shape of the existing characteristics figure:
# our frequency box plots carry ANNUAL numbers on the y axis. If the y axis is
# converted to events per DECADE and per 30 YEARS -- that is, every pixel's
# annual rate multiplied by 10 and by 30 -- does the median-and-band picture
# still put SPI and SPEI-T in the same bin?
#
# Multiplying by the window is the correct back-end operation: a rate in
# events/yr expressed over w years is rate * w. It is a linear rescaling, so
# the median and every percentile scale by the same factor and the box keeps
# its shape. What changes is the RESOLUTION of the axis the team reads.
#
# Style follows Codes/reports/_slide_helpers_agg.R: box + jittered pixels,
# white diamond = mean, bold label = median, thresholds stacked as facet rows.
# Here the facet COLUMNS are the three reporting windows.
#
# Builds:
#   B1_box_windows.png       — median + IQR band per window, SPI/SPEI-T/SPEI-P
#   B2_band_membership.png   — does the rounded median share a bin, per window
# =============================================================================

source("Codes/agg_window/00_config.R")
suppressMessages({
  library(arrow); library(dplyr); library(tidyr); library(ggplot2); library(stringr)
  library(patchwork)
})
dir.create(Q5$FIGS, showWarnings = FALSE, recursive = TRUE)

SCALES_OUT <- c(3, 6, 12)   # accumulation periods to draw, one figure each

FAM_COL <- c("SPI" = "#c0392b", "SPEI-T" = "#2e8b3d", "SPEI-P" = "#2c6fb5")

RATE <- as.data.frame(read_parquet(file.path(Q5$OUT, "rate_pixel.parquet")))

# -- Per-pixel annual rates, rescaled to each reporting window -----------------
# rate_pixel.parquet already holds one annual rate per pixel x family x scale x
# threshold, with zero-event pixels kept as 0.
prep_px <- function(sc) {
  RATE |>
    filter(scale == sc) |>
    cross_join(tibble(window = Q5$WINDOWS)) |>
    mutate(value  = as_window(rate, window),   # <- the multiply by 1 / 10 / 30
           fam    = factor(fam, levels = c("SPI", "SPEI-T", "SPEI-P")),
           thr_f  = thr_lab(start_thr),
           win_f  = win_lab(window))
}

# -- Box statistics, so the label matches exactly what the box draws -----------
box_stat <- function(px) {
  px |>
    group_by(thr_f, win_f, window, fam) |>
    summarise(med  = median(value),
              q25  = quantile(value, 0.25),
              q75  = quantile(value, 0.75),
              mn   = mean(value),
              ytop = min(quantile(value, 0.75) + 1.5 * IQR(value), max(value)),
              .groups = "drop")
}

# -- What the audience reads: each median rounded to a whole event -------------
# The team reads a whole number off the axis, so round each family's median and
# report the range those rounded values span. At the annual window the three
# indices collapse onto one or two whole numbers; the same three medians over
# 30 years land on widely separated integers.
read_range <- function(stat) {
  stat |>
    group_by(thr_f, win_f, window) |>
    summarise(lo      = min(round(med)),
              hi      = max(round(med)),
              .groups = "drop") |>
    mutate(lab = ifelse(lo == hi,
                        sprintf("all three read as %d event%s",
                                lo, ifelse(lo == 1, "", "s")),
                        sprintf("reads as %d to %d events", lo, hi)))
}

# -- B1: the box picture, one column per reporting window ---------------------
# Each window is drawn as its OWN plot and the three are joined side by side.
# facet_grid(scales = "free_y") frees the scale per ROW only, which would force
# all three windows onto one axis and flatten the per-year boxes to a line.
.one_win <- function(w, show_y, px, stat, dev, sc) {
  dd <- filter(px, window == w)
  ss <- filter(stat, window == w)
  ggplot(dd, aes(fam, value, fill = fam)) +
    geom_boxplot(outlier.shape = NA, width = 0.62, colour = "grey20",
                 linewidth = 0.3) +
    geom_point(position = position_jitter(width = 0.12, seed = 1),
               size = 0.3, alpha = 0.25, colour = "grey15") +
    stat_summary(fun = mean, geom = "point", shape = 23, size = 1.6,
                 fill = "white", colour = "black", stroke = 0.3) +
    # Median label in two pieces: the true value in black, the whole number the
    # audience reads in orange. geom_label cannot colour part of one string, so
    # the halves are drawn as separate layers and nudged apart.
    geom_label(data = ss, aes(x = fam, y = ytop, label = sprintf("%.2f ~", med)),
               inherit.aes = FALSE, size = 2.0, vjust = -0.3, hjust = 1,
               nudge_x = 0.30, colour = "grey15",
               fontface = "bold", fill = "white", alpha = 0.8,
               label.size = 0, label.padding = unit(0.4, "pt")) +
    geom_label(data = ss, aes(x = fam, y = ytop, label = sprintf("%d", round(med))),
               inherit.aes = FALSE, size = 2.4, vjust = -0.28, hjust = 0,
               nudge_x = 0.32, colour = "#d95f02",
               fontface = "bold", fill = "white", alpha = 0.8,
               label.size = 0, label.padding = unit(0.4, "pt")) +
    # Headline: the whole number each index rounds to, and the range those
    # rounded values span. This is the figure an audience reads off the axis.
    geom_text(data = filter(dev, window == w), inherit.aes = FALSE,
              aes(x = 2, y = Inf, label = lab), vjust = 1.25, size = 2.3,
              colour = "#d95f02", fontface = "bold") +
    facet_grid(thr_f ~ win_f, scales = "free_y") +
    scale_fill_manual(values = FAM_COL, name = NULL) +
    scale_colour_manual(values = FAM_COL, guide = "none") +
    scale_y_continuous(expand = expansion(mult = c(0.06, 0.26))) +
    labs(x = NULL,
         y = if (show_y) sprintf("drought events (%d-month accumulation)", sc)
             else NULL) +
    theme_bw(base_size = 9) +
    theme(legend.position = "bottom",
          panel.border = element_rect(colour = "grey30", fill = NA,
                                      linewidth = 0.5),
          strip.background = element_rect(colour = "grey30", fill = "grey92"),
          strip.text = element_text(face = "bold", size = 8),
          panel.grid = element_blank(),
          panel.spacing.y = unit(2, "pt"),
          axis.text.x = element_text(size = 6.5))
}

# One figure per accumulation period. The 3-month file keeps its original name
# so the deck's existing include path does not move.
STATS <- list()
for (sc in SCALES_OUT) {
  px_sc  <- prep_px(sc)
  st_sc  <- box_stat(px_sc)
  dv_sc  <- read_range(st_sc)
  STATS[[as.character(sc)]] <- list(px = px_sc, stat = st_sc, dev = dv_sc)

  p1 <- (.one_win(1,  TRUE,  px_sc, st_sc, dv_sc, sc) |
         .one_win(10, FALSE, px_sc, st_sc, dv_sc, sc) |
         .one_win(30, FALSE, px_sc, st_sc, dv_sc, sc)) +
    plot_layout(guides = "collect") &
    theme(legend.position = "bottom")

  fn <- if (sc == 3) "B1_box_windows.png"
        else sprintf("B1_box_windows_%02dmo.png", sc)
  ggsave(file.path(Q5$FIGS, fn), p1, width = 9.6, height = 6.0, dpi = 200)
  cat("  ", fn, "\n")
}

# The 3-month objects stay in scope under their old names for B2 below.
px <- STATS[["3"]]$px; stat <- STATS[["3"]]$stat; dev <- STATS[["3"]]$dev

# -- B2: does the rounded median still put SPI and SPEI-T in one bin? ----------
# The team reads a whole number off the box. Two indices share a bin when their
# medians round to the same integer.
bin <- stat |>
  filter(fam %in% c("SPI", "SPEI-T")) |>
  select(thr_f, win_f, window, fam, med) |>
  pivot_wider(names_from = fam, values_from = med) |>
  mutate(shown_SPI  = round(SPI),
         shown_SPEI = round(`SPEI-T`),
         same_bin   = shown_SPI == shown_SPEI,
         note       = ifelse(same_bin, "same bin", "separates"))

write_parquet(bin, file.path(Q5$OUT, "band_membership.parquet"))

p2 <- ggplot(bin, aes(x = win_f)) +
  geom_tile(aes(y = 1, fill = same_bin), colour = "white", linewidth = 1.2,
            width = 0.95, height = 0.95) +
  geom_text(aes(y = 1, label = sprintf("%.2f vs %.2f\n%d vs %d\n%s",
                                       SPI, `SPEI-T`, shown_SPI, shown_SPEI, note)),
            size = 2.6, lineheight = 1.05, colour = "white", fontface = "bold") +
  facet_wrap(~ thr_f, nrow = 1) +
  scale_fill_manual(values = c("TRUE" = "#c0392b", "FALSE" = "#2e8b3d"),
                    guide = "none") +
  labs(x = NULL, y = NULL,
       caption = paste("Median of the 154 per-pixel rates. Top = true value,",
                       "middle = the whole number read off the axis.",
                       "\nRed = SPI and SPEI-T collapse into one bin;",
                       "green = the band separates them.")) +
  theme_bw(base_size = 9) +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        panel.grid = element_blank(),
        strip.background = element_rect(colour = "grey30", fill = "grey92"),
        strip.text = element_text(face = "bold", size = 8),
        plot.caption = element_text(hjust = 0, size = 7, colour = "grey30"))

ggsave(file.path(Q5$FIGS, "B2_band_membership.png"), p2,
       width = 9.6, height = 3.0, dpi = 200)

# -- Console summary, so the deck numbers can be quoted exactly ---------------
# What the audience reads at each accumulation period: the rounded range per
# threshold x window. This is the table the deck quotes.
for (sc in SCALES_OUT) {
  cat("\n== reads-as range,", sc, "month ==\n")
  print(as.data.frame(STATS[[as.character(sc)]]$dev |>
                        select(thr_f, win_f, lo, hi)), digits = 3)
}
cat("\n== bin membership, SPI vs SPEI-T (3-month) ==\n")
print(as.data.frame(bin), digits = 3)
cat("\n11 box windows ->", Q5$FIGS, "\n")
