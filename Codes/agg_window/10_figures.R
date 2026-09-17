# =============================================================================
# 10_figures.R — Q5 reporting-window figures (R / ggplot, matches Q3/Q4 style)
# =============================================================================
# Builds:
#   W1_same_bars_three_windows.png — the identical SPI/SPEI bars drawn per year,
#       per decade and per 30 years, faceted by WMO threshold. The shape never
#       changes; only the axis numbers do.
#   W2_rounding_collapse.png       — the count an audience READS after rounding
#       to whole events, by window. Shows where SPI and SPEI-T collide.
#   W3_gap_vs_spread.png           — the SPI - SPEI-T gap per decade with its
#       bootstrap CI, against the pixel-to-pixel spread.
#
# All inputs are LOCAL parquet written by 01 — no Databricks cluster needed.
# =============================================================================

source("Codes/agg_window/00_config.R")
suppressMessages({ library(arrow); library(dplyr); library(ggplot2); library(tidyr) })
dir.create(Q5$FIGS, showWarnings = FALSE, recursive = TRUE)

FAM_COL <- c("SPI" = "#2c7fb8", "SPEI-T" = "#d95f0e", "SPEI-P" = "#31a354")
base_thm <- theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(),
        legend.position  = "top",
        legend.title     = element_blank(),
        strip.text       = element_text(face = "bold"))

summ <- as.data.frame(read_parquet(file.path(Q5$OUT, "rate_summary.parquet"))) |>
  mutate(thr_f = thr_lab(start_thr), win_f = win_lab(window), sc_f = sc_lab(scale))
coll <- as.data.frame(read_parquet(file.path(Q5$OUT, "collide.parquet"))) |>
  mutate(thr_f = thr_lab(start_thr), win_f = win_lab(window), sc_f = sc_lab(scale))
boot <- as.data.frame(read_parquet(file.path(Q5$OUT, "gap_bootstrap.parquet"))) |>
  mutate(thr_f = thr_lab(start_thr), sc_f = sc_lab(scale))

SC <- 3  # headline accumulation period for the deck

# -- W1: same bars, three reporting windows -----------------------------------
d1 <- summ |> filter(scale == SC)

p1 <- ggplot(d1, aes(fam, count_win, fill = fam)) +
  geom_col(width = 0.68) +
  geom_text(aes(label = sprintf("%.1f", count_win)),
            vjust = -0.35, size = 3) +
  facet_grid(win_f ~ thr_f, scales = "free_y") +
  scale_fill_manual(values = FAM_COL) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
  labs(title = sprintf("The same %d-month result, told on three reporting windows", SC),
       subtitle = paste("Benin, 154 pixels, 2003-2023. Rescaling multiplies every",
                        "index by the same constant:\nthe bar heights are identical,",
                        "only the axis numbers change."),
       x = NULL, y = "drought events") +
  base_thm
ggsave(file.path(Q5$FIGS, "W1_same_bars_three_windows.png"), p1,
       width = 9.5, height = 6.2, dpi = 200)

# -- W2: what the audience reads after rounding -------------------------------
# The bar an audience reads is the rounded one. Collisions are annotated.
d2 <- summ |>
  filter(scale == SC, fam %in% c("SPI", "SPEI-T"))
lab2 <- coll |>
  filter(scale == SC) |>
  mutate(note = ifelse(same_0dp, "same bin", "separates"))

p2 <- ggplot(d2, aes(fam, shown_0dp, fill = fam)) +
  geom_col(width = 0.68) +
  geom_text(aes(label = shown_0dp), vjust = -0.35, size = 3.2) +
  geom_text(data = lab2, inherit.aes = FALSE,
            aes(x = 1.5, y = Inf, label = note,
                colour = same_0dp),
            vjust = 1.5, size = 3.4, fontface = "bold") +
  facet_grid(win_f ~ thr_f, scales = "free_y") +
  scale_fill_manual(values = FAM_COL, guide = "none") +
  scale_colour_manual(values = c("TRUE" = "#c0392b", "FALSE" = "#27ae60"),
                      guide = "none") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.3))) +
  labs(title = "Rounded to whole events, the annual view merges SPI and SPEI-T",
       subtitle = paste("Same rates as above, rounded the way a bar label is read.",
                        "Red = the two indices display the same\nnumber. The gap does",
                        "not appear at longer windows; it stops being rounded away."),
       x = NULL, y = "events, as displayed") +
  base_thm
ggsave(file.path(Q5$FIGS, "W2_rounding_collapse.png"), p2,
       width = 9.5, height = 6.2, dpi = 200)

# -- W3: is the gap larger than the spread across pixels? ---------------------
d3 <- boot |> filter(window == 10)

p3 <- ggplot(d3, aes(sc_f, gap_yr_win)) +
  # pixel-to-pixel spread of the per-pixel gap (+/- 1 sd), the honest backdrop
  geom_crossbar(aes(ymin = gap_yr_win - sd_px_win,
                    ymax = gap_yr_win + sd_px_win),
                width = 0.55, fill = "grey88", colour = NA) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey40") +
  geom_errorbar(aes(ymin = lo_yr_win, ymax = hi_yr_win), width = 0.16,
                linewidth = 0.7, colour = "#2c3e50") +
  geom_point(size = 2.4, colour = "#2c3e50") +
  # agreement share printed under the x axis, one row, so it can never collide
  # with the CI or be clipped at the panel edge
  geom_text(aes(y = -Inf, label = sprintf("%.0f%%", 100 * sign_agree)),
            vjust = -0.6, size = 3.0, colour = "grey25") +
  facet_wrap(~ thr_f, nrow = 1) +
  coord_cartesian(clip = "off") +
  labs(title = "SPI minus SPEI-T, events per decade: real in the mean, not per pixel",
       subtitle = paste("Dark bar = 95% bootstrap CI on the country mean (154 pixels,",
                        "paired). Grey band = +/- 1 sd of the\nper-pixel gap. Where the",
                        "grey band crosses zero, individual pixels disagree with the",
                        "country story."),
       x = "accumulation period  (% = share of pixels whose gap has the country sign)",
       y = "gap (events per decade)") +
  base_thm
ggsave(file.path(Q5$FIGS, "W3_gap_vs_spread.png"), p3,
       width = 9.5, height = 4.6, dpi = 200)

cat("10 figures ->", Q5$FIGS, "\n",
    " W1_same_bars_three_windows.png\n",
    " W2_rounding_collapse.png\n",
    " W3_gap_vs_spread.png\n")
