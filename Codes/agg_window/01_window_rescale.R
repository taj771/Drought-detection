# =============================================================================
# 01 — Drought frequency under three reporting windows (1 / 10 / 30 years).
# =============================================================================
# Reads the detected events (all thresholds) and produces, for every
# index-family x scale x WMO threshold:
#   * the per-pixel event rate (events/yr) and its country mean
#   * that rate expressed per decade and per 30 years
#   * the value an audience reads off the bar at 0 and 1 decimal places
#   * whether SPI and SPEI-T still separate after that rounding
#   * a bootstrap CI on the SPI - SPEI-T gap, to test the gap against the
#     pixel-to-pixel spread rather than against zero
#
# Outputs: out/rate_pixel.parquet     per-pixel rates (the raw material)
#          out/rate_summary.parquet   country means at each reporting window
#          out/gap_bootstrap.parquet  SPI vs SPEI-T gap + CI, per thr x scale
# =============================================================================
source("Codes/agg_window/00_config.R")
suppressMessages({library(arrow); library(dplyr); library(tidyr); library(stringr)})

ev <- as.data.frame(read_parquet(Q5$EVENTS)) |>
  filter(method == "threshold",
         event_year >= Q5$Y0, event_year <= Q5$Y1,
         start_thr %in% Q5$THR,
         min_duration == Q5$MINDUR)

# `index` is spi3 / spei3 / spei3_pm -> family + accumulation scale.
ev <- ev |>
  mutate(scale = as.integer(str_extract(index, "\\d+")),
         fam   = case_when(str_starts(index, "spi")            ~ "SPI",
                           str_ends(index,   "_pm")            ~ "SPEI-P",
                           str_starts(index, "spei")           ~ "SPEI-T")) |>
  filter(scale %in% Q5$SCALES)

# Full pixel grid, so pixels with zero events in a cell are counted as 0 and do
# not silently vanish from the mean.
px   <- ev |> distinct(x, y)
cells <- expand_grid(px, fam = unique(ev$fam), scale = Q5$SCALES, start_thr = Q5$THR)

rate_pixel <- ev |>
  count(x, y, fam, scale, start_thr, name = "events") |>
  right_join(cells, by = c("x", "y", "fam", "scale", "start_thr")) |>
  mutate(events = coalesce(events, 0L),
         rate   = events / Q5$NYR)          # events per year, per pixel

write_parquet(rate_pixel, file.path(Q5$OUT, "rate_pixel.parquet"))

# -- Country mean rate, expressed at each reporting window --------------------
rate_summary <- rate_pixel |>
  group_by(fam, scale, start_thr) |>
  summarise(rate     = mean(rate),
            sd_px    = sd(rate),
            q25      = quantile(rate, 0.25),
            q75      = quantile(rate, 0.75),
            .groups  = "drop") |>
  cross_join(tibble(window = Q5$WINDOWS)) |>
  mutate(count_win  = as_window(rate, window),      # events over the window
         iqr_win    = as_window(q75 - q25, window),
         shown_0dp  = displayed(count_win, 0),
         shown_1dp  = displayed(count_win, 1),
         ret_months = return_months(rate))

write_parquet(rate_summary, file.path(Q5$OUT, "rate_summary.parquet"))

# -- Does SPI still separate from SPEI-T after rounding? ----------------------
# Two indices "collide" in a bin when the numbers an audience reads are equal.
collide <- rate_summary |>
  filter(fam %in% c("SPI", "SPEI-T")) |>
  select(fam, scale, start_thr, window, count_win, shown_0dp, shown_1dp) |>
  pivot_wider(names_from = fam, values_from = c(count_win, shown_0dp, shown_1dp)) |>
  mutate(gap        = `count_win_SPI` - `count_win_SPEI-T`,
         same_0dp   = `shown_0dp_SPI` == `shown_0dp_SPEI-T`,
         same_1dp   = `shown_1dp_SPI` == `shown_1dp_SPEI-T`)

write_parquet(collide, file.path(Q5$OUT, "collide.parquet"))

# -- Is the gap bigger than the spread across pixels? -------------------------
# Paired bootstrap over pixels: both indices are measured on the SAME 154
# pixels, so resample pixels, not index values.
set.seed(Q5$SEED)
wide <- rate_pixel |>
  filter(fam %in% c("SPI", "SPEI-T")) |>
  select(x, y, fam, scale, start_thr, rate) |>
  pivot_wider(names_from = fam, values_from = rate)

boot_gap <- wide |>
  group_by(scale, start_thr) |>
  group_modify(function(d, key) {
    diffs <- d$SPI - d$`SPEI-T`
    n     <- length(diffs)
    bs    <- replicate(Q5$NBOOT, mean(sample(diffs, n, replace = TRUE)))
    tibble(gap_yr      = mean(diffs),
           lo_yr       = unname(quantile(bs, 0.025)),
           hi_yr       = unname(quantile(bs, 0.975)),
           sd_px       = sd(diffs),
           # share of pixels where the sign of the gap agrees with the mean
           sign_agree  = mean(sign(diffs) == sign(mean(diffs))))
  }) |>
  ungroup() |>
  cross_join(tibble(window = Q5$WINDOWS)) |>
  mutate(across(c(gap_yr, lo_yr, hi_yr, sd_px), ~ .x * window,
                .names = "{.col}_win"))

write_parquet(boot_gap, file.path(Q5$OUT, "gap_bootstrap.parquet"))

cat("01 window rescale ->", Q5$OUT, "\n",
    "  rate_pixel  ", nrow(rate_pixel),  "rows\n",
    "  rate_summary", nrow(rate_summary), "rows\n",
    "  collide     ", nrow(collide),      "rows\n",
    "  gap_boot    ", nrow(boot_gap),     "rows\n")
