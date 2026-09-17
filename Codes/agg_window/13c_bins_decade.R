# =============================================================================
# 13c_bins_decade.R — C5a bin membership on the DECADE reporting window
# =============================================================================
# 13_config_rounding.R draws the C5 bin trees on the annual window: the value
# binned is the mean per-pixel event rate per YEAR, rounded to a whole number.
# The team asked what the same picture looks like when the report quotes a
# per-decade count instead.
#
# Only the frequency characteristic is redrawn. Duration and severity do not
# rescale with the reporting window -- a four-month drought is four months
# however often the report is published -- so a "decadal" version of those two
# would be the annual figure under a misleading title.
#
# The rate is the same number 13 computes; only the multiplier and the bin
# labels change (rate * 10, rounded once, after the rescale). The drawing
# functions are taken from 13_config_rounding.R itself rather than copied, so
# the annual and decadal figures cannot drift apart in layout.
#
# Builds:
#   C5d_bins_frequency_decade.png      — observation window 2003-2023
#   C5d_bins_frequency_decade_ref.png  — reference window 1973-2002
# =============================================================================

source("Codes/agg_window/00_config.R")
suppressMessages({
  library(arrow); library(dplyr); library(tidyr); library(ggplot2)
  library(stringr); library(patchwork)
})

SRC <- "Codes/agg_window/13_config_rounding.R"

# Pull the shared constants and the three drawing functions out of script 13 by
# name. Sourcing the whole file would re-run the C1-C4 figures; copying the
# functions here would let the two versions drift. Parsing and evaluating just
# the wanted top-level definitions avoids both.
.borrow <- function(path, names) {
  ex  <- parse(path)
  got <- character(0)
  for (e in ex) {
    if (is.call(e) && length(e) >= 3 &&
        as.character(e[[1]]) %in% c("<-", "=") &&
        is.name(e[[2]]) && as.character(e[[2]]) %in% names) {
      eval(e, envir = globalenv())
      got <- c(got, as.character(e[[2]]))
    }
  }
  missing <- setdiff(names, got)
  if (length(missing)) stop("not found in ", path, ": ",
                            paste(missing, collapse = ", "))
}

.borrow(SRC, c("ALL_THR", "FAM_LV", "FAM_COL", "BAR_W",
               "LEAF_MAX", "WRAP_CH", "PAD_ROW",
               ".bin_layout", ".bin_tree", ".bin_bar"))

# Events table and pixel grid, same filters as script 13.
ev_all <- as.data.frame(read_parquet(Q5$EVENTS)) |>
  filter(method == "threshold",
         start_thr %in% ALL_THR,
         min_duration == Q5$MINDUR) |>
  mutate(scale = as.integer(str_extract(index, "\\d+")),
         fam   = case_when(str_starts(index, "spi")  ~ "SPI",
                           str_ends(index,   "_pm")  ~ "SPEI-P",
                           str_starts(index, "spei") ~ "SPEI-T"))

grid <- expand_grid(distinct(ev_all, x, y),
                    distinct(ev_all, fam, scale, start_thr))

.cfg_rate <- function(ev, nyr) {
  ev |>
    count(x, y, fam, scale, start_thr, name = "events") |>
    right_join(grid, by = c("x", "y", "fam", "scale", "start_thr")) |>
    mutate(events = coalesce(events, 0L), rate = events / nyr) |>
    group_by(fam, scale, start_thr) |>
    summarise(rate = mean(rate), .groups = "drop")
}

# Decadal twin of .c5_data, frequency only. The rescale happens before the
# rounding -- that ordering is the whole reason the decadal bins differ from
# the annual ones times ten.
.c5_data_decade <- function(cfg_tab) {
  cfg_tab |>
    transmute(metric = factor("events per decade"),
              fam, scale, start_thr,
              value  = rate * 10,
              digits = 0,
              shown  = round(rate * 10),
              fam_f  = factor(fam, levels = FAM_LV),
              cfg    = sprintf("%s-%d @ %.2f", fam, scale, start_thr))
}

PERIODS <- list(
  list(tag = "",     lab = sprintf("observation %d-%d", Q5$Y0, Q5$Y1),
       y0 = Q5$Y0, y1 = Q5$Y1, nyr = Q5$NYR),
  list(tag = "_ref", lab = sprintf("reference %d-%d",   Q5$R0, Q5$R1),
       y0 = Q5$R0, y1 = Q5$R1, nyr = Q5$NRYR)
)

for (pr in PERIODS) {
  ev_p <- filter(ev_all, event_year >= pr$y0, event_year <= pr$y1)
  dd   <- .c5_data_decade(.cfg_rate(ev_p, pr$nyr))
  lay  <- .bin_layout(dd)
  p    <- .bin_bar(dd, lay) +
          .bin_tree(lay, sprintf("events per decade, %s", pr$lab)) +
          plot_layout(widths = c(1, 1.9))

  ggsave(file.path(Q5$FIGS,
                   sprintf("C5d_bins_frequency_decade%s.png", pr$tag)), p,
         width = 10.0,
         height = max(3.6, min(9, 0.17 * sum(lay$nodes$h) + 1.2)),
         dpi = 200)

  cat(sprintf("\n== decadal frequency bins (%s) ==\n", pr$lab))
  print(as.data.frame(count(dd, shown, fam_f, name = "n")))
  cat("distinct decadal bins:", dplyr::n_distinct(dd$shown), "of 60 configs\n")
}
