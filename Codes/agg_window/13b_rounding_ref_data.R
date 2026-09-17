# =============================================================================
# 13b_rounding_ref_data.R — reference-window twin of config_rounding.parquet
# =============================================================================
# 13_config_rounding.R writes config_rounding.parquet for the observation
# window only. The C6 bin-stability figure is drawn for both periods, so the
# same quantity is needed over the reference window as well.
#
# The event rule, the threshold set, the pixel grid and the rate definition are
# reproduced exactly as in 13_config_rounding.R -- deriving them differently
# here would make the two figures incomparable, which is the whole point of
# showing them side by side. Only the year window differs.
#
# Builds:
#   config_rounding_ref.parquet        — per-config rate and displayed value
#   config_characteristics_ref.parquet — per-config mean duration and peak
# =============================================================================

source("Codes/agg_window/00_config.R")

suppressMessages({
  library(arrow); library(dplyr); library(tidyr); library(stringr)
})

ALL_THR <- c(0.0, -0.25, -0.5, -1.0, -1.5)

ev_all <- as.data.frame(read_parquet(Q5$EVENTS)) |>
  filter(method == "threshold",
         start_thr %in% ALL_THR,
         min_duration == Q5$MINDUR) |>
  mutate(scale = as.integer(str_extract(index, "\\d+")),
         fam   = case_when(str_starts(index, "spi")  ~ "SPI",
                           str_ends(index,   "_pm")  ~ "SPEI-P",
                           str_starts(index, "spei") ~ "SPEI-T"))

# Grid from the FULL table so event-free pixels stay true zeros in the window.
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

# Duration and peak severity average over the events a config produces, not
# over the pixel grid, so a zero-event pixel contributes nothing rather than an
# invented 0-month drought.
.cfg_char <- function(ev) {
  ev |>
    group_by(fam, scale, start_thr) |>
    summarise(dur  = mean(duration_months),
              peak = mean(peak_index),
              .groups = "drop")
}

ev_ref <- filter(ev_all, event_year >= Q5$R0, event_year <= Q5$R1)
cfg    <- .cfg_rate(ev_ref, Q5$NRYR)
char   <- .cfg_char(ev_ref)

cat("reference window:", Q5$R0, "-", Q5$R1, "|", Q5$NRYR, "years\n")
cat("configs summarised:", nrow(cfg), "\n")

cfg_win <- cfg |>
  cross_join(tibble(window = Q5$WINDOWS)) |>
  mutate(value = as_window(rate, window),
         shown = round(value),
         win_f = win_lab(window),
         thr_f = factor(sprintf("thr %.2f", start_thr),
                        levels = sprintf("thr %.2f", ALL_THR)))

write_parquet(cfg_win, file.path(Q5$OUT, "config_rounding_ref.parquet"))
write_parquet(char,    file.path(Q5$OUT, "config_characteristics_ref.parquet"))

cat("wrote config_rounding_ref.parquet        ", nrow(cfg_win), "rows\n")
cat("wrote config_characteristics_ref.parquet ", nrow(char),    "rows\n")
