# =============================================================================
# 01_threshold_sweep.R — event count, below-months, and duration vs threshold
# =============================================================================
# Reproduces the char-deck / Q3 frequency numbers straight from events.parquet
# and adds the two contrasting quantities that explain the non-monotonic bump:
#   - event count            (non-monotonic in threshold: the bump)
#   - total below-months     (monotonic decreasing)
#   - mean duration          (= below-months / events; the mirror of the bump)
#
# freq = events / pixel / year, median across pixels, obs window 2003-2023.
# Output: out/sweep.parquet + a printed table.
# =============================================================================

source("Codes/threshold_freq/00_config.R")
suppressMessages({ library(arrow); library(dplyr) })

ev <- as.data.frame(read_parquet(Q4$EVENTS)) |>
  filter(method == "threshold",
         start_thr %in% Q4$THR,
         event_year >= Q4$Y0, event_year <= Q4$Y1)

# family + scale from the index string (spi3, spei6, spei12_pm, ...)
fam_of <- function(ix) ifelse(grepl("_pm$", ix), "SPEI-P",
                       ifelse(grepl("^spei", ix), "SPEI-T", "SPI"))
sc_of  <- function(ix) as.integer(sub("\\D+", "", sub("_pm$", "", ix)))

ev <- ev |> mutate(fam = fam_of(index), scale = sc_of(index))

# per pixel: event count, total below-months (from duration), mean duration
per_pixel <- ev |>
  group_by(fam, scale, start_thr, x, y) |>
  summarise(events       = n(),
            below_months = sum(duration_months, na.rm = TRUE),
            .groups = "drop") |>
  mutate(freq = events / Q4$NYR,
         mean_dur = below_months / events)

# median across pixels per (fam, scale, threshold)
sweep <- per_pixel |>
  group_by(fam, scale, start_thr) |>
  summarise(med_freq         = round(median(freq), 3),
            med_below_months = round(median(below_months), 1),
            med_mean_dur     = round(median(mean_dur), 2),
            n_pixels         = n(),
            .groups = "drop") |>
  arrange(fam, scale, desc(start_thr))

dir.create(Q4$OUT, showWarnings = FALSE, recursive = TRUE)
write_parquet(sweep, file.path(Q4$OUT, "sweep.parquet"))

cat("Saved:", file.path(Q4$OUT, "sweep.parquet"), "\n\n")
cat("=== SPI vs SPEI, scale 3, freq (median) by threshold ===\n")
sweep |>
  filter(scale == 3) |>
  select(fam, start_thr, med_freq, med_below_months, med_mean_dur) |>
  arrange(fam, desc(start_thr)) |>
  print(row.names = FALSE)
