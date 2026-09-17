# =============================================================================
# N7. Path-A: drought CHARACTERISTICS from SVI at SPI-matched thresholds
# =============================================================================
# The standardized path. SVI is a z-score of NDVI on the SAME scale as SPI/SPEI,
# so we can apply the SAME thresholds (0, -0.5, -1, -1.5), detect drought events
# identically, and compare drought CHARACTERISTICS — frequency, duration,
# severity — like-for-like between the meteorological indices and the vegetation
# index. (This is the novel comparison; the literature uses value correlation.)
#
# SVI is monthly (no accumulation), so we compare it against the 3-month
# meteorological indices (SPI-3, SPEI-T-3, SPEI-P-3) — the scale that best
# matched vegetation in the correlation analysis.
#
# ⚠ Caveat (for the deck): SVI is itself standardized, so this path shares the
# distribution assumption. It is presented alongside the value correlation (N4),
# which does not need a threshold; agreement across both = robust.
#
# LOCAL. In: svi_benin_monthly.parquet, events.parquet (SPI/SPEI events, 3-mo).
# Out: out/N7_char_by_pixel.parquet, out/N7_summary.csv
# =============================================================================
suppressMessages({ library(arrow); library(dplyr); library(tidyr); library(stringr) })
setwd("c:/Users/tajayalath/Box/R - Projects - Tharaka/Drought-indices")
DAT <- "Codes/_archive/_data"; OUT <- "Codes/ndvi_validation/out"
dir.create(OUT, showWarnings = FALSE, recursive = TRUE)

THRS <- c(0.0, -0.5, -1.0, -1.5)
OBS0 <- 2003; OBS1 <- 2023            # MODIS/SVI era
NYR  <- OBS1 - OBS0 + 1

# ---- SVI events (per pixel, per threshold): run-length detection -------------
svi <- as.data.frame(read_parquet(file.path(DAT, "svi_benin_monthly.parquet"))) |>
  filter(year >= OBS0, year <= OBS1, !is.na(svi)) |>
  arrange(x, y, year, month) |>
  mutate(t = (year - 1950) * 12 + month)

svi_char <- lapply(THRS, function(thr) {
  svi |> group_by(x, y) |>
    mutate(dry = svi < thr,
           new = dry & (is.na(lag(dry)) | !lag(dry) | (t - lag(t) != 1)),
           run = cumsum(new)) |>
    filter(dry) |>
    group_by(x, y, run) |>
    summarise(dur = n(), peak = min(svi), .groups = "drop") |>   # peak = driest
    group_by(x, y) |>
    summarise(freq = n() / NYR,
              mean_dur = mean(dur),
              mean_peak = mean(peak), .groups = "drop") |>
    mutate(index = "SVI", start_thr = thr)
}) |> bind_rows()

# ---- SPI/SPEI-3 characteristics from events.parquet (same obs, same thr) ------
ev <- as.data.frame(read_parquet(file.path(DAT, "events.parquet"))) |>
  filter(method == "threshold", event_year >= OBS0, event_year <= OBS1) |>
  mutate(scale = as.integer(str_extract(index, "\\d+")),
         fam = case_when(str_detect(index, "_pm$") ~ "SPEI-P",
                         str_detect(index, "^spei") ~ "SPEI-T",
                         str_detect(index, "^spi") ~ "SPI", TRUE ~ index)) |>
  filter(scale == 3, start_thr %in% THRS)

met_char <- ev |> group_by(fam, start_thr, x, y) |>
  summarise(freq = n() / NYR,
            mean_dur = mean(duration_months, na.rm = TRUE),
            mean_peak = mean(peak_index, na.rm = TRUE), .groups = "drop") |>
  rename(index = fam)

char <- bind_rows(met_char, svi_char) |>
  mutate(index = factor(index, levels = c("SPI","SPEI-T","SPEI-P","SVI")))
write_parquet(char, file.path(OUT, "N7_char_by_pixel.parquet"))

# ---- summary: median characteristic per index x threshold -------------------
summ <- char |> group_by(index, start_thr) |>
  summarise(n_px = n(),
            med_freq = round(median(freq), 2),
            med_dur  = round(median(mean_dur), 2),
            med_peak = round(median(mean_peak), 2), .groups = "drop") |>
  arrange(start_thr, index)
write.csv(summ, file.path(OUT, "N7_summary.csv"), row.names = FALSE)

cat("=== Drought characteristics: SPI/SPEI-3 vs SVI (matched thresholds, obs) ===\n")
print(as.data.frame(summ), row.names = FALSE)
cat("\nN7 done.\n")
