# =============================================================================
# N3. Event-frequency comparison — NDVI (SVI) vs SPI/SPEI (script 09 style)
# =============================================================================
# Detect vegetation-drought EVENTS from SVI (run of months below threshold),
# count per-pixel frequency, then Spearman-correlate across pixels against the
# per-pixel SPI/SPEI drought frequency. Mirrors the CDD validation (script 09):
# "do pixels flagged dry by the met index also show more vegetation-drought?"
#
# LOCAL run. Obs period 2003-2023 (MODIS/NDVI era overlap).
# NDVI event = SVI < THR for >= 1 month; consecutive below-THR months = 1 event.
# SPI/SPEI freq pulled from events.parquet (threshold method) per config/pixel.
#
# In : svi_benin_monthly.parquet (N2), events.parquet (SPI/SPEI events)
# Out: out/N3_ndvi_freq_by_pixel.parquet          (NDVI event freq/pixel)
#      out/N3_spearman_ndvi_vs_indices.csv         (Spearman r per config)
# =============================================================================
suppressMessages({ library(arrow); library(dplyr); library(tidyr); library(stringr) })
setwd("c:/Users/tajayalath/Box/R - Projects - Tharaka/Drought-indices")
DAT <- "Codes/_archive/_data"; OUT <- "Codes/ndvi_validation/out"
dir.create(OUT, showWarnings = FALSE)

OBS0 <- 2003; OBS1 <- 2023
SVI_THR <- -1.0                 # veg-drought threshold (match SPI/SPEI thr -1.0)
P <- 0.05

# --- NDVI events from SVI (consecutive-run detection per pixel) ---------------
svi <- as.data.frame(read_parquet(file.path(DAT, "svi_benin_monthly.parquet"))) |>
  filter(year >= OBS0, year <= OBS1, !is.na(svi)) |>
  arrange(x, y, year, month) |>
  mutate(t = (year - 1950) * 12 + month)

# per pixel: flag below-threshold months, count runs (event = new run start)
ndvi_freq <- svi |>
  group_by(x, y) |>
  mutate(dry = svi < SVI_THR,
         # new event when dry AND (first row OR prev month not consecutive-dry)
         new_evt = dry & (is.na(lag(dry)) | !lag(dry) | (t - lag(t) != 1))) |>
  summarise(ndvi_n_events = sum(new_evt, na.rm = TRUE),
            ndvi_dry_months = sum(dry, na.rm = TRUE),
            n_months = n(),
            ndvi_freq = sum(new_evt, na.rm = TRUE) / (OBS1 - OBS0 + 1),
            .groups = "drop")
cat("NDVI event freq — pixels:", nrow(ndvi_freq),
    " mean events/pixel:", round(mean(ndvi_freq$ndvi_n_events), 1),
    " mean freq/yr:", round(mean(ndvi_freq$ndvi_freq), 3), "\n")
write_parquet(ndvi_freq, file.path(OUT, "N3_ndvi_freq_by_pixel.parquet"))

# --- SPI/SPEI per-pixel freq from events.parquet -----------------------------
ev <- as.data.frame(read_parquet(file.path(DAT, "events.parquet"))) |>
  filter(method == "threshold", event_year >= OBS0, event_year <= OBS1)
# config = index (e.g. spi3) x threshold; keep thr -1.0 to match SVI_THR
spi_freq <- ev |> filter(start_thr == -1.0) |>
  group_by(index, x, y) |>
  summarise(idx_n_events = n(),
            idx_freq = n() / (OBS1 - OBS0 + 1), .groups = "drop")

# --- Spearman across pixels: NDVI freq vs each index config freq -------------
configs <- unique(spi_freq$index)
res <- lapply(configs, function(cf) {
  j <- inner_join(ndvi_freq |> select(x, y, ndvi_freq),
                  spi_freq |> filter(index == cf) |> select(x, y, idx_freq),
                  by = c("x", "y"))
  if (nrow(j) < 5) return(NULL)
  r  <- cor(j$ndvi_freq, j$idx_freq, method = "spearman", use = "complete.obs")
  ts <- r * sqrt((nrow(j) - 2) / (1 - r^2))
  pv <- 2 * pt(abs(ts), df = nrow(j) - 2, lower.tail = FALSE)
  data.frame(index = cf, n_pixels = nrow(j),
             spearman_r = round(r, 3), p_value = round(pv, 5),
             significant = pv <= P)
})
spear <- bind_rows(res) |> arrange(desc(spearman_r))
write.csv(spear, file.path(OUT, "N3_spearman_ndvi_vs_indices.csv"), row.names = FALSE)

cat("\n=== Spearman: NDVI event freq vs SPI/SPEI freq (thr -1.0, obs) ===\n")
print(spear, row.names = FALSE)
cat("\nN3 done.\n")
