# =============================================================================
# N4. Value correlation — SVI vs SPI/SPEI (per pixel, across time, lags+scales)
# =============================================================================
# Core validation (Vicente-Serrano 2013 / Ji & Peters 2003 protocol).
# For each pixel, correlate the monthly SVI series against each meteorological
# index series (SPI, SPEI-Thornthwaite, SPEI-Penman) at accumulation scales
# 1/3/6/12 and lags 0-3 months. Pearson r. NDVI lags rain, so index is shifted
# BACK relative to SVI: corr( SVI[t], index[t-lag] ).
#
# LOCAL run (no Databricks) — all inputs are small parquets.
# In : svi_benin_monthly.parquet (N2) + spi/spei_thornthwaite/spei_penman_results
# Out: Codes/ndvi_validation/out/N4_corr_by_pixel_scale_lag.parquet  (full grid)
#      Codes/ndvi_validation/out/N4_best_per_pixel.parquet            (argmax r)
#      Codes/ndvi_validation/out/N4_summary_by_index_scale_lag.csv    (medians)
# =============================================================================
suppressMessages({ library(arrow); library(dplyr); library(tidyr) })
setwd("c:/Users/tajayalath/Box/R - Projects - Tharaka/Drought-indices")
DAT <- "Codes/_archive/_data"
OUT <- "Codes/ndvi_validation/out"; dir.create(OUT, showWarnings = FALSE)

SCALES <- c(1, 3, 6, 12)
LAGS   <- 0:3
MIN_N  <- 24            # need >=24 paired months for a stable r

svi <- as.data.frame(read_parquet(file.path(DAT, "svi_benin_monthly.parquet"))) |>
  select(x, y, year, month, svi)

# load the three met indices, tag family, pivot scales to long.
# cols must be in scale order 1,3,6,12 (e.g. spi1,spi3,spi6,spi12).
load_idx <- function(file, fam, cols) {
  d <- as.data.frame(read_parquet(file.path(DAT, file)))
  d <- d[, c("x", "y", "year", "month", cols)]
  names(d)[5:8] <- c("1", "3", "6", "12")
  d |> pivot_longer(c(`1`, `3`, `6`, `12`),
                    names_to = "scale", values_to = "idx") |>
    mutate(scale = as.integer(scale), family = fam)
}
idx <- bind_rows(
  load_idx("spi_results.parquet",              "SPI",    c("spi1","spi3","spi6","spi12")),
  load_idx("speit.parquet",                    "SPEI-T", c("spei1","spei3","spei6","spei12")),
  load_idx("spei_penman_results.parquet",      "SPEI-P", c("spei1","spei3","spei6","spei12"))
)

# monotone month index (for lagging): months since 1950-01
mi <- function(y, m) (y - 1950) * 12 + m
svi$t   <- mi(svi$year, svi$month)
idx$t   <- mi(idx$year, idx$month)

# correlate per pixel x family x scale x lag
res <- list(); k <- 0
for (lag in LAGS) {
  # shift index forward by lag so idx at t-lag lines up with svi at t
  idx_l <- idx |> mutate(t = t + lag) |> select(x, y, t, family, scale, idx)
  j <- svi |> inner_join(idx_l, by = c("x", "y", "t"))
  # Pearson + Spearman (lit prefers Spearman for skewed NDVI/SVI;
  # Vicente-Serrano 2013 uses Pearson — report both).
  g <- j |> group_by(x, y, family, scale) |>
    summarise(n = sum(!is.na(svi) & !is.na(idx)),
              r  = if (sum(!is.na(svi) & !is.na(idx)) >= MIN_N)
                     cor(svi, idx, use = "complete.obs", method = "pearson")
                   else NA_real_,
              rho = if (sum(!is.na(svi) & !is.na(idx)) >= MIN_N)
                      cor(svi, idx, use = "complete.obs", method = "spearman")
                    else NA_real_,
              .groups = "drop") |>
    mutate(lag = lag)
  k <- k + 1; res[[k]] <- g
}
corr <- bind_rows(res)
cat("corr rows:", nrow(corr), " (154 px x 3 fam x 4 scale x 4 lag =",
    154*3*4*4, ")\n")
write_parquet(corr, file.path(OUT, "N4_corr_by_pixel_scale_lag.parquet"))

# best (scale,lag) per pixel per family — max r
best <- corr |> filter(!is.na(r)) |>
  group_by(x, y, family) |>
  slice_max(r, n = 1, with_ties = FALSE) |> ungroup()
write_parquet(best, file.path(OUT, "N4_best_per_pixel.parquet"))

# summary: median r AND rho across pixels, per family x scale x lag
summ <- corr |> filter(!is.na(r)) |>
  group_by(family, scale, lag) |>
  summarise(median_r   = round(median(r), 3),
            median_rho = round(median(rho), 3),
            mean_r     = round(mean(r), 3),
            p25 = round(quantile(r, .25), 3),
            p75 = round(quantile(r, .75), 3),
            n_px = n(), .groups = "drop") |>
  arrange(family, scale, lag)
write.csv(summ, file.path(OUT, "N4_summary_by_index_scale_lag.csv"), row.names = FALSE)

cat("\n=== median SVI~index correlation by family/scale/lag (top rows) ===\n")
print(as.data.frame(summ |> group_by(family) |> slice_max(median_r, n = 3)),
      row.names = FALSE)

cat("\n=== best-matching (scale,lag) per family (median over pixels) ===\n")
bestcfg <- summ |> group_by(family) |> slice_max(median_r, n = 1)
print(as.data.frame(bestcfg), row.names = FALSE)

cat("\nN4 done.\n")
