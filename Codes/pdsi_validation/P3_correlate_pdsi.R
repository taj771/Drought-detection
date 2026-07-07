# =============================================================================
# P3. Inter-method comparison: SPI / SPEI vs sc-PDSI (per pixel, across time)
# =============================================================================
# NOT an independent validation — PDSI is another meteorological index from the
# same climate inputs. This is a consistency / formulation cross-check.
# Literature (Vicente-Serrano 2010; Wang 2017) expects: sc-PDSI is a medium-to-
# long-memory index (~9-12 months) that correlates best with LONGER SPI/SPEI
# accumulation scales; SPEI should track PDSI more closely than SPI because both
# include PET, especially under a warming/drying trend.
#
# PDSI has one series per pixel (no scale). We correlate it against each
# SPI/SPEI scale (1/3/6/12) per pixel and report the best-matching scale.
# Magnitude of PDSI is irrelevant here — Pearson/Spearman use rank/covariance,
# robust to the wide PDSI range in this water-limited climate.
#
# LOCAL. In: pdsi_benin_monthly.parquet (P2), spi/speit/spei_penman parquets.
# Out: out/P3_corr_by_pixel_scale.parquet, out/P3_summary_by_family_scale.csv
# =============================================================================
suppressMessages({ library(arrow); library(dplyr); library(tidyr) })
setwd("c:/Users/tajayalath/Box/R - Projects - Tharaka/Drought-indices")
DAT <- "Codes/_archive/_data"; OUT <- "Codes/pdsi_validation/out"
dir.create(OUT, showWarnings = FALSE, recursive = TRUE)
SCALES <- c(1, 3, 6, 12); MIN_N <- 60

pdsi <- as.data.frame(read_parquet(file.path(OUT, "pdsi_benin_monthly.parquet"))) |>
  mutate(t = (year - 1950) * 12 + month) |> select(x, y, t, pdsi)

load_idx <- function(file, fam, cols) {
  d <- as.data.frame(read_parquet(file.path(DAT, file)))
  d <- d[, c("x", "y", "year", "month", cols)]
  names(d)[5:8] <- c("1", "3", "6", "12")
  d |> mutate(t = (year - 1950) * 12 + month) |>
    pivot_longer(c(`1`, `3`, `6`, `12`), names_to = "scale", values_to = "idx") |>
    mutate(scale = as.integer(scale), family = fam) |>
    select(x, y, t, family, scale, idx)
}
idx <- bind_rows(
  load_idx("spi_results.parquet",          "SPI",    c("spi1","spi3","spi6","spi12")),
  load_idx("speit.parquet",                "SPEI-T", c("spei1","spei3","spei6","spei12")),
  load_idx("spei_penman_results.parquet",  "SPEI-P", c("spei1","spei3","spei6","spei12")))

j <- inner_join(idx, pdsi, by = c("x", "y", "t")) |>
  filter(!is.na(idx), !is.na(pdsi))

corr <- j |> group_by(x, y, family, scale) |>
  summarise(n = n(),
            r   = if (n() >= MIN_N) cor(idx, pdsi, method = "pearson")  else NA_real_,
            rho = if (n() >= MIN_N) cor(idx, pdsi, method = "spearman") else NA_real_,
            .groups = "drop") |>
  mutate(family = factor(family, levels = c("SPI","SPEI-T","SPEI-P")))
write_parquet(corr, file.path(OUT, "P3_corr_by_pixel_scale.parquet"))

summ <- corr |> filter(!is.na(r)) |>
  group_by(family, scale) |>
  summarise(median_r = round(median(r), 3), median_rho = round(median(rho), 3),
            p25 = round(quantile(r, .25), 3), p75 = round(quantile(r, .75), 3),
            n_px = n(), .groups = "drop") |> arrange(family, scale)
write.csv(summ, file.path(OUT, "P3_summary_by_family_scale.csv"), row.names = FALSE)

cat("=== SPI/SPEI vs sc-PDSI correlation by family x scale (median over 154 px) ===\n")
print(as.data.frame(summ), row.names = FALSE)
cat("\n=== best-matching scale per family (max median r) ===\n")
best <- summ |> group_by(family) |> slice_max(median_r, n = 1)
print(as.data.frame(best), row.names = FALSE)
cat("\nP3 done.\n")
