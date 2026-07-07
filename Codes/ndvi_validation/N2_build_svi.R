# =============================================================================
# N2. Build SVI (Standardized Vegetation Index) from NDVI anomaly
# =============================================================================
# SVI = z-score of NDVI per pixel per calendar month (Peters et al. 2002).
# We already have the anomaly (NDVI - 2001-2023 monthly mean, from the source
# S2001-2023 product), so mean(anom) ~ 0 per pixel-month already. SVI is then
# just anomaly / sd, where sd is computed per pixel per calendar month over the
# baseline years (2001-2023, matching the product baseline).
#
# WHY SVI: SPI/SPEI are z-scores; SVI is the z-score of NDVI -> same units,
# same thresholds (-0.5/-1/-1.5) -> apples-to-apples with the met indices.
#
# In : Codes/_archive/_data/ndvi_anom_benin_monthly.parquet (N1 output)
#      cols x, y, year, month, ndvi_anom  (43,104 rows, 154 px, 2000-2023)
# Out: Codes/_archive/_data/svi_benin_monthly.parquet
#      cols x, y, year, month, ndvi_anom, ndvi_sd, svi
# =============================================================================
suppressMessages({ library(arrow); library(dplyr) })

BASE0 <- 2001; BASE1 <- 2023           # baseline for sd (matches S2001-2023)
IN  <- "Codes/_archive/_data/ndvi_anom_benin_monthly.parquet"
OUT <- "Codes/_archive/_data/svi_benin_monthly.parquet"

d <- as.data.frame(read_parquet(IN))
cat("in rows:", nrow(d), " pixels:", nrow(distinct(d, x, y)),
    " years:", min(d$year), "-", max(d$year), "\n")

# per pixel per calendar month: sd of anomaly over baseline years
sd_tab <- d |>
  filter(year >= BASE0, year <= BASE1) |>
  group_by(x, y, month) |>
  summarise(ndvi_sd = sd(ndvi_anom, na.rm = TRUE),
            n_base  = sum(!is.na(ndvi_anom)), .groups = "drop")

cat("pixel-month sd cells:", nrow(sd_tab),
    " (expect ~154*12 =", 154*12, ")\n")
cat("sd range:", round(range(sd_tab$ndvi_sd, na.rm = TRUE), 4),
    " median n_base:", median(sd_tab$n_base), "\n")

svi <- d |>
  left_join(sd_tab, by = c("x", "y", "month")) |>
  mutate(svi = ifelse(ndvi_sd > 0, ndvi_anom / ndvi_sd, NA_real_))

# sanity: SVI over baseline should be ~mean 0, sd ~1 per pixel-month
chk <- svi |> filter(year >= BASE0, year <= BASE1) |>
  group_by(x, y, month) |>
  summarise(m = mean(svi, na.rm = TRUE), s = sd(svi, na.rm = TRUE),
            .groups = "drop")
cat("\nSVI baseline check — mean(mean):", round(mean(chk$m, na.rm=TRUE), 3),
    " mean(sd):", round(mean(chk$s, na.rm = TRUE), 3), "(expect ~0 and ~1)\n")
cat("SVI overall range:", round(range(svi$svi, na.rm = TRUE), 2), "\n")
cat("n SVI < -1 (veg drought):", sum(svi$svi < -1, na.rm = TRUE),
    "of", sum(!is.na(svi$svi)), "\n")

write_parquet(svi, OUT)
cat("\nwrote", OUT, "rows:", nrow(svi), "\n")
