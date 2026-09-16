# =============================================================================
# P2. Compute self-calibrated PDSI (sc-PDSI) per pixel for Benin
# =============================================================================
# INTER-METHOD comparison (NOT independent validation): PDSI, like SPI/SPEI, is
# a meteorological index from the SAME climate inputs. It is a PHYSICAL water-
# balance model (soil bucket + memory), not a fitted-distribution z-score — so
# it is a useful non-distribution cross-check on SPI/SPEI, and answers the
# team's "SPI is a forced distribution" concern from a different angle.
#
# sc-PDSI (Wells et al. 2004) recalibrates Palmer's constants to local climate
# so extremes are comparable. Inputs: precip + CRU Penman PET + soil AWC.
# AWC uniform 100 mm (standard when no soil map; sc-PDSI absorbs the choice).
# Calibration 1973-2002 (matches SPI/SPEI reference period).
#
# LOCAL run (scPDSI pkg + Rscript). In:
#   spei_penman_results.parquet  -> monthly_precip (per pixel per month)
#   Data/cru_pet/cru_pet_benin_monthly.parquet -> pet_mm (CRU Penman)
# Out: Codes/pdsi_validation/out/pdsi_benin_monthly.parquet (x,y,year,month,pdsi)
# =============================================================================
suppressMessages({ library(arrow); library(dplyr); library(scPDSI) })
setwd("c:/Users/tajayalath/Box/R - Projects - Tharaka/Drought-indices")
DAT <- "Codes/_archive/_data"
OUT <- "Codes/pdsi_validation/out"; dir.create(OUT, showWarnings = FALSE, recursive = TRUE)

AWC      <- 100          # soil available water capacity (mm), uniform
# Calibration = FULL record 1950-2023, to MATCH the SPI/SPEI pipeline (which
# calibrates on the full series; see calibration_year_initial/final in
# 01_v2_SPI_pandas_udf.py). Was 1973-2002 (Ref only) -> made Obs read as near-
# permanent severe drought (mean -3.56) because the Obs period is drier than the
# 1973-2002 baseline. Full-period cal keeps the real drying signal but on the
# same footing as SPI/SPEI (Obs mean -1.67). See P2b_calibration_experiment.R.
# NOTE: whether the whole project should instead use Ref-only calibration
# (option 2) is an OPEN team decision -- see memory project_calibration_open_decision.
CAL0     <- 1950; CAL1 <- 2023
Y0       <- 1950; Y1   <- 2023    # full record (PDSI needs continuous series)

# precip (per pixel per month) from the SPEI-Penman table
pr <- as.data.frame(read_parquet(file.path(DAT, "spei_penman_results.parquet"))) |>
  select(x, y, year, month, monthly_precip)
# CRU Penman PET (already 154 px, mm/month)
pet <- as.data.frame(read_parquet("Data/cru_pet/cru_pet_benin_monthly.parquet")) |>
  select(x, y, year, month, pet_mm)

d <- inner_join(pr, pet, by = c("x", "y", "year", "month")) |>
  filter(year >= Y0, year <= Y1) |>
  arrange(x, y, year, month)
cat("joined rows:", nrow(d), " px:", nrow(distinct(d, x, y)),
    " yrs:", min(d$year), "-", max(d$year), "\n")

# per-pixel sc-PDSI
pix <- distinct(d, x, y)
res <- vector("list", nrow(pix))
for (i in seq_len(nrow(pix))) {
  s <- d |> filter(x == pix$x[i], y == pix$y[i]) |> arrange(year, month)
  # scPDSI::pdsi wants complete monthly series; ours is continuous 1950-2023
  out <- tryCatch(
    pdsi(P = s$monthly_precip, PE = s$pet_mm, AWC = AWC,
         start = min(s$year), end = max(s$year),
         cal_start = CAL0, cal_end = CAL1, sc = TRUE),
    error = function(e) { message(sprintf("px %d failed: %s", i, e$message)); NULL })
  if (is.null(out)) next
  xvals <- as.numeric(out$X)            # monthly sc-PDSI series
  s$pdsi <- xvals[seq_len(nrow(s))]
  res[[i]] <- s[, c("x", "y", "year", "month", "pdsi")]
  if (i %% 40 == 0) cat("  ", i, "/", nrow(pix), "pixels\n")
}
pdsi_df <- bind_rows(res)
cat("\nsc-PDSI rows:", nrow(pdsi_df),
    " px:", nrow(distinct(pdsi_df, x, y)), "\n")
cat("pdsi range:", round(range(pdsi_df$pdsi, na.rm = TRUE), 2),
    " mean:", round(mean(pdsi_df$pdsi, na.rm = TRUE), 3),
    " sd:", round(sd(pdsi_df$pdsi, na.rm = TRUE), 3), "\n")
cat("frac < -2 (severe dry):", round(mean(pdsi_df$pdsi < -2, na.rm = TRUE), 3),
    "  > +2 (very wet):", round(mean(pdsi_df$pdsi > 2, na.rm = TRUE), 3), "\n")

write_parquet(pdsi_df, file.path(OUT, "pdsi_benin_monthly.parquet"))
cat("\nwrote", file.path(OUT, "pdsi_benin_monthly.parquet"), "\nP2 done.\n")
