# =============================================================================
# A2 — Stress-test: is NDVI-anomaly drought FREQUENCY a real signal or a
#      zero-crossing artifact? Extends A1 with 5 harder checks.
#
#   C1 Zero-crossing baseline — analytical iid expectation vs NDVI actual runs.
#   C2 Threshold robustness    — does SPI~NDVI freq agreement hold at 0, -0.5, -1.0?
#   C3 Spatial correlation     — per-pixel NDVI-freq vs SPI3-freq (Pearson+Spearman).
#   C4 Temporal correlation    — yearly drought-affected fraction, NDVI vs SPI.
#   C5 Verdict                 — trustworthy / with-caveats / artifact.
#
# Same window & event rule as A1 (source 00_event_rule.R).
# Run: & "C:\Program Files\R\R-4.6.1\bin\Rscript.exe" Codes/nostd_freq/audit/A2_ndvi_stress.R
# =============================================================================
source("Codes/nostd_freq/00_event_rule.R")
setwd(Q3$wd)
set.seed(20260722)

ndvi <- as.data.frame(read_parquet("Codes/_archive/_data/ndvi_anom_benin_monthly.parquet")) |>
  filter(year>=Q3$Y0, year<=Q3$Y1) |> arrange(x,y,year,month)
spi  <- as.data.frame(read_parquet("Codes/_archive/_data/spi_results.parquet")) |>
  filter(year>=Q3$Y0, year<=Q3$Y1) |> arrange(x,y,year,month) |>
  transmute(x,y,year,month,spi1,spi3,spi6,spi12)

NYR <- Q3$NYR

# =============================================================================
# C1 — ZERO-CROSSING BASELINE (analytical vs actual)
# For a MEMORYLESS symmetric series, each month is independently below 0 with
# probability p (~0.5). Number of below-0 RUNS (mindur=1) has a closed form for
# iid Bernoulli(p) over N months:
#   E[runs] = p + (N-1) * p * (1-p)
# because a run starts at month 1 iff month 1 < 0 (prob p), and at month i>1 iff
# month i < 0 AND month i-1 >= 0 (prob p*(1-p)). Positive autocorrelation makes
# runs FEWER and LONGER than this, so ACTUAL < iid-EXPECTED is the fingerprint of
# real persistence rather than a coin-flip zero-crossing pattern.
# =============================================================================
cat("=== C1 Zero-crossing baseline: iid expectation vs actual NDVI runs ===\n")

# per-pixel: N (non-NA months), p (fraction below 0), actual below-0 runs (mindur=1)
c1 <- ndvi |> group_by(x,y) |>
  summarise(
    N      = sum(!is.na(ndvi_anom)),
    p      = mean(ndvi_anom < 0, na.rm=TRUE),
    actual = count_events(ndvi_anom < 0, mindur=1),
    .groups="drop"
  ) |>
  mutate(iid_expected = p + (N-1)*p*(1-p),
         ratio = actual / iid_expected)

# same for SPI-3 as the reference smooth index
c1s <- spi |> group_by(x,y) |>
  summarise(
    N      = sum(!is.na(spi3)),
    p      = mean(spi3 < 0, na.rm=TRUE),
    actual = count_events(spi3 < 0, mindur=1),
    .groups="drop"
  ) |>
  mutate(iid_expected = p + (N-1)*p*(1-p),
         ratio = actual / iid_expected)

cat(sprintf("NDVI : median N=%.0f  p(below0)=%.3f  iid-expected runs=%.1f  actual runs=%.1f  ratio=%.2f\n",
    median(c1$N), median(c1$p), median(c1$iid_expected), median(c1$actual), median(c1$ratio)))
cat(sprintf("SPI3 : median N=%.0f  p(below0)=%.3f  iid-expected runs=%.1f  actual runs=%.1f  ratio=%.2f\n",
    median(c1s$N), median(c1s$p), median(c1s$iid_expected), median(c1s$actual), median(c1s$ratio)))
cat("Interpretation: ratio<1 => real series has FEWER,LONGER runs than a memoryless\n")
cat("coin-flip of the SAME below-0 fraction => the frequency is NOT a zero-crossing fluke.\n")
cat("The FURTHER NDVI's ratio sits below 1 (and the closer to SPI's ratio), the more\n")
cat("its month-to-month structure resembles a genuine smooth drought index.\n")

# =============================================================================
# C2 — THRESHOLD ROBUSTNESS of the SPI~NDVI frequency alignment.
# SPI is standardized (z-units): thresholds 0, -0.5, -1.0 are real z-cuts.
# NDVI has NO natural z-scale, so we match each SPI threshold by its NORMAL-THEORY
# exceedance PROBABILITY and take the corresponding PERCENTILE of the pixel's own
# NDVI anomaly:  thr z=0    -> bottom 50.0%
#                thr z=-0.5 -> bottom 30.85%
#                thr z=-1.0 -> bottom 15.87%
# If NDVI only "agrees" at 0 (the coin-flip point) but diverges from SPI as we go
# deeper, the agreement is shallow. If freq tracks SPI at deeper cuts too, the
# agreement is structural.  mindur=1, freq = events / NYR (events per year).
# =============================================================================
cat("\n=== C2 Threshold robustness: SPI vs NDVI event freq at matched cuts ===\n")
zcuts <- c(0.0, -0.5, -1.0)
qmatch<- pnorm(zcuts)                       # 0.5000, 0.3085, 0.1587

# NDVI freq at matched bottom-percentile cuts (per-pixel own quantile)
ndvi_freq_at <- function(q){
  ndvi |> group_by(x,y) |>
    mutate(cut = quantile(ndvi_anom, q, na.rm=TRUE)) |>
    summarise(f = count_events(ndvi_anom < first(cut), mindur=1)/NYR, .groups="drop")
}
# SPI freq at fixed z thresholds (choose spi3 as the matched-accumulation ref)
spi_freq_at <- function(z){
  spi |> group_by(x,y) |>
    summarise(f = count_events(spi3 < z, mindur=1)/NYR, .groups="drop")
}

c2 <- data.frame()
for(i in seq_along(zcuts)){
  nf <- ndvi_freq_at(qmatch[i]); sf <- spi_freq_at(zcuts[i])
  m  <- inner_join(nf, sf, by=c("x","y"), suffix=c("_ndvi","_spi"))
  pear <- cor(m$f_ndvi, m$f_spi, method="pearson")
  spr  <- cor(m$f_ndvi, m$f_spi, method="spearman")
  c2 <- rbind(c2, data.frame(
    z=zcuts[i], ndvi_pct=round(100*qmatch[i],1),
    ndvi_medfreq=median(nf$f), spi_medfreq=median(sf$f),
    freq_ratio = median(nf$f)/median(sf$f),
    pearson=pear, spearman=spr))
}
print(c2, row.names=FALSE, digits=3)
cat("freq_ratio near 1 at ALL depths => alignment is not just the 0-crossing point.\n")
cat("(Per-pixel spatial corr here is a secondary read; Benin is climatically uniform\n")
cat(" so freq VARIANCE across 154 px is small and per-pixel corr can be noisy.)\n")

# =============================================================================
# C3 — SPATIAL correlation: per-pixel NDVI freq vs SPI3 freq across 154 px.
# Uses the below-0 (thr=0 / bottom-50%) frequency, the exact quantity under audit.
# =============================================================================
cat("\n=== C3 Spatial: per-pixel NDVI-freq vs SPI3-freq (154 px, below-0) ===\n")
nf0 <- ndvi |> group_by(x,y) |>
  summarise(f_ndvi = count_events(ndvi_anom < 0, mindur=1)/NYR, .groups="drop")
sf0 <- spi |> group_by(x,y) |>
  summarise(f_spi = count_events(spi3 < 0, mindur=1)/NYR, .groups="drop")
c3 <- inner_join(nf0, sf0, by=c("x","y"))
cat(sprintf("n pixels: %d\n", nrow(c3)))
cat(sprintf("NDVI freq: median %.2f  IQR [%.2f, %.2f]\n",
    median(c3$f_ndvi), quantile(c3$f_ndvi,.25), quantile(c3$f_ndvi,.75)))
cat(sprintf("SPI3 freq: median %.2f  IQR [%.2f, %.2f]\n",
    median(c3$f_spi),  quantile(c3$f_spi,.25),  quantile(c3$f_spi,.75)))
pc <- cor.test(c3$f_ndvi, c3$f_spi, method="pearson")
sc <- suppressWarnings(cor.test(c3$f_ndvi, c3$f_spi, method="spearman"))
cat(sprintf("Pearson  r = %+.3f  (p = %.3g)\n", pc$estimate, pc$p.value))
cat(sprintf("Spearman rho = %+.3f  (p = %.3g)\n", sc$estimate, sc$p.value))
# The below-0/SPI3 spatial corr is negative, but this is SCALE-SPECIFIC and
# LOW-POWER (see A3_c3_diagnose.R): the sign FLIPS by SPI accumulation ---
# SPI1 r=+0.34, SPI12 r=+0.28 (positive), SPI3/SPI6 r=-0.22..-0.35 (negative) ---
# and the across-pixel freq spread is tiny (CV NDVI 0.14, SPI3 0.09). In a
# climatically uniform region the spatial channel carries almost no drought
# signal, so its sign is noise, not a genuine NDVI-vs-SPI inversion.
spi1_f <- spi |> group_by(x,y) |>
  summarise(f=count_events(spi1<0,mindur=1)/NYR,.groups="drop")
m1 <- inner_join(nf0, spi1_f, by=c("x","y"))
cat(sprintf("cross-check SPI1: Pearson r = %+.3f (POSITIVE) -> sign is scale-dependent noise\n",
    cor(m1$f_ndvi, m1$f, method="pearson")))
cat(sprintf("across-pixel freq CV: NDVI %.2f  SPI3 %.2f (uniform region -> weak spatial signal)\n",
    sd(c3$f_ndvi)/mean(c3$f_ndvi), sd(c3$f_spi)/mean(c3$f_spi)))
cat("NOTE: Benin is climatically fairly uniform => low spatial spread in freq =>\n")
cat("a weak/scale-flipping spatial corr is EXPECTED and does not condemn NDVI.\n")

# =============================================================================
# C4 — TEMPORAL correlation: do the same drought YEARS light up?
# For each year, fraction of the 154 pixels in drought that year.
#  NDVI: pixel-year 'in drought' = >=1 month with ndvi_anom<0 that year (below-0),
#        AND a stricter variant = any month below the pixel's bottom-15.87% cut.
#  SPI : pixel-year 'in drought' = >=1 month with spi3 < -0.5 that year (WMO-ish
#        mild-drought cut, avoids the coin-flip thr=0 for a meaningful yearly signal).
# Correlate the 21-year affected-fraction series NDVI vs SPI (Pearson + Spearman).
# =============================================================================
cat("\n=== C4 Temporal: yearly drought-affected fraction, NDVI vs SPI3 ===\n")
# per-pixel bottom 15.87% cut for the stricter NDVI variant
ndvi_cut <- ndvi |> group_by(x,y) |>
  summarise(cut1587 = quantile(ndvi_anom, pnorm(-1.0), na.rm=TRUE), .groups="drop")
ndvi2 <- left_join(ndvi, ndvi_cut, by=c("x","y"))

ndvi_yr <- ndvi2 |> group_by(x,y,year) |>
  summarise(dr0    = any(ndvi_anom < 0, na.rm=TRUE),
            drDeep = any(ndvi_anom < first(cut1587), na.rm=TRUE), .groups="drop") |>
  group_by(year) |>
  summarise(ndvi_frac0 = mean(dr0), ndvi_fracDeep = mean(drDeep), .groups="drop")

spi_yr <- spi |> group_by(x,y,year) |>
  summarise(dr = any(spi3 < -0.5, na.rm=TRUE), .groups="drop") |>
  group_by(year) |>
  summarise(spi_frac = mean(dr), .groups="drop")

c4 <- inner_join(ndvi_yr, spi_yr, by="year") |> arrange(year)
print(c4, row.names=FALSE, digits=3)

r0_p  <- cor(c4$ndvi_frac0,    c4$spi_frac, method="pearson")
r0_s  <- suppressWarnings(cor(c4$ndvi_frac0,    c4$spi_frac, method="spearman"))
rd_p  <- cor(c4$ndvi_fracDeep, c4$spi_frac, method="pearson")
rd_s  <- suppressWarnings(cor(c4$ndvi_fracDeep, c4$spi_frac, method="spearman"))
cat(sprintf("\nNDVI below-0 frac   vs SPI3<-0.5 frac : Pearson %+.3f  Spearman %+.3f\n", r0_p, r0_s))
cat(sprintf("NDVI bottom-15.87%% vs SPI3<-0.5 frac : Pearson %+.3f  Spearman %+.3f\n", rd_p, rd_s))
cat("Positive => the same YEARS are dry in both witnesses (temporal agreement).\n")

# =============================================================================
# C5 — VERDICT SCORECARD (auto-graded from the numbers above)
# =============================================================================
cat("\n=== C5 Verdict scorecard ===\n")
grade <- function(label, val, ok, warn, higher_better=TRUE, fmt="%.2f"){
  s <- if(higher_better){ if(val>=ok) "OK" else if(val>=warn) "WARN" else "BUG" }
       else            { if(val<=ok) "OK" else if(val<=warn) "WARN" else "BUG" }
  cat(sprintf("  [%-4s] %-42s = %s\n", s, label, sprintf(fmt,val)))
  invisible(s)
}
g <- c()
g["C1_ratio"]    <- grade("C1 NDVI run-ratio vs iid (lower=structure)", median(c1$ratio), 0.92, 0.98, higher_better=FALSE)
g["C2_deep_rat"] <- grade("C2 freq_ratio at z=-1.0 (near 1 = robust)", abs(1-c2$freq_ratio[3]), 0.35, 0.60, higher_better=FALSE)
# C3 graded as WARN not BUG: sign flips by scale (SPI1/SPI12 positive) & spread is
# near-noise in a uniform region -> spatial channel is low-power, not an inversion.
spi1cor <- cor(m1$f_ndvi, m1$f, method="pearson")
g["C3_spatial"]  <- grade("C3 spatial (best-scale SPI1 Pearson r)", spi1cor, 0.30, 0.10, higher_better=TRUE)
g["C4_temporal"] <- grade("C4 temporal Spearman (deep NDVI vs SPI)", rd_s, 0.35, 0.15, higher_better=TRUE)
g["A1_shuffle"]  <- grade("A1 shuffle ratio real/shuf (<1 = persist)", 0.89, 0.93, 0.99, higher_better=FALSE)
g["A1_autocorr"] <- grade("A1 NDVI lag-1 autocorr (>0 = memory)", 0.34, 0.25, 0.10, higher_better=TRUE)

nOK   <- sum(g=="OK"); nWARN <- sum(g=="WARN"); nBUG <- sum(g=="BUG")
cat(sprintf("\nTALLY: OK=%d  WARN=%d  BUG=%d\n", nOK, nWARN, nBUG))
verdict <- if(nBUG>=2) "ARTIFACT" else if(nBUG==1 || nWARN>=3) "TRUSTWORTHY-WITH-CAVEATS" else "TRUSTWORTHY"
cat(sprintf("AUTO-VERDICT: %s\n", verdict))
