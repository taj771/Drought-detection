# =============================================================================
# A2 — Stress-test: is SOIL-MOISTURE-anomaly drought FREQUENCY a real signal or
#      a zero-crossing artifact? Extends A1 with 5 harder checks.
#
#   C1 Zero-crossing baseline — analytical iid expectation vs SoilM actual runs.
#   C2 Threshold robustness    — SPI~SoilM freq agreement at 0, -0.5, -1.0?
#   C3 Spatial correlation     — per-pixel SoilM-freq vs SPI3-freq (Pearson+Spearman).
#   C4 Temporal correlation     — yearly drought-affected fraction, SoilM vs SPI.
#   C5 Verdict                 — trustworthy / with-caveats / artifact.
#
# Same window & event rule as A1 (source 00_event_rule.R).
# Run: & "C:\Program Files\R\R-4.6.1\bin\Rscript.exe" Codes/sm_validation/audit/A2_sm_stress.R
# =============================================================================
source("Codes/nostd_freq/00_event_rule.R")
setwd(Q3$wd)
set.seed(20260722)

sm  <- as.data.frame(read_parquet("Codes/_archive/_data/sm_benin_monthly.parquet")) |>
  filter(year>=Q3$Y0, year<=Q3$Y1) |> arrange(x,y,year,month)
spi <- as.data.frame(read_parquet("Codes/_archive/_data/spi_results.parquet")) |>
  filter(year>=Q3$Y0, year<=Q3$Y1) |> arrange(x,y,year,month) |>
  transmute(x,y,year,month,spi1,spi3,spi6,spi12)

NYR <- Q3$NYR

# =============================================================================
# C1 — ZERO-CROSSING BASELINE (analytical vs actual)
# For a MEMORYLESS symmetric series, each month is independently below 0 with
# probability p (~0.5). Below-0 RUNS (mindur=1) for iid Bernoulli(p) over N months:
#   E[runs] = p + (N-1) * p * (1-p)
# Positive autocorrelation makes runs FEWER and LONGER, so ACTUAL < iid-EXPECTED
# is the fingerprint of real persistence, not a coin-flip zero-crossing pattern.
# =============================================================================
cat("=== C1 Zero-crossing baseline: iid expectation vs actual SoilM runs ===\n")
c1 <- sm |> group_by(x,y) |>
  summarise(N=sum(!is.na(sm_anom)), p=mean(sm_anom<0,na.rm=TRUE),
            actual=count_events(sm_anom<0,mindur=1), .groups="drop") |>
  mutate(iid_expected=p+(N-1)*p*(1-p), ratio=actual/iid_expected)
c1s <- spi |> group_by(x,y) |>
  summarise(N=sum(!is.na(spi3)), p=mean(spi3<0,na.rm=TRUE),
            actual=count_events(spi3<0,mindur=1), .groups="drop") |>
  mutate(iid_expected=p+(N-1)*p*(1-p), ratio=actual/iid_expected)
cat(sprintf("SoilM: median N=%.0f  p(below0)=%.3f  iid-expected runs=%.1f  actual runs=%.1f  ratio=%.2f\n",
    median(c1$N), median(c1$p), median(c1$iid_expected), median(c1$actual), median(c1$ratio)))
cat(sprintf("SPI3 : median N=%.0f  p(below0)=%.3f  iid-expected runs=%.1f  actual runs=%.1f  ratio=%.2f\n",
    median(c1s$N), median(c1s$p), median(c1s$iid_expected), median(c1s$actual), median(c1s$ratio)))
cat("Interpretation: ratio<1 => real series has FEWER,LONGER runs than a memoryless\n")
cat("coin-flip of the SAME below-0 fraction => the frequency is NOT a zero-crossing fluke.\n")

# =============================================================================
# C2 — THRESHOLD ROBUSTNESS of the SPI~SoilM frequency alignment.
# SPI z-cuts 0,-0.5,-1.0 matched to SoilM by NORMAL-THEORY exceedance PROBABILITY
# taken as the PERCENTILE of the pixel's own anomaly.
# =============================================================================
cat("\n=== C2 Threshold robustness: SPI vs SoilM event freq at matched cuts ===\n")
zcuts <- c(0.0, -0.5, -1.0)
qmatch<- pnorm(zcuts)
sm_freq_at <- function(q){
  sm |> group_by(x,y) |>
    mutate(cut=quantile(sm_anom,q,na.rm=TRUE)) |>
    summarise(f=count_events(sm_anom < first(cut), mindur=1)/NYR, .groups="drop")
}
spi_freq_at <- function(z){
  spi |> group_by(x,y) |>
    summarise(f=count_events(spi3 < z, mindur=1)/NYR, .groups="drop")
}
c2 <- data.frame()
for(i in seq_along(zcuts)){
  nf <- sm_freq_at(qmatch[i]); sf <- spi_freq_at(zcuts[i])
  m  <- inner_join(nf, sf, by=c("x","y"), suffix=c("_sm","_spi"))
  c2 <- rbind(c2, data.frame(z=zcuts[i], sm_pct=round(100*qmatch[i],1),
    sm_medfreq=median(nf$f), spi_medfreq=median(sf$f),
    freq_ratio=median(nf$f)/median(sf$f),
    pearson=cor(m$f_sm,m$f_spi,method="pearson"),
    spearman=cor(m$f_sm,m$f_spi,method="spearman")))
}
print(c2, row.names=FALSE, digits=3)
cat("freq_ratio near 1 at ALL depths => alignment is not just the 0-crossing point.\n")

# =============================================================================
# C3 — SPATIAL correlation: per-pixel SoilM freq vs SPI3 freq across 154 px (below-0).
# =============================================================================
cat("\n=== C3 Spatial: per-pixel SoilM-freq vs SPI3-freq (154 px, below-0) ===\n")
nf0 <- sm  |> group_by(x,y) |>
  summarise(f_sm=count_events(sm_anom<0,mindur=1)/NYR, .groups="drop")
sf0 <- spi |> group_by(x,y) |>
  summarise(f_spi=count_events(spi3<0,mindur=1)/NYR, .groups="drop")
c3 <- inner_join(nf0, sf0, by=c("x","y"))
cat(sprintf("n pixels: %d\n", nrow(c3)))
cat(sprintf("SoilM freq: median %.2f  IQR [%.2f, %.2f]\n",
    median(c3$f_sm), quantile(c3$f_sm,.25), quantile(c3$f_sm,.75)))
cat(sprintf("SPI3 freq:  median %.2f  IQR [%.2f, %.2f]\n",
    median(c3$f_spi), quantile(c3$f_spi,.25), quantile(c3$f_spi,.75)))
pc <- cor.test(c3$f_sm, c3$f_spi, method="pearson")
sc <- suppressWarnings(cor.test(c3$f_sm, c3$f_spi, method="spearman"))
cat(sprintf("Pearson  r = %+.3f  (p = %.3g)\n", pc$estimate, pc$p.value))
cat(sprintf("Spearman rho = %+.3f  (p = %.3g)\n", sc$estimate, sc$p.value))
spi1_f <- spi |> group_by(x,y) |>
  summarise(f=count_events(spi1<0,mindur=1)/NYR,.groups="drop")
m1 <- inner_join(nf0, spi1_f, by=c("x","y"))
cat(sprintf("cross-check SPI1: Pearson r = %+.3f\n", cor(m1$f_sm,m1$f,method="pearson")))
cat(sprintf("across-pixel freq CV: SoilM %.2f  SPI3 %.2f (uniform region -> weak spatial signal)\n",
    sd(c3$f_sm)/mean(c3$f_sm), sd(c3$f_spi)/mean(c3$f_spi)))
cat("NOTE: Benin climatically fairly uniform => weak/scale-flipping spatial corr EXPECTED.\n")

# =============================================================================
# C4 — TEMPORAL correlation: do the same drought YEARS light up?
# SoilM: pixel-year 'in drought' = any month sm_anom<0 (below-0), and stricter
#        variant = any month below the pixel's bottom-15.87% cut.
# SPI : pixel-year 'in drought' = any month spi3 < -0.5 (WMO-ish mild cut).
# Correlate the 21-year affected-fraction series (Pearson + Spearman).
# =============================================================================
cat("\n=== C4 Temporal: yearly drought-affected fraction, SoilM vs SPI3 ===\n")
sm_cut <- sm |> group_by(x,y) |>
  summarise(cut1587=quantile(sm_anom, pnorm(-1.0), na.rm=TRUE), .groups="drop")
sm2 <- left_join(sm, sm_cut, by=c("x","y"))
sm_yr <- sm2 |> group_by(x,y,year) |>
  summarise(dr0=any(sm_anom<0,na.rm=TRUE),
            drDeep=any(sm_anom < first(cut1587),na.rm=TRUE), .groups="drop") |>
  group_by(year) |>
  summarise(sm_frac0=mean(dr0), sm_fracDeep=mean(drDeep), .groups="drop")
spi_yr <- spi |> group_by(x,y,year) |>
  summarise(dr=any(spi3 < -0.5,na.rm=TRUE), .groups="drop") |>
  group_by(year) |> summarise(spi_frac=mean(dr), .groups="drop")
c4 <- inner_join(sm_yr, spi_yr, by="year") |> arrange(year)
print(c4, row.names=FALSE, digits=3)
r0_p <- cor(c4$sm_frac0,    c4$spi_frac, method="pearson")
r0_s <- suppressWarnings(cor(c4$sm_frac0,    c4$spi_frac, method="spearman"))
rd_p <- cor(c4$sm_fracDeep, c4$spi_frac, method="pearson")
rd_s <- suppressWarnings(cor(c4$sm_fracDeep, c4$spi_frac, method="spearman"))
cat(sprintf("\nSoilM below-0 frac   vs SPI3<-0.5 frac : Pearson %+.3f  Spearman %+.3f\n", r0_p, r0_s))
cat(sprintf("SoilM bottom-15.87%% vs SPI3<-0.5 frac : Pearson %+.3f  Spearman %+.3f\n", rd_p, rd_s))
cat("(NDVI ref: temporal r up to 0.60.) Positive => same YEARS dry in both witnesses.\n")

# =============================================================================
# C5 — VERDICT SCORECARD (auto-graded)
# =============================================================================
cat("\n=== C5 Verdict scorecard ===\n")
grade <- function(label, val, ok, warn, higher_better=TRUE, fmt="%.2f"){
  s <- if(higher_better){ if(val>=ok) "OK" else if(val>=warn) "WARN" else "BUG" }
       else            { if(val<=ok) "OK" else if(val<=warn) "WARN" else "BUG" }
  cat(sprintf("  [%-4s] %-44s = %s\n", s, label, sprintf(fmt,val)))
  invisible(s)
}
# recompute A1 autocorr + shuffle here so scorecard is self-contained
ac1 <- function(v){ v<-v[!is.na(v)]; if(length(v)<12) return(NA); cor(v[-1], v[-length(v)]) }
sm_ac <- median((sm |> group_by(x,y) |> summarise(ac=ac1(sm_anom),.groups="drop"))$ac, na.rm=TRUE)
lcg <- function(n, seed){ x<-seed; out<-numeric(n)
  for(i in 1:n){ x<-(1103515245*x + 12345) %% 2147483648; out[i]<-x }; out }
real_ev <- sm |> group_by(x,y) |> summarise(e=count_events(sm_anom<0,mindur=2),.groups="drop")
one_shuffle <- function(seed){ sm |> group_by(x,y) |>
    mutate(perm=order(lcg(n(), seed+as.integer(first(x)*1000)))) |>
    summarise(e=count_events((sm_anom[perm])<0,mindur=2),.groups="drop") }
shuf <- bind_rows(lapply(1:10, one_shuffle)) |> group_by(x,y) |> summarise(e_shuf=mean(e),.groups="drop")
s2r  <- median((real_ev|>left_join(shuf,by=c("x","y")))$e) / median(shuf$e_shuf)

g <- c()
g["C1_ratio"]    <- grade("C1 SoilM run-ratio vs iid (lower=structure)", median(c1$ratio), 0.92, 0.98, higher_better=FALSE)
g["C2_deep_rat"] <- grade("C2 |1-freq_ratio| at z=-1.0 (near 0 = robust)", abs(1-c2$freq_ratio[3]), 0.35, 0.60, higher_better=FALSE)
g["C3_spatial"]  <- grade("C3 spatial (best-scale SPI1 Pearson r)", cor(m1$f_sm,m1$f,method="pearson"), 0.30, 0.10, higher_better=TRUE)
g["C4_temporal"] <- grade("C4 temporal Spearman (deep SoilM vs SPI)", rd_s, 0.35, 0.15, higher_better=TRUE)
g["A1_shuffle"]  <- grade("A1 shuffle ratio real/shuf (<1 = persist)", s2r, 0.93, 0.99, higher_better=FALSE)
g["A1_autocorr"] <- grade("A1 SoilM lag-1 autocorr (>0 = memory)", sm_ac, 0.25, 0.10, higher_better=TRUE)

nOK <- sum(g=="OK"); nWARN <- sum(g=="WARN"); nBUG <- sum(g=="BUG")
cat(sprintf("\nTALLY: OK=%d  WARN=%d  BUG=%d\n", nOK, nWARN, nBUG))
verdict <- if(nBUG>=2) "ARTIFACT" else if(nBUG==1 || nWARN>=3) "TRUSTWORTHY-WITH-CAVEATS" else "TRUSTWORTHY"
cat(sprintf("AUTO-VERDICT: %s\n", verdict))
