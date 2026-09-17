# =============================================================================
# A1 — Can we trust the NDVI-anomaly drought frequency?
# Concern: NDVI is assumed noisy, yet its event frequency (anom<0, >=1mo) lands
# near SPI at threshold 0. Is that a real signal, or the trivial fact that any
# zero-mean series crosses 0 about half the time?
# Four checks (local, no Databricks):
#   S1 autocorrelation  — is NDVI-anom actually smooth or white noise?
#   S2 shuffle null      — does REAL NDVI beat time-shuffled NDVI? (the key test)
#   S3 threshold scan    — does alignment hold below 0, or only AT 0?
#   S4 distribution      — is NDVI-anom symmetric around 0?
# Compares against SPI-3 as the reference smooth index.
# =============================================================================
source("Codes/nostd_freq/00_event_rule.R")
setwd(Q3$wd)

ndvi <- as.data.frame(read_parquet("Codes/_archive/_data/ndvi_anom_benin_monthly.parquet")) |>
  filter(year>=Q3$Y0, year<=Q3$Y1) |> arrange(x,y,year,month)
spi  <- as.data.frame(read_parquet("Codes/_archive/_data/spi_results.parquet")) |>
  filter(year>=Q3$Y0, year<=Q3$Y1) |> arrange(x,y,year,month) |> transmute(x,y,year,month,v=spi3)

# ---- S1: lag-1 autocorrelation per pixel ------------------------------------
ac1 <- function(v){ v<-v[!is.na(v)]; if(length(v)<12) return(NA); cor(v[-1], v[-length(v)]) }
s1_ndvi <- ndvi |> group_by(x,y) |> summarise(ac=ac1(ndvi_anom), .groups="drop")
s1_spi  <- spi  |> group_by(x,y) |> summarise(ac=ac1(v), .groups="drop")
cat("=== S1 lag-1 autocorrelation (1 = smooth, 0 = white noise) ===\n")
cat(sprintf("NDVI-anom: median %.2f   SPI-3: median %.2f\n",
            median(s1_ndvi$ac,na.rm=TRUE), median(s1_spi$ac,na.rm=TRUE)))
cat("(NDVI well above 0 = it is NOT white noise; carries real month-to-month memory.)\n")

# ---- S2: shuffle null with PERSISTENCE rule (mindur=2). ---------------------
# At mindur=1 the event count = number of below-0 stretches, ~driven by the
# zero-crossing RATE, which shuffling barely changes -> uninformative. To test
# temporal STRUCTURE we require >=2 consecutive months. Real persistent drought
# has FEWER but LONGER runs than a shuffled (memoryless) version, so real events
# < shuffled events. Randomise per pixel with a proper seeded permutation.
lcg <- function(n, seed){ x<-seed; out<-numeric(n)
  for(i in 1:n){ x<-(1103515245*x + 12345) %% 2147483648; out[i]<-x }; out }
real_ev <- ndvi |> group_by(x,y) |>
  summarise(e=count_events(ndvi_anom < 0, mindur=2), .groups="drop")
one_shuffle <- function(seed){
  ndvi |> group_by(x,y) |>
    mutate(perm=order(lcg(n(), seed + as.integer(first(x)*1000)))) |>
    summarise(e=count_events((ndvi_anom[perm]) < 0, mindur=2), .groups="drop")
}
shuf <- bind_rows(lapply(1:10, one_shuffle)) |> group_by(x,y) |>
  summarise(e_shuf=mean(e), .groups="drop")
s2 <- real_ev |> left_join(shuf, by=c("x","y"))
cat("\n=== S2 shuffle null, PERSISTENT events (>=2mo): real vs shuffled ===\n")
cat(sprintf("REAL events/pixel:     median %.2f\n", median(s2$e)))
cat(sprintf("SHUFFLED events/pixel: median %.2f\n", median(s2$e_shuf)))
cat(sprintf("ratio real/shuffled:   %.2f\n", median(s2$e)/median(s2$e_shuf)))
cat("(<1 = real has fewer/longer runs = genuine persistence; ~1 = memoryless, no structure.)\n")

# ---- S3: threshold scan. NDVI freq at anom quantile cuts (no z-score) --------
# Match SPI thresholds by PERCENTILE of the pixel's own anomaly, so we can ask:
# does NDVI still behave sensibly below 0, or only at 0?
cat("\n=== S3 NDVI event freq at deeper cuts (percentile of own anomaly) ===\n")
for(q in c(0.50,0.30,0.15,0.075)){   # 0.50 ~ below-0-ish; deeper = stricter
  fq <- ndvi |> group_by(x,y) |>
    mutate(cut=quantile(ndvi_anom, q, na.rm=TRUE)) |>
    summarise(f=count_events(ndvi_anom < first(cut), mindur=1)/Q3$NYR, .groups="drop")
  cat(sprintf("  bottom %4.1f%% : median freq %.2f\n", q*100, median(fq$f)))
}
cat("(Monotone DROP as cut deepens = sensible drought behaviour, not a 0-crossing fluke.)\n")

# ---- S4: distribution shape -------------------------------------------------
va <- ndvi$ndvi_anom[!is.na(ndvi$ndvi_anom)]
cat("\n=== S4 NDVI-anomaly distribution ===\n")
cat(sprintf("mean %.3f  median %.3f  sd %.3f  skew %.2f  %%below0 %.1f%%\n",
    mean(va), median(va), sd(va),
    mean((va-mean(va))^3)/sd(va)^3, 100*mean(va<0)))
cat("(mean~0 & ~50%% below 0 = symmetric; 'below 0' alone is a coin-flip, which is\n why event PERSISTENCE / min-duration, not the level, is what makes it drought.)\n")
