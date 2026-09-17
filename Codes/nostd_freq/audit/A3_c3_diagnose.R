# =============================================================================
# A3 — Diagnose the NEGATIVE spatial correlation found in A2/C3.
# C3 gave per-pixel NDVI-freq vs SPI3-freq r = -0.23 (sig). Is that a real
# inversion, a scale artifact, or just spread-limited noise in a uniform region?
# Checks:
#   D1 does the sign persist across SPI1/3/6/12 and across mindur?
#   D2 is it driven by the SPI 'p(below0)' baseline differing by pixel?
#   D3 how big is the freq spread relative to noise (coef of variation)?
#   D4 rank-agreement on the DEEP cut (bottom-15.87% NDVI vs SPI3<-1) — the
#      scientifically meaningful drought signal, not the coin-flip level.
# =============================================================================
source("Codes/nostd_freq/00_event_rule.R")
setwd(Q3$wd)

ndvi <- as.data.frame(read_parquet("Codes/_archive/_data/ndvi_anom_benin_monthly.parquet")) |>
  filter(year>=Q3$Y0, year<=Q3$Y1) |> arrange(x,y,year,month)
spi  <- as.data.frame(read_parquet("Codes/_archive/_data/spi_results.parquet")) |>
  filter(year>=Q3$Y0, year<=Q3$Y1) |> arrange(x,y,year,month)
NYR <- Q3$NYR

nf0 <- ndvi |> group_by(x,y) |>
  summarise(f_ndvi = count_events(ndvi_anom < 0, mindur=1)/NYR, .groups="drop")

cat("=== D1: NDVI below-0 freq vs SPI<0 freq, all scales & mindur ===\n")
for(sc in c("spi1","spi3","spi6","spi12")){
  for(md in c(1,2)){
    sf <- spi |> group_by(x,y) |>
      summarise(f = count_events(.data[[sc]] < 0, mindur=md)/NYR, .groups="drop")
    m <- inner_join(nf0, sf, by=c("x","y"))
    cat(sprintf("  %-5s mindur=%d : Pearson %+.3f  Spearman %+.3f\n",
        sc, md, cor(m$f_ndvi,m$f,method="pearson"),
        suppressWarnings(cor(m$f_ndvi,m$f,method="spearman"))))
  }
}

cat("\n=== D3: freq spread across the 154 px (uniform region => small spread) ===\n")
cv <- function(v) sd(v)/mean(v)
sf3 <- spi |> group_by(x,y) |> summarise(f=count_events(spi3<0,mindur=1)/NYR,.groups="drop")
cat(sprintf("NDVI freq: mean %.2f sd %.3f CV %.3f range [%.2f,%.2f]\n",
    mean(nf0$f_ndvi),sd(nf0$f_ndvi),cv(nf0$f_ndvi),min(nf0$f_ndvi),max(nf0$f_ndvi)))
cat(sprintf("SPI3 freq: mean %.2f sd %.3f CV %.3f range [%.2f,%.2f]\n",
    mean(sf3$f),mean(sf3$f)*0+sd(sf3$f),cv(sf3$f),min(sf3$f),max(sf3$f)))

cat("\n=== D4: DEEP cut rank-agreement (the meaningful drought signal) ===\n")
# NDVI bottom-15.87% freq  vs  SPI3<-1.0 freq
ndeep <- ndvi |> group_by(x,y) |>
  mutate(cut=quantile(ndvi_anom, pnorm(-1), na.rm=TRUE)) |>
  summarise(f_ndvi=count_events(ndvi_anom<first(cut),mindur=1)/NYR,.groups="drop")
sdeep <- spi |> group_by(x,y) |>
  summarise(f_spi=count_events(spi3< -1.0,mindur=1)/NYR,.groups="drop")
md <- inner_join(ndeep,sdeep,by=c("x","y"))
cat(sprintf("bottom-15.87%% NDVI vs SPI3<-1 : Pearson %+.3f  Spearman %+.3f\n",
    cor(md$f_ndvi,md$f_spi,method="pearson"),
    suppressWarnings(cor(md$f_ndvi,md$f_spi,method="spearman"))))
