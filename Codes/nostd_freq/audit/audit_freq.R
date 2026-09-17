# =============================================================================
# INDEPENDENT cross-check of NDVI (and PDSI, SPI) drought event frequency.
# Does NOT use count_events()/rle. Uses a transition/diff-based onset counter.
# =============================================================================
suppressMessages({library(arrow); library(dplyr)})
setwd("c:/Users/tajayalath/Box/R - Projects - Tharaka/Drought-indices")

Y0 <- 2003; Y1 <- 2023; NYR <- Y1 - Y0 + 1

# --- INDEPENDENT onset counter -----------------------------------------------
# An "event" = a maximal run where cond is TRUE. Count onsets: positions where
# cond is TRUE AND the previous position is not TRUE (NA or FALSE or start).
# This is a diff/transition method, entirely different from rle().
count_onsets <- function(cond){
  cond[is.na(cond)] <- FALSE
  n <- length(cond)
  if(n == 0) return(0L)
  prev <- c(FALSE, cond[-n])          # cond shifted forward by 1 (start = FALSE)
  sum(cond & !prev)                   # rising edges = number of runs
}

# Second independent method: cumulative-sign-change via diff of an indicator.
# onset happens where diff(indicator) == +1 (0->1). Plus 1 if series starts at 1.
count_onsets_diff <- function(cond){
  cond[is.na(cond)] <- FALSE
  ind <- as.integer(cond)
  if(length(ind)==0) return(0L)
  d <- diff(ind)
  sum(d == 1L) + (ind[1] == 1L)
}

per_pixel <- function(df, condfun){
  df <- df[order(df$x, df$y, df$year, df$month), ]
  ux <- interaction(df$x, df$y, drop=TRUE)
  out <- tapply(seq_len(nrow(df)), ux, function(idx){
    cond <- condfun(df[idx, , drop=FALSE])
    c(onsets = count_onsets(cond), onsets_diff = count_onsets_diff(cond))
  })
  key <- do.call(rbind, lapply(strsplit(names(out), "\\."), function(p) as.numeric(p)))
  res <- data.frame(x=key[,1], y=key[,2],
                    events_mine     = sapply(out, `[`, "onsets"),
                    events_mine_diff= sapply(out, `[`, "onsets_diff"))
  rownames(res) <- NULL
  res
}

qsum <- function(v) {
  q <- quantile(v, c(0,.25,.5,.75,1))
  sprintf("min=%.4f Q1=%.4f median=%.4f Q3=%.4f max=%.4f mean=%.4f n=%d",
          q[1],q[2],q[3],q[4],q[5], mean(v), length(v))
}

cat("################ NDVI ################\n")
ndvi <- as.data.frame(read_parquet("Codes/_archive/_data/ndvi_anom_benin_monthly.parquet"))
ndvi <- ndvi[ndvi$year>=Y0 & ndvi$year<=Y1, ]
cat("rows after filter:", nrow(ndvi), " pixels:", nrow(unique(ndvi[,c("x","y")])), "\n")

# fraction of below-zero months overall (sanity #4)
frac_neg <- mean(ndvi$ndvi_anom < 0, na.rm=TRUE)
cat(sprintf("overall fraction ndvi_anom<0 = %.4f (%.1f%%)\n", frac_neg, 100*frac_neg))
months_per_px <- nrow(ndvi)/nrow(unique(ndvi[,c("x","y")]))
cat("mean months per pixel:", months_per_px, "\n")

mine <- per_pixel(ndvi, function(d) d$ndvi_anom < 0)
mine$freq_mine <- mine$events_mine / NYR

# consistency of two independent methods (sanity #3)
cat("\n-- my two independent counters agree? --\n")
cat("max abs diff (onsets vs onsets_diff):", max(abs(mine$events_mine - mine$events_mine_diff)), "\n")

cat("\n-- MY per-pixel freq distribution --\n")
cat(qsum(mine$freq_mine), "\n")

# compare to existing output
ex <- as.data.frame(read_parquet("Codes/nostd_freq/out/freq_ndvi.parquet"))
cat("\n-- EXISTING freq_ndvi.parquet distribution --\n")
cat(qsum(ex$freq), "\n")

cmp <- merge(mine[,c("x","y","events_mine","freq_mine")],
             ex[,c("x","y","events","freq")], by=c("x","y"))
cmp$dev <- cmp$events_mine - cmp$events
cat("\n-- pixel-by-pixel compare (n merged =", nrow(cmp), ") --\n")
cat("pixels where event count differs:", sum(cmp$dev != 0), "\n")
cat("max abs freq diff:", max(abs(cmp$freq_mine - cmp$freq)), "\n")
if(any(cmp$dev != 0)) print(head(cmp[cmp$dev!=0,], 20))

# sanity arithmetic #4
cat("\n-- SANITY ARITHMETIC (NDVI) --\n")
neg_months_per_px <- mean(tapply(ndvi$ndvi_anom < 0, interaction(ndvi$x,ndvi$y,drop=TRUE), sum, na.rm=TRUE))
cat(sprintf("mean below-0 months per pixel: %.1f over %.0f months\n", neg_months_per_px, months_per_px))
cat(sprintf("mean events per pixel (mine): %.2f -> mean run length = %.2f months\n",
            mean(mine$events_mine), neg_months_per_px/mean(mine$events_mine)))
cat(sprintf("mean freq/yr = %.3f ; median freq/yr = %.3f\n",
            mean(mine$freq_mine), median(mine$freq_mine)))

cat("\n\n################ PDSI ################\n")
pdsi <- as.data.frame(read_parquet("Codes/pdsi_validation/out/pdsi_benin_monthly.parquet"))
pdsi <- pdsi[pdsi$year>=Y0 & pdsi$year<=Y1, ]
minep <- per_pixel(pdsi, function(d) d$pdsi <= -2)
minep$freq_mine <- minep$events_mine / NYR
exp <- as.data.frame(read_parquet("Codes/nostd_freq/out/freq_pdsi.parquet"))
cat("MY  :", qsum(minep$freq_mine), "\n")
cat("THEM:", qsum(exp$freq), "\n")
cmpp <- merge(minep[,c("x","y","events_mine")], exp[,c("x","y","events","freq")], by=c("x","y"))
cat("pixels differing:", sum(cmpp$events_mine != cmpp$events),
    " | method agree:", max(abs(minep$events_mine - minep$events_mine_diff))==0, "\n")

cat("\n\n################ SPI (spi3) ################\n")
spi <- as.data.frame(read_parquet("Codes/_archive/_data/spi_results.parquet"))
spi <- spi[spi$year>=Y0 & spi$year<=Y1, ]
mines <- per_pixel(spi, function(d) d$spi3 < 0)
mines$freq_mine <- mines$events_mine / NYR
exs <- as.data.frame(read_parquet("Codes/nostd_freq/out/freq_spi_spei.parquet"))
exs3 <- exs[exs$witness=="SPI" & exs$scale==3, ]
cat("MY spi3 :", qsum(mines$freq_mine), "\n")
cat("THEM spi3:", qsum(exs3$freq), "\n")
cmps <- merge(mines[,c("x","y","events_mine")], exs3[,c("x","y","events","freq")], by=c("x","y"))
cat("pixels differing:", sum(cmps$events_mine != cmps$events), "\n")
cat("NOTE: script 01 uses count_events default mindur=2 for SPI (rule 'index<0' but >=2mo);\n")
cat("      my counter above uses >=1mo. Recompute with >=2 to match their rule:\n")

# match their >=2 rule independently: onset only if run length>=2
count_onsets_min2 <- function(cond){
  cond[is.na(cond)] <- FALSE
  n <- length(cond); if(n==0) return(0L)
  r <- cumsum(!cond)                  # group id changes only across FALSE gaps... use manual runs
  # manual run-length without rle:
  starts <- which(cond & c(TRUE, !cond[-n]))
  ends   <- which(cond & c(!cond[-1], TRUE))
  sum((ends - starts + 1) >= 2)
}
mines2 <- tapply(seq_len(nrow(spi)), interaction(spi$x,spi$y,drop=TRUE), function(idx){
  d <- spi[idx,]; d <- d[order(d$year,d$month),]; count_onsets_min2(d$spi3 < 0)
})
key <- do.call(rbind, lapply(strsplit(names(mines2),"\\."), as.numeric))
m2 <- data.frame(x=key[,1], y=key[,2], events_mine2=as.integer(mines2))
cmps2 <- merge(m2, exs3[,c("x","y","events")], by=c("x","y"))
cat("MY spi3 (>=2mo) median freq:", median(m2$events_mine2/NYR),
    " | pixels differing vs THEM:", sum(cmps2$events_mine2 != cmps2$events), "\n")

cat("\nDONE\n")
