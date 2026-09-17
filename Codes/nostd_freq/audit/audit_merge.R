suppressMessages({library(arrow); library(dplyr)})
setwd("c:/Users/tajayalath/Box/R - Projects - Tharaka/Drought-indices")
Y0<-2003;Y1<-2023;NYR<-21

count_onsets <- function(cond){
  cond[is.na(cond)] <- FALSE; n<-length(cond); if(n==0) return(0L)
  sum(cond & !c(FALSE, cond[-n]))
}

# round coords to avoid float merge mismatch
ndvi <- as.data.frame(read_parquet("Codes/_archive/_data/ndvi_anom_benin_monthly.parquet"))
ndvi <- ndvi[ndvi$year>=Y0 & ndvi$year<=Y1,]
ndvi$px <- paste(round(ndvi$x,4), round(ndvi$y,4))
ndvi <- ndvi[order(ndvi$px, ndvi$year, ndvi$month),]
mine <- tapply(seq_len(nrow(ndvi)), ndvi$px, function(i) count_onsets(ndvi$ndvi_anom[i] < 0))
mine <- data.frame(px=names(mine), events_mine=as.integer(mine))

ex <- as.data.frame(read_parquet("Codes/nostd_freq/out/freq_ndvi.parquet"))
ex$px <- paste(round(ex$x,4), round(ex$y,4))
cmp <- merge(mine, ex[,c("px","events","freq")], by="px")
cat("NDVI merged pixels:", nrow(cmp), "\n")
cat("pixels where MY event count != theirs:", sum(cmp$events_mine != cmp$events), "\n")
cat("max abs event diff:", max(abs(cmp$events_mine - cmp$events)), "\n")
cat("all 154 present:", nrow(cmp)==154, "\n")
if(any(cmp$events_mine!=cmp$events)) print(cmp[cmp$events_mine!=cmp$events,])
