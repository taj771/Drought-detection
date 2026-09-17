# =============================================================================
# P2b — Calibration-window experiment for sc-PDSI (does NOT overwrite P2 output).
# Compute PDSI three ways and compare the Observation-period behaviour:
#   (a) 1973-2002  (current, = SPI/SPEI "reference" period)
#   (b) 1950-2023  (full record, = what SPI/SPEI actually calibrate on)
#   (c) 1991-2020  (WMO 30-yr climate normal)
# Question: does aligning the calibration make Obs less "permanently in drought"
# and the event frequency more meaningful? Writes each to a separate parquet so
# 03_pdsi_freq can be re-pointed if we pick one.
# =============================================================================
suppressMessages({ library(arrow); library(dplyr); library(scPDSI) })
setwd("c:/Users/tajayalath/Box/R - Projects - Tharaka/Drought-indices")
DAT <- "Codes/_archive/_data"; OUT <- "Codes/pdsi_validation/out"
AWC <- 100; Y0 <- 1950; Y1 <- 2023
OBS0 <- 2003; OBS1 <- 2023

pr <- as.data.frame(read_parquet(file.path(DAT,"spei_penman_results.parquet"))) |>
  select(x,y,year,month,monthly_precip)
pet <- as.data.frame(read_parquet("Data/cru_pet/cru_pet_benin_monthly.parquet")) |>
  select(x,y,year,month,pet_mm)
d <- inner_join(pr,pet,by=c("x","y","year","month")) |>
  filter(year>=Y0,year<=Y1) |> arrange(x,y,year,month)
pix <- distinct(d,x,y)

run_cal <- function(cal0, cal1){
  res <- vector("list", nrow(pix))
  for(i in seq_len(nrow(pix))){
    s <- d |> filter(x==pix$x[i], y==pix$y[i]) |> arrange(year,month)
    out <- tryCatch(pdsi(P=s$monthly_precip, PE=s$pet_mm, AWC=AWC,
                         start=min(s$year), end=max(s$year),
                         cal_start=cal0, cal_end=cal1, sc=TRUE),
                    error=function(e) NULL)
    if(is.null(out)) next
    s$pdsi <- as.numeric(out$X)[seq_len(nrow(s))]
    res[[i]] <- s[,c("x","y","year","month","pdsi")]
  }
  bind_rows(res)
}

# count events: run of pdsi<=-2 >=1mo, per pixel, Obs window -> median freq
event_freq <- function(df){
  df |> filter(year>=OBS0, year<=OBS1) |> arrange(x,y,year,month) |>
    group_by(x,y) |>
    summarise(e=sum({d<-pdsi<=-2 & !is.na(pdsi); r<-rle(d); r$values}), .groups="drop") |>
    summarise(medfreq=median(e)/(OBS1-OBS0+1)) |> pull(medfreq)
}

cals <- list(c(1973,2002,"1973-2002 (current/Ref)"),
             c(1950,2023,"1950-2023 (full, match SPI)"),
             c(1991,2020,"1991-2020 (WMO normal)"))
cat("=== sc-PDSI calibration comparison (Obs 2003-2023) ===\n\n")
for(cc in cals){
  p <- run_cal(as.integer(cc[1]), as.integer(cc[2]))
  ob <- p |> filter(year>=OBS0, year<=OBS1)
  cat(sprintf("%-28s | Obs mean %6.2f | %% below0 %3.0f%% | %% <=-2 %3.0f%% | med freq %.2f\n",
      cc[3], mean(ob$pdsi,na.rm=T),
      100*mean(ob$pdsi<0,na.rm=T), 100*mean(ob$pdsi<=-2,na.rm=T),
      event_freq(p)))
  # save the full-period one for possible use
  if(cc[1]=="1950") write_parquet(p, file.path(OUT,"pdsi_benin_monthly_fullcal.parquet"))
}
cat("\n(Current P2 output pdsi_benin_monthly.parquet unchanged.)\n")
