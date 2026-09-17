# =============================================================================
# 03b — Soil-moisture drought event frequency (ERA5-Land root-zone, NOT standardised).
# Third quality-check witness alongside NDVI (02) and PDSI (03).
# Rule: sm_anom < 0, >= 1 month = one event. SAME rule as every witness
# (Q3$MINDUR=1) and the characteristics-deck pipeline. Per pixel. Window 2003-2023.
# sm_anom = ERA5-Land root-zone soil moisture minus its full-period climatology
# (built by SM2_anomaly.R). Not standardised (raw departure), like NDVI anomaly.
# Input: Codes/_archive/_data/sm_benin_monthly.parquet (needs SM1 + SM2 run first).
# Output: out/freq_soilm.parquet
# =============================================================================
source("Codes/nostd_freq/00_event_rule.R")
setwd(Q3$wd)

INP <- "Codes/_archive/_data/sm_benin_monthly.parquet"
if(!file.exists(INP))
  stop("sm_benin_monthly.parquet missing — run SM1_download_clip.py then SM2_anomaly.R first.")

sm <- as.data.frame(read_parquet(INP)) |>
  filter(year>=Q3$Y0, year<=Q3$Y1) |> arrange(x,y,year,month) |>
  group_by(x,y) |>
  summarise(events=count_events(sm_anom < 0, mindur=1), .groups="drop") |>
  mutate(freq=events/Q3$NYR, witness="SoilM-anom", scale=NA_integer_,
         rule="sm_anom < 0, >=1mo")

write_parquet(sm, file.path(Q3$out,"freq_soilm.parquet"))
cat("03b SoilM ->", file.path(Q3$out,"freq_soilm.parquet"),
    "| median freq", round(median(sm$freq),3), "\n")
