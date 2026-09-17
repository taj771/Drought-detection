# =============================================================================
# 02 — NDVI anomaly drought event frequency (RAW vegetation, NOT standardised).
# Rule: ndvi_anom < 0, >= 1 month = one event. SAME rule as SPI/SPEI/PDSI and the
# characteristics deck (a below-threshold run of any length is one event, e.g.
# Jan-only = 1 event; Jan-through-May continuous = 1 event). No special-case rule.
# NDVI anomaly has no accumulation scale (native monthly) and no fitted threshold,
# so one frequency per pixel. Window 2003-2023.
# Output: out/freq_ndvi.parquet
# =============================================================================
source("Codes/nostd_freq/00_event_rule.R")
setwd(Q3$wd)

ndvi <- as.data.frame(read_parquet("Codes/_archive/_data/ndvi_anom_benin_monthly.parquet")) |>
  filter(year>=Q3$Y0, year<=Q3$Y1) |> arrange(x,y,year,month) |>
  group_by(x,y) |>
  summarise(events=count_events(ndvi_anom < 0, mindur=1), .groups="drop") |>
  mutate(freq=events/Q3$NYR, witness="NDVI-anom", scale=NA_integer_,
         rule="anomaly < 0, >=1mo")

write_parquet(ndvi, file.path(Q3$out,"freq_ndvi.parquet"))
cat("02 NDVI ->", file.path(Q3$out,"freq_ndvi.parquet"),
    "| median freq", round(median(ndvi$freq),3), "\n")
