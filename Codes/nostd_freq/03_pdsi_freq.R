# =============================================================================
# 03 — PDSI drought event frequency (PHYSICAL water balance, NOT standardised).
# Rule: PDSI <= -2 (Palmer "moderate drought"), >= 1 month = one event. Same rule
# as every witness (Q3$MINDUR=1) and the characteristics-deck pipeline
# (min_duration = 1). Window 2003-2023. Per pixel.
# NOTE: stricter/milder cuts (-1 / -3) give NON-monotonic frequency because mild
# drought sits below threshold for long continuous runs that merge into single
# events (same merging effect as the SPEI-T threshold-0.0 audit). We keep the
# single -2 class to avoid that confusion in the standardisation deck.
# Output: out/freq_pdsi.parquet
# =============================================================================
source("Codes/nostd_freq/00_event_rule.R")
setwd(Q3$wd)

pdsi <- as.data.frame(read_parquet("Codes/pdsi_validation/out/pdsi_benin_monthly.parquet")) |>
  filter(year>=Q3$Y0, year<=Q3$Y1) |> arrange(x,y,year,month) |>
  group_by(x,y) |>
  summarise(events=count_events(pdsi <= -2, mindur=1), .groups="drop") |>
  mutate(freq=events/Q3$NYR, witness="PDSI", scale=NA_integer_,
         rule="PDSI <= -2, >=1mo")

write_parquet(pdsi, file.path(Q3$out,"freq_pdsi.parquet"))
cat("03 PDSI ->", file.path(Q3$out,"freq_pdsi.parquet"),
    "| median freq", round(median(pdsi$freq),3), "\n")
