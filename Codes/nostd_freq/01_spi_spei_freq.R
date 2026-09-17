# =============================================================================
# 01 — SPI / SPEI-T / SPEI-P drought event frequency (standardised baseline).
# Rule: index < 0, sustained >= 1 month (Q3$MINDUR=1, see 00_event_rule.R). Per
# pixel, per scale (1/3/6/12). Window 2003-2023.
# NOTE: SPI/SPEI here use index<0 on the monthly parquets so the event rule is
# IDENTICAL to NDVI/PDSI. (The characteristics deck instead reads the pipeline
# events.parquet at each start_thr; those are the "unchanged" threshold numbers.
# 10_figures.R uses BOTH: pipeline events for the threshold panels, this table is
# the matched-rule cross-check.)
# Output: out/freq_spi_spei.parquet
# =============================================================================
source("Codes/nostd_freq/00_event_rule.R")
setwd(Q3$wd)

idx_freq <- function(path, fam, cols){
  d <- as.data.frame(read_parquet(path)) |>
    filter(year>=Q3$Y0, year<=Q3$Y1) |> arrange(x,y,year,month)
  lapply(cols, function(cc){
    sc <- as.integer(str_extract(cc,"\\d+"))
    d |> group_by(x,y) |>
      summarise(events=count_events(.data[[cc]] < 0), .groups="drop") |>
      mutate(freq=events/Q3$NYR, witness=fam, scale=sc, rule="index < 0")
  }) |> bind_rows()
}

spi_spei <- bind_rows(
  idx_freq("Codes/_archive/_data/spi_results.parquet","SPI",
           c("spi1","spi3","spi6","spi12")),
  idx_freq("Codes/_archive/_data/speit.parquet","SPEI-T",
           c("spei1","spei3","spei6","spei12")),
  idx_freq("Codes/_archive/_data/spei_penman_results.parquet","SPEI-P",
           c("spei1","spei3","spei6","spei12"))
)
write_parquet(spi_spei, file.path(Q3$out,"freq_spi_spei.parquet"))
cat("01 SPI/SPEI ->", file.path(Q3$out,"freq_spi_spei.parquet"),
    "|", nrow(spi_spei), "rows\n")
