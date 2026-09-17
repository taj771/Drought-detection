# =============================================================================
# S0 — Reproduce & quantify the SPEI-T freq drop (Ref vs Obs)
# Supervisor concern: SPEI-T drought FREQUENCY DROPS in Observation period,
# especially at low thresholds; 12-month / threshold -1.0 looks odd.
# This script sizes the effect from the event table (all indices, for contrast).
# Local run, reads Codes/_archive/_data/events.parquet. No Databricks.
# =============================================================================
suppressMessages({library(arrow);library(dplyr);library(tidyr);library(stringr)})
setwd("c:/Users/tajayalath/Box/R - Projects - Tharaka/Drought-indices")
OUT <- "Codes/spei_audit/out"; dir.create(file.path(OUT,"figs"), showWarnings=FALSE, recursive=TRUE)

REF0<-1973; REF1<-2002; NREF<-REF1-REF0+1
OBS0<-2003; OBS1<-2023; NOBS<-OBS1-OBS0+1

ev <- as.data.frame(read_parquet("Codes/_archive/_data/events.parquet")) |>
  filter(method=="threshold") |>
  mutate(scale=as.integer(str_extract(index,"\\d+")),
         fam=case_when(str_detect(index,"_pm$")~"SPEI-P",
                       str_detect(index,"^spei")~"SPEI-T",
                       str_detect(index,"^spi")~"SPI", TRUE~index))

# frequency = events per pixel per year, Ref vs Obs, per family/scale/threshold
freq_period <- function(y0,y1,ny,lab){
  ev |> filter(event_year>=y0, event_year<=y1) |>
    group_by(fam, scale, start_thr, x, y) |>
    summarise(n=n(), .groups="drop") |>
    group_by(fam, scale, start_thr) |>
    summarise(freq=sum(n)/(dplyr::n_distinct(paste(x,y))*ny), .groups="drop") |>
    mutate(period=lab)
}
fp <- bind_rows(freq_period(REF0,REF1,NREF,"Reference"),
                freq_period(OBS0,OBS1,NOBS,"Observation"))

# wide table: Ref, Obs, delta, pct change
tab <- fp |>
  pivot_wider(names_from=period, values_from=freq) |>
  mutate(delta=Observation-Reference,
         pct=100*delta/Reference) |>
  arrange(fam, scale, desc(start_thr))

write.csv(tab, file.path(OUT,"S0_freq_ref_obs.csv"), row.names=FALSE)

cat("=== Frequency change Obs - Ref (events/pixel/yr) ===\n")
cat("Negative pct = FEWER droughts in Observation period.\n\n")
for(fm in c("SPI","SPEI-T","SPEI-P")){
  cat("---", fm, "---\n")
  sub <- tab |> filter(fam==fm)
  for(i in seq_len(nrow(sub))){
    r <- sub[i,]
    cat(sprintf("  scale %2d  thr %5.2f : Ref %.3f  Obs %.3f  d %+.3f (%+.0f%%)\n",
        r$scale, r$start_thr, r$Reference, r$Observation, r$delta, r$pct))
  }
}

# summary: how many configs DROP in Obs, per family
cat("\n=== configs with Obs < Ref (freq drop), per family ===\n")
tab |> group_by(fam) |>
  summarise(n_configs=n(), n_drop=sum(delta<0),
            mean_pct=round(mean(pct,na.rm=TRUE),1),
            worst_pct=round(min(pct,na.rm=TRUE),1), .groups="drop") |>
  as.data.frame() |> print()

cat("\nWrote", file.path(OUT,"S0_freq_ref_obs.csv"), "\n")
