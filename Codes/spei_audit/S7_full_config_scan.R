# =============================================================================
# S7 — Full config scan: is thr-0.0 the ONLY anomaly, or do others deviate?
# For every family x scale x threshold: Obs-vs-Ref % change in freq, duration,
# severity. Flag configs where behaviour is counter-intuitive:
#   - freq DROPS in Obs (fewer droughts under warming), OR
#   - duration BALLOONS (>+100%) = merging signature.
# Local. events.parquet.
# =============================================================================
suppressMessages({library(arrow);library(dplyr);library(tidyr);library(stringr)})
setwd("c:/Users/tajayalath/Box/R - Projects - Tharaka/Drought-indices")
OUT <- "Codes/spei_audit/out"
REF0<-1973;REF1<-2002;NREF<-30; OBS0<-2003;OBS1<-2023;NOBS<-21

ev <- as.data.frame(read_parquet("Codes/_archive/_data/events.parquet")) |>
  filter(method=="threshold") |>
  mutate(scale=as.integer(str_extract(index,"\\d+")),
         fam=case_when(str_detect(index,"_pm$")~"SPEI-P",
                       str_detect(index,"^spei")~"SPEI-T",
                       str_detect(index,"^spi")~"SPI", TRUE~index))

met <- function(y0,y1,ny,lab) ev |> filter(event_year>=y0,event_year<=y1) |>
  group_by(fam,scale,start_thr) |>
  summarise(freq=n()/(dplyr::n_distinct(paste(x,y))*ny),
            dur=mean(duration_months),
            sev=mean(peak_index), .groups="drop") |> mutate(period=lab)

m <- bind_rows(met(REF0,REF1,NREF,"Reference"), met(OBS0,OBS1,NOBS,"Observation")) |>
  pivot_wider(names_from=period, values_from=c(freq,dur,sev)) |>
  mutate(dfreq=round(100*(freq_Observation-freq_Reference)/freq_Reference),
         ddur =round(100*(dur_Observation -dur_Reference )/dur_Reference),
         freq_drop = freq_Observation < freq_Reference,
         dur_balloon = ddur > 100)

cat("=== CONFIGS WHERE FREQ DROPS IN OBS (counter-intuitive) ===\n")
m |> filter(freq_drop) |> arrange(dfreq) |>
  transmute(fam,scale,thr=start_thr,dfreq_pct=dfreq,ddur_pct=ddur) |>
  as.data.frame() |> print(row.names=FALSE)

cat("\n=== CONFIGS WHERE DURATION BALLOONS >100% (merging signature) ===\n")
m |> filter(dur_balloon) |> arrange(desc(ddur)) |>
  transmute(fam,scale,thr=start_thr,dfreq_pct=dfreq,ddur_pct=ddur) |>
  as.data.frame() |> print(row.names=FALSE)

cat("\n=== summary: which thresholds ever show a freq drop, by family ===\n")
m |> group_by(fam) |>
  summarise(drop_thrs=paste(sort(unique(start_thr[freq_drop])),collapse=", "),
            n_drop=sum(freq_drop),
            balloon_thrs=paste(sort(unique(start_thr[dur_balloon])),collapse=", "),
            .groups="drop") |> as.data.frame() |> print(row.names=FALSE)

write.csv(m, file.path(OUT,"S7_full_config_scan.csv"), row.names=FALSE)
cat("\nWrote", file.path(OUT,"S7_full_config_scan.csv"),"\n")
