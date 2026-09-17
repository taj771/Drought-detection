# =============================================================================
# S4 — Freq-drop reconciliation + S6 12-month deep dive
# Hypothesis: the SPEI-T freq DROP at threshold 0.0 is NOT droughts vanishing,
# but droughts MERGING — a warming/drying trend keeps the index below 0 for long
# stretches, so many short events become one long event: freq DOWN, duration UP.
# Test: at thr 0.0, does Obs duration RISE where Obs freq FALLS?
# Also decompose the S0 pattern by threshold to show the drop is thr-0.0-specific.
# Local run. Reads events.parquet + speit.parquet.
# =============================================================================
suppressMessages({library(arrow);library(dplyr);library(tidyr);library(stringr);library(ggplot2)})
setwd("c:/Users/tajayalath/Box/R - Projects - Tharaka/Drought-indices")
OUT <- "Codes/spei_audit/out"; FIG <- file.path(OUT,"figs")
REF0<-1973;REF1<-2002;NREF<-30; OBS0<-2003;OBS1<-2023;NOBS<-21

ev <- as.data.frame(read_parquet("Codes/_archive/_data/events.parquet")) |>
  filter(method=="threshold") |>
  mutate(scale=as.integer(str_extract(index,"\\d+")),
         fam=case_when(str_detect(index,"_pm$")~"SPEI-P",
                       str_detect(index,"^spei")~"SPEI-T",
                       str_detect(index,"^spi")~"SPI", TRUE~index))

# per family/scale/threshold/period: freq AND mean duration
metrics <- function(y0,y1,ny,lab){
  ev |> filter(event_year>=y0,event_year<=y1) |>
    group_by(fam,scale,start_thr) |>
    summarise(freq=n()/(dplyr::n_distinct(paste(x,y))*ny),
              dur=mean(duration_months), .groups="drop") |>
    mutate(period=lab)
}
m <- bind_rows(metrics(REF0,REF1,NREF,"Reference"), metrics(OBS0,OBS1,NOBS,"Observation"))

# reconciliation table for SPEI-T at thr 0.0 (the drop configs)
cat("=== SPEI-T @ threshold 0.0 : freq DOWN but duration UP? ===\n")
rec <- m |> filter(fam=="SPEI-T", start_thr==0.0) |>
  select(scale,period,freq,dur) |>
  pivot_wider(names_from=period, values_from=c(freq,dur)) |>
  mutate(freq_pct=round(100*(freq_Observation-freq_Reference)/freq_Reference),
         dur_pct =round(100*(dur_Observation -dur_Reference )/dur_Reference))
for(i in seq_len(nrow(rec))){ r<-rec[i,]
  cat(sprintf("  scale %2d : freq %+d%%  (%.2f->%.2f) | duration %+d%% (%.1f->%.1f mo)\n",
      r$scale, r$freq_pct, r$freq_Reference, r$freq_Observation,
      r$dur_pct, r$dur_Reference, r$dur_Observation)) }

# does the freq drop only happen at high thresholds (0.0/-0.25)?
cat("\n=== SPEI-T freq %change by threshold (mean over scales) ===\n")
m |> filter(fam=="SPEI-T") |>
  select(scale,start_thr,period,freq) |>
  pivot_wider(names_from=period, values_from=freq) |>
  group_by(start_thr) |>
  summarise(mean_pct=round(mean(100*(Observation-Reference)/Reference)), .groups="drop") |>
  as.data.frame() |> print()

# ---- fig: freq vs duration change, thr 0.0, all families --------------------
d0 <- m |> filter(start_thr==0.0) |>
  select(fam,scale,period,freq,dur) |>
  pivot_wider(names_from=period, values_from=c(freq,dur)) |>
  mutate(dfreq=100*(freq_Observation-freq_Reference)/freq_Reference,
         ddur =100*(dur_Observation -dur_Reference )/dur_Reference,
         scale_lab=paste0(scale,"-mo"))
g <- ggplot(d0, aes(dfreq, ddur, colour=fam, label=scale_lab)) +
  geom_hline(yintercept=0, colour="grey70") + geom_vline(xintercept=0, colour="grey70") +
  geom_point(size=2.5) + ggrepel::geom_text_repel(size=2.8, show.legend=FALSE) +
  scale_colour_manual(values=c("SPI"="#c0392b","SPEI-T"="#2e8b3d","SPEI-P"="#2c6fb5")) +
  labs(title="Threshold 0.0: fewer events but longer? (Obs vs Ref, % change)",
       subtitle="Lower-right quadrant = freq DOWN + duration UP = events merging, not vanishing.",
       x="Frequency change (%)", y="Duration change (%)", colour=NULL) +
  theme_bw(base_size=11) + theme(legend.position="bottom", panel.grid.minor=element_blank())
ggsave(file.path(FIG,"S4_freq_vs_duration_thr0.png"), g, width=8, height=5, dpi=150, bg="white")

cat("\nFig ->", file.path(FIG,"S4_freq_vs_duration_thr0.png"), "\n")
