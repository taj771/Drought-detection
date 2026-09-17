# Config-corner heatmap: for every family x scale x threshold, the Obs-vs-Ref
# frequency % change. Shows the "drop" is a predictable CORNER (shallow threshold
# x long scale x SPEI-T), not one stray config. Blue = more drought (expected),
# red = fewer events (merging).
suppressMessages({library(arrow);library(dplyr);library(tidyr);library(stringr);library(ggplot2)})
setwd("c:/Users/tajayalath/Box/R - Projects - Tharaka/Drought-indices")
FIG <- "Codes/spei_audit/out/figs"
REF0<-1973;REF1<-2002;NREF<-30; OBS0<-2003;OBS1<-2023;NOBS<-21

ev <- as.data.frame(read_parquet("Codes/_archive/_data/events.parquet")) |>
  filter(method=="threshold") |>
  mutate(scale=as.integer(str_extract(index,"\\d+")),
         fam=case_when(str_detect(index,"_pm$")~"SPEI-P",
                       str_detect(index,"^spei")~"SPEI-T",
                       str_detect(index,"^spi")~"SPI", TRUE~index))
fr <- function(y0,y1,ny,lab) ev|>filter(event_year>=y0,event_year<=y1)|>
  group_by(fam,scale,start_thr)|>
  summarise(freq=n()/(dplyr::n_distinct(paste(x,y))*ny),.groups="drop")|>mutate(period=lab)
m <- bind_rows(fr(REF0,REF1,NREF,"R"),fr(OBS0,OBS1,NOBS,"O"))|>
  pivot_wider(names_from=period,values_from=freq)|>
  mutate(pct=100*(O-R)/R,
         fam=factor(fam,levels=c("SPI","SPEI-T","SPEI-P")),
         thr_f=factor(start_thr,levels=c(0,-0.25,-0.5,-1,-1.5),
                      labels=c("0.0","-0.25","-0.5","-1.0","-1.5")),
         sc_f=factor(scale,levels=c(1,3,6,12),labels=paste0(c(1,3,6,12),"-mo")))

p <- ggplot(m, aes(thr_f, sc_f, fill=pct)) +
  geom_tile(colour="white", linewidth=0.6) +
  geom_text(aes(label=sprintf("%+d",round(pct))), size=2.6, fontface="bold") +
  facet_wrap(~fam) +
  scale_fill_gradient2(low="#c0392b", mid="grey95", high="#2c6fb5",
                       midpoint=0, name="Freq %\nchange") +
  labs(title="Mean-frequency change (Obs vs Ref): the drop is a corner, not one config",
       subtitle="Same mean as the box-plot diamond. Red = fewer events (merging), blue = more drought.",
       x="threshold", y="accumulation") +
  theme_minimal(base_size=10) +
  theme(strip.text=element_text(face="bold"), panel.grid=element_blank(),
        plot.title=element_text(size=10.5), plot.subtitle=element_text(size=8.5),
        legend.key.width=unit(10,"pt"))
ggsave(file.path(FIG,"S9_config_heatmap.png"), p, width=9, height=3.8, dpi=150, bg="white")
cat("fig ->", file.path(FIG,"S9_config_heatmap.png"), "\n")
