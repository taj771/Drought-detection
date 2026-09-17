# Analogy figure — REAL Benin SPEI-T data.
# One pixel, SPEI-T monthly. Threshold = 0 (below normal). Red = drought month.
# Top = Reference (1973-2002): dry months in several short bursts -> MANY events.
# Bottom = Observation (2003-2023): warming drift -> one long run -> FEW events.
# Pixel auto-picked: the one whose Obs-vs-Ref event-count DROP is largest at the
# chosen scale (so the story is real, not cherry-picked cosmetics).
suppressMessages({library(arrow);library(dplyr);library(tidyr);library(ggplot2)})
setwd("c:/Users/tajayalath/Box/R - Projects - Tharaka/Drought-indices")
FIG <- "Codes/spei_audit/out/figs"
SCALE_COL <- "spei3"          # 3-month: enough wiggle to show short bursts
REF0<-1973;REF1<-2002; OBS0<-2003;OBS1<-2023

spt <- as.data.frame(read_parquet("Codes/_archive/_data/speit.parquet")) |>
  filter(year>=REF0, year<=OBS1) |>
  transmute(x,y,year,month, spei=.data[[SCALE_COL]]) |>
  arrange(x,y,year,month)

# count events (runs of consecutive <0 months) per pixel per period
count_events <- function(v){ d<-v<0 & !is.na(v); sum(d & !dplyr::lag(d,default=FALSE)) }
by_px <- spt |> group_by(x,y) |>
  summarise(ev_ref=count_events(spei[year<=REF1]),
            ev_obs=count_events(spei[year>=OBS0]),
            .groups="drop") |>
  mutate(drop = ev_ref - ev_obs)
pick <- by_px |> filter(ev_ref>=6, ev_obs<=ev_ref) |> slice_max(drop, n=1)
px <- pick[1,c("x","y")]
cat(sprintf("Pixel (%.2f,%.2f): Ref events %d -> Obs events %d\n",
            px$x, px$y, pick$ev_ref[1], pick$ev_obs[1]))

ts <- spt |> filter(abs(x-px$x)<1e-6, abs(y-px$y)<1e-6) |>
  mutate(period=ifelse(year<=REF1,
           sprintf("Reference 1973-2002  →  %d separate droughts", pick$ev_ref[1]),
           sprintf("Observation 2003-2023 (warmer)  →  %d drought(s)", pick$ev_obs[1])),
         period=factor(period, levels=c(
           sprintf("Reference 1973-2002  →  %d separate droughts", pick$ev_ref[1]),
           sprintf("Observation 2003-2023 (warmer)  →  %d drought(s)", pick$ev_obs[1]))),
         drought = spei<0 & !is.na(spei),
         t = (year-ifelse(year<=REF1,REF0,OBS0))*12 + month)

p <- ggplot(ts, aes(t, spei, fill=drought)) +
  geom_col(width=1) +
  geom_hline(yintercept=0, linetype=2, colour="grey30") +
  facet_wrap(~period, ncol=1, scales="free_x") +
  scale_fill_manual(values=c("FALSE"="grey75","TRUE"="#c0392b"), guide="none") +
  labs(x="month within period", y="SPEI-T (3-month)",
       title=sprintf("A real Benin pixel (%.2f°E, %.2f°N): red = month below normal", px$x, px$y)) +
  theme_bw(base_size=11) +
  theme(strip.background=element_rect(fill="grey92",colour="grey30"),
        strip.text=element_text(face="bold", size=9.5),
        panel.grid.minor=element_blank(), panel.grid.major.x=element_blank(),
        plot.title=element_text(size=10.5))
ggsave(file.path(FIG,"S8_analogy_bars.png"), p, width=8.6, height=4.6, dpi=150, bg="white")
cat("fig ->", file.path(FIG,"S8_analogy_bars.png"), "\n")
