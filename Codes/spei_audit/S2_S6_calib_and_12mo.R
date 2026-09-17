# =============================================================================
# S2 — calibration/standardization sanity   +   S6 — 12-month deep dive
# S2: was SPEI z-scored so Ref mean ~0? Plot annual-mean SPEI-T by year; a
#     downward drift confirms the trend that drives thr-0.0 merging. Also verify
#     the same trend is weaker in SPI (no PET) -> isolates the PET/trend cause.
# S6: pull the worst 12-month/thr-0.0 pixel and show ONE long merged event.
# Local run. Reads speit + spi_results parquets, events for the pixel pick.
# =============================================================================
suppressMessages({library(arrow);library(dplyr);library(tidyr);library(ggplot2);library(stringr)})
setwd("c:/Users/tajayalath/Box/R - Projects - Tharaka/Drought-indices")
OUT <- "Codes/spei_audit/out"; FIG <- file.path(OUT,"figs")
REF0<-1973;REF1<-2002; OBS0<-2003;OBS1<-2023

spt <- as.data.frame(read_parquet("Codes/_archive/_data/speit.parquet")) |>
  filter(year>=REF0, year<=OBS1)
spi <- as.data.frame(read_parquet("Codes/_archive/_data/spi_results.parquet")) |>
  filter(year>=REF0, year<=OBS1)

# ---- S2: annual-mean index by year (154-px mean), SPEI-T vs SPI, 12-month ----
at <- spt |> group_by(year) |> summarise(SPEI_T=mean(spei12,na.rm=TRUE), .groups="drop")
ai <- spi |> group_by(year) |> summarise(SPI  =mean(spi12, na.rm=TRUE), .groups="drop")
a <- left_join(at, ai, by="year")

trend <- function(y,x) round(coef(lm(y~x))[2]*10,3)
cat("=== annual-mean 12-month index: trend per decade (1973-2023) ===\n")
cat(sprintf("SPEI-T : %+.3f /decade   (Ref mean %.2f, Obs mean %.2f)\n",
    trend(a$SPEI_T,a$year), mean(a$SPEI_T[a$year<=REF1]), mean(a$SPEI_T[a$year>=OBS0])))
cat(sprintf("SPI    : %+.3f /decade   (Ref mean %.2f, Obs mean %.2f)\n",
    trend(a$SPI,a$year), mean(a$SPI[a$year<=REF1]), mean(a$SPI[a$year>=OBS0])))
cat("(SPEI-T drifts more negative than SPI => PET-driven drying is the extra push.)\n")

g2 <- a |> pivot_longer(-year, names_to="index", values_to="val") |>
  mutate(index=recode(index, SPEI_T="SPEI-T")) |>
  ggplot(aes(year,val,colour=index)) +
  annotate("rect", xmin=OBS0-0.5, xmax=OBS1+0.5, ymin=-Inf, ymax=Inf, fill="grey88", alpha=0.5) +
  geom_hline(yintercept=0, colour="grey60") +
  geom_line(linewidth=0.7) + geom_smooth(method="lm", se=FALSE, linetype=2, linewidth=0.4) +
  scale_colour_manual(values=c("SPEI-T"="#2e8b3d","SPI"="#c0392b")) +
  labs(title="Annual-mean 12-month index drifts negative in Obs (SPEI-T > SPI)",
       subtitle="A sustained sub-zero drift is what merges threshold-0.0 events into few long ones.",
       x=NULL, y="Index (154-px annual mean)", colour=NULL) +
  theme_bw(base_size=11) + theme(legend.position="bottom", panel.grid.minor=element_blank())
ggsave(file.path(FIG,"S2_index_drift.png"), g2, width=8, height=4.5, dpi=150, bg="white")

# ---- S6: worst 12-month/thr-0.0 pixel: longest Obs event -> show the drift ---
ev <- as.data.frame(read_parquet("Codes/_archive/_data/events.parquet")) |>
  filter(method=="threshold", index=="spei12", start_thr==0.0,
         event_year>=OBS0, event_year<=OBS1)
worst <- ev |> slice_max(duration_months, n=1)
px <- worst[1,c("x","y")]
cat(sprintf("\n=== S6: worst SPEI-T-12 thr-0.0 Obs event = %.0f months at pixel (%.2f, %.2f) ===\n",
    worst$duration_months[1], px$x, px$y))

ts <- spt |> filter(abs(x-px$x)<1e-6, abs(y-px$y)<1e-6) |>
  mutate(date=as.Date(sprintf("%d-%02d-01",year,month))) |> arrange(date)
g6 <- ggplot(ts, aes(date, spei12)) +
  annotate("rect", xmin=as.Date(sprintf("%d-01-01",OBS0)), xmax=as.Date(sprintf("%d-12-31",OBS1)),
           ymin=-Inf, ymax=Inf, fill="grey88", alpha=0.5) +
  geom_hline(yintercept=0, colour="grey40") +
  geom_ribbon(aes(ymin=pmin(spei12,0), ymax=0), fill="#c0392b", alpha=0.5) +
  geom_line(linewidth=0.4) +
  labs(title=sprintf("SPEI-T-12 at the worst pixel: one long merged event (%.0f mo)", worst$duration_months[1]),
       subtitle="Threshold 0.0 counts any sub-zero stretch as one event; the Obs drift keeps it below 0 for years.",
       x=NULL, y="SPEI-T (12-month)") + theme_bw(base_size=11) +
  theme(panel.grid.minor=element_blank())
ggsave(file.path(FIG,"S6_worst_pixel_12mo.png"), g6, width=8, height=4.2, dpi=150, bg="white")

cat("Figs ->", FIG, "\n")
