# =============================================================================
# 05 — Side-by-side drought-event time series (2003-2023): SPI-3, NDVI anomaly,
# soil moisture. Each panel = country monthly series (median over 154 pixels),
# with the below-threshold stretches shaded red = detected drought, in the SAME
# style as the SPI event-detection plot (line + dashed threshold + red ribbon).
# Stacked so drought timing can be compared across the indices at a glance.
#   SPI-3      : threshold 0   (index < 0)
#   NDVI-anom  : threshold 0   (anomaly < 0)
#   SoilM-anom : threshold 0   (anomaly < 0)
# PDSI dropped from competitor per team decision (NDVI + soil moisture only);
# PDSI lines kept commented in case we re-add.
#   PDSI       : threshold -2  (Palmer moderate)   # to re-add
# =============================================================================
source("Codes/nostd_freq/00_event_rule.R")
setwd(Q3$wd)
suppressMessages(library(ggplot2))
FIG <- file.path(Q3$out,"figs")

# --- country monthly series = median over pixels, per index ------------------
monthly_median <- function(path, col){
  as.data.frame(read_parquet(path)) |>
    filter(year>=Q3$Y0, year<=Q3$Y1) |>
    group_by(year, month) |>
    summarise(v=median(.data[[col]], na.rm=TRUE), .groups="drop") |>
    mutate(date=as.Date(sprintf("%d-%02d-01", year, month)))
}
spi  <- monthly_median("Codes/_archive/_data/spi_results.parquet","spi3") |>
  mutate(index="SPI-3 (threshold 0)", thr=0)
ndvi <- monthly_median("Codes/_archive/_data/ndvi_anom_benin_monthly.parquet","ndvi_anom") |>
  mutate(index="NDVI anomaly (threshold 0)", thr=0)
soilm<- monthly_median("Codes/_archive/_data/sm_benin_monthly.parquet","sm_anom") |>
  mutate(index="Soil-moisture anomaly (threshold 0)", thr=0)
# pdsi <- monthly_median("Codes/pdsi_validation/out/pdsi_benin_monthly.parquet","pdsi") |>  # to re-add
#   mutate(index="PDSI (threshold -2)", thr=-2)

LVL <- c("SPI-3 (threshold 0)","NDVI anomaly (threshold 0)",
         "Soil-moisture anomaly (threshold 0)")  # ,"PDSI (threshold -2)" to re-add
d <- bind_rows(spi, ndvi, soilm) |>  # , pdsi to re-add
  mutate(index=factor(index, levels=LVL))

pal <- c("SPI-3 (threshold 0)"="#c0392b",
         "NDVI anomaly (threshold 0)"="#8c6d1f",
         "Soil-moisture anomaly (threshold 0)"="#1a9e9e")
         # ,"PDSI (threshold -2)"="#6a3d9a"  # to re-add

p <- ggplot(d, aes(date, v)) +
  # red ribbon where below threshold = detected drought
  geom_ribbon(aes(ymin=pmin(v, thr), ymax=thr), fill="#c0392b", alpha=0.35) +
  geom_hline(aes(yintercept=thr), linetype=2, colour="grey35", linewidth=0.4) +
  geom_line(aes(colour=index), linewidth=0.5) +
  facet_wrap(~index, ncol=1, scales="free_y") +
  scale_colour_manual(values=pal, guide="none") +
  scale_x_date(date_breaks="2 years", date_labels="%Y") +
  labs(title="Drought events over time: SPI-3, NDVI, soil moisture (Benin, 2003-2023)",
       subtitle="Country monthly series (median over 154 pixels). Red = below the drought threshold = detected drought. Dashed line = threshold.",
       x=NULL, y="Index value (per panel)") +
  theme_bw(base_size=10) +
  theme(strip.background=element_rect(fill="grey92", colour="grey30"),
        strip.text=element_text(face="bold", size=8.5),
        panel.grid.minor=element_blank(),
        plot.subtitle=element_text(size=8))
ggsave(file.path(FIG,"V5_event_timeseries.png"), p, width=9, height=6.4, dpi=150, bg="white")
cat("fig ->", file.path(FIG,"V5_event_timeseries.png"), "\n")
