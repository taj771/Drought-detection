# =============================================================================
# 04 — Yearly drought-frequency time series (2003-2023), SPI-3 (thr -0.5) vs
# NDVI anomaly vs soil moisture, with temporal correlation between the lines.
# PDSI dropped from competitor per team decision (NDVI + soil moisture only);
# PDSI lines kept commented in case we re-add.
# Per pixel per year we count events (below-threshold run >= 1 month, event
# attributed to its START year). Then take the MEAN over the 154 pixels -> one
# point per year per witness. (Mean, not median: median of small integer counts
# is too lumpy and destroys the year-to-year variation the correlation needs.)
# Correlate the yearly SPI line against NDVI and PDSI across the 21 years
# (Pearson + Spearman); r shown in the legend.
# Reads monthly parquets directly (fresh yearly event counts). Output: figure +
# yearly table.
# =============================================================================
source("Codes/nostd_freq/00_event_rule.R")
setwd(Q3$wd)
suppressMessages(library(ggplot2))
FIG <- file.path(Q3$out,"figs")

# --- per-pixel-per-year event count (event = run>=1mo, tagged to start year) --
# For a monthly series per pixel: find runs of cond TRUE; each run counts once,
# in the calendar year of its first month.
yearly_events <- function(df, condcol, thr_expr){
  df |> arrange(x,y,year,month) |>
    group_by(x,y) |>
    group_modify(function(g,key){
      cond <- eval(thr_expr, g); cond[is.na(cond)] <- FALSE
      r <- rle(cond)
      ends <- cumsum(r$lengths); starts <- ends - r$lengths + 1
      run_start_idx <- starts[r$values & r$lengths >= Q3$MINDUR]
      yrs <- g$year[run_start_idx]
      tibble(year=Q3$Y0:Q3$Y1) |>
        left_join(as.data.frame(table(year=yrs)) |>
                    mutate(year=as.integer(as.character(year))),
                  by="year") |>
        mutate(n=ifelse(is.na(Freq),0L,Freq)) |> select(year,n)
    }) |> ungroup()
}

# SPI-3 at threshold -0.5  (index < -0.5)
spi3 <- as.data.frame(read_parquet("Codes/_archive/_data/spi_results.parquet")) |>
  filter(year>=Q3$Y0, year<=Q3$Y1) |> transmute(x,y,year,month, v=spi3)
spi_yr <- yearly_events(spi3, , quote(v < -0.5)) |> mutate(witness="SPI-3 (thr -0.5)")

# NDVI anomaly < 0
ndvi <- as.data.frame(read_parquet("Codes/_archive/_data/ndvi_anom_benin_monthly.parquet")) |>
  filter(year>=Q3$Y0, year<=Q3$Y1) |> transmute(x,y,year,month, v=ndvi_anom)
ndvi_yr <- yearly_events(ndvi, , quote(v < 0)) |> mutate(witness="NDVI-anom")

# Soil-moisture anomaly < 0
soilm <- as.data.frame(read_parquet("Codes/_archive/_data/sm_benin_monthly.parquet")) |>
  filter(year>=Q3$Y0, year<=Q3$Y1) |> transmute(x,y,year,month, v=sm_anom)
soilm_yr <- yearly_events(soilm, , quote(v < 0)) |> mutate(witness="SoilM-anom")

# PDSI <= -2  (dropped from competitor; to re-add, uncomment + add pdsi_yr below)
# pdsi <- as.data.frame(read_parquet("Codes/pdsi_validation/out/pdsi_benin_monthly.parquet")) |>
#   filter(year>=Q3$Y0, year<=Q3$Y1) |> transmute(x,y,year,month, v=pdsi)
# pdsi_yr <- yearly_events(pdsi, , quote(v <= -2)) |> mutate(witness="PDSI")

# --- mean over pixels -> one point per year per witness ----------------------
# (mean, not median: median of small integer counts is too lumpy — 0/1/2 steps —
# and destroys the year-to-year variation the correlation needs.)
yr <- bind_rows(spi_yr, ndvi_yr, soilm_yr) |>  # , pdsi_yr to re-add
  group_by(witness, year) |>
  summarise(freq=mean(n), .groups="drop")

wide <- yr |> tidyr::pivot_wider(names_from=witness, values_from=freq)
r_ndvi_p <- cor(wide$`SPI-3 (thr -0.5)`, wide$`NDVI-anom`, method="pearson")
r_ndvi_s <- cor(wide$`SPI-3 (thr -0.5)`, wide$`NDVI-anom`, method="spearman")
r_soil_p <- cor(wide$`SPI-3 (thr -0.5)`, wide$`SoilM-anom`, method="pearson")
r_soil_s <- cor(wide$`SPI-3 (thr -0.5)`, wide$`SoilM-anom`, method="spearman")
# r_pdsi_p <- cor(wide$`SPI-3 (thr -0.5)`, wide$`PDSI`, method="pearson")   # to re-add
# r_pdsi_s <- cor(wide$`SPI-3 (thr -0.5)`, wide$`PDSI`, method="spearman")  # to re-add
cat(sprintf("SPI-3 vs NDVI  : r=%.2f (Pearson), rho=%.2f (Spearman)\n", r_ndvi_p, r_ndvi_s))
cat(sprintf("SPI-3 vs SoilM : r=%.2f (Pearson), rho=%.2f (Spearman)\n", r_soil_p, r_soil_s))
# cat(sprintf("SPI-3 vs PDSI : r=%.2f (Pearson), rho=%.2f (Spearman)\n", r_pdsi_p, r_pdsi_s))
write.csv(wide, file.path(Q3$out,"yearly_freq.csv"), row.names=FALSE)

lab <- c(sprintf("SPI-3 (thr -0.5)"),
         sprintf("NDVI-anom  (vs SPI: r=%.2f, rho=%.2f)", r_ndvi_p, r_ndvi_s),
         sprintf("SoilM-anom  (vs SPI: r=%.2f, rho=%.2f)", r_soil_p, r_soil_s))
         # , sprintf("PDSI  (vs SPI: r=%.2f, rho=%.2f)", r_pdsi_p, r_pdsi_s)  # to re-add
yr <- yr |> mutate(witness=factor(witness,
        levels=c("SPI-3 (thr -0.5)","NDVI-anom","SoilM-anom"), labels=lab))  # ,"PDSI" to re-add
pal <- setNames(c("#c0392b","#8c6d1f","#1a9e9e"), lab)  # ,"#6a3d9a" to re-add

p <- ggplot(yr, aes(year, freq, colour=witness)) +
  geom_line(linewidth=0.7) + geom_point(size=1.6) +
  scale_colour_manual(values=pal, name=NULL) +
  scale_x_continuous(breaks=seq(Q3$Y0,Q3$Y1,3)) +
  labs(title="Yearly drought frequency: SPI-3 vs NDVI & soil moisture (Benin, 2003-2023)",
       subtitle="One point = mean events/pixel that year (event = below-threshold run >=1mo, tagged to start year). r = temporal correlation of each line with SPI-3.",
       x=NULL, y="Mean events / pixel / year") +
  theme_bw(base_size=11) +
  theme(legend.position="bottom", panel.grid.minor=element_blank(),
        plot.subtitle=element_text(size=8)) +
  guides(colour=guide_legend(nrow=3))
ggsave(file.path(FIG,"V4_yearly_lines.png"), p, width=8.6, height=4.8, dpi=150, bg="white")
cat("fig ->", file.path(FIG,"V4_yearly_lines.png"), "\n")
