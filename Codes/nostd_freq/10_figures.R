# =============================================================================
# 10 — Deck figures for Q3. Pure plotting: reads the saved freq tables from
# 01/02/03, computes nothing new except the pipeline SPI/SPEI threshold numbers
# (from events.parquet, the "unchanged" characteristics-deck values).
# Produces one figure per accumulation scale (1/3/6/12-month): SPI/SPEI at each
# threshold + NDVI and soil-moisture as fixed grey reference boxes (same on every
# panel). PDSI dropped from competitor per team decision (NDVI + soil moisture
# only); PDSI lines kept commented in case we re-add.
# Outputs: out/figs/V3_ref_01mo.png .. V3_ref_12mo.png
# =============================================================================
source("Codes/nostd_freq/00_event_rule.R")
setwd(Q3$wd)
suppressMessages(library(ggplot2))
FIG <- file.path(Q3$out,"figs"); dir.create(FIG, showWarnings=FALSE, recursive=TRUE)

thr_lab <- function(v) factor(v, levels=Q3$THR,
  labels=c("Thr 0.0","Thr -0.5","Thr -1.0","Thr -1.5"))
sc_lab  <- function(v) factor(paste0(v,"-mo"), levels=paste0(Q3$SCALES,"-mo"))

# ---- SPI/SPEI per-threshold per-scale freq (UNCHANGED, pipeline events) ------
spi_freq <- as.data.frame(read_parquet("Codes/_archive/_data/events.parquet")) |>
  filter(method=="threshold", start_thr %in% Q3$THR,
         event_year>=Q3$Y0, event_year<=Q3$Y1) |>
  mutate(scale=as.integer(str_extract(index,"\\d+")),
         fam=case_when(str_detect(index,"_pm$")~"SPEI-P",
                       str_detect(index,"^spei")~"SPEI-T",
                       str_detect(index,"^spi")~"SPI", TRUE~index)) |>
  filter(scale %in% Q3$SCALES) |>
  group_by(fam, start_thr, scale, x, y) |>
  summarise(freq=n()/Q3$NYR, .groups="drop") |>
  transmute(grp=fam, thr_f=thr_lab(start_thr), sc_f=sc_lab(scale), freq)

# ---- NDVI + SoilM reference (saved tables), across all panels ----------------
ref <- bind_rows(
  as.data.frame(read_parquet(file.path(Q3$out,"freq_ndvi.parquet"))),
  as.data.frame(read_parquet(file.path(Q3$out,"freq_soilm.parquet")))
  # PDSI dropped from competitor (team: NDVI + soil moisture only). To re-add:
  # , as.data.frame(read_parquet(file.path(Q3$out,"freq_pdsi.parquet")))
) |> transmute(grp=witness, x, y, freq)
grid <- expand.grid(start_thr=Q3$THR, scale=Q3$SCALES)
ref_rep <- Map(function(t,s) ref |> mutate(thr_f=thr_lab(t), sc_f=sc_lab(s)),
               grid$start_thr, grid$scale) |> bind_rows() |>
  select(grp, thr_f, sc_f, freq)

d <- bind_rows(spi_freq, ref_rep) |>
  mutate(grp=factor(grp, levels=c("SPI","SPEI-T","SPEI-P",
                                  "NDVI-anom","SoilM-anom")),  # ,"PDSI" to re-add
         scale=as.integer(str_extract(as.character(sc_f),"\\d+")))

pal <- c("SPI"="#c0392b","SPEI-T"="#2e8b3d","SPEI-P"="#2c6fb5",
         "NDVI-anom"="#8c6d1f","SoilM-anom"="#1a9e9e")  # ,"PDSI"="#6a3d9a" to re-add

make_scale_fig <- function(sc){
  ds <- d |> filter(scale==sc)
  med <- ds |> group_by(thr_f,grp) |> summarise(m=median(freq), .groups="drop")
  p <- ggplot(ds, aes(grp, freq, fill=grp)) +
    geom_rect(data=distinct(ds, thr_f), inherit.aes=FALSE,
              aes(xmin=3.5, xmax=5.5, ymin=-Inf, ymax=Inf), fill="grey90", alpha=0.5) +
    geom_boxplot(width=0.62, outlier.size=0.22, colour="grey20", linewidth=0.28) +
    geom_label(data=med, aes(grp, m, label=sprintf("%.2f",m)), inherit.aes=FALSE,
               size=1.9, vjust=-0.4, fill="white", alpha=0.8, label.size=0,
               fontface="bold", label.padding=unit(0.35,"pt")) +
    facet_wrap(~thr_f, nrow=2) +
    scale_fill_manual(values=pal, name=NULL) +
    scale_y_continuous(expand=expansion(mult=c(0.03,0.12))) +
    labs(title=sprintf("Drought frequency, %d-month accumulation, with NDVI & soil-moisture reference", sc),
         subtitle="Events/pixel/yr, 2003-2023. SPI/SPEI numbers unchanged. Grey = raw witnesses (NDVI anom<0, soil-moisture anom<0), same on every panel, no threshold.",
         x=NULL, y="Frequency (events/pixel/yr)") +
    theme_bw(base_size=10) +
    theme(strip.background=element_rect(fill="grey92",colour="grey30"),
          strip.text=element_text(face="bold", size=9),
          axis.text.x=element_text(size=6.3, angle=20, hjust=1),
          panel.grid.minor=element_blank(), panel.grid.major.x=element_blank(),
          legend.position="none", plot.subtitle=element_text(size=6.8))
  ggsave(file.path(FIG, sprintf("V3_ref_%02dmo.png", sc)), p,
         width=9.2, height=5, dpi=150, bg="white")
}
invisible(lapply(Q3$SCALES, make_scale_fig))
cat("10 figures -> V3_ref_01/03/06/12mo.png in", FIG, "\n")
