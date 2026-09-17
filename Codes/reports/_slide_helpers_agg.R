# Data + figure helpers for the AGG (two-aggregation-path) slide deck.
# Same data + config as _slide_helpers.R, but each family x period now shows
# TWO boxes side by side:
#   * Spatial  = time-collapse (one value per pixel, 154 px)  <- the deck default
#   * Temporal = space-collapse (one value per year, ~21 yr)  <- the alternative
# Same overall mean either way; the two boxes show WHERE the spread lives
# (pixel-to-pixel vs year-to-year).
suppressMessages({ library(arrow); library(dplyr); library(tidyr); library(ggplot2); library(stringr) })

EVENTS_PATH <- "../_archive/_data/events.parquet"
THRESHOLDS  <- c(0.0,-0.5,-1.0,-1.5)
REF0<-1973;REF1<-2002;NREF<-REF1-REF0+1
OBS0<-2003;OBS1<-2023;NOBS<-OBS1-OBS0+1
thr_labels <- c("Threshold 0.0","Threshold -0.5","Threshold -1.0","Threshold -1.5")

# family x period palette (unchanged), used to colour the box FILL.
pal <- c("SPI · Reference"="#f4a6a0","SPI · Observation"="#c0392b",
         "SPEI-T · Reference"="#a3d9a5","SPEI-T · Observation"="#2e8b3d",
         "SPEI-P · Reference"="#a8c8ec","SPEI-P · Observation"="#2c6fb5")

ev <- as.data.frame(arrow::read_parquet(EVENTS_PATH)) |>
  filter(method=="threshold", start_thr %in% THRESHOLDS) |>
  mutate(scale=as.integer(str_extract(index,"\\d+")),
         fam_chr=case_when(str_detect(index,"_pm$")~"SPEI-P",
                           str_detect(index,"^spei")~"SPEI-T",
                           str_detect(index,"^spi")~"SPI",TRUE~index))
total_px <- length(unique(paste(ev$x, ev$y)))

# ---- path 1: SPATIAL (time-collapse) : one value per pixel -------------------
pixel_metric <- function(metric, sc) {
  sub <- ev |> filter(scale==sc)
  one <- function(y0,y1,ny,lab) {
    g <- sub |> filter(event_year>=y0, event_year<=y1) |>
      group_by(fam_chr, start_thr, x, y)
    out <- switch(metric,
      frequency = summarise(g, value=n()/ny, .groups="drop"),
      duration  = summarise(g, value=mean(duration_months,na.rm=TRUE), .groups="drop"),
      severity  = summarise(g, value=mean(peak_index,na.rm=TRUE), .groups="drop"))
    out$period <- lab; out$path <- "Spatial (per pixel)"; out
  }
  bind_rows(one(REF0,REF1,NREF,"Reference"), one(OBS0,OBS1,NOBS,"Observation"))
}

# ---- path 2: TEMPORAL (space-collapse) : one value per year ------------------
# For each year: average the characteristic over the pixels active that year,
# giving one number per year -> the box spans the ~21 (or 30) years.
yearly_metric <- function(metric, sc) {
  sub <- ev |> filter(scale==sc)
  one <- function(y0,y1,lab) {
    yr <- sub |> filter(event_year>=y0, event_year<=y1)
    out <- switch(metric,
      # frequency: events per pixel that year = # events / # pixels present
      frequency = yr |> group_by(fam_chr, start_thr, event_year) |>
        summarise(value=n()/total_px, .groups="drop"),
      duration  = yr |> group_by(fam_chr, start_thr, event_year) |>
        summarise(value=mean(duration_months,na.rm=TRUE), .groups="drop"),
      severity  = yr |> group_by(fam_chr, start_thr, event_year) |>
        summarise(value=mean(peak_index,na.rm=TRUE), .groups="drop"))
    out$period <- lab; out$path <- "Temporal (per year)"; out
  }
  bind_rows(one(REF0,REF1,"Reference"), one(OBS0,OBS1,"Observation"))
}

# both paths stacked for one metric/scale
both_paths <- function(metric, sc) {
  bind_rows(pixel_metric(metric,sc), yearly_metric(metric,sc))
}

add_aes <- function(d, sc) {
  d |> mutate(
    family=factor(fam_chr, levels=c("SPI","SPEI-T","SPEI-P")),
    fam_lab=factor(paste0(fam_chr,"-",sc),
                   levels=paste0(c("SPI","SPEI-T","SPEI-P"),"-",sc)),
    period=factor(period, levels=c("Reference","Observation")),
    path=factor(path, levels=c("Spatial (per pixel)","Temporal (per year)")),
    thr_f=factor(start_thr, levels=THRESHOLDS, labels=thr_labels),
    grp=interaction(family, period, sep=" · "),
    # unique x position: family-period box, split into two by path via dodge grp
    xpos=interaction(fam_lab, period, sep="\n"))
}

base_theme <- function() theme_bw(base_size=9) +
  theme(legend.position="bottom", legend.text=element_text(size=7),
        legend.margin=margin(0,0,0,0), legend.box.margin=margin(-2,0,0,0),
        axis.text.x=element_text(angle=0, hjust=0.5, size=6),
        panel.border=element_rect(colour="grey30",fill=NA,linewidth=0.5),
        strip.background=element_rect(colour="grey30",fill="grey92"),
        strip.text=element_text(face="bold",size=8),
        plot.title=element_blank(),
        plot.margin=margin(2,6,4,2))

# ---- box slide: two SUB-PANELS by path, INDEPENDENT y per column -------------
# Two plots side by side (patchwork) so each column carries its OWN y-scale:
#   Left  = Time-collapse (154 px)  = EXISTING view, tight -> own y shows spread.
#   Right = Space-collapse (21 yr)  = NEW, grey background, wider year-to-year.
# Each keeps original box style: fill = family x period (full colour), Ref/Obs
# dodged per family, + jitter. Thresholds stacked (facet rows). Shared legend.
suppressMessages(library(patchwork))
PATH_LAB <- c("Spatial (per pixel)"="Time-collapse (154 px)",
              "Temporal (per year)"="Space-collapse (per year)")

.one_col <- function(dd, ylab, shade=FALSE) {
  # per-box mean + median for labels (dodged by grp within family)
  lab <- dd |> group_by(thr_f, path, fam_lab, grp) |>
    summarise(med=median(value, na.rm=TRUE),
              mn=mean(value, na.rm=TRUE),
              ytop=quantile(value,0.75,na.rm=TRUE)+1.5*IQR(value,na.rm=TRUE),
              ymax=max(value,na.rm=TRUE), .groups="drop") |>
    mutate(ytxt=pmin(ytop,ymax))
  # per-family % change of the MEDIAN, Obs vs Ref, placed at panel bottom
  # (median = the number labelled on the box, so the % matches it)
  pctd <- dd |> group_by(thr_f, path, fam_lab, period) |>
    summarise(md=median(value, na.rm=TRUE), .groups="drop") |>
    tidyr::pivot_wider(names_from=period, values_from=md) |>
    mutate(pct=100*(Observation-Reference)/Reference,
           lbl=ifelse(is.finite(pct) & Reference!=0, sprintf("%+.0f%%", pct), "n/a"))
  ybot <- dd |> group_by(thr_f) |>
    summarise(ymin=min(value, na.rm=TRUE), .groups="drop")
  pctd <- pctd |> left_join(ybot, by="thr_f")
  p <- ggplot(dd, aes(x=fam_lab, y=value, fill=grp))
  if (shade) p <- p + geom_rect(xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf,
                                fill="grey88", alpha=0.5, inherit.aes=FALSE)
  p +
    geom_boxplot(outlier.shape=NA, width=0.7, colour="grey20", linewidth=0.3,
                 position=position_dodge(width=0.8)) +
    geom_point(aes(group=grp), position=position_jitterdodge(
                 jitter.width=0.12, dodge.width=0.8, seed=1),
               size=0.3, alpha=0.28, colour="grey15") +
    # mean = white diamond
    stat_summary(aes(group=grp), fun=mean, geom="point", shape=23, size=1.5,
                 fill="white", colour="black", stroke=0.3,
                 position=position_dodge(width=0.8)) +
    # median label above box (diamond above marks the mean)
    geom_label(data=lab, aes(x=fam_lab, y=ytxt, label=sprintf("%.2f",med),
                             group=grp), position=position_dodge(width=0.8),
               size=1.9, vjust=-0.35, fontface="bold", fill="white",
               alpha=0.8, label.size=0, label.padding=unit(0.4,"pt"),
               inherit.aes=FALSE) +
    # per-family % change of the mean (Obs vs Ref), centred under each family
    geom_text(data=pctd, aes(x=fam_lab, y=ymin, label=lbl),
              inherit.aes=FALSE, vjust=1.6, size=2.0, fontface="bold",
              colour="grey20") +
    facet_grid(thr_f ~ path) +
    scale_fill_manual(values=pal, name=NULL, breaks=names(pal),
                      guide=guide_legend(nrow=1)) +
    scale_y_continuous(expand=expansion(mult=c(0.12,0.10))) +
    labs(x=NULL, y=ylab) + base_theme() +
    theme(axis.text.x=element_text(size=6.5),
          strip.text.x=element_text(face="bold", size=8.5),
          strip.text.y=element_text(face="bold", size=7.5),
          panel.spacing.y=unit(2,"pt"),
          panel.grid=element_blank())
}

box_slide_agg <- function(d, ylab, title) {
  d <- d |> mutate(path=factor(PATH_LAB[as.character(path)],
                               levels=PATH_LAB))
  left  <- .one_col(filter(d, path==PATH_LAB[["Spatial (per pixel)"]]), ylab, shade=FALSE)
  right <- .one_col(filter(d, path==PATH_LAB[["Temporal (per year)"]]), NULL,  shade=TRUE)
  (left | right) +
    plot_layout(guides="collect") +
    plot_annotation(title=title,
                    theme=theme(legend.position="bottom",
                                plot.title=element_blank())) &
    theme(legend.position="bottom")
}
