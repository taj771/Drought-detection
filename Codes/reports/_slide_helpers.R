# Data + figure helpers for the slide deck (sourced by the qmd).
suppressMessages({ library(arrow); library(dplyr); library(tidyr); library(ggplot2); library(stringr) })

EVENTS_PATH <- "../_archive/_data/events.parquet"
THRESHOLDS  <- c(0.0,-0.5,-1.0,-1.5)
REF0<-1973;REF1<-2002;NREF<-REF1-REF0+1
OBS0<-2003;OBS1<-2023;NOBS<-OBS1-OBS0+1
thr_labels <- c("Threshold 0.0","Threshold -0.5","Threshold -1.0","Threshold -1.5")
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

pixel_metric <- function(metric, sc) {
  sub <- ev |> filter(scale==sc)
  one <- function(y0,y1,ny,lab) {
    g <- sub |> filter(event_year>=y0, event_year<=y1) |>
      group_by(fam_chr, start_thr, x, y)
    out <- switch(metric,
      frequency = summarise(g, value=n()/ny, .groups="drop"),
      duration  = summarise(g, value=mean(duration_months,na.rm=TRUE), .groups="drop"),
      severity  = summarise(g, value=mean(peak_index,na.rm=TRUE), .groups="drop"))  # keep negative (driest point)
    out$period <- lab; out
  }
  bind_rows(one(REF0,REF1,NREF,"Reference"), one(OBS0,OBS1,NOBS,"Observation"))
}

pct_metric <- function(sc) {
  sub <- ev |> filter(scale==sc)
  one <- function(y0,y1,lab) sub |> filter(event_year>=y0,event_year<=y1) |>
    group_by(fam_chr,start_thr) |>
    summarise(value=100*n_distinct(paste(x,y))/total_px, .groups="drop") |>
    mutate(period=lab)
  bind_rows(one(REF0,REF1,"Reference"), one(OBS0,OBS1,"Observation"))
}

add_aes <- function(d, sc) {
  d |> mutate(
    family=factor(fam_chr, levels=c("SPI","SPEI-T","SPEI-P")),
    fam_lab=factor(paste0(fam_chr,"-",sc),
                   levels=paste0(c("SPI","SPEI-T","SPEI-P"),"-",sc)),
    period=factor(period, levels=c("Reference","Observation")),
    thr_f=factor(start_thr, levels=THRESHOLDS, labels=thr_labels),
    grp=interaction(family, period, sep=" · "))
}

base_theme <- function() theme_bw(base_size=9) +
  theme(legend.position="bottom", legend.text=element_text(size=7),
        legend.margin=margin(0,0,0,0), legend.box.margin=margin(-2,0,0,0),
        axis.text.x=element_text(angle=0, hjust=0.5, size=7),  # horizontal, no clip
        panel.border=element_rect(colour="grey30",fill=NA,linewidth=0.5),
        strip.background=element_rect(colour="grey30",fill="grey92"),
        strip.text=element_text(face="bold",size=8),
        plot.title=element_blank(),                 # title is the slide header
        plot.margin=margin(2,6,4,2))                # extra bottom margin

box_slide <- function(d, ylab, title) {
  ld <- d |> group_by(fam_lab,thr_f,period,grp) |>
    summarise(med=median(value),
              ytop=quantile(value,0.75)+1.5*IQR(value), ymax=max(value),
              .groups="drop") |> mutate(ylab=pmin(ytop,ymax))
  ggplot(d, aes(x=fam_lab, y=value, fill=grp)) +
    geom_boxplot(outlier.size=0.25, width=0.7, colour="grey20", linewidth=0.3,
                 position=position_dodge(width=0.8)) +
    geom_label(data=ld, aes(x=fam_lab, y=ylab, label=sprintf("%.2f",med), group=grp),
               position=position_dodge(width=0.8), size=2.1, vjust=-0.3,
               fontface="bold", fill="white", alpha=0.75, label.size=0,
               label.padding=unit(0.5,"pt")) +
    facet_wrap(~thr_f, nrow=2) +
    scale_fill_manual(values=pal, name=NULL, breaks=names(pal)) +
    scale_y_continuous(expand=expansion(mult=c(0.02,0.13))) +
    labs(title=title, x=NULL, y=ylab) + base_theme() +
    guides(fill=guide_legend(nrow=1))
}

bar_slide <- function(d, ylab, title) {
  ggplot(d, aes(x=fam_lab, y=value, fill=grp)) +
    geom_col(colour="grey20", linewidth=0.3, width=0.7,
             position=position_dodge(width=0.8)) +
    geom_label(aes(label=sprintf("%.0f",value), group=grp),
               position=position_dodge(width=0.8), size=2.1, vjust=-0.2,
               fontface="bold", fill="white", alpha=0.75, label.size=0,
               label.padding=unit(0.5,"pt")) +
    facet_wrap(~thr_f, nrow=2) +
    scale_fill_manual(values=pal, name=NULL, breaks=names(pal)) +
    scale_y_continuous(expand=expansion(mult=c(0.02,0.13))) +
    labs(title=title, x=NULL, y=ylab) + base_theme() +
    guides(fill=guide_legend(nrow=1))
}

# --- %-area-affected as a time series (replaces the misleading static bar) ----
# Annual % of pixels in drought. One slide = one threshold; facet grid rows =
# index (SPI/SPEI-T/SPEI-P), cols = accumulation scale (1/3/6/12-month).
# Observation period shaded grey. Line only (no fill) -- keeps the four columns
# legible. The ref/obs split is the shaded rectangle.
YR0<-1973; YR1<-2023
PCT_SCALES <- c(1,3,6,12)
line_pal <- c("SPI"="#c0392b","SPEI-T"="#2e8b3d","SPEI-P"="#2c6fb5")

pct_ts <- function(thr) {
  ev |> filter(start_thr==thr, scale %in% PCT_SCALES,
               event_year>=YR0, event_year<=YR1) |>
    mutate(fam=factor(fam_chr, levels=c("SPI","SPEI-T","SPEI-P"))) |>
    group_by(fam, scale, event_year) |>
    summarise(pct=100*n_distinct(paste(x,y))/total_px, .groups="drop") |>
    tidyr::complete(fam, scale, event_year=YR0:YR1, fill=list(pct=0)) |>
    mutate(scale_lab=factor(paste0(scale,"-month"),
                            levels=paste0(PCT_SCALES,"-month")))
}

pct_ts_slide <- function(d, ylab, title) {
  ggplot(d, aes(event_year, pct, colour=fam)) +
    annotate("rect", xmin=OBS0-0.5, xmax=OBS1+0.5, ymin=-Inf, ymax=Inf,
             fill="grey88", alpha=0.6) +
    geom_line(linewidth=0.4) +
    facet_grid(fam ~ scale_lab, switch="y") +
    scale_colour_manual(values=line_pal, guide="none") +
    scale_y_continuous(limits=c(0,100), breaks=c(0,50,100)) +
    labs(title=title, x=NULL, y=ylab) + base_theme() +
    theme(strip.text.y.left=element_text(angle=0, face="bold"),
          strip.text.x=element_text(face="bold"),
          panel.grid.minor=element_blank(), panel.spacing=unit(3,"pt"),
          legend.position="none")
}
