# N7 figures: drought characteristics, SVI as a 4th index beside SPI/SPEI-3,
# at matched z-thresholds. Box = spread over the 154 pixels. One figure per
# characteristic (frequency, duration, severity), faceted by threshold.
suppressMessages({ library(arrow); library(dplyr); library(tidyr); library(ggplot2) })
setwd("c:/Users/tajayalath/Box/R - Projects - Tharaka/Drought-indices")
OUT <- "Codes/ndvi_validation/out"; FIG <- file.path(OUT, "figs")
dir.create(FIG, showWarnings = FALSE, recursive = TRUE)
THR_LAB <- c("0.0"="Threshold 0.0","-0.5"="Threshold -0.5",
             "-1"="Threshold -1.0","-1.5"="Threshold -1.5")
pal <- c("SPI"="#c0392b","SPEI-T"="#2e8b3d","SPEI-P"="#2c6fb5","SVI"="#8c510a")

d <- as.data.frame(read_parquet(file.path(OUT, "N7_char_by_pixel.parquet"))) |>
  mutate(index = factor(index, levels=c("SPI","SPEI-T","SPEI-P","SVI")),
         thr_f = factor(as.character(start_thr),
                        levels=c("0","-0.5","-1","-1.5"),
                        labels=c("Threshold 0.0","Threshold -0.5",
                                 "Threshold -1.0","Threshold -1.5")))

box_char <- function(col, ylab, title, file) {
  ld <- d |> group_by(index, thr_f) |>
    summarise(med = median(.data[[col]]), .groups="drop")
  p <- ggplot(d, aes(index, .data[[col]], fill=index)) +
    geom_boxplot(outlier.size=0.25, width=0.7, colour="grey20", linewidth=0.3) +
    geom_label(data=ld, aes(index, med, label=sprintf("%.2f",med)),
               fill="white", alpha=0.75, label.size=0, size=2.2,
               label.padding=unit(0.5,"pt"), vjust=-0.4, fontface="bold") +
    facet_wrap(~thr_f, nrow=2) +
    scale_fill_manual(values=pal, name=NULL) +
    labs(title=title, x=NULL, y=ylab,
         subtitle="Spread over 154 Benin pixels, 2003-2023. Vegetation SVI vs meteorological SPI/SPEI (3-month), same thresholds.") +
    theme_bw(base_size=9) +
    theme(plot.title=element_blank(),
          strip.background=element_rect(fill="grey92", colour="grey30"),
          strip.text=element_text(face="bold", size=8),
          axis.text.x=element_text(size=8),
          legend.position="bottom", legend.margin=margin(0,0,0,0),
          plot.subtitle=element_text(size=7)) +
    guides(fill=guide_legend(nrow=1))
  # shorter height so the whole figure (incl. bottom panels + legend) fits a slide
  ggsave(file.path(FIG, file), p, width=9, height=4.2, dpi=150, bg="white")
}

box_char("freq",     "Frequency (events/pixel/yr)",        "Freq",     "N7_freq.png")
box_char("mean_dur", "Duration (months)",                  "Duration", "N7_duration.png")
box_char("mean_peak","Peak severity (index; more negative = worse)", "Severity", "N7_severity.png")
cat("N7 figures written to", FIG, "\n")
