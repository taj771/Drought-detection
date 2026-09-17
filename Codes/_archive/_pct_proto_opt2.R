# Option 2: %-area time series, fixed threshold -1.0, vary SCALE (1/3/6/12).
# Facet grid: rows = index family, cols = accumulation scale. One slide = one
# threshold, shows all four scales.
suppressMessages({ library(arrow); library(dplyr); library(tidyr); library(ggplot2); library(stringr) })
ev <- as.data.frame(read_parquet("Codes/_archive/_data/events.parquet")) |>
  mutate(scale=as.integer(str_extract(index,"\\d+")),
         fam=case_when(str_detect(index,"_pm$")~"SPEI-P",
                       str_detect(index,"^spei")~"SPEI-T",
                       str_detect(index,"^spi")~"SPI",TRUE~index),
         fam=factor(fam,levels=c("SPI","SPEI-T","SPEI-P")))
total_px <- length(unique(paste(ev$x, ev$y)))

sub <- ev |> filter(method=="threshold", start_thr==-1.0,
                    scale %in% c(1,3,6,12),
                    event_year>=1973, event_year<=2023)
yr <- sub |> group_by(fam, scale, event_year) |>
  summarise(pct=100*n_distinct(paste(x,y))/total_px, .groups="drop") |>
  complete(fam, scale, event_year=1973:2023, fill=list(pct=0)) |>
  mutate(scale_lab=factor(paste0(scale,"-month"),
                          levels=paste0(c(1,3,6,12),"-month")))

pal <- c("SPI"="#c0392b","SPEI-T"="#2e8b3d","SPEI-P"="#2c6fb5")

p <- ggplot(yr, aes(event_year, pct, colour=fam, fill=fam)) +
  annotate("rect", xmin=2002.5, xmax=2023.5, ymin=-Inf, ymax=Inf,
           fill="grey88", alpha=0.6) +
  geom_line(linewidth=0.4) +
  facet_grid(fam ~ scale_lab, switch="y") +
  scale_colour_manual(values=pal, guide="none") +
  scale_fill_manual(values=pal, guide="none") +
  scale_y_continuous(limits=c(0,100), breaks=c(0,50,100)) +
  labs(title="Pixels affected (%) over time - threshold -1.0",
       subtitle="Rows = index. Cols = accumulation scale. Grey = Observation (2003-2023).",
       x=NULL, y="% pixels in drought") +
  theme_bw(base_size=9) +
  theme(strip.text.y.left=element_text(angle=0, face="bold"),
        strip.text.x=element_text(face="bold"),
        panel.grid.minor=element_blank(),
        panel.spacing=unit(3,"pt"))
ggsave("Codes/_archive/_pct_opt2.png", p, width=10, height=5, dpi=150, bg="white")
cat("done\n")
