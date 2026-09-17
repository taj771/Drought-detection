# %-pixels-affected as time series, FACETED per index (SPI/SPEI-T/SPEI-P
# each its own panel, not overlaid). No bars. One figure = one threshold, all
# scales overlaid as line colour? No -> user wants index separated. Show 6-month
# here as the sketch; scales handled later. Ref/obs shading kept.
suppressMessages({ library(arrow); library(dplyr); library(tidyr); library(ggplot2); library(stringr) })
ev <- as.data.frame(read_parquet("Codes/_archive/_data/events.parquet")) |>
  mutate(scale=as.integer(str_extract(index,"\\d+")),
         fam=case_when(str_detect(index,"_pm$")~"SPEI-P",
                       str_detect(index,"^spei")~"SPEI-T",
                       str_detect(index,"^spi")~"SPI",TRUE~index),
         fam=factor(fam,levels=c("SPI","SPEI-T","SPEI-P")))
total_px <- length(unique(paste(ev$x, ev$y)))

sub <- ev |> filter(method=="threshold", scale==6, start_thr==-1.0,
                    event_year>=1973, event_year<=2023)
yr <- sub |> group_by(fam, event_year) |>
  summarise(pct = 100*n_distinct(paste(x,y))/total_px, .groups="drop") |>
  complete(fam, event_year=1973:2023, fill=list(pct=0))

pal <- c("SPI"="#c0392b","SPEI-T"="#2e8b3d","SPEI-P"="#2c6fb5")

p <- ggplot(yr, aes(event_year, pct, colour=fam, fill=fam)) +
  annotate("rect", xmin=2002.5, xmax=2023.5, ymin=-Inf, ymax=Inf,
           fill="grey88", alpha=0.6) +
  geom_area(alpha=0.25, colour=NA) +
  geom_line(linewidth=0.5) +
  facet_wrap(~fam, ncol=1, strip.position="right") +
  scale_colour_manual(values=pal, guide="none") +
  scale_fill_manual(values=pal, guide="none") +
  scale_y_continuous(limits=c(0,100), breaks=c(0,50,100)) +
  labs(title="Pixels affected (%) over time - 6-month, threshold -1.0",
       subtitle="Each panel = one index. Grey = Observation period (2003-2023).",
       x=NULL, y="% pixels in drought") +
  theme_bw(base_size=10) +
  theme(strip.text.y=element_text(angle=0, face="bold"),
        panel.grid.minor=element_blank())
ggsave("Codes/_archive/_pctC.png", p, width=10, height=5, dpi=150, bg="white")
cat("done\n")
