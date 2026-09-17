# Prototype two ways to show %-pixels-affected over time (SPI/SPEI-T/SPEI-P,
# threshold -1.0, 6-month). (A) annual line + widespread-year bars, (B) heatmap.
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

# annual % pixels affected per index
yr <- sub |> group_by(fam, event_year) |>
  summarise(pct = 100*n_distinct(paste(x,y))/total_px, .groups="drop") |>
  complete(fam, event_year=1973:2023, fill=list(pct=0))

pal <- c("SPI"="#c0392b","SPEI-T"="#2e8b3d","SPEI-P"="#2c6fb5")

# ---- (A) line + widespread-year bars -----------------------------------------
WIDE <- 50   # "widespread" = >50% of country in drought that year
bars <- yr |> filter(pct>WIDE) |> mutate(y=-6 - (as.integer(fam)-1)*4)  # bar rows

pA <- ggplot(yr, aes(event_year, pct, colour=fam)) +
  annotate("rect", xmin=2003, xmax=2023, ymin=-Inf, ymax=Inf, fill="grey90", alpha=0.5) +
  geom_line(linewidth=0.5) +
  geom_hline(yintercept=WIDE, linetype="dashed", colour="grey50", linewidth=0.3) +
  # widespread-year bars below axis, one row per index
  geom_tile(data=bars, aes(x=event_year, y=y, fill=fam), height=3, colour=NA, inherit.aes=FALSE) +
  scale_colour_manual(values=pal, name=NULL) +
  scale_fill_manual(values=pal, guide="none") +
  annotate("text", x=1988, y=105, label="Reference", size=3, colour="grey45") +
  annotate("text", x=2013, y=105, label="Observation", size=3, colour="grey30") +
  labs(title="Pixels affected (%) over time - SPI/SPEI 6-month, threshold -1.0",
       subtitle="Line = % of Benin in drought each year. Bars below = years with >50% affected.",
       x=NULL, y="% pixels affected") +
  theme_bw(base_size=10) + theme(legend.position="top")
ggsave("Codes/_archive/_pctA.png", pA, width=10, height=5, dpi=150, bg="white")

# ---- (B) heatmap strip -------------------------------------------------------
pB <- ggplot(yr, aes(event_year, fam, fill=pct)) +
  geom_tile(colour="white", linewidth=0.2) +
  geom_vline(xintercept=2002.5, colour="black", linewidth=0.5) +
  scale_fill_viridis_c(option="inferno", direction=-1, name="% area", limits=c(0,100)) +
  labs(title="Pixels affected (%) over time - heatmap - 6-month, threshold -1.0",
       subtitle="Colour = % of Benin in drought each year. Vertical line = ref/obs split.",
       x=NULL, y=NULL) +
  theme_minimal(base_size=10) + theme(legend.position="right")
ggsave("Codes/_archive/_pctB.png", pB, width=10, height=3.2, dpi=150, bg="white")
cat("done\n")
