# Appendix time series: SPEI-T-12 for the extreme pixel, highlighting the single
# 193-month event (2004-06 to 2020-06) distinctly from the other obs-period events.
suppressMessages({ library(arrow); library(dplyr); library(ggplot2) })
d <- as.data.frame(read_parquet("Codes/_archive/_data/speit.parquet"))

PX <- 0.875; PY <- 10.375
EV0 <- as.Date("2004-06-01"); EV1 <- as.Date("2020-06-01")   # the 193-mo event

ts <- d |> filter(abs(x-PX)<0.01, abs(y-PY)<0.01) |>
  arrange(year, month) |>
  mutate(date = as.Date(sprintf("%04d-%02d-01", year, month)),
         in_event = date>=EV0 & date<=EV1)

yhi <- max(ts$spei12, na.rm=TRUE)

p <- ggplot(ts, aes(x=date, y=spei12)) +
  # highlight band = the single 193-month event
  annotate("rect", xmin=EV0, xmax=EV1, ymin=-Inf, ymax=Inf,
           fill="#f5c518", alpha=0.22) +
  # drought shading (below 0)
  geom_area(aes(y=pmin(spei12,0)), fill="#c0392b", alpha=0.32) +
  geom_line(colour="grey20", linewidth=0.35) +
  geom_hline(yintercept=0, colour="black", linewidth=0.4) +
  geom_hline(yintercept=c(-1,-1.5), colour="grey55", linetype="dashed", linewidth=0.3) +
  geom_vline(xintercept=as.Date("2003-01-01"), colour="steelblue",
             linetype="dotted", linewidth=0.5) +
  # bracket + label for the event
  annotate("segment", x=EV0, xend=EV1, y=yhi*0.98, yend=yhi*0.98,
           colour="#b8860b", linewidth=0.6) +
  annotate("text", x=EV0+(EV1-EV0)/2, y=yhi*1.12,
           label="Single 193-month event (2004-2020)", size=3, colour="#8a6d00", fontface="bold") +
  annotate("text", x=as.Date("1988-01-01"), y=yhi*1.12, label="Reference 1973-2002",
           size=3, colour="grey40") +
  labs(title="SPEI-Thornthwaite-12 at pixel (0.88, 10.38), Benin",
       subtitle="Highlighted band = the single continuous 193-month event; smaller separate events occur after 2020",
       x=NULL, y="SPEI-T-12 (index)") +
  scale_y_continuous(expand=expansion(mult=c(0.03,0.18))) +
  theme_bw(base_size=10)

ggsave("Codes/reports/appendix_ts_example.png", p, width=10, height=4.6, dpi=150, bg="white")
cat("done\n")
