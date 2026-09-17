# Example sc-PDSI time series for the deck: shows the physical water-balance
# index swinging wet/dry, with the ref/obs split, to explain what PDSI is and
# why the range is wide in this water-limited climate.
suppressMessages({ library(arrow); library(dplyr); library(ggplot2) })
setwd("c:/Users/tajayalath/Box/R - Projects - Tharaka/Drought-indices")
p <- as.data.frame(read_parquet("Codes/pdsi_validation/out/pdsi_benin_monthly.parquet"))
# a mid-country pixel
px <- p |> distinct(x,y) |> arrange(y) |> slice(80)
s <- p |> filter(x==px$x, y==px$y) |> arrange(year, month) |>
  mutate(date = as.Date(sprintf("%d-%02d-01", year, month)))
pl <- ggplot(s, aes(date, pdsi)) +
  annotate("rect", xmin=as.Date("2003-01-01"), xmax=as.Date("2023-12-31"),
           ymin=-Inf, ymax=Inf, fill="grey88", alpha=0.6) +
  geom_hline(yintercept=0, colour="grey50") +
  geom_hline(yintercept=c(-2,2), linetype="dashed", colour="grey60", linewidth=0.3) +
  geom_line(linewidth=0.35, colour="#8c510a") +
  labs(title=sprintf("Example sc-PDSI (pixel %.3f E, %.3f N)", px$x, px$y),
       subtitle="Physical soil-water-balance index. Grey = Observation (2003-2023). Dashed = +/-2 (moderate).",
       x=NULL, y="sc-PDSI") +
  theme_bw(base_size=10)
ggsave("Codes/pdsi_validation/deck/pdsi_example_ts.png", pl, width=9, height=3.4, dpi=150, bg="white")
cat("done\n")
