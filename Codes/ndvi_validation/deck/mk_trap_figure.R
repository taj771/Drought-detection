# Teaching figure for the deck: why the naive "count NDVI events, compare to SPI
# event count across pixels" comparison fails, and the correct SVI value-corr.
# LEFT: naive event-freq Spearman (N3) — scattered, near-zero/negative, no signal.
# RIGHT: SVI value correlation (N4) — coherent positive, 3-month peak.
suppressMessages({ library(arrow); library(dplyr); library(ggplot2); library(tidyr) })
setwd("c:/Users/tajayalath/Box/R - Projects - Tharaka/Drought-indices")
OUT <- "Codes/ndvi_validation/deck"

# LEFT — naive event-freq Spearman (from N3 output)
n3 <- read.csv("Codes/ndvi_validation/out/N3_spearman_ndvi_vs_indices.csv")
n3$fam <- ifelse(grepl("_pm$", n3$index), "SPEI-P",
          ifelse(grepl("^spei", n3$index), "SPEI-T", "SPI"))
pL <- ggplot(n3, aes(reorder(index, spearman_r), spearman_r, fill = spearman_r > 0)) +
  geom_col(colour = "grey30", linewidth = 0.3) +
  geom_hline(yintercept = 0, colour = "grey20") +
  scale_fill_manual(values = c(`TRUE` = "#4d9221", `FALSE` = "#c51b7d"),
                    guide = "none") +
  coord_flip() +
  labs(title = "NAIVE: count NDVI events, compare across pixels",
       subtitle = "Event-frequency Spearman vs SPI/SPEI (thr -1.0). Scattered, several negative.",
       x = NULL, y = "Spearman r (freq across pixels)") +
  theme_bw(base_size = 9)

# RIGHT — correct SVI value correlation (from N4), best lag per scale
n4 <- as.data.frame(read_parquet(
        "Codes/ndvi_validation/out/N4_corr_by_pixel_scale_lag.parquet")) |>
  filter(lag == 0, !is.na(r)) |>
  group_by(family, scale) |>
  summarise(median_r = median(r), .groups = "drop") |>
  mutate(family = factor(family, levels = c("SPI","SPEI-T","SPEI-P")))
pR <- ggplot(n4, aes(factor(scale), median_r, colour = family, group = family)) +
  geom_line(linewidth = 0.7) + geom_point(size = 2) +
  scale_colour_manual(values = c("SPI"="#c0392b","SPEI-T"="#2e8b3d","SPEI-P"="#2c6fb5"),
                      name = NULL) +
  labs(title = "CORRECT: standardize NDVI (SVI), correlate values over time",
       subtitle = "Per-pixel Pearson r, lag 0. Coherent, positive, peaks at 3 months.",
       x = "accumulation scale (months)", y = "median r across pixels") +
  ylim(0, NA) + theme_bw(base_size = 9) + theme(legend.position = "bottom")

ggsave(file.path(OUT, "trap_naive.png"),   pL, width = 5.2, height = 3.4, dpi = 150, bg = "white")
ggsave(file.path(OUT, "trap_correct.png"), pR, width = 5.2, height = 3.4, dpi = 150, bg = "white")
cat("trap figures written\n")
