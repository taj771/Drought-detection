# =============================================================================
# N5. Stratify (north/south) + figures for SVI-vs-SPI/SPEI value correlation
# =============================================================================
# Literature (Vicente-Serrano 2013; Oueme-Benin 2025) says SPI/SPEI-vs-NDVI
# consistency is biome/aridity-dependent — strong in Sudano-Sahelian north,
# weaker in humid south (Dahomey Gap). So we report correlation PER ZONE, not
# one national number, and make the standard figures:
#   F1 scale x lag heatmap of median r per family
#   F2 best-r map (154 px) — spatial pattern of validation strength
#   F3 north vs south boxplots of best-pixel r per family
#   T1 summary table: median r/rho by zone x family (best scale/lag)
#
# LOCAL run. In: out/N4_corr_by_pixel_scale_lag.parquet, N4_best_per_pixel.parquet
# Out: out/figs/*.png, out/N5_zone_summary.csv
# =============================================================================
suppressMessages({ library(arrow); library(dplyr); library(tidyr); library(ggplot2) })
setwd("c:/Users/tajayalath/Box/R - Projects - Tharaka/Drought-indices")
OUT <- "Codes/ndvi_validation/out"; FIG <- file.path(OUT, "figs")
dir.create(FIG, showWarnings = FALSE, recursive = TRUE)

ZCUT <- 9.5                            # lat split: North >= 9.5, South < 9.5
zone <- function(y) ifelse(y >= ZCUT, "North (Sudano-Sahelian)",
                                      "South (Guinean / Dahomey Gap)")
fam_lv <- c("SPI", "SPEI-T", "SPEI-P")

corr <- as.data.frame(read_parquet(file.path(OUT, "N4_corr_by_pixel_scale_lag.parquet"))) |>
  mutate(family = factor(family, levels = fam_lv), zone = zone(y))
best <- as.data.frame(read_parquet(file.path(OUT, "N4_best_per_pixel.parquet"))) |>
  mutate(family = factor(family, levels = fam_lv), zone = zone(y))

# ---- F1: scale x lag heatmap of median r per family -------------------------
h <- corr |> filter(!is.na(r)) |>
  group_by(family, scale, lag) |>
  summarise(median_r = median(r), .groups = "drop")
pF1 <- ggplot(h, aes(factor(lag), factor(scale), fill = median_r)) +
  geom_tile(colour = "white") +
  geom_text(aes(label = sprintf("%.2f", median_r)), size = 2.8) +
  facet_wrap(~family) +
  scale_fill_gradient2(low = "#b2182b", mid = "#f7f7f7", high = "#2166ac",
                       midpoint = 0, name = "median r") +
  labs(title = "SVI vs SPI/SPEI — median Pearson r by accumulation scale x lag",
       subtitle = "Across 154 Benin pixels, 2003-2023. Higher = better match.",
       x = "lag (months, index leads vegetation)", y = "accumulation scale (months)") +
  theme_bw(base_size = 10) + theme(panel.grid = element_blank())
ggsave(file.path(FIG, "N5_F1_scale_lag_heatmap.png"), pF1, width = 9, height = 3.6,
       dpi = 150, bg = "white")

# ---- F2: best-r map (spatial pattern of validation strength) ----------------
pF2 <- ggplot(best, aes(x, y, fill = r)) +
  geom_tile(width = 0.25, height = 0.25) +
  geom_hline(yintercept = ZCUT, linetype = "dashed", colour = "grey40") +
  facet_wrap(~family) + coord_equal() +
  scale_fill_viridis_c(option = "viridis", name = "best r", limits = c(0, NA)) +
  labs(title = "Spatial pattern of SVI-vs-index agreement (best scale/lag per pixel)",
       subtitle = "Dashed line = north/south split (lat 9.5). Brighter = stronger match.",
       x = "lon", y = "lat") +
  theme_bw(base_size = 10)
ggsave(file.path(FIG, "N5_F2_best_r_map.png"), pF2, width = 9, height = 4.2,
       dpi = 150, bg = "white")

# ---- F3: north vs south boxplots of best-pixel r ----------------------------
pF3 <- ggplot(best, aes(family, r, fill = zone)) +
  geom_boxplot(outlier.size = 0.4, width = 0.6,
               position = position_dodge(0.7)) +
  scale_fill_manual(values = c("North (Sudano-Sahelian)" = "#d8b365",
                               "South (Guinean / Dahomey Gap)" = "#5ab4ac"),
                    name = NULL) +
  labs(title = "SVI-vs-index correlation by climate zone (best scale/lag per pixel)",
       subtitle = "North (drier savanna) vs South (humid Dahomey Gap). Lit predicts north stronger.",
       x = NULL, y = "best Pearson r per pixel") +
  theme_bw(base_size = 10) + theme(legend.position = "bottom")
ggsave(file.path(FIG, "N5_F3_zone_boxplots.png"), pF3, width = 7, height = 4.4,
       dpi = 150, bg = "white")

# ---- T1: zone summary table -------------------------------------------------
# median best r + rho per zone x family, and the modal best scale/lag
zsumm <- best |>
  group_by(zone, family) |>
  summarise(n_px = n(),
            median_best_r   = round(median(r), 3),
            median_best_rho = round(median(rho), 3),
            modal_scale = as.integer(names(sort(table(scale), decreasing = TRUE))[1]),
            modal_lag   = as.integer(names(sort(table(lag),   decreasing = TRUE))[1]),
            .groups = "drop") |>
  arrange(family, desc(zone))
write.csv(zsumm, file.path(OUT, "N5_zone_summary.csv"), row.names = FALSE)

cat("=== N5 zone summary (best-per-pixel r by zone x family) ===\n")
print(as.data.frame(zsumm), row.names = FALSE)
cat("\nfigures ->", FIG, "\nN5 done.\n")
