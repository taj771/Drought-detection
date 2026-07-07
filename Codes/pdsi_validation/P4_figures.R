# =============================================================================
# P4. Figures for the SPI/SPEI vs sc-PDSI inter-method comparison
# =============================================================================
#   G1  correlation vs accumulation scale, one line per family (rises to 12-mo)
#   G2  spatial map of best (12-mo) correlation per family
#   G3  north/south zone boxplots at the best (12-mo) scale
#   T1  zone summary csv
# LOCAL. In: out/P3_corr_by_pixel_scale.parquet. Out: out/figs/*.png, csv.
# =============================================================================
suppressMessages({ library(arrow); library(dplyr); library(ggplot2) })
setwd("c:/Users/tajayalath/Box/R - Projects - Tharaka/Drought-indices")
OUT <- "Codes/pdsi_validation/out"; FIG <- file.path(OUT, "figs")
dir.create(FIG, showWarnings = FALSE, recursive = TRUE)
ZCUT <- 9.5
fam_lv <- c("SPI","SPEI-T","SPEI-P")
pal <- c("SPI"="#c0392b","SPEI-T"="#2e8b3d","SPEI-P"="#2c6fb5")

corr <- as.data.frame(read_parquet(file.path(OUT, "P3_corr_by_pixel_scale.parquet"))) |>
  mutate(family = factor(family, levels = fam_lv),
         zone = ifelse(y >= ZCUT, "North (Sudano-Sahelian)", "South (Guinean / Dahomey Gap)"))

# ---- G1: correlation vs scale (median over pixels) --------------------------
g1 <- corr |> filter(!is.na(r)) |> group_by(family, scale) |>
  summarise(median_r = median(r), lo = quantile(r,.25), hi = quantile(r,.75),
            .groups="drop")
pG1 <- ggplot(g1, aes(factor(scale), median_r, colour = family, group = family)) +
  geom_ribbon(aes(ymin=lo, ymax=hi, fill=family), alpha=0.12, colour=NA) +
  geom_line(linewidth=0.7) + geom_point(size=2) +
  scale_colour_manual(values=pal, name=NULL) + scale_fill_manual(values=pal, guide="none") +
  labs(title="SPI/SPEI vs sc-PDSI — correlation rises with accumulation scale",
       subtitle="Median Pearson r over 154 Benin pixels. PDSI's ~9-12 mo memory matches long scales; SPEI-Penman closest.",
       x="accumulation scale (months)", y="correlation with sc-PDSI") +
  ylim(0, NA) + theme_bw(base_size=10) + theme(legend.position="bottom")
ggsave(file.path(FIG,"P4_G1_scale_curve.png"), pG1, width=8, height=4.4, dpi=150, bg="white")

# ---- G2: best (12-mo) correlation map ---------------------------------------
best <- corr |> filter(scale==12, !is.na(r))
pG2 <- ggplot(best, aes(x, y, fill=r)) +
  geom_tile(width=0.25, height=0.25) +
  geom_hline(yintercept=ZCUT, linetype="dashed", colour="grey30") +
  facet_wrap(~family) + coord_equal() +
  scale_fill_viridis_c(option="viridis", name="r (12-mo)") +
  labs(title="SPI/SPEI-12 vs sc-PDSI agreement (per pixel)",
       subtitle="Dashed = north/south split. SPEI-Penman agrees most strongly with the PET-based PDSI.",
       x="lon", y="lat") + theme_bw(base_size=10)
ggsave(file.path(FIG,"P4_G2_best_r_map.png"), pG2, width=9, height=4.2, dpi=150, bg="white")

# ---- G3: zone boxplots at 12-mo ---------------------------------------------
pG3 <- ggplot(best, aes(family, r, fill=zone)) +
  geom_boxplot(outlier.size=0.4, width=0.6, position=position_dodge(0.7)) +
  scale_fill_manual(values=c("North (Sudano-Sahelian)"="#d8b365",
                             "South (Guinean / Dahomey Gap)"="#5ab4ac"), name=NULL) +
  labs(title="SPI/SPEI-12 vs sc-PDSI by climate zone",
       subtitle="Inter-method agreement (same climate inputs). SPEI-Penman highest; all high at 12-month.",
       x=NULL, y="correlation with sc-PDSI (12-month)") +
  theme_bw(base_size=10) + theme(legend.position="bottom")
ggsave(file.path(FIG,"P4_G3_zone_boxplots.png"), pG3, width=7, height=4.4, dpi=150, bg="white")

# ---- T1: zone summary -------------------------------------------------------
zsumm <- best |> group_by(zone, family) |>
  summarise(n_px=n(), median_r=round(median(r),3), median_rho=round(median(rho),3),
            .groups="drop") |> arrange(family, desc(zone))
write.csv(zsumm, file.path(OUT,"P4_zone_summary.csv"), row.names=FALSE)
cat("=== zone summary (12-mo, best) ===\n"); print(as.data.frame(zsumm), row.names=FALSE)
cat("\nfigures ->", FIG, "\nP4 done.\n")
