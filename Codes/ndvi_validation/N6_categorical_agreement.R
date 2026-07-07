# =============================================================================
# N6. Categorical drought-class AGREEMENT — SVI vs SPI/SPEI (secondary check)
# =============================================================================
# The ONE frequency-style comparison the literature sanctions (as a SECONDARY
# check, e.g. Ta et al. 2022 Sahel): classify each pixel-month drought/no-drought
# from BOTH the vegetation index (SVI) and each meteorological index at the SAME
# threshold, then report contingency statistics — NOT raw event counts.
#
# Per pixel, over 2003-2023 monthly, using the best-matching 3-month scale, lag 0:
#   drought if index < THR (=-1.0).  Build 2x2 contingency:
#     hit  = both dry;  miss = met dry, veg wet;  false = met wet, veg dry;  cn = both wet
#   % agreement = (hit+cn)/n
#   POD (hit rate) = hit/(hit+miss)     -- when the met index flags drought, does veg agree
#   FAR            = false/(hit+false)   -- met flags drought but veg does not
#   Cohen's kappa  = chance-corrected agreement
#
# LOCAL. In: svi_benin_monthly.parquet, spi/speit/spei_penman_results.parquet
# Out: out/N6_agreement_by_pixel.parquet, out/N6_agreement_summary.csv,
#      out/figs/N6_agreement_map.png
# =============================================================================
suppressMessages({ library(arrow); library(dplyr); library(tidyr); library(ggplot2) })
setwd("c:/Users/tajayalath/Box/R - Projects - Tharaka/Drought-indices")
DAT <- "Codes/_archive/_data"; OUT <- "Codes/ndvi_validation/out"
FIG <- file.path(OUT, "figs"); dir.create(FIG, showWarnings = FALSE, recursive = TRUE)

THR  <- -1.0                # WMO-style moderate-drought threshold (both sides)
SC   <- 3                   # best accumulation from N4 (3-month)
LAGS <- 0:3                 # met index leads vegetation by 0-3 months
OBS0 <- 2003; OBS1 <- 2023
MIN_N <- 24
ZCUT <- 9.5
mi <- function(y, m) (y - 1950) * 12 + m   # monotone month index for lagging

svi <- as.data.frame(read_parquet(file.path(DAT, "svi_benin_monthly.parquet"))) |>
  filter(year >= OBS0 - 1, year <= OBS1) |>   # keep -1 yr so lagged met has a match
  transmute(x, y, t = mi(year, month), svi_dry = svi < THR)

# each met index at the 3-month scale, tagged by family
load_met <- function(file, col, fam) {
  as.data.frame(read_parquet(file.path(DAT, file))) |>
    filter(year >= OBS0 - 1, year <= OBS1) |>
    transmute(x, y, t = mi(year, month), met_dry = .data[[col]] < THR, family = fam)
}
met <- bind_rows(
  load_met("spi_results.parquet",         "spi3",  "SPI"),
  load_met("speit.parquet",               "spei3", "SPEI-T"),
  load_met("spei_penman_results.parquet", "spei3", "SPEI-P"))

# contingency per pixel x family x lag: shift met FORWARD by lag so met[t-lag]
# (the earlier met state) aligns with veg[t] (vegetation responds later).
one_lag <- function(lag) {
  m <- met |> mutate(t = t + lag)                     # met leads veg by `lag`
  inner_join(m, svi, by = c("x", "y", "t")) |>
    filter(!is.na(svi_dry), !is.na(met_dry)) |>
    group_by(x, y, family) |>
    summarise(n = n(),
              hit   = sum(met_dry & svi_dry),
              miss  = sum(met_dry & !svi_dry),
              false = sum(!met_dry & svi_dry),
              cn    = sum(!met_dry & !svi_dry),
              .groups = "drop") |>
    mutate(lag = lag)
}
allw <- bind_rows(lapply(LAGS, one_lag)) |>
  filter(n >= MIN_N) |>
  mutate(pct_agree = 100 * (hit + cn) / n,
         POD = ifelse((hit + miss) > 0, hit / (hit + miss), NA_real_),
         FAR = ifelse((hit + false) > 0, false / (hit + false), NA_real_),
         po  = (hit + cn) / n,
         pe  = ((hit+miss)/n)*((hit+false)/n) + ((false+cn)/n)*((miss+cn)/n),
         kappa = ifelse(pe < 1, (po - pe) / (1 - pe), NA_real_))
write_parquet(allw, file.path(OUT, "N6_agreement_by_pixel_lag.parquet"))

# best lag per pixel x family = max kappa (chance-corrected agreement)
agr <- allw |> filter(!is.na(kappa)) |>
  group_by(x, y, family) |>
  slice_max(kappa, n = 1, with_ties = FALSE) |> ungroup() |>
  mutate(family = factor(family, levels = c("SPI","SPEI-T","SPEI-P")),
         zone = ifelse(y >= ZCUT, "North", "South"))
write_parquet(agr, file.path(OUT, "N6_agreement_by_pixel.parquet"))

cat("=== best lag chosen per pixel (by kappa) — lag distribution ===\n")
print(table(agr$family, agr$lag))

# summary by family (and by zone)
summ <- agr |> group_by(family) |>
  summarise(median_pct_agree = round(median(pct_agree), 1),
            median_POD = round(median(POD, na.rm=TRUE), 2),
            median_FAR = round(median(FAR, na.rm=TRUE), 2),
            median_kappa = round(median(kappa, na.rm=TRUE), 2),
            .groups = "drop")
zsumm <- agr |> group_by(family, zone) |>
  summarise(median_pct_agree = round(median(pct_agree),1),
            median_POD = round(median(POD, na.rm=TRUE),2),
            median_kappa = round(median(kappa, na.rm=TRUE),2),
            .groups = "drop")
write.csv(summ,  file.path(OUT, "N6_agreement_summary.csv"), row.names = FALSE)
write.csv(zsumm, file.path(OUT, "N6_agreement_zone_summary.csv"), row.names = FALSE)

cat("=== N6 categorical agreement (SVI vs index, 3-mo, thr -1.0) ===\n")
print(as.data.frame(summ), row.names = FALSE)
cat("\n--- by zone ---\n"); print(as.data.frame(zsumm), row.names = FALSE)

# ---- map: % agreement per pixel per family ----------------------------------
pMap <- ggplot(agr, aes(x, y, fill = pct_agree)) +
  geom_tile(width = 0.25, height = 0.25) +
  geom_hline(yintercept = ZCUT, linetype = "dashed", colour = "grey30") +
  facet_wrap(~family) + coord_equal() +
  scale_fill_viridis_c(option = "mako", direction = -1, name = "% agree") +
  labs(title = "Categorical drought agreement: SVI vs SPI/SPEI (3-month, threshold -1.0)",
       subtitle = "% of months both agree drought/no-drought. Dashed = north/south split.",
       x = "lon", y = "lat") +
  theme_bw(base_size = 10)
ggsave(file.path(FIG, "N6_agreement_map.png"), pMap, width = 9, height = 4.2,
       dpi = 150, bg = "white")
cat("\nmap ->", file.path(FIG, "N6_agreement_map.png"), "\nN6 done.\n")
