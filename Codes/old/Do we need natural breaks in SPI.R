# ============================================================
# Locally calibrated SPI thresholds
# For each pixel find the empirical percentile-based threshold
# ============================================================

library(dplyr)
library(ggplot2)
library(tidyr)

# ── Step 1: Choose your target percentile ───────────────────
# This is the key decision — what percentile defines drought onset?
# Common choices in literature:
#   20th percentile → mild drought onset
#   15th percentile → moderate drought onset
#   10th percentile → severe drought onset

TARGET_PERCENTILE <- 0.20   # 20th percentile = driest 20% of months

# ── Step 2: Calculate empirical threshold per pixel per scale ──

local_thresholds <- pixel_data %>%
  group_by(x, y) %>%
  summarise(
    
    # Empirical threshold for each SPI scale
    threshold_spi3  = quantile(spi3,  TARGET_PERCENTILE,
                               na.rm = TRUE),
    threshold_spi6  = quantile(spi6,  TARGET_PERCENTILE,
                               na.rm = TRUE),
    threshold_spi12 = quantile(spi12, TARGET_PERCENTILE,
                               na.rm = TRUE),
    
    # Also calculate mean and SD to check normality
    mean_spi3  = mean(spi3,  na.rm = TRUE),
    sd_spi3    = sd(spi3,    na.rm = TRUE),
    mean_spi6  = mean(spi6,  na.rm = TRUE),
    sd_spi6    = sd(spi6,    na.rm = TRUE),
    
    # How far is empirical threshold from theoretical?
    # If SPI were perfectly normal the 20th percentile = -0.842
    # Deviation from this tells you how non-normal each pixel is
    deviation_spi3 = quantile(spi3, TARGET_PERCENTILE,
                              na.rm = TRUE) - (-0.842),
    deviation_spi6 = quantile(spi6, TARGET_PERCENTILE,
                              na.rm = TRUE) - (-0.842),
    
    .groups = "drop"
  )

cat("=== LOCAL THRESHOLD SUMMARY ===\n\n")

cat("Theoretical 20th percentile (perfect normal): -0.842\n\n")

cat("SPI-3 empirical threshold:\n")
print(summary(local_thresholds$threshold_spi3))

cat("\nSPI-6 empirical threshold:\n")
print(summary(local_thresholds$threshold_spi6))

cat("\nDeviation from theoretical (SPI-3):\n")
print(summary(local_thresholds$deviation_spi3))

# ── Step 3: Map the spatial variation in thresholds ─────────

p_threshold_map <- ggplot(
  local_thresholds,
  aes(x = x, y = y, fill = threshold_spi3)
) +
  geom_tile() +
  scale_fill_gradient2(
    low      = "#E53935",
    mid      = "white",
    high     = "#1565C0",
    midpoint = -0.842,
    name     = "Local\nthreshold"
  ) +
  coord_equal() +
  facet_wrap(~ "SPI-3 local threshold (20th percentile)") +
  labs(
    title    = "Locally calibrated SPI-3 drought threshold",
    subtitle = sprintf(
      "Target: 20th percentile | Theoretical N(0,1) value: -0.842 | Red = lower threshold, Blue = higher threshold"
    ),
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"))

print(p_threshold_map)

# ── Step 4: Check how much thresholds vary across pixels ────

cat("\n=== HOW MUCH DO THRESHOLDS VARY? ===\n\n")

threshold_variation <- local_thresholds %>%
  summarise(
    spi3_min      = round(min(threshold_spi3,  na.rm = TRUE), 3),
    spi3_max      = round(max(threshold_spi3,  na.rm = TRUE), 3),
    spi3_range    = round(max(threshold_spi3,  na.rm = TRUE) -
                            min(threshold_spi3,  na.rm = TRUE), 3),
    spi3_sd       = round(sd(threshold_spi3,   na.rm = TRUE), 3),
    spi6_min      = round(min(threshold_spi6,  na.rm = TRUE), 3),
    spi6_max      = round(max(threshold_spi6,  na.rm = TRUE), 3),
    spi6_range    = round(max(threshold_spi6,  na.rm = TRUE) -
                            min(threshold_spi6,  na.rm = TRUE), 3),
    spi6_sd       = round(sd(threshold_spi6,   na.rm = TRUE), 3)
  )

print(threshold_variation, width = Inf)

cat("\nInterpretation:\n")
cat("  If range is close to 0 → SPI is near-perfectly normal\n")
cat("     locally calibrated = same as fixed threshold\n")
cat("  If range is large (>0.3) → meaningful spatial variation\n")
cat("     locally calibrated thresholds add genuine value\n")







# ── Check which pixels have the most extreme thresholds ─────────────────────

cat("=== PIXELS WITH LOWEST THRESHOLD (most sensitive) ===\n")
local_thresholds %>%
  arrange(threshold_spi3) %>%
  head(10) %>%
  select(x, y, threshold_spi3, threshold_spi6,
         mean_spi3, sd_spi3) %>%
  print(width = Inf)

cat("\n=== PIXELS WITH HIGHEST THRESHOLD (least sensitive) ===\n")
local_thresholds %>%
  arrange(desc(threshold_spi3)) %>%
  head(10) %>%
  select(x, y, threshold_spi3, threshold_spi6,
         mean_spi3, sd_spi3) %>%
  print(width = Inf)

# ── Check if extreme thresholds cluster by country ───────────────────────────
local_thresholds_country <- local_thresholds %>%
  left_join(
    pixel_data %>% select(x, y, country) %>% distinct(),
    by = c("x", "y")
  )

cat("\n=== THRESHOLD SUMMARY BY COUNTRY ===\n\n")
local_thresholds_country %>%
  group_by(country) %>%
  summarise(
    n_pixels       = n(),
    mean_thr_spi3  = round(mean(threshold_spi3,  na.rm = TRUE), 3),
    min_thr_spi3   = round(min(threshold_spi3,   na.rm = TRUE), 3),
    max_thr_spi3   = round(max(threshold_spi3,   na.rm = TRUE), 3),
    range_thr_spi3 = round(max(threshold_spi3,   na.rm = TRUE) -
                             min(threshold_spi3,   na.rm = TRUE), 3),
    mean_thr_spi6  = round(mean(threshold_spi6,  na.rm = TRUE), 3),
    .groups = "drop"
  ) %>%
  arrange(mean_thr_spi3) %>%
  print(width = Inf)

# ── Check deviation from theoretical -0.842 ─────────────────────────────────
cat("\n=== DEVIATION FROM THEORETICAL N(0,1) THRESHOLD ===\n\n")

local_thresholds_country %>%
  group_by(country) %>%
  summarise(
    mean_deviation = round(mean(deviation_spi3, na.rm = TRUE), 3),
    max_deviation  = round(max(abs(deviation_spi3), na.rm = TRUE), 3),
    pct_within_01  = round(100 * mean(abs(deviation_spi3) < 0.1,
                                      na.rm = TRUE), 1),
    pct_within_02  = round(100 * mean(abs(deviation_spi3) < 0.2,
                                      na.rm = TRUE), 1),
    .groups = "drop"
  ) %>%
  print(width = Inf)

















