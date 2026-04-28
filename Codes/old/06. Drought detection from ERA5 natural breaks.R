# identify natural breraks of SPI and SPEI based on ERA5 

# load data

# clean memory
rm(list = ls())


library(dplyr)
library(ggplot2)
library(tidyr)
library(here)

spi_results <- readRDS("./Data/spi_results.rds")%>%
  filter(
    !is.na(spi1) &
      !is.na(spi3) &
      !is.na(spi6) &
      !is.na(spi12)
  )


# ══════════════════════════════════════════════════════════════════════════════
# STEP 1: Pivot all SPI scales long — one row per pixel/year/month/scale
# ══════════════════════════════════════════════════════════════════════════════
spi_long <- spi_results %>%
  pivot_longer(
    cols      = c(spi1, spi3, spi6, spi12),
    names_to  = "spi_scale",
    values_to = "spi_value"
  ) %>%
  filter(!is.na(spi_value)) %>%
  mutate(
    spi_scale = factor(spi_scale, 
                       levels = c("spi1","spi3","spi6","spi12"),
                       labels = c("SPI-1","SPI-3","SPI-6","SPI-12"))
  )

# ══════════════════════════════════════════════════════════════════════════════
# STEP 2: Compute Percentiles Per Month × SPI Scale
# ══════════════════════════════════════════════════════════════════════════════

# ── Define month labels first ─────────────────────────────────────────────────
month_labels <- c("Jan","Feb","Mar","Apr","May","Jun",
                  "Jul","Aug","Sep","Oct","Nov","Dec")

percentiles_all <- spi_long %>%
  group_by(spi_scale, month) %>%
  summarise(
    p05  = quantile(spi_value, 0.05),
    p10  = quantile(spi_value, 0.10),
    p20  = quantile(spi_value, 0.20),
    p50  = quantile(spi_value, 0.50),
    p80  = quantile(spi_value, 0.80),
    p90  = quantile(spi_value, 0.90),
    p95  = quantile(spi_value, 0.95),
    mean = mean(spi_value),
    sd   = sd(spi_value),
    n    = n(),
    .groups = "drop"
  ) %>%
  mutate(month_label = factor(month_labels[month], levels = month_labels))

print(percentiles_all)

# ══════════════════════════════════════════════════════════════════════════════
# STEP 3: Percentile lines data — long format for plotting
# ══════════════════════════════════════════════════════════════════════════════
percentiles_lines <- percentiles_all %>%
  select(spi_scale, month, month_label, p05, p10, p20, p80, p90, p95) %>%
  pivot_longer(
    cols      = c(p05, p10, p20, p80, p90, p95),
    names_to  = "percentile",
    values_to = "value"
  ) %>%
  mutate(
    drought     = percentile %in% c("p05", "p10", "p20"),
    line_color  = ifelse(drought, "#E63946", "#457B9D")
  )

# ══════════════════════════════════════════════════════════════════════════════
# STEP 4: Density Plot — one page per SPI scale, faceted by month
# ══════════════════════════════════════════════════════════════════════════════
spi_long <- spi_long %>%
  mutate(month_label = factor(month_labels[month], levels = month_labels))

# Loop over each SPI scale → one plot each
spi_scales <- levels(spi_long$spi_scale)

plots <- lapply(spi_scales, function(scale) {
  
  plot_data  <- spi_long        %>% filter(spi_scale == scale)
  line_data  <- percentiles_lines %>% filter(spi_scale == scale)
  
  ggplot(plot_data, aes(x = spi_value)) +
    geom_density(fill = "skyblue", alpha = 0.3, color = "grey40") +
    
    # Dry percentile lines — red
    geom_vline(data = line_data %>% filter(drought),
               aes(xintercept = value, linetype = percentile),
               color = "#E63946", linewidth = 0.6) +
    
    # Wet percentile lines — blue
    geom_vline(data = line_data %>% filter(!drought),
               aes(xintercept = value, linetype = percentile),
               color = "#457B9D", linewidth = 0.6) +
    
    # Zero reference
    geom_vline(xintercept = 0, color = "black", 
               linewidth = 0.4, alpha = 0.5) +
    
    # ── Manual x-axis breaks ─────────────────────────────────────────────────
    scale_x_continuous(
      breaks = c(-3, -2, -1, 0, 1, 2, 3)          # ← SPI natural scale
    ) +
    
    scale_linetype_manual(
      values = c("p05" = "dotted",   "p10" = "dashed", "p20" = "longdash"),
      labels = c("p05" = "5th (Extreme dry)",  "p10" = "10th (Severe dry)",
                 "p20" = "20th (Moderate dry)")
    ) +
    
    facet_wrap(~ month_label, ncol = 4, scales = "free_y") +
    
    labs(
      title    = paste("Historical", scale, "Distribution by Month"),
      subtitle = "Red = dry percentiles | Blue = wet percentiles | Black = zero",
      x        = scale,
      y        = "Density",
      linetype = "Percentile"
    ) +
    theme_minimal() +
    theme(
      strip.text      = element_text(face = "bold"),
      legend.position = "bottom",
      legend.title    = element_text(face = "bold")
    )
})

# ── Print all plots ───────────────────────────────────────────────────────────
names(plots) <- spi_scales
for (scale in spi_scales) print(plots[[scale]])

# ══════════════════════════════════════════════════════════════════════════════
# STEP 5: Heatmap — all SPI scales side by side
# ══════════════════════════════════════════════════════════════════════════════
percentiles_all %>%
  select(spi_scale, month_label, p05, p10, p20, p50, p80, p90, p95) %>%
  pivot_longer(
    cols      = c(p05, p10, p20, p50, p80, p90, p95),
    names_to  = "percentile",
    values_to = "spi_value"
  ) %>%
  mutate(percentile = factor(percentile,
                             levels = c("p05","p10","p20","p50",
                                        "p80","p90","p95"))) %>%
  ggplot(aes(x = month_label, y = percentile, fill = spi_value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(spi_value, 2)), size = 2.5) +
  scale_fill_gradient2(low = "#E63946", mid = "white", 
                       high = "#457B9D", midpoint = 0) +
  
  # ← one panel per SPI scale
  facet_wrap(~ spi_scale, ncol = 2) +
  
  labs(
    title    = "SPI Percentile Thresholds by Month and Scale",
    subtitle = "Red = dry | Blue = wet",
    x        = "Month", y = "Percentile", fill = "SPI Value"
  ) +
  theme_minimal() +
  theme(
    strip.text      = element_text(face = "bold", size = 11),
    axis.text.x     = element_text(angle = 45, hjust = 1),
    legend.position = "right"
  )

ggsave(here('Figures/HeatMapSPI.PDF'), height = 7, width =10, units = 'in',  bg = "transparent")


# ══════════════════════════════════════════════════════════════════════════════
# STEP 6: Save all density plots
# ══════════════════════════════════════════════════════════════════════════════
dir.create(here("Figures"), recursive = TRUE, showWarnings = FALSE)

for (scale in spi_scales) {
  ggsave(
    here(paste0("Figures/", scale, "_monthly_distribution.PDF")),
    plot   = plots[[scale]],
    height = 10, width = 18, units = "in", bg = "transparent"
  )
}





