# Test: Frequency, 6-month — 4 threshold panels, boxes = SPI/SPEI-T/SPEI-P across pixels.
suppressMessages({ library(arrow); library(dplyr); library(tidyr); library(ggplot2); library(stringr) })

ev <- as.data.frame(read_parquet("Codes/_archive/_data/events.parquet"))

REF0 <- 1973; REF1 <- 2002        # reference period for frequency denominator
N_YEARS <- REF1 - REF0 + 1
THRESHOLDS <- c(0.0, -0.5, -1.0, -1.5)
SCALE <- 6                          # this figure = 6-month

# tag family + scale from index name
ev2 <- ev %>%
  filter(method == "threshold", start_thr %in% THRESHOLDS,
         event_year >= REF0, event_year <= REF1) %>%
  mutate(
    scale = as.integer(str_extract(index, "\\d+")),
    family = case_when(str_detect(index,"_pm$")~"SPEI-P",
                       str_detect(index,"^spei")~"SPEI-T",
                       str_detect(index,"^spi")~"SPI", TRUE~index),
    family = factor(family, levels=c("SPI","SPEI-T","SPEI-P"))
  ) %>%
  filter(scale == SCALE)

# per-pixel frequency = events at that pixel / N_YEARS, per config(threshold×family)
pix_freq <- ev2 %>%
  group_by(family, start_thr, x, y) %>%
  summarise(freq = n() / N_YEARS, .groups="drop") %>%
  mutate(thr_f = factor(start_thr, levels=THRESHOLDS,
                        labels=c("thr 0.0","thr -0.5","thr -1.0","thr -1.5")))

p <- ggplot(pix_freq, aes(x = family, y = freq, fill = family)) +
  geom_boxplot(outlier.size = 0.5, width = 0.6) +
  facet_wrap(~ thr_f, nrow = 1) +
  labs(title = "Frequency — 6-month accumulation",
       subtitle = "Box = spread across pixels (Benin, ref 1973-2002)",
       x = NULL, y = "Frequency (events/pixel/yr)") +
  theme_minimal(base_size = 10) +
  theme(legend.position = "none")

ggsave("Codes/_archive/_box_test.png", p, width = 9, height = 4.2, dpi = 150, bg = "white")
cat("done — pixels per box approx:", nrow(pix_freq)/12, "\n")
