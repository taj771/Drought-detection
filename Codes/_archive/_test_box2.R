# Two test figures, 6-month, Frequency:
#   (A) single-period, boxes labelled with scale (SPI-6 etc.)
#   (B) ref vs obs, two boxes per index (paired), like the earlier write-up.
suppressMessages({ library(arrow); library(dplyr); library(tidyr); library(ggplot2); library(stringr) })

ev <- as.data.frame(read_parquet("Codes/_archive/_data/events.parquet"))
THRESHOLDS <- c(0.0, -0.5, -1.0, -1.5)
SCALE <- 6
REF0 <- 1973; REF1 <- 2002; NREF <- REF1 - REF0 + 1
OBS0 <- 2003; OBS1 <- 2023; NOBS <- OBS1 - OBS0 + 1

base <- ev %>%
  filter(method == "threshold", start_thr %in% THRESHOLDS) %>%
  mutate(
    scale  = as.integer(str_extract(index, "\\d+")),
    family = case_when(str_detect(index,"_pm$")~"SPEI-P",
                       str_detect(index,"^spei")~"SPEI-T",
                       str_detect(index,"^spi")~"SPI", TRUE~index)
  ) %>%
  filter(scale == SCALE) %>%
  mutate(
    fam_lab = factor(paste0(family, "-", scale),
                     levels = c(paste0("SPI-",SCALE),
                                paste0("SPEI-T-",SCALE),
                                paste0("SPEI-P-",SCALE))),
    thr_f = factor(start_thr, levels=THRESHOLDS,
                   labels=c("thr 0.0","thr -0.5","thr -1.0","thr -1.5"))
  )

# per-pixel frequency in a period
pix_freq_period <- function(d, y0, y1, ny, plabel) {
  d %>% filter(event_year >= y0, event_year <= y1) %>%
    group_by(fam_lab, thr_f, x, y) %>%
    summarise(freq = n()/ny, .groups="drop") %>%
    mutate(period = plabel)
}

# (A) single period (reference), boxes labelled with scale
A <- pix_freq_period(base, REF0, REF1, NREF, "Reference")
pA <- ggplot(A, aes(x = fam_lab, y = freq, fill = fam_lab)) +
  geom_boxplot(outlier.size = 0.4, width = 0.6) +
  facet_wrap(~ thr_f, nrow = 1) +
  labs(title="Frequency — 6-month accumulation",
       subtitle="Box = spread across pixels (Benin, reference 1973-2002)",
       x=NULL, y="Frequency (events/pixel/yr)") +
  theme_minimal(base_size=10) + theme(legend.position="none",
       axis.text.x = element_text(angle=30, hjust=1))
ggsave("Codes/_archive/_boxA.png", pA, width=9, height=4.4, dpi=150, bg="white")

# (B) ref vs obs — two boxes per index, paired by period
B <- bind_rows(
  pix_freq_period(base, REF0, REF1, NREF, "Reference (1973-2002)"),
  pix_freq_period(base, OBS0, OBS1, NOBS, "Observation (2003-2023)")
) %>% mutate(period = factor(period,
       levels=c("Reference (1973-2002)","Observation (2003-2023)")))
pB <- ggplot(B, aes(x = fam_lab, y = freq, fill = period)) +
  geom_boxplot(outlier.size = 0.4, width = 0.7,
               position = position_dodge(width = 0.8)) +
  facet_wrap(~ thr_f, nrow = 1) +
  scale_fill_manual(values = c("grey70","firebrick")) +
  labs(title="Frequency — 6-month accumulation: Reference vs Observation",
       subtitle="Box = spread across pixels (Benin)",
       x=NULL, y="Frequency (events/pixel/yr)", fill=NULL) +
  theme_minimal(base_size=10) + theme(legend.position="top",
       axis.text.x = element_text(angle=30, hjust=1))
ggsave("Codes/_archive/_boxB.png", pB, width=10, height=4.6, dpi=150, bg="white")
cat("done\n")
