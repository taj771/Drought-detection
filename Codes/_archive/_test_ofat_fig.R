# Render one OFAT figure standalone to eyeball it.
suppressMessages({ library(arrow); library(dplyr); library(tidyr); library(ggplot2); library(stringr) })
raw <- as.data.frame(read_parquet("Codes/_archive/_data/annual_summary.parquet"))
THRESHOLDS <- c(0.0, -0.5, -1.0, -1.5)
df <- raw %>%
  filter(method == "threshold", start_thr %in% THRESHOLDS) %>%
  mutate(
    scale = as.integer(str_extract(index, "\\d+")),
    family = case_when(str_detect(index,"_pm$")~"SPEI-P",
                       str_detect(index,"^spei")~"SPEI-T",
                       str_detect(index,"^spi")~"SPI", TRUE~index),
    family = factor(family, levels=c("SPI","SPEI-T","SPEI-P")),
    scale_f = factor(scale, levels=c(1,3,6,12), labels=c("1-mo","3-mo","6-mo","12-mo"))
  ) %>%
  rename(frequency = annual_freq_events)

sub <- df %>% filter(start_thr == -1.0)
p <- ggplot(sub, aes(x = scale_f, y = frequency, fill = family)) +
  geom_col(position = position_dodge(width=0.8), width=0.7) +
  labs(title="Frequency — thr -1.0 (varying accumulation period)",
       x=NULL, y="Frequency (events/pixel/yr)", fill=NULL) +
  theme_minimal(base_size=11) + theme(legend.position="top")
ggsave("Codes/_archive/_ofat_test.png", p, width=7.5, height=4.2, dpi=150, bg="white")
cat("done\n")
