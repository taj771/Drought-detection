# Ref vs obs, 6-month, Frequency — colour by INDEX, shade by PERIOD.
suppressMessages({ library(arrow); library(dplyr); library(tidyr); library(ggplot2); library(stringr) })

ev <- as.data.frame(read_parquet("Codes/_archive/_data/events.parquet"))
THRESHOLDS <- c(0.0, -0.5, -1.0, -1.5)
SCALE <- 6
REF0 <- 1973; REF1 <- 2002; NREF <- REF1-REF0+1
OBS0 <- 2003; OBS1 <- 2023; NOBS <- OBS1-OBS0+1

base <- ev %>%
  filter(method == "threshold", start_thr %in% THRESHOLDS) %>%
  mutate(scale = as.integer(str_extract(index,"\\d+")),
         family = case_when(str_detect(index,"_pm$")~"SPEI-P",
                            str_detect(index,"^spei")~"SPEI-T",
                            str_detect(index,"^spi")~"SPI", TRUE~index)) %>%
  filter(scale == SCALE) %>%
  mutate(fam_lab = factor(paste0(family,"-",scale),
            levels=c(paste0("SPI-",SCALE),paste0("SPEI-T-",SCALE),paste0("SPEI-P-",SCALE))),
         family = factor(family, levels=c("SPI","SPEI-T","SPEI-P")),
         thr_f = factor(start_thr, levels=THRESHOLDS,
            labels=c("Threshold 0.0","Threshold -0.5",
                     "Threshold -1.0","Threshold -1.5")))

pix <- function(y0,y1,ny,lab) base %>%
  filter(event_year>=y0, event_year<=y1) %>%
  group_by(fam_lab, family, thr_f, x, y) %>%
  summarise(freq=n()/ny, .groups="drop") %>% mutate(period=lab)

dat <- bind_rows(pix(REF0,REF1,NREF,"Reference"),
                 pix(OBS0,OBS1,NOBS,"Observation")) %>%
  mutate(period = factor(period, levels=c("Reference","Observation")),
         grp = interaction(family, period, sep=" · "))

# colour by index (hue), shade by period (light=ref, dark=obs)
pal <- c(
  "SPI · Reference"    = "#f4a6a0", "SPI · Observation"    = "#c0392b",  # red
  "SPEI-T · Reference" = "#a3d9a5", "SPEI-T · Observation" = "#2e8b3d",  # green
  "SPEI-P · Reference" = "#a8c8ec", "SPEI-P · Observation" = "#2c6fb5"   # blue
)

p <- ggplot(dat, aes(x = fam_lab, y = freq, fill = grp)) +
  geom_boxplot(outlier.size=0.4, width=0.7, colour="grey20", linewidth=0.4,
               position = position_dodge(width=0.8)) +
  facet_wrap(~ thr_f, nrow=1) +
  scale_fill_manual(values = pal, name = NULL,
                    breaks = names(pal)) +
  labs(title="Frequency — 6-month: index colour, period shade",
       subtitle="Light = Reference (1973-2002), Dark = Observation (2003-2023). Box = across pixels (Benin).",
       x=NULL, y="Frequency (events/pixel/yr)") +
  theme_bw(base_size=10) +
  theme(legend.position="top", legend.text=element_text(size=8),
        axis.text.x=element_text(angle=30, hjust=1),
        panel.border = element_rect(colour="grey30", fill=NA, linewidth=0.6),
        panel.spacing = unit(0.6, "lines"),
        strip.background = element_rect(colour="grey30", fill="grey92"),
        strip.text = element_text(face="bold")) +
  guides(fill=guide_legend(nrow=2))

ggsave("Codes/_archive/_boxC.png", p, width=11, height=5, dpi=150, bg="white")
cat("done\n")
