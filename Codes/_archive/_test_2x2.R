# 2x2 facet + median labelled on each box. Frequency, 6-month, ref vs obs.
suppressMessages({ library(arrow); library(dplyr); library(tidyr); library(ggplot2); library(stringr) })

ev <- as.data.frame(read_parquet("Codes/_archive/_data/events.parquet"))
THRESHOLDS <- c(0.0,-0.5,-1.0,-1.5); SCALE <- 6
REF0<-1973;REF1<-2002;NREF<-30; OBS0<-2003;OBS1<-2023;NOBS<-21

base <- ev %>% filter(method=="threshold", start_thr %in% THRESHOLDS) %>%
  mutate(scale=as.integer(str_extract(index,"\\d+")),
         fam_chr=case_when(str_detect(index,"_pm$")~"SPEI-P",
                           str_detect(index,"^spei")~"SPEI-T",
                           str_detect(index,"^spi")~"SPI",TRUE~index)) %>%
  filter(scale==SCALE) %>%
  mutate(family=factor(fam_chr, levels=c("SPI","SPEI-T","SPEI-P")),
         fam_lab=factor(paste0(fam_chr,"-",SCALE),
            levels=paste0(c("SPI","SPEI-T","SPEI-P"),"-",SCALE)),
         thr_f=factor(start_thr,levels=THRESHOLDS,
            labels=c("Threshold 0.0","Threshold -0.5","Threshold -1.0","Threshold -1.5")))

pix <- function(y0,y1,ny,lab) base %>% filter(event_year>=y0,event_year<=y1) %>%
  group_by(fam_lab,family,thr_f,x,y) %>% summarise(freq=n()/ny,.groups="drop") %>% mutate(period=lab)

dat <- bind_rows(pix(REF0,REF1,NREF,"Reference"), pix(OBS0,OBS1,NOBS,"Observation")) %>%
  mutate(period=factor(period,levels=c("Reference","Observation")),
         grp=interaction(family,period,sep=" · "))

# median per box (the value shown) + upper-whisker top (where the label sits)
labs <- dat %>% group_by(fam_lab,thr_f,period,grp) %>%
  summarise(med=median(freq),
            ytop = quantile(freq,0.75) + 1.5*IQR(freq),   # boxplot upper whisker
            ymax = max(freq),
            .groups="drop") %>%
  mutate(ylab = pmin(ytop, ymax))   # cap at actual max (no floating above outliers)

pal <- c("SPI · Reference"="#f4a6a0","SPI · Observation"="#c0392b",
         "SPEI-T · Reference"="#a3d9a5","SPEI-T · Observation"="#2e8b3d",
         "SPEI-P · Reference"="#a8c8ec","SPEI-P · Observation"="#2c6fb5")

p <- ggplot(dat, aes(x=fam_lab, y=freq, fill=grp)) +
  geom_boxplot(outlier.size=0.3, width=0.7, colour="grey20", linewidth=0.35,
               position=position_dodge(width=0.8)) +
  geom_text(data=labs, aes(x=fam_lab, y=ylab, label=sprintf("%.2f",med), group=grp),
            position=position_dodge(width=0.8), size=2.5, vjust=-0.6, fontface="bold") +
  facet_wrap(~thr_f, nrow=2) +
  scale_fill_manual(values=pal, name=NULL, breaks=names(pal)) +
  scale_y_continuous(expand=expansion(mult=c(0.02,0.12))) +
  labs(title="Frequency — 6-month accumulation",
       subtitle="Light = Reference, Dark = Observation. Box = across pixels; label = median.",
       x=NULL, y="Frequency (events/pixel/yr)") +
  theme_bw(base_size=10) +
  theme(legend.position="top", legend.text=element_text(size=8),
        axis.text.x=element_text(angle=25,hjust=1),
        panel.border=element_rect(colour="grey30",fill=NA,linewidth=0.6),
        strip.background=element_rect(colour="grey30",fill="grey92"),
        strip.text=element_text(face="bold")) +
  guides(fill=guide_legend(nrow=2))

ggsave("Codes/_archive/_box2x2.png", p, width=8, height=6.5, dpi=150, bg="white")
cat("done\n")
