# One wide table per CHARACTERISTIC (Frequency example): rows = index×scale,
# columns = threshold × period. Median across pixels.
suppressMessages({ library(arrow); library(dplyr); library(tidyr); library(stringr); library(knitr) })

ev <- as.data.frame(read_parquet("Codes/_archive/_data/events.parquet")) %>%
  filter(method=="threshold", start_thr %in% c(0.0,-0.5,-1.0,-1.5)) %>%
  mutate(scale=as.integer(str_extract(index,"\\d+")),
         family=case_when(str_detect(index,"_pm$")~"SPEI-P",
                          str_detect(index,"^spei")~"SPEI-T",
                          str_detect(index,"^spi")~"SPI", TRUE~index))
REF0<-1973; REF1<-2002; NREF<-30; OBS0<-2003; OBS1<-2023; NOBS<-21

pix <- function(y0,y1,ny,lab) ev %>% filter(event_year>=y0,event_year<=y1) %>%
  group_by(family,scale,start_thr,x,y) %>% summarise(f=n()/ny,.groups="drop") %>%
  group_by(family,scale,start_thr) %>% summarise(med=round(median(f),2),.groups="drop") %>%
  mutate(period=lab)

dat <- bind_rows(pix(REF0,REF1,NREF,"Ref"), pix(OBS0,OBS1,NOBS,"Obs")) %>%
  mutate(config=paste0(family,"-",scale),
         col=paste0("thr", start_thr, "_", period)) %>%
  arrange(factor(family,levels=c("SPI","SPEI-T","SPEI-P")), scale)

wide <- dat %>% select(config, col, med) %>%
  pivot_wider(names_from=col, values_from=med) %>%
  # order columns: thr 0.0 Ref/Obs, -0.5 Ref/Obs, ...
  select(config,
         `thr0_Ref`,`thr0_Obs`,
         `thr-0.5_Ref`,`thr-0.5_Obs`,
         `thr-1_Ref`,`thr-1_Obs`,
         `thr-1.5_Ref`,`thr-1.5_Obs`)

cat("\n=== Frequency (median events/pixel/yr): one wide table, all configs ===\n\n")
print(kable(wide, format="pipe",
  col.names=c("Config","Ref","Obs","Ref","Obs","Ref","Obs","Ref","Obs")))
cat("\nColumn groups: Thr 0.0 | Thr -0.5 | Thr -1.0 | Thr -1.5\n")
