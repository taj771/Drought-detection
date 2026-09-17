suppressMessages({library(arrow);library(dplyr);library(tidyr);library(stringr)})
ev <- as.data.frame(read_parquet("Codes/_archive/_data/events.parquet")) %>%
  filter(method=="threshold", start_thr %in% c(0,-0.5,-1,-1.5)) %>%
  mutate(scale=as.integer(str_extract(index,"\\d+")),
         family=case_when(str_detect(index,"_pm$")~"SPEI-P",str_detect(index,"^spei")~"SPEI-T",str_detect(index,"^spi")~"SPI",TRUE~index))
sub <- ev %>% filter(scale==6, event_year>=1973, event_year<=2002) %>%
  group_by(family,start_thr,x,y) %>% summarise(v=n()/30,.groups="drop") %>%
  group_by(family,start_thr) %>% summarise(med=round(median(v),2),.groups="drop")
w <- sub %>% mutate(fam=paste0(family,"-6")) %>% select(fam,start_thr,med) %>%
  pivot_wider(names_from=start_thr,values_from=med)
print(as.data.frame(w))
