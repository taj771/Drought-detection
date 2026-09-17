suppressMessages({library(arrow);library(dplyr);library(stringr)})
ev <- as.data.frame(read_parquet("Codes/_archive/_data/events.parquet")) %>%
  filter(method=="threshold", start_thr==-1.0) %>%
  mutate(scale=as.integer(str_extract(index,"\\d+")),
         fam=case_when(str_detect(index,"_pm$")~"SPEI-P",str_detect(index,"^spei")~"SPEI-T",str_detect(index,"^spi")~"SPI",TRUE~index))
ev %>% group_by(fam,scale) %>%
  summarise(mean_dur=round(mean(duration_months),2), max_dur=max(duration_months), n=n(), .groups="drop") %>%
  arrange(fam,scale) %>% as.data.frame() %>% print()
