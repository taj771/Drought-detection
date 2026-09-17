# Check the %-pixels-affected: current (ever-in-period) vs a per-year version.
suppressMessages({ library(arrow); library(dplyr); library(stringr) })
ev <- as.data.frame(read_parquet("Codes/_archive/_data/events.parquet")) |>
  mutate(scale=as.integer(str_extract(index,"\\d+")),
         fam=case_when(str_detect(index,"_pm$")~"SPEI-P",
                       str_detect(index,"^spei")~"SPEI-T",
                       str_detect(index,"^spi")~"SPI",TRUE~index))
total_px <- length(unique(paste(ev$x, ev$y)))
cat("total pixels:", total_px, "\n\n")

# take SPI-6, threshold -1.0, obs period as an example
sub <- ev |> filter(method=="threshold", fam=="SPI", scale==6, start_thr==-1.0,
                    event_year>=2003, event_year<=2023)

# CURRENT metric: % pixels with >=1 event anywhere in the 21-year period
ever <- 100 * length(unique(paste(sub$x,sub$y))) / total_px
cat("CURRENT (>=1 event in 2003-2023):", round(ever,1), "%\n\n")

# PER-YEAR: for each year, % pixels in drought that year -> then its range/mean
yr <- sub |> group_by(event_year) |>
  summarise(pct = 100*n_distinct(paste(x,y))/total_px, .groups="drop")
cat("PER-YEAR % pixels affected (2003-2023):\n")
print(as.data.frame(yr))
cat("\nmean per-year:", round(mean(yr$pct),1),
    " min:", round(min(yr$pct),1), " max:", round(max(yr$pct),1), "%\n")
