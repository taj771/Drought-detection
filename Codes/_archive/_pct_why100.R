# Why is %-area line near 100% at threshold 0.0? Inspect the raw counts.
suppressMessages({ library(arrow); library(dplyr); library(stringr) })
ev <- as.data.frame(read_parquet("Codes/_archive/_data/events.parquet")) |>
  mutate(scale=as.integer(str_extract(index,"\\d+")),
         fam=case_when(str_detect(index,"_pm$")~"SPEI-P",
                       str_detect(index,"^spei")~"SPEI-T",
                       str_detect(index,"^spi")~"SPI",TRUE~index))
total_px <- length(unique(paste(ev$x, ev$y)))
cat("total_px (denominator):", total_px, "\n")
cat("methods present:", paste(unique(ev$method), collapse=", "), "\n")
cat("thresholds present:", paste(sort(unique(ev$start_thr)), collapse=", "), "\n\n")

# SPI-6, a few years, compare threshold 0.0 vs -1.0
for (thr in c(0.0, -1.0)) {
  cat("=== SPI-6, threshold", thr, "===\n")
  sub <- ev |> filter(method=="threshold", fam=="SPI", scale==6, start_thr==thr)
  yr <- sub |> filter(event_year %in% c(1990,2000,2010)) |>
    group_by(event_year) |>
    summarise(px_in_drought=n_distinct(paste(x,y)),
              n_events=n(), .groups="drop") |>
    mutate(pct=round(100*px_in_drought/total_px,1))
  print(as.data.frame(yr))
  cat("\n")
}

# how many MONTHS/year does a pixel spend "in event" at thr 0.0? (duration)
cat("=== duration (months) of events, SPI-6 ===\n")
ev |> filter(method=="threshold", fam=="SPI", scale==6, start_thr %in% c(0.0,-1.0)) |>
  group_by(start_thr) |>
  summarise(mean_dur=round(mean(duration_months,na.rm=TRUE),1),
            max_dur=max(duration_months,na.rm=TRUE),
            n_events=n(), .groups="drop") |>
  as.data.frame() |> print()
cat("done\n")
