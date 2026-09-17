# Investigate extreme-duration events: threshold 0.0, observation period.
suppressMessages({ library(arrow); library(dplyr); library(stringr) })
ev <- as.data.frame(read_parquet("Codes/_archive/_data/events.parquet")) |>
  mutate(scale=as.integer(str_extract(index,"\\d+")),
         fam=case_when(str_detect(index,"_pm$")~"SPEI-P",
                       str_detect(index,"^spei")~"SPEI-T",
                       str_detect(index,"^spi")~"SPI",TRUE~index))

sub <- ev |> filter(method=="threshold", start_thr==0.0,
                    event_year>=2003, event_year<=2023)

cat("=== duration distribution (thr 0.0, obs) by index family ===\n")
sub |> group_by(fam,scale) |>
  summarise(n=n(), mean=round(mean(duration_months),1),
            p95=quantile(duration_months,0.95), max=max(duration_months),
            .groups="drop") |> arrange(desc(max)) |> head(12) |> as.data.frame() |> print()

cat("\n=== top 10 longest events (thr 0.0, obs) ===\n")
top <- sub |> arrange(desc(duration_months)) |>
  select(fam,scale,index,x,y,start_date,end_date,duration_months,peak_index) |>
  head(10)
print(as.data.frame(top))

cat("\n=== how many events > 24 months? ===\n")
cat("count:", sum(sub$duration_months>24), " of ", nrow(sub), "\n")
