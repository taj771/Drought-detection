# Check: is the 193-month event truly continuous, or does spei12 pop above 0?
suppressMessages({ library(arrow); library(dplyr) })
d <- as.data.frame(read_parquet("Codes/_archive/_data/speit.parquet"))
PX<-0.875; PY<-10.375
ts <- d |> filter(abs(x-PX)<0.01, abs(y-PY)<0.01) |> arrange(year,month) |>
  mutate(date=as.Date(sprintf("%04d-%02d-01",year,month)))

# the event window from the detection: 2004-06 to 2020-06
win <- ts |> filter(date>=as.Date("2004-06-01"), date<=as.Date("2020-06-01"))
cat("event window rows:", nrow(win), "\n")
cat("any spei12 >= 0 inside the window?:", any(win$spei12>=0, na.rm=TRUE), "\n")
cat("count spei12 >= 0 in window:", sum(win$spei12>=0, na.rm=TRUE), "\n")
if (any(win$spei12>=0, na.rm=TRUE))
  print(win |> filter(spei12>=0) |> select(date, spei12))

# whole obs period 2003-2023: how many separate below-0 runs?
obs <- ts |> filter(year>=2003, year<=2023, !is.na(spei12))
below <- obs$spei12 < 0
rl <- rle(below)
cat("\nObs period: number of separate below-0 runs:", sum(rl$values), "\n")
cat("run lengths (below-0 only):", paste(rl$lengths[rl$values], collapse=","), "\n")
