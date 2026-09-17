# Local test harness for 04_APP_Visuals.R — synthetic data, no Databricks.
# Mimics the environment 04 expects (events_ref/obs, index_monthly, boundary,
# app_summary_table + config vars) and sources the plot logic.
suppressPackageStartupMessages({ library(dplyr); library(ggplot2); library(sf); library(tidyr) })
set.seed(1)

# -- config vars 04 reads -----------------------------------------------------
APP_COUNTRY <- "TestLand"; APP_INDEX <- "SPI"; APP_SCALE <- 6; APP_THRESHOLD <- -0.5
REF_START <- 1973; REF_END <- 2002; OBS_START <- 2003; OBS_END <- 2023
.index_col <- "spi6"
OUTPUT_DIR <- file.path(tempdir(), "app_test")

# -- synthetic boundary (a square polygon) ------------------------------------
boundary <- st_sfc(st_polygon(list(rbind(
  c(0,0), c(3,0), c(3,3), c(0,3), c(0,0)))), crs = 4326)

# -- synthetic pixels inside the square ---------------------------------------
px <- expand.grid(x = seq(0.25, 2.75, 0.5), y = seq(0.25, 2.75, 0.5))
months <- expand.grid(year = 1973:2023, month = 1:12)
index_monthly <- do.call(rbind, lapply(seq_len(nrow(px)), function(i) {
  data.frame(x = px$x[i], y = px$y[i], months,
             spi6 = rnorm(nrow(months), 0, 1))
}))

# -- synthetic events ---------------------------------------------------------
mk_events <- function(yr0, yr1, n) data.frame(
  x = sample(px$x, n, TRUE), y = sample(px$y, n, TRUE),
  drought_id = seq_len(n),
  start_date = as.Date(sprintf("%d-0%d-01", sample(yr0:yr1, n, TRUE), sample(1:9, n, TRUE))),
  duration_months = sample(2:8, n, TRUE),
  peak_index = -runif(n, 0.5, 2.5),
  total_severity = -runif(n, 2, 12)
)
events_ref <- mk_events(1973, 2002, 40); events_ref$end_date <- events_ref$start_date + 90
events_obs <- mk_events(2003, 2023, 55); events_obs$end_date <- events_obs$start_date + 90

# -- summary table (what 03 makes) --------------------------------------------
app_summary_table <- data.frame(
  characteristic = c("Frequency (events/pixel/yr)","Duration (months)",
                     "Peak severity (index)","Pixels affected (%)"),
  reference   = c(0.85, 3.4, -1.25, 42),
  observation = c(1.02, 3.9, -1.48, 55),
  pct_change  = c(20.0, 14.7, 18.4, 31.0))

# -- source the real visuals script -------------------------------------------
cat("Sourcing 04_APP_Visuals.R...\n")
source("Codes/app_monitor/04_APP_Visuals.R")

# -- report -------------------------------------------------------------------
figs <- list.files(file.path(OUTPUT_DIR, "figures", "TestLand"), full.names = TRUE)
cat("\n=== RESULT ===\n")
cat("Figures produced:", length(figs), "\n")
for (f in figs) cat(sprintf("  %s (%.0f KB)\n", basename(f), file.info(f)$size/1024))
