# =============================================================================
# SM2. Soil-moisture ANOMALY (+ standardized SSMI) from ERA5-Land root-zone SM.
# =============================================================================
# Mirrors the NDVI approach (N2). Two products, per pixel per calendar month:
#   sm_anom = sm - climatological_mean   (raw departure, like NDVI anomaly)
#   ssmi    = (sm - mean) / sd           (z-score, like SVI; the standardized one)
# Baseline for mean/sd = 2001-2023, to MATCH the NDVI anomaly baseline. NDVI and
# soil moisture are the two "anomaly" witnesses (departure from a recent normal),
# so we align their baselines. A full-1950-2023 baseline made soil moisture read
# 65% below zero in 2003-2023 (the recent decades are drier than the long-term
# mean), which inflated the below-0 event count; a 2001-2023 baseline re-centres
# it (~50% below 0) like NDVI. (SPI/SPEI/PDSI use full-period calibration — the
# baseline-window choice is an open team question, project_calibration_open_decision.)
#
# In : Codes/_archive/_data/sm_benin_monthly.parquet (x,y,year,month,sm) from SM1
# Out: same parquet + columns sm_anom, ssmi, sm_sd  (overwrites in place)
# LOCAL R.
# =============================================================================
suppressMessages({ library(arrow); library(dplyr) })
setwd("c:/Users/tajayalath/Box/R - Projects - Tharaka/Drought-indices")
IN <- "Codes/_archive/_data/sm_benin_monthly.parquet"
BASE0 <- 2001; BASE1 <- 2023        # match NDVI anomaly baseline (recent normal)

d <- as.data.frame(read_parquet(IN))
stopifnot(all(c("x","y","year","month","sm") %in% names(d)))
# keep only base columns so a rerun doesn't collide with prior anomaly columns
d <- d[, c("x","y","year","month","sm")]

clim <- d |> filter(year>=BASE0, year<=BASE1) |>
  group_by(x, y, month) |>
  summarise(sm_mean=mean(sm, na.rm=TRUE),
            sm_sd  =sd(sm,  na.rm=TRUE), .groups="drop")

out <- d |> left_join(clim, by=c("x","y","month")) |>
  mutate(sm_anom = sm - sm_mean,
         ssmi    = ifelse(sm_sd>0, (sm - sm_mean)/sm_sd, NA_real_)) |>
  select(x, y, year, month, sm, sm_anom, ssmi, sm_sd)

# sanity: anomaly ~ mean 0, ssmi ~ mean 0 sd 1 over baseline
b <- out |> filter(year>=BASE0, year<=BASE1)
cat(sprintf("sm_anom baseline mean %.4f (expect ~0)\n", mean(b$sm_anom, na.rm=TRUE)))
cat(sprintf("ssmi    baseline mean %.3f sd %.3f (expect ~0, ~1)\n",
            mean(b$ssmi, na.rm=TRUE), sd(b$ssmi, na.rm=TRUE)))
cat(sprintf("rows %d | px %d | yr %d-%d | %% sm_anom<0 %.0f%%\n",
    nrow(out), n_distinct(paste(out$x,out$y)), min(out$year), max(out$year),
    100*mean(out$sm_anom<0, na.rm=TRUE)))

write_parquet(out, IN)
cat("SM2 done ->", IN, "(added sm_anom, ssmi). Next: 03b_soilm_freq.R\n")
