# =============================================================================
# SM1. Download ERA5-Land soil moisture (swvl), clip Benin, monthly, 154 px.
# R / ecmwfr version — reuses the SAME CDS setup as the old PET download
# (Codes/PET_data_prep/01. PET_Download.R): key in .Renviron CDS_API_KEY, pkg ecmwfr.
# =============================================================================
# Third quality-check witness for Q3 (with NDVI anomaly + PDSI). ERA5-Land
# volumetric soil water swvl1-3 = 0-100 cm root zone, 0.1 deg, monthly, 1950-2023.
# Aggregate to the same 154 ERA5 pixels; SM2 makes the anomaly; 03b counts events.
#
# ⚠ PET-BUG LESSON: ERA5-Land *pev* was the WRONG variable (pan evap). swvl (soil
# water) is a DIFFERENT, standard variable — expected fine. Verify units (m3/m3,
# Benin ~0.05-0.35) + do NOT scale.
#
# Prereq: .Renviron has CDS_API_KEY=... (already set, same as PET download).
#         library(ecmwfr) installed (confirmed). LOCAL run via Rscript.
# Out: Data/era5land_sm/era5land_swvl_benin_1950_2023.nc (cached, gitignored)
#      Codes/_archive/_data/sm_benin_monthly.parquet (x,y,year,month,sm) 154 px
# =============================================================================
suppressMessages({ library(ecmwfr); library(terra); library(arrow); library(dplyr) })
setwd("c:/Users/tajayalath/Box/R - Projects - Tharaka/Drought-indices")

OUTD <- "Data/era5land_sm"; dir.create(OUTD, showWarnings=FALSE, recursive=TRUE)
FN   <- "era5land_swvl_benin_1950_2023.nc"
NC   <- file.path(OUTD, FN)
PXCSV<- "Codes/ndvi_validation/era5_pixels_benin.csv"
OUT  <- "Codes/_archive/_data/sm_benin_monthly.parquet"

# Benin bbox for CDS: c(N, W, S, E)
BBOX  <- c(13.0, 0.5, 6.0, 4.0)
YEARS <- as.character(1950:2023)
MONTHS<- sprintf("%02d", 1:12)
LAYERS  <- c("swvl1","swvl2","swvl3")      # 0-7, 7-28, 28-100 cm
THICK   <- c(7, 21, 72); WT <- THICK/sum(THICK)  # depth-weighted root zone

# --- CDS auth (same as PET download) ----------------------------------------
key <- Sys.getenv("CDS_API_KEY")
if(nchar(key)==0) stop("CDS_API_KEY not set in .Renviron")
wf_set_key(key = key)

# --- download once ----------------------------------------------------------
if(!file.exists(NC)){
  request <- list(
    dataset_short_name = "reanalysis-era5-land-monthly-means",
    product_type       = "monthly_averaged_reanalysis",
    variable           = c("volumetric_soil_water_layer_1",
                           "volumetric_soil_water_layer_2",
                           "volumetric_soil_water_layer_3"),
    year   = YEARS, month = MONTHS, time = "00:00",
    area   = BBOX, data_format = "netcdf", download_format = "unarchived",
    target = FN
  )
  cat("requesting ERA5-Land swvl from CDS (Benin, 1950-2023)...\n")
  for(attempt in 1:3){
    ok <- tryCatch({ wf_request(request=request, transfer=TRUE, path=OUTD, verbose=TRUE); TRUE },
                   error=function(e){ message(sprintf("[retry %d] %s", attempt, e$message))
                                      if(attempt<3) Sys.sleep(30*attempt); FALSE })
    if(ok) break
  }
}
if(!file.exists(NC)) stop("download failed — check CDS key/migration (UUID key may be dead post-2024).")
cat("nc ready:", NC, sprintf("%.0f MB\n", file.size(NC)/1e6))

# --- open, build root-zone SM, aggregate to 154 px --------------------------
r <- rast(NC)
cat("layers:", nlyr(r), " names sample:", paste(head(names(r),6),collapse=", "), "\n")
# terra reads each (var,time) as a layer; split by variable via name match
get_var <- function(v) r[[grep(v, names(r))]]
present <- LAYERS[sapply(LAYERS, function(v) any(grepl(v, names(r))))]
if(length(present)==0) stop(paste("no swvl vars found; got", paste(head(names(r)),collapse=",")))
w <- WT[match(present, LAYERS)]; w <- w/sum(w)
sm_stack <- Reduce(`+`, Map(function(v,wi) get_var(v)*wi, present, w))  # weighted root zone

px <- read.csv(PXCSV)
# NEAREST-CELL point extract at each of the 154 pixel centres (single 0.1deg
# ERA5-Land cell per pixel; NOT a box mean -- QC confirmed the two differ by only
# ~0.013 m3/m3, corr 0.94, so nearest-cell is fine at this resolution).
# -> matrix 154 x n_time (no ID col).
vals <- terra::extract(sm_stack, cbind(px$x, px$y))
vals <- as.matrix(vals)
# time from epoch seconds embedded in swvl1 layer names ("...valid_time=-631152000")
tnames <- names(get_var(present[1]))
epoch  <- as.numeric(sub(".*valid_time=", "", tnames))
tt     <- as.POSIXct(epoch, origin="1970-01-01", tz="UTC")
ym <- data.frame(year=as.integer(format(tt,"%Y")), month=as.integer(format(tt,"%m")))
stopifnot(nrow(ym)==ncol(vals))
rows <- list()
for(i in seq_len(nrow(px))){
  rows[[i]] <- data.frame(x=px$x[i], y=px$y[i], year=ym$year, month=ym$month,
                          sm=as.numeric(vals[i,]))
}
out <- bind_rows(rows) |> filter(!is.na(sm))
write_parquet(out, OUT)
cat(sprintf("sm mean %.3f m3/m3 (Benin expect ~0.05-0.35)\n", mean(out$sm,na.rm=TRUE)))
cat(sprintf("wrote %s: %d rows, %d px, %d-%d\n", OUT, nrow(out),
    dplyr::n_distinct(paste(out$x,out$y)), min(out$year), max(out$year)))
cat("SM1 done. Next: SM2_anomaly.R\n")
