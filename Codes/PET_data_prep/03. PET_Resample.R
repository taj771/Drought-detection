# =============================================================================
# 05. Resample ERA5-Land PET to ERA5 Grid
# =============================================================================
# Analysis Plan Reference: Section 1.2 — Calculating SPEI using ERA5 data
#                          PET Method (b): Penman-Monteith FAO-56
#
# Purpose: Resample downloaded ERA5-Land PET files (0.10 deg) to match
#          ERA5 precipitation grid (0.25 deg) so PET and precip can be
#          joined on x/y coordinates for water balance calculation.
#
# Steps:
#   1. Find downloaded NC files from Script 03
#   2. Check raw file structure
#   3. Get ERA5 coordinate grid from Databricks (defines resample target)
#   4. Build ERA5 template raster from actual coordinates
#   5. Resample: 0.10 deg → 0.25 deg (method = "average")
#   6. Clip to exact ERA5 extent (removes edge pixels outside ERA5 coverage)
#   7. Verify coordinate alignment (>= 95% pixel match required)
#   8. Test join with ERA5 normals table
#
# Pipeline position:
#   03 (download) → 04 (inspect) → 05 (this) → 02b (SPEI calc)
#
# Output: ./data/era5land_pet/clipped_era5grid/*_era5grid_clipped.nc
#         USE THESE FILES in Script 02b — not the raw or resampled files.
# =============================================================================


# -- Libraries ----------------------------------------------------------------
library(here)
library(terra)
library(dplyr)
library(lubridate)


# =============================================================================
# USER SETTINGS — edit this section only
# =============================================================================

# Input: raw NC files from Script 03
raw_dir       <- "./data/era5land_pet/"

# Intermediate: resampled files (before clipping)
resampled_dir <- "./data/era5land_pet/resampled_era5grid/"

# Output: clipped files aligned to ERA5 grid — USE THESE in Script 02b
clipped_dir   <- "./data/era5land_pet/clipped_era5grid/"

# Databricks catalog settings — must match Scripts 01/02a
project_folder <- "xx_drought_index"
catalog_name   <- "projects"

# Year range — used for layer date labelling, must match Script 03
years <- as.character(1970:2023)

# =============================================================================


# -- Step 1: Find downloaded NC files -----------------------------------------
message("=== Step 1: Finding downloaded NC files ===\n")

nc_files <- list.files(raw_dir, pattern = "\\.nc$", full.names = TRUE)

if (length(nc_files) == 0) {
  stop(sprintf("No NC files found in: %s\nRun Script 03 first.", raw_dir))
}

cat(sprintf("Found %d NC file(s):\n", length(nc_files)))
for (f in nc_files) {
  cat(sprintf("  %s  (%.1f MB)\n", basename(f), file.size(f) / 1e6))
}


# -- Step 2: Check raw file structure -----------------------------------------
message("\n=== Step 2: Checking raw NC file structure ===\n")

for (f in nc_files) {
  r <- rast(f)
  cat(sprintf("\nFile    : %s\n", basename(f)))
  cat(sprintf("Layers  : %d\n",   nlyr(r)))
  cat(sprintf("Rows    : %d  Cols: %d\n", nrow(r), ncol(r)))
  cat(sprintf("Res     : %.4f deg (~%.1f km)\n", res(r)[1], res(r)[1] * 111))
  cat(sprintf("Extent  : x [%.3f to %.3f]  y [%.3f to %.3f]\n",
              ext(r)$xmin, ext(r)$xmax, ext(r)$ymin, ext(r)$ymax))
  cat(sprintf("Values  : min=%.5f  mean=%.5f  max=%.5f  (raw metres)\n",
              global(r[[1]], "min",  na.rm = TRUE)[[1]],
              global(r[[1]], "mean", na.rm = TRUE)[[1]],
              global(r[[1]], "max",  na.rm = TRUE)[[1]]))
  cat(sprintf("In mm   : mean=%.1f mm/month after * -1000\n",
              global(r[[1]], "mean", na.rm = TRUE)[[1]] * -1000))
}


# -- Step 3: Build ERA5 0.25 deg template raster ------------------------------
# ERA5 uses a standard global 0.25 deg grid with cell centres at
# x = -179.875, -179.625, ... 179.875  (step 0.25, offset 0.125 from integer)
# y =  -89.875,  -89.625, ...  89.875  (step 0.25, offset 0.125 from integer)
# No Databricks connection needed — grid spec is fixed.
message("\n=== Step 3: Building ERA5 0.25 deg template raster ===\n")

x_res <- 0.25
y_res <- 0.25

era5_template <- rast(
  xmin       = -180,
  xmax       =  180,
  ymin       =  -90,
  ymax       =   90,
  resolution = 0.25,
  crs        = "EPSG:4326"
)

era5_exact_extent <- ext(-180, 180, -90, 90)

cat(sprintf("ERA5 template raster:\n"))
cat(sprintf("  Rows x Cols : %d x %d\n", nrow(era5_template), ncol(era5_template)))
cat(sprintf("  Resolution  : %.4f deg\n", x_res))


# -- Step 5: Resample function ------------------------------------------------
# Converts units, assigns clean layer names, resamples to ERA5 grid.
# method = "average": averages ~6 ERA5-Land cells (0.10 deg) per ERA5 cell (0.25 deg).
resample_nc_to_era5 <- function(nc_path, template) {

  fname     <- basename(nc_path)
  safe_name <- gsub("\\.nc$", "", fname)
  out_fname <- paste0(safe_name, "_era5grid.nc")
  out_path  <- file.path(resampled_dir, out_fname)

  if (file.exists(out_path)) {
    message(sprintf("[SKIP]  %s already resampled.", fname))
    return(invisible(out_path))
  }

  message(sprintf("\n[OPEN]  %s", fname))

  r_raw <- rast(nc_path)
  r_mm  <- r_raw * -1000  # metres (negative) → mm/month (positive)

  n_layers <- nlyr(r_mm)
  dates    <- seq(as.Date(paste0(min(years), "-01-01")),
                  by = "month", length.out = n_layers)
  names(r_mm) <- format(dates, "pet_%Y_%m")

  cat(sprintf("  Layers: %s to %s\n", names(r_mm)[1], names(r_mm)[n_layers]))

  # Crop template to file extent with small buffer before resampling
  file_ext         <- ext(r_mm)
  template_cropped <- crop(
    template,
    ext(file_ext$xmin - x_res, file_ext$xmax + x_res,
        file_ext$ymin - y_res, file_ext$ymax + y_res)
  )

  message(sprintf("[RESAMPLE] %.4f deg → %.4f deg ...",
                  res(r_mm)[1], res(template_cropped)[1]))

  r_resampled <- resample(r_mm, template_cropped, method = "average")
  writeRaster(r_resampled, out_path, overwrite = TRUE)

  cat(sprintf("[DONE]  %s  →  %.4f deg  %.1f MB\n",
              fname, res(r_resampled)[1], file.size(out_path) / 1e6))

  return(invisible(out_path))
}


# -- Step 5: Create output folders and run resampling -------------------------
for (d in c(resampled_dir, clipped_dir)) {
  if (!dir.exists(d)) {
    dir.create(d, recursive = TRUE)
    message("Created: ", d)
  }
}

message("\n=== Step 5: Resampling all NC files ===\n")

resampled_files <- lapply(nc_files, resample_nc_to_era5, template = era5_template)
names(resampled_files) <- basename(nc_files)


# -- Step 6: Clip to exact ERA5 extent ----------------------------------------
# Removes edge pixels that fall outside ERA5 coverage area.
message("\n=== Step 6: Clipping to exact ERA5 extent ===\n")

clip_to_era5 <- function(resampled_path, era5_extent) {

  if (is.null(resampled_path) || !file.exists(resampled_path)) {
    message(sprintf("[SKIP]  File not found: %s", resampled_path))
    return(invisible(NULL))
  }

  fname     <- basename(resampled_path)
  out_fname <- gsub("era5grid", "era5grid_clipped", fname)
  out_path  <- file.path(clipped_dir, out_fname)

  if (file.exists(out_path)) {
    message(sprintf("[SKIP]  %s already clipped.", fname))
    return(invisible(out_path))
  }

  message(sprintf("[CLIP]  %s ...", fname))

  r         <- rast(resampled_path)
  r_clipped <- crop(r, era5_extent)

  writeRaster(r_clipped, out_path, overwrite = TRUE)

  cat(sprintf("  Before: %d x %d pixels  →  After: %d x %d pixels\n",
              nrow(r), ncol(r), nrow(r_clipped), ncol(r_clipped)))

  return(invisible(out_path))
}

clipped_files <- lapply(resampled_files, clip_to_era5,
                        era5_extent = era5_exact_extent)
names(clipped_files) <- names(resampled_files)


# -- Step 7: Verify coordinate alignment on clipped files ---------------------
# Checks that clipped PET pixel centres match ERA5 pixel centres (>= 95% required).
message("\n=== Step 7: Verifying coordinate alignment ===\n")

cat(sprintf("%-45s %-15s %-15s %s\n", "File", "X match", "Y match", "Status"))
cat(strrep("-", 85), "\n")

all_pass <- TRUE

for (nm in names(clipped_files)) {

  fpath <- clipped_files[[nm]]

  if (is.null(fpath) || !file.exists(fpath)) {
    cat(sprintf("%-45s [FILE NOT FOUND]\n", nm))
    next
  }

  r      <- rast(fpath)
  df     <- as.data.frame(r[[1]], xy = TRUE, na.rm = TRUE)
  pet_x  <- sort(unique(round(df$x, 4)))
  pet_y  <- sort(unique(round(df$y, 4)))
  era5_x <- seq(-179.875, 179.875, by = 0.25)
  era5_y <- seq( -89.875,  89.875, by = 0.25)
  x_hit  <- sum(round(pet_x, 4) %in% round(era5_x, 4))
  y_hit  <- sum(round(pet_y, 4) %in% round(era5_y, 4))
  x_pct  <- round(100 * x_hit / length(pet_x), 1)
  y_pct  <- round(100 * y_hit / length(pet_y), 1)
  pass   <- x_pct >= 95 & y_pct >= 95
  if (!pass) all_pass <- FALSE

  cat(sprintf("%-45s %d/%d (%.0f%%)  %d/%d (%.0f%%)  [%s]\n",
              nm, x_hit, length(pet_x), x_pct,
              y_hit, length(pet_y), y_pct,
              ifelse(pass, "PASS", "CHECK")))
}

cat(strrep("-", 85), "\n")
cat(sprintf("Overall alignment: %s\n",
            ifelse(all_pass,
                   "ALL PASS — safe to use for SPEI",
                   "Some files need checking — review output above")))


# -- Step 8: skipped — Databricks join test not needed for local resample -----
message("\n=== Step 8: Skipped (ERA5 join verified via coordinate alignment above) ===\n")


# -- Summary ------------------------------------------------------------------
cat("\n========================================\n")
cat("RESAMPLING + CLIPPING COMPLETE\n")
cat("========================================\n")
cat(sprintf("Input     : %s\n", raw_dir))
cat(sprintf("Resampled : %s\n", resampled_dir))
cat(sprintf("Clipped   : %s  <- USE THESE IN SCRIPT 02b\n", clipped_dir))
cat(sprintf("Raw res   : 0.10 deg (~11 km)  ERA5-Land\n"))
cat(sprintf("Output res: %.4f deg (~%.1f km)  ERA5 grid\n", x_res, x_res * 111))
cat(sprintf("Alignment : %s\n",
            ifelse(all_pass, "ALL PASS", "SOME FILES NEED CHECKING")))
cat("========================================\n")

# Next step: run Script 02b to calculate SPEI using Penman-Monteith PET.
