# ============================================================
# Resample Downloaded ERA5-Land PET NC Files to ERA5 Grid
# Assumes NC files already downloaded locally at 0.10 deg
# Resamples to match ERA5 normals table resolution (0.25 deg)
# Includes clipping to exact ERA5 extent to fix edge pixels
# ============================================================

library(here)
library(sparklyr)
library(dbplyr)
library(ggplot2)
library(colorspace)
library(tidyterra)
library(terra)
library(ggpubr)
library(tidyr)
library(stringr)
library(lubridate)
library(modelsummary)
library(dplyr)

# ============================================================
# STEP 1 — USER SETTINGS
# Only edit this section
# ============================================================

# Folder where your downloaded NC files are saved
RAW_DIR       <- "./data/era5land_pet/"

# Folder to save resampled output files
RESAMPLED_DIR <- "./data/era5land_pet/resampled_era5grid/"

# Folder to save clipped output files (final files to use)
CLIPPED_DIR   <- "./data/era5land_pet/clipped_era5grid/"

# Databricks catalog settings
PROJECT_FOLDER <- "xx_drought_index"
CATALOG_NAME   <- "projects"

# Years — used for layer date labelling
YEARS <- as.character(2001:2002)

# ============================================================
# STEP 2 — Find all NC files in raw folder
# ============================================================

message("=== STEP 2: Finding downloaded NC files ===\n")

nc_files <- list.files(
  path       = RAW_DIR,
  pattern    = "\\.nc$",
  full.names = TRUE
)

if (length(nc_files) == 0) {
  stop(sprintf("No NC files found in: %s\nCheck RAW_DIR path.", RAW_DIR))
}

cat(sprintf("Found %d NC file(s):\n", length(nc_files)))
for (f in nc_files) {
  size_mb <- round(file.size(f) / 1e6, 1)
  cat(sprintf("  %s  (%.1f MB)\n", basename(f), size_mb))
}

# ============================================================
# STEP 3 — Quick check of raw files before resampling
# ============================================================

message("\n=== STEP 3: Checking raw NC file structure ===\n")

for (f in nc_files) {
  
  r <- rast(f)
  
  cat(sprintf("\nFile    : %s\n", basename(f)))
  cat(sprintf("Layers  : %d\n",   nlyr(r)))
  cat(sprintf("Rows    : %d  Cols: %d\n", nrow(r), ncol(r)))
  cat(sprintf("Res     : %.4f deg (~%.1f km)\n",
              res(r)[1], res(r)[1] * 111))
  cat(sprintf("Extent  : x [%.3f to %.3f]  y [%.3f to %.3f]\n",
              ext(r)$xmin, ext(r)$xmax,
              ext(r)$ymin, ext(r)$ymax))
  cat(sprintf("Values  : min=%.5f  mean=%.5f  max=%.5f  (raw metres)\n",
              global(r[[1]], "min",  na.rm = TRUE)[[1]],
              global(r[[1]], "mean", na.rm = TRUE)[[1]],
              global(r[[1]], "max",  na.rm = TRUE)[[1]]))
  cat(sprintf("In mm   : mean=%.1f mm/month after * -1000\n",
              global(r[[1]], "mean", na.rm = TRUE)[[1]] * -1000))
}

# ============================================================
# STEP 4 — Get ERA5 grid from Databricks normals table
# Extracts exact x/y coordinates to use as resampling template
# ============================================================

message("\n=== STEP 4: Getting ERA5 grid from Databricks ===\n")

# Connect to Databricks
sc <- spark_connect(
  method     = "databricks_connect",
  cluster_id = Sys.getenv("DATABRICKS_CLUSTER_ID")
)

era5_normals <- tbl(sc,
                    in_catalog(CATALOG_NAME, PROJECT_FOLDER, "drought_index_dailysummary"))

# Extract unique coordinate values
cat("Pulling ERA5 coordinate grid from catalog...\n")

era5_coords <- era5_normals %>%
  select(x, y) %>%
  distinct() %>%
  collect()

x_vals <- sort(unique(round(era5_coords$x, 6)))
y_vals <- sort(unique(round(era5_coords$y, 6)))
x_res  <- round(median(diff(x_vals)), 6)
y_res  <- round(median(diff(y_vals)), 6)

cat(sprintf("ERA5 grid:\n"))
cat(sprintf("  X: %d values  %.4f to %.4f  step=%.4f\n",
            length(x_vals), min(x_vals), max(x_vals), x_res))
cat(sprintf("  Y: %d values  %.4f to %.4f  step=%.4f\n",
            length(y_vals), min(y_vals), max(y_vals), y_res))
cat(sprintf("  Resolution: %.4f deg (~%.1f km)\n",
            x_res, x_res * 111))

# Build ERA5 template raster from actual coordinates
# Half-cell offset so cell centres land on exact ERA5 values
era5_template <- rast(
  xmin       = min(x_vals) - (x_res / 2),
  xmax       = max(x_vals) + (x_res / 2),
  ymin       = min(y_vals) - (y_res / 2),
  ymax       = max(y_vals) + (y_res / 2),
  resolution = x_res,
  crs        = "EPSG:4326"
)

# ERA5 normals exact extent — used for clipping later
era5_exact_extent <- ext(
  min(x_vals) - (x_res / 2),
  max(x_vals) + (x_res / 2),
  min(y_vals) - (y_res / 2),
  max(y_vals) + (y_res / 2)
)

cat(sprintf("\nERA5 template raster built:\n"))
cat(sprintf("  Rows x Cols: %d x %d\n",
            nrow(era5_template), ncol(era5_template)))
cat(sprintf("  Cell centres start at: x=%.4f  y=%.4f\n",
            min(x_vals), min(y_vals)))
cat(sprintf("  Extent: x [%.4f to %.4f]  y [%.4f to %.4f]\n",
            era5_exact_extent$xmin, era5_exact_extent$xmax,
            era5_exact_extent$ymin, era5_exact_extent$ymax))

# ============================================================
# STEP 5 — Resample function
# Opens each NC file, converts units, resamples to ERA5 grid
# ============================================================

resample_nc_to_era5 <- function(nc_path, template) {
  
  fname     <- basename(nc_path)
  safe_name <- gsub("\\.nc$", "", fname)
  out_fname <- paste0(safe_name, "_era5grid.nc")
  out_path  <- file.path(RESAMPLED_DIR, out_fname)
  
  # Skip if already done
  if (file.exists(out_path)) {
    message(sprintf("[SKIP]  %s already resampled.", fname))
    return(invisible(out_path))
  }
  
  message(sprintf("\n[OPEN]  %s", fname))
  
  # Load raw NC file
  r_raw <- rast(nc_path)
  
  cat(sprintf("  Raw: %d layers  %.4f deg (~%.1f km)\n",
              nlyr(r_raw), res(r_raw)[1], res(r_raw)[1] * 111))
  
  # Convert units: metres (negative) → mm/month (positive)
  r_mm <- r_raw * -1000
  
  # Assign clean date-based layer names
  n_layers <- nlyr(r_mm)
  dates    <- seq(
    as.Date(paste0(min(YEARS), "-01-01")),
    by         = "month",
    length.out = n_layers
  )
  names(r_mm) <- format(dates, "pet_%Y_%m")
  
  cat(sprintf("  Layers: %s to %s\n",
              names(r_mm)[1], names(r_mm)[n_layers]))
  
  # Crop template to this file's extent with small buffer
  file_ext         <- ext(r_mm)
  template_cropped <- crop(
    template,
    ext(
      file_ext$xmin - x_res,
      file_ext$xmax + x_res,
      file_ext$ymin - y_res,
      file_ext$ymax + y_res
    )
  )
  
  # Resample: 0.10 deg → ERA5 grid (0.25 deg)
  # method = "average": averages ~6 ERA5-Land cells per ERA5 cell
  message(sprintf("[RESAMPLE] %.4f deg → %.4f deg ...",
                  res(r_mm)[1], res(template_cropped)[1]))
  
  r_resampled <- resample(r_mm, template_cropped, method = "average")
  
  # Save resampled file
  writeRaster(r_resampled, out_path, overwrite = TRUE)
  
  cat(sprintf(
    "[DONE]  %s\n  Before: %.4f deg (~%.1f km)\n  After : %.4f deg (~%.1f km)\n  Size  : %.1f MB\n  Saved : %s\n",
    fname,
    res(r_mm)[1],        res(r_mm)[1]        * 111,
    res(r_resampled)[1], res(r_resampled)[1] * 111,
    file.size(out_path) / 1e6,
    out_path
  ))
  
  return(invisible(out_path))
}

# ============================================================
# STEP 6 — Create output folders and run resampling
# ============================================================

for (d in c(RESAMPLED_DIR, CLIPPED_DIR)) {
  if (!dir.exists(d)) {
    dir.create(d, recursive = TRUE)
    message("Created: ", d)
  }
}

message("\n=== STEP 5: Resampling all NC files ===\n")

resampled_files <- lapply(nc_files, resample_nc_to_era5,
                          template = era5_template)
names(resampled_files) <- basename(nc_files)

# ============================================================
# STEP 7 — Clip resampled files to exact ERA5 normals extent
# This removes edge pixels that fall outside ERA5 coverage
# Fixes the CHECK results seen in alignment verification
# ============================================================

message("\n=== STEP 6: Clipping to exact ERA5 extent ===\n")

clip_to_era5 <- function(resampled_path, era5_extent) {
  
  if (is.null(resampled_path) || !file.exists(resampled_path)) {
    message(sprintf("[SKIP]  File not found: %s", resampled_path))
    return(invisible(NULL))
  }
  
  fname     <- basename(resampled_path)
  out_fname <- gsub("era5grid", "era5grid_clipped", fname)
  out_path  <- file.path(CLIPPED_DIR, out_fname)
  
  if (file.exists(out_path)) {
    message(sprintf("[SKIP]  %s already clipped.", fname))
    return(invisible(out_path))
  }
  
  message(sprintf("[CLIP]  %s ...", fname))
  
  r         <- rast(resampled_path)
  r_clipped <- crop(r, era5_extent)
  
  writeRaster(r_clipped, out_path, overwrite = TRUE)
  
  cat(sprintf(
    "  Before: %d x %d pixels  →  After: %d x %d pixels\n",
    nrow(r), ncol(r), nrow(r_clipped), ncol(r_clipped)
  ))
  
  return(invisible(out_path))
}

clipped_files <- lapply(
  resampled_files,
  clip_to_era5,
  era5_extent = era5_exact_extent
)
names(clipped_files) <- names(resampled_files)

# ============================================================
# STEP 8 — Verify coordinate alignment on CLIPPED files
# ============================================================

message("\n=== STEP 7: Verifying coordinate alignment ===\n")

cat(sprintf("%-45s %-15s %-15s %s\n",
            "File", "X match", "Y match", "Status"))
cat(strrep("-", 85), "\n")

all_pass <- TRUE

for (nm in names(clipped_files)) {
  
  fpath <- clipped_files[[nm]]
  
  if (is.null(fpath) || !file.exists(fpath)) {
    cat(sprintf("%-45s [FILE NOT FOUND]\n", nm))
    next
  }
  
  r    <- rast(fpath)
  df   <- as.data.frame(r[[1]], xy = TRUE, na.rm = TRUE)
  
  pet_x <- sort(unique(round(df$x, 4)))
  pet_y <- sort(unique(round(df$y, 4)))
  
  x_hit <- sum(round(pet_x, 4) %in% round(x_vals, 4))
  y_hit <- sum(round(pet_y, 4) %in% round(y_vals, 4))
  
  x_pct <- round(100 * x_hit / length(pet_x), 1)
  y_pct <- round(100 * y_hit / length(pet_y), 1)
  
  pass  <- x_pct >= 95 & y_pct >= 95
  if (!pass) all_pass <- FALSE
  
  cat(sprintf(
    "%-45s %d/%d (%.0f%%)  %d/%d (%.0f%%)  [%s]\n",
    nm,
    x_hit, length(pet_x), x_pct,
    y_hit, length(pet_y), y_pct,
    ifelse(pass, "PASS", "CHECK")
  ))
}

cat(strrep("-", 85), "\n")
cat(sprintf("Overall alignment: %s\n",
            ifelse(all_pass,
                   "ALL PASS — safe to use for SPEI",
                   "Some files need checking — review output above")))

# ============================================================
# STEP 9 — Test actual join with ERA5 normals
# Uses CLIPPED files — should now be 100% match
# ============================================================

message("\n=== STEP 8: Testing join with ERA5 normals ===\n")

for (nm in names(clipped_files)) {
  
  fpath <- clipped_files[[nm]]
  if (is.null(fpath) || !file.exists(fpath)) next
  
  cat(sprintf("File: %s\n", nm))
  
  # Load first layer of clipped PET
  r      <- rast(fpath)
  df_pet <- as.data.frame(r[[1]], xy = TRUE, na.rm = TRUE) %>%
    rename(pet_mm = 3) %>%
    mutate(x = round(x, 4), y = round(y, 4))
  
  # ---- FIX: extract filter bounds as plain R scalars BEFORE Spark call ----
  x_min_filter <- min(df_pet$x) - 0.1
  x_max_filter <- max(df_pet$x) + 0.1
  y_min_filter <- min(df_pet$y) - 0.1
  y_max_filter <- max(df_pet$y) + 0.1
  
  # Now Spark can use these as simple values — no list/function issue
  df_era5 <- era5_normals %>%
    filter(
      x >= x_min_filter,
      x <= x_max_filter,
      y >= y_min_filter,
      y <= y_max_filter
    ) %>%
    select(x, y) %>%
    distinct() %>%
    collect() %>%
    mutate(x = round(x, 4), y = round(y, 4))
  
  # Join on x and y
  df_joined <- inner_join(df_era5, df_pet, by = c("x", "y"))
  
  pct <- round(100 * nrow(df_joined) / nrow(df_era5), 1)
  
  cat(sprintf("  ERA5 pixels  : %s\n",
              format(nrow(df_era5),   big.mark = ",")))
  cat(sprintf("  PET pixels   : %s\n",
              format(nrow(df_pet),    big.mark = ",")))
  cat(sprintf("  Joined rows  : %s (%.1f%% match)\n",
              format(nrow(df_joined), big.mark = ","), pct))
  cat(sprintf("  Result       : %s\n\n",
              ifelse(pct >= 95,
                     "PASS — safe to join on x and y",
                     "WARNING — check alignment")))
}
# ============================================================
# STEP 10 — Final summary
# ============================================================

cat("\n========================================\n")
cat("RESAMPLING + CLIPPING COMPLETE\n")
cat("========================================\n")
cat(sprintf("Input folder   : %s\n", RAW_DIR))
cat(sprintf("Resampled dir  : %s\n", RESAMPLED_DIR))
cat(sprintf("Clipped dir    : %s  ← USE THESE FILES\n", CLIPPED_DIR))
cat(sprintf("Raw res        : 0.10 deg (~11 km)  ERA5-Land\n"))
cat(sprintf("Resampled res  : %.4f deg (~%.1f km)  ERA5 grid\n",
            x_res, x_res * 111))
cat(sprintf("Alignment      : %s\n",
            ifelse(all_pass, "ALL PASS", "SOME FILES NEED CHECKING")))
cat("----------------------------------------\n")

for (nm in names(clipped_files)) {
  fpath  <- clipped_files[[nm]]
  status <- if (!is.null(fpath) && file.exists(fpath)) {
    r <- rast(fpath)
    sprintf("OK  %d layers  %.4f deg  %.1f MB",
            nlyr(r), res(r)[1], file.size(fpath) / 1e6)
  } else "FAILED"
  cat(sprintf("  %-45s %s\n", nm, status))
}

cat("========================================\n")
cat("Next steps:\n")
cat("  1. Upload clipped files from CLIPPED_DIR to DBFS\n")
cat("  2. Join with ERA5 precipitation on x and y\n")
cat("  3. Compute water balance: P - PET\n")
cat("  4. Run SPEI calculation\n")