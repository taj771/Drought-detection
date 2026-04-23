library(ecmwfr)
library(terra)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)

wf_set_key(key = "ae3526ee-8caf-441a-b28a-503eb7e98cf2")

# ============================================================
# USER SETTINGS — only edit this section
# ============================================================

COUNTRY_NAMES <- c(
  "Argentina",
  "Greece",
  "Thailand",
  "Mexico",
  "South Africa",
  "Benin"
)

YEARS      <- as.character(2001:2002)
MONTHS     <- sprintf("%02d", 1:12)
OUTPUT_DIR <- "./data/era5land_pet/"

# ============================================================
# Auto bbox lookup
# ============================================================

get_bbox <- function(country_name) {
  
  country_sf <- tryCatch(
    ne_countries(country = country_name,
                 scale   = "medium",
                 returnclass = "sf"),
    error = function(e) NULL
  )
  
  if (is.null(country_sf) || nrow(country_sf) == 0) {
    all        <- ne_countries(scale = "medium", returnclass = "sf")
    country_sf <- all[
      grepl(country_name, all$name,      ignore.case = TRUE) |
        grepl(country_name, all$name_long, ignore.case = TRUE) |
        grepl(country_name, all$admin,     ignore.case = TRUE), ]
  }
  
  if (is.null(country_sf) || nrow(country_sf) == 0) {
    warning(sprintf("'%s' not found — skipping.", country_name))
    return(NULL)
  }
  
  bb <- st_bbox(country_sf)
  
  list(
    name      = country_sf$name[1],
    iso3      = country_sf$iso_a3[1],
    continent = country_sf$continent[1],
    bbox      = c(
      ceiling(bb["ymax"]),  # N
      floor(bb["xmin"]),    # W
      floor(bb["ymin"]),    # S
      ceiling(bb["xmax"])   # E
    )
  )
}

# ============================================================
# Look up bounding boxes
# ============================================================

message("Looking up bounding boxes...\n")

COUNTRIES <- list()

for (cn in COUNTRY_NAMES) {
  info <- get_bbox(cn)
  if (!is.null(info)) {
    COUNTRIES[[gsub(" ", "_", cn)]] <- info
    cat(sprintf(
      "  %-25s %-5s  N=%5.1f  W=%6.1f  S=%5.1f  E=%6.1f\n",
      info$name, info$iso3,
      info$bbox[1], info$bbox[2],
      info$bbox[3], info$bbox[4]
    ))
  }
}

# ============================================================
# Create output folder
# ============================================================

if (!dir.exists(OUTPUT_DIR)) {
  dir.create(OUTPUT_DIR, recursive = TRUE)
  message("Created: ", OUTPUT_DIR)
}

# ============================================================
# Download function
# ============================================================

download_pet <- function(country_info) {
  
  cn        <- country_info$name
  bbox      <- country_info$bbox
  safe_name <- gsub("[^A-Za-z0-9]", "_", cn)
  fname     <- sprintf("era5land_pet_%s_%s_%s.nc",
                       safe_name, min(YEARS), max(YEARS))
  fpath     <- file.path(OUTPUT_DIR, fname)
  
  # Skip if already downloaded
  if (file.exists(fpath)) {
    message(sprintf("[SKIP]  %-20s already exists — skipping.", cn))
    return(invisible(fpath))
  }
  
  message(sprintf(
    "\n[START] %s  (N=%.0f W=%.0f S=%.0f E=%.0f)",
    cn, bbox[1], bbox[2], bbox[3], bbox[4]
  ))
  
  request <- list(
    dataset_short_name = "reanalysis-era5-land-monthly-means",
    product_type       = "monthly_averaged_reanalysis",
    variable           = "potential_evaporation",
    year               = YEARS,
    month              = MONTHS,
    time               = "00:00",
    area               = bbox,
    data_format        = "netcdf",
    download_format    = "unarchived",
    target             = fname
  )
  
  wf_request(
    request  = request,
    transfer = TRUE,
    path     = OUTPUT_DIR,
    verbose  = TRUE
  )
  
  # Verify after download
  if (file.exists(fpath)) {
    r <- rast(fpath)
    message(sprintf(
      "[DONE]  %-20s %d layers  %.1f MB",
      cn, nlyr(r), file.size(fpath) / 1e6
    ))
  }
  
  return(invisible(fpath))
}

# ============================================================
# Run downloads — one country at a time
# Skips countries already downloaded if you rerun
# ============================================================

message("\n=== Starting downloads ===\n")

for (nm in names(COUNTRIES)) {
  download_pet(COUNTRIES[[nm]])
}

message("\n=== All downloads complete ===")
message("Files saved to: ", OUTPUT_DIR)

# List all downloaded files
files <- list.files(OUTPUT_DIR, pattern = "\\.nc$", full.names = TRUE)
cat(sprintf("\nDownloaded files (%d):\n", length(files)))
for (f in files) {
  cat(sprintf("  %s  (%.1f MB)\n",
              basename(f),
              file.size(f) / 1e6))
}