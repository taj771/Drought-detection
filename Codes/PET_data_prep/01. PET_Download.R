# =============================================================================
# 07. Download ERA5-Land PET (Penman-Monteith)
# =============================================================================
# Analysis Plan Reference: Section 1.2 — Calculating SPEI using ERA5 data
#                          PET Method (b): Penman-Monteith FAO-56
#
# This script implements Section 1.2 Step 1(ii):
#   Download pre-calculated monthly PET from ERA5-Land dataset via CDS API.
#   ERA5-Land PET is derived using the Penman-Monteith FAO-56 method.
#   Downloaded at 0.10° resolution — resampled to ERA5 0.25° grid in Script 09.
#
# Pipeline position:
#   07 (this) → 08 (inspect) → 09 (resample to ERA5 grid) → 02b (SPEI calc)
#
# Prerequisites:
#   - CDS API account at cds.climate.copernicus.eu
#   - API key stored in environment variable CDS_API_KEY (NOT hardcoded)
#     Add to .Renviron: CDS_API_KEY=your-key-here
#     Then restart R.
#
# Output: ./data/era5land_pet/<country>_<year_start>_<year_end>.nc
# =============================================================================



# -- Libraries ----------------------------------------------------------------
library(ecmwfr)
library(terra)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)


# -- CDS API authentication ---------------------------------------------------
# API key stored in environment variable — never hardcode credentials in scripts.
# Set in .Renviron: CDS_API_KEY=xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx
wf_set_key(key = Sys.getenv("CDS_API_KEY"))


# =============================================================================
# USER SETTINGS — edit this section only
# =============================================================================

# Countries to download PET for — must match natural earth country names
COUNTRY_NAMES <- c(
  "Argentina",
  "Greece",
  "Thailand",
  "Mexico",
  "South Africa",
  "Benin"
)

# Analysis Plan Section 1.2 Step 1: full period 1970–2023.
# Covers reference period (1970–1999) + comparison period (2000–2020) + buffer to 2023.
# ERA5-Land available from 1950 onward.
YEARS      <- as.character(1970:2023)
MONTHS     <- sprintf("%02d", 1:12)
OUTPUT_DIR <- "./data/era5land_pet/"

# =============================================================================


# -- Auto bounding box lookup -------------------------------------------------
# Queries Natural Earth for each country's spatial extent.
# Adds small buffer via ceiling/floor to ensure full coverage at boundaries.
get_bbox <- function(country_name) {
  country_sf <- tryCatch(
    ne_countries(country = country_name, scale = "medium", returnclass = "sf"),
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


# -- Look up bounding boxes ---------------------------------------------------
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


# -- Create output folder -----------------------------------------------------
if (!dir.exists(OUTPUT_DIR)) {
  dir.create(OUTPUT_DIR, recursive = TRUE)
  message("Created: ", OUTPUT_DIR)
}


# -- Download function --------------------------------------------------------
# Downloads monthly PET from ERA5-Land CDS dataset per country.
# Skips countries already downloaded — safe to rerun after interruption.
# ERA5-Land PET variable: "potential_evaporation" (units: metres, negative by convention)
# Unit conversion (metres → mm/month) handled in Script 08.
download_pet <- function(country_info) {
  cn        <- country_info$name
  bbox      <- country_info$bbox
  safe_name <- gsub("[^A-Za-z0-9]", "_", cn)
  fname     <- sprintf("era5land_pet_%s_%s_%s.nc", safe_name, min(YEARS), max(YEARS))
  fpath     <- file.path(OUTPUT_DIR, fname)

  if (file.exists(fpath)) {
    message(sprintf("[SKIP]  %-20s already exists.", cn))
    return(invisible(fpath))
  }

  message(sprintf("\n[START] %s  (N=%.0f W=%.0f S=%.0f E=%.0f)",
                  cn, bbox[1], bbox[2], bbox[3], bbox[4]))

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

  # Retry up to 3 times with backoff — CDS API v2 rate limits polling
  for (attempt in 1:3) {
    result <- tryCatch({
      wf_request(
        request  = request,
        transfer = TRUE,
        path     = OUTPUT_DIR,
        verbose  = TRUE
      )
      "ok"
    }, error = function(e) {
      message(sprintf("[RETRY] attempt %d failed: %s", attempt, e$message))
      if (attempt < 3) Sys.sleep(30 * attempt)
      "error"
    })
    if (result == "ok") break
  }

  if (file.exists(fpath)) {
    r <- rast(fpath)
    message(sprintf("[DONE]  %-20s %d layers  %.1f MB",
                    cn, nlyr(r), file.size(fpath) / 1e6))
  }

  return(invisible(fpath))
}


# -- Run downloads ------------------------------------------------------------
# Downloads one country at a time. Skips already-downloaded files on rerun.
message("\n=== Starting downloads ===\n")

for (nm in names(COUNTRIES)) {
  download_pet(COUNTRIES[[nm]])
  Sys.sleep(10)   # pause between country requests — avoid CDS rate limit
}

message("\n=== Downloads complete ===")
message("Files saved to: ", OUTPUT_DIR)

files <- list.files(OUTPUT_DIR, pattern = "\\.nc$", full.names = TRUE)
cat(sprintf("\nDownloaded files (%d):\n", length(files)))
for (f in files) {
  cat(sprintf("  %s  (%.1f MB)\n", basename(f), file.size(f) / 1e6))
}

# Next step: run Script 08 to inspect and verify downloaded files.
