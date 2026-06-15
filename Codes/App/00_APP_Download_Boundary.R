# =============================================================================
# 00_APP_Download_Boundary.R
# GWSC Drought Monitor — Country Boundary Download
# =============================================================================
# Downloads country boundary from Natural Earth (rnaturalearth package) and
# saves as shapefile to Databricks Volume so all scripts can access it.
#
# NOTE: When integrating into full GWSC pipeline, skip this script entirely.
# Replace shapefile_path with the carto-provided shapefile path in the Volume.
# The spatial clip logic in 01_APP_Index_Calculation.R is identical either way.
#
# Called by: 00_APP_MASTER.Rmd (only runs if shapefile not already present)
#
# Requires in environment:
#   APP_COUNTRY   — character, country name matching Natural Earth admin-0
#   OUTPUT_DIR    — base output directory on Volume
#
# Produces:
#   <OUTPUT_DIR>/shapefiles/<country_slug>/boundary.shp (+ .dbf/.prj/.shx)
#   Sets in environment: APP_SHAPEFILE_PATH — path to boundary.shp
# =============================================================================

library(rnaturalearth)
library(sf)

.country_slug <- gsub(" ", "_", APP_COUNTRY)
.shp_dir      <- file.path(OUTPUT_DIR, "shapefiles", .country_slug)
.shp_path     <- file.path(.shp_dir, "boundary.shp")

if (file.exists(.shp_path)) {
  cat(sprintf("Shapefile already exists: %s\n", .shp_path))
} else {
  cat(sprintf("Downloading boundary for %s from Natural Earth...\n", APP_COUNTRY))
  dir.create(.shp_dir, showWarnings = FALSE, recursive = TRUE)

  boundary <- ne_countries(country = APP_COUNTRY, returnclass = "sf", scale = "medium")

  if (nrow(boundary) == 0) {
    stop(sprintf(
      "Country '%s' not found in Natural Earth admin-0 data.\n",
      "Check spelling — use English name e.g. 'Sri Lanka', 'South Africa'.",
      APP_COUNTRY
    ))
  }

  boundary <- boundary[, c("name", "iso_a3", "geometry")]
  st_write(boundary, .shp_path, quiet = TRUE)
  cat(sprintf("Saved: %s\n", .shp_path))
}

APP_SHAPEFILE_PATH <- .shp_path
cat(sprintf("APP_SHAPEFILE_PATH set: %s\n", APP_SHAPEFILE_PATH))
