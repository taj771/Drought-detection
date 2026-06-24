# =============================================================================
# 00_APP_Download_Boundary.R
# GWSC Drought Monitor — Country Boundary Path Setup
# =============================================================================
# Locates country boundary .gpkg on Databricks Volume and sets
# APP_SHAPEFILE_PATH for use by 01_APP_Index_Calculation.R.
#
# Shapefiles must be uploaded to Volume BEFORE running pipeline.
# Use run_app.ps1 from project root — it handles download + upload + deploy.
#
# GWSC pipeline integration note:
#   When integrating, set APP_SHAPEFILE_PATH directly to carto-provided
#   shapefile path on Volume and skip this script entirely.
#
# Requires in environment:
#   APP_COUNTRY — character, country name (e.g. "India", "Sri Lanka")
#   OUTPUT_DIR  — base output directory on Volume
#
# Produces in environment:
#   APP_SHAPEFILE_PATH — full path to boundary.gpkg on Volume
# =============================================================================

.country_slug      <- gsub(" ", "_", APP_COUNTRY)
APP_SHAPEFILE_PATH <- file.path(OUTPUT_DIR, "shapefiles", .country_slug, "boundary.gpkg")

if (!file.exists(APP_SHAPEFILE_PATH)) {
  stop(sprintf(paste0(
    "Boundary not found: %s\n",
    "Run from project root before deploying:\n",
    "  .\\run_app.ps1 -Country \"%s\"\n",
    "This downloads the boundary and uploads it to the Volume automatically."
  ), APP_SHAPEFILE_PATH, APP_COUNTRY))
}

cat(sprintf("Boundary found: %s\n", APP_SHAPEFILE_PATH))
