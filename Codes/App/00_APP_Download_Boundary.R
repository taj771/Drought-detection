# =============================================================================
# 00_APP_Download_Boundary.R
# GWSC Drought Monitor — Country Boundary Path Setup
# =============================================================================
# Locates country boundary shapefile on Databricks Volume and sets
# APP_SHAPEFILE_PATH for use by 01_APP_Index_Calculation.R.
#
# Shapefiles must be pre-uploaded to Volume before running pipeline:
#   /Volumes/.../app_outputs/shapefiles/<Country_Name>/boundary.gpkg
#
# To add a new country:
#   1. Locally: ne_countries(country="<Name>", returnclass="sf") → st_write(...)
#   2. Upload all 4 shapefile components (.shp/.dbf/.prj/.shx) to Volume path
#   3. Set APP_COUNTRY in 00_APP_MASTER.Rmd to match folder name (spaces→_)
#
# GWSC pipeline integration note:
#   When integrating, replace this script with direct path to carto-provided
#   shapefile in the project Volume. Logic in 01_APP_Index_Calculation.R
#   (st_read → st_intersects clip) is identical regardless of shapefile source.
#
# Requires in environment:
#   APP_COUNTRY — character, country name (spaces become underscores in path)
#   OUTPUT_DIR  — base output directory on Volume
#
# Produces in environment:
#   APP_SHAPEFILE_PATH — full path to boundary.gpkg on Volume
# =============================================================================

.country_slug      <- gsub(" ", "_", APP_COUNTRY)
APP_SHAPEFILE_PATH <- file.path(OUTPUT_DIR, "shapefiles", .country_slug, "boundary.gpkg")

if (!file.exists(APP_SHAPEFILE_PATH)) {
  stop(sprintf(paste0(
    "Shapefile not found: %s\n",
    "Upload boundary shapefile to Volume before running pipeline.\n",
    "Steps:\n",
    "  1. Locally: library(rnaturalearth); b <- ne_countries(country='%s', returnclass='sf')\n",
    "              st_write(b, 'boundary.gpkg')\n",
    "  2. databricks fs cp boundary.* dbfs:/Volumes/.../app_outputs/shapefiles/%s/ --overwrite"
  ), APP_SHAPEFILE_PATH, APP_COUNTRY, .country_slug))
}

cat(sprintf("APP_SHAPEFILE_PATH: %s\n", APP_SHAPEFILE_PATH))
