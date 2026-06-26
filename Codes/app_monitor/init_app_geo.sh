#!/bin/bash
# =============================================================================
# app_monitor cluster init — geo libraries for R terra / sf / rnaturalearth.
# =============================================================================
# Separate from v2's init_geo_libs.sh because the app personal cluster runs a
# DIFFERENT DBR (16.4 vs v2's 15.4). Everything here is BEST-EFFORT: a non-zero
# exit under `set -e` kills the cluster with INIT_SCRIPT_FAILURE. apt-get can
# fail on DBR 16.4 (different Ubuntu base than the `noble` PPM repo below), and
# the system geo libs are often already on the image — so apt must NOT abort.
# =============================================================================
set +e

export DEBIAN_FRONTEND=noninteractive
apt-get update -y || true
apt-get install -y --no-install-recommends \
  libgdal-dev libproj-dev libgeos-dev libudunits2-dev gdal-bin proj-bin || true

# terra: pin 1.7-78 (builds against the image's Rcpp; latest can fail). Logged
# to /dbfs/tmp for inspection; never call stop() so boot survives. rnaturalearth
# is bootstrapped on the driver in 01_APP_Index_Calculation.R if missing.
Rscript -e '
  repo <- "https://packagemanager.posit.co/cran/__linux__/noble/latest"
  log <- function(...) cat(..., "\n", file="/dbfs/tmp/init_app_geo.log", append=TRUE)
  if (!requireNamespace("remotes", quietly=TRUE)) try(install.packages("remotes", repos=repo))
  if (!requireNamespace("terra", quietly=TRUE))
    try(remotes::install_version("terra", version="1.7-78", repos=repo, upgrade="never"))
  log("terra:", requireNamespace("terra", quietly=TRUE))
  if (!requireNamespace("sf", quietly=TRUE)) try(install.packages("sf", repos=repo))
  log("sf:", requireNamespace("sf", quietly=TRUE))
  if (!requireNamespace("units", quietly=TRUE)) try(install.packages("units", repos=repo))
  log("units:", requireNamespace("units", quietly=TRUE))
' || true

# Always exit 0 — boot must never fail on best-effort package installs.
exit 0
