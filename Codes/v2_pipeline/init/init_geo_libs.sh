#!/bin/bash
# =============================================================================
# Cluster init script — system geo libraries for R terra / sf / rnaturalearth.
# =============================================================================
# The R packages terra, sf, units (a rnaturalearth dependency) compile against
# C system libraries that are not present on the DBR image, so their CRAN/PPM
# installs fail with "non-zero exit status". Install the dev headers here, on
# every node, BEFORE the cluster library installer runs the R packages.
#   terra -> GDAL + PROJ + GEOS
#   sf    -> GDAL + PROJ + GEOS
#   units -> udunits2
# =============================================================================
set -euo pipefail

export DEBIAN_FRONTEND=noninteractive
apt-get update -y
apt-get install -y --no-install-recommends \
  libgdal-dev \
  libproj-dev \
  libgeos-dev \
  libudunits2-dev \
  gdal-bin \
  proj-bin

# terra: the LATEST CRAN terra fails to compile against this image's Rcpp
# (RcppModule multi-arg .constructor unsupported). terra 1.7-78 builds cleanly
# on R 4.3.2 + the installed Rcpp, so pin it. rnaturalearth depends on terra.
#
# IMPORTANT: R installs are BEST-EFFORT — never abort boot. A non-zero Rscript
# exit (or `stop()`) under `set -e` kills the cluster with INIT_SCRIPT_FAILURE.
# So: disable -e for this block, never call stop(), and log outcome to a file
# for inspection. terra persists across restarts once built; rnaturalearth is
# guarded in script 04 if it ever misses.
set +e
Rscript -e '
  repo <- "https://packagemanager.posit.co/cran/__linux__/noble/latest"
  log <- function(...) cat(..., "\n", file="/dbfs/tmp/init_geo_libs.log", append=TRUE)
  if (!requireNamespace("remotes", quietly=TRUE))
    try(install.packages("remotes", repos=repo))

  # 1) terra 1.7-78 first (rnaturalearth depends on it).
  if (!requireNamespace("terra", quietly=TRUE))
    try(remotes::install_version("terra", version="1.7-78", repos=repo, upgrade="never"))
  log("terra:", requireNamespace("terra", quietly=TRUE))

  # 2) units (rnaturalearth dep, compiles vs udunits2). rnaturalearth itself is
  #    NOT installed here — at boot the Rscript dies before finishing it; it is
  #    bootstrapped on the driver in script 04 instead, where it builds cleanly.
  if (!requireNamespace("units", quietly=TRUE)) try(install.packages("units", repos=repo))
  log("units:", requireNamespace("units", quietly=TRUE))
' || true
set -e
