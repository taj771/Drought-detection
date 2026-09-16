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

  # 2) units (rnaturalearth + sf dep, compiles vs udunits2).
  #    Pinned: units >= 0.9 requires Rcpp >= 1.1.0, but this image ships Rcpp
  #    1.0.13. units 0.8-5 is the last release that builds against 1.0.13.
  #    Same failure mode as the terra pin above.
  if (!requireNamespace("units", quietly=TRUE))
    try(remotes::install_version("units", version="0.8-5", repos=repo, upgrade="never"))
  log("units:", requireNamespace("units", quietly=TRUE))

  # 3) Remaining R packages needed by the v2 R steps (04, 06-09). These must be
  #    installed HERE rather than as cluster libraries: the cluster library
  #    installer resolves against CRAN latest and re-resolves dependencies, which
  #    drags in units/Rcpp versions this image cannot build (ERROR_USER_R_INSTALLATION).
  #    PPM serves prebuilt binaries for this image, so these are fast and safe.
  # patchwork: pinned for the same reason as terra/units, and installed FIRST.
  # patchwork >= 1.3.x calls ggplot2::is_ggplot(), which only exists in
  # ggplot2 >= 3.5.2; this image ships an older ggplot2, so the install dies at
  # lazy-load with an error about is_ggplot not being exported by ggplot2.
  # 1.2.0 is the last release that builds against the ggplot2 in this image.
  # It must go in before the loop below: packages there pull patchwork in as a
  # dependency and would fetch the broken latest version first.
  if (!requireNamespace("patchwork", quietly=TRUE))
    try(remotes::install_version("patchwork", version="1.2.0", repos=repo, upgrade="never"))
  log("patchwork:", requireNamespace("patchwork", quietly=TRUE))

  for (p in c("sf", "SPEI", "rnaturalearthdata", "rnaturalearth")) {
    if (!requireNamespace(p, quietly=TRUE))
      try(install.packages(p, repos=repo, upgrade="never"))
    log(paste0(p, ":"), requireNamespace(p, quietly=TRUE))
  }
' || true
set -e
