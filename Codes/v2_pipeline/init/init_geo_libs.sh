#!/bin/bash
# =============================================================================
# Cluster init script — system geo libraries for R terra / sf / rnaturalearth.
# =============================================================================
# The R packages terra, sf, units (a rnaturalearth dependency) compile against
# C system libraries that are not present on the DBR image, so their CRAN/PPM
# installs fail with "non-zero exit status". Install the dev headers here,
# BEFORE the R packages.
#   terra -> GDAL + PROJ + GEOS
#   sf    -> GDAL + PROJ + GEOS
#   units -> udunits2
#
# Compiling terra/units/sf from source takes ~20 min, and a job cluster boots
# fresh on every run. So the compiled R packages are archived once to the
# Volume (build_r_lib_cache.py) and this script unpacks that archive in
# seconds. If the archive is missing or broken it falls back to compiling, so
# the cache only ever makes boot faster, never breaks it.
#
# Modes:
#   normal boot          driver: restore cache, else compile. workers: skip.
#   BUILD_R_LIB_TO=<dir> compile every package into <dir> and archive it to
#                        the Volume (set by build_r_lib_cache.py only).
# =============================================================================
set -euo pipefail

BUILD_DIR="${BUILD_R_LIB_TO:-}"

# Only the driver runs R in this pipeline (step 04 reads parquet locally with
# arrow, no SparkR/sparklyr on executors), so workers skip the geo setup.
if [[ -z "$BUILD_DIR" && "${DB_IS_DRIVER:-TRUE}" != "TRUE" ]]; then
  echo "init_geo_libs: worker node, skipping R geo setup"
  exit 0
fi

LOG=/dbfs/tmp/init_geo_libs.log
log() { echo "init_geo_libs: $*"; echo "$(date -u +%FT%TZ) $*" >> "$LOG" 2>/dev/null || true; }

export DEBIAN_FRONTEND=noninteractive
apt-get update -y
apt-get install -y --no-install-recommends \
  libgdal-dev \
  libproj-dev \
  libgeos-dev \
  libudunits2-dev \
  gdal-bin \
  proj-bin

# The archive is only valid for the DBR image + R version it was built on
# (compiled against that image's Rcpp/GDAL), so both are in its name. A runtime
# upgrade simply misses the cache and compiles until the cache is rebuilt.
PKGS='c("terra", "units", "patchwork", "sf", "SPEI", "rnaturalearthdata", "rnaturalearth")'
DBR_VER=$(cat /databricks/DBR_VERSION 2>/dev/null || echo "${DATABRICKS_RUNTIME_VERSION:-unknown}")
R_VER=$(Rscript -e 'cat(as.character(getRversion()))')
CACHE_DIR=/Volumes/projects/gwsc_drought/drought_outputs/_r_lib_cache
CACHE_TAR="$CACHE_DIR/r_geo_lib_dbr${DBR_VER}_R${R_VER}.tar.gz"
SITE_LIB=$(Rscript -e 'cat(.libPaths()[1])')

# -- Normal boot: restore from cache ------------------------------------------
if [[ -z "$BUILD_DIR" ]]; then
  if [[ -f "$CACHE_TAR" ]] \
     && tar -xzf "$CACHE_TAR" -C "$SITE_LIB" \
     && Rscript -e "q(status = as.integer(!all(sapply($PKGS, requireNamespace, quietly = TRUE))))"; then
    log "R packages restored from $CACHE_TAR"
    exit 0
  fi
  log "no usable cache at $CACHE_TAR, compiling from source"
fi

# terra: the LATEST CRAN terra fails to compile against this image's Rcpp
# (RcppModule multi-arg .constructor unsupported). terra 1.7-78 builds cleanly
# on R 4.3.2 + the installed Rcpp, so pin it. rnaturalearth depends on terra.
#
# IMPORTANT: on normal boot R installs are BEST-EFFORT — never abort boot. A
# non-zero Rscript exit (or `stop()`) under `set -e` kills the cluster with
# INIT_SCRIPT_FAILURE. So: disable -e for this block, never call stop(), and
# log outcome to a file for inspection. rnaturalearth is guarded in script 04
# if it ever misses. Build mode checks the result explicitly afterwards.
set +e
Rscript -e '
  repo <- "https://packagemanager.posit.co/cran/__linux__/noble/latest"
  log <- function(...) cat(..., "\n", file="/dbfs/tmp/init_geo_libs.log", append=TRUE)

  # Build mode installs into an empty directory so it can be archived; a
  # package counts as present only once it is in that directory.
  build <- Sys.getenv("BUILD_R_LIB_TO")
  lib <- if (nzchar(build)) build else .libPaths()[1]
  .libPaths(c(lib, .libPaths()))
  have <- function(p) {
    if (nzchar(build)) dir.exists(file.path(lib, p)) else requireNamespace(p, quietly=TRUE)
  }

  if (!requireNamespace("remotes", quietly=TRUE))
    try(install.packages("remotes", lib=lib, repos=repo))

  # 1) terra 1.7-78 first (rnaturalearth depends on it).
  if (!have("terra"))
    try(remotes::install_version("terra", version="1.7-78", lib=lib, repos=repo, upgrade="never"))
  log("terra:", have("terra"))

  # 2) units (rnaturalearth + sf dep, compiles vs udunits2).
  #    Pinned: units >= 0.9 requires Rcpp >= 1.1.0, but this image ships Rcpp
  #    1.0.13. units 0.8-5 is the last release that builds against 1.0.13.
  #    Same failure mode as the terra pin above.
  if (!have("units"))
    try(remotes::install_version("units", version="0.8-5", lib=lib, repos=repo, upgrade="never"))
  log("units:", have("units"))

  # 3) Remaining R packages needed by the R step (04). These must be
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
  if (!have("patchwork"))
    try(remotes::install_version("patchwork", version="1.2.0", lib=lib, repos=repo, upgrade="never"))
  log("patchwork:", have("patchwork"))

  for (p in c("sf", "SPEI", "rnaturalearthdata", "rnaturalearth")) {
    if (!have(p))
      try(install.packages(p, lib=lib, repos=repo, upgrade="never"))
    log(paste0(p, ":"), have(p))
  }
' || true
set -e

# -- Build mode: archive the compiled library to the Volume -------------------
if [[ -n "$BUILD_DIR" ]]; then
  # Every package must load from the build directory, or the archive would
  # silently be incomplete and every boot would fall back to compiling.
  Rscript -e ".libPaths(c('$BUILD_DIR', .libPaths())); ok <- sapply($PKGS, requireNamespace, quietly = TRUE); print(ok); q(status = as.integer(!all(ok)))"
  mkdir -p "$CACHE_DIR"
  tar -czf "$CACHE_TAR.tmp" -C "$BUILD_DIR" .
  mv -f "$CACHE_TAR.tmp" "$CACHE_TAR"
  log "built cache $CACHE_TAR ($(du -h "$CACHE_TAR" | cut -f1))"
  echo "CACHE_TAR=$CACHE_TAR"
fi
