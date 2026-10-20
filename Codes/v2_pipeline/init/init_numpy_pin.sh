#!/bin/bash
# =============================================================================
# Cluster init script — pin numpy<2 BEFORE installing climate-indices.
# =============================================================================
# Why: DBR 15.4 ships pyarrow built against numpy 1.x. Installing
# climate-indices as a cluster PyPI library pulls numpy>=2, which breaks the
# prebuilt pyarrow on workers:
#   ImportError: numpy.core.multiarray failed to import
# Pinning numpy<2 first keeps the prebuilt pyarrow ABI valid; climate-indices
# runs fine on numpy 1.26.
#
# Runs on driver AND every worker, so Pandas UDF workers get the pinned numpy.
# Remove the climate-indices PyPI cluster library before attaching this script
# (it re-pulls numpy 2.x and re-breaks pyarrow).
#
# climate-indices is pinned to 3.0.0. That release fixed the gamma SPI
# probability of zero rainfall: it is now measured on the calibration period's
# non-missing values, where 2.x counted every year of the record with missing
# years in the denominator. SPI changes in zero-precipitation months. Pinned so
# a fresh cluster never picks up a newer release without anyone deciding to.
# Keep this pin in step with CLIMATE_INDICES in scripts 01, 02a, 02b and 05.
# =============================================================================
set -euo pipefail

/databricks/python/bin/pip install \
  "numpy<2" \
  "climate-indices==3.0.0"
