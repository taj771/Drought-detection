#!/bin/bash
# Init script for cluster 0616-154127-izvo5r0w
# Installs system libraries required by R packages: sf, terra, units
set -e
apt-get update -qq
apt-get install -y --no-install-recommends \
  libudunits2-dev \
  libgdal-dev \
  libgeos-dev \
  libproj-dev \
  libsqlite3-dev
echo "sf system dependencies installed successfully"
