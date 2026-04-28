# =============================================================================
# 00. MASTER RUN SCRIPT — Phase 1 Pipeline
# =============================================================================
# Analysis Plan Reference: Phase 1 — Full workflow from raw ERA5 data to
#   detected drought events under all 45 configurations (Appendix A1).
#
# RUN ORDER:
#   Prerequisites (run once, local machine):
#     PET_data_prep/01. PET_Download.R      — download ERA5-Land PET via CDS API
#     PET_data_prep/02. PET_Inspect.R       — verify downloaded files
#     PET_data_prep/03. PET_Resample.R      — resample to ERA5 grid
#
#   Main pipeline (run on Databricks cluster):
#     01. SPI_Calculation.R                 — SPI-1/3/6/12 for all pixels
#     02a. SPEI_Calculation_Thornthwaite.R  — SPEI-1/3/6/12, Thornthwaite PET
#     02b. SPEI_Calculation_Penman.R        — SPEI-1/3/6/12, Penman-Monteith PET
#     03. Drought_Detection.R               — detect events, all 45 configs
#
# OUTPUTS (saved to ./Data/):
#   spi_results.rds
#   spei_thornthwaite_results.rds
#   spei_penman_results.rds
#   all_drought_events.rds
#   drought_summary_yearly.csv
#   drought_annual_summary.csv
#
# HOW TO USE THIS SCRIPT:
#   Option A — Run full pipeline end-to-end:
#     Set RUN_SPI, RUN_SPEI_T, RUN_SPEI_P, RUN_DETECTION all to TRUE,
#     then source this file.
#
#   Option B — Run individual steps only:
#     Set only the relevant flag(s) to TRUE, others to FALSE.
#     Useful when re-running detection after changing config settings,
#     without re-running the slow index calculation steps.
#
# NOTES:
#   - Scripts 01, 02a, 02b require Databricks cluster connection.
#     Ensure DATABRICKS_CLUSTER_ID is set in your .Renviron.
#   - Script 02b requires PET_data_prep steps to have been run first
#     and clipped NC files to exist in ./data/era5land_pet/clipped_era5grid/
#   - CDS API key required for PET_data_prep/01 — set CDS_API_KEY in .Renviron.
# =============================================================================


# -- Set working directory to Codes/ folder ----------------------------------
# source() uses relative paths — working directory must be the Codes/ folder.
# This line sets it automatically regardless of where R was launched from.
setwd(dirname(rstudioapi::getSourceEditorContext()$path))


# -- Package installer --------------------------------------------------------
# Checks all required packages across the full pipeline and installs any that
# are missing. Already-installed packages are skipped — safe to run every time.

# pysparklyr is NOT on CRAN — install from GitHub if missing
if (!requireNamespace("pysparklyr", quietly = TRUE)) {
  if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
  message("Installing pysparklyr from GitHub (mlverse/pysparklyr)...")
  remotes::install_github("mlverse/pysparklyr")
}

required_packages <- c(
  # Data access + Spark
  "sparklyr",
  "dbplyr",
  # Data manipulation
  "dplyr",
  "purrr",
  "lubridate",
  "stringr",
  "tidyr",
  # Drought index calculation
  "SPEI",
  # Spatial / raster
  "terra",
  "sf",
  # ERA5-Land download
  "ecmwfr",
  # Country boundaries
  "rnaturalearth",
  "rnaturalearthdata",
  # Plotting
  "ggplot2",
  "ggpubr",
  "colorspace",
  "tidyterra",
  "patchwork",
  # Utilities
  "here",
  "modelsummary",
  "rstudioapi"
)

missing_packages <- required_packages[
  !sapply(required_packages, requireNamespace, quietly = TRUE)
]

if (length(missing_packages) > 0) {
  message(sprintf("Installing %d missing package(s): %s",
                  length(missing_packages),
                  paste(missing_packages, collapse = ", ")))
  install.packages(missing_packages, dependencies = TRUE)
} else {
  message("All required packages already installed.")
}

# =============================================================================
# RUN FLAGS — set TRUE/FALSE to control which steps execute
# =============================================================================

RUN_SPI        <- TRUE   # Script 01: SPI calculation
RUN_SPEI_T     <- TRUE   # Script 02a: SPEI Thornthwaite
RUN_SPEI_P     <- TRUE   # Script 02b: SPEI Penman-Monteith (requires PET prep)
RUN_DETECTION  <- TRUE   # Script 03: Drought detection (requires 01 + 02a/02b)

# =============================================================================


# -- Run pipeline -------------------------------------------------------------

if (RUN_SPI) {
  message("\n================================================================")
  message("STEP 1: SPI Calculation")
  message("================================================================\n")
  source("01. SPI_Calculation.R")
}

if (RUN_SPEI_T) {
  message("\n================================================================")
  message("STEP 2a: SPEI Calculation — Thornthwaite PET")
  message("================================================================\n")
  source("02a. SPEI_Calculation_Thornthwaite.R")
}

if (RUN_SPEI_P) {
  message("\n================================================================")
  message("STEP 2b: SPEI Calculation — Penman-Monteith PET")
  message("  Requires: PET_data_prep/ steps completed first")
  message("================================================================\n")
  source("02b. SPEI_Calculation_Penman.R")
}

if (RUN_DETECTION) {
  message("\n================================================================")
  message("STEP 3: Drought Detection — All 45 Configurations")
  message("================================================================\n")
  source("03. Drought_Detection.R")
}

message("\n================================================================")
message("PIPELINE COMPLETE")
message("================================================================")
message("Outputs saved to ./Data/:")
if (RUN_SPI)       message("  spi_results.rds")
if (RUN_SPEI_T)    message("  spei_thornthwaite_results.rds")
if (RUN_SPEI_P)    message("  spei_penman_results.rds")
if (RUN_DETECTION) message("  all_drought_events.rds")
if (RUN_DETECTION) message("  drought_summary_yearly.csv")
if (RUN_DETECTION) message("  drought_annual_summary.csv")
