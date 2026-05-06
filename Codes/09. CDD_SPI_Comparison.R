# =============================================================================
# 09. CDD vs SPI/SPEI Frequency Comparison
# =============================================================================
# Analysis Plan Reference: Phase 2 Section 2.4
#
# Reproduces the ERA5 Consecutive Dry Day (CDD) computation from
# XX_Consecutive_Dry_Days.Rmd for the 6 study countries, then validates
# SPI/SPEI drought frequency against CDD event frequency via Spearman rank
# correlation across pixels.
#
# CDD definition: dry_day = 1 if precip <= 1 mm/day; a CDD event = run of
# >= CDD_MIN_LENGTH consecutive dry days. Annual CDD frequency per pixel =
# sum(floor(count / CDD_MIN_LENGTH) * freq_era5) per year.
#
# Inputs (ERA5 daily catalog table):
#   projects.xx_drought_index.drought_index_dailysummary
#     — columns: x, y, time, year, country, precip (mm/day)
#
# Inputs (VOLUME_PATH):
#   drought_summary_yearly_ref.rds — annual SPI/SPEI freq per config × country × pixel
#     OR pixel_freq_ref.rds        — pixel-level ref period freq from Script 03
#
# Outputs (VOLUME_PATH):
#   cdd_era5_annual.rds            — CDD annual freq per x, y, country, year
#   cdd_era5_ref_avg.rds           — avg annual CDD freq per pixel over ref period
#   analysis3_cdd_spearman.rds     — Spearman r per config × country
#   analysis3_cdd_summary.rds      — summary table
# =============================================================================


# -- Libraries ----------------------------------------------------------------
library(sparklyr)
library(dplyr)
library(tidyr)
library(purrr)


# -- Configuration ------------------------------------------------------------
VOLUME_PATH    <- "/Volumes/projects/gwsc_drought/drought_outputs"
ERA5_CATALOG   <- "projects.xx_drought_index.drought_index_dailysummary"
CDD_MIN_LENGTH <- 7       # minimum consecutive dry days to count as event
DRY_THRESHOLD  <- 1       # mm/day; <= this = dry day
REF_START      <- 1973
REF_END        <- 2002
P_THRESHOLD    <- 0.05

COUNTRIES <- c("Ethiopia", "Kenya", "Somalia", "South Sudan", "Sudan", "Uganda")


# -- Spark connection ---------------------------------------------------------
message("Connecting to Spark...")
sc <- spark_connect(method = "databricks")


# =============================================================================
# STEP 1 — Compute ERA5 CDD annual frequency per pixel
# Logic mirrors XX_Consecutive_Dry_Days.Rmd ERA5 section exactly.
# =============================================================================

message("\n=== Step 1: ERA5 CDD run-length detection ===")

dd_era5 <- tbl(sc, sql(sprintf(
  "SELECT x, y, time, year, country, precip FROM %s
   WHERE country IN (%s)",
  ERA5_CATALOG,
  paste(sprintf("'%s'", COUNTRIES), collapse = ", ")
)))

# Flag dry days
dd_era5 <- dd_era5 %>%
  mutate(
    dry_day   = if_else(precip <= DRY_THRESHOLD, 1L, 0L),
    group_var = paste(x, y, year, country, sep = "_")
  )

# Run-length detection via Spark window functions (mirrors Rmd logic)
cdd_runs <- dd_era5 %>%
  group_by(x, y, country) %>%
  window_order(x, y, country, time) %>%
  mutate(
    change = as.integer(
      group_var != lag(group_var, default = first(group_var)) |
      dry_day   != lag(dry_day,   default = first(dry_day))
    )
  ) %>%
  group_by(run = cumsum(change), group_var) %>%
  summarise(
    count   = n(),
    dry_day = first_value(dry_day),
    x       = first_value(x),
    y       = first_value(y),
    country = first_value(country),
    year    = first_value(year),
    .groups = "drop"
  ) %>%
  filter(dry_day == 1L, count >= CDD_MIN_LENGTH)

# Annual CDD event frequency per pixel
# freq_era5 = number of times each exact run length occurred
# mult = how many CDD_MIN_LENGTH units fit in that run
# freq = mult * freq_era5 = effective annual event count
cdd_annual <- cdd_runs %>%
  group_by(x, y, country, year, count) %>%
  summarise(freq_era5 = n(), .groups = "drop") %>%
  mutate(
    mult = floor(count / CDD_MIN_LENGTH),
    freq = mult * freq_era5
  ) %>%
  group_by(x, y, country, year) %>%
  summarise(cdd_freq = sum(freq, na.rm = TRUE), .groups = "drop") %>%
  collect()

message(sprintf("CDD annual rows: %d", nrow(cdd_annual)))
saveRDS(cdd_annual, file.path(VOLUME_PATH, "cdd_era5_annual.rds"))
message("Saved: cdd_era5_annual.rds")

spark_disconnect(sc)
rm(dd_era5, cdd_runs)


# =============================================================================
# STEP 2 — Reference period average CDD frequency per pixel
# =============================================================================

message("\n=== Step 2: Reference period pixel averages ===")

cdd_ref <- cdd_annual %>%
  filter(year >= REF_START, year <= REF_END) %>%
  group_by(x, y, country) %>%
  summarise(
    cdd_avg_freq  = mean(cdd_freq, na.rm = TRUE),
    cdd_n_years   = n(),
    .groups = "drop"
  )

cat(sprintf("Pixels with CDD data: %d\n", nrow(cdd_ref)))
saveRDS(cdd_ref, file.path(VOLUME_PATH, "cdd_era5_ref_avg.rds"))
message("Saved: cdd_era5_ref_avg.rds")


# =============================================================================
# STEP 3 — Load SPI/SPEI pixel freq from Script 03 output
# =============================================================================

message("\n=== Step 3: Load SPI/SPEI pixel frequency ===")

# pixel_freq_ref.rds: columns — config_id, country, x, y, spi_freq (avg annual events ref period)
# Produced by Script 03 spatial block
pixel_freq_path <- file.path(VOLUME_PATH, "pixel_freq_ref.rds")

if (!file.exists(pixel_freq_path)) {
  stop("pixel_freq_ref.rds not found. Run Script 03 first.")
}

pixel_freq <- readRDS(pixel_freq_path)
message(sprintf("SPI/SPEI pixel freq rows: %d", nrow(pixel_freq)))
message(sprintf("Configs: %d", length(unique(pixel_freq$config_id))))


# =============================================================================
# STEP 4 — Spearman rank correlation: CDD freq vs SPI/SPEI freq per pixel
# =============================================================================

message("\n=== Step 4: Spearman r — CDD vs SPI/SPEI frequency ===")

configs     <- unique(pixel_freq$config_id)
countries   <- intersect(COUNTRIES, unique(cdd_ref$country))

spearman_results <- map_dfr(countries, function(ctry) {

  cdd_ctry  <- cdd_ref %>% filter(country == ctry)

  map_dfr(configs, function(cfg) {

    spi_ctry <- pixel_freq %>%
      filter(config_id == cfg, country == ctry) %>%
      select(x, y, spi_freq)

    joined <- inner_join(cdd_ctry, spi_ctry, by = c("x", "y"))

    n_pixels <- nrow(joined)
    if (n_pixels < 5) return(NULL)

    r_val <- cor(joined$cdd_avg_freq, joined$spi_freq,
                 method = "spearman", use = "complete.obs")
    n_complete <- sum(complete.cases(joined[, c("cdd_avg_freq", "spi_freq")]))

    # Spearman p-value via t-approximation
    t_stat <- r_val * sqrt((n_complete - 2) / (1 - r_val^2))
    p_val  <- 2 * pt(abs(t_stat), df = n_complete - 2, lower.tail = FALSE)

    data.frame(
      config_id    = cfg,
      country      = ctry,
      n_pixels     = n_complete,
      spearman_r   = round(r_val, 4),
      p_value      = round(p_val, 6),
      significant  = p_val <= P_THRESHOLD,
      stringsAsFactors = FALSE
    )
  })
})

cat(sprintf("\nSpearman results rows: %d\n", nrow(spearman_results)))
print(spearman_results %>% arrange(country, config_id), row.names = FALSE)

saveRDS(spearman_results, file.path(VOLUME_PATH, "analysis3_cdd_spearman.rds"))
message("Saved: analysis3_cdd_spearman.rds")


# =============================================================================
# STEP 5 — Summary table
# =============================================================================

message("\n=== Step 5: Summary ===")

summary_cdd <- spearman_results %>%
  group_by(country) %>%
  summarise(
    n_configs       = n(),
    median_r        = round(median(spearman_r, na.rm = TRUE), 3),
    pct_sig         = round(100 * mean(significant), 1),
    min_r           = round(min(spearman_r,  na.rm = TRUE), 3),
    max_r           = round(max(spearman_r,  na.rm = TRUE), 3),
    .groups = "drop"
  )

cat("\n=== CDD vs SPI/SPEI Spearman summary ===\n")
print(summary_cdd, row.names = FALSE)

saveRDS(summary_cdd, file.path(VOLUME_PATH, "analysis3_cdd_summary.rds"))
message("Saved: analysis3_cdd_summary.rds")

message("\n=== Script 09 complete ===")
message(sprintf(
  "CDD definition: >= %d consecutive dry days (precip <= %g mm/day), ref period %d-%d",
  CDD_MIN_LENGTH, DRY_THRESHOLD, REF_START, REF_END
))
