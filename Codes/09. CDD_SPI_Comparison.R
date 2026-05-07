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

COUNTRIES <- c("Argentina", "Benin", "Thailand", "Greece", "Mexico", "South Africa")


# -- Spark connection ---------------------------------------------------------
message("Connecting to Spark...")
sc <- spark_connect(method = "databricks")


# =============================================================================
# STEP 1 — Compute ERA5 CDD annual frequency per pixel
# Logic mirrors XX_Consecutive_Dry_Days.Rmd ERA5 section exactly.
# =============================================================================

message("\n=== Step 1: ERA5 CDD run-length detection ===")

# CDD run-length detection via pure SQL window functions.
# Uses island-gap technique: assign run_id = row_number - cumulative_dry_days_so_far.
# Rows with same (x, y, country, year, run_id) AND dry_day=1 form one consecutive dry run.
# This avoids global cumsum shuffle that OOMs in sparklyr dplyr approach.
cdd_sql <- sprintf(
  "WITH base AS (
     SELECT x, y, time, year,
            country,
            CASE WHEN precip <= %g THEN 1 ELSE 0 END AS dry_day
     FROM %s
     WHERE country IN (%s)
   ),
   numbered AS (
     SELECT x, y, country, year, dry_day,
            ROW_NUMBER() OVER (
              PARTITION BY x, y, country, year ORDER BY time
            ) AS rn,
            SUM(dry_day) OVER (
              PARTITION BY x, y, country, year ORDER BY time
              ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW
            ) AS cum_dry
     FROM base
   ),
   tagged AS (
     SELECT x, y, country, year, dry_day,
            (rn - cum_dry) AS run_id
     FROM numbered
   ),
   runs AS (
     SELECT x, y, country, year, run_id,
            COUNT(*) AS run_len
     FROM tagged
     WHERE dry_day = 1
     GROUP BY x, y, country, year, run_id
     HAVING COUNT(*) >= %d
   ),
   annual AS (
     SELECT x, y, country, year,
            SUM(FLOOR(CAST(run_len AS DOUBLE) / %d)) AS cdd_freq
     FROM runs
     GROUP BY x, y, country, year
   ),
   ref_avg AS (
     SELECT x, y, country,
            AVG(cdd_freq)  AS cdd_avg_freq,
            COUNT(*)       AS cdd_n_years
     FROM annual
     WHERE year BETWEEN %d AND %d
     GROUP BY x, y, country
   )
   SELECT x, y, country, cdd_avg_freq, cdd_n_years FROM ref_avg",
  DRY_THRESHOLD, ERA5_CATALOG,
  paste(sprintf("'%s'", COUNTRIES), collapse = ", "),
  CDD_MIN_LENGTH, CDD_MIN_LENGTH,
  REF_START, REF_END
)

cdd_ref_spark <- tbl(sc, sql(cdd_sql)) %>% collect()
message(sprintf("CDD ref-period pixel rows: %d", nrow(cdd_ref_spark)))
message(sprintf("CDD ref columns: %s", paste(names(cdd_ref_spark), collapse = ", ")))

# Separate query for full annual series (cdd_era5_annual.rds)
cdd_annual_sql <- sprintf(
  "WITH base AS (
     SELECT x, y, time, year,
            country,
            CASE WHEN precip <= %g THEN 1 ELSE 0 END AS dry_day
     FROM %s
     WHERE country IN (%s)
   ),
   numbered AS (
     SELECT x, y, country, year, dry_day,
            ROW_NUMBER() OVER (
              PARTITION BY x, y, country, year ORDER BY time
            ) AS rn,
            SUM(dry_day) OVER (
              PARTITION BY x, y, country, year ORDER BY time
              ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW
            ) AS cum_dry
     FROM base
   ),
   tagged AS (
     SELECT x, y, country, year, dry_day,
            (rn - cum_dry) AS run_id
     FROM numbered
   ),
   runs AS (
     SELECT x, y, country, year, run_id,
            COUNT(*) AS run_len
     FROM tagged
     WHERE dry_day = 1
     GROUP BY x, y, country, year, run_id
     HAVING COUNT(*) >= %d
   )
   SELECT x, y, country, year,
          SUM(FLOOR(CAST(run_len AS DOUBLE) / %d)) AS cdd_freq
   FROM runs
   GROUP BY x, y, country, year",
  DRY_THRESHOLD, ERA5_CATALOG,
  paste(sprintf("'%s'", COUNTRIES), collapse = ", "),
  CDD_MIN_LENGTH, CDD_MIN_LENGTH
)

cdd_annual_spark <- tbl(sc, sql(cdd_annual_sql)) %>% collect()
message(sprintf("CDD annual rows: %d", nrow(cdd_annual_spark)))
saveRDS(cdd_annual_spark, file.path(VOLUME_PATH, "cdd_era5_annual.rds"))
message("Saved: cdd_era5_annual.rds")

spark_disconnect(sc)
rm(cdd_annual_spark)


# =============================================================================
# STEP 2 — Reference period average CDD frequency per pixel
# =============================================================================

message("\n=== Step 2: Reference period pixel averages ===")

# cdd_ref_spark already aggregated to ref period inside Spark (Step 1)
cdd_ref <- cdd_ref_spark
rm(cdd_ref_spark)

cat(sprintf("Pixels with CDD data: %d\n", nrow(cdd_ref)))
saveRDS(cdd_ref, file.path(VOLUME_PATH, "cdd_era5_ref_avg.rds"))
message("Saved: cdd_era5_ref_avg.rds")


# =============================================================================
# STEP 3 — Load SPI/SPEI pixel freq from Script 03 output
# =============================================================================

message("\n=== Step 3: Load SPI/SPEI pixel frequency ===")

# pixel_freq_ref.rds: columns — config_id, country, x, y, ref_n_events
# Produced by Script 03 spatial block.
# If missing (Script 03 assembly OOM'd before writing it), reconstruct from
# all_drought_events.rds — filter to ref period, sum events per pixel × config.
pixel_freq_path    <- file.path(VOLUME_PATH, "pixel_freq_ref.rds")
all_events_path    <- file.path(VOLUME_PATH, "all_drought_events.rds")

tmp_dir <- file.path(VOLUME_PATH, "detection_tmp")

if (file.exists(pixel_freq_path)) {
  pixel_freq <- readRDS(pixel_freq_path)
  if (inherits(pixel_freq, "data.table")) pixel_freq <- as.data.frame(pixel_freq)
  message(sprintf("Loaded pixel_freq_ref.rds — cols: %s | country present: %s",
                  paste(names(pixel_freq), collapse = ", "),
                  "country" %in% names(pixel_freq)))
  # Rebuild if country column missing (stale file from earlier run)
  if (!"country" %in% names(pixel_freq)) {
    message("pixel_freq_ref.rds missing country — will rebuild from cfg files")
    file.remove(pixel_freq_path)
    pixel_freq <- NULL
  }
}

if (!exists("pixel_freq") || is.null(pixel_freq)) {
  cfg_files <- list.files(tmp_dir, pattern = "^cfg_.*\\.rds$", full.names = TRUE)
  if (length(cfg_files) == 0) {
    stop("No cfg_*.rds files in detection_tmp/. Run Script 03 first.")
  }
  message(sprintf("Rebuilding pixel_freq_ref from %d cfg files...", length(cfg_files)))
  library(data.table)
  pf_list <- vector("list", length(cfg_files))
  for (i in seq_along(cfg_files)) {
    chunk <- readRDS(cfg_files[[i]])
    if (is.null(chunk) || nrow(chunk) == 0) next
    if (!"country" %in% names(chunk)) next
    dt <- as.data.table(chunk)
    if (!"start_year" %in% names(dt) && "start_date" %in% names(dt)) {
      dt[, start_year := as.integer(format(as.Date(start_date), "%Y"))]
    }
    if (!"start_year" %in% names(dt)) next
    pf_list[[i]] <- dt[
      start_year >= REF_START & start_year <= REF_END,
      .(ref_n_events = .N),
      by = .(config_id, country, x, y)
    ]
    rm(chunk, dt)
  }
  non_null <- Filter(Negate(is.null), pf_list)
  rm(pf_list); gc()
  if (length(non_null) == 0) stop("No ref-period events found in cfg files.")
  pixel_freq <- as.data.frame(rbindlist(non_null, use.names = TRUE, fill = TRUE))
  rm(non_null); gc()
  message(sprintf("Rebuilt pixel_freq — %s rows, cols: %s",
                  format(nrow(pixel_freq), big.mark = ","),
                  paste(names(pixel_freq), collapse = ", ")))
  saveRDS(pixel_freq, pixel_freq_path)
  message("Saved: pixel_freq_ref.rds")
}

message(sprintf("SPI/SPEI pixel freq rows: %s", format(nrow(pixel_freq), big.mark = ",")))
message(sprintf("Configs: %d", length(unique(pixel_freq$config_id))))


# =============================================================================
# STEP 4 — Spearman rank correlation: CDD freq vs SPI/SPEI freq per pixel
# =============================================================================

message("\n=== Step 4: Spearman r — CDD vs SPI/SPEI frequency ===")

configs     <- unique(pixel_freq$config_id)
countries   <- intersect(COUNTRIES, unique(cdd_ref$country))

# Round x/y to 6 d.p. — Spark collect() and data.table RDS encode same grid
# value differently at 10th+ decimal place → inner_join misses all pixels
cdd_ref    <- cdd_ref    %>% mutate(x = round(x, 6), y = round(y, 6))
pixel_freq <- pixel_freq %>% mutate(x = round(x, 6), y = round(y, 6))

spearman_results <- map_dfr(countries, function(ctry) {

  cdd_ctry  <- cdd_ref %>% filter(country == ctry)

  map_dfr(configs, function(cfg) {

    freq_col <- if ("spi_freq" %in% names(pixel_freq)) "spi_freq" else "ref_n_events"
    spi_ctry <- pixel_freq %>%
      filter(config_id == cfg, country == ctry) %>%
      select(x, y, spi_freq = all_of(freq_col))

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
