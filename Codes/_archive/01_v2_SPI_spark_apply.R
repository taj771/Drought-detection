# =============================================================================
# 01_v2. SPI Calculation — SparkR gapply distributed architecture
# =============================================================================
# Architecture: SparkR gapply — Databricks-native, no package .tar shipping,
# scales to global (864k pixels). Replaces sparklyr spark_apply.
#
# Data source: cds_era5.silver.era5_daily (global, 0.25°, CDS/Copernicus)
# Spatial filter: ESRI world boundaries via sf clip (driver only)
# Architecture: SparkR::gapply(cols=c("x","y")) — one pixel group per task
#
# Column mapping from cds_era5:
#   longitude (0-360) → x (-180 to 180 after conversion)
#   latitude          → y
#   sum_tp_mm         → monthly_precip
#   mean_t2m_c        → mean_temp_monthly
#   time (date)       → year, month extracted
#
# Requires: standard DBR runtime (NOT ML) — SparkR pre-installed on all nodes.
# Tested on: DBR 15.4 LTS cluster 0616-154127-izvo5r0w.
#
# Output: spi_results.rds — same schema as V1, drop-in for Scripts 03-09.
# =============================================================================


# -- Libraries ----------------------------------------------------------------
library(SparkR)
library(sf)


# =============================================================================
# USER SETTINGS
# =============================================================================
COUNTRY_FILTER  <- c("Benin")
ERA5_YEAR_START <- 1950L
ERA5_YEAR_END   <- 2023L

# Parallelization mode:
#   FALSE = driver lapply (single-node OK, ~5-10 min for 6 countries)
#   TRUE  = SparkR gapply (multi-node cluster 0616-154127-izvo5r0w, needed for global 864k pixels)
USE_GAPPLY <- FALSE
# =============================================================================


# -- Databricks SparkR session ------------------------------------------------
sparkR.session()
message("SparkR session ready")

# Pre-register empty package bundle so Databricks DBR worker bootstrap finds it.
# DBR 15.4 patches RWorker to always fetch packages.<uuid>.tar at worker start.
# UUID = sparklyr's sc$sessionId, which is a UUID generated once per Databricks
# notebook session and reused across runs. We discover it from env then register.
local({
  # UUID is stored in env var set by Databricks when sparklyr was previously used
  # or can be derived. Try multiple sources:
  uuid <- Sys.getenv("SPARKLYR_SESSION_ID", unset = "")
  if (nchar(uuid) == 0) {
    # Fall back: read from any existing packages.*.tar in tempdir
    existing <- list.files(tempdir(), pattern = "^packages\\.[0-9a-f-]+\\.tar$",
                           recursive = TRUE, full.names = FALSE)
    if (length(existing) > 0) {
      uuid <- sub("^packages\\.(.+)\\.tar$", "\\1", existing[1])
    }
  }
  if (nchar(uuid) == 0) {
    # Hard-code the fixed UUID observed in all runs on this Databricks session
    uuid <- "5413bc40-6a61-11f1-8776-00163ee65f67"
  }
  bundle_dir  <- file.path(tempdir(), "spark_bundle")
  dir.create(bundle_dir, showWarnings = FALSE, recursive = TRUE)
  bundle_file <- file.path(bundle_dir, paste0("packages.", uuid, ".tar"))
  if (!file.exists(bundle_file)) {
    empty_dir <- tempfile("empty_"); dir.create(empty_dir)
    system2("tar", c("-cf", bundle_file, "-C", empty_dir, "."))
  }
  sc_java <- SparkR:::sparkR.callJStatic("org.apache.spark.SparkContext", "getOrCreate")
  SparkR:::callJMethod(sc_java, "addFile", bundle_file)
  message("Bundle pre-registered: ", basename(bundle_file))
})


# -- Load country boundaries from ESRI table (driver) -------------------------
message("Loading country boundaries from ESRI table...")
boundaries_raw <- collect(
  SparkR::filter(
    SparkR::select(
      sql("SELECT * FROM `era5-daily-data`.silver_staging.esri_worldcountryboundaries_global_silver"),
      "COUNTRY", "chip_wkb"
    ),
    SparkR::column("COUNTRY") %in% COUNTRY_FILTER
  )
)

boundaries_sf <- st_as_sf(
  boundaries_raw,
  wkb = boundaries_raw$chip_wkb,
  crs = 4326
)
boundaries_sf$chip_wkb <- NULL
sf::sf_use_s2(FALSE)
boundaries_sf <- sf::st_make_valid(boundaries_sf)
message(sprintf("Boundaries loaded: %d features for %d countries",
                nrow(boundaries_sf), length(COUNTRY_FILTER)))


# -- ERA5 daily → monthly aggregation in Spark --------------------------------
era5_raw <- tableToDF("cds_era5.silver.era5_daily")

# lon 0-360 → -180/180; extract year/month
era5_conv <- SparkR::withColumn(era5_raw, "lon_180",
  SparkR::ifelse(SparkR::column("longitude") > 180,
                 SparkR::column("longitude") - 360,
                 SparkR::column("longitude")))
era5_conv <- SparkR::withColumn(era5_conv, "year",
  SparkR::year(SparkR::column("time")))
era5_conv <- SparkR::withColumn(era5_conv, "month",
  SparkR::month(SparkR::column("time")))

# Year filter
era5_conv <- SparkR::filter(era5_conv,
  SparkR::column("year") >= ERA5_YEAR_START &
  SparkR::column("year") <= ERA5_YEAR_END)

# Bounding box pre-filter derived from COUNTRY_FILTER boundaries (dynamic)
bb <- sf::st_bbox(boundaries_sf)
era5_bbox <- SparkR::filter(era5_conv,
  SparkR::column("lon_180") >= bb[["xmin"]] &
  SparkR::column("lon_180") <= bb[["xmax"]] &
  SparkR::column("latitude") >= bb[["ymin"]] &
  SparkR::column("latitude") <= bb[["ymax"]]
)

# Monthly aggregation
monthly_spark <- SparkR::agg(
  SparkR::groupBy(era5_bbox, "lon_180", "latitude", "year", "month"),
  monthly_precip    = SparkR::expr("sum(sum_tp_mm)"),
  mean_temp_monthly = SparkR::expr("avg(mean_t2m_c)"),
  n_days_observed   = SparkR::expr("count(time)"),
  n_days_missing    = SparkR::expr("sum(cast(isnull(sum_tp_mm) as int))")
)

monthly_spark <- SparkR::filter(monthly_spark,
  SparkR::column("n_days_missing") < 5)
monthly_spark <- SparkR::withColumnRenamed(monthly_spark, "lon_180",  "x")
monthly_spark <- SparkR::withColumnRenamed(monthly_spark, "latitude", "y")

message("Collecting monthly data to driver for spatial clip...")
monthly_df <- collect(monthly_spark)
message(sprintf("Collected: %s rows, %d pixels",
                format(nrow(monthly_df), big.mark = ","),
                length(unique(paste(monthly_df$x, monthly_df$y)))))


# -- Spatial clip: assign country to each pixel (driver) ----------------------
pixels    <- unique(monthly_df[, c("x", "y")])
pixels_sf <- st_as_sf(pixels, coords = c("x", "y"), crs = 4326)
inside    <- sf::st_intersects(pixels_sf, boundaries_sf)
hit       <- lengths(inside) > 0

pixels$country <- NA_character_
pixels$country[hit] <- boundaries_sf$COUNTRY[sapply(inside[hit], `[[`, 1)]
pixels_with_country <- pixels[!is.na(pixels$country), ]
message(sprintf("Pixels after spatial clip: %d (of %d total)",
                nrow(pixels_with_country), nrow(pixels)))

monthly_clipped <- merge(monthly_df, pixels_with_country, by = c("x", "y"))
message(sprintf("Rows after clip: %s", format(nrow(monthly_clipped), big.mark = ",")))


# -- SPI calculation ----------------------------------------------------------
spi_schema <- structType(
  structField("x",                 "double"),
  structField("y",                 "double"),
  structField("country",           "string"),
  structField("year",              "integer"),
  structField("month",             "integer"),
  structField("monthly_precip",    "double"),
  structField("mean_temp_monthly", "double"),
  structField("n_days_observed",   "integer"),
  structField("n_days_missing",    "integer"),
  structField("spi1",              "double"),
  structField("spi3",              "double"),
  structField("spi6",              "double"),
  structField("spi12",             "double")
)

spi_worker <- function(key, df) {
  df <- df[order(df$year, df$month), ]

  safe_spi <- function(precip_vec, scale) {
    tryCatch({
      min_obs <- max(scale * 4L, 24L)
      if (sum(!is.na(precip_vec)) < min_obs) return(rep(NA_real_, length(precip_vec)))
      as.numeric(SPEI::spi(precip_vec, scale = scale, na.rm = TRUE)$fitted)
    }, error = function(e) rep(NA_real_, length(precip_vec)))
  }

  df$spi1  <- safe_spi(df$monthly_precip, 1L)
  df$spi3  <- safe_spi(df$monthly_precip, 3L)
  df$spi6  <- safe_spi(df$monthly_precip, 6L)
  df$spi12 <- safe_spi(df$monthly_precip, 12L)

  df$x                 <- as.double(df$x)
  df$y                 <- as.double(df$y)
  df$country           <- as.character(df$country)
  df$year              <- as.integer(df$year)
  df$month             <- as.integer(df$month)
  df$monthly_precip    <- as.double(df$monthly_precip)
  df$mean_temp_monthly <- as.double(df$mean_temp_monthly)
  df$n_days_observed   <- as.integer(df$n_days_observed)
  df$n_days_missing    <- as.integer(df$n_days_missing)
  df$spi1              <- as.double(df$spi1)
  df$spi3              <- as.double(df$spi3)
  df$spi6              <- as.double(df$spi6)
  df$spi12             <- as.double(df$spi12)

  df[, c("x", "y", "country", "year", "month",
         "monthly_precip", "mean_temp_monthly",
         "n_days_observed", "n_days_missing",
         "spi1", "spi3", "spi6", "spi12")]
}

if (USE_GAPPLY) {
  message("Running SPI via SparkR gapply (multi-node)...")
  monthly_spark2 <- createDataFrame(monthly_clipped)
  monthly_spark2 <- SparkR::repartition(monthly_spark2, numPartitions = 4L)
  spi_results_sdf <- SparkR::gapply(
    monthly_spark2,
    cols   = c("x", "y"),
    func   = spi_worker,
    schema = spi_schema
  )
  message("Collecting SPI results...")
  spi_results <- collect(spi_results_sdf)
} else {
  message("Running SPI on driver via lapply (single-node mode)...")
  library(SPEI)
  pixels_list <- split(monthly_clipped,
                       list(monthly_clipped$x, monthly_clipped$y), drop = TRUE)
  spi_list <- lapply(pixels_list, function(df) {
    key <- df[1, c("x", "y"), drop = FALSE]
    spi_worker(key, df)
  })
  spi_results <- do.call(rbind, spi_list)
  rownames(spi_results) <- NULL
}
message(sprintf("SPI complete! %s rows, %d pixels",
                format(nrow(spi_results), big.mark = ","),
                length(unique(paste(spi_results$x, spi_results$y)))))


# -- Save output --------------------------------------------------------------
if (!exists("VOLUME_PATH")) {
  VOLUME_PATH <- "/Volumes/projects/gwsc_drought/drought_outputs_v2"
}
dir.create(VOLUME_PATH, showWarnings = FALSE, recursive = TRUE)
saveRDS(spi_results, file.path(VOLUME_PATH, "spi_results.rds"))
message("Saved: spi_results.rds")
