# =============================================================================
# 02b_v2. SPEI Calculation — Penman-Monteith PET — SparkR gapply distributed
# =============================================================================
# Architecture: SparkR gapply — Databricks-native, no package .tar shipping,
# scales to global (864k pixels). Replaces sparklyr spark_apply.
#
# Data source: cds_era5.silver.era5_daily (global, 0.25°, CDS/Copernicus)
# Spatial filter: ESRI world boundaries via sf clip (driver only)
# PET: loaded from NC files on Volume, joined to ERA5 on driver
#
# Requires: standard DBR runtime (NOT ML) — SparkR pre-installed on all nodes.
# Tested on: DBR 15.4 LTS cluster 0616-154127-izvo5r0w.
#
# Output: spei_penman_results.rds — same schema as V1, drop-in for Scripts 03-09.
# =============================================================================


# -- Libraries ----------------------------------------------------------------
library(SparkR)
library(sf)
library(terra)


# =============================================================================
# USER SETTINGS
# =============================================================================
COUNTRY_FILTER  <- c("Benin")
ERA5_YEAR_START <- 1950L
ERA5_YEAR_END   <- 2023L
pet_dir         <- "/Volumes/projects/gwsc_drought/drought_outputs/pet_clipped/"
pet_year_start  <- 1970
pet_year_end    <- 2023

# FALSE = driver lapply (single-node OK)  |  TRUE = gapply (multi-node 0616-154127-izvo5r0w)
USE_GAPPLY <- FALSE
# =============================================================================


# -- Databricks SparkR session ------------------------------------------------
sparkR.session()
message("SparkR session ready")

# Pre-register empty package bundle so Databricks DBR worker bootstrap finds it.
local({
  uuid <- Sys.getenv("SPARKLYR_SESSION_ID", unset = "")
  if (nchar(uuid) == 0) {
    existing <- list.files(tempdir(), pattern = "^packages\\.[0-9a-f-]+\\.tar$",
                           recursive = TRUE, full.names = FALSE)
    if (length(existing) > 0) uuid <- sub("^packages\\.(.+)\\.tar$", "\\1", existing[1])
  }
  if (nchar(uuid) == 0) uuid <- "5413bc40-6a61-11f1-8776-00163ee65f67"
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

era5_conv <- SparkR::withColumn(era5_raw, "lon_180",
  SparkR::ifelse(SparkR::column("longitude") > 180,
                 SparkR::column("longitude") - 360,
                 SparkR::column("longitude")))
era5_conv <- SparkR::withColumn(era5_conv, "year",
  SparkR::year(SparkR::column("time")))
era5_conv <- SparkR::withColumn(era5_conv, "month",
  SparkR::month(SparkR::column("time")))

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

monthly_spark <- SparkR::agg(
  SparkR::groupBy(era5_bbox, "lon_180", "latitude", "year", "month"),
  monthly_precip = SparkR::expr("sum(sum_tp_mm)"),
  n_days_missing = SparkR::expr("sum(cast(isnull(sum_tp_mm) as int))")
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


# -- Load Penman-Monteith PET from NC files (driver) --------------------------
load_pet_file <- function(nc_path, year_start, year_end) {
  r     <- rast(nc_path)
  dates <- seq(as.Date(paste0(year_start, "-01-01")),
               by = "month", length.out = nlyr(r))
  names(r) <- format(dates, "pet_%Y_%m")
  df_wide  <- as.data.frame(r, xy = TRUE, na.rm = TRUE)
  pet_cols <- grep("^pet_", names(df_wide), value = TRUE)
  df_long  <- reshape(df_wide, varying = pet_cols, v.names = "pet_mm",
                      timevar = "layer_name", times = pet_cols,
                      direction = "long")
  df_long$year       <- as.integer(substr(df_long$layer_name, 5, 8))
  df_long$month      <- as.integer(substr(df_long$layer_name, 10, 11))
  df_long$pet_mm     <- round(df_long$pet_mm, 2)
  df_long$id         <- NULL
  df_long$layer_name <- NULL
  df_long[order(df_long$x, df_long$y, df_long$year, df_long$month),
          c("x", "y", "year", "month", "pet_mm")]
}

pet_files <- list.files(pet_dir, pattern = "1970_2023.*\\.nc$", full.names = TRUE)
if (length(pet_files) == 0) {
  stop(sprintf("No PET NC files in: %s — run Scripts 03-05 first.", pet_dir))
}
pet_r   <- do.call(rbind, lapply(pet_files, load_pet_file,
                                 year_start = pet_year_start,
                                 year_end   = pet_year_end))
pet_r$x <- round(pet_r$x, 4)
pet_r$y <- round(pet_r$y, 4)
message(sprintf("PET loaded: %s rows", format(nrow(pet_r), big.mark = ",")))


# -- Join PET to clipped monthly ERA5 (driver) --------------------------------
monthly_clipped$x <- round(monthly_clipped$x, 4)
monthly_clipped$y <- round(monthly_clipped$y, 4)

monthly_with_pet <- merge(monthly_clipped, pet_r, by = c("x", "y", "year", "month"))
monthly_with_pet$water_balance <- monthly_with_pet$monthly_precip -
                                  monthly_with_pet$pet_mm
message(sprintf("After PET join: %s rows",
                format(nrow(monthly_with_pet), big.mark = ",")))


# -- SPEI-Penman calculation --------------------------------------------------
spei_p_schema <- structType(
  structField("x",              "double"),
  structField("y",              "double"),
  structField("country",        "string"),
  structField("year",           "integer"),
  structField("month",          "integer"),
  structField("monthly_precip", "double"),
  structField("n_days_missing", "integer"),
  structField("pet_mm",         "double"),
  structField("water_balance",  "double"),
  structField("spei1",          "double"),
  structField("spei3",          "double"),
  structField("spei6",          "double"),
  structField("spei12",         "double")
)

spei_p_worker <- function(key, df) {
  df <- df[order(df$year, df$month), ]

  safe_spei <- function(wb_vec, scale) {
    tryCatch({
      min_obs <- max(scale * 4L, 24L)
      if (sum(!is.na(wb_vec)) < min_obs) return(rep(NA_real_, length(wb_vec)))
      result <- as.numeric(SPEI::spei(wb_vec, scale = scale, na.rm = TRUE)$fitted)
      result[is.infinite(result)] <- NA_real_
      result
    }, error = function(e) rep(NA_real_, length(wb_vec)))
  }

  df$spei1  <- safe_spei(df$water_balance, 1L)
  df$spei3  <- safe_spei(df$water_balance, 3L)
  df$spei6  <- safe_spei(df$water_balance, 6L)
  df$spei12 <- safe_spei(df$water_balance, 12L)

  df$x              <- as.double(df$x)
  df$y              <- as.double(df$y)
  df$country        <- as.character(df$country)
  df$year           <- as.integer(df$year)
  df$month          <- as.integer(df$month)
  df$monthly_precip <- as.double(df$monthly_precip)
  df$n_days_missing <- as.integer(df$n_days_missing)
  df$pet_mm         <- as.double(df$pet_mm)
  df$water_balance  <- as.double(df$water_balance)
  df$spei1          <- as.double(df$spei1)
  df$spei3          <- as.double(df$spei3)
  df$spei6          <- as.double(df$spei6)
  df$spei12         <- as.double(df$spei12)

  df[, c("x", "y", "country", "year", "month",
         "monthly_precip", "n_days_missing",
         "pet_mm", "water_balance",
         "spei1", "spei3", "spei6", "spei12")]
}

if (USE_GAPPLY) {
  message("Running SPEI-Penman via SparkR gapply (multi-node)...")
  monthly_spark2 <- createDataFrame(monthly_with_pet)
  monthly_spark2 <- SparkR::repartition(monthly_spark2, numPartitions = 4L)
  spei_p_results_sdf <- SparkR::gapply(
    monthly_spark2,
    cols   = c("x", "y"),
    func   = spei_p_worker,
    schema = spei_p_schema
  )
  message("Collecting SPEI-Penman results...")
  spei_penman_results <- collect(spei_p_results_sdf)
} else {
  message("Running SPEI-Penman on driver via lapply (single-node mode)...")
  library(SPEI)
  pixels_list <- split(monthly_with_pet,
                       list(monthly_with_pet$x, monthly_with_pet$y), drop = TRUE)
  spei_list <- lapply(pixels_list, function(df) {
    key <- df[1, c("x", "y"), drop = FALSE]
    spei_p_worker(key, df)
  })
  spei_penman_results <- do.call(rbind, spei_list)
  rownames(spei_penman_results) <- NULL
}
message(sprintf("SPEI (Penman) complete! %s rows, %d pixels",
                format(nrow(spei_penman_results), big.mark = ","),
                length(unique(paste(spei_penman_results$x,
                                    spei_penman_results$y)))))


# -- Save output --------------------------------------------------------------
if (!exists("VOLUME_PATH")) {
  VOLUME_PATH <- "/Volumes/projects/gwsc_drought/drought_outputs_v2"
}
dir.create(VOLUME_PATH, showWarnings = FALSE, recursive = TRUE)
saveRDS(spei_penman_results, file.path(VOLUME_PATH, "spei_penman_results.rds"))
message("Saved: spei_penman_results.rds")
