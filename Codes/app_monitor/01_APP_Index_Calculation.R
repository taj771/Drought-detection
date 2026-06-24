# =============================================================================
# 01_APP_Index_Calculation.R
# GWSC Drought Monitor — ERA5 Index Calculation
# =============================================================================
# Pulls ERA5 daily data from Databricks for APP_COUNTRY, aggregates to monthly,
# and computes the selected drought index (SPI, SPEI-Thornthwaite, or
# SPEI-Penman-Monteith) at the selected accumulation scale.
#
# Called by: 00_APP_MASTER.Rmd (parameters already set in environment)
#
# Requires in environment (set by MASTER):
#   APP_COUNTRY   — character, country name
#   APP_INDEX     — "SPI" | "SPEI-T" | "SPEI-P"
#   APP_SCALE     — integer, accumulation period (3, 6, or 12)
#   REF_START     — integer, first year of reference period
#   OBS_END       — integer, last year of observation period
#   .index_col    — internal column name (e.g. "spi6", "spei3", "spei6_pm")
#   USE_SPARK_APPLY — logical, TRUE = distributed spark_apply (experimental),
#                     FALSE = collect to driver then R loop (default, proven)
#
# Produces in environment:
#   index_monthly — data frame, one row per pixel × month
#                   columns: x, y, country, year, month, <.index_col>
# =============================================================================

library(sparklyr)
library(dplyr)
library(dbplyr)
library(sf)
library(SPEI)

# Switch: set TRUE to test spark_apply distributed path (data eng team suggestion)
# Set FALSE (default) for proven collect-then-R-loop path
USE_SPARK_APPLY <- FALSE


# -- Databricks connection ----------------------------------------------------
sc <- spark_connect(method = "databricks")


# -- Load country boundary shapefile ------------------------------------------
# APP_SHAPEFILE_PATH set by 00_APP_Download_Boundary.R.
# When integrating into GWSC pipeline: set APP_SHAPEFILE_PATH to carto
# shapefile path on Volume and skip 00_APP_Download_Boundary.R entirely.
sf::sf_use_s2(FALSE)  # disable s2 — avoids NULL from st_intersects on complex polygons
boundary <- st_read(APP_SHAPEFILE_PATH, quiet = TRUE)
boundary <- st_transform(boundary, crs = 4326)
boundary <- sf::st_make_valid(boundary)
boundary <- sf::st_union(boundary)  # dissolve to single geometry, handles multi-feature

# Derive bbox from shapefile for initial Spark pre-filter (fast partition pruning)
.bb       <- st_bbox(boundary)
# unname() strips bbox name attributes — prevents sparklyr injecting "AS ymin" etc.
.lat_min  <- unname(floor(.bb["ymin"]   * 4) / 4  - 0.25)
.lat_max  <- unname(ceiling(.bb["ymax"] * 4) / 4  + 0.25)
.lon_min  <- unname(floor(.bb["xmin"]   * 4) / 4  - 0.25)
.lon_max  <- unname(ceiling(.bb["xmax"] * 4) / 4  + 0.25)
cat(sprintf("Shapefile bbox (with margin): lat [%.2f, %.2f], lon [%.2f, %.2f]\n",
            .lat_min, .lat_max, .lon_min, .lon_max))



# -- ERA5 source: GWSC product table (same as Phase 1 pipeline) ---------------
# projects.xx_drought_index.drought_index_dailysummary: daily, 0.25 deg,
# covers 6 GWSC countries (Argentina, Benin, Thailand, Greece, Mexico, South Africa).
# Columns: x, y, country, year, month, precip, mean_temp.
# Switched from silver_staging.era5_daily_nn which is South/SE Asia only.
era5 <- tbl(sc, in_catalog("projects", "xx_drought_index", "drought_index_dailysummary"))


# -- SPEI-Penman: check PET files exist before pulling data -------------------
if (APP_INDEX == "SPEI-P") {
  library(terra)
  pet_dir <- "/Volumes/projects/gwsc_drought/drought_outputs/pet_clipped/"
  pet_files <- list.files(pet_dir, pattern = "\\.nc$", full.names = TRUE)
  if (length(pet_files) == 0) {
    stop(sprintf(
      "SPEI-P selected but no PET NC files found in:\n  %s\nRun ERA5-Land PET preprocessing first.",
      pet_dir
    ))
  }
  cat(sprintf("Found %d PET file(s) in %s\n", length(pet_files), pet_dir))
}


# -- Step 1: country filter + daily->monthly aggregation in Spark -------------
# Filter by country name (GWSC table already scoped to 6 countries).
# Bbox pre-filter retained as partition pruning hint; exact polygon clip in R.
monthly_spark <- era5 %>%
  filter(
    x >= !!.lon_min, x <= !!.lon_max,
    y >= !!.lat_min, y <= !!.lat_max,
    year >= !!REF_START, year <= !!OBS_END
  ) %>%
  group_by(x, y, year, month) %>%
  summarise(
    monthly_precip    = sum(precip,     na.rm = TRUE),
    mean_temp_monthly = mean(mean_temp, na.rm = TRUE),
    n_days_missing    = sum(as.integer(is.na(precip))),
    .groups = "drop"
  ) %>%
  filter(n_days_missing < 5) %>%
  select(x, y, year, month, monthly_precip, mean_temp_monthly) %>%
  arrange(x, y, year, month)
# country added after collect — not in source table

cat(sprintf("Monthly aggregation complete for %s (%d-%d).\n",
            APP_COUNTRY, REF_START, OBS_END))


# =============================================================================
# PATH A — spark_apply: distributed index fitting (experimental)
# Each Spark executor receives one pixel group and runs SPEI in R.
# Requires SPEI package installed on every worker node.
# Advantage: no collect() bottleneck — parallelises across all worker cores.
# Risk: R executor spawn overhead; SPEI must be on workers; flaky on some
#       Databricks runtimes. Test with small run before relying on this.
# =============================================================================

if (USE_SPARK_APPLY) {

  cat("Using spark_apply (distributed) path...\n")

  # Capture APP_SCALE into function scope — spark_apply closures need explicit capture
  .scale <- APP_SCALE
  .index <- APP_INDEX

  # Materialize lazy Spark df as temp view — resolves query plan before spark_apply.
  # spark_apply on unresolved lazy plans causes column metadata errors on some DBR versions.
  sparklyr::sdf_register(monthly_spark, "monthly_tmp")
  monthly_mat <- dplyr::tbl(sc, "monthly_tmp")

  # No group_by — sparklyr's columns_infer crashes with group_by on this DBR runtime.
  # country not in ERA5 source; pass via context and add inside function.
  index_sdf <- spark_apply(
    monthly_mat,
    function(df, context) {
      library(SPEI)
      .scale   <- context$.scale
      .index   <- context$.index
      .country <- context$.country

      safe_spi <- function(v, scale) {
        tryCatch({
          if (sum(!is.na(v)) < max(scale * 4, 24)) return(rep(NA_real_, length(v)))
          as.numeric(SPEI::spi(v, scale = scale, na.rm = TRUE)$fitted)
        }, error = function(e) rep(NA_real_, length(v)))
      }

      safe_spei_t <- function(precip, temp, scale, lat) {
        tryCatch({
          if (sum(!is.na(precip) & !is.na(temp)) < max(scale * 4, 24))
            return(rep(NA_real_, length(precip)))
          if (mean(temp, na.rm = TRUE) > 100) temp <- temp - 273.15
          pet <- SPEI::thornthwaite(temp, lat = lat)
          wb  <- precip - pet
          as.numeric(SPEI::spei(wb, scale = scale, na.rm = TRUE)$fitted)
        }, error = function(e) rep(NA_real_, length(precip)))
      }

      # Split by pixel, apply index, recombine; add country from context
      df$country <- .country
      pixel_key <- paste(df$x, df$y)
      result <- do.call(rbind, lapply(split(df, pixel_key), function(px) {
        px <- px[order(px$year, px$month), ]
        if (.index == "SPI") {
          px$index_value <- safe_spi(px$monthly_precip, .scale)
        } else if (.index == "SPEI-T") {
          px$index_value <- safe_spei_t(px$monthly_precip, px$mean_temp_monthly,
                                         .scale, lat = px$y[1])
        } else {
          px$index_value <- NA_real_
        }
        px[, c("x", "y", "country", "year", "month", "index_value")]
      }))
      result
    },
    packages = "SPEI",
    context  = list(.scale = .scale, .index = .index, .country = APP_COUNTRY)
  )

  cat("Collecting spark_apply results...\n")
  index_monthly_raw <- index_sdf %>%
    collect() %>%
    rename(!!.index_col := index_value)

  spark_disconnect(sc)
  cat(sprintf("spark_apply collected: %s rows, %d pixels (bbox).\n",
              format(nrow(index_monthly_raw), big.mark = ","),
              length(unique(paste(index_monthly_raw$x, index_monthly_raw$y)))))

  # Polygon clip (same as PATH B — bbox includes pixels outside boundary)
  pixels_sf2 <- st_as_sf(
    unique(index_monthly_raw[, c("x", "y")]),
    coords = c("x", "y"), crs = 4326
  )
  inside2    <- sf::st_intersects(pixels_sf2, boundary)
  pixels_in2 <- pixels_sf2[lengths(inside2) > 0, ]
  valid_coords2 <- st_coordinates(pixels_in2)
  valid_key2    <- paste(valid_coords2[, 1], valid_coords2[, 2])
  index_monthly <- index_monthly_raw[paste(index_monthly_raw$x, index_monthly_raw$y) %in% valid_key2, ]

  cat(sprintf("spark_apply complete. %s rows after polygon clip, %d pixels inside boundary.\n",
              format(nrow(index_monthly), big.mark = ","),
              length(unique(paste(index_monthly$x, index_monthly$y)))))


# =============================================================================
# PATH B — collect then R loop: proven, single-driver (default)
# =============================================================================

} else {

  cat("Using collect-then-R path (default)...\n")
  monthly_r <- monthly_spark %>% collect()
  spark_disconnect(sc)
  cat(sprintf("  %s rows (bbox), %d unique pixels before polygon clip.\n",
              format(nrow(monthly_r), big.mark = ","),
              length(unique(paste(monthly_r$x, monthly_r$y)))))

  # -- Step 2: exact polygon clip (same logic as GWSC pipeline spatial join) ---
  pixels_sf <- st_as_sf(
    unique(monthly_r[, c("x", "y")]),
    coords = c("x", "y"), crs = 4326
  )
  inside <- sf::st_intersects(pixels_sf, boundary)
  pixels_in <- pixels_sf[lengths(inside) > 0, ]
  valid_coords <- st_coordinates(pixels_in)
  valid_key    <- paste(valid_coords[, 1], valid_coords[, 2])

  monthly_r <- monthly_r[paste(monthly_r$x, monthly_r$y) %in% valid_key, ]
  monthly_r$country <- APP_COUNTRY

  cat(sprintf("  %s rows after polygon clip, %d pixels inside boundary.\n",
              format(nrow(monthly_r), big.mark = ","),
              length(unique(paste(monthly_r$x, monthly_r$y)))))

  # Ensure year/month are integer (Spark may return numeric)
  monthly_r$year  <- as.integer(monthly_r$year)
  monthly_r$month <- as.integer(monthly_r$month)


  # -- Helper functions -------------------------------------------------------
  safe_spi <- function(precip_vec, scale) {
    tryCatch({
      min_obs <- max(scale * 4, 24)
      if (sum(!is.na(precip_vec)) < min_obs) return(rep(NA_real_, length(precip_vec)))
      as.numeric(spi(precip_vec, scale = scale, na.rm = TRUE)$fitted)
    }, error = function(e) rep(NA_real_, length(precip_vec)))
  }

  safe_spei_t <- function(precip_vec, temp_vec, scale, lat) {
    tryCatch({
      min_obs <- max(scale * 4, 24)
      valid   <- !is.na(precip_vec) & !is.na(temp_vec)
      if (sum(valid) < min_obs) return(rep(NA_real_, length(precip_vec)))
      if (mean(temp_vec[valid], na.rm = TRUE) > 100) temp_vec <- temp_vec - 273.15
      pet_vec <- thornthwaite(temp_vec, lat = lat)
      wb      <- precip_vec - pet_vec
      as.numeric(spei(wb, scale = scale, na.rm = TRUE)$fitted)
    }, error = function(e) rep(NA_real_, length(precip_vec)))
  }

  load_pet_monthly <- function(pet_dir, year_start, year_end, pixels_df) {
    pet_files <- list.files(pet_dir, pattern = "\\.nc$", full.names = TRUE)
    pet_list <- lapply(pet_files, function(f) {
      r     <- rast(f)
      dates <- seq(as.Date(paste0(year_start, "-01-01")),
                   by = "month", length.out = nlyr(r))
      names(r) <- format(dates, "pet_%Y_%m")
      pts    <- vect(pixels_df, geom = c("x", "y"), crs = "EPSG:4326")
      vals   <- terra::extract(r, pts, ID = FALSE)
      result <- cbind(pixels_df[, c("x", "y")], vals)
      tidyr::pivot_longer(result, -c(x, y),
                          names_to = "pet_key", values_to = "pet_mm") %>%
        dplyr::mutate(
          year  = as.integer(sub("pet_(\\d{4})_.*",       "\\1", pet_key)),
          month = as.integer(sub("pet_\\d{4}_(\\d{2})", "\\1", pet_key))
        ) %>%
        dplyr::select(x, y, year, month, pet_mm)
    })
    do.call(rbind, pet_list)
  }

  safe_spei_p <- function(precip_vec, pet_vec, scale) {
    tryCatch({
      min_obs <- max(scale * 4, 24)
      valid   <- !is.na(precip_vec) & !is.na(pet_vec)
      if (sum(valid) < min_obs) return(rep(NA_real_, length(precip_vec)))
      wb <- precip_vec - pet_vec
      as.numeric(spei(wb, scale = scale, na.rm = TRUE)$fitted)
    }, error = function(e) rep(NA_real_, length(precip_vec)))
  }


  # -- Apply index per pixel --------------------------------------------------
  cat(sprintf("Computing %s-%d for all pixels...\n", APP_INDEX, APP_SCALE))

  if (APP_INDEX == "SPI") {

    index_monthly <- monthly_r %>%
      arrange(x, y, year, month) %>%
      group_by(x, y, country) %>%
      mutate(!!.index_col := safe_spi(monthly_precip, scale = APP_SCALE)) %>%
      ungroup() %>%
      select(x, y, country, year, month, all_of(.index_col))

  } else if (APP_INDEX == "SPEI-T") {

    index_monthly <- monthly_r %>%
      arrange(x, y, year, month) %>%
      group_by(x, y, country) %>%
      mutate(!!.index_col := safe_spei_t(monthly_precip, mean_temp_monthly,
                                          scale = APP_SCALE, lat = first(y))) %>%
      ungroup() %>%
      select(x, y, country, year, month, all_of(.index_col))

  } else if (APP_INDEX == "SPEI-P") {

    pixels_df <- unique(monthly_r[, c("x", "y")])
    pet_data  <- load_pet_monthly(pet_dir, REF_START, OBS_END, pixels_df)
    monthly_r <- monthly_r %>%
      left_join(pet_data, by = c("x", "y", "year", "month"))

    col_name <- paste0("spei", APP_SCALE, "_pm")
    index_monthly <- monthly_r %>%
      arrange(x, y, year, month) %>%
      group_by(x, y, country) %>%
      mutate(!!col_name := safe_spei_p(monthly_precip, pet_mm, scale = APP_SCALE)) %>%
      ungroup() %>%
      select(x, y, country, year, month, all_of(col_name))

    .index_col <- col_name
  }

  cat(sprintf("Index calculation complete. %s rows.\n",
              format(nrow(index_monthly), big.mark = ",")))
}
