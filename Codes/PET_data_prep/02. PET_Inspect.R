# =============================================================================
# 04. Inspect and Verify ERA5-Land PET Downloads
# =============================================================================
# Analysis Plan Reference: Section 1.2 — Calculating SPEI using ERA5 data
#                          PET Method (b): Penman-Monteith FAO-56
#
# Purpose: Verify downloaded ERA5-Land PET files before resampling.
#   - Check resolution, layers, spatial extent
#   - Convert units: metres (negative ERA5 convention) → mm/month (positive)
#   - Confirm values are physically plausible per country
#   - Convert to long-format dataframe (one row per pixel per month)
#
# Pipeline position:
#   03 (download) → 04 (this) → 05 (resample to ERA5 grid) → 02b (SPEI calc)
#
# Prerequisites: Script 03 completed — NC files exist in INPUT_DIR
# =============================================================================


# -- Libraries ----------------------------------------------------------------
library(terra)
library(dplyr)


# =============================================================================
# USER SETTINGS — edit this section only
# =============================================================================

INPUT_DIR <- "./data/era5land_pet/"

# Year range must match YEARS used in Script 03
YEAR_START <- 1970
YEAR_END   <- 2000

# =============================================================================


# -- Find all downloaded NC files ---------------------------------------------
nc_files <- list.files(INPUT_DIR, pattern = "\\.nc$", full.names = TRUE)

if (length(nc_files) == 0) {
  stop(sprintf("No NC files found in: %s\nRun Script 03 first.", INPUT_DIR))
}

cat(sprintf("Found %d NC file(s) to inspect:\n", length(nc_files)))
for (f in nc_files) {
  cat(sprintf("  %s  (%.1f MB)\n", basename(f), file.size(f) / 1e6))
}


# -- Inspect function ---------------------------------------------------------
# Runs per-file checks and converts to long-format dataframe.
# ERA5-Land PET is stored as metres, negative (evaporation convention).
# Multiply by -1000 to get mm/month positive values.
inspect_pet_file <- function(nc_path, year_start, year_end) {

  fname <- basename(nc_path)
  cat(sprintf("\n=== %s ===\n", fname))

  r <- rast(nc_path)

  # -- Basic structure --------------------------------------------------------
  n_layers_expected <- (year_end - year_start + 1) * 12
  cat(sprintf("Layers      : %d  (expected %d = %d years x 12 months)\n",
              nlyr(r), n_layers_expected, year_end - year_start + 1))
  cat(sprintf("Resolution  : %.4f deg (~%.1f km)\n", res(r)[1], res(r)[1] * 111))
  cat(sprintf("CRS         : %s\n", crs(r, describe = TRUE)$name))
  cat(sprintf("Extent      : x [%.3f to %.3f]  y [%.3f to %.3f]\n",
              ext(r)$xmin, ext(r)$xmax, ext(r)$ymin, ext(r)$ymax))

  # -- Raw values (metres, negative) ------------------------------------------
  cat(sprintf("\nRaw layer 1 (should be negative metres):\n"))
  cat(sprintf("  Min  : %.6f m\n", global(r[[1]], "min",  na.rm = TRUE)[[1]]))
  cat(sprintf("  Mean : %.6f m\n", global(r[[1]], "mean", na.rm = TRUE)[[1]]))
  cat(sprintf("  Max  : %.6f m\n", global(r[[1]], "max",  na.rm = TRUE)[[1]]))

  # -- Convert units ----------------------------------------------------------
  # ERA5 convention: PET stored as negative metres (energy leaving surface).
  # Multiply by -1000 to get positive mm/month for water balance calculation.
  r_mm <- r * -1000

  cat(sprintf("\nConverted (mm/month, should be positive):\n"))
  for (lyr in c(1, 7)) {
    if (lyr <= nlyr(r_mm)) {
      val <- global(r_mm[[lyr]], "mean", na.rm = TRUE)[[1]]
      cat(sprintf("  Layer %2d: mean = %.1f mm/month\n", lyr, val))
    }
  }

  # -- Annual total check (first year) ----------------------------------------
  annual <- sum(r_mm[[1:12]], na.rm = TRUE)
  cat(sprintf("\nAnnual total PET (%d):\n", year_start))
  cat(sprintf("  Min  : %.0f mm/yr\n", global(annual, "min",  na.rm = TRUE)[[1]]))
  cat(sprintf("  Mean : %.0f mm/yr\n", global(annual, "mean", na.rm = TRUE)[[1]]))
  cat(sprintf("  Max  : %.0f mm/yr\n", global(annual, "max",  na.rm = TRUE)[[1]]))
  cat("  Expected range: ~400-1800 mm/yr depending on climate\n")

  # -- Assign clean layer names -----------------------------------------------
  dates    <- seq(as.Date(paste0(year_start, "-01-01")), by = "month", length.out = nlyr(r_mm))
  names(r_mm) <- format(dates, "pet_%Y_%m")

  # -- Convert to long-format dataframe ---------------------------------------
  # One row per pixel per month — matches ERA5 monthly table structure.
  df_wide <- as.data.frame(r_mm, xy = TRUE, na.rm = TRUE)

  # Pivot wide → long using base R (no tidyr dependency)
  pet_cols  <- grep("^pet_", names(df_wide), value = TRUE)
  df_long   <- reshape(
    df_wide,
    varying      = pet_cols,
    v.names      = "pet_mm",
    timevar      = "layer_name",
    times        = pet_cols,
    direction    = "long"
  ) |>
    within({
      year       <- as.integer(substr(layer_name, 5, 8))
      month      <- as.integer(substr(layer_name, 10, 11))
      pet_mm     <- round(pet_mm, 2)
      id         <- NULL
      layer_name <- NULL
    }) |>
    (\(d) {
      d[order(d$x, d$y, d$year, d$month),
        c("x", "y", "year", "month", "pet_mm")]
    })()

  cat(sprintf("\nLong-format dataframe:\n"))
  cat(sprintf("  Rows   : %s\n", format(nrow(df_long), big.mark = ",")))
  cat(sprintf("  Pixels : %s\n", format(n_distinct(paste(df_long$x, df_long$y)), big.mark = ",")))
  cat(sprintf("  Period : %d-%02d to %d-%02d\n",
              min(df_long$year), min(df_long$month[df_long$year == min(df_long$year)]),
              max(df_long$year), max(df_long$month[df_long$year == max(df_long$year)])))

  # -- Quick plots ------------------------------------------------------------
  # Plot 1: raw metres layer 1
  plot(r[[1]],
       main = paste("ERA5-Land PET — Raw (metres)\n", names(r)[1]),
       col  = hcl.colors(100, "Blues"))

  # Plot 2: mm/month layer 1
  plot(r_mm[[1]],
       main = paste("ERA5-Land PET — mm/month\n", names(r_mm)[1]),
       col  = hcl.colors(100, "YlOrRd"))

  # Plot 3: annual total first year
  plot(annual,
       main = sprintf("ERA5-Land PET — Annual total %d (mm/yr)", year_start),
       col  = hcl.colors(100, "Spectral", rev = TRUE))

  # Plot 4: seasonal cycle at raster centroid (country-agnostic spot check)
  ctr    <- as.data.frame(crds(centroids(as.polygons(ext(r_mm)))))
  ctr_pt <- cbind(ctr$x, ctr$y)
  pet_ts <- as.numeric(extract(r_mm, ctr_pt))[-1]  # drop ID column

  plot(
    x    = seq_along(pet_ts),
    y    = pet_ts,
    type = "l",
    col  = "steelblue",
    lwd  = 1.5,
    xlab = sprintf("Month (1 = Jan %d)", year_start),
    ylab = "PET (mm/month)",
    main = sprintf("Seasonal PET cycle — centroid (%.2f E, %.2f N)", ctr$x, ctr$y)
  )
  abline(h = mean(pet_ts, na.rm = TRUE), col = "firebrick", lty = 2)
  legend("topright", legend = c("Monthly PET", "Mean"),
         col = c("steelblue", "firebrick"), lty = c(1, 2), bty = "n")

  invisible(df_long)
}


# -- Run inspection on all downloaded files -----------------------------------
results <- lapply(nc_files, inspect_pet_file,
                  year_start = YEAR_START,
                  year_end   = YEAR_END)

cat("\n=== Inspection complete ===\n")
cat("Data ready for Script 05 (resample) if:\n")
cat("  - Layer count matches expected (years x 12)\n")
cat("  - Resolution is 0.10 deg (~11 km)\n")
cat("  - mm/month values positive after conversion\n")
cat("  - Annual totals plausible (400-1800 mm/yr)\n")

# Next step: run Script 05 to resample to ERA5 0.25 deg grid.
