library(terra)

# ============================================================
# Load and inspect the downloaded NetCDF file
# ============================================================

# Point to your downloaded file — adjust path/filename
nc_file <- "./data/era5land_pet/era5land_pet_Argentina_2001_2002.nc"

# Load raster
r <- rast(nc_file)

# ============================================================
# 1. Basic structure
# ============================================================
cat("=== BASIC STRUCTURE ===\n")
cat(sprintf("Class       : %s\n",   class(r)))
cat(sprintf("Dimensions  : %d rows x %d cols\n", nrow(r), ncol(r)))
cat(sprintf("Layers      : %d  (should be 276 = 23 years x 12 months)\n", nlyr(r)))
cat(sprintf("Resolution  : %.4f deg x %.4f deg  (~%.1f km)\n",
            res(r)[1], res(r)[2], res(r)[1] * 111))
cat(sprintf("CRS         : %s\n",   crs(r, describe = TRUE)$name))
cat(sprintf("File size   : %.1f MB\n\n", file.size(nc_file) / 1e6))

# ============================================================
# 2. Spatial extent
# ============================================================
cat("=== SPATIAL EXTENT ===\n")
ext_r <- ext(r)
cat(sprintf("West  (xmin): %.2f\n", ext_r$xmin))
cat(sprintf("East  (xmax): %.2f\n", ext_r$xmax))
cat(sprintf("South (ymin): %.2f\n", ext_r$ymin))
cat(sprintf("North (ymax): %.2f\n\n", ext_r$ymax))

# ============================================================
# 3. Layer names (should be dates)
# ============================================================
cat("=== LAYER NAMES (first 6 and last 6) ===\n")
cat("First 6 layers:\n")
print(names(r)[1:6])
cat("Last 6 layers:\n")
print(tail(names(r), 6))
cat("\n")

# ============================================================
# 4. Raw values check (ERA5 stores PET in metres, negative)
# ============================================================
cat("=== RAW VALUES (metres, negative by ERA5 convention) ===\n")

# Check first layer (January 2001)
layer1 <- r[[1]]
cat(sprintf("Layer 1 (%s):\n", names(r)[1]))
cat(sprintf("  Min  : %.6f m\n", global(layer1, "min",  na.rm = TRUE)[[1]]))
cat(sprintf("  Mean : %.6f m\n", global(layer1, "mean", na.rm = TRUE)[[1]]))
cat(sprintf("  Max  : %.6f m\n", global(layer1, "max",  na.rm = TRUE)[[1]]))
cat(sprintf("  NAs  : %d pixels\n\n", global(is.na(layer1), "sum")[[1]]))

# ============================================================
# 5. Convert to mm/month and check again
# ============================================================
cat("=== CONVERTED VALUES (mm/month, positive) ===\n")
cat("Formula: pet_mm = raw_value * -1000\n\n")

r_mm <- r * -1000

# Check a summer month (January = layer 1) and winter (July = layer 7)
for (lyr in c(1, 7, 13)) {
  val <- global(r_mm[[lyr]], "mean", na.rm = TRUE)[[1]]
  cat(sprintf("  Layer %2d (%s): mean = %.1f mm/month\n",
              lyr, names(r)[lyr], val))
}
# Expected for Argentina:
#   January (summer) : ~80-150 mm/month
#   July    (winter) : ~20-50  mm/month

cat("\n")

# ============================================================
# 6. Annual total check for first year
# ============================================================
cat("=== ANNUAL TOTAL CHECK (year 2001) ===\n")
annual_2001 <- sum(r_mm[[1:12]], na.rm = TRUE)
cat(sprintf("  Min  annual PET: %.0f mm/yr\n",
            global(annual_2001, "min",  na.rm = TRUE)[[1]]))
cat(sprintf("  Mean annual PET: %.0f mm/yr\n",
            global(annual_2001, "mean", na.rm = TRUE)[[1]]))
cat(sprintf("  Max  annual PET: %.0f mm/yr\n",
            global(annual_2001, "max",  na.rm = TRUE)[[1]]))
cat("  Expected range for Argentina: 400-1200 mm/yr\n\n")

# ============================================================
# 7. Quick plots
# ============================================================
cat("=== PLOTTING ===\n")

# Plot 1 — January 2001 (raw, metres)
plot(r[[1]],
     main = paste("ERA5-Land PET — Raw (metres)\n", names(r)[1]),
     col  = hcl.colors(100, "Blues"))

# Plot 2 — January 2001 converted to mm
plot(r_mm[[1]],
     main = paste("ERA5-Land PET — mm/month\n", names(r)[1]),
     col  = hcl.colors(100, "YlOrRd"))

# Plot 3 — Annual total 2001
plot(annual_2001,
     main = "ERA5-Land PET — Annual total 2001 (mm/yr)",
     col  = hcl.colors(100, "Spectral", rev = TRUE))

# Plot 4 — Seasonal cycle at one point (Buenos Aires approx)
ba_lon <- -58.4
ba_lat <- -34.6
ba_pt  <- cbind(ba_lon, ba_lat)

pet_ts <- as.numeric(extract(r_mm, ba_pt))

plot(
  x    = 1:length(pet_ts),
  y    = pet_ts,
  type = "l",
  col  = "steelblue",
  lwd  = 1.5,
  xlab = "Month (1 = Jan 2001)",
  ylab = "PET (mm/month)",
  main = "Seasonal PET cycle — Buenos Aires (~34.6°S, 58.4°W)"
)
abline(h = mean(pet_ts, na.rm = TRUE),
       col = "firebrick", lty = 2)
legend("topright",
       legend = c("Monthly PET", "Mean"),
       col    = c("steelblue", "firebrick"),
       lty    = c(1, 2), bty = "n")

# ============================================================
# 8. Data frame preview (first few pixels)
# ============================================================
cat("=== DATAFRAME PREVIEW (first 3 layers, 5 pixels) ===\n")
df_preview <- as.data.frame(r_mm[[1:3]], xy = TRUE, na.rm = TRUE)
colnames(df_preview)[3:5] <- paste0("pet_mm_", names(r)[1:3])
print(head(df_preview, 5))

cat("\n=== INSPECTION COMPLETE ===\n")
cat("Data looks good if:\n")
cat("  - Resolution is 0.1 deg (~11 km)\n")
cat("  - 276 layers (23 years x 12 months)\n")
cat("  - mm values are positive after * -1000\n")
cat("  - Annual totals are 400-1200 mm/yr for Argentina\n")



nc_file <- "./data/era5land_pet/era5land_pet_Argentina_2001_2002.nc"
r       <- rast(nc_file)

# Convert all layers to mm/month
r_mm <- r * -1000

# ============================================================
# Convert to long format dataframe with proper year/month columns
# ============================================================

# Generate proper date sequence matching the 276 layers
dates <- seq(
  as.Date("2001-01-01"),
  by     = "month",
  length.out = nlyr(r_mm)
)

# Assign clean layer names before extracting
names(r_mm) <- format(dates, "pet_%Y_%m")

# Convert raster to wide dataframe (one column per layer)
df_wide <- as.data.frame(r_mm, xy = TRUE, na.rm = TRUE)

# Pivot to long format — one row per pixel per month
df_long <- df_wide %>%
  tidyr::pivot_longer(
    cols      = starts_with("pet_"),
    names_to  = "layer_name",
    values_to = "pet_mm"
  ) %>%
  # Extract year and month from layer name
  mutate(
    year    = as.integer(substr(layer_name, 5, 8)),
    month   = as.integer(substr(layer_name, 10, 11)),
    date    = as.Date(paste(year, month, "01", sep = "-")),
    pet_mm  = round(pet_mm, 2)
  ) %>%
  # Select and order final columns
  select(x, y, date, year, month, pet_mm) %>%
  arrange(x, y, date)

# ============================================================
# Preview
# ============================================================
cat("=== LONG FORMAT (one row per pixel per month) ===\n")
cat(sprintf("Total rows  : %s\n",
            format(nrow(df_long), big.mark = ",")))
cat(sprintf("Pixels      : %s\n",
            format(n_distinct(paste(df_long$x, df_long$y)),
                   big.mark = ",")))
cat(sprintf("Date range  : %s to %s\n",
            min(df_long$date), max(df_long$date)))
cat(sprintf("Months      : %d\n\n", n_distinct(df_long$date)))

print(head(df_long, 12))

# ============================================================
# Also show wide format — one row per pixel, columns = Year_Month
# ============================================================
cat("\n=== WIDE FORMAT (one row per pixel, columns = Year_Month) ===\n")

df_wide_clean <- df_long %>%
  mutate(col_name = format(date, "%Y_%m")) %>%
  select(x, y, col_name, pet_mm) %>%
  tidyr::pivot_wider(
    names_from  = col_name,
    values_from = pet_mm
  )

cat(sprintf("Rows        : %s  (one per pixel)\n",
            format(nrow(df_wide_clean), big.mark = ",")))
cat(sprintf("Columns     : %d  (x, y + 276 months)\n\n",
            ncol(df_wide_clean)))

# Show first 5 pixels, first 6 month columns only
preview_cols <- c("x", "y",
                  grep("^20", names(df_wide_clean),
                       value = TRUE)[1:6])
print(df_wide_clean[1:5, preview_cols])