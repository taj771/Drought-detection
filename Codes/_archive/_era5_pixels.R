# Extract the 154 ERA5 pixel x/y coords + grid step from local events.parquet.
# Needed to map NDVI 0.09deg cells onto ERA5 pixels (aggregate option A).
suppressMessages({ library(arrow); library(dplyr) })
ev <- as.data.frame(read_parquet("Codes/_archive/_data/events.parquet"))
px <- ev |> distinct(x, y) |> arrange(y, x)
cat("n pixels:", nrow(px), "\n")
cat("x range:", range(px$x), "  y range:", range(px$y), "\n")
xs <- sort(unique(px$x)); ys <- sort(unique(px$y))
cat("x step:", round(median(diff(xs)),4), "  y step:", round(median(diff(ys)),4), "\n")
cat("lon convention:", if (min(px$x) < 0) "-180..180 (has neg)" else "all positive (0-360 or E-only)", "\n")
write.csv(px, "Codes/ndvi_validation/era5_pixels_benin.csv", row.names=FALSE)
cat("wrote era5_pixels_benin.csv\n")
print(head(px, 5)); print(tail(px, 5))
