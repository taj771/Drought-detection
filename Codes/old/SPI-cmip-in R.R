# ======================================================================
# SPI Calculation — CMIP Future Data
# Mirrors ERA5 historical approach exactly — model added as extra dimension
# 4,660 pixels × 17 models × 45 years = ~43M monthly rows
# ======================================================================


# ----------------------------------------------------------------------
# SECTION 1: Libraries
# ----------------------------------------------------------------------
library(here)
library(sparklyr)
library(dplyr)
library(dbplyr)
library(SPEI)


# ----------------------------------------------------------------------
# SECTION 2: Connect
# ----------------------------------------------------------------------
sc <- spark_connect(
  cluster_id = Sys.getenv("DATABRICKS_CLUSTER_ID"),
  method     = "databricks_connect"
)


# ----------------------------------------------------------------------
# SECTION 3: Load CMIP data
# ----------------------------------------------------------------------
project_folder <- "xx_drought_index"

cmip_normals <- tbl(sc, in_catalog("projects", project_folder,
                                   "drought_index_cmip_future"))


# ----------------------------------------------------------------------
# SECTION 4: Aggregate Daily → Monthly in Spark
# Identical to ERA5 pipeline — model added to group_by only
# No arrange() in Spark — sort happens in R after collect()
# ----------------------------------------------------------------------
monthly_spark <- cmip_normals %>%
  
  group_by(x, y, year, month, model) %>%   # ← model added, otherwise identical to ERA5
  
  filter(year == 2027)%>%
  
  summarise(
    monthly_precip    = sum(precip, na.rm = TRUE),
    mean_temp_monthly = mean(mean_temp, na.rm = TRUE),
    n_days_observed   = n(),
    n_days_missing    = sum(as.integer(is.na(precip))),
    .groups = "drop"
  ) %>%
  
  filter(n_days_missing < 5)
# No arrange() here — Spark column resolution unreliable on lazy tbls


# ----------------------------------------------------------------------
# SECTION 5: Collect to R
# 4,660 pixels × 17 models × 45 years × 12 months = ~43M rows
# ERA5 handled 4,660 × 50 years × 12 = ~2.8M fine
# CMIP is 17x larger — if memory is tight reduce via filter below
# ----------------------------------------------------------------------
monthly_r <- monthly_spark %>% collect()

message("Collected : ", nrow(monthly_r), " rows")
message("Pixels    : ", n_distinct(paste(monthly_r$x, monthly_r$y)))
message("Models    : ", n_distinct(monthly_r$model))
message("Memory    : ", format(object.size(monthly_r), units = "MB"))


# ----------------------------------------------------------------------
# SECTION 6: Define SPI calculator — identical to ERA5
# ----------------------------------------------------------------------
safe_spi <- function(precip_vec, scale = 3) {
  tryCatch({
    min_obs <- max(scale * 4, 24)
    if (sum(!is.na(precip_vec)) < min_obs) {
      return(rep(NA_real_, length(precip_vec)))
    }
    as.numeric(spi(precip_vec, scale = scale, na.rm = TRUE)$fitted)
  }, error = function(e) {
    return(rep(NA_real_, length(precip_vec)))
  })
}


# ----------------------------------------------------------------------
# SECTION 7: Apply SPI — identical to ERA5, model added to group_by
#
# Each pixel+model combination gets its OWN Gamma distribution
# A pixel under model "CESM2" and the same pixel under "MPI-ESM"
# are each compared to THEIR OWN model's precipitation history
# ----------------------------------------------------------------------
spi_results <- monthly_r %>%
  
  arrange(x, y, model, year, month) %>%   # ← model added to sort
  
  group_by(x, y, model) %>%              # ← model added to group_by
  
  mutate(
    spi1  = safe_spi(monthly_precip, scale = 1),
    spi3  = safe_spi(monthly_precip, scale = 3),
    spi6  = safe_spi(monthly_precip, scale = 6),
    spi12 = safe_spi(monthly_precip, scale = 12)
  ) %>%
  
  ungroup()

message("SPI complete! Rows: ", nrow(spi_results))
head(spi_results)


# ----------------------------------------------------------------------
# SECTION 8: Save
# ----------------------------------------------------------------------
dir.create(here("Data"), showWarnings = FALSE, recursive = TRUE)
saveRDS(spi_results, here("Data", "spi_results_cmip.rds"))
message("Saved to: ", here("Data", "spi_results_cmip.rds"))