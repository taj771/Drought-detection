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
# SECTION 3: Load Data
# ----------------------------------------------------------------------
project_folder <- "xx_drought_index"

cmip_normals <- tbl(sc, in_catalog("projects", project_folder, 
                                   "drought_index_cmip_future"))

# ----------------------------------------------------------------------
# SECTION 4: Aggregate Daily → Monthly in Spark
# ----------------------------------------------------------------------
monthly_spark <- cmip_normals %>%
  
  group_by(x, y, year, month, model) %>%
  
  summarise(
    monthly_precip    = sum(precip, na.rm = TRUE),
    mean_temp_monthly = mean(mean_temp, na.rm = TRUE),
    n_days_observed   = n(),
    n_days_missing    = sum(as.integer(is.na(precip))),
    .groups = "drop"
  ) %>%
  
  filter(n_days_missing < 5) %>%
  
  arrange(x, y, model, year, month)   # model included in sort


# ----------------------------------------------------------------------
# SECTION 5: Sanity Check in Spark BEFORE collecting
# All of this runs in the cluster — no data moves to R
# ----------------------------------------------------------------------
#monthly_spark %>%
#  summarise(
#    n_rows   = n(),
#    n_pixels = n_distinct(paste(x, y)),
#    n_models = n_distinct(model)
#  ) %>%
#  collect() %>%   # only pulls back 1 row of summary stats
#  print()

# Check memory estimate — calculate expected size before committing
#monthly_spark %>%
#  summarise(n = n()) %>%
#  collect() %>%
#  mutate(
#    estimated_mb = round(n * 8 * 12 / 1e6, 1),  # 8 bytes × 12 columns
#    message      = paste0("Estimated R memory: ~", estimated_mb, " MB")
#  ) %>%
#  pull(message) %>%
#  message()

# ----------------------------------------------------------------------
# SECTION 6: Collect to R (only after sanity check passes)
# ----------------------------------------------------------------------
#monthly_r <- monthly_spark %>% collect()
#message("Memory used: ", format(object.size(monthly_r), units = "MB"))
# ----------------------------------------------------------------------
# SECTION 6: Define SPI Calculator
# ----------------------------------------------------------------------
safe_spi <- function(precip_vec, scale = 3) {
  tryCatch({
    min_obs <- max(scale * 4, 24)
    if (sum(!is.na(precip_vec)) < min_obs) return(rep(NA_real_, length(precip_vec)))
    as.numeric(spi(precip_vec, scale = scale, na.rm = TRUE)$fitted)
  }, error = function(e) rep(NA_real_, length(precip_vec)))
}


# ----------------------------------------------------------------------
# SECTION 7: Calculate SPI — grouped by pixel AND model
# ----------------------------------------------------------------------
spi_results <- monthly_r %>%
  
  arrange(x, y, model, year, month) %>%   # chronological per pixel per model
  
  group_by(x, y, model) %>%               # CRITICAL: model must be here
  
  mutate(
    spi1  = safe_spi(monthly_precip, scale = 1),
    spi3  = safe_spi(monthly_precip, scale = 3),
    spi6  = safe_spi(monthly_precip, scale = 6),
    spi12 = safe_spi(monthly_precip, scale = 12)
  ) %>%
  
  ungroup()

message("SPI done! Rows: ", nrow(spi_results))
head(spi_results)


# ----------------------------------------------------------------------
# SECTION 8: Write Back to Delta Catalog
# ----------------------------------------------------------------------
spi_spark <- copy_to(sc, spi_results,
                     name      = "spi_results_temp",
                     overwrite = TRUE)

DBI::dbExecute(sc, "
  CREATE OR REPLACE TABLE projects.xx_drought_index.drought_index_cmip_spi
  USING DELTA
  AS SELECT * FROM spi_results_temp
")

message("Written to Delta catalog.")


# ----------------------------------------------------------------------
# SECTION 9: Local Backup
# ----------------------------------------------------------------------
dir.create(here("Data"), showWarnings = FALSE, recursive = TRUE)
saveRDS(spi_results, here("Data", "spi_results_cmip.rds"))
message("Local backup saved.")