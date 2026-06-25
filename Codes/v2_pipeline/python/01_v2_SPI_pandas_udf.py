# =============================================================================
# 01_v2. SPI Calculation — PySpark Pandas UDF (distributed, multi-node)
# =============================================================================
# Architecture: PySpark Pandas UDF grouped by (x, y) — one pixel per task.
# Data stays in Spark throughout — no toPandas() on monthly data.
# Country assigned via bbox filter only (Benin bbox tight enough).
#
# Data source: `era5-daily-data`.silver_staging.era5_daily_nn (global, 0.25 deg,
#              GWSC product pipeline ERA5 source, 1950-2026, PET-matched grid)
# SPI method: climate-indices package (gamma distribution, matches R SPEI pkg)
#
# Requires cluster libraries: climate-indices, geopandas
#
# Output: spi_results.parquet in VOLUME_PATH
#   Read in R scripts 03+: arrow::read_parquet("...spi_results.parquet")
# =============================================================================

# -- USER SETTINGS ------------------------------------------------------------
COUNTRY_FILTER  = ["Benin"]   # change to all 6 for production run
ERA5_YEAR_START = 1950
ERA5_YEAR_END   = 2023
VOLUME_PATH     = "/Volumes/projects/gwsc_drought/drought_outputs/v2"
# -----------------------------------------------------------------------------

from pyspark.sql import SparkSession
spark = SparkSession.builder.getOrCreate()

import numpy as np
import pandas as pd
import geopandas as gpd
from shapely import wkb
from pyspark.sql import functions as F
from pyspark.sql.types import (
    StructType, StructField, DoubleType, IntegerType, StringType
)
from pyspark.sql.functions import pandas_udf, PandasUDFType

print("Spark session active:", spark.version)


# -- Load country boundaries (driver, small collect) --------------------------
print("Loading country boundaries...")
boundaries_raw = (
    spark.table("`era5-daily-data`.silver_staging.esri_worldcountryboundaries_global_silver")
    .select("COUNTRY", "chip_wkb")
    .filter(F.col("COUNTRY").isin(COUNTRY_FILTER))
    .toPandas()
)

boundaries_raw["geometry"] = boundaries_raw["chip_wkb"].apply(
    lambda b: wkb.loads(bytes(b))
)
boundaries_gdf = gpd.GeoDataFrame(boundaries_raw, geometry="geometry", crs=4326)
boundaries_gdf = boundaries_gdf[boundaries_gdf.geometry.is_valid]
print(f"Boundaries loaded: {len(boundaries_gdf)} features for {COUNTRY_FILTER}")

# Dynamic bounding box — filters ERA5 in Spark before any collect
bb = boundaries_gdf.total_bounds  # [xmin, ymin, xmax, ymax]
print(f"Bbox: lon [{bb[0]:.2f}, {bb[2]:.2f}], lat [{bb[1]:.2f}, {bb[3]:.2f}]")


# -- ERA5 daily -> monthly aggregation fully in Spark -------------------------
print("Aggregating ERA5 daily -> monthly in Spark (no collect)...")
era5_raw = spark.table("`era5-daily-data`.silver_staging.era5_daily_nn")

monthly_spark = (
    era5_raw
    .withColumn("lon_180",
        F.when(F.col("longitude") > 180, F.col("longitude") - 360)
         .otherwise(F.col("longitude")))
    .withColumn("year",  F.year("time"))
    .withColumn("month", F.month("time"))
    .filter((F.col("year") >= ERA5_YEAR_START) & (F.col("year") <= ERA5_YEAR_END))
    .filter(
        (F.col("lon_180")  >= float(bb[0])) & (F.col("lon_180")  <= float(bb[2])) &
        (F.col("latitude") >= float(bb[1])) & (F.col("latitude") <= float(bb[3]))
    )
    .groupBy("lon_180", "latitude", "year", "month")
    .agg(
        F.sum("sum_tp_mm").alias("monthly_precip"),
        F.avg("mean_t2m_c").alias("mean_temp_monthly"),
        F.count("time").alias("n_days_observed"),
        F.sum(F.col("sum_tp_mm").isNull().cast("int")).alias("n_days_missing")
    )
    .filter(F.col("n_days_missing") < 5)
    .withColumnRenamed("lon_180",  "x")
    .withColumnRenamed("latitude", "y")
)

# -- Polygon clip: keep only pixels INSIDE the country boundary ----------------
# The bbox above is a rectangle; it grabs ocean + neighbour-country cells that
# would otherwise be mislabelled as this country. Clip the distinct pixel grid
# to the boundary polygon on the driver (only ~hundreds of pixels), then keep
# matching rows. Same gpd.sjoin(predicate="within") approach as 02a/02b so SPI
# and SPEI share an identical pixel set. Country assigned from the spatial join.
pixels = monthly_spark.select("x", "y").distinct().toPandas()
pixels_gdf = gpd.GeoDataFrame(
    pixels, geometry=gpd.points_from_xy(pixels["x"], pixels["y"]), crs=4326
)
joined = gpd.sjoin(pixels_gdf, boundaries_gdf[["COUNTRY", "geometry"]],
                   how="left", predicate="within")
pixels_with_country = (
    joined[joined["COUNTRY"].notna()][["x", "y", "COUNTRY"]]
    .rename(columns={"COUNTRY": "country"})
    .drop_duplicates(subset=["x", "y"])
)
print(f"Polygon clip: {len(pixels_with_country)} of {len(pixels)} bbox pixels inside boundary")

valid_sdf = spark.createDataFrame(pixels_with_country[["x", "y", "country"]])
monthly_spark = monthly_spark.join(F.broadcast(valid_sdf), on=["x", "y"], how="inner")

# Repartition by pixel for even UDF distribution
n_par = spark.sparkContext.defaultParallelism * 2
monthly_spark = monthly_spark.repartition(n_par, "x", "y")
print(f"Monthly Spark DF ready. Partitions: {n_par}")


# -- SPI Pandas UDF -----------------------------------------------------------
spi_schema = StructType([
    StructField("x",                 DoubleType(),  True),
    StructField("y",                 DoubleType(),  True),
    StructField("country",           StringType(),  True),
    StructField("year",              IntegerType(), True),
    StructField("month",             IntegerType(), True),
    StructField("monthly_precip",    DoubleType(),  True),
    StructField("mean_temp_monthly", DoubleType(),  True),
    StructField("n_days_observed",   IntegerType(), True),
    StructField("n_days_missing",    IntegerType(), True),
    StructField("spi1",              DoubleType(),  True),
    StructField("spi3",              DoubleType(),  True),
    StructField("spi6",              DoubleType(),  True),
    StructField("spi12",             DoubleType(),  True),
])

@pandas_udf(spi_schema, PandasUDFType.GROUPED_MAP)
def spi_udf(df: pd.DataFrame) -> pd.DataFrame:
    from climate_indices import indices, compute

    df = df.sort_values(["year", "month"]).reset_index(drop=True)
    precip = df["monthly_precip"].values.astype(float)

    def safe_spi(precip_arr, scale):
        min_obs = max(scale * 4, 24)
        valid = precip_arr[~np.isnan(precip_arr)]
        if len(valid) < min_obs:
            return np.full(len(precip_arr), np.nan)
        try:
            result = indices.spi(
                values=precip_arr,
                scale=scale,
                distribution=indices.Distribution.gamma,
                data_start_year=int(df["year"].iloc[0]),
                calibration_year_initial=int(df["year"].iloc[0]),
                calibration_year_final=int(df["year"].iloc[-1]),
                periodicity=compute.Periodicity.monthly
            )
            return np.where(np.isinf(result), np.nan, result)
        except Exception:
            return np.full(len(precip_arr), np.nan)

    df["spi1"]  = safe_spi(precip, 1)
    df["spi3"]  = safe_spi(precip, 3)
    df["spi6"]  = safe_spi(precip, 6)
    df["spi12"] = safe_spi(precip, 12)

    return df[[
        "x", "y", "country", "year", "month",
        "monthly_precip", "mean_temp_monthly",
        "n_days_observed", "n_days_missing",
        "spi1", "spi3", "spi6", "spi12"
    ]].astype({
        "x": float, "y": float, "country": str,
        "year": "int32", "month": "int32",
        "monthly_precip": float, "mean_temp_monthly": float,
        "n_days_observed": "int32", "n_days_missing": "int32",
        "spi1": float, "spi3": float, "spi6": float, "spi12": float
    })


# -- Apply UDF + save directly from Spark (no collect to driver) --------------
print("Running SPI via Pandas UDF (distributed, no driver collect)...")
import os
os.makedirs(VOLUME_PATH, exist_ok=True)
out_path = f"{VOLUME_PATH}/spi_results.parquet"

spi_sdf = monthly_spark.groupby("x", "y").apply(spi_udf)

# Disable Arrow — climate-indices on workers changes numpy version, conflicts with pyarrow
spark.conf.set("spark.sql.execution.arrow.pyspark.enabled", "false")

# Collect to driver then save — Volume path doesn't support Spark native writer
print("Collecting SPI results to driver...")
spi_results = spi_sdf.toPandas()
n_pixels = spi_results[["x", "y"]].drop_duplicates().shape[0]
print(f"SPI complete! {len(spi_results):,} rows, {n_pixels} pixels")

spi_results.to_parquet(out_path, index=False)
print("Read in R: arrow::read_parquet('/Volumes/.../spi_results.parquet')")
