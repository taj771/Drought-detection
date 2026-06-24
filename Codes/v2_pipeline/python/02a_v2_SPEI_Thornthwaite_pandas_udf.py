# =============================================================================
# 02a_v2. SPEI Calculation — Thornthwaite PET — PySpark Pandas UDF
# =============================================================================
# PET method: Thornthwaite (temperature-based). climate-indices computes PET
# internally when pet_mm=None and lat is passed.
#
# Output: spei_thornthwaite_results.parquet in VOLUME_PATH
#   Read in R: arrow::read_parquet("...spei_thornthwaite_results.parquet")
# =============================================================================

# -- USER SETTINGS ------------------------------------------------------------
COUNTRY_FILTER  = ["Benin"]
ERA5_YEAR_START = 1950
ERA5_YEAR_END   = 2023
VOLUME_PATH     = "/Volumes/projects/gwsc_drought/drought_outputs/v2"
# -----------------------------------------------------------------------------

import subprocess, sys

def pip_install(pkg):
    subprocess.check_call([sys.executable, "-m", "pip", "install", pkg, "-q"])

for pkg in ["climate-indices", "geopandas"]:
    try:
        __import__(pkg.replace("-", "_"))
    except ImportError:
        pip_install(pkg)

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


# -- Load country boundaries (driver) ----------------------------------------
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
boundaries_gdf = boundaries_gdf.drop(columns=["chip_wkb"])
boundaries_gdf = boundaries_gdf[boundaries_gdf.geometry.is_valid]
print(f"Boundaries loaded: {len(boundaries_gdf)} features for {COUNTRY_FILTER}")

bb = boundaries_gdf.total_bounds  # [xmin, ymin, xmax, ymax]


# -- ERA5 daily -> monthly aggregation in Spark -------------------------------
era5_raw = spark.table("`era5-daily-data`.silver_staging.era5_daily_nn")

era5_conv = (
    era5_raw
    .withColumn("lon_180",
        F.when(F.col("longitude") > 180, F.col("longitude") - 360)
         .otherwise(F.col("longitude")))
    .withColumn("year",  F.year("time"))
    .withColumn("month", F.month("time"))
    .filter((F.col("year") >= ERA5_YEAR_START) & (F.col("year") <= ERA5_YEAR_END))
    .filter(
        (F.col("lon_180") >= float(bb[0])) & (F.col("lon_180") <= float(bb[2])) &
        (F.col("latitude") >= float(bb[1])) & (F.col("latitude") <= float(bb[3]))
    )
)

monthly_spark = (
    era5_conv
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

print("Collecting monthly data to driver...")
monthly_df = monthly_spark.toPandas()
print(f"Collected: {len(monthly_df):,} rows, {monthly_df[['x','y']].drop_duplicates().shape[0]} pixels")


# -- Spatial clip (driver) ----------------------------------------------------
pixels = monthly_df[["x", "y"]].drop_duplicates().copy()
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
print(f"Pixels after clip: {len(pixels_with_country)} (of {len(pixels)} total)")

monthly_clipped = monthly_df.merge(pixels_with_country, on=["x", "y"], how="inner").reset_index(drop=True)
print(f"Rows after clip: {len(monthly_clipped):,}")


# -- SPEI-Thornthwaite Pandas UDF ---------------------------------------------
spei_t_schema = StructType([
    StructField("x",                 DoubleType(),  True),
    StructField("y",                 DoubleType(),  True),
    StructField("country",           StringType(),  True),
    StructField("year",              IntegerType(), True),
    StructField("month",             IntegerType(), True),
    StructField("monthly_precip",    DoubleType(),  True),
    StructField("mean_temp_monthly", DoubleType(),  True),
    StructField("n_days_observed",   IntegerType(), True),
    StructField("n_days_missing",    IntegerType(), True),
    StructField("water_balance",     DoubleType(),  True),
    StructField("spei1",             DoubleType(),  True),
    StructField("spei3",             DoubleType(),  True),
    StructField("spei6",             DoubleType(),  True),
    StructField("spei12",            DoubleType(),  True),
])

@pandas_udf(spei_t_schema, PandasUDFType.GROUPED_MAP)
def spei_t_udf(df: pd.DataFrame) -> pd.DataFrame:
    from climate_indices import indices, compute

    df = df.sort_values(["year", "month"]).reset_index(drop=True)
    precip = df["monthly_precip"].values.astype(float)
    temp   = df["mean_temp_monthly"].values.astype(float)
    lat    = float(df["y"].iloc[0])
    start_year = int(df["year"].iloc[0])
    n = len(precip)

    # Thornthwaite PET computed once from monthly mean temperature + latitude.
    # climate_indices.indices.spei() does NOT compute PET internally — it must
    # be supplied. indices.pet() returns a monthly PET ndarray (mm).
    def compute_pet():
        if np.all(np.isnan(temp)):
            return np.full(n, np.nan)
        try:
            t = np.where(np.isnan(temp), 0.0, temp)   # pet() rejects NaNs
            return indices.pet(temperature_celsius=t,
                               latitude_degrees=lat,
                               data_start_year=start_year)
        except Exception as e:
            print(f"[pet] failed: {type(e).__name__}: {e}")
            return np.full(n, np.nan)

    pet = compute_pet()

    def safe_spei(precip_arr, pet_arr, scale):
        min_obs = max(scale * 4, 24)
        valid = precip_arr[~np.isnan(precip_arr)]
        if len(valid) < min_obs or np.all(np.isnan(pet_arr)):
            return np.full(len(precip_arr), np.nan)
        try:
            result = indices.spei(
                precips_mm=precip_arr,
                pet_mm=pet_arr,        # Thornthwaite PET (precomputed above)
                scale=scale,
                distribution=indices.Distribution.pearson,
                periodicity=compute.Periodicity.monthly,
                data_start_year=start_year,
                calibration_year_initial=start_year,
                calibration_year_final=int(df["year"].iloc[-1]),
            )
            return np.where(np.isinf(result), np.nan, result)
        except Exception as e:
            print(f"[spei scale={scale}] failed: {type(e).__name__}: {e}")
            return np.full(len(precip_arr), np.nan)

    spei1  = safe_spei(precip, pet, 1)
    spei3  = safe_spei(precip, pet, 3)
    spei6  = safe_spei(precip, pet, 6)
    spei12 = safe_spei(precip, pet, 12)
    pet1 = pet12 = pet

    # water_balance = precip - PET (use scale-1 PET as representative)
    # climate-indices returns PET alongside SPEI at scale=1
    pet = pet1
    df["water_balance"] = precip - pet

    df["spei1"]  = spei1
    df["spei3"]  = spei3
    df["spei6"]  = spei6
    df["spei12"] = spei12

    return df[[
        "x", "y", "country", "year", "month",
        "monthly_precip", "mean_temp_monthly",
        "n_days_observed", "n_days_missing",
        "water_balance", "spei1", "spei3", "spei6", "spei12"
    ]].astype({
        "x": float, "y": float, "country": str,
        "year": "int32", "month": "int32",
        "monthly_precip": float, "mean_temp_monthly": float,
        "n_days_observed": "int32", "n_days_missing": "int32",
        "water_balance": float,
        "spei1": float, "spei3": float, "spei6": float, "spei12": float
    })


# -- Distribute ---------------------------------------------------------------
print("Creating Spark DataFrame from clipped data...")
monthly_sdf = spark.createDataFrame(monthly_clipped)
monthly_sdf = monthly_sdf.repartition(spark.sparkContext.defaultParallelism * 2, "x", "y")

print("Running SPEI-Thornthwaite via Pandas UDF (distributed)...")
spei_t_sdf = monthly_sdf.groupby("x", "y").apply(spei_t_udf)

print("Collecting results...")
spei_results = spei_t_sdf.toPandas()
n_pixels = spei_results[["x", "y"]].drop_duplicates().shape[0]
print(f"SPEI-Thornthwaite complete! {len(spei_results):,} rows, {n_pixels} pixels")


# -- Save output --------------------------------------------------------------
import os
os.makedirs(VOLUME_PATH, exist_ok=True)
out_path = os.path.join(VOLUME_PATH, "spei_thornthwaite_results.parquet")
spei_results.to_parquet(out_path, index=False)
print(f"Saved: {out_path}")
