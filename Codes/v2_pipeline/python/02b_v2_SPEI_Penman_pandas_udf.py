# =============================================================================
# 02b_v2. SPEI Calculation — Penman-Monteith PET — PySpark Pandas UDF
# =============================================================================
# PET: precomputed Penman-Monteith, loaded from NC files on Volume.
# Joined to ERA5 precip on driver before distributing to workers.
#
# Output: spei_penman_results.parquet in VOLUME_PATH
#   Read in R: arrow::read_parquet("...spei_penman_results.parquet")
# =============================================================================

# -- USER SETTINGS ------------------------------------------------------------
# COUNTRY is injected by the runner (_run_py_step) from the job parameter, so
# every task in a run agrees. Falls back to Benin for standalone/notebook use.
COUNTRY_TAG     = globals().get("COUNTRY", "Benin")
COUNTRY_FILTER  = [COUNTRY_TAG]   # one country per run
ERA5_YEAR_START = 1950
ERA5_YEAR_END   = 2023   # CRU PET runs 1901-2023; inner-join caps SPEI-P here
# PET now = CRU TS 4.08 Penman-Monteith, staged as parquet on the Volume
# (see CRU_PET_PARQUET below). Old ERA5-Land pev NC dir retired (was wrong).
# Per-country output dir: filenames stay identical across countries, so runs for
# different countries never collide and downstream scripts need no rename logic.
VOLUME_PATH     = f"/Volumes/projects/gwsc_drought/drought_outputs/v2/{COUNTRY_TAG}"
# -----------------------------------------------------------------------------

import subprocess, sys

def pip_install(pkg):
    subprocess.check_call([sys.executable, "-m", "pip", "install", pkg, "-q"])

for pkg in ["climate-indices", "geopandas", "xarray", "netCDF4"]:
    try:
        __import__(pkg.replace("-", "_"))
    except ImportError:
        pip_install(pkg)

from pyspark.sql import SparkSession
spark = SparkSession.builder.getOrCreate()

import os
import numpy as np
import pandas as pd
import geopandas as gpd
import xarray as xr
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

bb = boundaries_gdf.total_bounds


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


# -- Load Penman-Monteith PET (CRU TS 4.08) ----------------------------------
# CORRECTED 2026-07: the old ERA5-Land `pev` files were WRONG for SPEI —
# ERA5-Land pev is open-water/pan evaporation (not FAO reference ETo) AND the
# metres->mm conversion dropped the ×days_in_month factor, giving Benin PET of
# ~200-400 mm/yr instead of ~1300-1600. Replaced with CRU TS 4.08 PET, which is
# a genuine Penman-Monteith reference PET (temp + vapour pressure + cloud + wind
# climatology; Harris et al. 2020, Sci Data), monthly, 1901-2023.
#
# PET is pre-clipped/aggregated to this country's ERA5 grid (mm/month) by the
# local script Codes/pdsi_validation/P1_cru_pet_download_clip.py and staged as a
# parquet on the Volume. Columns: x, y, year, month, pet_mm.
# The country lives in the directory (VOLUME_PATH), not the filename, so each
# country's PET sits beside its own outputs under the same name.
CRU_PET_PARQUET = os.path.join(VOLUME_PATH, "cru_pet_monthly.parquet")
print(f"Loading CRU Penman-Monteith PET: {CRU_PET_PARQUET}")
pet_df = pd.read_parquet(CRU_PET_PARQUET)[["x", "y", "year", "month", "pet_mm"]]
pet_df = pet_df.drop_duplicates(subset=["x", "y", "year", "month"])
pet_df["x"] = pet_df["x"].round(4)
pet_df["y"] = pet_df["y"].round(4)
print(f"PET loaded: {len(pet_df):,} rows, "
      f"{pet_df[['x','y']].drop_duplicates().shape[0]} pixels, "
      f"{pet_df.year.min()}-{pet_df.year.max()}, "
      f"mean {pet_df.pet_mm.mean():.1f} mm/mo")


# -- Join PET to clipped ERA5 (driver) ----------------------------------------
# era5_daily_nn is on the SAME .125/.375/.625/.875 grid as the PET files,
# so an exact (x, y, year, month) merge works — no grid snap needed.
monthly_clipped["x"] = monthly_clipped["x"].round(4)
monthly_clipped["y"] = monthly_clipped["y"].round(4)

monthly_with_pet = monthly_clipped.merge(
    pet_df, on=["x", "y", "year", "month"], how="inner"
)
monthly_with_pet["water_balance"] = monthly_with_pet["monthly_precip"] - monthly_with_pet["pet_mm"]
pct = round(100 * len(monthly_with_pet) / max(len(monthly_clipped), 1), 1)
print(f"After PET join: {len(monthly_with_pet):,} rows ({pct}% of clipped ERA5)")
if len(monthly_with_pet) == 0:
    raise ValueError("PET join produced 0 rows — check PET/ERA5 grid overlap")


# -- SPEI-Penman Pandas UDF ---------------------------------------------------
spei_p_schema = StructType([
    StructField("x",              DoubleType(),  True),
    StructField("y",              DoubleType(),  True),
    StructField("country",        StringType(),  True),
    StructField("year",           IntegerType(), True),
    StructField("month",          IntegerType(), True),
    StructField("monthly_precip", DoubleType(),  True),
    StructField("n_days_missing", IntegerType(), True),
    StructField("pet_mm",         DoubleType(),  True),
    StructField("water_balance",  DoubleType(),  True),
    StructField("spei1",          DoubleType(),  True),
    StructField("spei3",          DoubleType(),  True),
    StructField("spei6",          DoubleType(),  True),
    StructField("spei12",         DoubleType(),  True),
])

@pandas_udf(spei_p_schema, PandasUDFType.GROUPED_MAP)
def spei_p_udf(df: pd.DataFrame) -> pd.DataFrame:
    from climate_indices import indices, compute

    df = df.sort_values(["year", "month"]).reset_index(drop=True)
    wb = df["water_balance"].values.astype(float)

    def safe_spei(wb_arr, scale):
        min_obs = max(scale * 4, 24)
        valid = wb_arr[~np.isnan(wb_arr)]
        if len(valid) < min_obs:
            return np.full(len(wb_arr), np.nan)
        try:
            result = indices.spei(
                precips_mm=df["monthly_precip"].values.astype(float),
                pet_mm=df["pet_mm"].values.astype(float),  # precomputed Penman PET
                scale=scale,
                distribution=indices.Distribution.pearson,
                periodicity=compute.Periodicity.monthly,
                data_start_year=int(df["year"].iloc[0]),
                calibration_year_initial=int(df["year"].iloc[0]),
                calibration_year_final=int(df["year"].iloc[-1]),
            )
            return np.where(np.isinf(result), np.nan, result)
        except Exception as e:
            print(f"[spei scale={scale}] failed: {type(e).__name__}: {e}")
            return np.full(len(wb_arr), np.nan)

    df["spei1"]  = safe_spei(wb, 1)
    df["spei3"]  = safe_spei(wb, 3)
    df["spei6"]  = safe_spei(wb, 6)
    df["spei12"] = safe_spei(wb, 12)

    return df[[
        "x", "y", "country", "year", "month",
        "monthly_precip", "n_days_missing",
        "pet_mm", "water_balance",
        "spei1", "spei3", "spei6", "spei12"
    ]].astype({
        "x": float, "y": float, "country": str,
        "year": "int32", "month": "int32",
        "monthly_precip": float, "n_days_missing": "int32",
        "pet_mm": float, "water_balance": float,
        "spei1": float, "spei3": float, "spei6": float, "spei12": float
    })


# -- Distribute ---------------------------------------------------------------
print("Creating Spark DataFrame from joined data...")
monthly_sdf = spark.createDataFrame(monthly_with_pet)
monthly_sdf = monthly_sdf.repartition(spark.sparkContext.defaultParallelism * 2, "x", "y")

print("Running SPEI-Penman via Pandas UDF (distributed)...")
spei_p_sdf = monthly_sdf.groupby("x", "y").apply(spei_p_udf)

print("Collecting results...")
spei_penman_results = spei_p_sdf.toPandas()
n_pixels = spei_penman_results[["x", "y"]].drop_duplicates().shape[0]
print(f"SPEI-Penman complete! {len(spei_penman_results):,} rows, {n_pixels} pixels")


# -- Save output --------------------------------------------------------------
os.makedirs(VOLUME_PATH, exist_ok=True)
out_path = os.path.join(VOLUME_PATH, "spei_penman_results.parquet")
spei_penman_results.to_parquet(out_path, index=False)
print(f"Saved: {out_path}")
