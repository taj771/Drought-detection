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
# COUNTRY is injected by the runner (_run_py_step) from the job parameter, so
# every task in a run agrees. Falls back to Benin for standalone/notebook use.
COUNTRY_TAG     = globals().get("COUNTRY", "Benin")
ERA5_YEAR_START = 1950
ERA5_YEAR_END   = 2023
# Per-country output dir: filenames stay identical across countries, so runs for
# different countries never collide and downstream scripts need no rename logic.
VOLUME_PATH     = f"/Volumes/projects/gwsc_drought/drought_outputs/v2/{COUNTRY_TAG}"
# -----------------------------------------------------------------------------

import subprocess, sys

def pip_install(pkg):
    subprocess.check_call([sys.executable, "-m", "pip", "install", pkg, "-q"])

# Pinned: releases can change SPI values (3.0.0 fixed the probability of zero
# rainfall). Must match init_numpy_pin.sh. Fails loudly if the cluster has a
# different version.
CLIMATE_INDICES = "3.0.0"
try:
    import climate_indices
except ImportError:
    pip_install(f"climate-indices=={CLIMATE_INDICES}")
from importlib.metadata import version
assert version("climate-indices") == CLIMATE_INDICES, (
    f"climate-indices {version('climate-indices')} installed, expected {CLIMATE_INDICES}"
)

from pyspark.sql import SparkSession
spark = SparkSession.builder.getOrCreate()

import os
import numpy as np
import pandas as pd
from pyspark.sql.types import (
    StructType, StructField, DoubleType, IntegerType, StringType
)
from pyspark.sql import functions as F
from pyspark.sql.functions import pandas_udf, PandasUDFType

print("Spark session active:", spark.version)


# -- Monthly ERA5 (built once by 00_v2_ERA5_Monthly) ---------------------------
# Already deduplicated to one row per pixel-day, aggregated to monthly, stripped
# of incomplete months and clipped to the country polygon. This script only
# applies its own year window. Read and filtered in Spark, so the monthly data
# never passes through the driver; only a one-row summary is collected.
monthly_path = f"{VOLUME_PATH}/era5_monthly.parquet"
print(f"Loading monthly ERA5: {monthly_path}")
monthly_sdf = (
    spark.read.parquet(monthly_path)
    .filter((F.col("year") >= ERA5_YEAR_START) & (F.col("year") <= ERA5_YEAR_END))
)
_summary = monthly_sdf.agg(
    F.count(F.lit(1)).alias("rows"),
    F.countDistinct("x", "y").alias("pixels"),
    F.max(F.col("year") * 100 + F.col("month")).alias("last"),
).first()
if _summary["rows"] == 0:
    raise ValueError(f"no monthly rows for {COUNTRY_TAG} in {ERA5_YEAR_START}-{ERA5_YEAR_END}")
print(f"Monthly ERA5: {_summary['rows']:,} rows, {_summary['pixels']} pixels")
# Logged so a run with monthly_00 skipped (STEPS) shows which file it used.
_built = pd.Timestamp(os.path.getmtime(monthly_path), unit="s").strftime("%Y-%m-%d %H:%M")
print(f"era5_monthly.parquet written {_built} UTC; last month used {_summary['last']}")


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
monthly_sdf = monthly_sdf.repartition(spark.sparkContext.defaultParallelism * 2, "x", "y")

print("Running SPEI-Thornthwaite via Pandas UDF (distributed)...")
spei_t_sdf = monthly_sdf.groupby("x", "y").apply(spei_t_udf)

print("Collecting results...")
spei_results = spei_t_sdf.toPandas()
n_pixels = spei_results[["x", "y"]].drop_duplicates().shape[0]
print(f"SPEI-Thornthwaite complete! {len(spei_results):,} rows, {n_pixels} pixels")


# -- Save output --------------------------------------------------------------
os.makedirs(VOLUME_PATH, exist_ok=True)
out_path = os.path.join(VOLUME_PATH, "spei_thornthwaite_results.parquet")
spei_results.to_parquet(out_path, index=False)
print(f"Saved: {out_path}")
