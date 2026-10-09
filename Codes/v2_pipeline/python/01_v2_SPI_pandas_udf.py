# =============================================================================
# 01_v2. SPI Calculation — PySpark Pandas UDF (distributed, multi-node)
# =============================================================================
# Architecture: PySpark Pandas UDF grouped by (x, y) — one pixel per task.
# Reads the monthly series built by 00_v2_ERA5_Monthly (era5_monthly.parquet).
# Dedupe, completeness checks and the polygon clip all happen in 00.
#
# Input: era5_monthly.parquet in VOLUME_PATH, built by 00_v2_ERA5_Monthly from
#        `era5-daily-data`.silver_staging.era5_daily_nn (global, 0.25 deg).
# SPI method: climate-indices package (gamma distribution, matches R SPEI pkg)
#
# Requires: climate-indices — pip-installed below if absent.
#
# Output: spi_results.parquet in VOLUME_PATH
#   Read in R scripts 03+: arrow::read_parquet("...spi_results.parquet")
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

# Self-install rather than rely on cluster libraries: a cluster rebuild drops
# every attached library, and this script then fails at import. Same block as
# 02a/02b so all three steps have identical dependency handling.
import subprocess, sys

def pip_install(pkg):
    subprocess.check_call([sys.executable, "-m", "pip", "install", pkg, "-q"])

for pkg in ["climate-indices"]:
    try:
        __import__(pkg.replace("-", "_"))
    except ImportError:
        pip_install(pkg)

from pyspark.sql import SparkSession
spark = SparkSession.builder.getOrCreate()

import os
import numpy as np
import pandas as pd
from pyspark.sql.types import (
    StructType, StructField, DoubleType, IntegerType, StringType
)
from pyspark.sql.functions import pandas_udf, PandasUDFType

print("Spark session active:", spark.version)


# -- Monthly ERA5 (built once by 00_v2_ERA5_Monthly) ---------------------------
# Already deduplicated to one row per pixel-day, aggregated to monthly, stripped
# of incomplete months and clipped to the country polygon. This script only
# applies its own year window.
monthly_path = f"{VOLUME_PATH}/era5_monthly.parquet"
print(f"Loading monthly ERA5: {monthly_path}")
monthly_pdf = pd.read_parquet(monthly_path)
monthly_pdf = monthly_pdf[
    (monthly_pdf["year"] >= ERA5_YEAR_START) & (monthly_pdf["year"] <= ERA5_YEAR_END)
].reset_index(drop=True)
if monthly_pdf.empty:
    raise ValueError(f"no monthly rows for {COUNTRY_TAG} in {ERA5_YEAR_START}-{ERA5_YEAR_END}")
print(f"Monthly ERA5: {len(monthly_pdf):,} rows, "
      f"{monthly_pdf[['x', 'y']].drop_duplicates().shape[0]} pixels")
# Logged so a run with monthly_00 skipped (STEPS) shows which file it used.
_built = pd.Timestamp(os.path.getmtime(monthly_path), unit="s").strftime("%Y-%m-%d %H:%M")
_last = int((monthly_pdf["year"] * 100 + monthly_pdf["month"]).max())
print(f"era5_monthly.parquet written {_built} UTC; last month used {_last}")
monthly_spark = spark.createDataFrame(monthly_pdf)

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
