# =============================================================================
# 00_v2. ERA5 daily -> monthly, once per country
# =============================================================================
# Builds the monthly precipitation and temperature series that 01 (SPI),
# 02a (SPEI-Thornthwaite) and 02b (SPEI-Penman) all read. Before this step each
# of the three scripts queried ERA5 and repeated the same aggregation and clip.
#
# Data source: `era5-daily-data`.silver_staging.era5_daily_nn (global, 0.25 deg)
#
# One row per pixel per day is enforced, not assumed. From 2025-01 the source
# carries each pixel-day twice: a preliminary copy (data_version v1) and a
# reprocessed copy (v2). Summing both doubles the monthly rainfall, which reads
# as an absence of drought. The rule here keeps the highest version of each
# pixel-day and falls back to the lower one where no higher exists -- the same
# rule as the team's ERA5 data pull (era5_clip). Months after the v2 reprocess
# exist only as v1, so filtering to v2 alone would lose them.
#
# Two copies of the SAME version that disagree in value cannot be resolved by
# that rule, so the script stops instead of keeping an arbitrary one.
#
# Output: era5_monthly.parquet in VOLUME_PATH, clipped to the country polygon.
#   Columns: x, y, country, year, month, monthly_precip, mean_temp_monthly,
#            n_days_observed, n_days_missing, days_in_month
# =============================================================================

# -- USER SETTINGS ------------------------------------------------------------
# COUNTRY is injected by the runner (_run_py_step) from the job parameter, so
# every task in a run agrees. Falls back to Benin for standalone/notebook use.
COUNTRY_TAG     = globals().get("COUNTRY", "Benin")
# No end year: the file runs to the latest complete month, and each index
# script applies its own ERA5_YEAR_END. That keeps the index calibration
# window where it was while letting the monthly file serve the present.
ERA5_YEAR_START = 1950
# A month is kept when fewer than this many days are missing, counting both
# days with no row and days whose rainfall is null. Same threshold the index
# scripts used before, so 1950-2023 is unchanged -- 1950-01 (one day short,
# the table starts on the 2nd) still passes. The trailing month, typically
# around a third reported, is dropped.
MAX_DAYS_MISSING = 5
VOLUME_PATH     = f"/Volumes/projects/gwsc_drought/drought_outputs/v2/{COUNTRY_TAG}"
OUT_PATH        = f"{VOLUME_PATH}/era5_monthly.parquet"
# -----------------------------------------------------------------------------

ERA5_TABLE     = "`era5-daily-data`.silver_staging.era5_daily_nn"
BOUNDARY_TABLE = "`era5-daily-data`.silver_staging.esri_worldcountryboundaries_global_silver"
KEY            = ["longitude", "latitude", "time"]
VALUE_COLS     = ["sum_tp_mm", "mean_t2m_c", "max_t2m_c", "min_t2m_c"]

import subprocess, sys

def pip_install(pkg):
    subprocess.check_call([sys.executable, "-m", "pip", "install", pkg, "-q"])

for pkg in ["geopandas"]:
    try:
        __import__(pkg)
    except ImportError:
        pip_install(pkg)

from pyspark.sql import SparkSession, Window
spark = SparkSession.builder.getOrCreate()

import os
import geopandas as gpd
from shapely import wkb
from pyspark.sql import functions as F

print("Spark session active:", spark.version)


# -- Country boundary (driver, small collect) ---------------------------------
boundaries_raw = (
    spark.table(BOUNDARY_TABLE)
    .select("COUNTRY", "chip_wkb")
    .filter(F.col("COUNTRY") == COUNTRY_TAG)
    .toPandas()
)
if boundaries_raw.empty:
    raise ValueError(f"no boundary found for {COUNTRY_TAG!r} in {BOUNDARY_TABLE}")
boundaries_raw["geometry"] = boundaries_raw["chip_wkb"].apply(lambda b: wkb.loads(bytes(b)))
boundaries_gdf = gpd.GeoDataFrame(boundaries_raw, geometry="geometry", crs=4326)
boundaries_gdf = boundaries_gdf.drop(columns=["chip_wkb"])
boundaries_gdf = boundaries_gdf[boundaries_gdf.geometry.is_valid]
bb = boundaries_gdf.total_bounds  # [xmin, ymin, xmax, ymax]
print(f"{COUNTRY_TAG}: {len(boundaries_gdf)} boundary features, "
      f"bbox lon [{bb[0]:.2f}, {bb[2]:.2f}], lat [{bb[1]:.2f}, {bb[3]:.2f}]")


# -- Daily rows inside the bbox ------------------------------------------------
# Bbox filter first so the dedupe window below runs on one country, not the globe.
daily = (
    spark.table(ERA5_TABLE)
    .withColumn("lon_180",
        F.when(F.col("longitude") > 180, F.col("longitude") - 360)
         .otherwise(F.col("longitude")))
    .filter(F.year("time") >= ERA5_YEAR_START)
    .filter(
        (F.col("lon_180")  >= float(bb[0])) & (F.col("lon_180")  <= float(bb[2])) &
        (F.col("latitude") >= float(bb[1])) & (F.col("latitude") <= float(bb[3]))
    )
)


# -- One row per pixel-day ----------------------------------------------------
# 1. Collapse exact repeats of the same version. ingest_date and verified_date
#    are left out of the comparison: they record when a row arrived, not what
#    it says.
# 2. Rank the remaining rows of each pixel-day by version number, highest
#    first, and keep the top one. The number is parsed rather than compared as
#    text so a future "v10" does not sort below "v9". A missing version ranks
#    lowest. Ties on version go to the latest ingest.
# 3. Flag any pixel-day where the winning version still has more than one row:
#    same version, different values. Only the winning version is checked --
#    disagreement among superseded rows does not reach the output. Fatal.
# 4. Flag any pixel-day carrying a version label that does not parse as
#    "v<integer>". It would otherwise rank lowest without notice. Fatal.
parsed_version = F.expr("try_cast(substr(data_version, 2) AS INT)")

daily = (
    daily
    .dropDuplicates(KEY + ["data_version"] + VALUE_COLS)
    .withColumn("version_num", F.coalesce(parsed_version, F.lit(0)))
    .withColumn("bad_version",
        (F.col("data_version").isNotNull()
         & (~F.col("data_version").startswith("v") | parsed_version.isNull())).cast("int"))
)

versioned = daily   # pre-ranking rows, kept for the bad-version report

by_day         = Window.partitionBy(*KEY)
by_day_version = Window.partitionBy(*KEY, "version_num")
ranked         = by_day.orderBy(F.col("version_num").desc(),
                                F.col("ingest_date").desc_nulls_last())

daily = (
    daily
    .withColumn("rows_for_day",     F.count(F.lit(1)).over(by_day))
    .withColumn("rows_for_version", F.count(F.lit(1)).over(by_day_version))
    .withColumn("bad_version",      F.max("bad_version").over(by_day))
    .withColumn("rank",             F.row_number().over(ranked))
    .filter(F.col("rank") == 1)
    .withColumn("conflict", (F.col("rows_for_version") > 1).cast("int"))
    .withColumn("superseded", F.col("rows_for_day") - 1)
)


# -- Monthly aggregation ------------------------------------------------------
monthly = (
    daily
    .withColumn("year",  F.year("time"))
    .withColumn("month", F.month("time"))
    .groupBy("lon_180", "latitude", "year", "month")
    .agg(
        F.sum("sum_tp_mm").alias("monthly_precip"),
        F.avg("mean_t2m_c").alias("mean_temp_monthly"),
        F.count("time").alias("n_days_observed"),
        F.sum(F.col("sum_tp_mm").isNull().cast("int")).alias("n_days_missing"),
        F.sum("conflict").alias("n_conflict_days"),
        F.sum("superseded").alias("n_rows_superseded"),
        F.sum("bad_version").alias("n_bad_version_days"),
    )
    .withColumn("days_in_month",
        F.dayofmonth(F.last_day(F.make_date(F.col("year"), F.col("month"), F.lit(1)))))
    .withColumnRenamed("lon_180",  "x")
    .withColumnRenamed("latitude", "y")
)

print("Aggregating to monthly and collecting to driver...")
monthly_df = monthly.toPandas()
print(f"Collected: {len(monthly_df):,} pixel-months, "
      f"{monthly_df[['x', 'y']].drop_duplicates().shape[0]} bbox pixels")


# -- Guards -------------------------------------------------------------------
n_bad_version = int(monthly_df["n_bad_version_days"].sum())
if n_bad_version:
    labels = [r[0] for r in versioned.filter(F.col("bad_version") == 1)
              .select("data_version").distinct().limit(20).collect()]
    raise ValueError(
        f"{n_bad_version} pixel-day(s) carry a data_version that does not parse as "
        f"'v<integer>' (labels seen: {labels}). Version preference cannot rank them. "
        f"Update the parsing rule before rerunning."
    )

n_conflict = int(monthly_df["n_conflict_days"].sum())
if n_conflict:
    worst = monthly_df.nlargest(1, "n_conflict_days").iloc[0]
    raise ValueError(
        f"{n_conflict} pixel-day(s) carry two rows of the same data_version with "
        f"different values (worst {int(worst.year)}-{int(worst.month):02d}). "
        f"Version preference cannot choose between them. Investigate {ERA5_TABLE} "
        f"before rerunning."
    )

if monthly_df["days_in_month"].isna().any():
    raise ValueError("days_in_month is null for some rows; calendar guard cannot run")

over = monthly_df[monthly_df["n_days_observed"] > monthly_df["days_in_month"]]
if len(over):
    raise ValueError(
        f"{len(over)} pixel-month(s) exceed their calendar length after "
        f"deduplication (max {int(over.n_days_observed.max())} days). The source "
        f"repeats something finer than (pixel, day); rainfall would be summed twice."
    )

superseded = monthly_df[monthly_df["n_rows_superseded"] > 0]
if len(superseded):
    months = superseded[["year", "month"]].drop_duplicates().sort_values(["year", "month"])
    print(f"Dedupe: removed {int(superseded.n_rows_superseded.sum()):,} superseded "
          f"daily rows across {len(months)} month(s), "
          f"{int(months.year.iloc[0])}-{int(months.month.iloc[0]):02d} to "
          f"{int(months.year.iloc[-1])}-{int(months.month.iloc[-1]):02d}")
else:
    print("Dedupe: no repeated pixel-days in range")

# Missing days = days with no row + days whose rainfall is null.
missing_total = (monthly_df["days_in_month"] - monthly_df["n_days_observed"]
                 + monthly_df["n_days_missing"])
complete = monthly_df[missing_total < MAX_DAYS_MISSING].copy()
dropped = monthly_df.loc[missing_total >= MAX_DAYS_MISSING, ["year", "month"]].drop_duplicates()
if len(dropped):
    shown = ", ".join(f"{int(r.year)}-{int(r.month):02d}" for r in dropped.tail(6).itertuples())
    print(f"Dropped {len(dropped)} incomplete month(s); most recent: {shown}")


# -- Polygon clip -------------------------------------------------------------
# The bbox is a rectangle; it grabs ocean and neighbour-country cells. Clip the
# distinct pixel grid to the country polygon. Same sjoin(predicate="within")
# the index scripts used, so the pixel set is unchanged.
pixels = complete[["x", "y"]].drop_duplicates().copy()
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
if pixels_with_country.empty:
    raise ValueError(f"polygon clip removed every pixel for {COUNTRY_TAG!r}")
print(f"Polygon clip: {len(pixels_with_country)} of {len(pixels)} bbox pixels inside boundary")

out = complete.merge(pixels_with_country, on=["x", "y"], how="inner")
out = out[[
    "x", "y", "country", "year", "month",
    "monthly_precip", "mean_temp_monthly",
    "n_days_observed", "n_days_missing", "days_in_month",
]].astype({
    "x": float, "y": float, "country": str,
    "year": "int32", "month": "int32",
    "monthly_precip": float, "mean_temp_monthly": float,
    "n_days_observed": "int32", "n_days_missing": "int32", "days_in_month": "int32",
}).sort_values(["x", "y", "year", "month"]).reset_index(drop=True)

span_first = (out.year * 100 + out.month).min()
span_last  = (out.year * 100 + out.month).max()
print(f"Monthly series: {len(out):,} rows, {len(pixels_with_country)} pixels, "
      f"{span_first} to {span_last}")


# -- Save ---------------------------------------------------------------------
# Written from the driver with pandas, as the index scripts do: Volume paths
# accept a single-file pandas write.
os.makedirs(VOLUME_PATH, exist_ok=True)
out.to_parquet(OUT_PATH, index=False)
print(f"Saved: {OUT_PATH}")
