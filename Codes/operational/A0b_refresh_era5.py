"""
A0b — refresh observed ERA5 monthly precipitation for the operational arm.

Runs ON Databricks. The source table is global daily ERA5
back to 1950 and is far too large to pull down; this script aggregates to monthly
and clips to one country in-cluster, then writes a small parquet (154 pixels for
Benin) that every later step reads locally.

Why this exists. A2 splices observed months in front of the forecast so that SPI-3,
SPI-6 and SPI-12 have full accumulation windows. Those months must be observation,
not an older forecast, and they must run right up to the forecast issue date. The
research arm's local export stops at December 2023, which is fine for a frozen study
but useless here: a September 2026 forecast spliced onto 2023 history is a silent
error, not a loud one. So the operational arm refreshes its own copy on each run.

The aggregation deliberately mirrors 01_v2_SPI_pandas_udf.py -- same table, same
longitude wrap, same bbox-before-collect, same n_days_missing guard, same polygon
clip via gpd.sjoin(predicate="within"). Matching matters more than elegance: the
frozen Gamma parameters were fitted on pixels produced by that script, and a pixel
set that differs even slightly would be scored against parameters belonging to
somewhere else. The polygon clip is the part most easily dropped by accident and
the most costly to drop -- without it a bbox alone returns roughly 300 pixels for
Benin instead of 154, the extra ones being ocean and neighbouring countries.

What this script does NOT do: compute SPI. It only refreshes observed rainfall.
Scoring happens in A3 against the frozen 1991-2020 parameters.

One row per pixel per day is assumed by the monthly sum and enforced rather than
trusted. The source has carried repeated daily rows, and because duplicates are
summed, the effect was roughly doubled rainfall -- read downstream as an absence
of drought. See monthly_precipitation and the calendar-length guard in main.

Usage, from the repository root:

    databricks jobs submit --json '{
      "run_name": "A0b_refresh_era5",
      "tasks": [{"task_key": "refresh",
                 "existing_cluster_id": "<cluster-id>",
                 "notebook_task": {"notebook_path": "<workspace-path>",
                                   "base_parameters": {"COUNTRY": "Benin"}}}]}'

then copy the output parquet down with `databricks fs cp`.
"""

# -- Configuration ------------------------------------------------------------
# COUNTRY is injected by the job parameter so every task in a run agrees.
COUNTRY_TAG = globals().get("COUNTRY", "Benin")

# Start year matches the research arm's export so the two series are comparable.
# There is no end year: the point of this script is to reach the present, and
# hardcoding a bound is exactly the staleness it exists to prevent.
ERA5_YEAR_START = 1950

# A month is kept only if nearly every day reported. The trailing month is always
# partial -- ERA5 runs about five days behind real time -- and summing eleven days
# of September as a monthly total would read as a drought that did not happen.
MAX_DAYS_MISSING = 5
MIN_DAYS_PRESENT = 28

# A month cannot have more days than the calendar gives it. The two guards above
# are both lower bounds, so before this check a month carrying duplicate daily
# rows passed as MORE complete than required -- and the duplicates were summed,
# roughly doubling rainfall. That is a wet bias, which suppresses drought, which
# is the direction that costs most. Observed 2025-01 to 2026-05 with up to 62
# days in a month.
MAX_DAYS_OVER_CALENDAR = 0

OUT_DIR = f"/Volumes/projects/gwsc_drought/drought_outputs/operational/{COUNTRY_TAG}"
OUT_PATH = f"{OUT_DIR}/era5_monthly_observed.parquet"

ERA5_TABLE = "`era5-daily-data`.silver_staging.era5_daily_nn"
BOUNDARY_TABLE = (
    "`era5-daily-data`.silver_staging.esri_worldcountryboundaries_global_silver"
)

# -- Dependencies -------------------------------------------------------------
# Self-install rather than rely on cluster libraries: a cluster rebuild drops every
# attached library and this script would then fail at import. Same handling as the
# v2 scripts.
import subprocess
import sys


def pip_install(package):
    subprocess.check_call([sys.executable, "-m", "pip", "install", package, "-q"])


try:
    import geopandas  # noqa: F401
except ImportError:
    pip_install("geopandas")

import geopandas as gpd
from pyspark.sql import SparkSession
from pyspark.sql import functions as F
from shapely import wkb

spark = SparkSession.builder.getOrCreate()


def country_boundary(country):
    """Country polygon as a GeoDataFrame, and its bounding box.

    Small enough to collect to the driver -- one country, a handful of features.
    """
    raw = (
        spark.table(BOUNDARY_TABLE)
        .select("COUNTRY", "chip_wkb")
        .filter(F.col("COUNTRY") == country)
        .toPandas()
    )
    if raw.empty:
        raise ValueError(f"no boundary found for {country!r} in {BOUNDARY_TABLE}")

    raw["geometry"] = raw["chip_wkb"].apply(lambda blob: wkb.loads(bytes(blob)))
    gdf = gpd.GeoDataFrame(raw, geometry="geometry", crs=4326)
    gdf = gdf[gdf.geometry.is_valid]
    return gdf, gdf.total_bounds


def monthly_precipitation(bbox, year_start):
    """Daily ERA5 aggregated to monthly totals inside the bounding box.

    Everything here stays in Spark. The bbox filter runs before any collect, which
    is what keeps a global table tractable; collecting first would pull far more
    than the driver can hold.
    """
    daily = spark.table(ERA5_TABLE)

    return (
        daily
        # One row per pixel per day, enforced rather than assumed. The source
        # table carries repeated daily rows, and F.sum below adds each one, so a
        # duplicated day is counted twice as rainfall. Dropping here rather than
        # after the groupBy is what makes the sum correct, not merely the day
        # count.
        #
        # Deliberately a whole-row dedupe, not dropDuplicates on
        # (longitude, latitude, time). Keying on the three columns would always
        # leave exactly one row per day and so would always satisfy the
        # calendar-length guard below -- including in the case that guard exists
        # to catch, where the same day appears twice with DIFFERENT rainfall.
        # There the keyed form silently keeps an arbitrary one. Whole-row dedupe
        # collapses exact repeats, which is what the observed 2025-01 to 2026-05
        # contamination is, and leaves genuine conflicts in place to fail loudly.
        .dropDuplicates()
        # The table stores longitude on 0-360; the boundaries and the NMME grid
        # both use -180 to 180.
        .withColumn(
            "lon_180",
            F.when(F.col("longitude") > 180, F.col("longitude") - 360).otherwise(
                F.col("longitude")
            ),
        )
        .withColumn("year", F.year("time"))
        .withColumn("month", F.month("time"))
        .filter(F.col("year") >= year_start)
        .filter(
            (F.col("lon_180") >= float(bbox[0]))
            & (F.col("lon_180") <= float(bbox[2]))
            & (F.col("latitude") >= float(bbox[1]))
            & (F.col("latitude") <= float(bbox[3]))
        )
        .groupBy("lon_180", "latitude", "year", "month")
        .agg(
            F.sum("sum_tp_mm").alias("monthly_precip"),
            F.avg("mean_t2m_c").alias("mean_temp_monthly"),
            F.count("time").alias("n_days_observed"),
            F.sum(F.col("sum_tp_mm").isNull().cast("int")).alias("n_days_missing"),
        )
        .withColumnRenamed("lon_180", "x")
        .withColumnRenamed("latitude", "y")
    )


def clip_to_country(monthly, boundaries, country):
    """Keep only pixels whose centre falls inside the country polygon.

    The bbox is a rectangle and grabs ocean and neighbouring-country cells that
    would otherwise be labelled as this country. The distinct pixel grid is a few
    hundred points, so the spatial join runs on the driver and the result is
    broadcast back as a filter.
    """
    pixels = monthly.select("x", "y").distinct().toPandas()
    pixels_gdf = gpd.GeoDataFrame(
        pixels, geometry=gpd.points_from_xy(pixels.x, pixels.y), crs=4326
    )
    joined = gpd.sjoin(
        pixels_gdf,
        boundaries[["COUNTRY", "geometry"]],
        how="inner",
        predicate="within",
    )
    inside = joined[["x", "y"]].drop_duplicates()
    if inside.empty:
        raise ValueError(f"polygon clip removed every pixel for {country!r}")

    keep = spark.createDataFrame(inside)
    return monthly.join(F.broadcast(keep), on=["x", "y"], how="inner").withColumn(
        "country", F.lit(country)
    )


def main():
    boundaries, bbox = country_boundary(COUNTRY_TAG)
    print(f"{COUNTRY_TAG}: {len(boundaries)} boundary features")
    print(f"bbox: lon [{bbox[0]:.2f}, {bbox[2]:.2f}], lat [{bbox[1]:.2f}, {bbox[3]:.2f}]")

    monthly = monthly_precipitation(bbox, ERA5_YEAR_START)
    clipped = clip_to_country(monthly, boundaries, COUNTRY_TAG)

    # Drop months that did not fully report. Both guards are needed: a month can
    # have few explicit nulls but simply stop part way, which is what the trailing
    # month always looks like.
    # Days the calendar actually offers, so the count has an upper bound as well
    # as a lower one. make_date takes the integer columns directly rather than
    # going through a formatted string: a string built from ints and re-parsed
    # yields null on any format mismatch, and a null here would compare false
    # and quietly readmit every month this guard exists to reject. Leap years
    # come out right because the calendar, not arithmetic, decides.
    clipped = clipped.withColumn(
        "days_in_month",
        F.dayofmonth(F.last_day(F.make_date(F.col("year"), F.col("month"), F.lit(1)))),
    )

    # A null days_in_month would make the upper-bound comparison false and drop
    # the row rather than admit it, but it would do so as an apparent data
    # problem rather than a code one. Check it directly instead.
    n_no_calendar = clipped.filter(F.col("days_in_month").isNull()).count()
    if n_no_calendar:
        raise ValueError(
            f"days_in_month is null for {n_no_calendar} row(s); the calendar-length "
            f"guard cannot be evaluated"
        )

    complete = clipped.filter(
        (F.col("n_days_missing") < MAX_DAYS_MISSING)
        & (F.col("n_days_observed") >= MIN_DAYS_PRESENT)
        & (F.col("n_days_observed") <= F.col("days_in_month") + MAX_DAYS_OVER_CALENDAR)
    )

    dropped = (
        clipped.join(complete, on=["x", "y", "year", "month"], how="left_anti")
        .select("year", "month", "n_days_observed", "days_in_month")
        .distinct()
        .orderBy("year", "month")
        .collect()
    )

    columns = [
        "x", "y", "country", "year", "month",
        "monthly_precip", "mean_temp_monthly", "n_days_observed", "n_days_missing",
    ]
    out = complete.select(*columns)

    n_pixels = out.select("x", "y").distinct().count()
    span = out.agg(
        F.min(F.col("year") * 100 + F.col("month")).alias("first"),
        F.max(F.col("year") * 100 + F.col("month")).alias("last"),
    ).collect()[0]

    print(f"pixels after polygon clip: {n_pixels}")
    print(f"complete months: {span.first} to {span.last}")
    if dropped:
        shown = ", ".join(
            f"{r.year}-{r.month:02d} ({r.n_days_observed}d"
            f"{'>cal' if r.n_days_observed > r.days_in_month else ''})"
            for r in dropped[-6:]
        )
        print(f"dropped {len(dropped)} incomplete month(s); most recent: {shown}")

        # A month over the calendar length survived deduplication, so the source
        # repeats something finer than (pixel, day) and the sum is still wrong.
        # Refuse rather than write a quietly wet record.
        impossible = [r for r in dropped if r.n_days_observed > r.days_in_month]
        if impossible:
            worst = max(impossible, key=lambda r: r.n_days_observed)
            raise ValueError(
                f"{len(impossible)} month(s) exceed their calendar length after "
                f"whole-row deduplication -- worst {worst.year}-{worst.month:02d} "
                f"with {worst.n_days_observed} days of {worst.days_in_month}. "
                f"Exact repeats have already been removed, so the remaining "
                f"duplicates differ in their values: the same day carries two "
                f"different rainfall totals and there is no basis here for "
                f"choosing between them. Duplicate rainfall is summed, so the "
                f"record would be biased wet and drought under-reported. "
                f"Investigate the source table before rerunning."
            )

    out.coalesce(1).write.mode("overwrite").parquet(OUT_PATH)
    print(f"wrote {OUT_PATH}")

    try:
        import json

        dbutils.notebook.exit(  # noqa: F821  (provided by Databricks)
            json.dumps(
                {
                    "country": COUNTRY_TAG,
                    "pixels": n_pixels,
                    "first": int(span.first),
                    "last": int(span.last),
                    "dropped_months": len(dropped),
                    "path": OUT_PATH,
                }
            )
        )
    except NameError:
        pass


if __name__ == "__main__":
    main()
