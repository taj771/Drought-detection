# =============================================================================
# 03_v2. Drought Detection — PySpark Pandas UDF (distributed, multi-node)
# =============================================================================
# Port of "03. Drought_Detection.R" to a distributed pandas_udf architecture.
# Each pixel runs ALL 75 configurations inside one applyInPandas call, so
# detection is distributed across cluster workers (not a driver-only mclapply).
#
# Inputs (parquet, from v2 index scripts 01/02a/02b):
#   spi_results.parquet, spei_thornthwaite_results.parquet,
#   spei_penman_results.parquet  in VOLUME_PATH
#
# Outputs (rds-compatible parquet, read by Scripts 06/07/09):
#   all_drought_events.parquet
#   drought_summary_yearly_ref.parquet
#   drought_annual_summary_ref.parquet
#   pixel_freq_ref.parquet
#
# Algorithm matches the R script exactly:
#   - threshold method: RLE of months below start_thr (>= min_duration)
#   - cumulative method: scale-1 only; event ends when cumsum >= 0
#   - severity_class from peak_index; same event characteristics
# =============================================================================

# -- USER SETTINGS ------------------------------------------------------------
YEAR_FROM        = 1970
YEAR_TO          = 2023
REF_START        = 1973          # reference period (frozen params)
REF_END          = 2002
START_THRESHOLDS = [0.0, -0.25, -0.5, -1.0, -1.5]   # all 5 — do not remove
MIN_DURATION     = 1
# COUNTRY is injected by the runner (_run_py_step) from the job parameter, so
# this always matches the 01/02a/02b output it reads. Falls back to Benin for
# standalone/notebook use.
COUNTRY_TAG      = globals().get("COUNTRY", "Benin")
COUNTRY_FILTER   = [COUNTRY_TAG]   # one country per run; follows the job parameter
# Per-country dir. Note detection_tmp/ now lives under it, so the config-id cache
# can no longer be reused across countries by accident.
VOLUME_PATH      = f"/Volumes/projects/gwsc_drought/drought_outputs/v2/{COUNTRY_TAG}"
# -----------------------------------------------------------------------------

from pyspark.sql import SparkSession
spark = SparkSession.builder.getOrCreate()

import numpy as np
import pandas as pd
from pyspark.sql import functions as F
from pyspark.sql.types import (
    StructType, StructField, DoubleType, IntegerType, StringType
)

print("Spark session active:", spark.version)

import os
SPI_PATH    = f"{VOLUME_PATH}/spi_results.parquet"
SPEI_T_PATH = f"{VOLUME_PATH}/spei_thornthwaite_results.parquet"
SPEI_P_PATH = f"{VOLUME_PATH}/spei_penman_results.parquet"


# -- Step 1: Load + join index data (Spark) -----------------------------------
print("Loading index parquet files...")
spi = spark.read.parquet(SPI_PATH).select(
    "x", "y", "country", "year", "month", "spi1", "spi3", "spi6", "spi12")

spei_t = spark.read.parquet(SPEI_T_PATH).select(
    "x", "y", "year", "month",
    F.col("spei1").alias("spei1"), F.col("spei3").alias("spei3"),
    F.col("spei6").alias("spei6"), F.col("spei12").alias("spei12"))

joined = spi.join(spei_t, on=["x", "y", "year", "month"], how="left")

# Penman SPEI optional — rename to *_pm to avoid clash with Thornthwaite cols.
has_penman = False
try:
    spei_p = spark.read.parquet(SPEI_P_PATH).select(
        "x", "y", "year", "month",
        F.col("spei1").alias("spei1_pm"), F.col("spei3").alias("spei3_pm"),
        F.col("spei6").alias("spei6_pm"), F.col("spei12").alias("spei12_pm"))
    joined = joined.join(spei_p, on=["x", "y", "year", "month"], how="left")
    has_penman = True
    print("Penman-Monteith SPEI joined.")
except Exception as e:
    print(f"Penman SPEI not loaded ({e}) — running SPI + Thornthwaite only.")

# Filters: year + country
joined = joined.filter((F.col("year") >= YEAR_FROM) & (F.col("year") <= YEAR_TO))
if COUNTRY_FILTER:
    joined = joined.filter(F.col("country").isin(COUNTRY_FILTER))

# Repartition by pixel for even UDF distribution
n_par = spark.sparkContext.defaultParallelism * 2
joined = joined.repartition(n_par, "x", "y")
print(f"Index data ready. Partitions: {n_par}")


# -- Step 2: Build configuration table ----------------------------------------
INDICES = ["spi1", "spi3", "spi6", "spi12", "spei1", "spei3", "spei6", "spei12"]
if has_penman:
    INDICES += ["spei1_pm", "spei3_pm", "spei6_pm", "spei12_pm"]

SCALE1 = {"spi1", "spei1", "spei1_pm"}
PREFIX = {"spi1": "S1", "spi3": "S3", "spi6": "S6", "spi12": "S12",
          "spei1": "ET1", "spei3": "ET3", "spei6": "ET6", "spei12": "ET12",
          "spei1_pm": "EP1", "spei3_pm": "EP3", "spei6_pm": "EP6", "spei12_pm": "EP12"}

def build_configs():
    rows = []
    for idx in INDICES:
        methods = ["threshold", "cumulative"] if idx in SCALE1 else ["threshold"]
        for m in methods:
            for thr in START_THRESHOLDS:
                cid = f"{PREFIX[idx]}-{'T' if m == 'threshold' else 'C'}-{round(abs(thr) * 100):03d}"
                rows.append((cid, idx, m, float(thr), MIN_DURATION))
    return rows

CONFIGS = build_configs()
print(f"Total configurations: {len(CONFIGS)}")


# -- Step 3: Detection (runs inside the pandas UDF, per pixel) -----------------
def severity_class(peak):
    if peak >= -1.0:                return "Mild"
    if -1.5 <= peak < -1.0:        return "Moderate"
    if -2.0 <= peak < -1.5:        return "Severe"
    return "Extreme"

def detect_one(iv, dt, method, start_thr, min_duration):
    """Return list of event dicts for one pixel/config. Mirrors R detect_droughts."""
    mask = ~np.isnan(iv)
    iv = iv[mask]; dt = dt[mask]
    n = len(iv)
    if n < min_duration:
        return []
    events = []

    if method == "threshold":
        below = iv < start_thr
        if not below.any():
            return []
        # RLE of `below`
        change = np.flatnonzero(np.diff(below.astype(int))) + 1
        starts = np.concatenate(([0], change))
        ends = np.concatenate((change, [n]))   # exclusive end
        eid = 0
        for s, e in zip(starts, ends):
            if not below[s] or (e - s) < min_duration:
                continue
            eid += 1
            ev = iv[s:e]; ed = dt[s:e]
            cs = np.cumsum(ev)
            events.append(_mk_event(eid, ev, ed, cs))
    else:
        # cumulative — scale-1 only
        eid = 0; i = 0
        while i < n:
            if iv[i] >= start_thr:
                i += 1; continue
            cs = np.cumsum(iv[i:n])
            rec = np.flatnonzero(cs >= 0)
            if rec.size == 0:
                ev = iv[i:n]; ed = dt[i:n]; i = n
            else:
                r = rec[0] + 1                 # 1-based recovery offset
                ev_end = i + r - 2             # last month in event (exclusive recovery)
                if ev_end < i:
                    i += r; continue
                ev = iv[i:ev_end + 1]; ed = dt[i:ev_end + 1]; i += r
            if len(ev) == 0:
                continue
            eid += 1
            events.append(_mk_event(eid, ev, ed, np.cumsum(ev)))
    return events

def _mk_event(eid, ev, ed, cs):
    peak = float(np.min(ev))
    return {
        "drought_id": eid,
        "start_date": pd.Timestamp(ed[0]),
        "end_date": pd.Timestamp(ed[-1]),
        "duration_months": int(len(ed)),
        "entry_index": float(ev[0]),
        "peak_index": peak,
        "peak_date": pd.Timestamp(ed[int(np.argmin(ev))]),
        "cum_deficit_neg": float(ev[ev < 0].sum()),
        "total_severity": float(ev.sum()),
        "min_cum_deficit": float(np.min(cs)),
        "severity_class": severity_class(peak),
    }

EVENT_SCHEMA = StructType([
    StructField("x", DoubleType()), StructField("y", DoubleType()),
    StructField("country", StringType()),
    StructField("config_id", StringType()), StructField("index", StringType()),
    StructField("method", StringType()), StructField("start_thr", DoubleType()),
    StructField("min_duration", IntegerType()),
    StructField("drought_id", IntegerType()),
    StructField("start_date", StringType()), StructField("end_date", StringType()),
    StructField("duration_months", IntegerType()),
    StructField("entry_index", DoubleType()), StructField("peak_index", DoubleType()),
    StructField("peak_date", StringType()),
    StructField("cum_deficit_neg", DoubleType()), StructField("total_severity", DoubleType()),
    StructField("min_cum_deficit", DoubleType()), StructField("severity_class", StringType()),
    StructField("event_year", IntegerType()),
])

def detect_pixel(pdf: pd.DataFrame) -> pd.DataFrame:
    """All 75 configs for one pixel. pdf = one pixel's monthly time series."""
    pdf = pdf.sort_values(["year", "month"]).reset_index(drop=True)
    dt = pd.to_datetime(dict(year=pdf["year"], month=pdf["month"], day=1)).values
    px = float(pdf["x"].iloc[0]); py = float(pdf["y"].iloc[0])
    country = pdf["country"].iloc[0] if "country" in pdf.columns else None
    out = []
    for cid, idx, method, thr, mind in CONFIGS:
        if idx not in pdf.columns:
            continue
        iv = pdf[idx].to_numpy(dtype=float)
        if np.all(np.isnan(iv)):
            continue
        for ev in detect_one(iv, dt, method, thr, mind):
            ev2 = {"x": px, "y": py, "country": country,
                   "config_id": cid, "index": idx, "method": method,
                   "start_thr": thr, "min_duration": mind, **ev}
            ev2["start_date"] = str(ev2["start_date"].date())
            ev2["end_date"]   = str(ev2["end_date"].date())
            ev2["peak_date"]  = str(ev2["peak_date"].date())
            ev2["event_year"] = int(ev2["start_date"][:4])
            out.append(ev2)
    if not out:
        return pd.DataFrame(columns=[f.name for f in EVENT_SCHEMA.fields])
    return pd.DataFrame(out)[[f.name for f in EVENT_SCHEMA.fields]]

print("Running distributed detection (all configs per pixel)...")
events_sdf = joined.groupBy("x", "y").applyInPandas(detect_pixel, schema=EVENT_SCHEMA)
events_sdf = events_sdf.filter(~F.isnan("peak_index") & F.col("country").isNotNull()).cache()
n_events = events_sdf.count()
print(f"Detection complete: {n_events:,} events")


# -- Step 4: Summaries (distributed Spark aggregations) -----------------------
# total pixels per country (from index data, pre-detection)
ppc = (joined.select("x", "y", "country").distinct()
       .groupBy("country").agg(F.count(F.lit(1)).alias("total_pixels_country")))

grp = ["config_id", "index", "method", "start_thr", "min_duration", "country", "event_year"]

# Per-pixel-year then per-config-year (mirrors px_yr -> cy in R)
px_yr = events_sdf.groupBy(*grp, "x", "y").agg(
    F.count(F.lit(1)).alias("pixel_n_events"),
    F.mean("duration_months").alias("pixel_mean_duration"),
    F.mean("peak_index").alias("pixel_mean_peak"),
    F.mean("total_severity").alias("pixel_mean_severity"),
)

cy = px_yr.groupBy(*grp).agg(
    F.count(F.lit(1)).alias("pixels_with_drought"),
    F.sum("pixel_n_events").alias("total_pixel_events"),
    F.round(F.mean("pixel_n_events"), 1).alias("mean_events_per_pixel"),
    F.round(F.mean("pixel_mean_duration"), 1).alias("mean_duration_months"),
    F.round(F.mean("pixel_mean_peak"), 2).alias("mean_peak_index"),
    F.round(F.mean("pixel_mean_severity"), 2).alias("mean_total_severity"),
)

drought_summary_yearly = (cy.join(ppc, on="country", how="left")
    .withColumn("pct_pixels_affected",
                F.round(100 * F.col("pixels_with_drought") / F.col("total_pixels_country"), 1)))

# annual summary — one row per config per country (Scripts 06/07 read THIS)
gcfg = ["config_id", "index", "method", "start_thr", "min_duration", "country"]
drought_annual_summary = (drought_summary_yearly.groupBy(*gcfg).agg(
    F.round(F.sum("total_pixel_events") /
            (F.first("total_pixels_country") * F.countDistinct("event_year")), 3
            ).alias("annual_freq_events"),
    F.round(F.sum("pct_pixels_affected") / F.countDistinct("event_year"), 1).alias("annual_pct_pixels"),
    F.round(F.mean("mean_duration_months"), 1).alias("annual_mean_duration"),
    F.round(F.mean("mean_peak_index"), 2).alias("annual_mean_peak"),
    F.round(F.mean("mean_total_severity"), 2).alias("annual_mean_severity"),
))

# pixel-level ref-period frequency (Script 06 Level 3)
pixel_freq_ref = (px_yr.filter((F.col("event_year") >= REF_START) & (F.col("event_year") <= REF_END))
    .groupBy("config_id", "country", "x", "y")
    .agg(F.sum("pixel_n_events").alias("ref_n_events")))


# -- Step 5: Write outputs (collect to driver, Volume parquet) ----------------
print("Writing outputs...")
os.makedirs(VOLUME_PATH, exist_ok=True)

drought_summary_yearly.toPandas().to_parquet(
    f"{VOLUME_PATH}/drought_summary_yearly_ref.parquet", index=False)
drought_annual_summary.toPandas().to_parquet(
    f"{VOLUME_PATH}/drought_annual_summary_ref.parquet", index=False)
pixel_freq_ref.toPandas().to_parquet(
    f"{VOLUME_PATH}/pixel_freq_ref.parquet", index=False)
events_pd = events_sdf.toPandas()
events_pd.to_parquet(f"{VOLUME_PATH}/all_drought_events.parquet", index=False)

# Per-config files for Script 04 (sanity checks streams one config at a time
# to avoid loading the full event table). Mirrors the R detection_tmp/cfg_*.rds
# layout, as parquet: detection_tmp/cfg_<config_id>.parquet.
tmp_dir = f"{VOLUME_PATH}/detection_tmp"
os.makedirs(tmp_dir, exist_ok=True)
for cid, sub in events_pd.groupby("config_id"):
    sub.to_parquet(f"{tmp_dir}/cfg_{cid}.parquet", index=False)
print(f"Wrote {events_pd['config_id'].nunique()} per-config files to detection_tmp/")

print(f"Done. {n_events:,} events, {len(CONFIGS)} configs, countries={COUNTRY_FILTER}")
print("Outputs: all_drought_events / drought_summary_yearly_ref / "
      "drought_annual_summary_ref / pixel_freq_ref (.parquet) + detection_tmp/cfg_*.parquet")
