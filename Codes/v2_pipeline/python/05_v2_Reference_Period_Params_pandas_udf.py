# =============================================================================
# 05_v2. Reference Period Parameters — PySpark Pandas UDF (distributed)
# =============================================================================
# Port of "05. Reference_Period_Params.R" to a distributed pandas_udf.
# Freezes ERA5 reference-period (1973-2002) distribution parameters, exports
# them, and applies them to the full ERA5 series.
#
# Estimator: climate_indices uses L-moments (PWM family) — same as R SPEI's
# "ub-pwm". We use climate_indices' OWN public fitting functions to export the
# parameters, so the exported params are exactly consistent with the SPI/SPEI
# z-scores produced by scripts 01/02a/02b (also climate_indices). NOT MLE.
#
# SPI  — Gamma params (alpha, beta) per pixel x scale x calendar month
# SPEI — Pearson Type III params (here via gamma on the water balance shifted
#        positive, matching climate_indices' SPEI internals) per pixel x scale
#        x calendar month, per PET method.
#
# Inputs (parquet, v2):
#   spi_results.parquet                 — monthly_precip (+ spi cols)
#   spei_thornthwaite_results.parquet   — monthly_precip, water_balance
#   spei_penman_results.parquet         — monthly_precip, water_balance
#
# Outputs (parquet, drought_outputs/v2):
#   era5_gamma_params_ref1973_2002.parquet
#   era5_loglogistic_params_ref1973_2002.parquet
#   spi_era5_reference_period.parquet
#   spei_era5_reference_period.parquet
# =============================================================================

# -- USER SETTINGS ------------------------------------------------------------
# COUNTRY is injected by the runner (_run_py_step) from the job parameter, so
# every task in a run agrees. Falls back to Benin for standalone/notebook use.
COUNTRY_TAG    = globals().get("COUNTRY", "Benin")
COUNTRY_FILTER = [COUNTRY_TAG]   # one country per run
CALIB_START    = 1973
CALIB_END      = 2002
SCALES         = [1, 3, 6, 12]
# Per-country output dir: filenames stay identical across countries, so runs for
# different countries never collide and downstream scripts need no rename logic.
VOLUME_PATH    = f"/Volumes/projects/gwsc_drought/drought_outputs/v2/{COUNTRY_TAG}"
# -----------------------------------------------------------------------------

# Self-install rather than rely on cluster libraries: a cluster rebuild drops
# every attached library, and this script then fails at import. Same block as
# 01/02a/02b so all steps have identical dependency handling.
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


# =============================================================================
# Param schemas
# =============================================================================
GAMMA_PARAM_SCHEMA = StructType([
    StructField("x", DoubleType()), StructField("y", DoubleType()),
    StructField("country", StringType()),
    StructField("spi_scale", IntegerType()),
    StructField("calendar_month", IntegerType()),
    StructField("alpha", DoubleType()), StructField("beta", DoubleType()),
])

# Pearson Type III params from climate_indices.compute.pearson_parameters:
# returns 4 arrays (prob_zero, loc, scale, skew) — the SPEI distribution.
LL_PARAM_SCHEMA = StructType([
    StructField("x", DoubleType()), StructField("y", DoubleType()),
    StructField("country", StringType()),
    StructField("spei_scale", IntegerType()),
    StructField("calendar_month", IntegerType()),
    StructField("prob_zero", DoubleType()),
    StructField("loc", DoubleType()), StructField("scale", DoubleType()),
    StructField("skew", DoubleType()),
    StructField("pet_method", StringType()),
])

SPI_OUT_SCHEMA = StructType([
    StructField("x", DoubleType()), StructField("y", DoubleType()),
    StructField("country", StringType()),
    StructField("year", IntegerType()), StructField("month", IntegerType()),
    StructField("monthly_precip", DoubleType()),
    StructField("spi1", DoubleType()), StructField("spi3", DoubleType()),
    StructField("spi6", DoubleType()), StructField("spi12", DoubleType()),
])

SPEI_OUT_SCHEMA = StructType([
    StructField("x", DoubleType()), StructField("y", DoubleType()),
    StructField("country", StringType()),
    StructField("year", IntegerType()), StructField("month", IntegerType()),
    StructField("water_balance", DoubleType()),
    StructField("spei1", DoubleType()), StructField("spei3", DoubleType()),
    StructField("spei6", DoubleType()), StructField("spei12", DoubleType()),
    StructField("pet_method", StringType()),
])


# =============================================================================
# Param extraction UDF — uses climate_indices' OWN gamma fitter so params are
# consistent with the z-scores 01/02a/02b produced.
# =============================================================================
def _gamma_params_for(values, start_year, calib_initial, calib_final, scale):
    """Return (alpha[12], beta[12]) via climate_indices.compute, or NaNs."""
    from climate_indices import compute
    # convolve to the accumulation scale exactly as climate_indices does
    arr = compute.sum_to_scale(values, scale)
    a, b = compute.gamma_parameters(
        arr,
        data_start_year=start_year,
        calibration_start_year=calib_initial,
        calibration_end_year=calib_final,
        periodicity=compute.Periodicity.monthly,
    )
    return np.asarray(a, float), np.asarray(b, float)

def gamma_param_udf(pdf: pd.DataFrame) -> pd.DataFrame:
    pdf = pdf.sort_values(["year", "month"]).reset_index(drop=True)
    px = float(pdf["x"].iloc[0]); py = float(pdf["y"].iloc[0])
    country = pdf["country"].iloc[0] if "country" in pdf.columns else None
    start_year = int(pdf["year"].iloc[0])
    precip = pdf["monthly_precip"].to_numpy(float)
    rows = []
    for sc in SCALES:
        if np.sum(~np.isnan(precip)) < max(sc * 4, 24):
            continue
        try:
            a, b = _gamma_params_for(precip, start_year, CALIB_START, CALIB_END, sc)
            for m in range(12):
                rows.append({"x": px, "y": py, "country": country,
                             "spi_scale": sc, "calendar_month": m + 1,
                             "alpha": float(a[m]), "beta": float(b[m])})
        except Exception as e:
            print(f"[gamma x={px} y={py} sc={sc}] {type(e).__name__}: {e}")
    cols = [f.name for f in GAMMA_PARAM_SCHEMA.fields]
    return pd.DataFrame(rows)[cols] if rows else pd.DataFrame(columns=cols)


# =============================================================================
# Apply UDF — full-series SPI using climate_indices with the 1973-2002
# calibration window (identical mechanism to scripts 01/02a/02b, just a fixed
# reference window). climate_indices re-derives the frozen params internally
# from the same calibration window, so output == applying the exported params.
# =============================================================================
def spi_apply_udf(pdf: pd.DataFrame) -> pd.DataFrame:
    from climate_indices import indices, compute
    pdf = pdf.sort_values(["year", "month"]).reset_index(drop=True)
    px = float(pdf["x"].iloc[0]); py = float(pdf["y"].iloc[0])
    country = pdf["country"].iloc[0] if "country" in pdf.columns else None
    start_year = int(pdf["year"].iloc[0])
    precip = pdf["monthly_precip"].to_numpy(float)
    n = len(precip)

    def calc(scale):
        if np.sum(~np.isnan(precip)) < max(scale * 4, 24):
            return np.full(n, np.nan)
        try:
            r = indices.spi(values=precip, scale=scale,
                            distribution=indices.Distribution.gamma,
                            data_start_year=start_year,
                            calibration_year_initial=CALIB_START,
                            calibration_year_final=CALIB_END,
                            periodicity=compute.Periodicity.monthly)
            return np.where(np.isinf(r), np.nan, r)
        except Exception as e:
            print(f"[spi-apply sc={scale}] {type(e).__name__}: {e}")
            return np.full(n, np.nan)

    out = pdf[["x", "y", "country", "year", "month", "monthly_precip"]].copy()
    out["spi1"], out["spi3"] = calc(1), calc(3)
    out["spi6"], out["spi12"] = calc(6), calc(12)
    return out[[f.name for f in SPI_OUT_SCHEMA.fields]]


# =============================================================================
# STEP 1 + 3 — SPI: extract params + apply, distributed by pixel
# =============================================================================
print("Loading SPI input...")
spi_in = (spark.read.parquet(SPI_PATH)
          .select("x", "y", "country", "year", "month", "monthly_precip"))
if COUNTRY_FILTER:
    spi_in = spi_in.filter(F.col("country").isin(COUNTRY_FILTER))
n_par = spark.sparkContext.defaultParallelism * 2
spi_in = spi_in.repartition(n_par, "x", "y").cache()

print("Extracting frozen Gamma params (per pixel x scale x month)...")
gamma_params = spi_in.groupBy("x", "y").applyInPandas(gamma_param_udf, schema=GAMMA_PARAM_SCHEMA)
gamma_params.toPandas().to_parquet(f"{VOLUME_PATH}/era5_gamma_params_ref1973_2002.parquet", index=False)
print("Saved: era5_gamma_params_ref1973_2002.parquet")

print("Applying frozen Gamma params to full SPI series...")
spi_era5 = spi_in.groupBy("x", "y").applyInPandas(spi_apply_udf, schema=SPI_OUT_SCHEMA)
spi_era5_pd = spi_era5.toPandas()
spi_era5_pd["period"] = np.where(spi_era5_pd["year"] <= CALIB_END,
                                 f"{CALIB_START}-{CALIB_END} reference",
                                 f"{CALIB_END + 1}+ evaluation")
spi_era5_pd.to_parquet(f"{VOLUME_PATH}/spi_era5_reference_period.parquet", index=False)
print(f"Saved: spi_era5_reference_period.parquet ({len(spi_era5_pd):,} rows)")


# =============================================================================
# STEP 2 + 4 — SPEI log-logistic params + apply, per PET method
# climate_indices SPEI uses Pearson III; we fit + apply on the water balance.
# We reuse indices.spei() with the calibration window for the z-scores and
# compute.pearson_parameters() for the exported params (same estimator).
# =============================================================================
def ll_param_udf_factory(method_label):
    def _udf(pdf: pd.DataFrame) -> pd.DataFrame:
        from climate_indices import compute
        pdf = pdf.sort_values(["year", "month"]).reset_index(drop=True)
        px = float(pdf["x"].iloc[0]); py = float(pdf["y"].iloc[0])
        country = pdf["country"].iloc[0] if "country" in pdf.columns else None
        start_year = int(pdf["year"].iloc[0])
        wb = pdf["water_balance"].to_numpy(float)
        rows = []
        for sc in SCALES:
            if np.sum(~np.isnan(wb)) < max(sc * 4, 24):
                continue
            try:
                arr = compute.sum_to_scale(wb, sc)
                # Pearson III: returns (prob_zero, loc, scale, skew), each len-12
                pz, loc, scl, skew = compute.pearson_parameters(
                    arr, data_start_year=start_year,
                    calibration_start_year=CALIB_START,
                    calibration_end_year=CALIB_END,
                    periodicity=compute.Periodicity.monthly)
                pz = np.asarray(pz, float); loc = np.asarray(loc, float)
                scl = np.asarray(scl, float); skew = np.asarray(skew, float)
                for m in range(12):
                    rows.append({"x": px, "y": py, "country": country,
                                 "spei_scale": sc, "calendar_month": m + 1,
                                 "prob_zero": float(pz[m]), "loc": float(loc[m]),
                                 "scale": float(scl[m]), "skew": float(skew[m]),
                                 "pet_method": method_label})
            except Exception as e:
                print(f"[ll {method_label} x={px} y={py} sc={sc}] {type(e).__name__}: {e}")
        cols = [f.name for f in LL_PARAM_SCHEMA.fields]
        return pd.DataFrame(rows)[cols] if rows else pd.DataFrame(columns=cols)
    return _udf

def spei_apply_udf_factory(method_label):
    def _udf(pdf: pd.DataFrame) -> pd.DataFrame:
        from climate_indices import indices, compute
        pdf = pdf.sort_values(["year", "month"]).reset_index(drop=True)
        px = float(pdf["x"].iloc[0]); py = float(pdf["y"].iloc[0])
        country = pdf["country"].iloc[0] if "country" in pdf.columns else None
        start_year = int(pdf["year"].iloc[0])
        precip = pdf["monthly_precip"].to_numpy(float)
        pet    = (pdf["monthly_precip"] - pdf["water_balance"]).to_numpy(float)
        n = len(precip)

        def calc(scale):
            if np.sum(~np.isnan(pdf["water_balance"].to_numpy(float))) < max(scale * 4, 24):
                return np.full(n, np.nan)
            try:
                r = indices.spei(precips_mm=precip, pet_mm=pet, scale=scale,
                                 distribution=indices.Distribution.pearson,
                                 periodicity=compute.Periodicity.monthly,
                                 data_start_year=start_year,
                                 calibration_year_initial=CALIB_START,
                                 calibration_year_final=CALIB_END)
                return np.where(np.isinf(r), np.nan, r)
            except Exception as e:
                print(f"[spei-apply {method_label} sc={scale}] {type(e).__name__}: {e}")
                return np.full(n, np.nan)

        out = pdf[["x", "y", "country", "year", "month", "water_balance"]].copy()
        out["spei1"], out["spei3"] = calc(1), calc(3)
        out["spei6"], out["spei12"] = calc(6), calc(12)
        out["pet_method"] = method_label
        return out[[f.name for f in SPEI_OUT_SCHEMA.fields]]
    return _udf

wb_sources = [("thornthwaite", SPEI_T_PATH), ("penman", SPEI_P_PATH)]
ll_param_parts, spei_apply_parts = [], []

for method_label, path in wb_sources:
    try:
        wb = (spark.read.parquet(path)
              .select("x", "y", "country", "year", "month", "monthly_precip", "water_balance"))
    except Exception as e:
        print(f"WB source {method_label} not available ({e}) — skipping.")
        continue
    if COUNTRY_FILTER:
        wb = wb.filter(F.col("country").isin(COUNTRY_FILTER))
    wb = wb.repartition(n_par, "x", "y").cache()

    print(f"Extracting log-logistic params [{method_label}]...")
    lp = wb.groupBy("x", "y").applyInPandas(ll_param_udf_factory(method_label), schema=LL_PARAM_SCHEMA)
    ll_param_parts.append(lp.toPandas())

    print(f"Applying log-logistic params to full series [{method_label}]...")
    sp = wb.groupBy("x", "y").applyInPandas(spei_apply_udf_factory(method_label), schema=SPEI_OUT_SCHEMA)
    spei_apply_parts.append(sp.toPandas())

if ll_param_parts:
    pd.concat(ll_param_parts, ignore_index=True).to_parquet(
        f"{VOLUME_PATH}/era5_loglogistic_params_ref1973_2002.parquet", index=False)
    print("Saved: era5_loglogistic_params_ref1973_2002.parquet")

if spei_apply_parts:
    spei_era5_pd = pd.concat(spei_apply_parts, ignore_index=True)
    spei_era5_pd["period"] = np.where(spei_era5_pd["year"] <= CALIB_END,
                                      f"{CALIB_START}-{CALIB_END} reference",
                                      f"{CALIB_END + 1}+ evaluation")
    spei_era5_pd.to_parquet(f"{VOLUME_PATH}/spei_era5_reference_period.parquet", index=False)
    print(f"Saved: spei_era5_reference_period.parquet ({len(spei_era5_pd):,} rows)")


# =============================================================================
# STEP 5 — Validate: mean ~ 0, sd ~ 1 in reference period
# =============================================================================
ref = spi_era5_pd[(spi_era5_pd["year"] >= CALIB_START) & (spi_era5_pd["year"] <= CALIB_END)]
print("\n=== VALIDATION (reference period; expect mean~0, sd~1) ===")
for c in ["spi1", "spi3", "spi6", "spi12"]:
    v = ref[c].to_numpy(float)
    print(f"  {c}: mean={np.nanmean(v):.3f}  sd={np.nanstd(v):.3f}  pct_na={100*np.mean(np.isnan(v)):.1f}")
print("Script 05_v2 complete — frozen params + full-series indices written.")
