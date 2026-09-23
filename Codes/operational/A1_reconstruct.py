"""
A1 — NMME forecast anomalies to absolute monthly precipitation on the ERA5 grid.

Stage A of the operational arm produces SPI from a seasonal forecast. The NMME
archive stores anomalies, not absolute rainfall, and it stores them on a 1 degree
grid while the frozen ERA5 Gamma parameters are fitted per 0.25 degree ERA5 pixel.
This script closes both gaps by spatial disaggregation, following the BCSD family
(Wood et al. 2004; Voisin et al. 2010):

    coarse_abs(cell, month) = max(0, clim_coarse(cell, month) + anom_NMME(cell, month))
    ratio(pixel, month)     = clim_fine(pixel, month) / clim_coarse(cell, month)
    fine_abs(pixel, month)  = coarse_abs(cell, month) * ratio(pixel, month)

Rainfall is reconstructed at the forecast's own native 1 degree resolution first,
then split down to 0.25 degree pixels in proportion to each pixel's share of its
cell's normal. An earlier version interpolated the anomaly to 0.25 degrees and added
the fine climatology; that is not the standard route and is not used here. The
difference matters most on dry months, where a bilinear anomaly can drive an
individual pixel negative while the cell total stays positive.

Four decisions are deliberate and worth stating.

Baseline. The reconstruction climatology is ERA5 over CLIM_Y0-CLIM_Y1 (1991-2020),
chosen to match the baseline NMME defines its anomalies against. The OU archive
documents this three ways in products_outputs.md: the climatology build step, the
storage path .../climatology/monthly/1991-2020/, and the file naming convention
{Model}.{var}_{lev}.clim.1991-2020.nc. Using a different window here would mean
adding an anomaly to a normal it was never measured against, which is exactly the
self-consistent-baseline error the downscaling literature warns about (Thober 2015).

The frozen Gamma parameters are now fitted over this same 1991-2020 window, so the
two coincide. They remain separate decisions serving different purposes -- this one
reconstructs absolute rainfall and must match NMME's baseline, the other scores that
rainfall and could in principle sit anywhere ERA5 covers. The Gamma window was
1973-2002 until recently, which is why this note exists: the fact that both now read
1991-2020 is agreement, not a single setting, and changing one does not change the
other.

NMME's own model climatology is deliberately not added back. Subtracting a model's
hindcast mean IS the mean-bias correction; adding it again would return the raw
model forecast and undo the correction. Adding the observed climatology instead
keeps the forecast signal and discards the model's own level. This corrects the
mean only -- variance biases in NMME pass through uncorrected, which would need
quantile mapping against per-member hindcasts that the archive does not publish.

The model climatology is also lead-dependent, (month, lead, lat, lon), while ERA5
has no lead dimension and the climatology here is a single clim(x, y, month). Model
drift with lead already cancels inside the anomaly, so collapsing that dimension is
sound; it is noted because the two arrays are not shaped alike.

Ensemble composition. Only models finite at every lead are kept, and MME is
recomputed as their mean rather than read from the file. The archive's own MME
averages whatever models are present at each lead, so its composition changes part
way along the series -- for the September 2026 issue it is 7 models to lead 9, 6 at
lead 10, then 5. A series whose source changes mid-stream is not something SPI
should be asked to interpret, so a constant ensemble is used throughout. The cost
is real: dropping the short-coverage models changes the early leads by up to
6.2 mm/day, though with no systematic wet or dry shift.

Resolution. Splitting to 0.25 degrees does not create information. A fully interior
NMME cell covers 16 ERA5 pixels, but the country polygon clips most of them: for
Benin 18 NMME cells are touched, holding 1 to 16 ERA5 pixels each and averaging 8.6.
Every pixel in a cell lands at the same percentage of its own normal, so the 154
output values carry 18 independent forecast numbers. Read "percent of country in
drought" with that in mind. The spatial detail in the final SPI comes from the ERA5
climatology underneath, not from the forecast.

Cell averaging is a plain unweighted mean of the ERA5 pixels in each cell rather
than a cos-latitude area weighting. Across one degree at Benin's latitude the
difference is well under a percent.

Run locally. The arrays are small and no cluster is needed.
"""

from pathlib import Path

import numpy as np
import pandas as pd
import xarray as xr

# -- Configuration -----------------------------------------------------------

COUNTRY = "Benin"

# Reference period for the reconstruction climatology. Must match the baseline NMME
# defines its anomalies against, NOT the window the frozen Gamma parameters use.
CLIM_Y0, CLIM_Y1 = 1991, 2020

# ERA5 monthly precipitation, carrying the 0.25 degree grid and monthly_precip in mm.
# The operational arm's own refresh, written by A0b_refresh_era5.py -- the same file
# every other step reads. This used to point at the research arm's frozen export,
# Codes/_archive/_data/spi_results.parquet, which agreed exactly over 1991-2020 and
# so produced identical output. It agreed by luck: that archive stops at 2023, and
# nothing checked that it covered the climatology window or carried the same pixels.
# Move the window past 2023, or run a country the archive never held, and the
# reconstruction would have been built on a climatology that silently went short.
# Assertions below now enforce what was previously coincidence.
ERA5_PARQUET = Path("Codes/operational/data/era5_monthly_observed.parquet")

# NMME monthly ensemble-mean anomaly forecast, downloaded by A0_download_nmme.py.
# The newest forecast directory is used unless NMME_NC is set to a specific file.
NMME_RAW_ROOT = Path("Codes/operational/data/nmme_raw")
NMME_NC = None  # set to a Path to pin a specific forecast file

OUT_PARQUET = Path("Codes/operational/out/nmme_absolute_era5grid.parquet")

# The archive's own multi-model mean. Read to report the difference, not used.
ARCHIVE_MME = "MME"

# Name given to the mean recomputed over the constant-composition ensemble.
MME_NAME = "MME"


def era5_climatology(parquet_path, country, y0, y1):
    """Fine and coarse monthly climatologies from the same ERA5 record.

    Returns (clim_fine, clim_coarse):

        clim_fine   (x, y, cx, cy, month) -> clim_fine    per 0.25 degree ERA5 pixel
        clim_coarse (cx, cy, month)       -> clim_coarse  per 1 degree NMME cell

    NMME cell centres sit on whole degrees, so cell boundaries fall on the halves and
    np.round assigns each ERA5 pixel to the cell whose centre it is nearest. Pixels
    are never split between cells, which keeps the later ratio exactly mass conserving.
    """
    if not parquet_path.exists():
        raise FileNotFoundError(
            f"{parquet_path} not found. Run A0b_refresh_era5.py and copy the output down."
        )

    era5 = pd.read_parquet(
        parquet_path, columns=["x", "y", "country", "year", "month", "monthly_precip"]
    )

    # Check coverage against the whole record before narrowing to the window, so the
    # error can say what is actually there rather than that the result was empty.
    available = era5[era5.country == country]
    if available.empty:
        raise ValueError(f"no ERA5 rows for {country!r} in {parquet_path}")
    if available.year.min() > y0 or available.year.max() < y1:
        raise ValueError(
            f"ERA5 record for {country} spans {int(available.year.min())}-"
            f"{int(available.year.max())} and does not cover the climatology window "
            f"{y0}-{y1}. A short window would silently rebase the reconstruction."
        )

    era5 = available[available.year.between(y0, y1)]

    # Every (pixel, calendar month) needs the full window behind it. A normal
    # averaged over five years is not the same quantity as one averaged over thirty,
    # and mixing the two across pixels would put a spatial pattern into the
    # reconstruction that came from data availability, not climate.
    expected = y1 - y0 + 1
    per_slot = era5.groupby(["x", "y", "month"]).size()
    short = per_slot[per_slot < expected]
    if not short.empty:
        worst = int(short.min())
        raise ValueError(
            f"{len(short)} (pixel, month) climatology slots have fewer than {expected} "
            f"years (worst {worst}). The reconstruction climatology would not be "
            f"comparable across pixels."
        )

    era5 = era5.assign(
        cx=np.round(era5.x).astype(int),
        cy=np.round(era5.y).astype(int),
    )

    clim_fine = (
        era5.groupby(["x", "y", "cx", "cy", "month"], as_index=False)["monthly_precip"]
        .mean()
        .rename(columns={"monthly_precip": "clim_fine"})
    )
    clim_coarse = (
        era5.groupby(["cx", "cy", "month"], as_index=False)["monthly_precip"]
        .mean()
        .rename(columns={"monthly_precip": "clim_coarse"})
    )
    return clim_fine, clim_coarse


def find_latest_forecast(raw_root):
    """Newest downloaded precipitation forecast, and its YYYYMM issue id."""
    candidates = sorted(raw_root.glob("*/**/*anom.monthly.prec_sfc*.nc"))
    if not candidates:
        raise FileNotFoundError(
            f"no NMME precipitation file under {raw_root}. Run A0_download_nmme.py first."
        )
    latest = candidates[-1]
    # .../nmme_raw/<YYYYMM>/monthly/<file>.nc
    issue = latest.relative_to(raw_root).parts[0]
    return latest, issue


def select_models(ds):
    """Models finite at every lead, and the archive MME's composition per lead.

    Models are discovered rather than hardcoded. The roster is reshuffled between
    forecast issues -- June 2024 carried CanCM4i-IC3, GEM5-NEMO and GFDL-SPEAR,
    September 2026 carries CanESM5, GEM5.2-NEMO, COLA-RSMAS-CESM1 and NOAA-SFS
    instead -- so any fixed list would fail on some issue.

    Selecting on full lead coverage keeps the ensemble constant down the whole
    series. The per-lead count is returned so the drop can be reported.
    """
    members = [name for name in ds.data_vars if name != ARCHIVE_MME]
    if not members:
        raise KeyError(f"no model variables found; data_vars = {list(ds.data_vars)}")

    full, partial = [], {}
    for name in members:
        finite_per_lead = np.isfinite(ds[name].values).any(axis=(1, 2))
        n_leads = int(finite_per_lead.sum())
        if n_leads == ds.sizes["time"]:
            full.append(name)
        else:
            partial[name] = n_leads

    if not full:
        raise ValueError("no model covers every lead; cannot form a constant ensemble")

    return full, partial


def nmme_anomalies(nc_path, issue):
    """NMME anomalies at native 1 degree resolution, in mm per month.

    No interpolation happens here. The anomaly stays on its own grid until it has
    been turned into absolute rainfall, which is the whole point of the method.

    The time coordinate is unreliable and is not used. It holds days since the issue
    date in nominal 30-day steps with no units attribute, and its trailing entries
    are NaN -- for the September 2026 issue, three of twelve. That NaN pattern
    reflects the shortest model in the file, not the extent of the data: MME and five
    of the seven models are finite at all twelve leads. Filtering on the coordinate
    would therefore discard three months of valid forecast. Lead is taken from array
    position instead, and the calendar date derived from the issue id.
    """
    ds = xr.open_dataset(nc_path, decode_times=False)

    models, partial = select_models(ds)

    units = ds[models[0]].attrs.get("units", "")
    # The archive has used both spellings across issues.
    if units.replace("-1", "").replace("/", "").lower() not in ("mmday",):
        raise ValueError(f"unexpected units {units!r} in {nc_path.name}; expected mm/day")

    issue_date = pd.Timestamp(int(issue[:4]), int(issue[4:]), 1)
    n_leads = ds.sizes["time"]

    # Recompute the multi-model mean over the constant ensemble. The archive's own
    # MME averages a different set of models at different leads, so it is not used.
    ensemble = ds[models].to_array("model")
    recomputed = ensemble.mean("model")

    archive_diff = None
    if ARCHIVE_MME in ds.data_vars:
        delta = (recomputed - ds[ARCHIVE_MME]).values
        archive_diff = float(np.nanmax(np.abs(delta)))

    # Replace the unreliable time coordinate with the lead index before converting
    # to a frame, so lead survives the reshape without depending on row order.
    lead_index = np.arange(n_leads)

    frames = []
    for name in models + [MME_NAME]:
        array = recomputed if name == MME_NAME else ds[name]
        array = array.assign_coords(time=lead_index).rename(time="lead")
        df = array.to_dataframe(name="anom_mmday").reset_index()
        df = df.rename(columns={"lon": "x", "lat": "y"})
        df["model"] = name
        frames.append(df[["x", "y", "lead", "model", "anom_mmday"]])

    nmme = pd.concat(frames, ignore_index=True).dropna(subset=["anom_mmday"])

    nmme["forecast_date"] = [
        issue_date + pd.DateOffset(months=int(lead)) for lead in nmme["lead"]
    ]
    nmme["year"] = nmme.forecast_date.dt.year
    nmme["month"] = nmme.forecast_date.dt.month

    # The archive stores a rate. SPI needs a monthly total, so scale by the real
    # length of each forecast month rather than a nominal 30 days.
    nmme["anom_mm"] = nmme.anom_mmday * nmme.forecast_date.dt.days_in_month

    # Snap the 1 degree centres to the integer cell index the climatology uses.
    nmme["cx"] = np.round(nmme.x).astype(int)
    nmme["cy"] = np.round(nmme.y).astype(int)

    cols = ["cx", "cy", "model", "lead", "forecast_date", "year", "month", "anom_mm"]
    return nmme[cols], models, partial, n_leads, archive_diff


def reconstruct(nmme, clim_fine, clim_coarse):
    """Rebuild absolute rainfall at 1 degree, then split it to the ERA5 grid.

    Two steps, in this order and not the reverse. The cell total is formed first so
    that the clamp at zero applies to a quantity the forecast actually predicts;
    splitting afterwards keeps every pixel at the same fraction of its own normal
    and conserves the cell mean exactly.
    """
    coarse = nmme.merge(clim_coarse, on=["cx", "cy", "month"], how="inner")
    if coarse.empty:
        raise ValueError("no overlap between the NMME cells and the ERA5 climatology cells")

    coarse["coarse_abs"] = coarse.clim_coarse + coarse.anom_mm

    # A large negative anomaly on a dry month can drive the sum below zero, which is
    # not physical. Clamping at zero is the standard treatment and matters for the
    # Gamma fit, which is undefined for negative values. Clamping here rather than
    # per pixel means one decision per cell instead of sixteen inconsistent ones.
    n_clamped = int((coarse.coarse_abs < 0).sum())
    coarse["coarse_abs"] = coarse.coarse_abs.clip(lower=0)

    out = coarse.merge(clim_fine, on=["cx", "cy", "month"], how="inner")

    # Where a cell's normal is zero the ratio is undefined. That only happens on
    # months with no rain at all in the baseline, where the forecast has nothing to
    # scale; zero out rather than propagate an infinity.
    out["ratio"] = np.where(
        out.clim_coarse > 0, out.clim_fine / out.clim_coarse.replace(0, np.nan), 0.0
    )
    out["precip_mm"] = out.coarse_abs * out.ratio

    return out, n_clamped


def main():
    clim_fine, clim_coarse = era5_climatology(ERA5_PARQUET, COUNTRY, CLIM_Y0, CLIM_Y1)
    n_pixels = clim_fine[["x", "y"]].drop_duplicates().shape[0]
    n_cells = clim_coarse[["cx", "cy"]].drop_duplicates().shape[0]
    per_cell = clim_fine[["x", "y", "cx", "cy"]].drop_duplicates().groupby(["cx", "cy"]).size()
    print(f"ERA5 climatology {CLIM_Y0}-{CLIM_Y1}: {n_pixels} pixels in {n_cells} NMME cells")
    print(f"  pixels per cell: min {per_cell.min()}, max {per_cell.max()}, mean {per_cell.mean():.1f}")

    # The coarse climatology must be the mean of the fine pixels inside each cell,
    # or the ratio split would not conserve mass. Cheap to check, so check it.
    check = (
        clim_fine.groupby(["cx", "cy", "month"], as_index=False).clim_fine.mean()
        .merge(clim_coarse, on=["cx", "cy", "month"])
    )
    residual = (check.clim_fine - check.clim_coarse).abs().max()
    print(f"  mass check: max |mean(fine) - coarse| = {residual:.2e} mm")

    nc_path, issue = (
        (NMME_NC, NMME_NC.parent.parent.name) if NMME_NC else find_latest_forecast(NMME_RAW_ROOT)
    )
    print(f"\nforecast issue {issue}: {nc_path.name}")

    nmme, models, partial, n_leads, archive_diff = nmme_anomalies(nc_path, issue)
    print(f"ensemble ({len(models)} models, all {n_leads} leads): {', '.join(models)}")
    if partial:
        dropped = ", ".join(f"{name} ({n}/{n_leads} leads)" for name, n in partial.items())
        print(f"dropped for partial lead coverage: {dropped}")
    if archive_diff is not None:
        print(f"recomputed MME vs archive MME: max abs diff {archive_diff:.3f} mm/day")
    print(f"NMME anomalies: {len(nmme):,} rows at native 1 degree")

    out, n_clamped = reconstruct(nmme, clim_fine, clim_coarse)
    n_coarse = len(nmme.merge(clim_coarse, on=["cx", "cy", "month"], how="inner"))
    pct = 100 * n_clamped / n_coarse if n_coarse else 0
    print(f"reconstructed: {len(out):,} rows, {n_clamped:,} cell-months clamped at zero ({pct:.1f}%)")

    mme = out[out.model == MME_NAME]
    print(f"\n{MME_NAME} precip_mm by lead:")
    print(mme.groupby("lead").precip_mm.agg(["min", "mean", "max"]).round(1))

    out["issue"] = issue

    OUT_PARQUET.parent.mkdir(parents=True, exist_ok=True)
    cols = [
        "x", "y", "cx", "cy", "model", "issue", "lead", "forecast_date", "year", "month",
        "precip_mm", "clim_fine", "clim_coarse", "coarse_abs", "ratio", "anom_mm",
    ]
    out[cols].to_parquet(OUT_PARQUET, index=False)
    print(f"\nwrote {OUT_PARQUET}")


if __name__ == "__main__":
    main()
