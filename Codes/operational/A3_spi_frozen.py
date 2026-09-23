"""
A3 — SPI for the spliced series, scored against frozen ERA5 parameters.

A2 hands over one precipitation series per (pixel, model): observed months up to
the forecast issue date, then reconstructed NMME months after it. This script
turns that into SPI at each accumulation scale.

The parameters are not fitted here and never will be. They were fitted once on
ERA5 over the reference window by reference/build_gamma_params.py; A3 reads them
and applies them. That is the whole point of the operational design: a forecast
series is 48 months long and contains no calibration history of its own, so the
usual indices.spi(calibration_year_initial=...) call is not available. More
importantly, refitting per run would mean each forecast was scored against its
own recent climate, and "drought" would drift with the baseline instead of being
measured against a fixed one.

The window is currently 1991-2020. It was 1973-2002, inherited from the research
arm. The operational arm now fits its own, which means operational and research
event counts are no longer directly comparable: the same rainfall scores drier
against the wetter recent decades, so events get more frequent without the
weather having changed. That is a property of the yardstick, not the climate.

Three parameters, not two. SPI is fitted to a mixed distribution -- a point mass
at zero rainfall plus a Gamma for the positive part -- and freezing alpha and beta
while letting p0 float does not freeze it. See spi_from_params for the measurement
that showed this and what it cost.

Accumulation across the splice. SPI-3 for September 2026 sums July, August and
September: two observed months and one forecast month. That is intended -- it is
what makes the index meaningful at short lead -- but it means an SPI value is not
purely a forecast. The n_forecast_* columns from A2 carry through unchanged so
that anything downstream can tell the difference.

Warm-up NaN. The first scale-1 months of each series have no complete accumulation
window and come back as NaN. A2 carries 36 months of observed history ahead of the
forecast so this warm-up is spent on history rather than on the months of interest.
"""

from pathlib import Path

import numpy as np
import pandas as pd

# -- Configuration -----------------------------------------------------------

SPLICED_PARQUET = Path("Codes/operational/out/spliced_series.parquet")

# All three frozen parameters -- alpha, beta and p0 -- in one file, fitted over one
# window by reference/build_gamma_params.py. They used to come from two places (the
# research arm's script 05 for the Gamma, a separate script for p0); that worked
# only because both happened to use 1973-2002, and nothing enforced it. See
# spi_from_params for why alpha and beta alone are not enough.
PARAMS_PARQUET = Path("Codes/operational/data/era5_spi_params_ref1991_2020.parquet")
REF_LABEL = "1991-2020"

OUT_PARQUET = Path("Codes/operational/out/spi_forecast.parquet")

SCALES = [1, 3, 6, 12]

# The bounds indices.spi() clips to, mirroring the library's
# _FITTED_INDEX_VALID_MIN / _FITTED_INDEX_VALID_MAX. transform_fitted_gamma does
# not apply them itself; without this the two disagree by several SPI units in the
# tails. 3.09 is norm.ppf(0.999).
FITTED_INDEX_VALID_MIN = -3.09
FITTED_INDEX_VALID_MAX = 3.09


def accumulate(values, scale):
    """Rolling sum over `scale` months, matching climate_indices.compute.sum_to_scale.

    The first scale-1 positions have no full window and come back as NaN.
    """
    if scale == 1:
        return values.astype(float)
    out = np.full(len(values), np.nan)
    csum = np.cumsum(np.insert(values.astype(float), 0, 0.0))
    out[scale - 1:] = csum[scale:] - csum[:-scale]
    return out


def spi_from_params(accumulated, months, params_by_month, p0_by_month):
    """Apply frozen Gamma parameters AND a frozen p0 to one accumulated series.

    This deliberately does not call transform_fitted_gamma, and the reason is the
    whole point of the operational design.

    SPI is fitted to a mixed distribution -- a point mass at zero rainfall plus a
    Gamma for the positive part:

        H(x) = p0 + (1 - p0) * G(x;  alpha, beta)

    Three numbers define it, not two. transform_fitted_gamma accepts frozen alpha
    and beta but derives p0 itself, from whatever series it is handed:

        probabilities_of_zero = zeros / values.shape[0]      compute.py, line 56

    On a 74-year ERA5 series that is close to the reference-period p0 and the
    difference never surfaces. On a 48-month forecast series it is a different
    number, and SPI then drifts from run to run as the window slides, even though
    alpha and beta never move. Measured at pixel (0.875, 10.375), February, SPI-1:
    p0 = 0.367 over 1973-2002 against p0 = 0.000 over the operational window, and
    the same 0.004 mm of rainfall scores -0.34 or -1.61 depending on which was
    used.

    So the composition is done here with the frozen p0 supplied. The arithmetic
    below is the library's own (compute.py lines 83-112), not a reinvention of it:
    zeros are excluded from the Gamma, their Gamma probability is set to zero, and
    the point mass is added back. What changes is only where p0 comes from.

    Because the mixed CDF is evaluated per element, no reshaping to (years, 12) is
    needed and the calendar-alignment padding that transform_fitted_gamma required
    disappears with it -- `months` carries the alignment directly.
    """
    from scipy.stats import gamma, norm

    values = np.asarray(accumulated, float)
    alpha = np.array([params_by_month.get(m, (np.nan, np.nan))[0] for m in months])
    beta = np.array([params_by_month.get(m, (np.nan, np.nan))[1] for m in months])
    p0 = np.array([p0_by_month.get(m, np.nan) for m in months])

    # Zeros belong to the point mass, not the Gamma. gamma.cdf(0) is 0 anyway, but
    # stating it keeps the two components explicit and mirrors the library.
    is_zero = values == 0
    with np.errstate(invalid="ignore", divide="ignore"):
        g = gamma.cdf(values, a=alpha, scale=beta)
    g = np.where(is_zero, 0.0, g)

    h = p0 + (1.0 - p0) * g

    with np.errstate(invalid="ignore", divide="ignore"):
        out = norm.ppf(h)

    # indices.spi() clips to +/-3.09 as a final step; without it the tails disagree
    # by several SPI units. 3.09 is norm.ppf(0.999).
    out = np.clip(out, FITTED_INDEX_VALID_MIN, FITTED_INDEX_VALID_MAX)

    # norm.ppf returns infinities at the distribution tails; the research arm maps
    # those to NaN, so match it.
    return np.where(np.isinf(out), np.nan, out)


def load_params(path):
    """Index the frozen distribution as two lookups keyed by (x, y, scale).

    Returns (alpha, beta) and p0 separately because spi_from_params composes them
    separately, but they come from one file and one fit -- pairing a p0 with a
    Gamma from a different window would describe no climate in particular.
    """
    if not path.exists():
        raise FileNotFoundError(
            f"{path} not found. Run reference/build_gamma_params.py first."
        )
    params = pd.read_parquet(path)

    gamma_lookup = {}
    p0_lookup = {}
    for (px, py, sc), grp in params.groupby(["x", "y", "spi_scale"]):
        key = (px, py, int(sc))
        gamma_lookup[key] = {
            int(r.calendar_month): (r.alpha, r.beta) for r in grp.itertuples()
        }
        p0_lookup[key] = {
            int(r.calendar_month): float(r.p0) for r in grp.itertuples()
        }
    return params, gamma_lookup, p0_lookup


def compute_spi(spliced, lookup, p0_lookup, scales):
    """SPI at every scale for every (pixel, model) series.

    Returns the input frame with one spi{scale} column added per scale, so the
    n_forecast_* tags travel alongside the values they describe.
    """
    out = spliced.sort_values(["x", "y", "model", "t"]).reset_index(drop=True)
    for scale in scales:
        out[f"spi{scale}"] = np.nan

    missing_params = set()

    for (px, py, model), grp in out.groupby(["x", "y", "model"], sort=False):
        precip = grp["precip_mm"].to_numpy(float)
        months = grp["month"].to_numpy(int)

        for scale in scales:
            key = (px, py, scale)
            if key not in lookup or key not in p0_lookup:
                missing_params.add((px, py, scale))
                continue
            values = spi_from_params(
                accumulate(precip, scale), months, lookup[key], p0_lookup[key]
            )
            out.loc[grp.index, f"spi{scale}"] = values

    return out, missing_params


def main():
    if not SPLICED_PARQUET.exists():
        raise FileNotFoundError(
            f"{SPLICED_PARQUET} not found. Run A2_splice_history.py first."
        )

    spliced = pd.read_parquet(SPLICED_PARQUET)
    params, lookup, p0_lookup = load_params(PARAMS_PARQUET)

    n_pixels = spliced[["x", "y"]].drop_duplicates().shape[0]
    param_pixels = params[["x", "y"]].drop_duplicates().shape[0]
    print(
        f"spliced: {len(spliced):,} rows, {n_pixels} pixels, "
        f"{spliced.model.nunique()} series"
    )
    print(
        f"params:  {len(params):,} rows, {param_pixels} pixels, frozen {REF_LABEL} "
        f"(alpha, beta, p0); p0 > 0 in {int((params.p0 > 0).sum()):,} cells"
    )

    # A pixel without parameters cannot be scored, and a parameter set applied to
    # the wrong pixel is worse than no answer. Fail rather than score a subset.
    spliced_pixels = set(map(tuple, spliced[["x", "y"]].drop_duplicates().values))
    param_pixel_set = set(map(tuple, params[["x", "y"]].drop_duplicates().values))
    if not spliced_pixels <= param_pixel_set:
        n_missing = len(spliced_pixels - param_pixel_set)
        raise ValueError(
            f"{n_missing} pixel(s) in the spliced series have no frozen parameters. "
            f"Both must come from the same country clip."
        )

    result, missing = compute_spi(spliced, lookup, p0_lookup, SCALES)
    if missing:
        raise ValueError(f"{len(missing)} (pixel, scale) combinations lack parameters")

    print("\nSPI coverage and range, all series:")
    for scale in SCALES:
        col = result[f"spi{scale}"]
        finite = col[np.isfinite(col)]
        n_nan = int(col.isna().sum())
        print(
            f"  SPI-{scale:<2} {len(finite):6,} values, {n_nan:5,} NaN "
            f"(warm-up), range [{finite.min():6.2f}, {finite.max():5.2f}], "
            f"mean {finite.mean():6.3f}"
        )

    # SPI is standardised against the reference window, so a mean far from zero
    # over the
    # recent observed months is a climate signal or a bug, not noise. Worth seeing
    # separately from the forecast half.
    observed = result[result.source == "observed"]
    forecast = result[result.source == "forecast"]
    print("\nmean SPI, observed vs forecast months:")
    for scale in SCALES:
        obs = observed[f"spi{scale}"]
        fcs = forecast[f"spi{scale}"]
        print(
            f"  SPI-{scale:<2} observed {obs.mean():6.3f}   forecast {fcs.mean():6.3f}"
        )

    mme = result[result.model == "MME"]
    one = mme[(mme.x == mme.x.iloc[0]) & (mme.y == mme.y.iloc[0])]
    print(f"\none pixel ({one.x.iloc[0]}, {one.y.iloc[0]}), MME:")
    preview = one[
        ["year", "month", "source", "precip_mm"]
        + [f"spi{s}" for s in SCALES]
        + ["n_forecast_3"]
    ].tail(14)
    print(preview.round(3).to_string(index=False))

    OUT_PARQUET.parent.mkdir(parents=True, exist_ok=True)
    result.to_parquet(OUT_PARQUET, index=False)
    print(f"\nwrote {OUT_PARQUET}")


if __name__ == "__main__":
    main()
