"""
Fit the frozen SPI distribution on the reference period: alpha, beta and p0
together, from the operational arm's own ERA5 record.

Until now the operational arm borrowed alpha and beta from the research arm's
script 05, which fitted them over 1973-2002, and measured p0 separately to match.
This script replaces that arrangement with a single fit over one window, so all
three parameters of

    H(x) = p0 + (1 - p0) * G(x;  alpha, beta)

come from the same years and the same record. That matters more than it sounds:
p0 and the Gamma are not independent. p0 is the mass the Gamma does not describe,
so pairing a p0 from one window with a Gamma from another describes no climate in
particular.

The Gamma fit uses climate_indices.compute.gamma_parameters -- the library's own
fitter, the same call script 05 makes. Fitting by hand here would risk a different
convention (the library excludes zeros before fitting and uses a specific
estimator), and a parameter set that does not match what the transform expects is
wrong in a way nothing downstream would catch.

WINDOW. Set below. The operational arm's window is a free choice in a way the
research arm's is not: A1 already consumed NMME's own 1991-2020 climatology when
it reconstructed absolute rainfall, and A3 receives millimetres with no memory of
it. Any window works provided ERA5 covers it, it is long enough for a stable fit,
and everything being compared uses the same one.

The consequence of changing it is not cosmetic. SPI answers "dry relative to
what", and Benin's later decades are wetter, so the same rainfall scores drier
against 1991-2020 than against 1973-2002. Events get more frequent, not because
the weather changed but because the yardstick did. Anything compared across the
two windows is comparing yardsticks.

Run once, or whenever the window changes. Writes alpha, beta and p0 to one file.
"""

from pathlib import Path

import numpy as np
import pandas as pd
from scipy.stats import norm

# -- Configuration -----------------------------------------------------------

COUNTRY = "Benin"

ERA5_PARQUET = Path("Codes/operational/data/era5_monthly_observed.parquet")

# The reference window. Changing this changes what "drought" means -- see the
# module docstring. 1991-2020 is the current WMO climate normal and the window
# NMME's own anomalies are defined against.
REF_Y0, REF_Y1 = 1991, 2020

OUT_PARQUET = Path(
    f"Codes/operational/data/era5_spi_params_ref{REF_Y0}_{REF_Y1}.parquet"
)

SCALES = [1, 3, 6, 12]

# Reported only, to show where the zero-rainfall floor blocks a threshold.
THRESHOLDS = [0.0, -0.25, -0.5, -1.0, -1.5]


def accumulate(values, scale):
    """Rolling sum over `scale` months; first scale-1 positions are NaN."""
    if scale == 1:
        return values.astype(float)
    out = np.full(len(values), np.nan)
    csum = np.cumsum(np.insert(values.astype(float), 0, 0.0))
    out[scale - 1:] = csum[scale:] - csum[:-scale]
    return out


def fit_gamma(accumulated, first_year, y0, y1):
    """alpha and beta per calendar month, via climate_indices' own fitter.

    gamma_parameters reshapes to (years, 12) and fits each column, so the series
    must start in January. The caller guarantees that.
    """
    from climate_indices import compute

    alphas, betas = compute.gamma_parameters(
        accumulated,
        data_start_year=first_year,
        calibration_start_year=y0,
        calibration_end_year=y1,
        periodicity=compute.Periodicity.monthly,
    )
    return np.asarray(alphas, float), np.asarray(betas, float)


def fit_p0(accumulated, months, years, y0, y1):
    """p0 per calendar month: the fraction of reference years with zero total.

    The test is `== 0`, matching the library's _replace_zeros_with_nan. A trace
    amount of rain is a positive value belonging in the Gamma, not in the point
    mass; treating it as zero would inflate p0 and raise the floor for no reason.
    """
    in_window = (years >= y0) & (years <= y1)

    p0 = {}
    n_years = {}
    for month in range(1, 13):
        sel = in_window & (months == month) & ~np.isnan(accumulated)
        if sel.sum() == 0:
            p0[month] = np.nan
            n_years[month] = 0
            continue
        values = accumulated[sel]
        p0[month] = float((values == 0).sum() / len(values))
        n_years[month] = int(len(values))
    return p0, n_years


def main():
    if not ERA5_PARQUET.exists():
        raise FileNotFoundError(
            f"{ERA5_PARQUET} not found. Run A0b_refresh_era5.py and copy the output down."
        )

    era5 = pd.read_parquet(ERA5_PARQUET)
    if "country" in era5.columns:
        era5 = era5[era5.country == COUNTRY]
    era5 = era5.sort_values(["x", "y", "year", "month"])

    n_pixels = era5[["x", "y"]].drop_duplicates().shape[0]
    print(f"ERA5: {len(era5):,} rows, {n_pixels} pixels, {era5.year.min()}-{era5.year.max()}")
    print(f"fitting window: {REF_Y0}-{REF_Y1}")

    if era5.year.min() > REF_Y0 or era5.year.max() < REF_Y1:
        raise ValueError(
            f"observed record {era5.year.min()}-{era5.year.max()} does not cover "
            f"{REF_Y0}-{REF_Y1}"
        )

    rows = []
    for (px, py), grp in era5.groupby(["x", "y"]):
        # gamma_parameters reshapes to whole years, so the series must begin in
        # January. Trim rather than pad: a partial leading year would shift every
        # calendar month by its length.
        first = grp[grp.month == 1].iloc[0]
        grp = grp[
            (grp.year * 12 + grp.month) >= (int(first.year) * 12 + 1)
        ]

        precip = grp["monthly_precip"].to_numpy(float)
        months = grp["month"].to_numpy(int)
        years = grp["year"].to_numpy(int)
        first_year = int(grp["year"].iloc[0])

        # Drop any trailing partial year for the same reason.
        n_whole = (len(precip) // 12) * 12
        precip, months, years = precip[:n_whole], months[:n_whole], years[:n_whole]

        for scale in SCALES:
            accumulated = accumulate(precip, scale)
            alphas, betas = fit_gamma(accumulated, first_year, REF_Y0, REF_Y1)
            p0, n_years = fit_p0(accumulated, months, years, REF_Y0, REF_Y1)

            for month in range(1, 13):
                p = p0[month]
                spi_min = float(norm.ppf(p)) if p > 0 else -np.inf
                rows.append(
                    {
                        "x": px,
                        "y": py,
                        "country": COUNTRY,
                        "spi_scale": scale,
                        "calendar_month": month,
                        "alpha": float(alphas[month - 1]),
                        "beta": float(betas[month - 1]),
                        "p0": p,
                        "spi_min": spi_min,
                        "n_ref_years": n_years[month],
                    }
                )

    params = pd.DataFrame(rows)

    expected = REF_Y1 - REF_Y0 + 1
    short = params[(params.n_ref_years > 0) & (params.n_ref_years < expected)]
    if not short.empty:
        raise ValueError(
            f"{len(short)} (pixel, scale, month) combinations have fewer than "
            f"{expected} reference years"
        )

    bad = params[~np.isfinite(params.alpha) | ~np.isfinite(params.beta)]
    if not bad.empty:
        # A cell that is dry in every reference year has no positive values to fit
        # and is described entirely by p0. That is legitimate; anything else is not.
        unexplained = bad[bad.p0 < 1.0]
        if not unexplained.empty:
            raise ValueError(
                f"{len(unexplained)} cells have a non-finite Gamma fit but p0 < 1"
            )
        print(f"\n{len(bad)} cells dry in every reference year (p0 = 1, Gamma undefined)")

    print(f"\nfitted {len(params):,} (pixel, scale, month) parameter sets")
    for scale in SCALES:
        sub = params[params.spi_scale == scale]
        nz = sub[sub.p0 > 0]
        print(
            f"  SPI-{scale:<2} alpha [{sub.alpha.min():8.3f}, {sub.alpha.max():9.3f}]  "
            f"p0 > 0 in {len(nz):4,} cells"
        )

    print("\nthresholds made unreachable by the zero-rainfall floor (of 1,848):")
    print("  scale  " + "  ".join(f"thr{t:>6.2f}" for t in THRESHOLDS))
    for scale in SCALES:
        sub = params[params.spi_scale == scale]
        counts = [int((sub.spi_min >= t).sum()) for t in THRESHOLDS]
        print(f"  {scale:<6} " + "  ".join(f"{c:9,}" for c in counts))

    OUT_PARQUET.parent.mkdir(parents=True, exist_ok=True)
    params.to_parquet(OUT_PARQUET, index=False)
    print(f"\nwrote {OUT_PARQUET}")


if __name__ == "__main__":
    main()
