"""
Export the zero-rainfall probability p0 over the reference period, as a frozen
parameter alongside alpha and beta.

Why this file has to exist.

SPI is fitted to a mixed distribution: a point mass at zero rainfall plus a Gamma
for the positive part.

    H(x) = p0 + (1 - p0) * G(x;  alpha, beta)

Three numbers per (pixel, scale, calendar month) define it, not two. Script 05 of
the research arm exports alpha and beta. It does not export p0, because
climate_indices computes p0 internally -- and it computes it from whatever series
it is handed:

    probabilities_of_zero = zeros / values.shape[0]      compute.py, line 56

For a 74-year ERA5 series that is nearly the reference-period p0, so the omission
never showed. For a 48-month operational series it is a different number entirely,
and the consequence is that SPI drifts between runs even though alpha and beta are
frozen. Measured at pixel (0.875, 10.375), February, SPI-1:

    reference 1973-2002    11 of 30 Februaries dry    p0 = 0.367    floor -0.34
    operational 48 months   0 of  4 Februaries dry    p0 = 0.000    SPI  -1.61

Same rainfall, two answers, and the second one moves as the window slides. Freezing
alpha and beta while letting p0 float does not freeze the distribution.

So p0 is measured here once, over the same window and from the same ERA5 record the
Gamma parameters were fitted on, and A3 applies it explicitly instead of letting the
library infer one.

A consequence worth stating: p0 also sets a hard floor on SPI, since G(0) = 0 gives
SPI_min = norm.ppf(p0). Where p0 is large that floor can sit above a drought
threshold, making the threshold unreachable -- a cell that reports "no drought"
because the arithmetic forbids it, not because conditions are good. The floor is
written out here too, and A4 uses it to distinguish "no drought" from "no drought
detectable".

Run once, or whenever the reference window changes.
"""

from pathlib import Path

import numpy as np
import pandas as pd
from scipy.stats import norm

# -- Configuration -----------------------------------------------------------

COUNTRY = "Benin"

ERA5_PARQUET = Path("Codes/operational/data/era5_monthly_observed.parquet")
OUT_PARQUET = Path("Codes/operational/data/era5_p0_ref1973_2002.parquet")

SCALES = [1, 3, 6, 12]

# Must match the window alpha and beta were fitted on. p0 is part of the same
# distribution; measuring it over a different period would pair a zero-probability
# from one climate with a Gamma from another.
REF_Y0, REF_Y1 = 1973, 2002

# The thresholds A4 detects against, used only to report reachability here.
THRESHOLDS = [0.0, -0.25, -0.5, -1.0, -1.5]


def accumulate(values, scale):
    """Rolling sum over `scale` months; first scale-1 positions are NaN."""
    if scale == 1:
        return values.astype(float)
    out = np.full(len(values), np.nan)
    csum = np.cumsum(np.insert(values.astype(float), 0, 0.0))
    out[scale - 1:] = csum[scale:] - csum[:-scale]
    return out


def zero_probability(accumulated, months, years, y0, y1):
    """p0 per calendar month over the reference window.

    Measured on the ACCUMULATED series because that is what the Gamma was fitted
    to. At scale 1 this is the fraction of dry months; at scale 6 it is the
    fraction of dry six-month windows, which in Benin is zero everywhere.

    The test is `== 0`, matching climate_indices' _replace_zeros_with_nan rather
    than `<= 0`. A trace amount of rainfall is a positive value that belongs in
    the Gamma, not in the point mass, and treating it as a zero would inflate p0
    and raise the floor for no reason.
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
    print(f"reference window: {REF_Y0}-{REF_Y1}")

    covered = era5[(era5.year >= REF_Y0) & (era5.year <= REF_Y1)]
    if covered.empty:
        raise ValueError(
            f"observed record does not cover {REF_Y0}-{REF_Y1}; p0 cannot be measured"
        )

    rows = []
    for (px, py), grp in era5.groupby(["x", "y"]):
        precip = grp["monthly_precip"].to_numpy(float)
        months = grp["month"].to_numpy(int)
        years = grp["year"].to_numpy(int)

        for scale in SCALES:
            accumulated = accumulate(precip, scale)
            p0, n_years = zero_probability(accumulated, months, years, REF_Y0, REF_Y1)
            for month, p in p0.items():
                # p0 = 0 means no dry years and no floor: SPI is unbounded below,
                # which is the ordinary case at long accumulations.
                spi_min = float(norm.ppf(p)) if p > 0 else -np.inf
                rows.append(
                    {
                        "x": px,
                        "y": py,
                        "spi_scale": scale,
                        "calendar_month": month,
                        "p0": p,
                        "spi_min": spi_min,
                        "n_ref_years": n_years[month],
                    }
                )

    params = pd.DataFrame(rows)

    # Every (pixel, scale, month) must have a full reference window behind it. A
    # p0 measured on five years is not the same quantity as one measured on thirty
    # and must not be silently mixed in.
    expected = (REF_Y1 - REF_Y0 + 1)
    short = params[(params.n_ref_years > 0) & (params.n_ref_years < expected)]
    if not short.empty:
        raise ValueError(
            f"{len(short)} (pixel, scale, month) combinations have fewer than "
            f"{expected} reference years; p0 would not be comparable across cells"
        )

    print(f"\np0 over the reference window, by scale:")
    for scale in SCALES:
        sub = params[params.spi_scale == scale]
        nonzero = sub[sub.p0 > 0]
        print(
            f"  SPI-{scale:<2} p0 > 0 in {len(nonzero):4,} of {len(sub):,} cells, "
            f"max p0 {sub.p0.max():.3f}, floor as high as {sub.spi_min.max():+.2f}"
            if len(nonzero)
            else f"  SPI-{scale:<2} p0 = 0 everywhere; no floor at any cell"
        )

    print("\nthresholds made unreachable by the floor (cells per scale, of 1,848):")
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
