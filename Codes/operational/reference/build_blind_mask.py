"""
Which (pixel, scale, calendar month) combinations can physically reach a drought
threshold, and which cannot.

SPI is built from a mixed distribution: a point mass at zero rainfall plus a Gamma
for the positive part. The cumulative probability is

    H(x) = p0 + (1 - p0) * G(x)

where p0 is the fraction of years with zero rainfall in that calendar month. Since
G(0) = 0, the smallest value SPI can ever take is

    SPI_min = norm.ppf(p0)

That bound is a property of the climate, not of any particular year. In northern
Benin, January has essentially no rain in most years. If p0 = 0.6 then
SPI_min = norm.ppf(0.6) = +0.25, and a threshold of -1.0 is not merely unmet --
it is unreachable. The month reports "no drought" in every year on record, and
that zero is arithmetic, not meteorology.

Left unflagged this is the worst kind of error: it looks like good news. A map
shows the dry north as drought-free precisely where drought matters most, and
nothing in the output says the number could not have come out otherwise.

So the mask is computed once from the ERA5 record, on the same 1973-2002 window
the frozen parameters use, and written to a small table that A4 reads. A4 then
distinguishes "no event" from "no event possible".

Note this affects short accumulations only. SPI-1 sums one month, so a dry-season
month is often entirely zero. SPI-6 and SPI-12 always include wet-season months,
p0 collapses to zero, and the bound disappears.

Run once, or whenever the reference window changes. Output is tiny.
"""

from pathlib import Path

import numpy as np
import pandas as pd
from scipy.stats import norm

# -- Configuration -----------------------------------------------------------

COUNTRY = "Benin"

# The observed ERA5 record, from A0b_refresh_era5.py.
ERA5_PARQUET = Path("Codes/operational/data/era5_monthly_observed.parquet")

OUT_PARQUET = Path("Codes/operational/reference/blind_cells.parquet")

SCALES = [1, 3, 6, 12]

# Must match the window the frozen Gamma parameters were fitted on. p0 is part of
# the same fitted distribution; measuring it over a different period would give a
# bound that does not belong to the parameters being applied.
REF_Y0, REF_Y1 = 1973, 2002

# The thresholds A4 detects against. All five, matching the research arm.
THRESHOLDS = [0.0, -0.25, -0.5, -1.0, -1.5]


def accumulate(values, scale):
    """Rolling sum over `scale` months; first scale-1 positions are NaN."""
    if scale == 1:
        return values.astype(float)
    out = np.full(len(values), np.nan)
    csum = np.cumsum(np.insert(values.astype(float), 0, 0.0))
    out[scale - 1:] = csum[scale:] - csum[:-scale]
    return out


def zero_probability(series, months, scale, y0, y1, years):
    """p0 per calendar month: the fraction of reference years with zero total.

    Computed on the ACCUMULATED series, because that is what SPI is fitted to.
    For scale 1 this is the fraction of dry months; for scale 3 the fraction of
    dry three-month windows, which is rarer and often zero.
    """
    accumulated = accumulate(series, scale)
    in_window = (years >= y0) & (years <= y1)

    p0 = {}
    for month in range(1, 13):
        sel = in_window & (months == month) & ~np.isnan(accumulated)
        if sel.sum() == 0:
            p0[month] = np.nan
            continue
        values = accumulated[sel]
        p0[month] = float((values <= 0).sum() / len(values))
    return p0


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
    print(f"reference window for p0: {REF_Y0}-{REF_Y1}")

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
            p0 = zero_probability(precip, months, scale, REF_Y0, REF_Y1, years)
            for month, p in p0.items():
                # norm.ppf(0) is -inf, which is the ordinary case: no dry years,
                # no floor, every threshold reachable.
                spi_min = float(norm.ppf(p)) if p > 0 else -np.inf
                rows.append(
                    {
                        "x": px,
                        "y": py,
                        "spi_scale": scale,
                        "calendar_month": month,
                        "p0": p,
                        "spi_min": spi_min,
                    }
                )

    mask = pd.DataFrame(rows)

    # A threshold is unreachable when the floor sits at or above it.
    for thr in THRESHOLDS:
        column = f"blind_{round(abs(thr) * 100):03d}"
        mask[column] = mask.spi_min >= thr

    print("\nunreachable (pixel, month) combinations, out of 154 x 12 = 1,848 per scale:")
    header = "  scale  " + "  ".join(f"thr{t:>6.2f}" for t in THRESHOLDS)
    print(header)
    for scale in SCALES:
        sub = mask[mask.spi_scale == scale]
        counts = [
            int(sub[f"blind_{round(abs(t) * 100):03d}"].sum()) for t in THRESHOLDS
        ]
        print(f"  {scale:<6} " + "  ".join(f"{c:9,}" for c in counts))

    # The dry-season months are where this concentrates; show them explicitly so
    # the pattern is visible rather than inferred from a total.
    print("\nSPI-1, unreachable at -1.0, by calendar month:")
    s1 = mask[mask.spi_scale == 1]
    by_month = s1.groupby("calendar_month")["blind_100"].sum()
    for month, count in by_month.items():
        if count:
            print(f"  month {month:2d}: {int(count):3d} of {n_pixels} pixels")

    OUT_PARQUET.parent.mkdir(parents=True, exist_ok=True)
    mask.to_parquet(OUT_PARQUET, index=False)
    print(f"\nwrote {OUT_PARQUET}")


if __name__ == "__main__":
    main()
