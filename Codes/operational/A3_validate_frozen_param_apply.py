"""
A3 validation — does A3's own transform reproduce the SPI that climate_indices
produces internally?

This tests the function A3 actually calls, A3_spi_frozen.spi_from_params, imported
from A3 rather than copied. A copy would drift: the point of a test is to fail when
the code changes, and a reimplementation here would keep passing while A3 broke.

What is being checked. A3 does not call transform_fitted_gamma. It composes the
mixed distribution explicitly:

    H(x) = p0 + (1 - p0) * G(x;  alpha, beta)

because transform_fitted_gamma accepts frozen alpha and beta but re-derives p0 from
the calibration window of whatever series it is handed. On a 74-year ERA5 series that
window holds the reference years; on a 48-month forecast series it does not, p0 is a
different number, and SPI then drifts run to run even though alpha and beta never
move. A3 exists to close that gap, so the test has to exercise the composition
and not the library call it replaced.

An earlier version of this file tested transform_fitted_gamma directly. It reported
EXACT, and would have gone on reporting EXACT however wrong A3's p0 handling became,
because it never ran A3's code. That is the failure mode this rewrite removes.

Ground truth. spi_era5_reference_period.parquet holds SPI for the same 154 pixels
produced by climate_indices 3.0.0 from the 1973-2002 parameters. To compare like
with like, p0 is supplied here as the library itself derives it: the zero fraction of
each calendar month's non-missing values over 1973-2002. (climate_indices 2.x counted
zeros over the whole series instead, with missing values in the denominator; a
ground-truth file from 2.x will not match.) Anything else would make the two
legitimately differ and the comparison would test the choice of p0 rather than the
arithmetic.

The negative control matters as much as the agreement. A test that only confirms
agreement cannot distinguish correct code from a comparison that is blind. So the
same check is repeated with a deliberately wrong p0, and it must fail. If both pass,
the test is not measuring anything.

Verdict thresholds: agreement to ~1e-6 means the transform is exact. Agreement to
~1e-2 with no bias means it is usable but worth understanding. Systematic offset
means the transform is wrong.
"""

import importlib.util
from pathlib import Path

import numpy as np
import pandas as pd

DATA = Path("Codes/operational/data")

# The 1973-2002 Gamma parameters and the SPI climate_indices produced from them.
# This pair is the ground truth and stays on that window deliberately: the question
# is whether A3's arithmetic matches the library's, which is independent of which
# window the parameters came from. A3 itself now runs on 1991-2020.
PARAMS = DATA / "era5_gamma_params_ref1973_2002.parquet"
KNOWN_GOOD = DATA / "spi_era5_reference_period.parquet"

SCALES = [1, 3, 6, 12]

# The calibration window script 05 handed the library, which is the window the
# library measures p0 over.
REF_Y0, REF_Y1 = 1973, 2002

# A wrong p0 for the negative control. 0.2 is large enough to move SPI well past any
# tolerance yet small enough to stay inside the distribution, so a failure to detect
# it is a real blindness rather than an arithmetic overflow.
CONTROL_P0 = 0.2


def load_a3():
    """Import A3 by path, so the test runs the shipped code and not a copy."""
    path = Path("Codes/operational/A3_spi_frozen.py")
    if not path.exists():
        raise FileNotFoundError(f"{path} not found; cannot validate A3 without A3")
    spec = importlib.util.spec_from_file_location("a3_spi_frozen", path)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def library_p0(accumulated, years):
    """p0 as climate_indices 3.0.0 derives it, over the calibration window.

        zeros / non-missing, per calendar month, calibration years only
        (compute._calibration_probabilities_of_zero)

    Reproduced here so the comparison isolates the arithmetic. NaN is left out of
    the denominator, so the warm-up positions at scale > 1 do not count; a month
    with no non-missing calibration value has no defined p0 and gets NaN.
    """
    values = np.asarray(accumulated, float)
    years = np.asarray(years, int)
    reshaped = values.reshape(-1, 12)
    in_window = (years.reshape(-1, 12)[:, 0] >= REF_Y0) & (years.reshape(-1, 12)[:, 0] <= REF_Y1)

    p0 = {}
    for month in range(1, 13):
        column = reshaped[in_window, month - 1]
        valid = ~np.isnan(column)
        p0[month] = float((column[valid] == 0).sum() / valid.sum()) if valid.any() else np.nan
    return p0


def compare(mine, theirs):
    """Difference statistics over positions where both are finite."""
    both = ~np.isnan(mine) & ~np.isnan(theirs)
    if both.sum() == 0:
        return None
    diff = mine[both] - theirs[both]
    return {
        "n": int(both.sum()),
        "max_abs_diff": float(np.abs(diff).max()),
        "mean_diff": float(diff.mean()),
        "rmse": float(np.sqrt((diff ** 2).mean())),
    }


def run(a3, known, plookup, scales, p0_override=None):
    """Recompute SPI through A3's transform and compare against the known-good file.

    p0_override replaces the library-equivalent p0 everywhere, which is how the
    negative control is built.
    """
    results = []

    for (px, py), grp in known.groupby(["x", "y"]):
        grp = grp.sort_values(["year", "month"]).reset_index(drop=True)
        precip = grp["monthly_precip"].to_numpy(float)
        months = grp["month"].to_numpy(int)
        years = grp["year"].to_numpy(int)

        # A3's composition is per element and needs no reshaping, but library_p0
        # does reshape, so the series must be whole years starting in January.
        if len(precip) % 12 or int(grp["month"].iloc[0]) != 1:
            raise ValueError(
                f"pixel ({px}, {py}) is not whole calendar years starting in January; "
                f"the library-equivalent p0 cannot be reproduced"
            )

        for scale in scales:
            key = (px, py, scale)
            if key not in plookup:
                continue

            accumulated = a3.accumulate(precip, scale)
            p0 = (
                {m: p0_override for m in range(1, 13)}
                if p0_override is not None
                else library_p0(accumulated, years)
            )

            mine = a3.spi_from_params(accumulated, months, plookup[key], p0)
            stats = compare(mine, grp[f"spi{scale}"].to_numpy(float))
            if stats:
                results.append({"scale": scale, **stats})

    return (
        pd.DataFrame(results)
        .groupby("scale")
        .agg(
            n=("n", "sum"),
            max_abs_diff=("max_abs_diff", "max"),
            mean_diff=("mean_diff", "mean"),
            rmse=("rmse", "mean"),
        )
        .round(8)
    )


def main():
    for path in (PARAMS, KNOWN_GOOD):
        if not path.exists():
            raise FileNotFoundError(f"{path} not found")

    a3 = load_a3()
    params = pd.read_parquet(PARAMS)
    known = pd.read_parquet(KNOWN_GOOD)

    print(f"testing:    {a3.__file__}")
    print(f"            spi_from_params, the transform A3 applies")
    print(f"params:     {len(params):,} rows, "
          f"{params[['x', 'y']].drop_duplicates().shape[0]} pixels")
    print(f"known-good: {len(known):,} rows, {known.year.min()}-{known.year.max()}")

    plookup = {}
    for (px, py, sc), grp in params.groupby(["x", "y", "spi_scale"]):
        plookup[(px, py, int(sc))] = {
            int(r.calendar_month): (r.alpha, r.beta) for r in grp.itertuples()
        }

    summary = run(a3, known, plookup, SCALES)
    print("\nA3 transform vs climate_indices, matched p0:")
    print(summary)

    worst = float(summary.max_abs_diff.max())
    bias = float(summary.mean_diff.abs().max())

    # A test that can only pass proves nothing. Repeat with a p0 known to be wrong;
    # the same comparison must reject it, or the agreement above is not evidence.
    control = run(a3, known, plookup, SCALES, p0_override=CONTROL_P0)
    control_worst = float(control.max_abs_diff.max())
    print(f"\nnegative control, p0 forced to {CONTROL_P0} everywhere:")
    print(control)

    print()
    if control_worst < 1e-2:
        print(
            f"TEST IS BLIND — a deliberately wrong p0 still agrees to "
            f"{control_worst:.2e}. The comparison is not sensitive to p0, so the "
            f"result above is not evidence that A3 is correct."
        )
        return

    print(f"control rejected as it should be (max abs diff {control_worst:.2f})")

    if worst < 1e-6:
        print(f"EXACT — max abs diff {worst:.2e}. A3's transform matches the library.")
    elif worst < 1e-2 and bias < 1e-3:
        print(f"CLOSE — max abs diff {worst:.2e}, no systematic bias ({bias:.2e}).")
        print("Usable, but understand the source of the difference before relying on it.")
    else:
        print(f"DIVERGENT — max abs diff {worst:.2e}, bias {bias:.2e}.")
        print("A3's transform does not match the library. Do not trust its output.")


if __name__ == "__main__":
    main()
