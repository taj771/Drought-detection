"""
A5 — the forecast outlook: what A1 through A4 produced, reduced to the part
someone acts on.

A4's event table is the complete record: every event, every pixel, every model,
every scale, every threshold, observed months and forecast months together. That
is the right thing to store and the wrong thing to hand someone. This script cuts
it down to the forecast horizon and summarises it two ways.

Per month and configuration -- how much of the country is in drought, and how
sure the ensemble is. Per pixel -- the same, for mapping.

Three decisions worth stating, because each one could reasonably have gone the
other way.

MME for the headline, members for the spread. The multi-model mean is the single
best estimate, so the area figures come from it. But a mean of five models hides
whether they agree, and five models forecasting mild drought is a different
situation from four forecasting nothing and one forecasting severe. So the member
models are kept alongside, as a count: agreement is the number of the five that
put that pixel below the threshold in that month. This is not a probability. Five
models is far too small a sample to calibrate one from, and dressing a count of
five up as a percentage would invite exactly that misreading.

Forecast months only. A2 carries 36 months of observed history so SPI has a
warm-up; A3 and A4 score all of it. Those months are already known and belong to
the monitoring record, not the outlook. A5 keeps only months at or after the issue
date. The observed months remain in A3's and A4's output for anyone who wants the
context -- the outlook just does not lead with them.

Blind cells excluded from the denominator, not hidden. Where the zero-rainfall
floor sits above a threshold, that pixel cannot report drought at that threshold no
matter what happens. Counting it as "not in drought" would dilute the percentage
with cells that never had a vote. So the area fraction is computed over pixels that
could have responded, and the number excluded is reported next to it. A figure of
"12% of area in drought" means 12% of the area capable of showing it, and the
adjacent column says how much area that was.

Ensemble-mean variance compression, which every number here inherits: averaging
five models damps extremes. MME SPI is pulled toward zero relative to any single
member, so the area figures are conservative at the tails. This is a property of
averaging, not of the forecast, and it does not go away by choosing a different
threshold.
"""

from pathlib import Path

import numpy as np
import pandas as pd

# -- Configuration -----------------------------------------------------------

SPI_PARQUET = Path("Codes/operational/out/spi_forecast.parquet")
EVENTS_PARQUET = Path("Codes/operational/out/drought_events.parquet")
PARAMS_PARQUET = Path("Codes/operational/data/era5_spi_params_ref1991_2020.parquet")

OUT_MONTHLY = Path("Codes/operational/out/outlook_monthly.parquet")
OUT_PIXEL = Path("Codes/operational/out/outlook_pixel.parquet")

SCALES = [1, 3, 6, 12]
THRESHOLDS = [0.0, -0.25, -0.5, -1.0, -1.5]

# The headline series. Members are summarised around it, never replace it.
MME_MODEL = "MME"

# What the printed summary leads with. The full grid is written to parquet; this
# is only which slice a human reads first. SPI-3 at -1.0 is the conventional
# short-term operational pairing.
HEADLINE_SCALE = 3
HEADLINE_THRESHOLD = -1.0


def blind_lookup(params, thresholds):
    """Per (x, y, scale, month, threshold): could a drought register at all?

    From the same parameters A3 scored with, so the two cannot drift apart. The
    floor is norm.ppf(p0), already computed as spi_min; a threshold at or below it
    is unreachable.
    """
    frames = []
    for threshold in thresholds:
        sub = params[["x", "y", "spi_scale", "calendar_month", "spi_min"]].copy()
        sub["start_thr"] = threshold
        sub["blind"] = sub.spi_min >= threshold
        frames.append(sub.drop(columns=["spi_min"]))
    return pd.concat(frames, ignore_index=True)


def forecast_months(spi):
    """Rows at or after the issue date, for every model.

    `issue` is the forecast issue month as YYYYMM; `t` is the month index. Compare
    on (year, month) rather than on `source`, because a month can be observed for
    one purpose and still sit inside the outlook window.
    """
    issue = int(spi.issue.iloc[0])
    issue_t = (issue // 100) * 12 + (issue % 100)
    return spi[(spi.year * 12 + spi.month) >= issue_t].copy()


def pixel_table(spi, blind, scales, thresholds):
    """One row per (pixel, month, scale, threshold): MME state and member agreement.

    The MME value is the estimate. The agreement count says how many of the member
    models independently put the pixel below the threshold -- context for how much
    weight the estimate can carry, not a probability.
    """
    members = spi[spi.model != MME_MODEL]
    n_members = members.model.nunique()

    rows = []
    for scale in scales:
        column = f"spi{scale}"

        mme = spi[spi.model == MME_MODEL][
            ["x", "y", "year", "month", "t", column, f"n_forecast_{scale}"]
        ].rename(columns={column: "spi", f"n_forecast_{scale}": "n_forecast"})

        for threshold in thresholds:
            agree = (
                members.assign(below=members[column] < threshold)
                .groupby(["x", "y", "year", "month"], as_index=False)["below"]
                .sum()
                .rename(columns={"below": "n_models_below"})
            )

            table = mme.merge(agree, on=["x", "y", "year", "month"], how="left")
            table["spi_scale"] = scale
            table["start_thr"] = threshold
            table["in_drought"] = table.spi < threshold
            table["n_models"] = n_members
            rows.append(table)

    out = pd.concat(rows, ignore_index=True)

    out = out.merge(
        blind.rename(columns={"calendar_month": "month"}),
        on=["x", "y", "spi_scale", "month", "start_thr"],
        how="left",
    )
    out["blind"] = out.blind.fillna(False)

    # A blind pixel has no opinion. Saying "not in drought" there would be an
    # answer the arithmetic was never capable of contradicting.
    out.loc[out.blind, "in_drought"] = False
    return out


def monthly_table(pixels):
    """Percent of country in drought per (month, scale, threshold).

    The denominator is pixels that could have registered, not all pixels. Both
    numbers travel together so the fraction is never read without its base.
    """
    grouped = pixels.groupby(["year", "month", "t", "spi_scale", "start_thr"])

    out = grouped.apply(
        lambda g: pd.Series(
            {
                "n_pixels": len(g),
                "n_blind": int(g.blind.sum()),
                "n_eligible": int((~g.blind).sum()),
                "n_in_drought": int(g.in_drought.sum()),
                "mean_spi": float(g.loc[~g.blind, "spi"].mean()),
                "min_spi": float(g.loc[~g.blind, "spi"].min()),
                # Mean agreement across pixels currently in drought: when the MME
                # says drought, how much of the ensemble went along with it.
                "mean_models_below": float(
                    g.loc[g.in_drought, "n_models_below"].mean()
                )
                if g.in_drought.any()
                else np.nan,
            }
        ),
        include_groups=False,
    ).reset_index()

    out["pct_area"] = np.where(
        out.n_eligible > 0, 100.0 * out.n_in_drought / out.n_eligible, np.nan
    )
    return out


def main():
    for path in (SPI_PARQUET, EVENTS_PARQUET, PARAMS_PARQUET):
        if not path.exists():
            raise FileNotFoundError(f"{path} not found. Run the earlier steps first.")

    spi = pd.read_parquet(SPI_PARQUET)
    events = pd.read_parquet(EVENTS_PARQUET)
    params = pd.read_parquet(PARAMS_PARQUET)

    issue = int(spi.issue.iloc[0])
    outlook = forecast_months(spi)

    n_pixels = outlook[["x", "y"]].drop_duplicates().shape[0]
    months = outlook[["year", "month"]].drop_duplicates().sort_values(["year", "month"])
    first, last = months.iloc[0], months.iloc[-1]
    print(
        f"issue {issue}: {len(months)} forecast months, "
        f"{int(first.year)}-{int(first.month):02d} to {int(last.year)}-{int(last.month):02d}"
    )
    print(f"{n_pixels} pixels, {outlook.model.nunique()} series "
          f"({MME_MODEL} + {outlook.model.nunique() - 1} members)")

    blind = blind_lookup(params, THRESHOLDS)
    pixels = pixel_table(outlook, blind, SCALES, THRESHOLDS)
    monthly = monthly_table(pixels)

    head = monthly[
        (monthly.spi_scale == HEADLINE_SCALE)
        & (monthly.start_thr == HEADLINE_THRESHOLD)
    ].sort_values("t")

    print(f"\nSPI-{HEADLINE_SCALE} below {HEADLINE_THRESHOLD}, {MME_MODEL}:")
    print("  month     % area   in drought   eligible   blind   mean SPI   models agreeing")
    for r in head.itertuples():
        agree = f"{r.mean_models_below:.1f} of 5" if np.isfinite(r.mean_models_below) else "-"
        print(
            f"  {int(r.year)}-{int(r.month):02d}  {r.pct_area:7.1f}  "
            f"{int(r.n_in_drought):10d}  {int(r.n_eligible):9d}  {int(r.n_blind):6d}  "
            f"{r.mean_spi:8.2f}   {agree:>16}"
        )

    print("\npeak % area in drought, by scale and threshold:")
    peak = (
        monthly.groupby(["spi_scale", "start_thr"])["pct_area"]
        .max()
        .unstack()
        .round(1)
    )
    print(peak.to_string())

    # Events that are entirely forecast are the ones this outlook is about; the
    # rest are already-known conditions carried along for context.
    fc = events[(events.model == MME_MODEL) & (events.forecast_fraction == 1.0)]
    print(f"\n{MME_MODEL} events lying entirely in the forecast window: {len(fc):,}")
    if not fc.empty:
        by_class = fc.groupby(["spi_scale", "severity_class"]).size().unstack(fill_value=0)
        print(by_class.to_string())

    for path, frame in ((OUT_MONTHLY, monthly), (OUT_PIXEL, pixels)):
        path.parent.mkdir(parents=True, exist_ok=True)
        frame.to_parquet(path, index=False)
        print(f"\nwrote {path}  ({len(frame):,} rows)")


if __name__ == "__main__":
    main()
