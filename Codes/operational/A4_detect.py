"""
A4 — turn the forecast SPI series into drought events.

A3 produces one SPI value per month. A4 turns that into a list of events, each
with a start, an end, a duration and a severity, using the same run-length rule
as the research arm's 03_v2_Drought_Detection_pandas_udf.py. The rule is copied
deliberately rather than improved: an operational event and a research event have
to mean the same thing or the two arms cannot be compared.

The detection itself is simple. Months below the threshold form a run; a run of at
least MIN_DURATION months is an event. Severity is the sum of the SPI values over
the run, the peak is the minimum, and the class comes from the peak.

Two things this script adds that the research version has no need for.

Forecast content per event. An event running July to September 2026 is mostly
measured rainfall; one running December to February is entirely predicted. Both
appear as a row with a start date and a severity, and nothing in those columns
distinguishes them. A4 carries n_forecast_months and forecast_fraction per event
so the difference is explicit. An event with forecast_fraction 0.0 already
happened; one with 1.0 is a prediction.

Blind cells. Where dry-season rainfall is zero in most years, SPI has an
arithmetic floor above the threshold and a drought cannot be detected there no
matter what the weather does. The floor is norm.ppf(p0) and comes from the same
parameter file A3 scores with, so it is guaranteed to describe the distribution
actually in use -- a separately measured mask could silently fall out of step with
the parameters after a refit. A4 reports the affected cells rather than letting
them sit in the output as apparent good news. This is the difference between
"no drought" and "no drought detectable", and only one of those is information.

A note on NaN handling, inherited from the research rule: NaN months are dropped
before the run-length pass, so they cannot split an event. That is correct for the
warm-up NaN at the head of each series, which is all this input contains. It would
be wrong for a gap in the middle -- two separate events either side of a hole would
merge into one. A2 refuses to produce such a gap, which is what makes this safe.
"""

from pathlib import Path

import numpy as np
import pandas as pd

# -- Configuration -----------------------------------------------------------

SPI_PARQUET = Path("Codes/operational/out/spi_forecast.parquet")

# The same file A3 scores with. spi_min = norm.ppf(p0) is the arithmetic floor.
PARAMS_PARQUET = Path("Codes/operational/data/era5_spi_params_ref1991_2020.parquet")
OUT_PARQUET = Path("Codes/operational/out/drought_events.parquet")

SCALES = [1, 3, 6, 12]

# All five, matching the research arm. Do not remove any: the set is what makes
# the two arms comparable, and a threshold that looks uninteresting for one
# country is the operative one for another.
THRESHOLDS = [0.0, -0.25, -0.5, -1.0, -1.5]

MIN_DURATION = 1

PREFIX = {1: "S1", 3: "S3", 6: "S6", 12: "S12"}


def severity_class(peak):
    """Peak-based class, matching the research arm's boundaries exactly."""
    if peak >= -1.0:
        return "Mild"
    if -1.5 <= peak < -1.0:
        return "Moderate"
    if -2.0 <= peak < -1.5:
        return "Severe"
    return "Extreme"


def detect_runs(values, threshold, min_duration):
    """Index ranges of runs below the threshold, as (start, end_exclusive) pairs.

    Run-length encoding over the boolean below-threshold series, the same
    construction the research arm uses.
    """
    below = values < threshold
    if not below.any():
        return []

    n = len(values)
    change = np.flatnonzero(np.diff(below.astype(int))) + 1
    starts = np.concatenate(([0], change))
    ends = np.concatenate((change, [n]))

    return [
        (s, e)
        for s, e in zip(starts, ends)
        if below[s] and (e - s) >= min_duration
    ]


def build_event(values, years, months, n_forecast, s, e, event_id):
    """One event record from a run spanning positions [s, e)."""
    window = values[s:e]
    cumulative = np.cumsum(window)
    peak_position = int(np.argmin(window))
    n_fc = n_forecast[s:e]

    return {
        "drought_id": event_id,
        "start_year": int(years[s]),
        "start_month": int(months[s]),
        "end_year": int(years[e - 1]),
        "end_month": int(months[e - 1]),
        "duration_months": int(e - s),
        "entry_index": float(window[0]),
        "peak_index": float(window.min()),
        "peak_year": int(years[s + peak_position]),
        "peak_month": int(months[s + peak_position]),
        "cum_deficit_neg": float(window[window < 0].sum()),
        "total_severity": float(window.sum()),
        "min_cum_deficit": float(cumulative.min()),
        "severity_class": severity_class(float(window.min())),
        # How much of the event is prediction rather than measurement. The count
        # is of months whose own accumulation window contains any forecast; the
        # fraction is that count over the event length.
        "n_forecast_months": int((n_fc > 0).sum()),
        "forecast_fraction": float((n_fc > 0).sum() / (e - s)),
    }


def detect_series(grp, scale, threshold, min_duration):
    """Events for one (pixel, model) series at one scale and threshold."""
    column = f"spi{scale}"
    finite = grp[column].notna().to_numpy()
    if finite.sum() < min_duration:
        return []

    values = grp.loc[finite, column].to_numpy(float)
    years = grp.loc[finite, "year"].to_numpy(int)
    months = grp.loc[finite, "month"].to_numpy(int)
    n_forecast = grp.loc[finite, f"n_forecast_{scale}"].to_numpy(float)

    events = []
    for event_id, (s, e) in enumerate(
        detect_runs(values, threshold, min_duration), start=1
    ):
        events.append(build_event(values, years, months, n_forecast, s, e, event_id))
    return events


def detect_all(spi, scales, thresholds, min_duration):
    """Every event, for every series, scale and threshold."""
    rows = []
    for (px, py, model), grp in spi.groupby(["x", "y", "model"], sort=False):
        grp = grp.sort_values("t")
        for scale in scales:
            for threshold in thresholds:
                config_id = f"{PREFIX[scale]}-T-{round(abs(threshold) * 100):03d}"
                for event in detect_series(grp, scale, threshold, min_duration):
                    rows.append(
                        {
                            "x": px,
                            "y": py,
                            "model": model,
                            "config_id": config_id,
                            "spi_scale": scale,
                            "start_thr": threshold,
                            "min_duration": min_duration,
                            **event,
                        }
                    )
    return pd.DataFrame(rows)


def build_blind_mask(params, thresholds):
    """Per (cell, scale, month, threshold): is the threshold arithmetically reachable?

    Derived from the scoring parameters rather than measured separately, so the two
    cannot disagree. A threshold is unreachable when the floor sits at or above it;
    where p0 = 0 the floor is -inf and every threshold is reachable.
    """
    mask = params[["x", "y", "spi_scale", "calendar_month", "p0", "spi_min"]].copy()
    for threshold in thresholds:
        mask[f"blind_{round(abs(threshold) * 100):03d}"] = mask.spi_min >= threshold
    return mask


def flag_blind(events, blind, thresholds):
    """Mark events in cells where the threshold is arithmetically unreachable.

    An event can still be detected in such a cell at a looser threshold, so the
    flag is per (cell, scale, month, threshold) and attaches to the event's START
    month -- the month that decides whether the run begins at all.
    """
    if blind is None:
        events["start_month_blind"] = False
        return events

    long = []
    for threshold in thresholds:
        column = f"blind_{round(abs(threshold) * 100):03d}"
        sub = blind[["x", "y", "spi_scale", "calendar_month", column]].copy()
        sub = sub.rename(columns={column: "start_month_blind"})
        sub["start_thr"] = threshold
        long.append(sub)
    long = pd.concat(long, ignore_index=True)

    merged = events.merge(
        long,
        left_on=["x", "y", "spi_scale", "start_month", "start_thr"],
        right_on=["x", "y", "spi_scale", "calendar_month", "start_thr"],
        how="left",
    ).drop(columns=["calendar_month"])
    merged["start_month_blind"] = merged.start_month_blind.fillna(False)
    return merged


def report_blind_coverage(spi, blind, scales, thresholds):
    """How many of the scored (cell, month) slots could not have produced an event.

    This is the number that must travel with any 'no drought detected' statement.
    """
    if blind is None:
        print("\nblind-cell mask not found -- unreachable thresholds are NOT flagged")
        return

    print("\nslots where the threshold is unreachable (of those scored):")
    print("  scale  " + "  ".join(f"thr{t:>6.2f}" for t in thresholds))

    for scale in scales:
        column = f"spi{scale}"
        scored = spi[spi[column].notna()][["x", "y", "month"]].drop_duplicates()
        sub = blind[blind.spi_scale == scale]
        counts = []
        for threshold in thresholds:
            flag = f"blind_{round(abs(threshold) * 100):03d}"
            blind_cells = sub[sub[flag]][["x", "y", "calendar_month"]].rename(
                columns={"calendar_month": "month"}
            )
            n = len(scored.merge(blind_cells, on=["x", "y", "month"], how="inner"))
            counts.append(n)
        total = len(scored)
        print(
            f"  {scale:<6} "
            + "  ".join(f"{c:4,}/{total:<4}" for c in counts)
        )


def main():
    if not SPI_PARQUET.exists():
        raise FileNotFoundError(f"{SPI_PARQUET} not found. Run A3_spi_frozen.py first.")

    spi = pd.read_parquet(SPI_PARQUET)

    if not PARAMS_PARQUET.exists():
        raise FileNotFoundError(
            f"{PARAMS_PARQUET} not found. Run reference/build_gamma_params.py first -- "
            f"without the floor, unreachable-threshold cells are reported as "
            f"drought-free, which reads as good news."
        )
    blind = build_blind_mask(pd.read_parquet(PARAMS_PARQUET), THRESHOLDS)

    n_pixels = spi[["x", "y"]].drop_duplicates().shape[0]
    print(
        f"SPI input: {len(spi):,} rows, {n_pixels} pixels, "
        f"{spi.model.nunique()} series, {len(SCALES)} scales x {len(THRESHOLDS)} thresholds"
    )

    events = detect_all(spi, SCALES, THRESHOLDS, MIN_DURATION)
    if events.empty:
        print("no events detected at any threshold")
        return

    events = flag_blind(events, blind, THRESHOLDS)

    print(f"\nevents: {len(events):,} across all configs")
    counts = (
        events.groupby(["spi_scale", "start_thr"])
        .size()
        .unstack(fill_value=0)
    )
    print("\nevent count by scale and threshold:")
    print(counts.to_string())

    # An event that is entirely forecast is a prediction; one that is entirely
    # observed already happened. Both are in the same table and only this column
    # separates them.
    print("\nevents by forecast content:")
    bins = [-0.001, 0.0, 0.999, 1.0]
    labels = ["observed only", "mixed", "forecast only"]
    events["content"] = pd.cut(events.forecast_fraction, bins=bins, labels=labels)
    for label, count in events.content.value_counts().reindex(labels).items():
        print(f"  {label:<14} {int(count):6,}")

    if blind is not None:
        n_blind = int(events.start_month_blind.sum())
        print(f"\nevents starting in an unreachable-threshold cell: {n_blind:,}")
        print("(expected to be 0 by construction -- an event cannot start where")
        print(" the threshold cannot be crossed; a non-zero count is a bug)")

    report_blind_coverage(spi, blind, SCALES, THRESHOLDS)

    mme = events[(events.model == "MME") & (events.start_thr == -1.0)]
    if not mme.empty:
        print("\nMME events at threshold -1.0, worst ten by severity:")
        worst = mme.nsmallest(10, "total_severity")[
            [
                "x", "y", "spi_scale", "start_year", "start_month",
                "duration_months", "peak_index", "total_severity",
                "severity_class", "forecast_fraction",
            ]
        ]
        print(worst.round(3).to_string(index=False))

    OUT_PARQUET.parent.mkdir(parents=True, exist_ok=True)
    events.drop(columns=["content"]).to_parquet(OUT_PARQUET, index=False)
    print(f"\nwrote {OUT_PARQUET}")


if __name__ == "__main__":
    main()
