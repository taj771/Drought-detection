"""
A2 — splice observed ERA5 history onto the reconstructed NMME forecast.

A1 produces absolute rainfall for the forecast months only, which for the September
2026 issue means September 2026 through August 2027. SPI cannot be computed from that
alone: SPI-3 for September needs July, August and September, and two of those months
are already in the past. They have to come from observation.

Splicing observed months in front of a forecast is standard practice in operational
seasonal drought monitoring. Dutra (2014) describes the approach as "basically a
concatenation of the monitoring with the seasonal forecast of precipitation", which
is exactly what happens here -- no blending, no weighting, just one series where the
past is measured and the future is predicted.


n_forecast_months. Every accumulated SPI value mixes observed and forecast months in
a proportion that changes with lead. September's SPI-3 is two months of measured rain
and one month of prediction; by November it is three months of prediction. Those two
numbers sit in the same column and look identical, but one is largely a statement
about the past and the other is entirely a statement about the future. This script
tags each row with how many of its accumulation months are forecast, per scale, so
that nothing downstream has to infer it. A caller that ignores the tag gets a series
that is still correct, just harder to interpret honestly.

A consequence worth stating: with a 12-month forecast, SPI-12 never becomes a pure
forecast. Its first value is eleven parts observation to one part prediction. Long
accumulation periods are dominated by what already happened, which is a property of
the index rather than a flaw in the forecast.

Observed months are identical across models. The history did not depend on which
model is being evaluated, so the same observed rows are repeated for every model
series. That is deliberate duplication: it keeps each model's series independently
accumulable without a join, and the volume is trivial at 154 pixels.

Gaps and overlaps are errors, not something to paper over. If the observed record
stops before the forecast begins there is a hole in the middle of the series, and
accumulating across it would silently produce SPI values for windows that are
missing months. The script refuses rather than guessing.

Both gap checks run per pixel. Pooling every pixel into one max answers whether
SOME pixel reaches the forecast, which is not the question -- a single pixel whose
record stops early hides behind the rest, and A0b filters incomplete months per
pixel, so a ragged edge is the expected shape of failure rather than an exotic one.
Internal holes are checked too, by comparing each pixel's distinct month count
against the span it covers. A hole in the middle passes any seam check by
construction while still corrupting every window that straddles it, and because A4
drops NaN before run-length encoding, two events either side of a hole would merge
into one longer event that never happened.
"""

from pathlib import Path

import numpy as np
import pandas as pd

# -- Configuration -----------------------------------------------------------

COUNTRY = "Benin"

# Observed monthly precipitation, refreshed by A0b_refresh_era5.py. The research
# arm's export is NOT interchangeable here -- it stops in 2023 and would leave a
# multi-year hole before the forecast.
ERA5_PARQUET = Path("Codes/operational/data/era5_monthly_observed.parquet")

# Reconstructed forecast on the ERA5 grid, from A1_reconstruct.py.
FORECAST_PARQUET = Path("Codes/operational/out/nmme_absolute_era5grid.parquet")

OUT_PARQUET = Path("Codes/operational/out/spliced_series.parquet")

# Accumulation periods SPI will be computed over in A3. The longest one sets how
# much observed history has to be carried: SPI-12 needs eleven months before its
# first forecast value.
SCALES = [1, 3, 6, 12]

# How many observed months to carry ahead of the forecast. The arithmetic minimum
# is max(SCALES) - 1 = 11: enough for the FIRST forecast month's SPI-12 window and
# nothing more. At that minimum every observed month is still inside its own
# warm-up, so observed SPI-12 comes out entirely NaN and there is nothing to
# compare the forecast against. Carrying more history costs nothing -- the
# observed record now reaches 1950 -- and buys a stretch of observed SPI at every
# scale immediately before the forecast begins.
HISTORY_MONTHS = 36


def month_index(year, month):
    """Months since year 0, so consecutive calendar months differ by exactly 1.

    Comparing (year, month) pairs directly is awkward across a December boundary;
    a single integer makes gap and overlap checks trivial.
    """
    return year * 12 + (month - 1)


def load_observed(path, country):
    """Observed monthly precipitation for one country, newest month last."""
    if not path.exists():
        raise FileNotFoundError(
            f"{path} not found. Run A0b_refresh_era5.py on Databricks and copy the "
            f"output down before running this step."
        )

    observed = pd.read_parquet(path)
    if "country" in observed.columns:
        observed = observed[observed.country == country]
    if observed.empty:
        raise ValueError(f"no observed rows for {country} in {path}")

    observed = observed[["x", "y", "year", "month", "monthly_precip"]].rename(
        columns={"monthly_precip": "precip_mm"}
    )
    observed["source"] = "observed"
    return observed.sort_values(["x", "y", "year", "month"]).reset_index(drop=True)


def load_forecast(path):
    """Reconstructed forecast, one series per model."""
    forecast = pd.read_parquet(path)
    keep = ["x", "y", "model", "issue", "lead", "year", "month", "precip_mm"]
    forecast = forecast[keep].copy()
    forecast["source"] = "forecast"
    return forecast


def check_join(observed, forecast):
    """Verify the two records meet cleanly, and report how they line up.

    Three things can go wrong and all three are silent if unchecked: a gap between
    the records, an overlap, or a mismatched pixel set. Each would produce SPI that
    looks plausible and is wrong.
    """
    # Take the extremes of the month index itself. Using year.max() and month.max()
    # separately is wrong and quietly so: a record ending 2026-08 but containing an
    # earlier December reports a maximum of 2026-12, which hides a gap.
    observed_t = month_index(observed.year, observed.month)
    forecast_t = month_index(forecast.year, forecast.month)
    first_forecast = int(forecast_t.min())

    # Per pixel, not pooled. A single max over every pixel at once answers "does
    # SOME pixel reach the forecast", which is not the question: one pixel whose
    # record stops early is hidden behind all the others, and A0b's completeness
    # filter drops months per pixel, so a ragged edge is exactly the shape of
    # failure to expect here. Each pixel is accumulated independently downstream
    # and so each one has to meet the forecast on its own.
    last_per_pixel = (
        observed.assign(t=observed_t).groupby(["x", "y"])["t"].max()
    )
    short = last_per_pixel[last_per_pixel < first_forecast - 1]
    if not short.empty:
        worst_t = int(short.min())
        worst_y, worst_m = divmod(worst_t, 12)
        first_y, first_m = divmod(first_forecast, 12)
        raise ValueError(
            f"{len(short)} pixel(s) have observed history ending before the forecast "
            f"begins; the earliest stops at {worst_y}-{worst_m + 1:02d} against a "
            f"forecast starting {first_y}-{first_m + 1:02d}. Accumulating across the "
            f"hole would produce SPI for windows that are missing months. Refresh the "
            f"observed record with A0b_refresh_era5.py."
        )

    # An internal hole is as damaging as a short tail and less visible: the record
    # reaches the forecast, so the seam check passes, but a window spanning the hole
    # sums fewer months than it claims. A4 drops NaN before run-length encoding, so
    # two separate events either side of a hole would merge into one longer event.
    # Count distinct months against the span they cover, per pixel.
    spans = (
        observed.assign(t=observed_t)
        .groupby(["x", "y"])["t"]
        .agg(["min", "max", "nunique"])
    )
    holed = spans[spans["nunique"] != spans["max"] - spans["min"] + 1]
    if not holed.empty:
        worst = holed.assign(
            missing=holed["max"] - holed["min"] + 1 - holed["nunique"]
        ).sort_values("missing", ascending=False)
        top = worst.iloc[0]
        raise ValueError(
            f"{len(holed)} pixel(s) have gaps inside their observed record; the worst "
            f"is missing {int(top.missing)} month(s) of the {int(top['max'] - top['min'] + 1)} "
            f"between its first and last. Accumulation would span the hole silently "
            f"and A4 would merge events across it."
        )

    last_observed = int(last_per_pixel.max())
    gap = first_forecast - last_observed

    overlap = 0
    if gap <= 0:
        overlap = 1 - gap

    observed_pixels = set(map(tuple, observed[["x", "y"]].drop_duplicates().values))
    forecast_pixels = set(map(tuple, forecast[["x", "y"]].drop_duplicates().values))
    if observed_pixels != forecast_pixels:
        only_observed = len(observed_pixels - forecast_pixels)
        only_forecast = len(forecast_pixels - observed_pixels)
        raise ValueError(
            f"pixel sets differ: {only_observed} only in observed, {only_forecast} "
            f"only in forecast. Both must come from the same country clip, or frozen "
            f"parameters would be applied to the wrong locations."
        )

    return overlap, len(observed_pixels)


def splice(observed, forecast, overlap):
    """Concatenate observed and forecast into one series per (pixel, model).

    Where the two records overlap the forecast is dropped, not the observation:
    a month that has already been measured is not improved by predicting it.
    """
    first_forecast = int(month_index(forecast.year, forecast.month).min())

    if overlap:
        observed = observed[
            month_index(observed.year, observed.month) < first_forecast
        ].copy()

    models = sorted(forecast.model.unique())
    issue = forecast.issue.iloc[0]

    # The observed record is the same for every model, so repeat it per series.
    repeated = []
    for model in models:
        block = observed.copy()
        block["model"] = model
        block["issue"] = issue
        block["lead"] = -1  # not a forecast month
        repeated.append(block)

    history = pd.concat(repeated, ignore_index=True)
    combined = pd.concat([history, forecast], ignore_index=True)

    combined["t"] = month_index(combined.year, combined.month)
    return combined.sort_values(["x", "y", "model", "t"]).reset_index(drop=True)


def tag_forecast_fraction(series, scales):
    """Count how many months of each accumulation window are forecast.

    Done as a rolling sum over an indicator rather than from the lead number, so it
    stays correct if the series ever starts part way into a forecast or carries a
    gap that an earlier check allowed.

    The first scale-1 rows of each series have no complete window; their count is
    left as NaN to match the NaN that SPI itself will carry there.
    """
    series = series.copy()
    is_forecast = (series.source == "forecast").astype(float)
    grouped = is_forecast.groupby([series.x, series.y, series.model])

    for scale in scales:
        column = f"n_forecast_{scale}"
        if scale == 1:
            series[column] = is_forecast
        else:
            series[column] = grouped.transform(
                lambda values, w=scale: values.rolling(w).sum()
            )

    return series


def main():
    observed = load_observed(ERA5_PARQUET, COUNTRY)
    forecast = load_forecast(FORECAST_PARQUET)

    def span(frame):
        t = month_index(frame.year, frame.month)
        lo, hi = divmod(int(t.min()), 12), divmod(int(t.max()), 12)
        return f"{lo[0]}-{lo[1] + 1:02d} to {hi[0]}-{hi[1] + 1:02d}"

    print(f"observed: {len(observed):,} rows, {span(observed)}")
    print(
        f"forecast: {len(forecast):,} rows, issue {forecast.issue.iloc[0]}, "
        f"{forecast.model.nunique()} series, {span(forecast)}"
    )

    overlap, n_pixels = check_join(observed, forecast)
    print(f"pixels: {n_pixels} (identical in both records)")
    if overlap:
        print(f"overlap of {overlap} month(s): observed kept, forecast dropped")
    else:
        print("records meet with no gap and no overlap")

    combined = splice(observed, forecast, overlap)
    combined = tag_forecast_fraction(combined, SCALES)

    # Trim to the recent tail. Nothing before HISTORY_MONTHS affects any window
    # that contains a forecast month, and keeping the full record would carry
    # seven decades per pixel per model for no gain.
    first_forecast = int(month_index(forecast.year, forecast.month).min())
    combined = combined[combined.t >= first_forecast - HISTORY_MONTHS].copy()

    per_series = combined.groupby(["x", "y", "model"]).size()
    print(
        f"\nspliced: {len(combined):,} rows, {per_series.iloc[0]} months per series "
        f"({int((combined.source == 'observed').sum() / combined.model.nunique())} "
        f"observed + {int((combined.source == 'forecast').sum() / combined.model.nunique())} "
        f"forecast per series)"
    )

    mme = combined[combined.model == "MME"]
    one_pixel = mme[(mme.x == mme.x.iloc[0]) & (mme.y == mme.y.iloc[0])]
    print("\nforecast months per accumulation window (one pixel, MME):")
    preview = one_pixel[
        ["year", "month", "source", "precip_mm"] + [f"n_forecast_{s}" for s in SCALES]
    ].tail(14)
    print(preview.to_string(index=False))

    OUT_PARQUET.parent.mkdir(parents=True, exist_ok=True)
    columns = [
        "x", "y", "model", "issue", "year", "month", "t", "lead",
        "precip_mm", "source",
    ] + [f"n_forecast_{s}" for s in SCALES]
    combined[columns].to_parquet(OUT_PARQUET, index=False)
    print(f"\nwrote {OUT_PARQUET}")


if __name__ == "__main__":
    main()
