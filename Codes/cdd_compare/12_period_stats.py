"""Step 12: the headline numbers each row's own tradition would report.

The ribbon shows five rows side by side, but the five are not read the same
way. GWSC reports CDD as an average number of dry days per year and CDM as
dry months per year; our own rows are read as frequency, duration and
severity of events. The point of the info boxes at the end of each ribbon is
to make that difference concrete: same country, same years, five different
answers to "how much drought was there".

Each number is split into a reference window and everything else, so the
slide can say whether the metric moved. The reference window is the pipeline's
own calibration period, 2000-2020. Note that CDM's internal dry-month test is
still calibrated on 1973-2002, as in step 2 - that is its climatology, not
the window used here for reporting.
"""
import json

import numpy as np
import pandas as pd

EVENTS = "Codes/_archive/_data/events.parquet"
CDD_RUNS = "Codes/cdd_compare/out/cdd_runs_benin.parquet"
SPI = "Codes/_archive/_data/spi_results.parquet"
OUT = "Codes/cdd_compare/out/period_stats.json"

Y0, Y1 = 1975, 2023          # the ribbon's clipped extent
REF_START, REF_END = 2000, 2020
CDM_REF_START, CDM_REF_END = 1973, 2002
THR = -1.0
FAMILIES = [("spi3", "SPI-3"), ("spei3", "SPEI-T-3"), ("spei3_pm", "SPEI-P-3")]


def split_years(years):
    """Boolean masks for the reference window and everything outside it."""
    ref = (years >= REF_START) & (years <= REF_END)
    return ref, ~ref


def n_years(mask_years):
    return int(np.unique(mask_years).size)


def cdd_stats():
    """Average consecutive-dry-days per pixel-year, GWSC's reading of CDD."""
    d = pd.read_parquet(CDD_RUNS)
    d = d[(d.start_year >= Y0) & (d.start_year <= Y1)]
    n_pixels = d[["x", "y"]].drop_duplicates().shape[0]

    out = {}
    for label, sel in (("ref", (d.start_year >= REF_START) & (d.start_year <= REF_END)),
                       ("obs", ~((d.start_year >= REF_START) & (d.start_year <= REF_END)))):
        sub = d[sel]
        yrs = n_years(sub.start_year.values)
        out[label] = {
            "years": yrs,
            # dry days per pixel per year: total run length over pixel-years
            "days_per_year": float(sub.run_len.sum() / (n_pixels * yrs)),
            "runs_per_year": float(len(sub) / (n_pixels * yrs)),
            "mean_run_len": float(sub.run_len.mean()),
        }
    return out


def cdm_stats():
    """Dry months per pixel-year on GWSC's one-SD-below-normal rule."""
    d = pd.read_parquet(SPI, columns=["x", "y", "year", "month", "monthly_precip"])
    d = d[(d.year >= 1970) & (d.year <= Y1)].copy()

    ref = d[(d.year >= CDM_REF_START) & (d.year <= CDM_REF_END)]
    stats = (ref.groupby(["x", "y", "month"], as_index=False)
                .agg(normal=("monthly_precip", "mean"),
                     sd=("monthly_precip", "std")))
    d = d.merge(stats, on=["x", "y", "month"], how="left")
    d["dry"] = d.monthly_precip < (d.normal - d.sd)

    # a spell is a run of consecutive dry months at one pixel; counting spells
    # rather than months is what makes CDM's number comparable to the event
    # frequency we report for SPI/SPEI - both then mean "how many times"
    d = d.sort_values(["x", "y", "year", "month"])
    d["mi"] = d.year * 12 + d.month
    prev = d.groupby(["x", "y"]).dry.shift(fill_value=False)
    gap = d.groupby(["x", "y"]).mi.diff() != 1
    d["spell_start"] = d.dry & (~prev | gap)

    d = d[d.year >= Y0]
    n_pixels = d[["x", "y"]].drop_duplicates().shape[0]

    out = {}
    in_ref = (d.year >= REF_START) & (d.year <= REF_END)
    for label, sel in (("ref", in_ref), ("obs", ~in_ref)):
        sub = d[sel]
        yrs = n_years(sub.year.values)
        spells = float(sub.spell_start.sum())
        out[label] = {
            "years": yrs,
            "months_per_year": float(sub.dry.sum() / (n_pixels * yrs)),
            "spells_per_year": spells / (n_pixels * yrs),
            # mean spell length, in months, from the same two counts
            "mean_spell_months": float(sub.dry.sum() / spells) if spells else 0.0,
        }
    return out


def index_stats():
    """Frequency, duration and severity per pixel-year for each index family."""
    ev = pd.read_parquet(EVENTS)
    ev = ev[(ev.start_thr == THR) & (ev.method == "threshold")].copy()
    ev["start_year"] = pd.to_datetime(ev.start_date).dt.year
    ev = ev[(ev.start_year >= Y0) & (ev.start_year <= Y1)]

    out = {}
    for key, label in FAMILIES:
        sub = ev[ev["index"] == key]
        n_pixels = sub[["x", "y"]].drop_duplicates().shape[0]
        in_ref = (sub.start_year >= REF_START) & (sub.start_year <= REF_END)

        row = {}
        for name, sel in (("ref", in_ref), ("obs", ~in_ref)):
            s = sub[sel]
            yrs = n_years(s.start_year.values)
            row[name] = {
                "years": yrs,
                "events_per_year": float(len(s) / (n_pixels * yrs)),
                "mean_duration_months": float(s.duration_months.mean()),
                "mean_severity": float(np.abs(s.total_severity).mean()),
            }
        out[label] = row
    return out


def build():
    res = {
        "ref_window": [REF_START, REF_END],
        "ribbon_window": [Y0, Y1],
        "CDD": cdd_stats(),
        "CDM": cdm_stats(),
        "indices": index_stats(),
    }
    with open(OUT, "w") as fh:
        json.dump(res, fh, indent=2)

    print(f"reference window {REF_START}-{REF_END}, rest is 'obs'\n")
    c = res["CDD"]
    print(f"CDD  ref {c['ref']['days_per_year']:6.1f} dry days/yr"
          f"   obs {c['obs']['days_per_year']:6.1f}"
          f"   (mean run {c['ref']['mean_run_len']:.1f} vs {c['obs']['mean_run_len']:.1f} d)")
    m = res["CDM"]
    print(f"CDD  ref {c['ref']['runs_per_year']:6.2f} spells/yr"
          f"   obs {c['obs']['runs_per_year']:6.2f}")
    print(f"CDM  ref {m['ref']['spells_per_year']:6.2f} spells/yr"
          f"   obs {m['obs']['spells_per_year']:6.2f}"
          f"   ({m['ref']['mean_spell_months']:.1f} vs "
          f"{m['obs']['mean_spell_months']:.1f} mo,"
          f" {m['ref']['months_per_year']:.2f} vs "
          f"{m['obs']['months_per_year']:.2f} months/yr)")
    for label, row in res["indices"].items():
        r, o = row["ref"], row["obs"]
        print(f"{label:9s} ref {r['events_per_year']:.2f} ev/yr"
              f"  {r['mean_duration_months']:.1f} mo  sev {r['mean_severity']:.2f}"
              f"   |  obs {o['events_per_year']:.2f} ev/yr"
              f"  {o['mean_duration_months']:.1f} mo  sev {o['mean_severity']:.2f}")
    print("\nwrote", OUT)


if __name__ == "__main__":
    build()
