"""Step 1b: the event-based rows, without the 50% country threshold.

The first version collapsed each month to a yes/no country flag at 50% of
pixels, which threw away spatial extent and then put a version of it back as
shading. This keeps the two apart, so the figure can carry one meaning per
visual channel:

    shade  = fraction of Benin's pixels in an event that month (all rows)
    height = mean severity among the pixels actually in the event

Severity is averaged over affected pixels rather than summed over all of them.
Summing would make the two channels move together - a small intense drought
would look mild only because few pixels were in it - and the point of the
figure is that extent and severity are different questions.
"""
import numpy as np
import pandas as pd

EVENTS = "Codes/_archive/_data/events.parquet"
OUT = "Codes/cdd_compare/out"

THR = -1.0
Y0, Y1 = 1970, 2023
NM = (Y1 - Y0 + 1) * 12

FAMILIES = [("spi3", "SPI-3"), ("spei3", "SPEI-T-3"), ("spei3_pm", "SPEI-P-3")]


def month_index(dates):
    d = pd.to_datetime(dates)
    return (d.dt.year - Y0) * 12 + (d.dt.month - 1)


def build():
    ev = pd.read_parquet(EVENTS)
    ev = ev[(ev.start_thr == THR) & (ev.method == "threshold")]

    out = {}
    for key, label in FAMILIES:
        sub = ev[ev["index"] == key]
        n_pixels = sub[["x", "y"]].drop_duplicates().shape[0]

        pix_count = np.zeros(NM)      # pixels in an event this month
        sev_sum = np.zeros(NM)        # their severity, summed
        ev_count = np.zeros(NM)       # events overlapping this month

        s = month_index(sub.start_date).values
        e = month_index(sub.end_date).values
        per_month = sub.total_severity.values / np.maximum(
            sub.duration_months.values, 1)

        for a, b, v in zip(s, e, per_month):
            a, b = max(a, 0), min(b, NM - 1)
            if b >= a:
                pix_count[a:b + 1] += 1
                sev_sum[a:b + 1] += v
                ev_count[a:b + 1] += 1

        frac = pix_count / n_pixels
        # mean severity among affected pixels; zero where none are affected
        sev_mean = np.divide(sev_sum, pix_count,
                             out=np.zeros(NM), where=pix_count > 0)

        out[f"frac_{key}"] = frac
        out[f"sev_{key}"] = np.abs(sev_mean)
        out[f"nev_{key}"] = ev_count

        print(f"{label:10s} pixels={n_pixels:4d}"
              f"  months with any pixel affected={int((frac > 0).sum()):4d}"
              f"  peak extent={frac.max():.2f}"
              f"  peak mean severity={np.abs(sev_mean).max():.2f}")

    np.savez(f"{OUT}/index_rows_cont.npz", **out)
    print("\nwrote", f"{OUT}/index_rows_cont.npz")


if __name__ == "__main__":
    build()
