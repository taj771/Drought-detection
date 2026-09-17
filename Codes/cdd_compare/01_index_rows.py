"""Step 1 of the metric-comparison ribbon: the three event-based rows.

Builds a monthly country-level drought mask for SPI-3, SPEI-T-3 and SPEI-P-3
from the detected events, using the Q5 convention that a month counts as
drought for the country when at least half its pixels are in an event.
Also ranks each family's worst episodes so the figure can box the top five.
"""
import numpy as np
import pandas as pd

EVENTS = "Codes/_archive/_data/events.parquet"
OUT = "Codes/cdd_compare/out"

THR = -1.0          # WMO moderate; the threshold Q5 leaned on
PIXEL_FRAC = 0.5    # a month is a country drought when >= half the pixels are
TOP_N = 5

# the three families at 3-month accumulation, in the order they stack
FAMILIES = [("spi3", "SPI-3"), ("spei3", "SPEI-T-3"), ("spei3_pm", "SPEI-P-3")]


def month_index(dates):
    """Months since 1970-01 - the figure's x coordinate."""
    d = pd.to_datetime(dates)
    return (d.dt.year - 1970) * 12 + (d.dt.month - 1)


def build():
    ev = pd.read_parquet(EVENTS)
    ev = ev[(ev.start_thr == THR) & (ev.method == "threshold")]

    npix = ev.groupby("index").apply(
        lambda g: g[["x", "y"]].drop_duplicates().shape[0], include_groups=False)
    print("pixels per index:\n", npix, "\n")

    nmonths = (2023 - 1970 + 1) * 12
    masks, severity, ranked = {}, {}, {}

    for key, label in FAMILIES:
        sub = ev[ev["index"] == key]
        n_pixels = sub[["x", "y"]].drop_duplicates().shape[0]

        # count pixels in drought per month, and sum their severity
        pix_count = np.zeros(nmonths)
        sev_sum = np.zeros(nmonths)
        s = month_index(sub.start_date).values
        e = month_index(sub.end_date).values
        # severity is per event; spread it evenly over the months it spans
        per_month = sub.total_severity.values / np.maximum(sub.duration_months.values, 1)
        for a, b, v in zip(s, e, per_month):
            a, b = max(a, 0), min(b, nmonths - 1)
            if b >= a:
                pix_count[a:b + 1] += 1
                sev_sum[a:b + 1] += v

        mask = pix_count >= PIXEL_FRAC * n_pixels
        masks[key] = mask
        severity[key] = sev_sum

        # consecutive drought months form one episode; rank on summed severity
        eps, run = [], None
        for i in range(nmonths):
            if mask[i] and run is None:
                run = i
            elif not mask[i] and run is not None:
                eps.append((run, i - 1)); run = None
        if run is not None:
            eps.append((run, nmonths - 1))
        epdf = pd.DataFrame(eps, columns=["m0", "m1"]).astype({"m0": int, "m1": int})
        if len(epdf):
            epdf["severity"] = [abs(sev_sum[a:b + 1]).sum() for a, b in eps]
            epdf = epdf.sort_values("severity", ascending=False).head(TOP_N)
            epdf["rank"] = range(1, len(epdf) + 1)
            epdf["year"] = 1970 + epdf.m0 // 12
        ranked[key] = epdf

        print(f"{label:10s} pixels={n_pixels:4d}  drought months={mask.sum():4d}"
              f"  episodes={len(eps):3d}")
        if len(epdf):
            print("   top 5:", ", ".join(
                f"#{int(r['rank'])} {1970 + int(r.m0) // 12}-{int(r.m0) % 12 + 1:02d}"
                for _, r in epdf.iterrows()))

    np.savez(f"{OUT}/index_rows.npz",
             **{f"mask_{k}": masks[k] for k, _ in FAMILIES},
             **{f"sev_{k}": severity[k] for k, _ in FAMILIES})
    pd.concat([r.assign(index=k) for k, r in ranked.items()]).to_parquet(
        f"{OUT}/index_ranked.parquet")
    print("\nwrote", OUT)


if __name__ == "__main__":
    build()
