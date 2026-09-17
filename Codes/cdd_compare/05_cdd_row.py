"""Step 4: the Consecutive Dry Days row, from the pulled runs.

Two things need care here. The daily catalog is not polygon-clipped, so it
carries 174 pixels against the indices' 154; we keep only the index pixels so
both halves of the figure describe the same Benin. And in a monsoon climate
the long runs are the dry season itself - a 237-day run is boreal winter, not
a drought - so the row is built from runs that fall inside the wet season,
where a dry spell is anomalous rather than expected.
"""
import numpy as np
import pandas as pd

RUNS = "Codes/cdd_compare/out/cdd_runs_benin.parquet"
SPI = "Codes/_archive/_data/spi_results.parquet"
OUT = "Codes/cdd_compare/out"

Y0, Y1 = 1970, 2023
NM = (Y1 - Y0 + 1) * 12
PIXEL_FRAC = 0.5
WET = (4, 9)           # April-September; Oct-Nov runs are the dry
                       # season onset (median 39-60 days), not drought


def build():
    runs = pd.read_parquet(RUNS)
    idx_px = (pd.read_parquet(SPI, columns=["x", "y"])
                .drop_duplicates())
    n_before = runs[["x", "y"]].drop_duplicates().shape[0]
    runs = runs.merge(idx_px, on=["x", "y"], how="inner")
    n_after = runs[["x", "y"]].drop_duplicates().shape[0]
    print(f"pixels: {n_before} in daily table -> {n_after} after "
          f"restricting to the index pixels")

    runs["run_start"] = pd.to_datetime(runs.run_start)
    runs["run_end"] = pd.to_datetime(runs.run_end)

    # seasonality check: where do the long runs sit?
    bym = runs.groupby(runs.run_start.dt.month).agg(
        n=("run_len", "size"), med=("run_len", "median"))
    print("\nruns by start month (n, median length):")
    for m, r in bym.iterrows():
        print(f"  {m:2d}  n={int(r.n):5d}  median={r.med:5.0f}")

    wet = runs[(runs.run_start.dt.month >= WET[0]) &
               (runs.run_start.dt.month <= WET[1])]
    print(f"\nwet-season runs: {len(wet):,} of {len(runs):,}"
          f"  median length {wet.run_len.median():.0f}"
          f"  max {wet.run_len.max()}")

    # A month is covered when a wet-season run overlaps it. Count each pixel
    # once per month - several runs can touch the same month, and summing them
    # would push the fraction above 1.
    n_pixels = n_after
    seen = [set() for _ in range(NM)]
    for r in wet.itertuples():
        a = (r.run_start.year - Y0) * 12 + r.run_start.month - 1
        b = (r.run_end.year - Y0) * 12 + r.run_end.month - 1
        a, b = max(a, 0), min(b, NM - 1)
        for m in range(a, b + 1):
            seen[m].add((r.x, r.y))
    pix = np.array([len(s) for s in seen], dtype=float)

    frac = pix / n_pixels
    mask = frac >= PIXEL_FRAC
    print(f"\nCDD  flagged months={int(mask.sum())}  peak pixel fraction={frac.max():.2f}")

    yr = pd.Series(mask).groupby(np.arange(NM) // 12).sum()
    top = yr[yr > 0].sort_values(ascending=False).head(10)
    print("     flagged months by year:",
          ", ".join(f"{Y0 + int(k)}:{int(v)}" for k, v in top.items()))

    np.savez(f"{OUT}/cdd_row.npz", mask=mask,
             intensity=np.clip(frac, 0, 1), pix_frac=frac)
    print("\nwrote", f"{OUT}/cdd_row.npz")


if __name__ == "__main__":
    build()
