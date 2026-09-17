"""Step 2: the Consecutive Dry Months row, GWSC's definition.

XX_Consecutive_Dry_Months.Rmd calls a month dry when its precipitation falls
more than one standard deviation below the normal for that calendar month at
that pixel, then counts runs of such months. The reference window there is
2000-2020; this uses the project's own 1973-2002 reference period instead, so
the row is calibrated on the same baseline as SPI/SPEI and the comparison is
not confounded by a different climatology.
"""
import numpy as np
import pandas as pd

SPI = "Codes/_archive/_data/spi_results.parquet"
OUT = "Codes/cdd_compare/out"

REF_START, REF_END = 1973, 2002
PIXEL_FRAC = 0.5     # same country rule as the index rows
MIN_RUN = 1          # a run of >= 1 dry month; the figure shades run length


def build():
    d = pd.read_parquet(SPI, columns=["x", "y", "year", "month", "monthly_precip"])
    d = d[(d.year >= 1970) & (d.year <= 2023)].copy()

    # normal and SD per pixel per calendar month, from the reference period
    ref = d[(d.year >= REF_START) & (d.year <= REF_END)]
    stats = (ref.groupby(["x", "y", "month"], as_index=False)
                .agg(normal=("monthly_precip", "mean"),
                     sd=("monthly_precip", "std")))
    d = d.merge(stats, on=["x", "y", "month"], how="left")

    d["dry"] = d.monthly_precip < (d.normal - d.sd)
    d["mi"] = (d.year - 1970) * 12 + (d.month - 1)

    nmonths = (2023 - 1970 + 1) * 12
    n_pixels = d[["x", "y"]].drop_duplicates().shape[0]

    # how many pixels are in a dry month, per month
    pix = d.groupby("mi").dry.sum().reindex(range(nmonths), fill_value=0).values
    mask = pix >= PIXEL_FRAC * n_pixels

    # run length of the country-level dry spell, for shading
    run_len = np.zeros(nmonths)
    i = 0
    while i < nmonths:
        if mask[i]:
            j = i
            while j + 1 < nmonths and mask[j + 1]:
                j += 1
            run_len[i:j + 1] = j - i + 1
            i = j + 1
        else:
            i += 1

    runs = [int(r) for r in np.unique(run_len) if r >= MIN_RUN]
    print(f"CDM  pixels={n_pixels}  dry months={int(mask.sum())}"
          f"  longest run={int(run_len.max())} months")
    print(f"     run lengths present: {runs}")
    print(f"     pixel-fraction peak: {pix.max() / n_pixels:.2f}")

    # which years carry the most dry months, as a sanity read
    yr = pd.Series(mask).groupby(np.arange(nmonths) // 12).sum()
    top = yr[yr > 0].sort_values(ascending=False).head(8)
    print("     dry months by year:",
          ", ".join(f"{1970 + int(k)}:{int(v)}" for k, v in top.items()))

    np.savez(f"{OUT}/cdm_row.npz", mask=mask, run_len=run_len,
             pix_frac=pix / n_pixels)
    print("\nwrote", f"{OUT}/cdm_row.npz")


if __name__ == "__main__":
    build()
