"""Step 3: the Consecutive Dry Years row.

NOTE ON PROVENANCE. Consecutive_Dry_Years.R consumes an already-flagged
dry-year input produced by an upstream notebook that is not in this repo, so
the dry-year rule itself could not be reproduced. What the script does supply
is the run rule: runs of >= 3 dry years, with frequency counted as
floor(count / 3). That part is kept exactly.

The dry-year flag below is our reconstruction, built by analogy with the
consecutive-dry-months definition: a year is dry when its total precipitation
falls more than one standard deviation below the pixel's annual normal over
the reference period. This must be labelled as ours on any slide.
"""
import numpy as np
import pandas as pd

SPI = "Codes/_archive/_data/spi_results.parquet"
OUT = "Codes/cdd_compare/out"

REF_START, REF_END = 1973, 2002
PIXEL_FRAC = 0.5
MIN_RUN = 3          # from Consecutive_Dry_Years.R: filter(count >= 3)


def build():
    d = pd.read_parquet(SPI, columns=["x", "y", "year", "month", "monthly_precip"])
    d = d[(d.year >= 1970) & (d.year <= 2023)]

    # annual total per pixel
    ann = (d.groupby(["x", "y", "year"], as_index=False)
             .agg(annual_precip=("monthly_precip", "sum")))

    ref = ann[(ann.year >= REF_START) & (ann.year <= REF_END)]
    stats = (ref.groupby(["x", "y"], as_index=False)
                .agg(normal=("annual_precip", "mean"),
                     sd=("annual_precip", "std")))
    ann = ann.merge(stats, on=["x", "y"], how="left")
    ann["dry"] = ann.annual_precip < (ann.normal - ann.sd)

    years = np.arange(1970, 2024)
    n_pixels = ann[["x", "y"]].drop_duplicates().shape[0]
    pix = (ann.groupby("year").dry.sum()
              .reindex(years, fill_value=0).values)
    dry_year = pix >= PIXEL_FRAC * n_pixels

    # runs of consecutive dry years; only runs >= MIN_RUN count as an event
    run_len = np.zeros(len(years))
    i = 0
    while i < len(years):
        if dry_year[i]:
            j = i
            while j + 1 < len(years) and dry_year[j + 1]:
                j += 1
            if j - i + 1 >= MIN_RUN:
                run_len[i:j + 1] = j - i + 1
            i = j + 1
        else:
            i += 1

    flagged = years[dry_year]
    in_run = years[run_len > 0]
    print(f"CDY  pixels={n_pixels}  dry years={int(dry_year.sum())}"
          f"  years inside a >={MIN_RUN}-year run={int((run_len > 0).sum())}")
    print("     dry years:", ", ".join(str(y) for y in flagged))
    print("     inside runs:", ", ".join(str(y) for y in in_run) or "(none)")

    # expand to the monthly grid so the row shares the figure's x axis
    nmonths = len(years) * 12
    mask_m = np.repeat(run_len > 0, 12)
    runlen_m = np.repeat(run_len, 12)
    dryyear_m = np.repeat(dry_year, 12)

    np.savez(f"{OUT}/cdy_row.npz", mask=mask_m, run_len=runlen_m,
             dry_year=dryyear_m, pix_frac=np.repeat(pix / n_pixels, 12))
    print("\nwrote", f"{OUT}/cdy_row.npz")


if __name__ == "__main__":
    build()
