# =============================================================================
# 20_q6_pet_maps.py — Q6 PET-formulation maps (Benin vs Eritrea)
# =============================================================================
# Q6 question: Benin alone suggested PET formulation "doesn't matter" — SPI,
# SPEI-Thornthwaite and SPEI-Penman gave near-identical drought frequencies.
# Does that survive in an arid country where temperature dominates the water
# balance? Eritrea (166 pixels) is matched to Benin (154) so the comparison is
# not confounded by sample size.
#
# Holds scale and threshold fixed and varies ONLY the PET treatment:
#   SPI   = no PET at all
#   SPEI-T = Thornthwaite PET (temperature only)
#   SPEI-P = Penman PET (full physics, CRU TS 4.08)
#
# Run from the repo root:  py -3.13 Codes/agg_window/20_q6_pet_maps.py
#
# NOTE: these are pixel scatter plots, not cartographic maps — no coastline or
# national border is drawn, so country outlines are only implied by where the
# pixels fall. Do not read spatial pattern claims off them without an overlay.
# =============================================================================
import os
import pandas as pd
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt

Q6 = dict(
    THR=-1.0,                     # one of the WMO-standard thresholds
    Y0=1970, Y1=2023,             # detection record (index files run from 1950)
    YEARS=[1983, 1990, 2005, 2021],   # sample years for the per-year panels
    EVENTS={"Benin":   "Codes/_archive/_data/events.parquet",
            "Eritrea": "Codes/_archive/_data/events_eritrea.parquet"},
    FIGS="Codes/agg_window/out/figs",
)
Q6["NYR"] = Q6["Y1"] - Q6["Y0"] + 1

# The three PET treatments at a common 3-month accumulation. Selected on the
# `index` column, NOT config_id: config_id is country-specific (Benin uses the
# S*/EP* prefixes differently from Eritrea) and does not join across countries.
TREAT = [("spi3",     "SPI-3\n(no PET)"),
         ("spei3",    "SPEI-3 Thornthwaite\n(temperature PET)"),
         ("spei3_pm", "SPEI-3 Penman\n(full-physics PET)")]

COLS = ["x", "y", "index", "start_thr", "event_year"]


def load(path):
    d = pd.read_parquet(path, columns=COLS)
    return d[d.start_thr == Q6["THR"]]


def per_pixel(d, idx, year=None):
    """Event count per pixel, over the country's own pixel grid.

    Merging onto the full grid matters: a pixel with no events in the selected
    slice is a real zero, not a missing observation, and must be plotted as 0
    rather than silently dropped (which would bias every mean upward).
    """
    sub = d[d["index"] == idx]
    if year is not None:
        sub = sub[sub.event_year == year]
    grid = d[["x", "y"]].drop_duplicates()
    cnt = sub.groupby(["x", "y"]).size().rename("n").reset_index()
    return grid.merge(cnt, on=["x", "y"], how="left").fillna({"n": 0})


def fig_three_index(data, out):
    """Q6a: long-run mean event rate, 3 PET treatments x 2 countries."""
    panels = {(c, i): per_pixel(d, i) for c, d in data.items() for i, _ in TREAT}
    for p in panels.values():
        p["rate"] = p.n / Q6["NYR"]
    vmax = max(p.rate.max() for p in panels.values())

    fig, axes = plt.subplots(2, 3, figsize=(12.5, 8.4))
    for r, country in enumerate(data):
        for c, (idx, lab) in enumerate(TREAT):
            ax, g = axes[r, c], panels[(country, idx)]
            sc = ax.scatter(g.x, g.y, c=g.rate, s=70, marker="s",
                            cmap="YlOrRd", vmin=0, vmax=vmax)
            ax.set_aspect("equal")
            ax.set_title(f"{lab}\nmean {g.rate.mean():.3f}/px/yr", fontsize=9)
            if c == 0:
                ax.set_ylabel(f"{country} ({len(g)} px)", fontsize=11)
    fig.colorbar(sc, ax=axes, shrink=0.7, label="drought events per pixel per year")
    fig.suptitle("Q6 — does PET formulation matter? Same scale (3 mo), same "
                 f"threshold ({Q6['THR']}); only PET differs", y=0.97)
    fig.savefig(out, dpi=140, bbox_inches="tight")
    plt.close(fig)
    return panels


def fig_penman_years(data, out):
    """Q6b: SPEI-Penman only, a handful of individual years."""
    yrs = Q6["YEARS"]
    panels = {(c, y): per_pixel(d, "spei3_pm", y) for c, d in data.items() for y in yrs}
    vmax = max(p.n.max() for p in panels.values())

    fig, axes = plt.subplots(2, len(yrs), figsize=(3.6 * len(yrs), 8.0))
    for r, country in enumerate(data):
        for c, yr in enumerate(yrs):
            ax, g = axes[r, c], panels[(country, yr)]
            sc = ax.scatter(g.x, g.y, c=g.n, s=70, marker="s",
                            cmap="YlOrRd", vmin=0, vmax=vmax)
            ax.set_aspect("equal")
            ax.set_title(f"{yr}  (mean {g.n.mean():.2f})", fontsize=10)
            if c == 0:
                ax.set_ylabel(f"{country}\n({len(g)} px)   lat", fontsize=10)
            if r == 1:
                ax.set_xlabel("lon", fontsize=9)
    fig.colorbar(sc, ax=axes, shrink=0.7, label="drought events in year")
    fig.suptitle(f"Q6 — SPEI-3 Penman, threshold {Q6['THR']}: events per pixel "
                 "per year", y=0.96)
    fig.savefig(out, dpi=140, bbox_inches="tight")
    plt.close(fig)


if __name__ == "__main__":
    os.makedirs(Q6["FIGS"], exist_ok=True)
    data = {c: load(p) for c, p in Q6["EVENTS"].items()}

    panels = fig_three_index(data, f"{Q6['FIGS']}/Q6a_pet_treatments.png")
    fig_penman_years(data, f"{Q6['FIGS']}/Q6b_penman_years.png")

    # Headline table: the spread across PET treatments, by country. A wider
    # spread in Eritrea is the Q6 signal; watch the ORDERING too, not just the
    # magnitude — the treatments can reorder between climates.
    print(f"{'country':9} {'SPI-3':>8} {'SPEI-T':>8} {'SPEI-P':>8}   spread (max-min, % of SPI)")
    for country in data:
        v = [panels[(country, i)].rate.mean() for i, _ in TREAT]
        print(f"{country:9} {v[0]:8.3f} {v[1]:8.3f} {v[2]:8.3f}   "
              f"{max(v)-min(v):.3f}  ({100*(max(v)-min(v))/v[0]:.1f}%)")
    print(f"\nwrote {Q6['FIGS']}/Q6a_pet_treatments.png")
    print(f"wrote {Q6['FIGS']}/Q6b_penman_years.png")
