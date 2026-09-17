"""The metric-comparison ribbon: GWSC's counting rules against event detection.

Five rows share one monthly axis for Benin. The top block holds the existing
GWSC metrics, which flag that a dry spell happened; the bottom block holds the
event-based indices, which also say how bad it was - so only those rows carry
ranked boxes. The CDD row stays empty until the daily precipitation pull runs.
"""
import numpy as np
import pandas as pd
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib.patches import Rectangle

OUT = "Codes/cdd_compare/out"
Y0, Y1 = 1970, 2023
NM = (Y1 - Y0 + 1) * 12

INK = "#1f2d34"; MUTED = "#8a97a0"; GRID = "#dfe5e9"
# rank colours follow the Q5 ribbon so the two figures read as siblings
RANK_C = {1: "#d62728", 2: "#9467bd", 3: "#1f77b4", 4: "#ff7f0e", 5: "#2ca02c"}

ROWS = [
    ("cdd",      "CDD",       "#5b6b73", "counting"),
    ("cdm",      "CDM",       "#5b6b73", "counting"),
    ("spi3",     "SPI-3",     "#1f5673", "event"),
    ("spei3",    "SPEI-T-3",  "#b8860b", "event"),
    ("spei3_pm", "SPEI-P-3",  "#2f7d5d", "event"),
]


def load():
    idx = np.load(f"{OUT}/index_rows.npz")
    cdm = np.load(f"{OUT}/cdm_row.npz")
    ranked = pd.read_parquet(f"{OUT}/index_ranked.parquet")
    data = {}
    for key, _, _, block in ROWS:
        if key == "cdd":
            try:
                z = np.load(f"{OUT}/cdd_row.npz")
                data[key] = (z["mask"], z["intensity"])
            except FileNotFoundError:
                data[key] = (None, None)          # pending the daily pull
        elif key == "cdm":
            rl = cdm["run_len"]
            data[key] = (cdm["mask"], rl / max(rl.max(), 1))
        else:
            sev = np.abs(idx[f"sev_{key}"])
            m = idx[f"mask_{key}"]
            data[key] = (m, sev / max(sev.max(), 1))
    return data, ranked


def draw():
    data, ranked = load()
    fig, ax = plt.subplots(figsize=(15.5, 6.4))

    row_h, gap = 0.62, 1.0
    ytick, ylab = [], []

    for i, (key, label, color, block) in enumerate(ROWS):
        y = (len(ROWS) - 1 - i) * gap
        ytick.append(y + row_h / 2); ylab.append(label)
        mask, inten = data[key]

        # the empty frame every row shares, so a blank row reads as empty
        # rather than as missing
        ax.add_patch(Rectangle((0, y), NM, row_h, facecolor="#f7f9fa",
                               edgecolor=GRID, linewidth=0.6, zorder=1))

        if mask is None:
            ax.text(NM / 2, y + row_h / 2, "awaiting daily precipitation pull",
                    ha="center", va="center", fontsize=9, color=MUTED,
                    style="italic", zorder=3)
            continue

        rgba = matplotlib.colors.to_rgba(color)
        for m in np.flatnonzero(mask):
            a = 0.42 + 0.58 * inten[m]        # shade by intensity / run length;
            # the floor keeps a single dry month readable next to a long run
            ax.add_patch(Rectangle((m, y), 1, row_h, linewidth=0,
                                   facecolor=(*rgba[:3], a), zorder=2))


    # the divider that carries the argument: counting rules above, detection below
    # midway between the CDM row's floor and the SPI row's ceiling
    ydiv = (len(ROWS) - 3) * gap + row_h + 0.19
    ax.axhline(ydiv, color=INK, linewidth=1.0, zorder=6)
    ax.text(-46, ydiv + 0.52, "GWSC\ncounting rules", fontsize=9.5,
            color=INK, va="center", ha="right", linespacing=1.35)
    ax.text(-46, ydiv - 1.30, "event-based\ndetection", fontsize=9.5,
            color=INK, va="center", ha="right", linespacing=1.35,
            fontweight="600")

    ticks = [(y - Y0) * 12 for y in range(1970, 2024, 5)]
    ax.set_xticks(ticks); ax.set_xticklabels(range(1970, 2024, 5), fontsize=10)
    ax.set_yticks(ytick); ax.set_yticklabels(ylab, fontsize=11)
    ax.set_xlim(-2, NM + 2); ax.set_ylim(-0.35, len(ROWS) * gap + 0.15)
    for s in ("top", "right", "left"):
        ax.spines[s].set_visible(False)
    ax.spines["bottom"].set_color(MUTED)
    ax.tick_params(axis="y", length=0)

    fig.suptitle("Benin: what each drought definition flags, on one timeline",
                 fontsize=15, fontweight="bold", color=INK, x=0.055, ha="left",
                 y=0.975)
    fig.text(0.055, 0.915,
             "A month is flagged when at least half of Benin's 154 pixels are. "
             "Shading is run length for the counting rules and severity for "
             "the indices.",
             fontsize=10, color=MUTED, ha="left")
    fig.savefig(f"{OUT}/figs/ribbon_metrics.png", dpi=150,
                bbox_inches="tight", facecolor="white")
    print("wrote", f"{OUT}/figs/ribbon_metrics.png")


if __name__ == "__main__":
    import os
    os.makedirs(f"{OUT}/figs", exist_ok=True)
    draw()
