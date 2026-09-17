"""The metric-comparison figure, second design.

One visual channel per question, held constant down the whole figure:

    position  when
    shade     how much of Benin (fraction of the 154 pixels)
    height    how severe (mean severity among affected pixels)

The counting rows have no height because CDD and CDM define no magnitude - a
dry spell either happened or it did not. That empty axis is the argument the
figure exists to make, so the rows are drawn as full-height presence bands
rather than as bars of zero height, which would read as "mild" instead of
"not defined".

No country threshold anywhere. The earlier version flagged a month when half
the pixels agreed, which lost droughts that sat in the north - Benin's dry
half - and never crossed 50%.
"""
import json

import numpy as np
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib.patches import Rectangle

OUT = "Codes/cdd_compare/out"
# the stored rows all run from 1970, but the daily table behind CDD only
# starts in 1975. Trimming every row to 1975 keeps the five comparable; a
# blank stretch on one row alone reads as "no dry spells" rather than
# "no data".
DATA_Y0 = 1970
Y0, Y1 = 1975, 2023
SKIP = (Y0 - DATA_Y0) * 12
NM = (Y1 - Y0 + 1) * 12

# how much of a row's band the tallest severity bar fills
BAR_FILL = 0.86
# palest a visible strip may be, so a 5% extent is still findable
SHADE_FLOOR = 0.18


def load():
    cdd = np.load(f"{OUT}/cdd_row.npz")
    cdm = np.load(f"{OUT}/cdm_row.npz")
    idx = np.load(f"{OUT}/index_rows_cont.npz")

    def cut(a):
        return np.asarray(a)[SKIP:SKIP + NM]

    rows = [
        dict(label="CDD", sub="dry spells, 7+ days", kind="count",
             frac=cut(cdd["pix_frac"]), sev=None, colour="#8c6d4f"),
        dict(label="CDM", sub="dry months, below normal - 1 SD", kind="count",
             frac=cut(cdm["pix_frac"]), sev=None, colour="#8c6d4f"),
        dict(label="SPI-3", sub="events, severity accumulated", kind="event",
             frac=cut(idx["frac_spi3"]), sev=cut(idx["sev_spi3"]), colour="#1f5c8b"),
        dict(label="SPEI-T-3", sub="events, Thornthwaite PET", kind="event",
             frac=cut(idx["frac_spei3"]), sev=cut(idx["sev_spei3"]), colour="#1f5c8b"),
        dict(label="SPEI-P-3", sub="events, Penman PET", kind="event",
             frac=cut(idx["frac_spei3_pm"]), sev=cut(idx["sev_spei3_pm"]),
             colour="#1f5c8b"),
    ]
    return rows


# the info boxes at the right edge: how wide they are, in month units, and
# how far off the last month they start
BOX_GAP, BOX_W = 26, 150


def box_lines():
    """The headline number each row's own tradition reports, ref vs rest.

    Step 12 writes these; the figure only formats them. Each row gets the
    quantity its own community would quote, which is the whole point - the
    five rows do not answer the same question, and the boxes say so in
    numbers rather than in a caption.
    """
    st = json.load(open(f"{OUT}/period_stats.json"))
    r0, r1 = st["ref_window"]

    def pair(ref, obs, fmt):
        return [f"{r0}-{r1}   {fmt.format(ref)}", f"other yrs   {fmt.format(obs)}"]

    c, m = st["CDD"], st["CDM"]
    # count first, then length, so every row reads the same way: how often,
    # then how long. That is what makes the GWSC rows and ours comparable at
    # a glance - the counts mean the same thing, the units after them do not.
    out = {
        "CDD": ("spells / mean length", [
            f"{r0}-{r1}   {c['ref']['runs_per_year']:.2f}/yr  "
            f"{c['ref']['mean_run_len']:.0f} d",
            f"other yrs   {c['obs']['runs_per_year']:.2f}/yr  "
            f"{c['obs']['mean_run_len']:.0f} d",
        ]),
        "CDM": ("spells / mean length", [
            f"{r0}-{r1}   {m['ref']['spells_per_year']:.2f}/yr  "
            f"{m['ref']['mean_spell_months']:.1f} mo",
            f"other yrs   {m['obs']['spells_per_year']:.2f}/yr  "
            f"{m['obs']['mean_spell_months']:.1f} mo",
        ]),
    }
    for label, row in st["indices"].items():
        ref, obs = row["ref"], row["obs"]
        out[label] = ("events / duration / severity", [
            f"{r0}-{r1}   {ref['events_per_year']:.2f}/yr  "
            f"{ref['mean_duration_months']:.1f} mo  {ref['mean_severity']:.2f}",
            f"other yrs   {obs['events_per_year']:.2f}/yr  "
            f"{obs['mean_duration_months']:.1f} mo  {obs['mean_severity']:.2f}",
        ])
    return out


def draw():
    rows = load()
    boxes = box_lines()
    n = len(rows)
    row_h, gap = 1.0, 1.34

    # one severity scale across the event rows, so they compare to each other
    sev_max = max(r["sev"].max() for r in rows if r["sev"] is not None)

    fig, ax = plt.subplots(figsize=(19.0, 6.4))

    for i, r in enumerate(rows):
        y = (n - 1 - i) * gap
        frac, sev = r["frac"], r["sev"]

        # the band the row lives in
        ax.add_patch(Rectangle((0, y), NM, row_h, facecolor="#f4f2ee",
                               edgecolor="none", zorder=0))

        for m in range(NM):
            f = frac[m]
            if f <= 0:
                continue
            shade = SHADE_FLOOR + (1 - SHADE_FLOOR) * min(f, 1.0)
            if r["kind"] == "count":
                # presence band: full height, because no magnitude is defined
                h, y0 = row_h, y
            else:
                h = BAR_FILL * row_h * (sev[m] / sev_max)
                if h <= 0:
                    continue
                y0 = y
            ax.add_patch(Rectangle((m, y0), 1.0, h, facecolor=r["colour"],
                                   alpha=shade, edgecolor="none", zorder=2))

        ax.text(-7, y + row_h * 0.62, r["label"], ha="right", va="center",
                fontsize=12, fontweight="bold", color="#222")
        ax.text(-7, y + row_h * 0.24, r["sub"], ha="right", va="center",
                fontsize=8.4, color="#666")

        # the row's own headline number, outside the plotting area
        title, lines = boxes[r["label"]]
        bx = NM + BOX_GAP
        ax.add_patch(Rectangle((bx, y), BOX_W, row_h, facecolor="#fbfaf8",
                               edgecolor=r["colour"], lw=0.8, alpha=0.95,
                               zorder=3, clip_on=False))
        ax.text(bx + 7, y + row_h * 0.74, title, ha="left", va="center",
                fontsize=7.6, color="#888", clip_on=False)
        for k, line in enumerate(lines):
            ax.text(bx + 7, y + row_h * (0.46 - 0.26 * k), line, ha="left",
                    va="center", fontsize=8.6, color="#333", clip_on=False,
                    fontfamily="DejaVu Sans Mono")

    # the line between the two families of definition
    # rows are drawn top-down, so row i sits at (n-1-i)*gap. The counting
    # rules are rows 0 and 1, so the divider goes in the gap below row 1 -
    # that is the bottom of its band, less half the spacing between bands.
    ydiv = (n - 1 - 1) * gap - (gap - row_h) * 0.74
    ax.plot([-4, NM], [ydiv, ydiv], color="#999", lw=0.9, ls=(0, (5, 4)),
            zorder=4, clip_on=False)
    # the divider sits low in the gap so the brown caption has room to clear
    # CDM's band above it; the blue one has the next band's gap below
    ax.text(NM, ydiv + 0.05, "GWSC counting rules", ha="right", va="bottom",
            fontsize=9.2, color="#8c6d4f", style="italic")
    # the gap alone cannot hold text on both sides, so the blue caption sits
    # just inside the top of the first event row, where the band is empty
    ax.text(NM, ydiv - 0.20, "event-based detection", ha="right", va="top",
            fontsize=9.2, color="#1f5c8b", style="italic")

    # decade ticks
    ticks = [(y - Y0) * 12 for y in range(Y0, Y1 + 1, 5)]
    ax.set_xticks(ticks)
    ax.set_xticklabels([str(y) for y in range(Y0, Y1 + 1, 5)], fontsize=10)
    for t in ticks:
        ax.plot([t, t], [-0.42, (n - 1) * gap + row_h], color="#fff", lw=0.7,
                zorder=1)

    ax.set_xlim(-2, NM + BOX_GAP + BOX_W + 4)
    ax.set_ylim(-0.72, (n - 1) * gap + row_h + 0.30)
    ax.set_yticks([])
    for s in ax.spines.values():
        s.set_visible(False)
    ax.tick_params(axis="x", length=0, pad=6)

    # legend for the two channels
    # shading means the same thing in every row, so the swatches show it in
    # both palettes - one colour alone reads as though only that family
    # carries extent
    lx, ly = 2, -0.62
    steps = [0.15, 0.4, 0.7, 1.0]
    for k, colour in enumerate(["#8c6d4f", "#1f5c8b"]):
        for j, f in enumerate(steps):
            ax.add_patch(Rectangle((lx + j * 8, ly + 0.185 * (1 - k)), 7, 0.15,
                                   facecolor=colour,
                                   alpha=SHADE_FLOOR + (1 - SHADE_FLOOR) * f,
                                   edgecolor="none", clip_on=False))
    ax.text(lx + len(steps) * 8 + 4, ly + 0.17,
            "shade = share of Benin affected  (both families)",
            ha="left", va="center", fontsize=9, color="#444")
    ax.text(NM, ly + 0.17,
            "bar height = mean severity  (counting rules define none)",
            ha="right", va="center", fontsize=9, color="#444")

    fig.tight_layout()
    fig.savefig(f"{OUT}/figs/ribbon_metrics_v2.png", dpi=190,
                bbox_inches="tight", facecolor="white")
    print("wrote", f"{OUT}/figs/ribbon_metrics_v2.png")
    print(f"severity scale max = {sev_max:.2f}")


if __name__ == "__main__":
    draw()
