"""
mk_detect_steps.py - Slide 6 build frames: how a drought event is detected and recorded.

Seven frames, one per click on the "Finding drought events" slide. Each frame adds
exactly what the presenter is saying at that moment:

  step1  bare SPI-3 series, no shading at all. The presenter is still explaining
         why we report events rather than index values; the detection rule has not
         been introduced yet, so nothing on the plot may pre-empt it.
  step2  threshold line + below-threshold points. The rule itself.
  step3  D1 shaded and labelled - one complete event, start to end.
  step4  D1 with its recorded characteristics called out (duration, peak, severity).
  step5  D2 and D3 shaded alongside D1.
  step6  every detected event shaded, plus the pixel summary panel.
  step7  same, with the "all 154 pixels x 75 configurations" scale-up note.

The series and event table are read from the cached pickles written by the original
(now missing) generator, so the numbers quoted in the presenter notes - D1 running
nine months to a peak of -3.09 with severity 7.33 - are reproduced exactly rather
than recomputed and risking drift.

Output: Figures/detect_step1.png .. detect_step7.png at 2210x1071, matching the
dimensions the deck's .detect-stack CSS is sized around.
"""

from pathlib import Path

import matplotlib
matplotlib.use("Agg")

import matplotlib.dates as mdates
import matplotlib.pyplot as plt
import pandas as pd
from matplotlib.patches import Patch

HERE = Path(__file__).resolve().parent
FIGDIR = HERE / "Figures"

THR = -0.5
DPI = 200
FIGSIZE = (2210 / DPI, 1071 / DPI)

COL_LINE = "#1D4ED8"
COL_BELOW = "#DC2626"
COL_SHADE = "#991B1B"   # light maroon once alpha is applied
COL_GREY = "#374151"
SHADE_ALPHA = 0.18


def load():
    series = pd.read_pickle(FIGDIR / "_series.pkl")
    events = pd.read_pickle(FIGDIR / "_events.pkl").sort_values("start").reset_index(drop=True)
    return series, events


def shade(ax, events):
    """Shade each event from its start to the end of its final month."""
    for _, ev in events.iterrows():
        ax.axvspan(
            ev["start"],
            ev["end"] + pd.offsets.MonthEnd(1),
            color=COL_SHADE,
            alpha=SHADE_ALPHA,
            linewidth=0,
            zorder=0,
        )


def label_event(ax, ev, name, y, detail=False):
    mid = ev["start"] + (ev["end"] - ev["start"]) / 2
    if detail:
        text = f"{name}\n{ev['dur']} months\npeak {ev['peak']:.2f}\nseverity {ev['sev']:.2f}"
    else:
        text = name
    ax.annotate(
        text,
        xy=(mid, y),
        ha="center",
        va="center",
        fontsize=11 if detail else 12,
        fontweight="bold",
        color=COL_SHADE,
        bbox=dict(boxstyle="round,pad=0.35", facecolor="white",
                  edgecolor=COL_SHADE, linewidth=0.9, alpha=0.95),
        zorder=6,
    )


def summary_panel(ax, events):
    lines = [
        "Pixel summary",
        f"events: {len(events)}",
        f"mean duration: {events['dur'].mean():.1f} months",
        f"mean peak: {events['peak'].mean():.2f}",
        f"mean severity: {events['sev'].mean():.2f}",
    ]
    ax.text(
        0.995, 0.03, "\n".join(lines),
        transform=ax.transAxes, ha="right", va="bottom",
        fontsize=11, family="monospace", color=COL_GREY,
        bbox=dict(boxstyle="round,pad=0.5", facecolor="#F9FAFB",
                  edgecolor="#9CA3AF", linewidth=0.9),
        zorder=6,
    )


def make_frame(n, series, events):
    fig, ax = plt.subplots(figsize=FIGSIZE, dpi=DPI)

    # Step 1 is deliberately bare: no threshold, no points, no shading.
    show_rule = n >= 2
    if n == 3 or n == 4:
        shown = events.iloc[:1]
    elif n == 5:
        shown = events.iloc[:3]
    elif n >= 6:
        shown = events
    else:
        shown = events.iloc[:0]

    if len(shown):
        shade(ax, shown)

    ax.axhline(0, color="#9CA3AF", linewidth=0.8, linestyle=":", zorder=1)
    if show_rule:
        ax.axhline(THR, color=COL_BELOW, linewidth=1.4, linestyle="--", zorder=2)
        ax.annotate(
            f"threshold = {THR}",
            xy=(series.index[0], THR),
            xytext=(6, 8), textcoords="offset points",
            ha="left", va="bottom", fontsize=12, color=COL_BELOW,
        )

    ax.plot(series.index, series.values, color=COL_LINE, linewidth=1.5, zorder=4)
    if show_rule:
        below = series[series < THR]
        ax.scatter(below.index, below.values, color=COL_BELOW, s=14, zorder=5)

    y_lo = min(series.min(), THR) - 0.45
    y_hi = max(series.max(), 0.5) + 0.55
    ax.set_ylim(y_lo, y_hi)

    if n == 3:
        label_event(ax, events.iloc[0], "D1", y_hi - 0.45)
    elif n == 4:
        label_event(ax, events.iloc[0], "D1", y_hi - 0.75, detail=True)
    elif n == 5:
        for i, name in enumerate(["D1", "D2", "D3"]):
            label_event(ax, events.iloc[i], name, y_hi - 0.45)

    if n >= 6:
        summary_panel(ax, events)
        ax.legend(
            handles=[Patch(facecolor=COL_SHADE, alpha=SHADE_ALPHA,
                           label=f"detected drought events ({len(events)})")],
            loc="upper right", frameon=True, fontsize=11,
        )

    if n == 7:
        ax.text(
            0.5, 1.045,
            "repeated for all 154 pixels x 75 configurations",
            transform=ax.transAxes, ha="center", va="bottom",
            fontsize=13, fontweight="bold", color=COL_GREY,
        )

    ax.set_ylabel("SPI-3", fontsize=13)
    ax.xaxis.set_major_locator(mdates.YearLocator(3))
    ax.xaxis.set_major_formatter(mdates.DateFormatter("%Y"))
    ax.tick_params(labelsize=12)
    for side in ("top", "right"):
        ax.spines[side].set_visible(False)
    ax.grid(axis="y", color="#E5E7EB", linewidth=0.6)
    ax.set_axisbelow(True)

    fig.tight_layout()
    out = FIGDIR / f"detect_step{n}.png"
    fig.savefig(out, dpi=DPI, facecolor="white")
    plt.close(fig)
    return out


def main():
    series, events = load()
    print(f"series: {len(series)} months, {series.index[0]:%Y-%m} to {series.index[-1]:%Y-%m}")
    print(f"events: {len(events)}")
    for n in range(1, 8):
        print("wrote", make_frame(n, series, events).name)


if __name__ == "__main__":
    main()
