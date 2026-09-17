"""Generate the SPI calculation build-up figures for the GWSC lab meeting deck.

Emits Figures/spi_step1.png .. spi_step7.png, a seven-frame fragment stack that
walks through how a monthly precipitation series becomes an SPI-3 value:

  1. monthly precipitation bars for one Benin pixel
  2. the 3-month accumulation window for September
  3. the window slid forward to October, overlapping the September window
  4. the distribution of every September 3-month total, with a fitted gamma
  5. the October distribution alongside it, visibly different
  6. the gamma -> cumulative probability -> standard normal mapping
  7. the resulting SPI-3 series with the worked month marked

All frames share one canvas size and one axes layout so the stack swaps cleanly
in place under Reveal's fragment classes.

Data: Codes/_archive/_data/spi_results.parquet (ERA5, Benin, 1950-2023, 154 px).
The SPI values plotted in frame 7 are the pipeline's own, not recomputed here;
the gamma fit in frames 4-6 is refitted locally only to draw the curve.
"""

import numpy as np
import pandas as pd
import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib.patches import Rectangle, FancyArrowPatch
from scipy import stats

# ---------------------------------------------------------------- configuration

PARQUET = "../../../Codes/_archive/_data/spi_results.parquet"
OUTDIR = "Figures"

PIXEL = (2.125, 10.375)   # the pixel used on the preceding slide
FOCUS_MONTH = 9           # September - a wet-season month, so the
FOCUS_YEAR = 1961         # fitted gamma is bell-shaped, not a decay curve
SHOW_FROM, SHOW_TO = 1959, 1962   # bar-panel window

INK = "#1f2d34"
BAND = "#1f5673"
ACCENT = "#f2a541"
W1 = "#1f5673"
W2 = "#c1666b"
MUTED = "#8a97a0"
GRID = "#dfe5e9"

FIGSIZE = (15.5, 7.6)
DPI = 150

MONTHS = ["Jan", "Feb", "Mar", "Apr", "May", "Jun",
          "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"]


# ---------------------------------------------------------------------- loading

def load_pixel():
    """Return the monthly series for the demo pixel with a 3-month accumulation."""
    df = pd.read_parquet(PARQUET)
    x, y = PIXEL
    px = df[(df.x == x) & (df.y == y)].sort_values(["year", "month"]).copy()
    if px.empty:
        raise SystemExit(f"pixel {PIXEL} not found in {PARQUET}")
    px["date"] = pd.to_datetime(dict(year=px.year, month=px.month, day=1))
    # SPI-3 for month m accumulates months m-2, m-1, m
    px["acc3"] = px.monthly_precip.rolling(3).sum()
    return px.reset_index(drop=True)


# ------------------------------------------------------------------- primitives

def new_canvas():
    """Three-panel canvas: bars on the left, distributions and SPI on the right."""
    fig = plt.figure(figsize=FIGSIZE, dpi=DPI)
    gs = fig.add_gridspec(
        2, 2, height_ratios=[1.0, 0.85],
        left=0.055, right=0.985, top=0.92, bottom=0.13,
        wspace=0.20, hspace=0.46,
    )
    ax_bar = fig.add_subplot(gs[0, :])   # panel A spans the full width
    ax_dist = fig.add_subplot(gs[1, 0])  # panel B
    ax_spi = fig.add_subplot(gs[1, 1])   # panel C
    for ax in (ax_bar, ax_dist, ax_spi):
        style_axes(ax)
    return fig, ax_bar, ax_dist, ax_spi


def style_axes(ax):
    for side in ("top", "right"):
        ax.spines[side].set_visible(False)
    for side in ("left", "bottom"):
        ax.spines[side].set_color(MUTED)
        ax.spines[side].set_linewidth(0.8)
    ax.tick_params(colors=INK, labelsize=9, length=3, width=0.8)
    ax.grid(axis="y", color=GRID, linewidth=0.7, zorder=0)
    ax.set_axisbelow(True)


def blank(ax):
    """Hide a panel that this frame has not reached yet."""
    ax.set_visible(False)


def panel_title(ax, text, color=BAND):
    ax.set_title(text, loc="left", fontsize=11.5, color=color,
                 fontweight="600", pad=8)


def caption(fig, text, tight=False):
    """One explanatory line under the figure - the point of the current frame.

    `tight` pulls the line up under the bar panel on the early frames, where the
    lower two panels are hidden; without it the caption anchors the bottom of the
    canvas and bbox_inches="tight" keeps a blank half-page below the bars.
    """
    y = 0.42 if tight else 0.055
    fig.text(0.055, y, text, fontsize=11.5, color=INK, ha="left")


def save(fig, n):
    fig.savefig(f"{OUTDIR}/spi_step{n}.png", dpi=DPI,
                facecolor="white", bbox_inches="tight", pad_inches=0.18)
    plt.close(fig)
    print(f"wrote {OUTDIR}/spi_step{n}.png")


# ------------------------------------------------------------------- panel A

def draw_bars(ax, px, highlight=None, second=None):
    """Monthly precipitation bars, optionally with accumulation windows drawn on.

    highlight/second are (year, month) end-points of a 3-month window.
    """
    sub = px[(px.year >= SHOW_FROM) & (px.year <= SHOW_TO)]
    idx = np.arange(len(sub))
    colors = [MUTED] * len(sub)

    windows = []
    for spec, color in ((highlight, W1), (second, W2)):
        if spec is None:
            continue
        yr, mo = spec
        end = sub.index[(sub.year == yr) & (sub.month == mo)]
        if len(end) == 0:
            continue
        pos = sub.index.get_loc(end[0])
        span = list(range(pos - 2, pos + 1))
        windows.append((span, color, spec))

    # A month inside both windows keeps the first window's colour and gains a
    # hatch, so the two-thirds overlap stays visible instead of being repainted.
    hatched = set()
    if len(windows) == 2:
        hatched = set(windows[0][0]) & set(windows[1][0])
    for span, color, _ in windows:
        for p in span:
            if p not in hatched or colors[p] == MUTED:
                colors[p] = color

    bars = ax.bar(idx, sub.monthly_precip.values, color=colors,
                  width=0.74, zorder=2)
    for p in hatched:
        bars[p].set_hatch("///")
        bars[p].set_edgecolor(W2)
        bars[p].set_linewidth(0)

    top = sub.monthly_precip.max()
    # the second window's caption sits a step lower so the two never overprint
    label_y = [1.34, 1.06]
    for tier, (span, color, (yr, mo)) in enumerate(windows):
        lo, hi = span[0], span[-1]
        # a box around the three months that make up this accumulation
        ax.add_patch(Rectangle(
            (lo - 0.48, 0), (hi - lo) + 0.96, top * (label_y[tier] - 0.14),
            fill=False, edgecolor=color, linewidth=2.0,
            linestyle="--", zorder=4,
        ))
        total = sub.acc3.iloc[hi]
        # name the three months that go into this window, written as the sum
        parts = " + ".join(MONTHS[sub.month.iloc[p] - 1] for p in span)
        # sit the caption clear above the box, never on its dashed top edge
        ax.text((lo + hi) / 2, top * (label_y[tier] - 0.06),
                f"{MONTHS[mo - 1]} {yr}  =  {parts}  =  {total:.0f} mm",
                ha="center", va="bottom", fontsize=10.5, color=color,
                fontweight="600", zorder=5)
        # tag each contributing bar so the rolling window is unambiguous; a short
        # bar gets its name above the bar instead of inside it
        for p in span:
            name = MONTHS[sub.month.iloc[p] - 1]
            h = sub.monthly_precip.iloc[p]
            if h > top * 0.22:
                ax.text(p, top * 0.035, name, ha="center", va="bottom",
                        fontsize=8.5, color="white", fontweight="600",
                        rotation=90, zorder=5)
            else:
                ax.text(p, h + top * 0.02, name, ha="center", va="bottom",
                        fontsize=8.5, color=color, fontweight="600",
                        rotation=90, zorder=5)

    ax.set_ylim(0, top * 1.52)
    ax.set_xlim(-0.8, len(sub) - 0.2)

    # label January of each year only - monthly ticks would be unreadable
    ticks = [i for i, m in enumerate(sub.month.values) if m == 1]
    ax.set_xticks(ticks)
    ax.set_xticklabels([str(y) for y in sub.year.values[ticks]], fontsize=9.5)
    ax.set_ylabel("Monthly precipitation (mm)", fontsize=10, color=INK)
    panel_title(ax, "A.  One pixel's monthly rainfall")


# ------------------------------------------------------------------- panel B

def fit_gamma(series):
    """Fit a 2-parameter gamma to the non-zero accumulations, as SPI does."""
    v = series.dropna().values
    v = v[v > 0]
    shape, loc, scale = stats.gamma.fit(v, floc=0)
    return v, (shape, loc, scale)


def gamma_box(ax, params):
    """Small box naming the functional form of the gamma, with the fitted values.

    Shown on the left of panel B, where the fitted density has no mass: the
    legend holds the upper right and the marked-value label sits at the axvline.
    Carrying the actual shape and scale keeps the box from reading as decoration
    - it is the fit the curve above it came from.
    """
    shape, _, scale = params
    # mathtext has no \dfrac, only \frac
    ax.text(
        0.02, 0.97,
        r"$f(x) = \frac{1}{\Gamma(k)\,\theta^{k}}\, x^{k-1} e^{-x/\theta}$"
        "\n"
        rf"$k = {shape:.2f}$,   $\theta = {scale:.0f}$ mm"
        "\n"
        r"$x > 0$: a gamma cannot take a negative value",
        transform=ax.transAxes, fontsize=7.5, color=INK,
        va="top", ha="left", linespacing=1.7, zorder=6,
        bbox=dict(boxstyle="round,pad=0.35", facecolor="#f7f9fa",
                  edgecolor=GRID, linewidth=0.8, alpha=0.92),
    )


def draw_dist(ax, px, months, mark=None):
    """Histogram + fitted gamma for every year's 3-month total ending in `months`."""
    colors = {FOCUS_MONTH: W1, FOCUS_MONTH + 1: W2}
    all_vals = []
    fits = {}

    for mo in months:
        vals, params = fit_gamma(px[px.month == mo].acc3)
        all_vals.append(vals)
        fits[mo] = params
        c = colors[mo]
        alpha = 0.55 if len(months) == 1 else 0.38
        ax.hist(vals, bins=14, density=True, color=c, alpha=alpha,
                edgecolor="white", linewidth=0.6, zorder=2,
                label=f"{MONTHS[mo - 1]} totals, {px.year.min()}-{px.year.max()}")
        grid = np.linspace(0, max(v.max() for v in all_vals) * 1.08, 400)
        ax.plot(grid, stats.gamma.pdf(grid, *params), color=c,
                linewidth=2.2, zorder=3)

    if mark is not None:
        mo, value = mark
        ax.axvline(value, color=ACCENT, linewidth=2.2, zorder=4)
        ax.text(value, ax.get_ylim()[1] * 0.94,
                f"  {MONTHS[mo - 1]} {FOCUS_YEAR}\n  {value:.0f} mm",
                fontsize=10, color="#a8720f", fontweight="600",
                va="top", ha="left", zorder=5)

    ax.set_xlabel("3-month precipitation total (mm)", fontsize=10, color=INK)
    ax.set_ylabel("density", fontsize=10, color=INK)
    ax.legend(fontsize=9, frameon=False, loc="upper right")
    gamma_box(ax, fits[FOCUS_MONTH])
    if len(months) == 1:
        panel_title(ax, "B.  Fit a gamma distribution to that calendar month")
    else:
        panel_title(ax, "B.  A separate gamma fit for every calendar month")
    return fits


# ------------------------------------------------------------------- panel C

def draw_mapping(ax, value, params, spi):
    """Show gamma CDF -> probability -> standard normal quantile for one month."""
    shape, loc, scale = params
    p = stats.gamma.cdf(value, shape, loc, scale)

    grid = np.linspace(-3.4, 3.4, 500)
    ax.plot(grid, stats.norm.pdf(grid), color=BAND, linewidth=2.2, zorder=3)
    ax.fill_between(grid[grid <= spi], stats.norm.pdf(grid[grid <= spi]),
                    color=ACCENT, alpha=0.85, zorder=2)
    ax.axvline(spi, color="#a8720f", linewidth=2.4, zorder=4)
    # the tail at -2.9 is too thin to see, so point at it explicitly
    ax.annotate(f"only {p * 100:.1f}% of years\nare drier than this",
                xy=(spi - 0.05, 0.012), xytext=(spi - 0.15, 0.20),
                fontsize=10, color="#a8720f", fontweight="600",
                ha="right", va="bottom",
                arrowprops=dict(arrowstyle="->", color="#a8720f", linewidth=1.4),
                zorder=5)

    ax.set_xlim(-3.4, 3.4)
    ax.set_ylim(0, 0.46)
    ax.set_yticks([])
    ax.set_xlabel("SPI (standard normal)", fontsize=10, color=INK)
    ax.text(3.3, 0.42,
            f"P = {p:.3f}\nSPI-3 = {spi:+.2f}",
            fontsize=13, color="#a8720f", fontweight="600",
            ha="right", va="top", zorder=5)
    panel_title(ax, "C.  Same probability, read off a normal curve")


def draw_spi_series(ax, px, mark_date):
    """The finished SPI-3 series with the worked month flagged."""
    sub = px[(px.year >= SHOW_FROM) & (px.year <= SHOW_TO)]
    idx = np.arange(len(sub))
    v = sub.spi3.values

    # Deliberately unshaded: W1/W2 are the September and October accumulation
    # windows in panel A, so reusing them here for wet/dry would read as those
    # two months rather than as sign.
    ax.axhline(0, color=MUTED, linewidth=0.9, zorder=2)
    ax.plot(idx, v, color=INK, linewidth=1.7, zorder=3)

    hit = sub.index[sub.date == mark_date]
    if len(hit):
        pos = sub.index.get_loc(hit[0])
        ax.plot([pos], [v[pos]], "o", color=ACCENT, markersize=9,
                markeredgecolor="#a8720f", markeredgewidth=1.4, zorder=5)
        # label above the point - the marked month is a deep negative, so a
        # label below it would sit on top of the year ticks
        ax.annotate(f"{MONTHS[mark_date.month - 1]} {mark_date.year}: {v[pos]:+.2f}",
                    xy=(pos, v[pos]), xytext=(pos + 4.0, v[pos] + 1.15),
                    fontsize=10, color="#a8720f", fontweight="600",
                    ha="center",
                    arrowprops=dict(arrowstyle="->", color="#a8720f", linewidth=1.3),
                    zorder=5)

    ticks = [i for i, m in enumerate(sub.month.values) if m == 1]
    ax.set_xticks(ticks)
    ax.set_xticklabels([str(y) for y in sub.year.values[ticks]], fontsize=9.5)
    ax.set_xlim(-0.8, len(sub) - 0.2)
    ax.set_ylim(min(v.min() - 0.35, -3.2), max(v.max() + 0.35, 2.6))
    ax.set_ylabel("SPI-3", fontsize=10, color=INK)
    panel_title(ax, "C.  Every month, and you have the SPI-3 series")


# ----------------------------------------------------------------------- frames

def main():
    px = load_pixel()
    focus = px[(px.year == FOCUS_YEAR) & (px.month == FOCUS_MONTH)].iloc[0]
    mar_value = focus.acc3
    mar_spi = focus.spi3
    focus_date = focus.date
    _, mar_params = fit_gamma(px[px.month == FOCUS_MONTH].acc3)

    # 1 - the raw series, nothing else
    fig, a, b, c = new_canvas()
    draw_bars(a, px)
    blank(b); blank(c)
    caption(fig, "Start with one pixel's monthly rainfall - no averaging over the country.", tight=True)
    save(fig, 1)

    # 2 - the March accumulation window
    fig, a, b, c = new_canvas()
    draw_bars(a, px, highlight=(FOCUS_YEAR, FOCUS_MONTH))
    blank(b); blank(c)
    caption(fig, "SPI-3 for September = July + August + September. The window is the memory.", tight=True)
    save(fig, 2)

    # 3 - the window slides, and overlaps
    fig, a, b, c = new_canvas()
    draw_bars(a, px, highlight=(FOCUS_YEAR, FOCUS_MONTH), second=(FOCUS_YEAR, FOCUS_MONTH + 1))
    blank(b); blank(c)
    caption(fig, "Slide it one month: October = August + September + October. Two of the three months are shared.", tight=True)
    save(fig, 3)

    # 4 - the September distribution across all years. Both windows stay on the
    # bars from frame 3 onwards: once the audience has seen the pair, removing
    # one reads as the window moving rather than as a second fit being added.
    fig, a, b, c = new_canvas()
    draw_bars(a, px, highlight=(FOCUS_YEAR, FOCUS_MONTH), second=(FOCUS_YEAR, FOCUS_MONTH + 1))
    draw_dist(b, px, [FOCUS_MONTH])
    blank(c)
    caption(fig, "Collect that September total for all 74 years and fit a gamma distribution to it.")
    save(fig, 4)

    # 5 - April's distribution is a different distribution
    fig, a, b, c = new_canvas()
    draw_bars(a, px, highlight=(FOCUS_YEAR, FOCUS_MONTH), second=(FOCUS_YEAR, FOCUS_MONTH + 1))
    draw_dist(b, px, [FOCUS_MONTH, FOCUS_MONTH + 1])
    blank(c)
    caption(fig, "October gets its own fit. Twelve fits per pixel - that is what removes the seasonal cycle.")
    save(fig, 5)

    # 6 - the probability -> z mapping for the worked month
    fig, a, b, c = new_canvas()
    draw_bars(a, px, highlight=(FOCUS_YEAR, FOCUS_MONTH), second=(FOCUS_YEAR, FOCUS_MONTH + 1))
    draw_dist(b, px, [FOCUS_MONTH, FOCUS_MONTH + 1], mark=(FOCUS_MONTH, mar_value))
    draw_mapping(c, mar_value, mar_params, mar_spi)
    caption(fig, f"September {FOCUS_YEAR} sits far down in its own history. That probability becomes the SPI value.")
    save(fig, 6)

    # 7 - repeat for every month and you have the series
    fig, a, b, c = new_canvas()
    draw_bars(a, px, highlight=(FOCUS_YEAR, FOCUS_MONTH), second=(FOCUS_YEAR, FOCUS_MONTH + 1))
    draw_dist(b, px, [FOCUS_MONTH, FOCUS_MONTH + 1])
    draw_spi_series(c, px, focus_date)
    caption(fig, "SPI is a rank in the pixel's own history - not an amount of rain.")
    save(fig, 7)


if __name__ == "__main__":
    main()
