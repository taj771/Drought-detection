import pandas as pd, matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib.lines import Line2D

d = pd.read_parquet('Codes/agg_window/out/config_rounding.parquet')
a, b = d[d.window == 1], d[d.window == 10]
k = ['fam', 'scale', 'start_thr']
m = a[k + ['shown']].merge(b[k + ['shown']], on=k, suffixes=('_yr', '_dec'))

# mock scope: 3-month accumulation only, so the slide is legible
m = m[m.scale == 3].copy()
m['lab'] = m.fam + '-' + m.scale.astype(str) + '  thr ' + m.start_thr.map('{:.2f}'.format)
m = m.sort_values(['shown_yr', 'shown_dec', 'fam']).reset_index(drop=True)
m['y'] = range(len(m))[::-1]

col = {'SPI': '#1b9e77', 'SPEI-T': '#d95f02', 'SPEI-P': '#7570b3'}
fig, ax = plt.subplots(figsize=(11, 6.2))
XL, XC, XR = 0.0, 1.0, 2.0

for _, r in m.iterrows():
    c = col[r.fam]
    # left: annual bin -> label (solid = collapsed together)
    ax.plot([XL + .06, XC - .40], [r.shown_yr * (len(m) - 1) / 3, r.y],
            color='grey', lw=.8, alpha=.5, zorder=1)
    # right: label -> decade bin (dashed = it moved apart)
    ax.annotate('', xy=(XR - .06, (r.shown_dec - 4) * (len(m) - 1) / 11), xytext=(XC + .40, r.y),
                arrowprops=dict(arrowstyle='->', ls='--', color=c, lw=1.0, alpha=.85), zorder=1)
    ax.text(XC, r.y, r.lab, ha='center', va='center', fontsize=6.5, color=c,
            bbox=dict(fc='white', ec='none', pad=1.2), zorder=3)

for v in sorted(m.shown_yr.unique()):
    y = v * (len(m) - 1) / 3
    ax.text(XL, y, f'{v:.0f}', ha='right', va='center', fontsize=10, fontweight='bold')
for v in sorted(m.shown_dec.unique()):
    y = (v - 4) * (len(m) - 1) / 11
    ax.text(XR, y, f'{v:.0f}', ha='left', va='center', fontsize=10, fontweight='bold')

ax.set_title('Does the clustering survive a decade axis?  (3-month configs, Benin)', fontsize=11)
ax.text(XL, len(m) + .5, 'reads as\nper YEAR', ha='right', va='bottom', fontsize=8.5, style='italic')
ax.text(XR, len(m) + .5, 'reads as\nper DECADE', ha='left', va='bottom', fontsize=8.5, style='italic')
ax.legend(handles=[Line2D([], [], color=v, ls='--', label=kk) for kk, v in col.items()],
          loc='lower center', bbox_to_anchor=(.5, -.13), ncol=3, frameon=False, fontsize=8)
ax.set_xlim(-.55, 2.55); ax.set_ylim(-1.2, len(m) + 1.6); ax.axis('off')
plt.tight_layout()
plt.savefig('mock_C6_annual_vs_decade.png', dpi=160, bbox_inches='tight')
print('ok', len(m), 'configs')
