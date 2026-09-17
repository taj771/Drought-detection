import pandas as pd, numpy as np, matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt

WMO = [-0.5, -1.0, -1.5]
d = pd.read_parquet('Codes/agg_window/out/config_rounding.parquet')
k = ['fam','scale','start_thr']
a, b = d[d.window==1], d[d.window==10]
m = a[k+['value','shown']].merge(b[k+['value','shown']], on=k, suffixes=('_yr','_dec'))
m = m[m.start_thr.round(2).isin(WMO)].copy()
m['naive'] = m.shown_yr*10
m['ok']    = m.shown_dec == m.naive

FAMS = ['SPI','SPEI-T','SPEI-P']
SCAL = [1,3,6,12]
col  = {'SPI':'#1b9e77','SPEI-T':'#d95f02','SPEI-P':'#7570b3'}
BAD, HELD = '#d7301f', '#2c7fb8'
X_FAM, X_SC, X_TH, X_YR, X_NAIVE, X_DEC = 0.0, 0.95, 1.85, 3.05, 3.85, 4.70

fig, axes = plt.subplots(1, 3, figsize=(15.0, 5.6))
for ax, fam in zip(axes, FAMS):
    s = m[m.fam==fam].sort_values(['scale','start_thr'],
                                  ascending=[True,False]).reset_index(drop=True)
    s['y'] = (len(s)-1-np.arange(len(s))) * 1.0

    for _, r in s.iterrows():
        if r.ok:  # band behind the rows that survive
            ax.add_patch(plt.Rectangle((X_YR-0.42, r.y-0.42), X_DEC-X_YR+0.84, 0.84,
                                       fc=HELD, alpha=.13, ec='none', zorder=0))
        ax.text(X_TH, r.y, f'{r.start_thr:+.1f}', fontsize=9, va='center', ha='right')
        ax.text(X_YR, r.y, f'{r.shown_yr:.0f}', fontsize=10, va='center', ha='center')
        ax.text(X_NAIVE, r.y, f'{r.naive:.0f}', fontsize=10, va='center', ha='center',
                color='grey')
        c = HELD if r.ok else BAD
        ax.text(X_DEC, r.y, f'{r.shown_dec:.0f}', fontsize=10.5, va='center', ha='center',
                color=c, fontweight='bold')
        if not r.ok:
            ax.annotate('', xy=(X_DEC-0.24, r.y), xytext=(X_NAIVE+0.20, r.y),
                        arrowprops=dict(arrowstyle='->', color=BAD, lw=1.0, alpha=.9))
        else:
            ax.plot([X_NAIVE+0.20, X_DEC-0.24], [r.y, r.y], color=HELD, lw=1.0, alpha=.8)

    for sc in SCAL:
        sub = s[s.scale==sc]
        ys = sub.y.values; ymid = ys.mean()
        ax.plot([X_SC, X_SC], [ys.min(), ys.max()], color='grey', lw=1.1)
        for y in ys:
            ax.plot([X_SC, X_TH+0.07], [y, y], color='grey', lw=0.8, alpha=.6)
        ax.plot([X_FAM+0.20, X_SC], [len(s)/2-0.5, ymid], color=col[fam], lw=1.4)
        ax.text(X_SC-0.12, ymid, f'{sc}mo', fontsize=9.5, va='center', ha='right',
                fontweight='bold')

    ax.text(X_FAM, len(s)/2-0.5, fam, fontsize=12, fontweight='bold', color=col[fam],
            va='center', ha='center', rotation=90)

    hy = len(s) - 0.35
    ax.text(X_YR,    hy, 'per\nyear',    fontsize=8.5, ha='center', va='bottom', style='italic')
    ax.text(X_NAIVE, hy, '$\times$10\npredicts', fontsize=8.5, ha='center', va='bottom',
            style='italic', color='grey')
    ax.text(X_DEC,   hy, 'per\ndecade',  fontsize=8.5, ha='center', va='bottom', style='italic')
    nbad = int((~s.ok).sum())
    ax.set_title(f'{fam}  —  {nbad} of {len(s)} move', fontsize=10.5, color=col[fam], pad=16)
    ax.set_xlim(-0.30, 5.15); ax.set_ylim(-0.9, len(s)+1.4); ax.axis('off')

fig.suptitle('Does a config keep its bin when the reporting window grows from a year to a decade?',
             fontsize=13, y=1.005)
fig.text(0.5, -0.035,
         'Blue = the decade value is exactly the annual value $\times$ 10.   '
         'Red = it lands in a different bin.   '
         'Benin, 154 pixels, 2003\u20132023, WMO thresholds only.',
         ha='center', fontsize=9)
plt.tight_layout()
plt.savefig('optE_tree_wmo.png', dpi=155, bbox_inches='tight')

tot = len(m); bad = int((~m.ok).sum())
print(f'WMO subset: {bad} of {tot} move ({100*bad/tot:.0f}%)')
print(m[~m.ok].groupby('scale').size().to_string())
