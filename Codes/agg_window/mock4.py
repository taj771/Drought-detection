import pandas as pd, numpy as np, matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt

d = pd.read_parquet('Codes/agg_window/out/config_rounding.parquet')
k = ['fam','scale','start_thr']
a, b = d[d.window==1], d[d.window==10]
m = a[k+['value','shown']].merge(b[k+['value','shown']], on=k, suffixes=('_yr','_dec'))
m['naive'] = m.shown_yr*10
m['ok']    = m.shown_dec == m.naive

FAMS  = ['SPI','SPEI-T','SPEI-P']
SCAL  = [1,3,6,12]
col   = {'SPI':'#1b9e77','SPEI-T':'#d95f02','SPEI-P':'#7570b3'}
GOOD, BAD = '#2c7fb8', '#d7301f'

# x positions
X_FAM, X_SC, X_TH, X_YR, X_NAIVE, X_DEC = 0.0, 0.9, 1.7, 3.0, 3.75, 4.6

fig, axes = plt.subplots(1, 3, figsize=(15.5, 7.2))
for ax, fam in zip(axes, FAMS):
    s = m[m.fam==fam].copy()
    s = s.sort_values(['scale','start_thr'], ascending=[True,False]).reset_index(drop=True)
    s['y'] = (len(s)-1-np.arange(len(s))) * 1.0

    # threshold leaves + value columns
    for _, r in s.iterrows():
        ax.text(X_TH, r.y, f'{r.start_thr:+.2f}', fontsize=7.5, va='center', ha='right')
        ax.text(X_YR, r.y, f'{r.shown_yr:.0f}', fontsize=8.5, va='center', ha='center')
        ax.text(X_NAIVE, r.y, f'{r.naive:.0f}', fontsize=8.5, va='center', ha='center',
                color='grey')
        c = GOOD if r.ok else BAD
        ax.text(X_DEC, r.y, f'{r.shown_dec:.0f}', fontsize=8.5, va='center', ha='center',
                color=c, fontweight='bold' if not r.ok else 'normal')
        if not r.ok:
            ax.annotate('', xy=(X_DEC-0.20, r.y), xytext=(X_NAIVE+0.16, r.y),
                        arrowprops=dict(arrowstyle='->', color=BAD, lw=0.9, alpha=.85))

    # scale nodes + branches
    for sc in SCAL:
        sub = s[s.scale==sc]
        if not len(sub): continue
        ys = sub.y.values
        ymid = ys.mean()
        ax.plot([X_SC, X_SC], [ys.min(), ys.max()], color='grey', lw=1.0)
        for y in ys:
            ax.plot([X_SC, X_TH+0.06], [y, y], color='grey', lw=0.7, alpha=.6)
        ax.plot([X_FAM+0.18, X_SC], [len(s)/2-0.5, ymid], color=col[fam], lw=1.2)
        ax.text(X_SC-0.10, ymid, f'{sc}mo', fontsize=8.5, va='center', ha='right',
                fontweight='bold')

    ax.text(X_FAM+0.10, len(s)/2-0.5, fam, fontsize=11, fontweight='bold',
            color=col[fam], va='center', ha='center', rotation=90)

    # headers
    hy = len(s) + 0.4
    ax.text(X_YR,    hy, 'per\nyear',   fontsize=8, ha='center', va='bottom', style='italic')
    ax.text(X_NAIVE, hy, r'$\times$10'+'\npredicts', fontsize=8, ha='center', va='bottom',
            style='italic', color='grey')
    ax.text(X_DEC,   hy, 'per\ndecade', fontsize=8, ha='center', va='bottom', style='italic')
    nbad = (~s.ok).sum()
    ax.set_title(f'{fam}   —   {nbad} of {len(s)} land in a different bin',
                 fontsize=10, color=col[fam])
    ax.set_xlim(-0.35, 5.05); ax.set_ylim(-1.0, len(s)+1.9); ax.axis('off')

fig.suptitle('Does rounding move each config to the same bin when the reporting window grows?',
             fontsize=13, y=0.985)
fig.text(0.5, 0.012,
         'Red = the decade bin is NOT the annual bin $\times$ 10.   '
         'Benin, 154 pixels, observation window 2003–2023.',
         ha='center', fontsize=9)
plt.tight_layout(rect=[0,0.03,1,0.95])
plt.savefig('optE_tree.png', dpi=155, bbox_inches='tight')
print('ok')
