import pandas as pd, numpy as np, matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt

d = pd.read_parquet('Codes/agg_window/out/config_rounding.parquet')
k = ['fam','scale','start_thr']
a, b = d[d.window==1], d[d.window==10]
m = a[k+['value','shown']].merge(b[k+['value','shown']], on=k, suffixes=('_yr','_dec'))
col = {'SPI':'#1b9e77','SPEI-T':'#d95f02','SPEI-P':'#7570b3'}

# ---------- OPTION B: stacked bin columns, all 60 configs ----------
fig, ax = plt.subplots(figsize=(11,5.4))
for xpos, cc, lab in [(0,'shown_yr','reads as per YEAR'), (1,'shown_dec','reads as per DECADE')]:
    vc = m[cc].value_counts().sort_index()
    for v, n in vc.items():
        sub = m[m[cc]==v]
        # stack one square per config, coloured by family
        order = sub.sort_values('fam')
        for j,(_,r) in enumerate(order.iterrows()):
            ax.add_patch(plt.Rectangle((xpos-0.16+ (j%6)*0.055, v-0.30+ (j//6)*0.16),
                                       0.05, 0.14, fc=col[r.fam], ec='none'))
        ax.text(xpos+0.22, v, f'{v:g}  (n={n})', va='center', fontsize=7.5)
ax.set_xlim(-0.5,1.75); ax.set_ylim(-1,24)
ax.set_xticks([0,1]); ax.set_xticklabels(['per YEAR\n4 distinct values','per DECADE\n22 distinct values'], fontsize=10)
ax.set_ylabel('printed value (events per window)')
ax.set_title('Same 60 configs, two reporting axes — one square = one config', fontsize=11)
for s in ['top','right']: ax.spines[s].set_visible(False)
plt.tight_layout(); plt.savefig('optB_bins.png', dpi=160, bbox_inches='tight'); plt.close()

# ---------- OPTION C: slope / dot-strip, log-free, per scale ----------
fig, axes = plt.subplots(1,4, figsize=(12,4.6), sharey=False)
for ax,(sc) in zip(axes,[1,3,6,12]):
    s = m[m.scale==sc]
    for _,r in s.iterrows():
        ax.plot([0,1],[r.shown_yr, r.shown_dec/10], color=col[r.fam], lw=1.1, alpha=.8,
                marker='o', ms=3)
    ax.set_title(f'{sc}-month', fontsize=10)
    ax.set_xticks([0,1]); ax.set_xticklabels(['year','decade/10'], fontsize=8)
    for sp in ['top','right']: ax.spines[sp].set_visible(False)
axes[0].set_ylabel('printed value, rescaled to per-year units')
fig.suptitle('Decade axis adds resolution; no config overtakes another', fontsize=11)
plt.tight_layout(); plt.savefig('optC_slope.png', dpi=160, bbox_inches='tight'); plt.close()
print('ok')
