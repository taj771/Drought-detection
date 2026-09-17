import pandas as pd, numpy as np, matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib.lines import Line2D

d = pd.read_parquet('Codes/agg_window/out/config_rounding.parquet')
k=['fam','scale','start_thr']
a,b = d[d.window==1], d[d.window==10]
m = a[k+['value','shown']].merge(b[k+['value','shown']],on=k,suffixes=('_yr','_dec'))
m['naive'] = m.shown_yr*10
m['dev']   = m.shown_dec - m.naive
col = {'SPI':'#1b9e77','SPEI-T':'#d95f02','SPEI-P':'#7570b3'}

fig, ax = plt.subplots(figsize=(11,5.6))
jit = {0:-0.18,1:-0.06,2:0.06,3:0.18}
for _,r in m.iterrows():
    x = r.shown_yr + jit.get(int(r.shown_yr),0)*0  # no jitter on x groups
    ax.plot([r.shown_yr], [r.naive], marker='_', ms=26, color='grey', alpha=.25, zorder=1)
# draw naive expectation line
xs = sorted(m.shown_yr.unique())
ax.plot(xs, [v*10 for v in xs], ls='--', color='grey', lw=1.4, zorder=1,
        label='what "annual $\times$ 10" predicts')
# actual points, jittered within each annual bin
rng = np.random.default_rng(3)
for _,r in m.iterrows():
    xj = r.shown_yr + rng.uniform(-.28,.28)
    ax.scatter(xj, r.shown_dec, s=34, color=col[r.fam],
               edgecolor='white', linewidth=.5, zorder=3, alpha=.9)
# highlight SPI-3 -0.5
h = m[(m.fam=='SPI')&(m.scale==3)&(m.start_thr==-0.5)].iloc[0]
ax.annotate(f'SPI-3, thr $-$0.5\nprints "2" per year\n$\Rightarrow$ expect 20\nactually 15',
            xy=(h.shown_yr+.30, h.shown_dec), xytext=(h.shown_yr+.75, h.shown_dec-9),
            fontsize=8.5, ha='left',
            arrowprops=dict(arrowstyle='->', color='black', lw=1.0))
ax.scatter([h.shown_yr+.30],[h.shown_dec], s=95, facecolor='none', edgecolor='black',
           linewidth=1.3, zorder=4)

ax.set_xticks(xs); ax.set_xticklabels([f'"{v:g}"' for v in xs], fontsize=11)
ax.set_xlabel('what the config prints PER YEAR', fontsize=10)
ax.set_ylabel('what it actually prints PER DECADE', fontsize=10)
ax.set_title('The annual number does not rescale: only 5 of 60 configs land where "$\times$10" predicts',
             fontsize=11.5)
ax.legend(handles=[Line2D([],[],ls='--',color='grey',label='what "annual $\times$ 10" predicts')]+
                  [Line2D([],[],marker='o',ls='',color=v,label=kk) for kk,v in col.items()],
          loc='upper left', frameon=False, fontsize=9)
for s in ['top','right']: ax.spines[s].set_visible(False)
ax.grid(axis='y', alpha=.18)
plt.tight_layout(); plt.savefig('optD_rescale_break.png', dpi=160, bbox_inches='tight')
print('ok')
