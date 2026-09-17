# Databricks notebook source
# Inspect drought_summary_yearly_ref for the OFAT characteristics report:
# columns, distinct configs, indices/scales/thresholds available.
import pandas as pd
p = "/Volumes/projects/gwsc_drought/drought_outputs/v2/drought_summary_yearly_ref.parquet"
d = pd.read_parquet(p)
out = []
out.append(f"cols={list(d.columns)}")
out.append(f"rows={len(d)}")
out.append(f"n_configs={d['config_id'].nunique()}")
out.append(f"sample_configs={sorted(d['config_id'].unique())[:12]}")
for c in ["index","method","start_thr","country"]:
    if c in d.columns:
        out.append(f"{c}_vals={sorted([str(v) for v in d[c].dropna().unique()])[:10]}")
# also the annual (aggregated) one
p2 = "/Volumes/projects/gwsc_drought/drought_outputs/v2/drought_annual_summary_ref.parquet"
d2 = pd.read_parquet(p2)
out.append(f"--- annual_summary cols={list(d2.columns)} rows={len(d2)}")
dbutils.notebook.exit(" || ".join(out))
