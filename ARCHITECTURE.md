# Drought Indices — Pipeline Overview & Architecture

> **Status:** Pipeline developed by TJ; sharing the architecture with the team
> for comments and input. GitHub repo:
> <https://github.com/Global-Water-Security-Center/gwsc-drought-indices>

---

## 1. The big picture

There are **three pipelines**, kept in separate folders under `Codes/`. They
share the same underlying data but serve different goals:

![Three-pipeline overview](Codes/reports/arch_diagram.png)

| Folder | Pipeline | Purpose | Status |
|--------|----------|---------|--------|
| `Codes/_archive/v1_R_single_cluster/` | v1 | Original research pipeline, pure R. Superseded by v2 (with Sarva's input) to cut compute time. | **Archived 2026-09-16** — do not run. Moved under `_archive/`; its Databricks job is kept (unscheduled, renamed `[ARCHIVED]`) so run history survives. Retained as the independent full-R reference: v2's Python steps were ported from it, so it is the check to reach for if a v2 number looks wrong. |
| `Codes/v2_pipeline/` | v2 | Research pipeline with statistical tests. Improves on v1's compute time; working with Sarva on further gains. | Running. Faster than v1, but needs a multi-node cluster — which (per Sarva) is more cost-effective at scale. |
| `Codes/_archive/app_monitor/` | App | Operational monitor: detects drought events and characteristics from ERA5. Standalone R reimplementation of the index + detection chain. | **Archived 2026-09-16** — do not run. Superseded by the operational branch, which trims v2 instead of reimplementing it. It used a different index engine (R `SPEI`, not Python `climate-indices`) and had drifted behind v2 on two result-changing fixes: it reads the broken ERA5-Land PET (replaced in v2 by CRU TS 4.08 Penman) and clips by bounding box only, never applying the country polygon (fixed in v2 script 01). |

---

## 2. The weather data (shared by all)

All pipelines read **ERA5** — a global, gridded weather dataset, covering
**1950–2026**. We use two variables: **total rainfall** and **mean temperature**,
aggregated to monthly values per grid cell.

For SPEI Penman-Monteith we use the ERA5-Land dataset, downloaded separately; the
code is available at `Codes/PET_data_prep`.

> **Tech notes**
> - Source table: `era5-daily-data.silver_staging.era5_daily_nn` (PET-matched
>   grid, global, 1950–2026).
> - Country boundaries: `…esri_worldcountryboundaries_global_silver` (`chip_wkb`
>   polygons), read in-cluster — no shapefile downloads.
> - Pixels are clipped to the boundary polygon (not just a bounding box).

---

## 3. Pipeline 1 — Research (v1)

The original research pipeline that produced the Phase-1 numbers and statistics.
It is written purely in R. As advised by the tech team, we moved to v2 — which
adds some Python — to improve compute time, so this version can probably be
deprecated. The pipeline is described in detail under v2 below.

---

## 4. Pipeline 2 — Research (v2)

The full scientific pipeline. It produces the numbers and statistics for the
Phase-1 report.

**What it does, step by step:**

1. **Calculate drought indices** — for every grid cell and month, how dry is it
   versus normal? Three flavours:
   - **SPI** — based on rainfall only
   - **SPEI (Thornthwaite)** — rainfall minus a simple estimate of water loss
   - **SPEI (Penman-Monteith)** — rainfall minus a more detailed water-loss
     estimate

   Each is computed at several "accumulation periods" (1, 3, 6, 12 months) and
   several severity thresholds → **75 configurations** in total.
2. **Detect drought events** — turn the monthly index into discrete events
   (start, end, duration, severity).
3. **Summarise & analyse** — frequency, duration, severity, area affected; then
   statistics: which definitions agree, how drought changed between the
   reference period (1973–2002) and the recent period (2003–2023), and a
   comparison with a separate "consecutive dry days" measure.

**What you get out:** event lists and summary tables (one set per configuration
per country), plus statistical results and a few figures.

> **Tech notes**
> - Heavy, distributed steps are **Python** (`pandas_udf` / `applyInPandas`);
>   statistics and figures are **R** (mature stats libraries).
> - It runs as a **single Databricks multi-task Job** (`drought_pipeline_v2`) —
>   a dependency graph you trigger with one command. Index calc (01/02a/02b) →
>   detection (03) → then sanity check (04), reference params (05), Friedman
>   stats (06), cross-period (07), appendix figure (08), CDD comparison (09).
> - Runner notebooks (`_run_py_step`, `_run_r_step`) launch each script via a
>   `STEP` parameter, so one job step = one script.
> - Cluster: `0616-154127-izvo5r0w` (DBR 15.4, multi-node). Benin validated:
>   full run ≈ 38 min, 10/10 steps green.

---

## 5. Pipeline 3 — Daily Monitor (app_monitor)

A lightweight, on-demand tool. Pick a country, one index, one threshold → get a
**snapshot of drought** for that country. No heavy statistics.

**What you get out:**

1. **A summary table** comparing the reference period (1973–2002) to the recent
   period (2003–2023):

   | Characteristic              | Reference | Observation | % Change |
   |-----------------------------|-----------|-------------|----------|
   | Frequency (events/pixel/yr) |   …       |   …         |   …      |
   | Duration (months)           |   …       |   …         |   …      |
   | Peak severity               |   …       |   …         |   …      |
   | Pixels affected (%)         |   …       |   …         |   …      |

2. **Four figures:**
   - **Hotspot map** — where droughts concentrate (per-pixel, clipped to country)
   - **Time-series** — the index month by month, with the drought threshold
   - **Reference vs Observation bars** — the four characteristics side by side
   - **Event timeline** — drought events as bars over time for the busiest cells

**Same data as the research pipeline, but reusable for any country on demand.**

> **For developers**
> - R + single-node personal cluster `0123-211315-fp4hw1e8` (DBR 16.4).
> - Index engine = R `SPEI` package (the research pipeline uses Python
>   `climate_indices`, a different library). Documented, acceptable.
> - Scripts: `00_APP_MASTER` (params) → `01` index → `02` detection →
>   `03` table → `04` visuals.
> - Cluster init `init_app_geo.sh` installs terra/sf/units (best-effort, never
>   blocks boot).

---

## 6. How it's run & deployed

- Code lives on **GitHub** (`gwsc-drought-indices`, branch `main`).
- It is pushed to **Databricks** as a "bundle" and run there on clusters.
- Outputs are written to a shared **Volume**:
  `/Volumes/projects/gwsc_drought/drought_outputs/`.

---

## 7. Key decisions (for technical reviewers)

- **Three pipelines, not one** — research (thorough, slow) and monitoring (fast,
  simple) have different needs; keeping them separate avoids one bloating the
  other. v1 retained only for traceability.
- **Hybrid Python + R for v2** — Python where distribution gives real speed-up
  (per-pixel index fitting); R where the statistics libraries are stronger.
- **75 configurations** — five thresholds × indices × accumulation periods, to
  test how sensitive the drought picture is to the definition chosen.

---

## 8. What we'd like your feedback on

1. **Daily monitor configuration** — which single index / accumulation period /
   threshold should the operational monitor use? (the research pipeline can
   inform this.)
2. **Outputs** — is the summary table + four figures the right set for daily use,
   or do you want different / additional views?
3. **Country scope** — current focus is the 6 study countries; what's the
   priority order for expansion?
4. **Efficiency** — any concerns on runtime / cost as we scale to more countries?

*Please add comments inline or in the PR. Thanks!*
