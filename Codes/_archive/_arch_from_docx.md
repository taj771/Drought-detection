# Drought Indices --- Pipeline Overview & Architecture

# Table of Contents {#table-of-contents .TOC-Heading}

[Drought Indices --- Pipeline Overview & Architecture
[1](#_Toc233358584)](#_Toc233358584)

[1. What this project does (in one minute)
[1](#_Toc233358585)](#_Toc233358585)

[2. The big picture [1](#the-big-picture)](#the-big-picture)

[3. The weather data (shared by all) [2](#data)](#data)

[4. Pipeline 2 --- Research (v2)
[3](#pipeline-2-research-v2)](#pipeline-2-research-v2)

[5. Pipeline 3 --- Daily Monitor (app_monitor)
[3](#pipeline-3-daily-monitor-app_monitor)](#pipeline-3-daily-monitor-app_monitor)

[6. How it's run & deployed
[4](#how-its-run-deployed)](#how-its-run-deployed)

[7. Key decisions (for technical reviewers)
[4](#key-decisions-for-technical-reviewers)](#key-decisions-for-technical-reviewers)

[8. What we'd like your feedback on
[5](#what-id-like-your-feedback-on)](#what-id-like-your-feedback-on)

# 

> **Status:** Pipeline has developed by TJ and sharing the architecture
> with team for their comments and inputs. The Github repo is available
> here:
> <https://github.com/Global-Water-Security-Center/gwsc-drought-indices>

## 1. The big picture

There are **three pipelines**, kept in separate folders under `Codes/`.
They share the same underlying data but serve different goals:

  ---------------------------------------------------------------------------------------------
  Folder                         Pipeline       Purpose                   Status
  ------------------------------ -------------- ------------------------- ---------------------
  `Codes/v1_R_single_cluster/`   v1             Original research         Probably we can
                                                pipeline purely based on  deprecate this one as
                                                R and with Sarva's input  for research purpose
                                                of saving compute time v2 (i.e. adding more
                                                has developed and in      counties) this will
                                                involve some python       run higher compute
                                                notebooks                 time (still running
                                                                          if we want full R
                                                                          interface)

  `Codes/v2_pipeline/`           v2             Still use for research    Running and made
                                                purpose and it has        improvement in
                                                improvement for v1 to     compute time compared
                                                save Databricks compute   to v1. However this
                                                time and working with     one need multi-node
                                                Sarva to see there are    cluster and as check
                                                any improvements. This    with Sarva this is
                                                pipeline has statistical  more cost effective
                                                test scripts as well      that

  `Codes/app_monitor/`           App            Daily operational         Running and use full
                                                monitor: detect drought   R interface as we
                                                events and                finalize 1-3
                                                characteristics based on  configurations with
                                                ERA5                      1-2 countries for
                                                                          GWSC daily use the
                                                                          multi node
                                                                          improvements made in
                                                                          v2 may not needed
  ---------------------------------------------------------------------------------------------

  : Three-pipeline overview

## 2. Data

All pipelines read **ERA5** --- a global, gridded weather dataset ,
covering **1950--2026**. We use two variables: **total rainfall** and
**mean temperature**, aggregated to monthly values per grid cell.

For SPEI- Penman-Monteith we use ERA5 Land data set download separately
and code available at Codes/PET_data_prep

> **Tech Notes** - Source table:
> `era5-daily-``data.silver``_``staging.era``5_daily_nn` (PET-matched
> grid, global, 1950--2026). - Country boundaries:
> `…esri_worldcountryboundaries_global_silver` (`chip_wkb` polygons),
> read in-cluster --- no shapefile downloads. - Pixels are clipped to
> the boundary polygon.

## 4. Pipeline 1 --- Research (v1)

This is the full scientific pipeline. It produces the numbers and
statistics for the Phase-1 report/ Purely in R langayage and as guided
by Tech team we shifted to v2 with use of some pythin codes to improve
compute time and this version we can probably depreciate. I will discuss
piplein in tetain in v2.

## 4. Pipeline 2 --- Research (v2)

This is the full scientific pipeline. It produces the numbers and
statistics for the Phase-1 report.

**What it does, step by step:**

1.  **Calculate drought indices** --- for every grid cell and month, how
    dry is it versus normal? Three flavours:
    - **SPI** --- based on rainfall only
    - **SPEI (Thornthwaite)** --- rainfall minus a simple estimate of
      water loss
    - **SPEI (Penman-Monteith)** --- rainfall minus a more detailed
      water-loss estimate Each is computed at several "accumulation
      periods" (1, 3, 6, 12 months) and several severity thresholds →
      **75 configurations** in total.
2.  **Detect drought events** --- turn the monthly index into discrete
    events (start, end, duration, severity).
3.  **Summarise & analyse** --- frequency, duration, severity, area
    affected; then statistics: which definitions agree, how drought
    changed between the reference period (1973--2002) and the recent
    period (2003--2023), and a comparison with a separate "consecutive
    dry days" measure.

**What you get out:** event lists and summary tables (one set per
configuration per country), plus statistical results and a few figures.

> **Tech Notes** - Heavy, distributed steps are **Python** (`pandas_udf`
> / `applyInPandas`); statistics and figures are **R** (mature stats
> libraries). - It runs as a **single Databricks multi-task Job**
> (`drought_pipeline_``v2`) --- a dependency graph you trigger with one
> command. Index calc (01/02a/02b) → detection (03) → then sanity check
> (04), reference params (05), Friedman stats (06), cross-period (07),
> appendix figure (08), CDD comparison (09). - Runner notebooks
> (`_run_py_step`, `_run_r_step`) launch each script via a `STEP`
> parameter, so one job step = one script. - Cluster:
> `0616-154127-izvo5r0w` (DBR 15.4, multi-node). Benin validated: full
> run ≈ 38 min, 10/10 steps green.

## 5. Pipeline 3 --- Daily Monitor (app_monitor)

A lightweight, on-demand tool. Pick a country, one index, one threshold
→ get a **snapshot of drought** for that country. No heavy statistics.

**What you get out:**

1.  **A summary table** comparing the reference period (1973--2002) to
    the recent period (2003--2023):

  -----------------------------------------------------------
  Characteristic          Reference   Observation   \% Change
  ----------------------- ----------- ------------- ---------
  Frequency               ...         ...           ...
  (events/pixel/yr)                                 

  Duration (months)       ...         ...           ...

  Peak severity           ...         ...           ...

  Pixels affected (%)     ...         ...           ...
  -----------------------------------------------------------

2.  **Four figures:**

    - **Hotspot map** --- where droughts concentrate (per-pixel, clipped
      to country)
    - **Time-series** --- the index month by month, with the drought
      threshold
    - **Reference vs Observation bars** --- the four characteristics
      side by side
    - **Event timeline** --- drought events as bars over time for the
      busiest cells

**Same data as the research pipeline, but reusable for any country on
demand.**

> **For developers** - R + single-node personal cluster
> `0123-211315-fp4hw1e8` (DBR 16.4). - Index engine = R `SPEI` package
> (the research pipeline uses Python `climate_indices`, different
> library). Documented, acceptable. - Scripts: `00_APP_MASTER` (params)
> → `01` index → `02` detection → `03` table → `04` visuals. - Cluster
> init `init_app_geo.sh` installs terra/sf/units (best-effort, never
> blocks boot).

------------------------------------------------------------------------

## 6. How it's run & deployed

- Code lives on **GitHub** (`gwsc-drought-indices`, branch `main`).
- It is pushed to **Databricks** as a "bundle" and run there on
  clusters.
- Outputs are written to a shared **Volume**:
  `/Volumes/projects/gwsc_drought/drought_outputs/`.

## 7. Key decisions (for technical reviewers)

- **Three pipelines, not one** --- research (thorough, slow) and
  monitoring (fast, simple) have different needs; keeping them separate
  avoids one bloating the other. v1 retained only for traceability.
- **Hybrid Python + R for v2** --- Python where distribution gives real
  speed-up (per-pixel index fitting); R where the statistics libraries
  are stronger.
- **75 configurations** --- five thresholds × indices × accumulation
  periods, to test how sensitive the drought picture is to the
  definition chosen.

------------------------------------------------------------------------

## 8. What I'd like your feedback on

1.  **Daily monitor configuration** --- which single index /
    accumulation period / threshold should the operational monitor use?
    (research pipeline can inform this.)
2.  **Outputs** --- is the summary table + four figures the right set
    for daily use, or do you want different / additional views?
3.  **Country scope** --- current focus is the 6 study countries; what's
    the priority order for expansion?
4.  **Efficiency** --- any concerns on runtime / cost as we scale to
    more countries?

*Please add comments inline or in the PR. Thanks!*
