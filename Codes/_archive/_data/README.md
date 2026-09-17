# `_data/` — local parquet cache

These files are **not tracked in git** (`.gitignore` excludes `*.parquet`). They are
local copies of pipeline outputs, pulled down from the Databricks Volume so that the
research scripts under `Codes/` can run on a laptop without a cluster.

If you have just cloned this repository, this directory will be empty and the scripts
listed below will fail with a missing-file error. Repopulate it as described under
*Regenerating* before running them.

Total size when populated: roughly 107 MB.

## Contents

| File | Source | Read by |
|------|--------|---------|
| `events.parquet` | v2 script 03 detection output, Benin | 33 references — the main input across most research areas |
| `events_eritrea.parquet` | as above, Eritrea | `agg_window`, `aridity_test` (two-country comparisons) |
| `spi_results.parquet` | v2 script 01 | 18 references |
| `speit.parquet` | v2 script 02a (SPEI-Thornthwaite) | 7 references |
| `spei_penman_CRU.parquet` | v2 script 02b, CRU TS 4.08 Penman PET | `spei_audit` |
| `spei_penman_results.parquet` | v2 script 02b | `spei_audit` |
| `annual_summary.parquet` | v2 script 03 assembly | 3 references |
| `summary_yearly.parquet` | v2 script 03 assembly | `reports` |
| `ndvi_anom_benin_monthly.parquet` | `ndvi_validation/N1`, from `aer-processed.ndvi_anom_10km` | `ndvi_validation`, `nostd_freq` |
| `svi_benin_monthly.parquet` | `ndvi_validation/N2` (derived from the above) | `ndvi_validation` |
| `sm_benin_monthly.parquet` | `sm_validation` SM1/SM2, ERA5-Land root-zone soil moisture via CDS | `sm_validation`, `nostd_freq` |

### Superseded — safe to delete

| File | Why |
|------|-----|
| `events_OLD_brokenPET.parquet` | Produced before the SPEI-Penman PET fix (ERA5-Land `pev` was pan-evaporation plus a unit error, replaced by CRU TS 4.08). Referenced by nothing. |
| `spei_penman_results_OLD.parquet` | Same PET bug. Referenced by nothing. |

Together these are about 29 MB. They are kept only as a record of the pre-fix state;
no script reads them, and any number computed from them is wrong.

## Regenerating

Most of these are v2 pipeline outputs written to the Volume:

```
/Volumes/projects/gwsc_drought/drought_outputs/v2/<COUNTRY>/
```

Run the pipeline for the country you need, then copy the parquet down:

```bash
databricks bundle run drought_pipeline_v2 --target dev --params COUNTRY=Benin
databricks fs cp \
  dbfs:/Volumes/projects/gwsc_drought/drought_outputs/v2/Benin/all_drought_events.parquet \
  Codes/_archive/_data/events.parquet
```

Note the filename changes on the way down — the scripts here expect the shorter names
in the table above, not the pipeline's own output names.

The three validation datasets come from elsewhere and are not v2 outputs:

- `ndvi_anom_benin_monthly.parquet` — run `Codes/ndvi_validation/N1_*`. Reads
  `aer-processed.ndvi_anom_10km`, which is **read-only and must stay on Databricks**;
  the N1 script does the aggregation in-cluster and brings down only the monthly series.
- `svi_benin_monthly.parquet` — run `Codes/ndvi_validation/N2_build_svi.R` once the
  NDVI file above exists.
- `sm_benin_monthly.parquet` — run the `Codes/sm_validation` SM1 and SM2 steps. SM1
  downloads ERA5-Land soil moisture from CDS and needs a working `ecmwfr` API key.

## Why these are not in git

They are derived data, they total over 100 MB, and the repository history is already
large. Committing them would grow every future clone for no benefit, since anyone with
Databricks access can regenerate them. Scripts reference this directory by relative
path from the repository root, so run them with the repository root as the working
directory.
