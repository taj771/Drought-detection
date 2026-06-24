# =============================================================================
# 00_v2. MASTER RUN — Python Pandas UDF Pipeline
# =============================================================================
# Controls which scripts run. Set flag to True to enable.
# Run one at a time: enable one, run job, verify output, enable next.
#
# Cluster: multi-node 0616-154127-izvo5r0w (drought-r-spark-apply)
# Output:  /Volumes/projects/gwsc_drought/drought_outputs_v2/
# =============================================================================

# -- RUN FLAGS ----------------------------------------------------------------
RUN_SPI    = False  # 01_v2_SPI_pandas_udf.py (done)
RUN_SPEI_T = True   # 02a_v2_SPEI_Thornthwaite_pandas_udf.py
RUN_SPEI_P = True   # 02b_v2_SPEI_Penman_pandas_udf.py
# -----------------------------------------------------------------------------

VOLUME_PATH = "/Volumes/projects/gwsc_drought/drought_outputs/v2"

from pyspark.sql import SparkSession
spark = SparkSession.builder.getOrCreate()

import time, os, json
from datetime import datetime, timezone

SCRIPT_DIR = "/Workspace/Users/tajayalath@ua.edu/Drought-indices/files/Codes"
TIMING_LOG = f"{VOLUME_PATH}/pipeline_timing_log.jsonl"

def run_script(path):
    print(f"\n{'='*60}")
    print(f"Running: {path}")
    print(f"{'='*60}")
    t0 = time.time()
    with open(path) as f:
        code = f.read()
    exec(compile(code, path, "exec"), {"spark": spark})
    elapsed = (time.time() - t0) / 60
    print(f"\nDone: {path} ({elapsed:.1f} min)")
    return elapsed

def log_timing(script, elapsed_min, country_filter, cluster_id, n_workers):
    """Append one JSON line to timing log for future cost comparison."""
    os.makedirs(VOLUME_PATH, exist_ok=True)
    entry = {
        "timestamp":      datetime.now(timezone.utc).isoformat(),
        "script":         script,
        "elapsed_min":    round(elapsed_min, 2),
        "country_filter": country_filter,
        "cluster_id":     cluster_id,
        "n_workers":      n_workers,
        "method":         "pandas_udf",
        "runtime":        "python",
    }
    # UC Volumes reject append-mode writes (OSError Errno 95). Read existing
    # content + rewrite the whole file instead of opening in "a" mode.
    prior = ""
    if os.path.exists(TIMING_LOG):
        with open(TIMING_LOG, "r") as f:
            prior = f.read()
    with open(TIMING_LOG, "w") as f:
        f.write(prior + json.dumps(entry) + "\n")
    print(f"Timing logged: {entry}")

# -- Edit these to match actual cluster used ----------------------------------
CLUSTER_ID = "0616-154127-izvo5r0w"   # multi-node drought-r-spark-apply
N_WORKERS  = 4                          # number of worker nodes
COUNTRIES  = ["Benin"]                  # must match COUNTRY_FILTER in each script
# -----------------------------------------------------------------------------

timings = {}

if RUN_SPI:
    elapsed = run_script(f"{SCRIPT_DIR}/01_v2_SPI_pandas_udf.py")
    timings["SPI"] = elapsed
    log_timing("01_v2_SPI_pandas_udf.py", elapsed, COUNTRIES, CLUSTER_ID, N_WORKERS)

if RUN_SPEI_T:
    elapsed = run_script(f"{SCRIPT_DIR}/02a_v2_SPEI_Thornthwaite_pandas_udf.py")
    timings["SPEI_T"] = elapsed
    log_timing("02a_v2_SPEI_Thornthwaite_pandas_udf.py", elapsed, COUNTRIES, CLUSTER_ID, N_WORKERS)

if RUN_SPEI_P:
    elapsed = run_script(f"{SCRIPT_DIR}/02b_v2_SPEI_Penman_pandas_udf.py")
    timings["SPEI_P"] = elapsed
    log_timing("02b_v2_SPEI_Penman_pandas_udf.py", elapsed, COUNTRIES, CLUSTER_ID, N_WORKERS)

print("\n" + "="*60)
print("PIPELINE COMPLETE")
for k, v in timings.items():
    print(f"  {k}: {v:.1f} min")
print(f"  TOTAL: {sum(timings.values()):.1f} min")
print(f"  Timing log: {TIMING_LOG}")
print("="*60)
