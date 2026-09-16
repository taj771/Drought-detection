# Databricks notebook source
# Runner: exec one v2 Python pandas_udf step + report timing.
# STEP is read from a notebook widget (set per job task via base_parameters).
import time
from pyspark.sql import SparkSession
spark = SparkSession.builder.getOrCreate()

SCRIPT_DIR = "/Workspace/Users/tajayalath@ua.edu/Drought-indices/files/Codes/v2_pipeline/python"

dbutils.widgets.text("STEP", "")
STEP = dbutils.widgets.get("STEP")
if not STEP:
    raise ValueError("STEP widget is empty — set it via the task base_parameters")

# COUNTRY is injected into every step so all tasks in one job run agree on the
# country. Set it once as a job parameter; scripts must not hardcode a country.
dbutils.widgets.text("COUNTRY", "Benin")
COUNTRY = dbutils.widgets.get("COUNTRY").strip()
if not COUNTRY:
    raise ValueError("COUNTRY widget is empty — set it via the job parameters")
print(f"=== COUNTRY = {COUNTRY} ===")

# STEPS selects which tasks actually do work in this run. "all" runs everything.
# Otherwise it is a pipe-separated list of task keys (pipe, not comma: --params
# is itself comma-separated). A task not in the list exits here in seconds, so
# the DAG shape and downstream dependencies are unchanged — only the work is
# skipped. TASK comes from the task's own base_parameters.
dbutils.widgets.text("TASK", "")
TASK = dbutils.widgets.get("TASK").strip()
dbutils.widgets.text("STEPS", "all")
STEPS = dbutils.widgets.get("STEPS").strip()

if STEPS.lower() != "all":
    wanted = [s.strip() for s in STEPS.split("|") if s.strip()]
    if not TASK:
        raise ValueError(
            "STEPS is set but TASK is empty — this task cannot tell whether it "
            "was selected. Add TASK to its base_parameters."
        )
    if TASK not in wanted:
        print(f"=== SKIP {TASK}: not in STEPS ({STEPS}) ===")
        dbutils.notebook.exit(f"skipped:{TASK}")

path = f"{SCRIPT_DIR}/{STEP}"
print(f"=== Running {STEP} ===")
t0 = time.time()
with open(path) as f:
    code = f.read()
exec(compile(code, path, "exec"),
     {"spark": spark, "dbutils": dbutils, "COUNTRY": COUNTRY})
print(f"=== {STEP} done in {(time.time()-t0)/60:.1f} min ===")
