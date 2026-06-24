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

path = f"{SCRIPT_DIR}/{STEP}"
print(f"=== Running {STEP} ===")
t0 = time.time()
with open(path) as f:
    code = f.read()
exec(compile(code, path, "exec"), {"spark": spark, "dbutils": dbutils})
print(f"=== {STEP} done in {(time.time()-t0)/60:.1f} min ===")
