# Databricks notebook source
# Greece app smoke-test: diag log + output csv pixel count.
app = "/Volumes/projects/gwsc_drought/drought_outputs/app_outputs"
import os
out = []
log = open(f"{app}/app_diag_log.txt").read().replace("\n"," | ") if os.path.exists(f"{app}/app_diag_log.txt") else "no log"
out.append(f"app_log=[{log}]")
csvs = [f for f in os.listdir(app) if "Greece" in f and f.endswith(".csv")]
out.append(f"greece_csvs={csvs}")
dbutils.notebook.exit(" || ".join(out))
