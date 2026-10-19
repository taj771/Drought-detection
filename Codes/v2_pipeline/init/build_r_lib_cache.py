# Databricks notebook source
# One-off: compile the R geo packages once and archive them to the Volume, so
# init_geo_libs.sh can unpack them at boot instead of compiling for ~20 min.
#
# Run on a fresh cluster with the SAME runtime as the pipeline job cluster and
# WITHOUT init_geo_libs.sh attached (otherwise the packages are already
# installed and the build directory would be incomplete). Rerun after any
# runtime upgrade or package pin change; the archive name carries the DBR and
# R versions, so an outdated archive is never picked up by a newer runtime.

import os
import subprocess

notebook_dir = os.path.dirname(
    dbutils.notebook.entry_point.getDbutils().notebook().getContext().notebookPath().get()
)
script = f"/Workspace{notebook_dir}/init_geo_libs.sh"
build_dir = "/local_disk0/r_geo_build"
os.makedirs(build_dir, exist_ok=True)

result = subprocess.run(
    ["bash", script],
    env=dict(os.environ, BUILD_R_LIB_TO=build_dir),
    capture_output=True,
    text=True,
)
print(result.stdout[-5000:])
print(result.stderr[-5000:])
if result.returncode != 0:
    raise RuntimeError(f"init_geo_libs.sh build mode failed (exit {result.returncode})")

cache_line = [l for l in result.stdout.splitlines() if l.startswith("CACHE_TAR=")]
dbutils.notebook.exit(cache_line[-1] if cache_line else "built, path not reported")
