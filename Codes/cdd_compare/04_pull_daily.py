"""Pull daily Benin precipitation for the consecutive-dry-days row.

Read-only against the ERA5 daily catalog table. The dry-day flag is evaluated
in SQL so only one row per pixel-month comes back rather than 4M daily rows:
for each pixel and month we need the days that sit inside a run of at least
seven consecutive dry days, and runs are found with the standard
"row_number difference" trick over the daily sequence.
"""
import json
import subprocess
import pandas as pd
from databricks import sql

HOST = "dbc-9c907c32-f434.cloud.databricks.com"
PATH = "/sql/1.0/warehouses/a6d21311197b1742"
TABLE = "projects.xx_drought_index.drought_index_dailysummary"
OUT = "Codes/cdd_compare/out"

DRY_THRESHOLD = 1     # mm/day; <= this is a dry day
CDD_MIN = 7           # a CDD event is a run of at least this many dry days


def token():
    r = subprocess.run(["databricks", "auth", "token", "--profile", "DEFAULT"],
                       capture_output=True, text=True, check=True)
    return json.loads(r.stdout)["access_token"]


QUERY = f"""
WITH daily AS (
  SELECT x, y, time,
         CASE WHEN precip <= {DRY_THRESHOLD} THEN 1 ELSE 0 END AS dry
  FROM {TABLE}
  WHERE country = 'Benin' AND year BETWEEN 1970 AND 2023
),
grp AS (
  SELECT x, y, time, dry,
         ROW_NUMBER() OVER (PARTITION BY x, y ORDER BY time)
       - ROW_NUMBER() OVER (PARTITION BY x, y, dry ORDER BY time) AS run_id
  FROM daily
),
runs AS (
  SELECT x, y, run_id, dry, COUNT(*) AS run_len,
         MIN(time) AS run_start, MAX(time) AS run_end
  FROM grp WHERE dry = 1
  GROUP BY x, y, run_id, dry
  HAVING COUNT(*) >= {CDD_MIN}
)
SELECT x, y,
       YEAR(run_start) AS start_year, MONTH(run_start) AS start_month,
       run_len, run_start, run_end
FROM runs
ORDER BY x, y, run_start
"""


def main():
    with sql.connect(server_hostname=HOST, http_path=PATH,
                     access_token=token()) as conn:
        with conn.cursor() as cur:
            print("querying CDD runs ...")
            cur.execute(QUERY)
            df = cur.fetchall_arrow().to_pandas()

    print(f"{len(df):,} CDD runs (>= {CDD_MIN} dry days), "
          f"{df[['x','y']].drop_duplicates().shape[0]} pixels")
    print(f"run length: median {df.run_len.median():.0f}, max {df.run_len.max()}")
    df.to_parquet(f"{OUT}/cdd_runs_benin.parquet")
    print("wrote", f"{OUT}/cdd_runs_benin.parquet")


if __name__ == "__main__":
    main()
