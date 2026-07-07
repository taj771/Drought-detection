# =============================================================================
# N1b. Extract NDVI anomaly for Benin -> aggregate to ERA5 154 pixels -> monthly
# =============================================================================
# Run on Databricks (personal cluster 0123-211315-fp4hw1e8). Read-only on the
# aer-processed Volume; writes ONE parquet to our projects Volume.
#
# Approach A (locked): aggregate NDVI 0.09deg cells -> ERA5 0.25deg pixels
# (mean of NDVI cells whose centre falls inside each ERA5 cell's 0.25deg box),
# so the output shares the exact 154-pixel grid as SPI/SPEI -> clean join.
#
# Input : /Volumes/aer-processed/ndvi/ndvi_anom_10km/<year>/*S2001-2023*10km.nc
#         var 'ndvi_anom', dims (lat,lon), 0.09deg, lon -180..180, 46 files/yr
# Grid  : ERA5 154 Benin pixels, 0.25deg, centres .125/.375/.625/.875,
#         x 0.875-3.625, y 6.375-12.375 (all positive lon, matches NDVI)
# Output: /Volumes/projects/gwsc_drought/drought_outputs/v2/ndvi_anom_benin_monthly.parquet
#         cols: x, y, year, month, ndvi_anom (mean over 8-day composites in month)
# Years : 2000-2023 (full record; SVI baseline built in N2)
# =============================================================================

import xarray as xr
import numpy as np
import pandas as pd
import glob, os, re

VOL_IN   = "/Volumes/aer-processed/ndvi/ndvi_anom_10km"
OUT_DIR  = "/Volumes/projects/gwsc_drought/drought_outputs/v2"
OUT      = f"{OUT_DIR}/ndvi_anom_benin_monthly.parquet"
PX_CSV   = f"{OUT_DIR}/era5_pixels_benin.csv"   # uploaded 154-px grid
BASELINE = "S2001-2023"
# smoke-test override: set widget NDVI_YEARS="2010" to run one year first
try:
    _yr = dbutils.widgets.get("NDVI_YEARS")
except Exception:
    _yr = ""
if _yr.strip():
    yy = [int(s) for s in _yr.split("-")]
    Y0, Y1 = (yy[0], yy[0]) if len(yy) == 1 else (yy[0], yy[1])
    OUT = f"{OUT_DIR}/ndvi_anom_benin_monthly_TEST{Y0}.parquet"
else:
    Y0, Y1 = 2000, 2023
HALF     = 0.125           # ERA5 cell half-width (0.25deg / 2)

# --- ERA5 target pixels -------------------------------------------------------
px = pd.read_csv(PX_CSV)                       # cols x, y (154 rows)
px_x = px["x"].to_numpy(); px_y = px["y"].to_numpy()
xmin, xmax = px_x.min()-HALF, px_x.max()+HALF
ymin, ymax = px_y.min()-HALF, px_y.max()+HALF
print(f"ERA5 pixels: {len(px)}  bbox x[{xmin},{xmax}] y[{ymin},{ymax}]")

def doy_to_month(year, doy):
    return (pd.Timestamp(f"{year}-01-01") + pd.Timedelta(days=int(doy)-1)).month

# --- aggregate one file to the 154 ERA5 pixels --------------------------------
def agg_file(f):
    ds = xr.open_dataset(f)
    da = ds["ndvi_anom"].sel(lat=slice(ymax, ymin), lon=slice(xmin, xmax))  # lat desc
    lat = da["lat"].to_numpy(); lon = da["lon"].to_numpy()
    arr = da.to_numpy()                       # (nlat, nlon)
    ds.close()
    # assign each NDVI cell to nearest ERA5 pixel centre, keep only within HALF box
    out = np.full(len(px), np.nan)
    cnt = np.zeros(len(px))
    acc = np.zeros(len(px))
    # snap NDVI cell coords to ERA5 grid: nearest 0.25 centre
    for j, la in enumerate(lat):
        # nearest ERA5 y centre
        iy = np.round((la - 6.375) / 0.25) * 0.25 + 6.375
        for i, lo in enumerate(lon):
            v = arr[j, i]
            if not np.isfinite(v):
                continue
            ix = np.round((lo - 0.875) / 0.25) * 0.25 + 0.875
            # find matching ERA5 pixel
            m = np.where((np.abs(px_x - ix) < 1e-6) & (np.abs(px_y - iy) < 1e-6))[0]
            if m.size and abs(lo - ix) <= HALF and abs(la - iy) <= HALF:
                acc[m[0]] += v; cnt[m[0]] += 1
    good = cnt > 0
    out[good] = acc[good] / cnt[good]
    return out

# --- loop years/files ---------------------------------------------------------
rows = []
for year in range(Y0, Y1 + 1):
    files = sorted(glob.glob(f"{VOL_IN}/{year}/*{BASELINE}*10km.nc"))
    if not files:
        print(f"  {year}: no {BASELINE} files, skip"); continue
    # month -> list of per-pixel arrays (8-day composites)
    mbins = {m: [] for m in range(1, 13)}
    for f in files:
        m = re.search(r"\.A(\d{4})(\d{3})\.", os.path.basename(f))
        doy = int(m.group(2))
        mon = doy_to_month(year, doy)
        mbins[mon].append(agg_file(f))
    for mon, lst in mbins.items():
        if not lst:
            continue
        month_mean = np.nanmean(np.vstack(lst), axis=0)   # mean over composites
        for k in range(len(px)):
            if np.isfinite(month_mean[k]):
                rows.append((float(px_x[k]), float(px_y[k]), year, mon,
                             float(month_mean[k])))
    print(f"  {year}: {len(files)} files -> rows so far {len(rows)}")

df = pd.DataFrame(rows, columns=["x", "y", "year", "month", "ndvi_anom"])
print(f"\nTotal rows: {len(df)}  pixels: {df[['x','y']].drop_duplicates().shape[0]}")
print(df.head())
df.to_parquet(OUT, index=False)
print(f"Wrote {OUT}")

summary = (f"N1b done. rows={len(df)} "
           f"pixels={df[['x','y']].drop_duplicates().shape[0]} "
           f"years={df.year.min()}-{df.year.max()} "
           f"ndvi_anom range=[{df.ndvi_anom.min():.3f},{df.ndvi_anom.max():.3f}]")
print(summary)
dbutils.notebook.exit(summary)
