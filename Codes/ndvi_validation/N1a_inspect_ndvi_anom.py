# =============================================================================
# N1a. Inspect one NDVI-anomaly NetCDF (structure discovery)
# =============================================================================
# Run on a Databricks cluster (files live on the aer-processed Volume).
# Goal: learn variable names, grid, scale factor, fill value BEFORE writing the
# clip/aggregate extract (N1b). Read-only. Touches nothing.
#
# Data: MODIS Terra GMOD09Q1 NDVI anomaly, 8-day, global latlon, C6.1,
#       baseline S2001-2023. Volume: aer-processed.ndvi.ndvi_anom_10km
# =============================================================================

import xarray as xr
import glob, os

VOL = "/Volumes/aer-processed/ndvi/ndvi_anom_10km"
BASELINE = "S2001-2023"   # the baseline version we standardise on

L = []                                   # collect lines -> exit as one string
def p(*a): L.append(" ".join(str(x) for x in a))

# pick one file (2010, first 8-day composite, chosen baseline)
year = "2010"
cands = sorted(glob.glob(f"{VOL}/{year}/*{BASELINE}*.nc"))
p(f"files matching {BASELINE} in {year}: {len(cands)}")
f = cands[0]
p("inspecting:", os.path.basename(f))

ds = xr.open_dataset(f)

p("\n=== DATASET ===")
p(repr(ds))
p("\n=== DATA VARS ===")
for v in ds.data_vars:
    da = ds[v]
    p(f"  {v}: dims={da.dims} shape={da.shape} dtype={da.dtype}")
    p(f"     attrs: {dict(da.attrs)}")

p("\n=== COORDS ===")
for c in ds.coords:
    ca = ds[c]
    p(f"  {c}: shape={ca.shape} range=[{float(ca.min())}, {float(ca.max())}]")

# grid spacing (to compare vs ERA5 0.25deg)
for latname in ["lat", "latitude", "y"]:
    if latname in ds.coords:
        lat = ds[latname].values
        p(f"\n  lat step (~res deg): {abs(lat[1]-lat[0]):.4f}  n={len(lat)}")
        break
for lonname in ["lon", "longitude", "x"]:
    if lonname in ds.coords:
        lon = ds[lonname].values
        p(f"  lon step (~res deg): {abs(lon[1]-lon[0]):.4f}  n={len(lon)}")
        p(f"  lon convention: min={lon.min():.3f} max={lon.max():.3f} "
          f"({'0-360' if lon.max() > 180 else '-180..180'})")
        break

# peek Benin window (lon 0.5-4.0 E, lat 6-13 N) to confirm data present + values
p("\n=== BENIN WINDOW SAMPLE ===")
vname = list(ds.data_vars)[0]
try:
    sub = ds[vname].sel(lat=slice(13, 6), lon=slice(0.5, 4.0))  # lat may be desc
    if sub.size == 0:
        sub = ds[vname].sel(lat=slice(6, 13), lon=slice(0.5, 4.0))
    p(f"  {vname} Benin window shape: {sub.shape}")
    p(f"  value range: [{float(sub.min())}, {float(sub.max())}], "
      f"mean={float(sub.mean()):.3f}")
    p(f"  n NaN: {int(sub.isnull().sum())} of {sub.size}")
except Exception as e:
    p("  window sel failed (check coord names/order):", e)

ds.close()
p("\nN1a inspect done.")

out = "\n".join(L)
print(out)
dbutils.notebook.exit(out)          # return text via run-output API
