# =============================================================================
# P0. hPET remote-clip TEST — can we slice Benin from data.bris without full DL?
# =============================================================================
# hPET = FAO-56 Penman-Monteith PET from ERA5-Land, 0.1 deg, daily, 1981-present
# (Singer et al. 2021, Sci Data). Global daily files ~2.25 GB each — too big to
# download all 43 yrs. Try opening ONE file remotely and reading only the Benin
# window via HTTP byte-range (NetCDF4 = HDF5, h5netcdf + fsspec can range-read).
#
# Run LOCAL with `py -3.13` (miniconda + xarray/fsspec/h5netcdf). Read-only, no
# full DL. If this pulls only a few MB for the Benin slice -> viable; loop next.
#
# Benin window: lon 0.5-4.0 E, lat 6-13 N (matches ERA5 154-px extent + buffer).
# =============================================================================

import time, sys

URL = "https://data.bris.ac.uk/datasets/qb8ujazzda0s2aykkv0oq0ctp/2000_daily_pet.nc"
LON0, LON1 = 0.5, 4.0
LAT0, LAT1 = 6.0, 13.0

L = []
def p(*a): L.append(" ".join(str(x) for x in a))

# --- 1. install deps if missing ---------------------------------------------
try:
    import fsspec, h5netcdf, xarray as xr
except Exception as e:
    import subprocess
    subprocess.check_call([sys.executable, "-m", "pip", "install", "-q",
                           "fsspec", "aiohttp", "h5netcdf", "xarray"])
    import fsspec, h5netcdf, xarray as xr

# --- 2. check server supports byte-range ------------------------------------
import urllib.request
req = urllib.request.Request(URL, method="HEAD")
with urllib.request.urlopen(req, timeout=60) as r:
    accept_ranges = r.headers.get("Accept-Ranges")
    total_mb = int(r.headers.get("Content-Length", 0)) / 1e6
p(f"Accept-Ranges: {accept_ranges}   file size: {total_mb:.0f} MB")

# --- 3. remote open + Benin slice (range reads via fsspec) ------------------
t0 = time.time()
# fsspec http filesystem with block caching -> only fetches needed byte ranges
of = fsspec.open(URL, mode="rb")
ds = xr.open_dataset(of.open(), engine="h5netcdf")
p("dims:", dict(ds.sizes))
p("data vars:", list(ds.data_vars))
# find coord names
latn = [c for c in ds.coords if c.lower() in ("lat","latitude","y")][0]
lonn = [c for c in ds.coords if c.lower() in ("lon","longitude","x")][0]
p("coord names:", latn, lonn)
lat = ds[latn].values; lon = ds[lonn].values
p(f"lat {lat.min():.2f}..{lat.max():.2f} (desc={lat[0]>lat[-1]})  "
  f"lon {lon.min():.2f}..{lon.max():.2f}")

var = list(ds.data_vars)[0]
# slice Benin (handle possible descending lat); single indexer dict
lat_sl = slice(LAT1, LAT0) if lat[0] > lat[-1] else slice(LAT0, LAT1)
sub = ds[var].sel({latn: lat_sl, lonn: slice(LON0, LON1)})
vals = sub.load().values           # forces the actual range fetch
dt = time.time() - t0
p(f"Benin slice shape: {vals.shape}  fetched in {dt:.1f}s")
import numpy as np
p(f"value range: [{np.nanmin(vals):.3f}, {np.nanmax(vals):.3f}]  "
  f"mean {np.nanmean(vals):.3f}")
p(f"n NaN: {int(np.isnan(vals).sum())} of {vals.size}")

ds.close()
out = "\n".join(L)
print(out)
try:
    dbutils.notebook.exit(out)   # Databricks return
except NameError:
    pass                          # local run: plain print above
