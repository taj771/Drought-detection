# =============================================================================
# P1. Download CRU TS4.08 PET (FAO-56 Penman-Monteith), clip a country, monthly.
# =============================================================================
# Fixes the broken v2 SPEI-Penman PET. CRU TS pet = FAO-56 Penman-Monteith
# reference PET, monthly, 0.5 deg, 1901-2023 (Harris et al. 2020, Sci Data).
# ONE global gzipped NetCDF for the whole period -> download once, clip the
# country, aggregate to its ERA5 pixels, write parquet for SPEI-P recompute.
#
# ADDING A COUNTRY: put its ERA5 pixel list at Data/cru_pet/era5_pixels_<lc>.csv
# (columns x,y) and set COUNTRY below. The clip window is derived from that file,
# so nothing else needs editing. The global download is cached and reused.
#
# LOCAL run with `py -3.13`. Needs a CEDA access token (Bearer) — set env var
# CEDA_TOKEN before running:  $env:CEDA_TOKEN = "eyJ..."
#
# In : CEDA archive cru_ts4.08 pet global .nc.gz
#      Data/cru_pet/era5_pixels_<country>.csv
# Out: Data/cru_pet/cru_pet_<country>_monthly.parquet  (x,y,year,month,pet_mm)
#      + raw global file cached in Data/cru_pet/ (gitignored, large-ish)
# =============================================================================
import os, gzip, shutil, urllib.request, sys
import numpy as np, pandas as pd, xarray as xr

# -- USER SETTING -------------------------------------------------------------
COUNTRY = os.environ.get("COUNTRY", "Eritrea")
# -----------------------------------------------------------------------------

TOKEN = os.environ.get("CEDA_TOKEN", "").strip()

BASE = "https://dap.ceda.ac.uk/badc/cru/data/cru_ts/cru_ts_4.08/data/pet"
FN   = "cru_ts4.08.1901.2023.pet.dat.nc.gz"
URL  = f"{BASE}/{FN}"
OUTD = "Data/cru_pet"; os.makedirs(OUTD, exist_ok=True)
GZ   = os.path.join(OUTD, FN)
NC   = GZ[:-3]
LC   = COUNTRY.lower().replace(" ", "_")
# Benin's list predates this script and lives with the NDVI work; later countries
# get theirs in Data/cru_pet/. Accept either location.
PX_CANDIDATES = [
    os.path.join(OUTD, f"era5_pixels_{LC}.csv"),
    f"Codes/ndvi_validation/era5_pixels_{LC}.csv",
]
PX = next((p for p in PX_CANDIDATES if os.path.exists(p)), None)
if PX is None:
    sys.exit(f"No ERA5 pixel list for {COUNTRY}. Looked in: {PX_CANDIDATES}")
OUT  = os.path.join(OUTD, f"cru_pet_{LC}_monthly.parquet")

# Clip window derived from the pixel list, padded by one CRU cell (0.5 deg) so
# every ERA5 pixel has CRU neighbours to draw from at the country edge.
HALF = 0.125
_px_bb = pd.read_csv(PX)
LON0, LON1 = _px_bb.x.min() - 0.5, _px_bb.x.max() + 0.5
LAT0, LAT1 = _px_bb.y.min() - 0.5, _px_bb.y.max() + 0.5
print(f"{COUNTRY}: {len(_px_bb)} ERA5 px | window "
      f"lon [{LON0:.2f}, {LON1:.2f}] lat [{LAT0:.2f}, {LAT1:.2f}]")

# --- 1. download (Bearer token) ---------------------------------------------
if not os.path.exists(NC):
    if not os.path.exists(GZ):
        # Token only matters here. The global file is one download for ALL
        # countries, so once it is cached later countries need no credential.
        if not TOKEN:
            sys.exit("Set CEDA_TOKEN env var first: $env:CEDA_TOKEN = 'eyJ...'")
        print("downloading CRU pet (global, whole period)...")
        req = urllib.request.Request(URL, headers={"Authorization": f"Bearer {TOKEN}"})
        with urllib.request.urlopen(req, timeout=600) as r, open(GZ, "wb") as f:
            shutil.copyfileobj(r, f)
        print(f"downloaded {os.path.getsize(GZ)/1e6:.0f} MB")
    print("gunzip...")
    with gzip.open(GZ, "rb") as fi, open(NC, "wb") as fo:
        shutil.copyfileobj(fi, fo)
print("nc ready:", NC, f"{os.path.getsize(NC)/1e6:.0f} MB")

# --- 2. open + clip Benin ----------------------------------------------------
ds = xr.open_dataset(NC)
pv = "pet" if "pet" in ds.data_vars else list(ds.data_vars)[0]
print("var:", pv, " units:", ds[pv].attrs.get("units"),
      " long_name:", ds[pv].attrs.get("long_name"))
latn = [c for c in ds.coords if c.lower() in ("lat","latitude","y")][0]
lonn = [c for c in ds.coords if c.lower() in ("lon","longitude","x")][0]
lat = ds[latn].values
lat_sl = slice(LAT1, LAT0) if lat[0] > lat[-1] else slice(LAT0, LAT1)
sub = ds[pv].sel({latn: lat_sl, lonn: slice(LON0, LON1)})
df = sub.to_dataframe(name="pet").reset_index()
df["year"]  = pd.to_datetime(df["time"]).dt.year
df["month"] = pd.to_datetime(df["time"]).dt.month
df = df.rename(columns={lonn: "lon", latn: "lat"}).dropna(subset=["pet"])
# CRU pet units = mm/day -> monthly total mm = pet * days_in_month
dim = pd.to_datetime(df["time"]).dt.days_in_month
if str(ds[pv].attrs.get("units","")).lower() in ("mm/day","mm day-1","mm day^-1"):
    df["pet_mm"] = df["pet"] * dim
else:
    df["pet_mm"] = df["pet"]     # already monthly total (verify from magnitude below)
ds.close()
print(f"CRU cells in {COUNTRY} window: {df[['lon','lat']].drop_duplicates().shape[0]}")
print(f"pet_mm sample ({COUNTRY}): mean {df['pet_mm'].mean():.1f} mm/mo "
      f"(sub-humid ~100-160/mo; arid higher)")

# --- 3. aggregate CRU 0.5deg -> ERA5 154 px (mean of CRU cells in each 0.25 box)
px = pd.read_csv(PX)
def nearest_px(lo, la):
    m = (np.abs(px.x - lo) <= HALF*2) & (np.abs(px.y - la) <= HALF*2)  # 0.5deg CRU
    return px[m]
# assign each ERA5 px the nearest CRU cell value (CRU coarser -> nearest works)
rows = []
crd = df[["lon","lat"]].drop_duplicates().values
for _, r in px.iterrows():
    d2 = (crd[:,0]-r.x)**2 + (crd[:,1]-r.y)**2
    clo, cla = crd[d2.argmin()]
    sub2 = df[(df.lon==clo) & (df.lat==cla)][["year","month","pet_mm"]].copy()
    sub2["x"] = r.x; sub2["y"] = r.y
    rows.append(sub2)
out = pd.concat(rows, ignore_index=True)[["x","y","year","month","pet_mm"]]
out.to_parquet(OUT, index=False)
print(f"\nwrote {OUT}: {len(out)} rows, {out[['x','y']].drop_duplicates().shape[0]} px, "
      f"{out.year.min()}-{out.year.max()}")
ann = out[(out.x==out.x.iloc[0])&(out.y==out.y.iloc[0])&(out.year==2000)]["pet_mm"].sum()
print(f"one-pixel 2000 annual PET: {ann:.0f} mm/yr")
# Sanity gate: PET must separate climates. Benin (sub-humid) ~1500-1800 mm/yr;
# an arid country should land well above that. If an arid country returns a
# Benin-like number, the clip or the nearest-cell join is wrong — stop, do not
# feed it to 02b.
print("  expect: sub-humid ~1500-1800 | arid ~1800-2500")
print("P1 done.")
