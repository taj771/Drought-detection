# =============================================================================
# P1. Download CRU TS4.08 PET (FAO-56 Penman-Monteith), clip Benin, monthly.
# =============================================================================
# Fixes the broken v2 SPEI-Penman PET. CRU TS pet = FAO-56 Penman-Monteith
# reference PET, monthly, 0.5 deg, 1901-2023 (Harris et al. 2020, Sci Data).
# ONE global gzipped NetCDF for the whole period -> download once, clip Benin,
# aggregate to the 154 ERA5 pixels, write parquet for SPEI-P recompute.
#
# LOCAL run with `py -3.13`. Needs a CEDA access token (Bearer) — set env var
# CEDA_TOKEN before running:  $env:CEDA_TOKEN = "eyJ..."
#
# In : CEDA archive cru_ts4.08 pet global .nc.gz
# Out: Data/cru_pet/cru_pet_benin_monthly.parquet  (x,y,year,month,pet_mm on 154 px)
#      + raw global file cached in Data/cru_pet/ (gitignored, large-ish)
# =============================================================================
import os, gzip, shutil, urllib.request, sys
import numpy as np, pandas as pd, xarray as xr

TOKEN = os.environ.get("CEDA_TOKEN", "").strip()
if not TOKEN:
    sys.exit("Set CEDA_TOKEN env var first: $env:CEDA_TOKEN = 'eyJ...'")

BASE = "https://dap.ceda.ac.uk/badc/cru/data/cru_ts/cru_ts_4.08/data/pet"
FN   = "cru_ts4.08.1901.2023.pet.dat.nc.gz"
URL  = f"{BASE}/{FN}"
OUTD = "Data/cru_pet"; os.makedirs(OUTD, exist_ok=True)
GZ   = os.path.join(OUTD, FN)
NC   = GZ[:-3]
PX   = "Codes/ndvi_validation/era5_pixels_benin.csv"   # 154 ERA5 px (x,y)
OUT  = os.path.join(OUTD, "cru_pet_benin_monthly.parquet")

# Benin window + ERA5 grid
LON0, LON1, LAT0, LAT1 = 0.5, 4.0, 6.0, 13.0
HALF = 0.125

# --- 1. download (Bearer token) ---------------------------------------------
if not os.path.exists(NC):
    if not os.path.exists(GZ):
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
print(f"CRU cells in Benin window: {df[['lon','lat']].drop_duplicates().shape[0]}")
print(f"pet_mm sample (Benin): mean {df['pet_mm'].mean():.1f} mm/mo "
      f"(expect ~100-160; Benin ~1500-1800/yr)")

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
print(f"one-pixel 2000 annual PET: {ann:.0f} mm/yr  (Benin expect ~1500-1800)")
print("P1 done.")
