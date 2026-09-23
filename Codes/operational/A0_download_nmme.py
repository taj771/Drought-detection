"""
A0 -- fetch the most recent NMME monthly forecast from the OU archive.

Source: Kathy Pegion's NMME forecast archive at the University of Oklahoma,
https://weather.ou.edu/~kpegion/nmme/forecasts_test/data/

The discovery and download logic follows Sarva's `nmme_download` notebook in
/Users/stpulla@ua.edu/00_Template_Dbx/ -- list the directory index, match the
six-digit YYYYMM directories, take the newest, download with retries. The
Databricks-specific parts (dbutils widgets, YAML job config, Volume paths) are
dropped so this runs standalone on a laptop.

Why this exists rather than reading the Databricks volume: the volume copy
(projects.00_nmme_processing.retrospectives_june2024) is a frozen June 2024
snapshot, and the operational arm needs the current forecast. The OU archive also
keeps only a rolling window -- five directories at the time of writing -- so there
is no stable URL to pin to and the newest must be discovered each run.

By default only the precipitation file is fetched, since Stage A computes SPI,
which is precipitation-only. Pass --all-variables to mirror Sarva's notebook and
take the whole tree (temperature, SST, 200hPa heights, seasonal, tercile
probabilities) if a later stage needs them.

Files are written to Codes/operational/data/nmme_raw/<YYYYMM>/ and skipped if
already present, so re-running is cheap and does not re-fetch.
"""

import argparse
import os
import re
import threading
import time
from concurrent.futures import ThreadPoolExecutor
from pathlib import Path
from urllib.parse import urljoin

import requests
from requests.adapters import HTTPAdapter
from urllib3.util.retry import Retry

BASE_URL = "https://weather.ou.edu/~kpegion/nmme/forecasts_test/data/"
OUT_ROOT = Path("Codes/operational/data/nmme_raw")

# Stage A computes SPI, which needs precipitation only. tref_2m would be required
# if SPEI is ever added to the operational arm.
DEFAULT_PATTERN = "anom.monthly.prec_sfc.emean.nc"

MAX_WORKERS = 8

_thread_local = threading.local()


def get_session():
    """One requests.Session per thread, with retry on transient failures."""
    if not hasattr(_thread_local, "session"):
        retry = Retry(
            total=4,
            backoff_factor=1,
            status_forcelist=(429, 500, 502, 503, 504),
            allowed_methods=("GET",),
        )
        session = requests.Session()
        session.mount("https://", HTTPAdapter(max_retries=retry))
        _thread_local.session = session
    return _thread_local.session


def get_links(url):
    response = get_session().get(url, timeout=(10, 60))
    response.raise_for_status()
    return re.findall(r'href="([^"]+)"', response.text)


def get_latest_directory():
    """Newest YYYYMM directory in the archive.

    The archive holds a rolling window, so the newest has to be discovered rather
    than hardcoded. Lexical max works because the names are zero-padded YYYYMM.
    """
    directories = [link for link in get_links(BASE_URL) if re.fullmatch(r"\d{6}/", link)]
    if not directories:
        raise RuntimeError(f"no YYYYMM forecast directories found at {BASE_URL}")
    return urljoin(BASE_URL, max(directories))


def get_files(url, relative_path=""):
    """Walk the directory tree, returning (source_url, relative_target) pairs."""
    files = []
    for link in get_links(url):
        if link.startswith(("?", "/", "../")):
            continue
        source = urljoin(url, link)
        target = os.path.join(relative_path, link.rstrip("/"))
        if link.endswith("/"):
            files.extend(get_files(source, target))
        else:
            files.append((source, target))
    return files


def download_file(source, target):
    """Stream one file to disk, writing to a .part file and renaming on success.

    The rename is atomic, so an interrupted run leaves no truncated file that a
    later run would mistake for complete.
    """
    target = Path(target)
    if target.exists():
        print(f"  skip (exists) {target.name}")
        return

    target.parent.mkdir(parents=True, exist_ok=True)
    temporary = target.with_suffix(target.suffix + ".part")

    for attempt in range(3):
        try:
            with get_session().get(source, stream=True, timeout=(10, 300)) as response:
                response.raise_for_status()
                with open(temporary, "wb") as output:
                    for chunk in response.iter_content(1024 * 1024):
                        output.write(chunk)
            os.replace(temporary, target)
            size_mb = target.stat().st_size / 1e6
            print(f"  got {target.name} ({size_mb:.1f} MB)")
            return
        except (OSError, requests.RequestException) as exc:
            if temporary.exists():
                temporary.unlink()
            if attempt == 2:
                raise RuntimeError(f"failed after 3 attempts: {source}") from exc
            time.sleep(2 ** attempt)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--all-variables",
        action="store_true",
        help="download the whole forecast tree rather than precipitation only",
    )
    parser.add_argument(
        "--directory",
        help="specific YYYYMM directory to fetch instead of the newest available",
    )
    args = parser.parse_args()

    if args.directory:
        if not re.fullmatch(r"\d{6}", args.directory):
            raise SystemExit(f"--directory must be YYYYMM, got {args.directory!r}")
        latest_url = urljoin(BASE_URL, f"{args.directory}/")
    else:
        latest_url = get_latest_directory()

    forecast_id = latest_url.rstrip("/").rsplit("/", 1)[-1]
    print(f"forecast directory: {forecast_id}")
    print(f"source: {latest_url}")

    all_files = get_files(latest_url)
    if args.all_variables:
        wanted = all_files
    else:
        wanted = [(src, rel) for src, rel in all_files if rel.endswith(DEFAULT_PATTERN)]
        if not wanted:
            raise SystemExit(
                f"no file matching {DEFAULT_PATTERN!r} in {forecast_id}; "
                f"archive layout may have changed. Found {len(all_files)} files."
            )

    out_dir = OUT_ROOT / forecast_id
    print(f"downloading {len(wanted)} of {len(all_files)} files to {out_dir}")

    downloads = [(src, out_dir / rel) for src, rel in wanted]
    with ThreadPoolExecutor(max_workers=MAX_WORKERS) as executor:
        list(executor.map(lambda item: download_file(*item), downloads))

    print(f"done. forecast {forecast_id} in {out_dir}")


if __name__ == "__main__":
    main()
