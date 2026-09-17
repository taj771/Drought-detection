# run_app.ps1 — GWSC Drought ERA5 Monitor — one-command launcher
# Run from project root after editing APP_COUNTRY in Codes/App/00_APP_MASTER.Rmd
#
# Usage: .\run_app.ps1

$ErrorActionPreference = "Stop"
$MasterRmd  = "Codes\App\00_APP_MASTER.Rmd"
$RScript    = "C:\Program Files\R\R-4.6.0\bin\Rscript.exe"
$VolBase    = "dbfs:/Volumes/projects/gwsc_drought/drought_outputs/app_outputs/shapefiles"

# -- Read APP_COUNTRY from MASTER ---------------------------------------------
$line    = (Get-Content $MasterRmd | Select-String 'APP_COUNTRY\s*<-').Line
$Country = ($line -replace '.*APP_COUNTRY\s*<-\s*"([^"]+)".*', '$1').Trim()
Write-Host "=== GWSC Drought ERA5 Monitor ===" -ForegroundColor Cyan
Write-Host "Country: $Country"

# -- Check if boundary already on Volume --------------------------------------
$Slug    = $Country -replace " ", "_"
$VolPath = "$VolBase/$Slug/boundary.gpkg"
try { $Exists = (databricks fs ls "$VolBase/$Slug/") -match "boundary.gpkg" }
catch { $Exists = $false }

if ($Exists) {
    Write-Host "[1/3] Boundary cached on Volume - skipping download." -ForegroundColor Green
} else {
    Write-Host "[1/3] Downloading boundary for '$Country'..." -ForegroundColor Yellow
    $TmpFile    = "$env:TEMP\${Slug}_boundary.gpkg"
    $TmpScript  = "$env:TEMP\dl_boundary_tmp.R"
    $TmpFileFwd = $TmpFile -replace "\\", "/"

    # Build R script as array of strings — avoids PowerShell parsing R syntax
    $RLines = @(
        'library(sf)',
        "b <- rnaturalearth::ne_countries(country=`"$Country`", returnclass=`"sf`", scale=`"medium`")",
        'if (nrow(b) == 0) stop("Country not found")',
        'b <- b[, c("name", "iso_a3", "geometry")]',
        "sf::st_write(b, `"$TmpFileFwd`", driver=`"GPKG`", quiet=TRUE, delete_dsn=TRUE)",
        'cat("Done:", nrow(b), "features\n")'
    )
    [System.IO.File]::WriteAllLines($TmpScript, $RLines)

    & $RScript $TmpScript
    if ($LASTEXITCODE -ne 0) { throw "R boundary download failed. Check country name." }

    databricks fs mkdirs "$VolBase/$Slug"
    databricks fs cp $TmpFile $VolPath --overwrite
    Write-Host "  Uploaded: $VolPath" -ForegroundColor Green
}

# -- Deploy -------------------------------------------------------------------
Write-Host "[2/3] Deploying bundle..." -ForegroundColor Yellow
databricks bundle deploy --target dev --force-lock
if ($LASTEXITCODE -ne 0) { throw "Bundle deploy failed." }

# -- Run ----------------------------------------------------------------------
Write-Host "[3/3] Running job..." -ForegroundColor Yellow
databricks bundle run drought_era5_monitor --target dev
if ($LASTEXITCODE -ne 0) { throw "Job failed. Check Databricks run logs." }

Write-Host ""
Write-Host "Done. Output CSV in Volume:" -ForegroundColor Green
Write-Host "  /Volumes/projects/gwsc_drought/drought_outputs/app_outputs/"
