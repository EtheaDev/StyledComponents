<#
.SYNOPSIS
    Builds the StyledComponents demo for one or more Delphi versions.

.DESCRIPTION
    Wrapper around BuildDemo.ps1 that builds the demo, for each requested
    Delphi version and platform, and prints a summary table. A Delphi version
    that is not installed under BDSRoot is reported as SKIP.

.PARAMETER DelphiVersion
    One or more versions (DXE6 ... D13), or "All" (default) for all of them.

.PARAMETER BDSRoot
    Root folder of RAD Studio installations (default: C:\BDS\Studio).

.PARAMETER Platform
    Target platform: Win32, Win64 or Both (default: Both).

.PARAMETER Config
    Build configuration: Debug or Release (default: Release).

.PARAMETER Target
    MSBuild target: Build, Clean or Rebuild (default: Rebuild).

.EXAMPLE
    .\BuildAllDemos.ps1
    # Rebuilds the demo for every installed Delphi, Win32 + Win64, Release.

.EXAMPLE
    .\BuildAllDemos.ps1 -DelphiVersion D12,D13 -Platform Win64 -Target Build
#>
param(
    [string[]]$DelphiVersion = @("All"),

    [string]$BDSRoot = "C:\BDS\Studio",

    [ValidateSet("Win32", "Win64", "Both")]
    [string]$Platform = "Both",

    [ValidateSet("Debug", "Release")]
    [string]$Config = "Release",

    [ValidateSet("Build", "Clean", "Rebuild")]
    [string]$Target = "Rebuild"
)

# --- Explorer pause: when started from Explorer the window stays open at the end, so the errors can be read ---
if (-not $global:ExplorerPauseRegistered) {
    $global:ExplorerPauseRegistered = $true
    try {
        $ppid = (Get-CimInstance Win32_Process -Filter "ProcessId=$PID").ParentProcessId
        if ((Get-Process -Id $ppid -ErrorAction Stop).ProcessName -eq 'explorer') {
            Register-EngineEvent -SourceIdentifier PowerShell.Exiting -Action {
                Write-Host ''
                Write-Host 'Press Enter to close...'
                [void](Read-Host)
            } | Out-Null
        }
    } catch { }
}


$ErrorActionPreference = "Stop"

$AllVersions = @("DXE6", "DXE7", "DXE8", "D10", "D10.1", "D10.2", "D10.3", "D10.4", "D11", "D12", "D13")
$bdsVersionMap = @{
    "DXE6"   = "14.0"
    "DXE7"   = "15.0"
    "DXE8"   = "16.0"
    "D10"    = "17.0"
    "D10.1"  = "18.0"
    "D10.2"  = "19.0"
    "D10.3"  = "20.0"
    "D10.4"  = "21.0"
    "D11"    = "22.0"
    "D12"    = "23.0"
    "D13"    = "37.0"
}

# accepts both -DelphiVersion D12,D13 and "D12,D13" (when launched with powershell -File)
$DelphiVersion = @($DelphiVersion | ForEach-Object { $_ -split "," } | Where-Object { $_ })
if ($DelphiVersion -contains "All") { $Versions = $AllVersions } else { $Versions = $DelphiVersion }
if ($Platform -eq "Both") { $Platforms = @("Win32", "Win64") } else { $Platforms = @($Platform) }

$BuildScript = Join-Path $PSScriptRoot "BuildDemo.ps1"
$Results = @()
$Failed = 0
$StartTime = Get-Date

foreach ($ver in $Versions) {
    foreach ($plat in $Platforms) {
        $installed = Test-Path (Join-Path (Join-Path $BDSRoot $bdsVersionMap[$ver]) "bin\rsvars.bat")
        if (-not $installed) {
            $Results += [pscustomobject]@{ Delphi = $ver; Platform = $plat; Status = "SKIP"; Seconds = 0 }
            continue
        }

        Write-Host ""
        Write-Host "############################################" -ForegroundColor Magenta
        Write-Host "  Demo / $ver / $plat / $Config" -ForegroundColor Magenta
        Write-Host "############################################" -ForegroundColor Magenta

        $stepStart = Get-Date
        & $BuildScript -DelphiVersion $ver -BDSRoot $BDSRoot -Platform $plat -Config $Config -Target $Target
        $exitCode = $LASTEXITCODE
        $Results += [pscustomobject]@{
            Delphi   = $ver
            Platform = $plat
            Status   = if ($exitCode -eq 0) { "OK" } else { "FAIL" }
            Seconds  = [math]::Round(((Get-Date) - $stepStart).TotalSeconds, 1)
        }
        if ($exitCode -ne 0) { $Failed++ }
    }
}

Write-Host ""
Write-Host "============================================" -ForegroundColor Cyan
Write-Host " SUMMARY (demo)" -ForegroundColor Cyan
Write-Host "============================================" -ForegroundColor Cyan
$Results | Format-Table -AutoSize | Out-String | Write-Host
Write-Host ("Total elapsed: {0:N1} s" -f ((Get-Date) - $StartTime).TotalSeconds) -ForegroundColor White

$built = @($Results | Where-Object { $_.Status -ne "SKIP" }).Count
if ($Failed -gt 0) {
    Write-Host "$Failed of $built builds FAILED" -ForegroundColor Red
    exit 1
}
else {
    Write-Host "All $built builds SUCCEEDED" -ForegroundColor Green
    exit 0
}
