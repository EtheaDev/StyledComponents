<#
.SYNOPSIS
    Rebuilds all StyledComponents packages for Delphi 12 Athens.

.DESCRIPTION
    Calls BuildAllPackages.ps1 -DelphiVersion D12: runtime package
    (StyledComponents) and design-time package (dclStyledComponents);
    with -Animated also the animated packages (require Skia4Delphi).
    The runtime package is built for Win32 and Win64; the design-time package
    for the platforms of the IDE (Win32 only before D12).

.PARAMETER BDSRoot
    Root folder of RAD Studio installations (default: C:\BDS\Studio).

.PARAMETER Platform
    Target platform: Win32, Win64 or Both (default: Both).

.PARAMETER Config
    Build configuration: Debug or Release (default: Release).

.PARAMETER Target
    MSBuild target: Build, Clean or Rebuild (default: Rebuild).

.PARAMETER Animated
    Builds also the animated packages (require Skia4Delphi).

.EXAMPLE
    .\BuildAllPackagesD12.ps1
    # Rebuilds the packages for D12 on Win32 + Win64 in Release.
#>
param(
    [string]$BDSRoot = "C:\BDS\Studio",

    [ValidateSet("Win32", "Win64", "Both")]
    [string]$Platform = "Both",

    [ValidateSet("Debug", "Release")]
    [string]$Config = "Release",

    [ValidateSet("Build", "Clean", "Rebuild")]
    [string]$Target = "Rebuild",

    [switch]$Animated
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


$BuildAll = Join-Path $PSScriptRoot "BuildAllPackages.ps1"
& $BuildAll -DelphiVersion "D12" -BDSRoot $BDSRoot -Platform $Platform -Config $Config -Target $Target -Animated:$Animated
exit $LASTEXITCODE
