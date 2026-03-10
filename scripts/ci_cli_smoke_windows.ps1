$ErrorActionPreference = "Stop"

$scriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
$repoRoot = Split-Path -Parent $scriptDir
$bashScript = Join-Path $scriptDir "ci_cli_smoke.sh"
$bashCommand = Get-Command bash -ErrorAction SilentlyContinue

if (-not $bashCommand) {
    throw "bash is not available in PATH"
}

if (-not (Test-Path $bashScript)) {
    throw "Smoke script not found: $bashScript"
}

$compilerInput = if ($env:APEX_COMPILER_PATH) {
    $env:APEX_COMPILER_PATH
} else {
    Join-Path $repoRoot "target\release\apex-compiler.exe"
}

if (-not (Test-Path $compilerInput)) {
    throw "Compiler binary not found: $compilerInput"
}

$bashScriptUnix = (& $bashCommand.Source -lc "cygpath -u '$bashScript'").Trim()
$compilerUnix = (& $bashCommand.Source -lc "cygpath -u '$compilerInput'").Trim()

if (-not $bashScriptUnix) {
    throw "Failed to convert smoke script path for bash: $bashScript"
}
if (-not $compilerUnix) {
    throw "Failed to convert compiler path for bash: $compilerInput"
}

$env:APEX_COMPILER_PATH = $compilerUnix

& $bashCommand.Source --noprofile --norc -e -o pipefail $bashScriptUnix
if ($LASTEXITCODE -ne 0) {
    throw "Windows CLI smoke wrapper failed with exit code $LASTEXITCODE"
}
