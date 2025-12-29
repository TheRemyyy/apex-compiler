@echo off
setlocal EnableDelayedExpansion

echo ========================================
echo      Apex Compiler Test Runner
echo ========================================

echo.
echo [1/2] Building Compiler...
cargo build --release
if %ERRORLEVEL% NEQ 0 (
    echo Build failed!
    exit /b 1
)

set COMPILER=target\release\apex-compiler.exe

if not exist "%COMPILER%" (
    echo Compiler binary not found at %COMPILER%
    exit /b 1
)

echo Build successful. Using %COMPILER%

echo.
echo [2/2] Running Examples...
echo.

set FAIL_COUNT=0
set PASS_COUNT=0

for %%f in (examples\*.apex) do (
    echo ----------------------------------------
    echo Testing %%f...
    
    "%COMPILER%" run "%%f"
    
    if !ERRORLEVEL! EQU 0 (
        echo [PASS] %%f
        set /a PASS_COUNT+=1
    ) else (
        echo [FAIL] %%f
        set /a FAIL_COUNT+=1
    )
)

echo.
echo ========================================
echo Test Summary
echo ========================================
echo Passed: %PASS_COUNT%
echo Failed: %FAIL_COUNT%

if %FAIL_COUNT% EQU 0 (
    echo ALL TESTS PASSED
    exit /b 0
) else (
    echo SOME TESTS FAILED
    exit /b 1
)

