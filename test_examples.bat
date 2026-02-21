@echo off
setlocal EnableDelayedExpansion

echo ========================================
echo      Apex Compiler Test Runner
echo ========================================

echo.
echo [1/3] Building Compiler...
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

set FAIL_COUNT=0
set PASS_COUNT=0

REM Test single-file examples
echo.
echo [2/3] Running Single-File Examples...
echo.

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

REM Test multi-file project
echo.
echo [3/3] Testing Multi-File Project...
echo.

if exist "examples\multi_file_project\apex.toml" (
    echo ----------------------------------------
    echo Testing examples\multi_file_project...
    
    cd examples\multi_file_project
    ..\..\%COMPILER% run
    
    if !ERRORLEVEL! EQU 0 (
        echo [PASS] multi_file_project
        set /a PASS_COUNT+=1
    ) else (
        echo [FAIL] multi_file_project
        set /a FAIL_COUNT+=1
    )
    cd ..\..
) else (
    echo Multi-file project not found, skipping...
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
