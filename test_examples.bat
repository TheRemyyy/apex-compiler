@echo off
setlocal EnableDelayedExpansion

echo ========================================
echo      Apex Compiler Test Runner
echo ========================================

echo.
echo [1/4] Building Compiler...
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
echo [2/4] Running Single-File Examples...
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

REM Test multi-file project (original)
echo.
echo [3/4] Testing Multi-File Project (Basic)...
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

REM Test multi-file project with Java-style namespaces
echo.
echo [4/4] Testing Java-Style Namespace Project...
echo.

if exist "examples\multi_file_depth_project\apex.toml" (
    echo ----------------------------------------
    echo Testing examples\multi_file_depth_project...
    
    cd examples\multi_file_depth_project
    ..\..\%COMPILER% run
    
    if !ERRORLEVEL! EQU 0 (
        echo [PASS] multi_file_depth_project (Java-style namespaces)
        set /a PASS_COUNT+=1
    ) else (
        echo [FAIL] multi_file_depth_project
        set /a FAIL_COUNT+=1
    )
    cd ..\..
) else (
    echo Java-style namespace project not found, skipping...
)

REM Test no-import project
echo.
echo [5/5] Testing No-Import Project (Global Scope)...
echo.

if exist "examples\test_no_import\apex.toml" (
    echo ----------------------------------------
    echo Testing examples\test_no_import...
    
    cd examples\test_no_import
    ..\..\%COMPILER% run
    
    if !ERRORLEVEL! EQU 0 (
        echo [PASS] test_no_import (global scope without imports)
        set /a PASS_COUNT+=1
    ) else (
        echo [FAIL] test_no_import
        set /a FAIL_COUNT+=1
    )
    cd ..\..
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
