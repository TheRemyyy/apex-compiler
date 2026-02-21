#!/bin/bash
set -e

# Apex Test Runner Script
# Runs all Apex tests in the tests/ directory

echo "=================================="
echo "Running Apex Tests"
echo "=================================="
echo ""

COMPILER="./target/release/apex-compiler"

# Check if compiler exists
if [ ! -f "$COMPILER" ]; then
    echo "Error: Compiler not found at $COMPILER"
    echo "Please run: cargo build --release"
    exit 1
fi

PASSED=0
FAILED=0
TOTAL=0

# Function to run a test file
run_test_file() {
    local file=$1
    local test_name=$(basename "$file" .apex)
    
    echo -n "Testing $test_name... "
    
    if $COMPILER check "$file" > /dev/null 2>&1; then
        echo "✓ PASSED"
        return 0
    else
        echo "✗ FAILED"
        $COMPILER check "$file" 2>&1 | head -20
        return 1
    fi
}

# Run unit tests
echo "Unit Tests:"
echo "-----------"
if [ -d "tests/unit" ]; then
    for file in tests/unit/*.apex; do
        if [ -f "$file" ]; then
            TOTAL=$((TOTAL + 1))
            if run_test_file "$file"; then
                PASSED=$((PASSED + 1))
            else
                FAILED=$((FAILED + 1))
            fi
        fi
    done
fi

echo ""
echo "Integration Tests:"
echo "------------------"
if [ -d "tests/integration" ]; then
    for file in tests/integration/*.apex; do
        if [ -f "$file" ]; then
            TOTAL=$((TOTAL + 1))
            if run_test_file "$file"; then
                PASSED=$((PASSED + 1))
            else
                FAILED=$((FAILED + 1))
            fi
        fi
    done
fi

echo ""
echo "Stdlib Tests:"
echo "-------------"
if [ -d "tests/stdlib" ]; then
    for file in tests/stdlib/*.apex; do
        if [ -f "$file" ]; then
            TOTAL=$((TOTAL + 1))
            if run_test_file "$file"; then
                PASSED=$((PASSED + 1))
            else
                FAILED=$((FAILED + 1))
            fi
        fi
    done
fi

echo ""
echo "=================================="
echo "Test Results:"
echo "  Passed: $PASSED"
echo "  Failed: $FAILED"
echo "  Total:  $TOTAL"
echo "=================================="

if [ $FAILED -eq 0 ]; then
    echo "All tests passed! ✓"
    exit 0
else
    echo "Some tests failed! ✗"
    exit 1
fi
