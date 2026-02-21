#!/bin/bash
set -e

# Check and run all examples

echo "=================================="
echo "Checking Examples"
echo "=================================="
echo ""

COMPILER="./target/release/apex-compiler"

if [ ! -f "$COMPILER" ]; then
    echo "Error: Compiler not found at $COMPILER"
    exit 1
fi

PASSED=0
FAILED=0
TOTAL=0

# Single-file examples
echo "Single-file examples:"
echo "---------------------"
for file in examples/*.apex; do
    if [ -f "$file" ]; then
        TOTAL=$((TOTAL + 1))
        echo -n "  $(basename "$file")... "
        
        if $COMPILER check "$file" > /dev/null 2>&1; then
            echo "✓ check"
            PASSED=$((PASSED + 1))
        else
            echo "✗ check failed"
            $COMPILER check "$file" 2>&1 | head -10
            FAILED=$((FAILED + 1))
        fi
    fi
done

# Multi-file projects
echo ""
echo "Multi-file projects:"
echo "--------------------"
for project in examples/*/; do
    if [ -f "$project/apex.toml" ]; then
        TOTAL=$((TOTAL + 1))
        project_name=$(basename "$project")
        echo -n "  $project_name... "
        
        cd "$project"
        if ../../target/release/apex-compiler check > /dev/null 2>&1; then
            echo "✓"
            PASSED=$((PASSED + 1))
        else
            echo "✗ failed"
            ../../target/release/apex-compiler check 2>&1 | head -10
            FAILED=$((FAILED + 1))
        fi
        cd ../..
    fi
done

echo ""
echo "=================================="
echo "Results: $PASSED passed, $FAILED failed, $TOTAL total"
echo "=================================="

if [ $FAILED -eq 0 ]; then
    exit 0
else
    exit 1
fi
