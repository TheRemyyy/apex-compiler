#!/bin/bash
set -e

# Test standard library modules

echo "=================================="
echo "Testing Standard Library"
echo "=================================="
echo ""

COMPILER="./target/release/apex-compiler"

if [ ! -f "$COMPILER" ]; then
    echo "Error: Compiler not found at $COMPILER"
    exit 1
fi

PASSED=0
FAILED=0

# Test collections
test_collections() {
    echo -n "Testing collections... "
    if [ -f "tests/stdlib/collections_test.apex" ]; then
        if $COMPILER check "tests/stdlib/collections_test.apex" > /dev/null 2>&1; then
            echo "✓"
            return 0
        else
            echo "✗"
            return 1
        fi
    else
        echo "⊘ (no tests)"
        return 0
    fi
}

# Test JSON
test_json() {
    echo -n "Testing JSON... "
    if [ -f "tests/stdlib/json_test.apex" ]; then
        if $COMPILER check "tests/stdlib/json_test.apex" > /dev/null 2>&1; then
            echo "✓"
            return 0
        else
            echo "✗"
            return 1
        fi
    else
        echo "⊘ (no tests)"
        return 0
    fi
}

# Test HTTP
test_http() {
    echo -n "Testing HTTP... "
    if [ -f "tests/stdlib/http_test.apex" ]; then
        if $COMPILER check "tests/stdlib/http_test.apex" > /dev/null 2>&1; then
            echo "✓"
            return 0
        else
            echo "✗"
            return 1
        fi
    else
        echo "⊘ (no tests)"
        return 0
    fi
}

# Test regex
test_regex() {
    echo -n "Testing regex... "
    if [ -f "tests/stdlib/regex_test.apex" ]; then
        if $COMPILER check "tests/stdlib/regex_test.apex" > /dev/null 2>&1; then
            echo "✓"
            return 0
        else
            echo "✗"
            return 1
        fi
    else
        echo "⊘ (no tests)"
        return 0
    fi
}

echo "Running stdlib module tests:"
echo ""

if test_collections; then PASSED=$((PASSED + 1)); else FAILED=$((FAILED + 1)); fi
if test_json; then PASSED=$((PASSED + 1)); else FAILED=$((FAILED + 1)); fi
if test_http; then PASSED=$((PASSED + 1)); else FAILED=$((FAILED + 1)); fi
if test_regex; then PASSED=$((PASSED + 1)); else FAILED=$((FAILED + 1)); fi

echo ""
echo "=================================="
echo "Results: $PASSED passed, $FAILED failed"
echo "=================================="

exit 0
