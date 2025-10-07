#!/bin/bash
# CLI Integration Tests for Malvolio
# Tests the full CLI pipeline with fixture files

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Track test results
TESTS_RUN=0
TESTS_PASSED=0
TESTS_FAILED=0

# Get script directory
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
CLI="$PROJECT_DIR/bin/malvolio"
FIXTURES="$SCRIPT_DIR/fixtures"
CLEAN_DIR="$FIXTURES/clean"
VIOLATIONS_DIR="$FIXTURES/violations"

echo "========================================="
echo "Malvolio CLI Integration Tests"
echo "========================================="
echo ""

# Test helper functions
test_start() {
    TESTS_RUN=$((TESTS_RUN + 1))
    echo -n "Test $TESTS_RUN: $1 ... "
}

test_pass() {
    TESTS_PASSED=$((TESTS_PASSED + 1))
    echo -e "${GREEN}PASS${NC}"
}

test_fail() {
    TESTS_FAILED=$((TESTS_FAILED + 1))
    echo -e "${RED}FAIL${NC}"
    if [ -n "$1" ]; then
        echo "  $1"
    fi
}

# Test clean files (should have no violations)
echo "Testing clean files (should pass)..."
echo ""

for file in "$CLEAN_DIR"/*.lisp; do
    if [ -f "$file" ]; then
        filename=$(basename "$file")

        # Test exit code
        test_start "Clean file '$filename' returns exit code 0"
        if "$CLI" "$file" 2>&1 > /dev/null; then
            test_pass
        else
            EXIT_CODE=$?
            test_fail "Expected exit code 0, got $EXIT_CODE"
        fi

        # Test output message
        test_start "Clean file '$filename' reports no violations"
        OUTPUT=$("$CLI" "$file" 2>&1 | grep -c "No violations" || true)
        if [ "$OUTPUT" -ge 1 ]; then
            test_pass
        else
            test_fail "Expected 'No violations' message"
        fi
    fi
done

echo ""
echo "Testing violation files (should detect violations)..."
echo ""

# Test violation files
for file in "$VIOLATIONS_DIR"/*.lisp; do
    if [ -f "$file" ]; then
        filename=$(basename "$file")
        expected_file="${file%.lisp}.expected"

        # Test that violations are detected
        test_start "Violation file '$filename' detects violations"
        OUTPUT=$("$CLI" "$file" 2>&1 | grep -c "violation" || true)
        if [ "$OUTPUT" -ge 1 ]; then
            test_pass
        else
            test_fail "Expected violations to be detected"
        fi

        # Test exit code (should be non-zero)
        test_start "Violation file '$filename' returns non-zero exit code"
        "$CLI" "$file" 2>&1 > /dev/null || EXIT_CODE=$?
        if [ $EXIT_CODE -ne 0 ]; then
            test_pass
        else
            test_fail "Expected non-zero exit code, got 0"
        fi

        # If .expected file exists, verify specific violations
        if [ -f "$expected_file" ]; then
            test_start "Violation file '$filename' matches expected violations"

            # Count expected violations (excluding comments and empty lines)
            EXPECTED_COUNT=$(grep -v '^#' "$expected_file" | grep -v '^$' | wc -l | tr -d ' ')

            # Count actual violations
            ACTUAL_COUNT=$("$CLI" "$file" 2>&1 | grep -E '^\s+.*:[0-9]+:[0-9]+' | wc -l | tr -d ' ')

            if [ "$ACTUAL_COUNT" -eq "$EXPECTED_COUNT" ]; then
                test_pass
            else
                test_fail "Expected $EXPECTED_COUNT violations, found $ACTUAL_COUNT"
            fi
        fi
    fi
done

echo ""
echo "Testing CLI features..."
echo ""

# Test JSON output format
test_start "JSON output format works"
OUTPUT=$("$CLI" --format json "$VIOLATIONS_DIR/line-length.lisp" 2>&1 | grep -c '"violations"' || true)
if [ "$OUTPUT" -ge 1 ]; then
    test_pass
else
    test_fail "Expected JSON structure with 'violations' key"
fi

# Test help flag
test_start "Help flag displays usage"
OUTPUT=$("$CLI" --help 2>&1 | grep -c "Usage:" || true)
if [ "$OUTPUT" -ge 1 ]; then
    test_pass
else
    test_fail "Expected help text with 'Usage:'"
fi

# Test version flag
test_start "Version flag displays version"
OUTPUT=$("$CLI" --version 2>&1 | grep -c "version" || true)
if [ "$OUTPUT" -ge 1 ]; then
    test_pass
else
    test_fail "Expected version information"
fi

# Test directory linting
test_start "Directory linting works"
OUTPUT=$("$CLI" "$VIOLATIONS_DIR" 2>&1 | grep -c "violation" || true)
if [ "$OUTPUT" -ge 1 ]; then
    test_pass
else
    test_fail "Expected violations from directory scan"
fi

# Test specific rule types
echo ""
echo "Testing specific rule types..."
echo ""

# Text-level rules
test_start "Line-length rule detects violations"
OUTPUT=$("$CLI" "$VIOLATIONS_DIR/line-length.lisp" 2>&1 | grep -c "Line exceeds maximum length" || true)
if [ "$OUTPUT" -ge 1 ]; then
    test_pass
else
    test_fail "Expected line-length violations"
fi

# Token-level rules
test_start "Comment-level rule detects violations"
OUTPUT=$("$CLI" "$VIOLATIONS_DIR/comment-level.lisp" 2>&1 | grep -c "semicolon" || true)
if [ "$OUTPUT" -ge 1 ]; then
    test_pass
else
    test_fail "Expected comment-level violations"
fi

# Form-level rules
test_start "If-without-else rule detects violations"
OUTPUT=$("$CLI" "$VIOLATIONS_DIR/form-rules.lisp" 2>&1 | grep -c "when.*unless" || true)
if [ "$OUTPUT" -ge 1 ]; then
    test_pass
else
    test_fail "Expected if-without-else violations"
fi

test_start "Bare-progn-in-if rule detects violations"
OUTPUT=$("$CLI" "$VIOLATIONS_DIR/form-rules.lisp" 2>&1 | grep -c "cond.*progn" || true)
if [ "$OUTPUT" -ge 1 ]; then
    test_pass
else
    test_fail "Expected bare-progn-in-if violations"
fi

test_start "Missing-otherwise rule detects violations"
OUTPUT=$("$CLI" "$VIOLATIONS_DIR/form-rules.lisp" 2>&1 | grep -c "should have 'otherwise' clause" || true)
if [ "$OUTPUT" -ge 1 ]; then
    test_pass
else
    test_fail "Expected missing-otherwise violations"
fi

test_start "Wrong-otherwise rule detects violations"
OUTPUT=$("$CLI" "$VIOLATIONS_DIR/form-rules.lisp" 2>&1 | grep -c "should not have 'otherwise'" || true)
if [ "$OUTPUT" -ge 1 ]; then
    test_pass
else
    test_fail "Expected wrong-otherwise violations"
fi

# Test severity levels
test_start "Form rules file returns exit code 2 (has errors)"
"$CLI" "$VIOLATIONS_DIR/form-rules.lisp" 2>&1 > /dev/null || EXIT_CODE=$?
if [ $EXIT_CODE -eq 2 ]; then
    test_pass
else
    test_fail "Expected exit code 2 (errors), got $EXIT_CODE"
fi

# Summary
echo ""
echo "========================================="
echo "Test Summary"
echo "========================================="
echo "Tests run:    $TESTS_RUN"
echo -e "Tests passed: ${GREEN}$TESTS_PASSED${NC}"
if [ $TESTS_FAILED -gt 0 ]; then
    echo -e "Tests failed: ${RED}$TESTS_FAILED${NC}"
else
    echo -e "Tests failed: $TESTS_FAILED"
fi
echo ""

if [ $TESTS_FAILED -eq 0 ]; then
    echo -e "${GREEN}✓ All tests passed!${NC}"
    exit 0
else
    echo -e "${RED}✗ Some tests failed.${NC}"
    exit 1
fi
