#!/bin/bash
# CLI Integration Tests for Mallet
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
CLI="$PROJECT_DIR/bin/mallet"
FIXTURES="$SCRIPT_DIR/fixtures"
FIXTURES_CONFIG="$FIXTURES/.mallet.lisp"
CLEAN_DIR="$FIXTURES/clean"
VIOLATIONS_DIR="$FIXTURES/violations"

echo "========================================="
echo "Mallet CLI Integration Tests"
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
        OUTPUT=$("$CLI" "$file" 2>&1 | grep -c "No problems found" || true)
        if [ "$OUTPUT" -ge 1 ]; then
            test_pass
        else
            test_fail "Expected 'No problems found' message"
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

        # Check if this file has expected violations
        EXPECTED_COUNT=0
        if [ -f "$expected_file" ]; then
            EXPECTED_COUNT=$(grep -v '^#' "$expected_file" | grep -v '^$' | wc -l | tr -d ' ')
        fi

        # Skip tests for files with no expected violations
        if [ "$EXPECTED_COUNT" -eq 0 ]; then
            continue
        fi

        # Test that violations are detected
        test_start "Violation file '$filename' detects violations"
        OUTPUT=$("$CLI" --config "$FIXTURES_CONFIG" "$file" 2>&1 | grep -c "problem" || true)
        if [ "$OUTPUT" -ge 1 ]; then
            test_pass
        else
            test_fail "Expected violations to be detected"
        fi

        # Determine expected exit code based on highest severity
        # Default --fail-on is error: exit 1 only for errors, not warnings/info
        EXPECTED_EXIT_CODE=0
        if [ -f "$expected_file" ]; then
            if grep -q ' error$' "$expected_file" 2>/dev/null; then
                EXPECTED_EXIT_CODE=1
            fi
        fi

        # Test exit code
        test_start "Violation file '$filename' returns exit code $EXPECTED_EXIT_CODE"
        EXIT_CODE=0
        "$CLI" --config "$FIXTURES_CONFIG" "$file" 2>&1 > /dev/null || EXIT_CODE=$?
        if [ "$EXIT_CODE" -eq "$EXPECTED_EXIT_CODE" ]; then
            test_pass
        else
            test_fail "Expected exit code $EXPECTED_EXIT_CODE, got $EXIT_CODE"
        fi

        # If .expected file exists, verify specific violations
        if [ -f "$expected_file" ]; then
            test_start "Violation file '$filename' matches expected violations"

            # Count expected violations (excluding comments and empty lines)
            EXPECTED_COUNT=$(grep -v '^#' "$expected_file" | grep -v '^$' | wc -l | tr -d ' ')

            # Count actual violations (format: "  line:col  severity  message  rule")
            ACTUAL_COUNT=$("$CLI" --config "$FIXTURES_CONFIG" "$file" 2>&1 | grep -E '^\s+[0-9]+:[0-9]+' | wc -l | tr -d ' ')

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
OUTPUT=$("$CLI" --config "$FIXTURES_CONFIG" --format json "$VIOLATIONS_DIR/line-length.lisp" 2>&1 | grep -c '"violations"' || true)
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
OUTPUT=$("$CLI" --config "$FIXTURES_CONFIG" "$VIOLATIONS_DIR" 2>&1 | grep -c "violation" || true)
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
OUTPUT=$("$CLI" --config "$FIXTURES_CONFIG" "$VIOLATIONS_DIR/line-length.lisp" 2>&1 | grep -c "Line exceeds maximum length" || true)
if [ "$OUTPUT" -ge 1 ]; then
    test_pass
else
    test_fail "Expected line-length violations"
fi

# Form-level rules
test_start "If-without-else rule detects violations"
OUTPUT=$("$CLI" --config "$FIXTURES_CONFIG" "$VIOLATIONS_DIR/form-rules.lisp" 2>&1 | grep -c "when.*unless" || true)
if [ "$OUTPUT" -ge 1 ]; then
    test_pass
else
    test_fail "Expected if-without-else violations"
fi

test_start "Bare-progn-in-if rule detects violations"
OUTPUT=$("$CLI" --config "$FIXTURES_CONFIG" "$VIOLATIONS_DIR/form-rules.lisp" 2>&1 | grep -c "cond.*progn" || true)
if [ "$OUTPUT" -ge 1 ]; then
    test_pass
else
    test_fail "Expected bare-progn-in-if violations"
fi

test_start "Missing-otherwise rule detects violations"
OUTPUT=$("$CLI" --config "$FIXTURES_CONFIG" "$VIOLATIONS_DIR/form-rules.lisp" 2>&1 | grep -c "should have 'otherwise' clause" || true)
if [ "$OUTPUT" -ge 1 ]; then
    test_pass
else
    test_fail "Expected missing-otherwise violations"
fi

test_start "Wrong-otherwise rule detects violations"
OUTPUT=$("$CLI" --config "$FIXTURES_CONFIG" "$VIOLATIONS_DIR/form-rules.lisp" 2>&1 | grep -c "should not have 'otherwise'" || true)
if [ "$OUTPUT" -ge 1 ]; then
    test_pass
else
    test_fail "Expected wrong-otherwise violations"
fi

# Test exit codes
test_start "Form rules file returns exit code 1 (has errors)"
EXIT_CODE=0
"$CLI" --config "$FIXTURES_CONFIG" "$VIOLATIONS_DIR/form-rules.lisp" 2>&1 > /dev/null || EXIT_CODE=$?
if [ $EXIT_CODE -eq 1 ]; then
    test_pass
else
    test_fail "Expected exit code 1 (errors), got $EXIT_CODE"
fi

# --fail-on flag tests
test_start "--fail-on error: exit 0 for warning-only violations"
EXIT_CODE=0
"$CLI" --config "$FIXTURES_CONFIG" --fail-on error "$VIOLATIONS_DIR/unused-variables.lisp" 2>&1 > /dev/null || EXIT_CODE=$?
if [ $EXIT_CODE -eq 0 ]; then
    test_pass
else
    test_fail "Expected exit code 0 (no errors, only warnings), got $EXIT_CODE"
fi

test_start "--fail-on warning: exit 1 for warning violations"
EXIT_CODE=0
"$CLI" --config "$FIXTURES_CONFIG" --fail-on warning "$VIOLATIONS_DIR/unused-variables.lisp" 2>&1 > /dev/null || EXIT_CODE=$?
if [ $EXIT_CODE -eq 1 ]; then
    test_pass
else
    test_fail "Expected exit code 1 (warnings with --fail-on warning), got $EXIT_CODE"
fi

test_start "--fail-on info: exit 1 for any violations"
EXIT_CODE=0
"$CLI" --config "$FIXTURES_CONFIG" --fail-on info "$VIOLATIONS_DIR/unused-variables.lisp" 2>&1 > /dev/null || EXIT_CODE=$?
if [ $EXIT_CODE -eq 1 ]; then
    test_pass
else
    test_fail "Expected exit code 1 (any violations with --fail-on info), got $EXIT_CODE"
fi

test_start "--strict: exit 1 for any violations (alias for --fail-on info)"
EXIT_CODE=0
"$CLI" --config "$FIXTURES_CONFIG" --strict "$VIOLATIONS_DIR/unused-variables.lisp" 2>&1 > /dev/null || EXIT_CODE=$?
if [ $EXIT_CODE -eq 1 ]; then
    test_pass
else
    test_fail "Expected exit code 1 (--strict with warnings), got $EXIT_CODE"
fi

test_start "--fail-on error: exit 1 for error violations"
EXIT_CODE=0
"$CLI" --config "$FIXTURES_CONFIG" --fail-on error "$VIOLATIONS_DIR/form-rules.lisp" 2>&1 > /dev/null || EXIT_CODE=$?
if [ $EXIT_CODE -eq 1 ]; then
    test_pass
else
    test_fail "Expected exit code 1 (errors with --fail-on error), got $EXIT_CODE"
fi

# Metrics rules - disabled-by-default with --enable flag
test_start "Comment-ratio rule detects violations when enabled via --enable flag"
OUTPUT=$("$CLI" --enable comment-ratio "$VIOLATIONS_DIR/comment-ratio.lisp" 2>&1 | grep -c "comment-ratio" || true)
if [ "$OUTPUT" -ge 1 ]; then
    test_pass
else
    test_fail "Expected comment-ratio violations when enabled via --enable flag"
fi

test_start "--none skips auto-discovered config file (only requested rules run)"
OUTPUT=$("$CLI" --none --enable comment-ratio "$VIOLATIONS_DIR/comment-ratio.lisp" 2>&1)
UNEXPECTED=$(echo "$OUTPUT" | grep -v "comment-ratio" | grep -E "^\s+[0-9]+:[0-9]+" | wc -l | tr -d ' ')
if [ "$UNEXPECTED" -eq 0 ]; then
    test_pass
else
    test_fail "--none with --enable produced unexpected rule violations: $OUTPUT"
fi

# Inline comment suppression tests
echo ""
echo "Testing inline comment suppression..."
echo ""

NO_VIOLATIONS_DIR="$FIXTURES/no-violations"

test_start "Active :suppress comment eliminates matching violation"
OUTPUT=$("$CLI" --none --enable needless-let* "$NO_VIOLATIONS_DIR/comment-suppress-active.lisp" 2>&1)
if echo "$OUTPUT" | grep -q "No problems found"; then
    test_pass
else
    test_fail "Expected no violations when needless-let* is suppressed: $OUTPUT"
fi

test_start "Active :suppress is not reported as stale"
STALE_COUNT=$("$CLI" --none --enable needless-let* --enable stale-suppression "$NO_VIOLATIONS_DIR/comment-suppress-active.lisp" 2>&1 | grep -c "stale-suppression" || true)
if [ "$STALE_COUNT" -eq 0 ]; then
    test_pass
else
    test_fail "Expected no stale-suppression violation when suppress was used"
fi

test_start "Stale :suppress generates stale-suppression warning"
STALE_COUNT=$("$CLI" --none --enable needless-let* --enable stale-suppression "$VIOLATIONS_DIR/comment-suppress-stale.lisp" 2>&1 | grep -c "stale-suppression" || true)
if [ "$STALE_COUNT" -ge 1 ]; then
    test_pass
else
    test_fail "Expected stale-suppression violation when suppress has no matching violation"
fi

test_start ":disable region suppresses forms until :enable"
IWE_COUNT=$("$CLI" --none --enable if-without-else "$VIOLATIONS_DIR/comment-disable-enable.lisp" 2>&1 | grep -c "if-without-else" || true)
if [ "$IWE_COUNT" -eq 2 ]; then
    test_pass
else
    test_fail "Expected exactly 2 if-without-else violations (before-disable and after-enable), got $IWE_COUNT"
fi

# Documentation completeness
test_start "RULES.md documents :comment-ratio rule under METRICS section"
if grep -q ":comment-ratio" "$PROJECT_DIR/RULES.md" && grep -q ":min-lines" "$PROJECT_DIR/RULES.md"; then
    test_pass
else
    test_fail "Expected :comment-ratio entry with options in RULES.md"
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
