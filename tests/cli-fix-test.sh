#!/bin/bash
# CLI Integration tests for --fix option

set -e  # Exit on error

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
MALLET="$PROJECT_ROOT/bin/mallet"
FIXTURES_DIR="$SCRIPT_DIR/fixtures/fix"
TEMP_DIR=$(mktemp -d)

trap "rm -rf $TEMP_DIR" EXIT

TESTS_RUN=0
TESTS_PASSED=0
TESTS_FAILED=0

# Test helper functions
pass() {
    echo -e "${GREEN}✓${NC} $1"
    TESTS_PASSED=$((TESTS_PASSED + 1))
}

fail() {
    echo -e "${RED}✗${NC} $1"
    TESTS_FAILED=$((TESTS_FAILED + 1))
}

test_name() {
    echo -e "\n${YELLOW}Testing:${NC} $1"
    TESTS_RUN=$((TESTS_RUN + 1))
}

# Test 1: --fix fixes trailing whitespace
test_name "trailing-whitespace auto-fix"
cp "$FIXTURES_DIR/trailing-whitespace.lisp" "$TEMP_DIR/test1.lisp"
$MALLET --all --fix "$TEMP_DIR/test1.lisp" > /dev/null 2>&1
if diff -q "$TEMP_DIR/test1.lisp" "$FIXTURES_DIR/trailing-whitespace.expected" > /dev/null; then
    pass "Trailing whitespace fixed correctly"
else
    fail "Trailing whitespace not fixed correctly"
    echo "Expected:"
    cat "$FIXTURES_DIR/trailing-whitespace.expected"
    echo "Got:"
    cat "$TEMP_DIR/test1.lisp"
fi

# Test 2: --fix fixes missing final newline
test_name "final-newline auto-fix"
cp "$FIXTURES_DIR/final-newline.lisp" "$TEMP_DIR/test2.lisp"
$MALLET --fix "$TEMP_DIR/test2.lisp" > /dev/null 2>&1
if diff -q "$TEMP_DIR/test2.lisp" "$FIXTURES_DIR/final-newline.expected" > /dev/null; then
    pass "Final newline fixed correctly"
else
    fail "Final newline not fixed correctly"
    echo "Expected:"
    cat -vet "$FIXTURES_DIR/final-newline.expected"
    echo "Got:"
    cat -vet "$TEMP_DIR/test2.lisp"
fi

# Test 3: --fix fixes consecutive blank lines
test_name "consecutive-blank-lines auto-fix"
cp "$FIXTURES_DIR/consecutive-blank-lines.lisp" "$TEMP_DIR/test3.lisp"
$MALLET --all --fix "$TEMP_DIR/test3.lisp" > /dev/null 2>&1
if diff -q "$TEMP_DIR/test3.lisp" "$FIXTURES_DIR/consecutive-blank-lines.expected" > /dev/null; then
    pass "Consecutive blank lines fixed correctly"
else
    fail "Consecutive blank lines not fixed correctly"
    echo "Expected:"
    cat -n "$FIXTURES_DIR/consecutive-blank-lines.expected"
    echo "Got:"
    cat -n "$TEMP_DIR/test3.lisp"
fi

# Test 4: --fix fixes multiple violations in one file
test_name "multiple violations auto-fix"
cp "$FIXTURES_DIR/mixed-violations.lisp" "$TEMP_DIR/test4.lisp"
$MALLET --all --fix "$TEMP_DIR/test4.lisp" > /dev/null 2>&1
if diff -q "$TEMP_DIR/test4.lisp" "$FIXTURES_DIR/mixed-violations.expected" > /dev/null; then
    pass "Multiple violations fixed correctly"
else
    fail "Multiple violations not fixed correctly"
    echo "Expected:"
    cat -n "$FIXTURES_DIR/mixed-violations.expected"
    echo "Got:"
    cat -n "$TEMP_DIR/test4.lisp"
fi

# Test 5: --fix-dry-run doesn't modify files
test_name "--fix-dry-run doesn't modify files"
cp "$FIXTURES_DIR/final-newline.lisp" "$TEMP_DIR/test5.lisp"
cp "$TEMP_DIR/test5.lisp" "$TEMP_DIR/test5-original.lisp"
$MALLET --fix-dry-run "$TEMP_DIR/test5.lisp" > /dev/null 2>&1
if diff -q "$TEMP_DIR/test5.lisp" "$TEMP_DIR/test5-original.lisp" > /dev/null; then
    pass "File not modified in dry-run mode"
else
    fail "File was modified in dry-run mode!"
fi

# Test 6: --fix exits 0 when all violations fixed
test_name "--fix exit code when all fixed"
cp "$FIXTURES_DIR/final-newline.lisp" "$TEMP_DIR/test6.lisp"
if $MALLET --fix "$TEMP_DIR/test6.lisp" > /dev/null 2>&1; then
    EXIT_CODE=$?
    if [ $EXIT_CODE -eq 0 ]; then
        pass "Exit code 0 when all violations fixed"
    else
        fail "Exit code $EXIT_CODE, expected 0"
    fi
else
    EXIT_CODE=$?
    if [ $EXIT_CODE -eq 0 ]; then
        pass "Exit code 0 when all violations fixed"
    else
        fail "Exit code $EXIT_CODE, expected 0"
    fi
fi

# Test 7: File with no violations
test_name "file with no violations"
cp "$FIXTURES_DIR/final-newline.expected" "$TEMP_DIR/test7.lisp"
OUTPUT=$($MALLET --fix "$TEMP_DIR/test7.lisp" 2>&1)
if echo "$OUTPUT" | grep -q "No auto-fixable violations found"; then
    pass "Correctly reports no violations to fix"
else
    fail "Should report no violations to fix"
    echo "Output: $OUTPUT"
fi

# Test 8: --fix output shows [FIXED]
test_name "--fix output shows [FIXED] marker"
cp "$FIXTURES_DIR/final-newline.lisp" "$TEMP_DIR/test8.lisp"
OUTPUT=$($MALLET --fix "$TEMP_DIR/test8.lisp" 2>&1)
if echo "$OUTPUT" | grep -q "\[FIXED\]"; then
    pass "Output contains [FIXED] marker"
else
    fail "Output doesn't contain [FIXED] marker"
    echo "Output: $OUTPUT"
fi

# Test 9: --fix-dry-run output shows would fix
test_name "--fix-dry-run output message"
cp "$FIXTURES_DIR/final-newline.lisp" "$TEMP_DIR/test9.lisp"
OUTPUT=$($MALLET --fix-dry-run "$TEMP_DIR/test9.lisp" 2>&1)
if echo "$OUTPUT" | grep -q "Would fix.*dry run"; then
    pass "Dry-run output shows 'Would fix' message"
else
    fail "Dry-run output doesn't show correct message"
    echo "Output: $OUTPUT"
fi

# Print summary
echo ""
echo "================================"
echo "Test Summary"
echo "================================"
echo "Tests run: $TESTS_RUN"
echo -e "${GREEN}Passed: $TESTS_PASSED${NC}"
if [ $TESTS_FAILED -gt 0 ]; then
    echo -e "${RED}Failed: $TESTS_FAILED${NC}"
    exit 1
else
    echo "All tests passed!"
    exit 0
fi
