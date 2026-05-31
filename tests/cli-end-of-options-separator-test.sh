#!/bin/bash
# CLI end-of-options separator tests.
#
# Contract:
#   After --, every remaining argument is a file name, even when it starts with -.

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
CLI="$PROJECT_DIR/bin/mallet"
TEMP_DIR=$(mktemp -d)
_cache="$PROJECT_DIR/.cache"
export ASDF_OUTPUT_TRANSLATIONS="(:output-translations (t (\"$_cache/\" :implementation)) :ignore-inherited-configuration)"

trap "rm -rf $TEMP_DIR" EXIT

RED='\033[0;31m'
GREEN='\033[0;32m'
NC='\033[0m'

TESTS_RUN=0
TESTS_PASSED=0
TESTS_FAILED=0

pass() {
    TESTS_PASSED=$((TESTS_PASSED + 1))
    echo -e "  ${GREEN}PASS${NC}: $1"
}

fail() {
    TESTS_FAILED=$((TESTS_FAILED + 1))
    echo -e "  ${RED}FAIL${NC}: $1"
    if [ -n "$2" ]; then
        echo "        $2"
    fi
}

run_test() {
    TESTS_RUN=$((TESTS_RUN + 1))
    echo "Test $TESTS_RUN: $1"
}

assert_dash_leading_file_after_separator_lints() {
    local output exit_code

    run_test "-- lints a dash-leading filename"
    printf '(defun has-trailing-whitespace () t)   \n' > "$TEMP_DIR/-dash.lisp"

    exit_code=0
    output=$(cd "$TEMP_DIR" && "$CLI" -- -dash.lisp 2>&1) || exit_code=$?

    if [ "$exit_code" -eq 3 ]; then
        fail "dash-leading filename after -- exits with usage error 3" "$output"
        return
    fi

    if echo "$output" | grep -q "Unknown option: -dash.lisp"; then
        fail "dash-leading filename after -- is still parsed as an option" "$output"
        return
    fi

    if [ "$exit_code" -ne 1 ]; then
        fail "dash-leading filename after -- exits $exit_code; expected lint exit 1" "$output"
        return
    fi

    if ! echo "$output" | grep -q -- "-dash.lisp"; then
        fail "output does not mention the -dash.lisp file that should have been linted" "$output"
        return
    fi

    if ! echo "$output" | grep -q "trailing-whitespace"; then
        fail "dash-leading filename was not linted for trailing whitespace" "$output"
        return
    fi

    pass "-- -dash.lisp lints the existing file and reports its trailing-whitespace violation"
}

assert_fix_after_separator_is_filename() {
    local output exit_code

    run_test "-- treats --fix as a filename"
    printf '(defun has-trailing-whitespace () t)   \n' > "$TEMP_DIR/--fix"
    printf '(defun clean () t)\n' > "$TEMP_DIR/f.lisp"

    exit_code=0
    output=$(cd "$TEMP_DIR" && "$CLI" -- --fix f.lisp 2>&1) || exit_code=$?

    if [ "$exit_code" -eq 3 ]; then
        fail "--fix after -- exits with usage error 3" "$output"
        return
    fi

    if [ "$exit_code" -ne 1 ]; then
        fail "--fix filename with trailing whitespace exits $exit_code; expected lint exit 1" "$output"
        return
    fi

    if ! echo "$output" | grep -q -- "--fix"; then
        fail "output does not mention the --fix file that should have been linted" "$output"
        return
    fi

    if ! echo "$output" | grep -q "trailing-whitespace"; then
        fail "--fix filename was not linted for trailing whitespace" "$output"
        return
    fi

    if ! grep -qE ' +$' "$TEMP_DIR/--fix"; then
        fail "--fix file was modified, which indicates fix mode was enabled" "$(cat -vet "$TEMP_DIR/--fix")"
        return
    fi

    pass "-- --fix f.lisp treats --fix as a linted filename and does not enable fix mode"
}

echo "=============================================="
echo "Mallet CLI End-of-Options Separator Tests"
echo "=============================================="
echo ""

assert_dash_leading_file_after_separator_lints
assert_fix_after_separator_is_filename

echo ""
echo "=============================================="
echo "Tests run: $TESTS_RUN"
echo "Passed: $TESTS_PASSED"
echo "Failed: $TESTS_FAILED"
echo "=============================================="

if [ "$TESTS_FAILED" -gt 0 ]; then
    exit 1
fi

exit 0
