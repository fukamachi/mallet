#!/bin/bash
# CLI config-file validation error contract tests.
#
# Contract being tested:
#   Invalid config files are CLI usage errors: they print "Error:" and exit 3.
#   Messages must be sanitized so implementation/package internals do not leak.
#   Valid config files must still be honored.

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
CLI="$PROJECT_DIR/bin/mallet"
_cache="$PROJECT_DIR/.cache"
export ASDF_OUTPUT_TRANSLATIONS="(:output-translations (t (\"$_cache/\" :implementation)) :ignore-inherited-configuration)"

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

write_source_file() {
    local path="$1"
    printf '(defun clean () t)\n' > "$path"
}

assert_usage_error() {
    local name="$1"
    local config_content="$2"
    local extra_forbidden="$3"
    local work_dir config_file source_file output exit_code

    run_test "$name"
    work_dir=$(mktemp -d)
    config_file="$work_dir/mallet-config.lisp"
    source_file="$work_dir/source.lisp"
    printf '%s\n' "$config_content" > "$config_file"
    write_source_file "$source_file"

    exit_code=0
    output=$("$CLI" --config "$config_file" "$source_file" 2>&1) || exit_code=$?
    rm -rf "$work_dir"

    if [ "$exit_code" -ne 3 ]; then
        fail "$name exits $exit_code; expected 3" \
             "config validation failures must be CLI usage errors, not fatal runtime errors"
        return
    fi

    case "$output" in
        Error:*)
            ;;
        *)
            fail "$name exits 3 but output does not begin with Error:" "$output"
            return
            ;;
    esac

    case "$output" in
        *"Fatal error:"*)
            fail "$name output contains Fatal error:" "$output"
            return
            ;;
    esac

    if [ -n "$extra_forbidden" ] && echo "$output" | grep -Eq "$extra_forbidden"; then
        fail "$name output leaks an implementation detail forbidden by the contract" "$output"
        return
    fi

    pass "$name exits 3 and reports a sanitized Error: message"
}

assert_binary_config_usage_error() {
    local work_dir config_file source_file output exit_code

    run_test "non-UTF-8 config bytes are sanitized usage errors"
    work_dir=$(mktemp -d)
    config_file="$work_dir/mallet-config.lisp"
    source_file="$work_dir/source.lisp"
    printf '\xff\xfe' > "$config_file"
    write_source_file "$source_file"

    exit_code=0
    output=$("$CLI" --config "$config_file" "$source_file" 2>&1) || exit_code=$?
    rm -rf "$work_dir"

    if [ "$exit_code" -ne 3 ]; then
        fail "non-UTF-8 config exits $exit_code; expected 3" \
             "reader/stream decoding failures in config files must be CLI usage errors"
        return
    fi

    case "$output" in
        Error:*)
            ;;
        *)
            fail "non-UTF-8 config exits 3 but output does not begin with Error:" "$output"
            return
            ;;
    esac

    if echo "$output" | grep -Eq 'SB-SYS|#<|\{[0-9A-Fa-f]{6,}\}|0x[0-9A-Fa-f]{6,}|#x[0-9A-Fa-f]{6,}'; then
        fail "non-UTF-8 config output leaks SBCL stream internals" "$output"
        return
    fi

    pass "non-UTF-8 config exits 3 and hides SBCL stream internals"
}

assert_unknown_config_form_reports_keyword_only() {
    local work_dir config_file source_file output exit_code

    run_test "unknown top-level config form reports directive keyword only"
    work_dir=$(mktemp -d)
    config_file="$work_dir/mallet-config.lisp"
    source_file="$work_dir/source.lisp"
    printf '(:k "secret")\n(:mallet-config)\n' > "$config_file"
    write_source_file "$source_file"

    exit_code=0
    output=$("$CLI" --config "$config_file" "$source_file" 2>&1) || exit_code=$?
    rm -rf "$work_dir"

    if [ "$exit_code" -ne 3 ]; then
        fail "unknown config form exits $exit_code; expected 3" "$output"
        return
    fi

    case "$output" in
        Error:*)
            ;;
        *)
            fail "unknown config form output does not begin with Error:" "$output"
            return
            ;;
    esac

    if ! echo "$output" | grep -q "Unknown config form:"; then
        fail "unknown config form output omits the error kind" "$output"
        return
    fi

    if ! echo "$output" | grep -Eq '(:K|:k|[( ]K|[( ]k)'; then
        fail "unknown config form output does not name offending keyword K" "$output"
        return
    fi

    if echo "$output" | grep -q "secret"; then
        fail "unknown config form output leaks form payload" "$output"
        return
    fi

    pass "unknown config form names K without leaking the payload"
}

assert_valid_config_still_loads() {
    local work_dir config_file source_file output exit_code

    run_test "valid config disabling trailing-whitespace is honored"
    work_dir=$(mktemp -d)
    config_file="$work_dir/mallet-config.lisp"
    source_file="$work_dir/source.lisp"
    printf '(:mallet-config (:disable trailing-whitespace))\n' > "$config_file"
    printf '(defun clean () t)   \n' > "$source_file"

    exit_code=0
    output=$("$CLI" --config "$config_file" "$source_file" 2>&1) || exit_code=$?
    rm -rf "$work_dir"

    if [ "$exit_code" -ne 0 ]; then
        fail "valid config exits $exit_code; expected 0" "$output"
        return
    fi

    if echo "$output" | grep -q "trailing-whitespace"; then
        fail "valid config exits 0 but still reports trailing-whitespace" "$output"
        return
    fi

    pass "valid config loads and suppresses trailing-whitespace"
}

echo "=============================================="
echo "Mallet CLI Config Validation Error Tests"
echo "=============================================="
echo ""

assert_usage_error \
    ":set-severity rejects invalid severity as usage error" \
    '(:mallet-config (:set-severity :correctness :bad))' \
    ''

assert_usage_error \
    ":enable rejects unknown rule as usage error" \
    '(:mallet-config (:enable :no-such-rule))' \
    ''

assert_usage_error \
    ":ignore rejects non-string pattern without leaking glob internals" \
    '(:mallet-config (:ignore :keyword))' \
    'TRIVIAL-GLOB|::'

assert_usage_error \
    ":for-paths rejects a non-list selector without leaking glob internals" \
    '(:mallet-config (:for-paths :keyword (:enable :line-length)))' \
    'TRIVIAL-GLOB|::'

assert_usage_error \
    ":for-paths rejects a non-string selector element without leaking glob internals" \
    '(:mallet-config (:for-paths ("ok" :keyword) (:enable :line-length)))' \
    'TRIVIAL-GLOB|::'

assert_binary_config_usage_error
assert_unknown_config_form_reports_keyword_only
assert_valid_config_still_loads

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
