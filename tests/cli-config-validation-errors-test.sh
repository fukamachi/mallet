#!/bin/bash
# CLI config-file validation error contract tests.
#
# Contract being tested:
#   Non-UTF-8 config files are CLI usage errors: they print "Error:" and exit 3.
#   Messages must be sanitized so implementation/package internals do not leak.

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

# Strip a leading SBCL/Quicklisp diagnostic preamble from captured output.
#
# CI runs the CLI through roswell's `ros run` shim, which bootstraps roswell's
# global Quicklisp before mallet runs. That emits noise ahead of the program's
# own output — compiler notes on a cold cache (`; note: ...`, `; compilation
# unit finished`) and, on every run, a `WARNING:` block ("redefining
# QL-DIST:INSTALL ...") whose detail continues on an indented line. None of it
# comes from the mallet process, so it cannot be muted from bin/mallet. Drop the
# leading run of such diagnostic lines — comment lines (`;...`), blank lines,
# `WARNING:`/`WARN`/`STYLE-WARNING:` headers, and their indented continuation
# lines — and assert against the program's real first line. Mallet's own output
# is never indented and never starts with these markers, so it survives intact.
strip_compiler_preamble() {
    awk '
        started { print; next }
        /^[[:space:]]*$/ { next }                 # blank line
        /^;/ { next }                             # SBCL compiler note / comment
        /^[[:space:]]/ { next }                   # indented continuation of a warning
        /^(WARNING|WARN|STYLE-WARNING|CAUTION|NOTE|debugger invoked):?/ { next }
        /redefining/ { next }                     # "redefining X ... in DEFMETHOD"
        { started = 1; print }
    '
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
    output=$(printf '%s\n' "$output" | strip_compiler_preamble)
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

# Invalid :for-paths shapes have no Lisp-unit coverage for the error path, so
# the process-level contract (exit 3, "Error:" prefix, no leaked glob internals)
# is asserted here rather than duplicating the Lisp config tests.
assert_for_paths_config_usage_error() {
    local description="$1"
    local config_content="$2"
    local work_dir config_file source_file output exit_code

    run_test "$description"
    work_dir=$(mktemp -d)
    config_file="$work_dir/mallet-config.lisp"
    source_file="$work_dir/source.lisp"
    printf '%s\n' "$config_content" > "$config_file"
    write_source_file "$source_file"

    exit_code=0
    output=$("$CLI" --config "$config_file" "$source_file" 2>&1) || exit_code=$?
    output=$(printf '%s\n' "$output" | strip_compiler_preamble)
    rm -rf "$work_dir"

    if [ "$exit_code" -ne 3 ]; then
        fail "$description exits $exit_code; expected 3" \
             "invalid :for-paths configs must be CLI usage errors"
        return
    fi

    case "$output" in
        Error:*)
            ;;
        *)
            fail "$description exits 3 but output does not begin with Error:" "$output"
            return
            ;;
    esac

    if echo "$output" | grep -Eq 'Fatal error:|TRIVIAL-GLOB|::'; then
        fail "$description output leaks an internal error detail" "$output"
        return
    fi

    pass "$description exits 3 and hides internal details"
}

echo "=============================================="
echo "Mallet CLI Config Validation Error Tests"
echo "=============================================="
echo ""

assert_binary_config_usage_error

assert_for_paths_config_usage_error \
    "for-paths non-list selector" \
    '(:mallet-config (:for-paths :keyword (:enable :line-length)))'

assert_for_paths_config_usage_error \
    "for-paths invalid selector element" \
    '(:mallet-config (:for-paths ("ok" :keyword) (:enable :line-length)))'

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
