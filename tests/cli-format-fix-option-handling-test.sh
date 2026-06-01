#!/bin/bash
# CLI option handling tests for --format, --fix, and rule option errors.

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

write_fixable_file() {
    local path="$1"
    printf '(defun f () nil)   \n' > "$path"
}

# Strip a leading SBCL compiler preamble from captured output.
#
# Under the roswell `ros run` shim used in CI, the first invocation on a cold
# FASL cache prints compiler notes (`; file: ...`, `; note: ...`, `;
# compilation unit finished`) before our program runs. That output originates
# outside the mallet process and cannot be muted from bin/mallet. Mallet's own
# diagnostics never begin with `;`, so drop the leading run of comment/blank
# lines and assert against the program's actual output.
strip_compiler_preamble() {
    awk 'started { print; next }
         /^[[:space:]]*$/ { next }
         /^;/ { next }
         { started = 1; print }'
}

assert_conflict_error() {
    local name="$1"
    local expected_fix_option="$2"
    shift
    shift
    local file stdout_file stderr_file exit_code stderr_text

    run_test "$name"
    file="$TEMP_DIR/$name.lisp"
    stdout_file="$TEMP_DIR/$name.stdout"
    stderr_file="$TEMP_DIR/$name.stderr"
    write_fixable_file "$file"

    exit_code=0
    "$CLI" "$@" "$file" >"$stdout_file" 2>"$stderr_file" || exit_code=$?
    local stdout_text
    stdout_text=$(strip_compiler_preamble < "$stdout_file")
    stderr_text=$(strip_compiler_preamble < "$stderr_file")

    if [ "$exit_code" -ne 3 ]; then
        fail "$name exits $exit_code; expected 3" "$stderr_text"
        return
    fi

    if [ -n "$stdout_text" ]; then
        fail "$name wrote to stdout; expected no stdout for usage errors" "$stdout_text"
        return
    fi

    case "$stderr_text" in
        Error:*)
            ;;
        *)
            fail "$name stderr does not begin with Error:" "$stderr_text"
            return
            ;;
    esac

    if ! echo "$stderr_text" | grep -q -- "--format"; then
        fail "$name stderr does not name --format" "$stderr_text"
        return
    fi

    if ! echo "$stderr_text" | grep -Fq -- "$expected_fix_option"; then
        fail "$name stderr does not name $expected_fix_option" "$stderr_text"
        return
    fi

    if ! echo "$stderr_text" | grep -Eiq "conflict|unsupported|cannot|text"; then
        fail "$name stderr does not explain the option conflict" "$stderr_text"
        return
    fi

    pass "$name exits 3 with no stdout and names the --format/$expected_fix_option conflict"
}

assert_fix_text_still_works() {
    local file stdout_file stderr_file exit_code

    run_test "--fix --format text still fixes normally"
    file="$TEMP_DIR/fix-text.lisp"
    stdout_file="$TEMP_DIR/fix-text.stdout"
    stderr_file="$TEMP_DIR/fix-text.stderr"
    write_fixable_file "$file"

    exit_code=0
    "$CLI" --fix --format text "$file" >"$stdout_file" 2>"$stderr_file" || exit_code=$?

    if [ "$exit_code" -ne 0 ]; then
        fail "--fix --format text exits $exit_code; expected 0" "$(cat "$stderr_file")"
        return
    fi

    if grep -qE ' +$' "$file"; then
        fail "--fix --format text exited 0 but left trailing whitespace" "$(cat -vet "$file")"
        return
    fi

    pass "--fix --format text exits 0 and removes fixable whitespace"
}

assert_invalid_format_lists_all_values() {
    local file output exit_code expected_line expected_values token
    local text_count line_count json_count

    run_test "--format bad lists text, line, and json"
    file="$TEMP_DIR/invalid-format.lisp"
    printf '(defun f () nil)\n' > "$file"

    exit_code=0
    output=$("$CLI" --format bad "$file" 2>&1) || exit_code=$?

    if [ "$exit_code" -ne 3 ]; then
        fail "--format bad exits $exit_code; expected 3" "$output"
        return
    fi

    expected_line=$(printf '%s\n' "$output" | grep -E '^[[:space:]]*Expected:' || true)
    if [ -z "$expected_line" ]; then
        fail "--format bad omits Expected: accepted-values text" "$output"
        return
    fi

    expected_values=$(printf '%s\n' "$expected_line" \
        | sed -E 's/^[[:space:]]*Expected:[[:space:]]*//; s/,/ /g; s/[[:space:]]+or[[:space:]]+/ /g')
    text_count=0
    line_count=0
    json_count=0

    for token in $expected_values; do
        case "$token" in
            text)
                text_count=$((text_count + 1))
                ;;
            line)
                line_count=$((line_count + 1))
                ;;
            json)
                json_count=$((json_count + 1))
                ;;
            *)
                fail "--format bad Expected: line includes non-format prose instead of only accepted values" "$output"
                return
                ;;
        esac
    done

    if [ "$text_count" -ne 1 ] || [ "$line_count" -ne 1 ] || [ "$json_count" -ne 1 ]; then
        fail "--format bad Expected: line must list text, line, and json exactly once as accepted values" "$output"
        return
    fi

    pass "--format bad reports all accepted format values in Expected: line"
}

assert_bad_rule_option_is_usage_error() {
    local file output exit_code

    run_test "--enable line-length:max=abc reports a clean usage error"
    file="$TEMP_DIR/bad-rule-option.lisp"
    printf '(defun f () nil)\n' > "$file"

    exit_code=0
    output=$("$CLI" --enable line-length:max=abc "$file" 2>&1) || exit_code=$?
    output=$(printf '%s\n' "$output" | strip_compiler_preamble)

    if [ "$exit_code" -ne 3 ]; then
        fail "bad rule option exits $exit_code; expected 3" "$output"
        return
    fi

    case "$output" in
        Error:*)
            ;;
        *)
            fail "bad rule option output does not begin with Error:" "$output"
            return
            ;;
    esac

    if echo "$output" | grep -q "Fatal error:"; then
        fail "bad rule option leaked Fatal error:" "$output"
        return
    fi

    pass "bad rule option exits 3 and reports Error:"
}

echo "==================================================="
echo "Mallet CLI --format / --fix Option Handling Tests"
echo "==================================================="
echo ""

assert_conflict_error "fix-json-conflict" "--fix" --fix --format json
assert_conflict_error "fix-dry-run-line-conflict" "--fix-dry-run" --fix-dry-run --format line
assert_fix_text_still_works
assert_invalid_format_lists_all_values
assert_bad_rule_option_is_usage_error

echo ""
echo "==================================================="
echo "Tests run: $TESTS_RUN"
echo "Passed: $TESTS_PASSED"
echo "Failed: $TESTS_FAILED"
echo "==================================================="

if [ "$TESTS_FAILED" -gt 0 ]; then
    exit 1
fi

exit 0
