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

for file in "$CLEAN_DIR"/*.lisp "$CLEAN_DIR"/*.asd; do
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

# Test violation files (.lisp and .asd)
for file in "$VIOLATIONS_DIR"/*.lisp "$VIOLATIONS_DIR"/*.asd; do
    if [ -f "$file" ]; then
        filename=$(basename "$file")
        base="${file%.*}"
        expected_file="${base}.expected"

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
        # Default --fail-on is warning: exit 1 for errors or warnings, not info
        EXPECTED_EXIT_CODE=0
        if [ -f "$expected_file" ]; then
            if grep -q ' error$' "$expected_file" 2>/dev/null || grep -q ' warning$' "$expected_file" 2>/dev/null; then
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
            ACTUAL_OUTPUT=$("$CLI" --config "$FIXTURES_CONFIG" "$file" 2>&1 || true)
            ACTUAL_COUNT=$(echo "$ACTUAL_OUTPUT" | grep -E '^\s+[0-9]+:[0-9]+' | wc -l | tr -d ' ')

            if [ "$ACTUAL_COUNT" -ne "$EXPECTED_COUNT" ]; then
                test_fail "Expected $EXPECTED_COUNT violations, found $ACTUAL_COUNT"
            else
                # Verify each expected violation line:col, rule-name, severity
                MATCH_FAIL=""
                while IFS= read -r expected_line; do
                    # Skip comments and empty lines
                    case "$expected_line" in
                        '#'*|'') continue ;;
                    esac
                    # Parse expected: "line:col rule-name severity"
                    exp_loc=$(echo "$expected_line" | awk '{print $1}')
                    exp_rule=$(echo "$expected_line" | awk '{print $2}')
                    exp_sev=$(echo "$expected_line" | awk '{print $3}')
                    # Check actual output contains a line with matching loc, rule, severity
                    # CLI format: "  line:col     severity     message  rule-name"
                    if ! echo "$ACTUAL_OUTPUT" | grep -E '^\s+'"$exp_loc"'\s' | grep -q "$exp_rule"; then
                        MATCH_FAIL="$MATCH_FAIL\n    Missing: $exp_loc $exp_rule $exp_sev"
                    elif ! echo "$ACTUAL_OUTPUT" | grep -E '^\s+'"$exp_loc"'\s' | grep "$exp_rule" | grep -q "$exp_sev"; then
                        MATCH_FAIL="$MATCH_FAIL\n    Wrong severity at $exp_loc $exp_rule: expected $exp_sev"
                    fi
                done < "$expected_file"
                if [ -z "$MATCH_FAIL" ]; then
                    test_pass
                else
                    test_fail "Violations do not match expected:$MATCH_FAIL"
                fi
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
test_start "missing-else rule detects violations"
OUTPUT=$("$CLI" --config "$FIXTURES_CONFIG" "$VIOLATIONS_DIR/form-rules.lisp" 2>&1 | grep -c "when.*unless" || true)
if [ "$OUTPUT" -ge 1 ]; then
    test_pass
else
    test_fail "Expected missing-else violations"
fi

test_start "Progn-in-conditional rule detects violations"
OUTPUT=$("$CLI" --config "$FIXTURES_CONFIG" "$VIOLATIONS_DIR/form-rules.lisp" 2>&1 | grep -c "cond.*progn" || true)
if [ "$OUTPUT" -ge 1 ]; then
    test_pass
else
    test_fail "Expected progn-in-conditional violations"
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

test_start "--strict: sets preset to :strict (alias for --preset strict)"
EXIT_CODE=0
"$CLI" --strict "$VIOLATIONS_DIR/no-package-use.lisp" 2>&1 > /dev/null || EXIT_CODE=$?
if [ $EXIT_CODE -eq 1 ]; then
    test_pass
else
    test_fail "Expected exit code 1 (no-package-use only fires under :strict, not :default), got $EXIT_CODE"
fi

test_start "--strict: strict-only rule fires; same file clean under --preset default"
EXIT_CODE=0
"$CLI" --preset default "$VIOLATIONS_DIR/no-package-use.lisp" 2>&1 > /dev/null || EXIT_CODE=$?
if [ $EXIT_CODE -eq 0 ]; then
    test_pass
else
    test_fail "Expected exit code 0 (no-package-use disabled under :default), got $EXIT_CODE"
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
IWE_COUNT=$("$CLI" --none --enable missing-else "$VIOLATIONS_DIR/comment-disable-enable.lisp" 2>&1 | grep -c "missing-else" || true)
if [ "$IWE_COUNT" -eq 2 ]; then
    test_pass
else
    test_fail "Expected exactly 2 missing-else violations (before-disable and after-enable), got $IWE_COUNT"
fi

# Text/token :disable/:enable suppression tests
test_start ":disable/:enable suppresses line-length violations inside region"
# Lines 10 and 11 are inside the disable/enable region and should NOT appear
REGION_COUNT=$("$CLI" --config "$FIXTURES_CONFIG" "$VIOLATIONS_DIR/line-length-disable.lisp" 2>&1 | grep -E '^\s+1[01]:' | grep -c "line-length" || true)
if [ "$REGION_COUNT" -eq 0 ]; then
    test_pass
else
    test_fail "Expected 0 line-length violations inside disabled region, got $REGION_COUNT"
fi

test_start ":disable/:enable still reports line-length violations outside region"
# Lines 7 and 14 are outside the disable/enable region and SHOULD appear
OUTSIDE_COUNT=$("$CLI" --config "$FIXTURES_CONFIG" "$VIOLATIONS_DIR/line-length-disable.lisp" 2>&1 | grep -E '^\s+(7|14):' | grep -c "line-length" || true)
if [ "$OUTSIDE_COUNT" -eq 2 ]; then
    test_pass
else
    test_fail "Expected 2 line-length violations outside disabled region, got $OUTSIDE_COUNT"
fi

# Documentation completeness
test_start "RULES.md documents :comment-ratio rule under METRICS section"
if grep -q ":comment-ratio" "$PROJECT_DIR/RULES.md" && grep -q ":min-lines" "$PROJECT_DIR/RULES.md"; then
    test_pass
else
    test_fail "Expected :comment-ratio entry with options in RULES.md"
fi

# Backward-compatible alias name tests
# These names are accepted by the tool (resolve-rule-alias maps them to canonical names).
echo ""
echo "Testing backward-compatible rule name aliases..."
echo ""

test_start "--enable eval-usage (alias name) does not produce a fatal error"
EXIT_CODE=0
"$CLI" --none --enable eval-usage "$CLEAN_DIR/basic.lisp" > /dev/null 2>&1 || EXIT_CODE=$?
if [ "$EXIT_CODE" -ne 3 ]; then
    test_pass
else
    test_fail "Expected no fatal error (exit 3) for alias name 'eval-usage', got exit $EXIT_CODE"
fi

test_start "--disable if-without-else (alias name) does not produce a fatal error"
EXIT_CODE=0
"$CLI" --none --disable if-without-else "$CLEAN_DIR/basic.lisp" > /dev/null 2>&1 || EXIT_CODE=$?
if [ "$EXIT_CODE" -ne 3 ]; then
    test_pass
else
    test_fail "Expected no fatal error (exit 3) for alias name 'if-without-else', got exit $EXIT_CODE"
fi

test_start "--enable interned-package-symbol (alias name) does not produce a fatal error"
EXIT_CODE=0
"$CLI" --none --enable interned-package-symbol "$CLEAN_DIR/basic.lisp" > /dev/null 2>&1 || EXIT_CODE=$?
if [ "$EXIT_CODE" -ne 3 ]; then
    test_pass
else
    test_fail "Expected no fatal error (exit 3) for alias name 'interned-package-symbol', got exit $EXIT_CODE"
fi

test_start "--enable ignore-errors-usage (alias name) does not produce a fatal error"
EXIT_CODE=0
"$CLI" --none --enable ignore-errors-usage "$CLEAN_DIR/basic.lisp" > /dev/null 2>&1 || EXIT_CODE=$?
if [ "$EXIT_CODE" -ne 3 ]; then
    test_pass
else
    test_fail "Expected no fatal error (exit 3) for alias name 'ignore-errors-usage', got exit $EXIT_CODE"
fi

test_start "--enable allow-other-keys (alias name) does not produce a fatal error"
EXIT_CODE=0
"$CLI" --none --enable allow-other-keys "$CLEAN_DIR/basic.lisp" > /dev/null 2>&1 || EXIT_CODE=$?
if [ "$EXIT_CODE" -ne 3 ]; then
    test_pass
else
    test_fail "Expected no fatal error (exit 3) for alias name 'allow-other-keys', got exit $EXIT_CODE"
fi

test_start "--enable final-newline (alias name) does not produce a fatal error"
EXIT_CODE=0
"$CLI" --none --enable final-newline "$CLEAN_DIR/basic.lisp" > /dev/null 2>&1 || EXIT_CODE=$?
if [ "$EXIT_CODE" -ne 3 ]; then
    test_pass
else
    test_fail "Expected no fatal error (exit 3) for alias name 'final-newline', got exit $EXIT_CODE"
fi

# Cross-file test-package detection
echo ""
echo "Testing cross-file test-package detection..."
echo ""

CROSSFILE_TMPDIR=$(mktemp -d)
cat > "$CROSSFILE_TMPDIR/package.lisp" <<'EOF'
(defpackage #:my-project/tests
  (:use #:cl #:rove))
EOF
cat > "$CROSSFILE_TMPDIR/tests.lisp" <<'EOF'
(in-package #:my-project/tests)
(deftest my-test
  (testing "internal access is fine in test files"
    (ok (some-lib::internal-fn 42))))
EOF

test_start "Cross-file: test package (package.lisp+tests.lisp) reports no double-colon violations"
CROSSFILE_OUTPUT=$("$CLI" --none --enable double-colon-access "$CROSSFILE_TMPDIR/tests.lisp" 2>&1)
if echo "$CROSSFILE_OUTPUT" | grep -q "No problems found"; then
    test_pass
else
    test_fail "Expected no violations for test-package ::access. Got: $CROSSFILE_OUTPUT"
fi

cat > "$CROSSFILE_TMPDIR/src.lisp" <<'EOF'
(in-package #:my-project/tests)
(defun bad-fn () some-lib::internal-fn)
EOF
test_start "Cross-file: test package file with :: reports violation when include-tests=t"
VIOLATION_COUNT=$("$CLI" --none --enable "double-colon-access:include-tests=t" "$CROSSFILE_TMPDIR/src.lisp" 2>&1 | grep -c "double-colon-access" || true)
if [ "$VIOLATION_COUNT" -ge 1 ]; then
    test_pass
else
    test_fail "Expected double-colon-access violation when include-tests=t, but found none"
fi

rm -rf "$CROSSFILE_TMPDIR"

# ---- User-defined preset tests ----
echo ""
echo "Testing user-defined presets..."
echo ""

PRESET_CONFIG="$FIXTURES/configs/user-preset.mallet.lisp"

# Create a temp file with a long line (>80 chars) for testing
TEMP_PRESET_FILE=$(mktemp /tmp/mallet-preset-test-XXXXXX.lisp)
echo '(defun foo () "This is a line that is definitely longer than eighty characters to trigger line-length rule")' > "$TEMP_PRESET_FILE"

# Test --preset with user-defined name (strict enables line-length with max 80)
test_start "--preset with user-defined 'strict' preset detects line-length"
OUTPUT=$("$CLI" --config "$PRESET_CONFIG" "$TEMP_PRESET_FILE" 2>&1 || true)
if echo "$OUTPUT" | grep -q "line-length"; then
    test_pass
else
    test_fail "Expected line-length violation with :strict preset"
fi

# Test --preset with user-defined relaxed (only trailing-whitespace, no line-length)
test_start "--preset with user-defined 'relaxed' preset ignores line-length"
OUTPUT=$("$CLI" --config "$PRESET_CONFIG" --preset relaxed "$TEMP_PRESET_FILE" 2>&1 || true)
if echo "$OUTPUT" | grep -q "line-length"; then
    test_fail "Unexpected line-length violation with :relaxed preset"
else
    test_pass
fi

# Test --preset with unknown name (no config) produces error mentioning .mallet.lisp
test_start "--preset unknown-name errors with .mallet.lisp mention"
OUTPUT=$("$CLI" --preset nonexistent "$TEMP_PRESET_FILE" 2>&1 || true)
if echo "$OUTPUT" | grep -qi ".mallet.lisp"; then
    test_pass
else
    test_fail "Expected error message to mention .mallet.lisp"
fi

# Test shadowed :default emits note to stderr
SHADOW_CONFIG="$FIXTURES/configs/shadow-default.mallet.lisp"
test_start "Shadowed :default emits note to stderr"
OUTPUT=$("$CLI" --config "$SHADOW_CONFIG" "$TEMP_PRESET_FILE" 2>&1 || true)
if echo "$OUTPUT" | grep -qi "shadowing"; then
    test_pass
else
    test_fail "Expected shadowing note in output"
fi

rm -f "$TEMP_PRESET_FILE"

# ---- End user-defined preset tests ----

# ---- Colon-prefixed rule name tests ----
echo ""
echo "Testing colon-prefixed rule names in --enable / --disable..."
echo ""

# Create a temp file with trailing whitespace (two spaces at end of first line)
COLON_TEST_FILE=$(mktemp /tmp/mallet-colon-test-XXXXXX.lisp)
printf '%s\n%s\n' '(defun foo ()  ' '  (+ 1 2))' > "$COLON_TEST_FILE"

test_start "--none --enable :trailing-whitespace reports trailing-whitespace warning"
COLON_TW_OUTPUT=$("$CLI" --none --enable :trailing-whitespace "$COLON_TEST_FILE" 2>&1 || true)
if echo "$COLON_TW_OUTPUT" | grep -q "trailing-whitespace" && echo "$COLON_TW_OUTPUT" | grep -q "warning"; then
    test_pass
else
    test_fail "Expected trailing-whitespace violation with 'warning' severity under --none --enable :trailing-whitespace, got: $COLON_TW_OUTPUT"
fi

test_start "--none --enable trailing-whitespace (bare) and :trailing-whitespace (colon) produce identical output"
set +e
BARE_TW_OUTPUT=$("$CLI" --none --enable trailing-whitespace "$COLON_TEST_FILE" 2>&1)
BARE_TW_STATUS=$?
COLON_TW_OUTPUT2=$("$CLI" --none --enable :trailing-whitespace "$COLON_TEST_FILE" 2>&1)
COLON_TW_STATUS=$?
set -e
if [ "$BARE_TW_STATUS" -eq "$COLON_TW_STATUS" ] && [ "$BARE_TW_OUTPUT" = "$COLON_TW_OUTPUT2" ]; then
    test_pass
else
    test_fail "Expected identical output and status for bare and colon-prefixed forms. Bare status: $BARE_TW_STATUS output: '$BARE_TW_OUTPUT' | Colon status: $COLON_TW_STATUS output: '$COLON_TW_OUTPUT2'"
fi

test_start "--none --disable :trailing-whitespace accepts colon-prefixed rule name"
set +e
DISABLE_TW_OUTPUT=$("$CLI" --none --disable :trailing-whitespace "$COLON_TEST_FILE" 2>&1)
DISABLE_TW_STATUS=$?
set -e
if [ "$DISABLE_TW_STATUS" -eq 0 ]; then
    test_pass
else
    test_fail "Expected --disable :trailing-whitespace to exit 0, got status $DISABLE_TW_STATUS with output: '$DISABLE_TW_OUTPUT'"
fi

rm -f "$COLON_TEST_FILE"

test_start "--enable :no-such-rule reports unknown-rule error containing the offending token"
set +e
NOSUCHRULE_OUTPUT=$("$CLI" --enable :no-such-rule "$CLEAN_DIR/basic.lisp" 2>&1)
NOSUCHRULE_STATUS=$?
set -e
if [ "$NOSUCHRULE_STATUS" -ne 0 ] && echo "$NOSUCHRULE_OUTPUT" | grep -q "Unknown rule:" && echo "$NOSUCHRULE_OUTPUT" | grep -qi "no-such-rule"; then
    test_pass
else
    test_fail "Expected non-zero 'Unknown rule:' error containing 'no-such-rule', got status $NOSUCHRULE_STATUS with output: '$NOSUCHRULE_OUTPUT'"
fi

# ---- End colon-prefixed rule name tests ----

# ---- Non-UTF-8 / unreadable file resilience tests ----
echo ""
echo "Testing non-UTF-8 / unreadable file resilience..."
echo ""

NON_UTF8_DIR=$(mktemp -d)
NON_UTF8_TMPOUT=$(mktemp)

# a-bad-encoding.lisp: C0 80 = invalid UTF-8 overlong two-byte sequence
printf '\xC0\x80' > "$NON_UTF8_DIR/a-bad-encoding.lisp"
# b-valid-one.lisp and c-valid-two.lisp: valid Lisp with trailing whitespace so they
# appear in the linter output (trailing-whitespace rule fires on them)
printf '(defun foo () nil)   \n' > "$NON_UTF8_DIR/b-valid-one.lisp"
printf '(defun bar () nil)   \n' > "$NON_UTF8_DIR/c-valid-two.lisp"

# Run once; capture output and exit code without letting set -e abort the script
NON_UTF8_EXIT=0
"$CLI" --no-color --none --enable trailing-whitespace "$NON_UTF8_DIR" \
    > "$NON_UTF8_TMPOUT" 2>&1 || NON_UTF8_EXIT=$?
NON_UTF8_OUTPUT=$(cat "$NON_UTF8_TMPOUT")
rm -f "$NON_UTF8_TMPOUT"

test_start "Non-UTF-8 in directory: run completes without fatal abort (exit != 3)"
if [ "$NON_UTF8_EXIT" -ne 3 ]; then
    test_pass
else
    test_fail "Expected run to complete; got exit $NON_UTF8_EXIT (fatal abort). Output: $NON_UTF8_OUTPUT"
fi

test_start "Non-UTF-8 in directory: both valid files are still processed and appear in output"
if echo "$NON_UTF8_OUTPUT" | grep -q "b-valid-one" && \
   echo "$NON_UTF8_OUTPUT" | grep -q "c-valid-two"; then
    test_pass
else
    test_fail "Expected both valid files in output. Got: $NON_UTF8_OUTPUT"
fi

test_start "Non-UTF-8 in directory: error message contains no raw Lisp printer artifacts"
if echo "$NON_UTF8_OUTPUT" | grep -qE '#P"|#<SB-'; then
    test_fail "Output contains raw Lisp printer artifacts (#P\" or #<SB-). Got: $NON_UTF8_OUTPUT"
else
    test_pass
fi

test_start "Non-UTF-8 in directory: overall exit is non-zero (unreadable file surfaces as failure)"
if [ "$NON_UTF8_EXIT" -ne 0 ]; then
    test_pass
else
    test_fail "Expected non-zero exit when scan contains an unreadable file; got exit 0"
fi

rm -rf "$NON_UTF8_DIR"

# ---- End non-UTF-8 resilience tests ----

# ---- Mutually exclusive flag tests ----
echo ""
echo "Testing mutually exclusive flags..."
echo ""

# F4: --fix and --fix-dry-run together must exit 3 with 'mutually exclusive' in stderr.
test_start "--fix --fix-dry-run exits 3 with message naming both flags"
EXCL_STDERR_FILE=$(mktemp)
EXCL_EXIT=0
"$CLI" --fix --fix-dry-run "$VIOLATIONS_DIR/line-length.lisp" >"$EXCL_STDERR_FILE.stdout" 2>"$EXCL_STDERR_FILE" || EXCL_EXIT=$?
EXCL_STDERR=$(cat "$EXCL_STDERR_FILE")
rm -f "$EXCL_STDERR_FILE" "$EXCL_STDERR_FILE.stdout"
if [ "$EXCL_EXIT" -eq 3 ] && \
   echo "$EXCL_STDERR" | grep -q -- "--fix" && \
   echo "$EXCL_STDERR" | grep -q -- "--fix-dry-run" && \
   echo "$EXCL_STDERR" | grep -qi "mutually exclusive"; then
    test_pass
else
    test_fail "Expected exit 3 and stderr naming both --fix and --fix-dry-run with phrase 'mutually exclusive'; got exit $EXCL_EXIT, stderr: $EXCL_STDERR"
fi

# F4b: --fix --fix-dry-run on a non-existent path must STILL exit 3 with 'mutually exclusive'
# in stderr, proving conflict detection precedes path validation. A violating implementation
# that validates paths first will emit a path-not-found error instead.
test_start "--fix --fix-dry-run on non-existent path still exits 3 with 'mutually exclusive'"
EXCL_NOPATH_STDERR_FILE=$(mktemp)
EXCL_NOPATH_EXIT=0
"$CLI" --fix --fix-dry-run "/tmp/mallet-does-not-exist-$$-xyzzy.lisp" \
    >"$EXCL_NOPATH_STDERR_FILE.stdout" 2>"$EXCL_NOPATH_STDERR_FILE" || EXCL_NOPATH_EXIT=$?
EXCL_NOPATH_STDERR=$(cat "$EXCL_NOPATH_STDERR_FILE")
rm -f "$EXCL_NOPATH_STDERR_FILE" "$EXCL_NOPATH_STDERR_FILE.stdout"
if [ "$EXCL_NOPATH_EXIT" -eq 3 ] && \
   echo "$EXCL_NOPATH_STDERR" | grep -qi "mutually exclusive"; then
    test_pass
else
    test_fail "Expected exit 3 and 'mutually exclusive' in stderr even for non-existent path; got exit $EXCL_NOPATH_EXIT, stderr: $EXCL_NOPATH_STDERR"
fi

# --fix alone must still work (no regression from the exclusivity check).
test_start "--fix alone (single flag) behaves normally"
FIX_TMPFILE=$(mktemp)
printf '(defun foo () nil)   \n' > "$FIX_TMPFILE"
FIX_EXIT=0
"$CLI" --fix "$FIX_TMPFILE" > /dev/null 2>&1 || FIX_EXIT=$?
rm -f "$FIX_TMPFILE"
if [ "$FIX_EXIT" -ne 3 ]; then
    test_pass
else
    test_fail "Expected --fix alone to work without fatal error; got exit $FIX_EXIT"
fi

# --all alone must still lint normally and detect violations (no regression).
test_start "--all alone (single flag) detects violations as before"
ALL_EXIT=0
"$CLI" --all "$VIOLATIONS_DIR/line-length.lisp" > /dev/null 2>&1 || ALL_EXIT=$?
if [ "$ALL_EXIT" -eq 1 ]; then
    test_pass
else
    test_fail "Expected --all alone to detect violations and exit 1; got exit $ALL_EXIT"
fi

# --fix <dir> must still fix files in the directory (no regression from exclusivity check).
# This also verifies the file was actually modified — exit code alone does not prove fixing ran.
test_start "--fix alone with a directory performs fix behavior"
FIX_DIR_TMPDIR=$(mktemp -d)
printf '(defun foo () nil)   \n' > "$FIX_DIR_TMPDIR/test.lisp"
FIX_DIR_EXIT=0
"$CLI" --fix "$FIX_DIR_TMPDIR" > /dev/null 2>&1 || FIX_DIR_EXIT=$?
FIX_DIR_CONTENT=$(cat "$FIX_DIR_TMPDIR/test.lisp" 2>/dev/null || echo "")
rm -rf "$FIX_DIR_TMPDIR"
if [ "$FIX_DIR_EXIT" -ne 3 ] && \
   ! echo "$FIX_DIR_CONTENT" | grep -q "   $"; then
    test_pass
else
    test_fail "Expected --fix <dir> to fix trailing whitespace in the file; got exit $FIX_DIR_EXIT, content: $FIX_DIR_CONTENT"
fi

# --all <dir> must still detect violations in directory (no regression from exclusivity check).
test_start "--all alone with a directory detects violations as before"
ALL_DIR_EXIT=0
"$CLI" --all "$VIOLATIONS_DIR" > /dev/null 2>&1 || ALL_DIR_EXIT=$?
if [ "$ALL_DIR_EXIT" -eq 1 ]; then
    test_pass
else
    test_fail "Expected --all <dir> to detect violations (exit 1); got exit $ALL_DIR_EXIT"
fi

# F3: --all and --none together must surface the conflict at the process level.
# This asserts the bin/mallet process boundary — not just the parse-args unit level —
# so a violating implementation that adds rejection only to parse-args while leaving
# the process path with last-wins silent behavior is caught here.
test_start "--all --none exits 3 with message naming both flags"
ALL_NONE_OUTPUT=""
ALL_NONE_EXIT=0
ALL_NONE_OUTPUT=$("$CLI" --all --none "$VIOLATIONS_DIR/line-length.lisp" 2>&1) || ALL_NONE_EXIT=$?
if [ "$ALL_NONE_EXIT" -eq 3 ] && \
   echo "$ALL_NONE_OUTPUT" | grep -q -- "--all" && \
   echo "$ALL_NONE_OUTPUT" | grep -q -- "--none"; then
    test_pass
else
    test_fail "Expected exit 3 and message naming both --all and --none; got exit $ALL_NONE_EXIT, output: $ALL_NONE_OUTPUT"
fi

# --none --all (reversed order) must also surface the conflict.
test_start "--none --all (reversed order) also exits 3 naming both flags"
NONE_ALL_OUTPUT=""
NONE_ALL_EXIT=0
NONE_ALL_OUTPUT=$("$CLI" --none --all "$VIOLATIONS_DIR/line-length.lisp" 2>&1) || NONE_ALL_EXIT=$?
if [ "$NONE_ALL_EXIT" -eq 3 ] && \
   echo "$NONE_ALL_OUTPUT" | grep -q -- "--all" && \
   echo "$NONE_ALL_OUTPUT" | grep -q -- "--none"; then
    test_pass
else
    test_fail "Expected exit 3 and message naming both --all and --none for reversed order; got exit $NONE_ALL_EXIT, output: $NONE_ALL_OUTPUT"
fi

# F3c: --all --none on an actual DIRECTORY must also surface the conflict.
# A violating implementation may route directory args through a separate scan path
# that applies last-wins preset selection before conflict validation, so directory
# invocations silently succeed while single-file invocations correctly exit 3.
test_start "--all --none on a directory exits 3 naming both flags"
ALL_NONE_DIR_OUTPUT=""
ALL_NONE_DIR_EXIT=0
ALL_NONE_DIR_OUTPUT=$("$CLI" --all --none "$VIOLATIONS_DIR" 2>&1) || ALL_NONE_DIR_EXIT=$?
if [ "$ALL_NONE_DIR_EXIT" -eq 3 ] && \
   echo "$ALL_NONE_DIR_OUTPUT" | grep -q -- "--all" && \
   echo "$ALL_NONE_DIR_OUTPUT" | grep -q -- "--none"; then
    test_pass
else
    test_fail "Expected exit 3 and message naming both --all and --none when invoked on a directory; got exit $ALL_NONE_DIR_EXIT, output: $ALL_NONE_DIR_OUTPUT"
fi

# ---- End mutually exclusive flag tests ----

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
