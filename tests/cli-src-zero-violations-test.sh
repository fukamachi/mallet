#!/bin/bash
# Regression coverage for the source-tree zero-violation gate.

set -u

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
CLI="$PROJECT_DIR/bin/mallet"

TESTS_RUN=0
TESTS_FAILED=0

pass() {
    echo "  PASS: $1"
}

fail() {
    TESTS_FAILED=$((TESTS_FAILED + 1))
    echo "  FAIL: $1"
    if [ -n "${2:-}" ]; then
        echo "        $2"
    fi
}

run_test() {
    TESTS_RUN=$((TESTS_RUN + 1))
    echo "Test $TESTS_RUN: $1"
}

run_test "./bin/mallet src/ exits 0 with no violation diagnostics"
EXIT_CODE=0
OUTPUT=$("$CLI" "$PROJECT_DIR/src/" 2>&1) || EXIT_CODE=$?
DIAGNOSTIC_OUTPUT=$(printf '%s\n' "$OUTPUT" |
    grep -E '(^|[[:space:]])[0-9]+:[0-9]+|[1-9][0-9]* problem(s)? found' || true)
if [ "$EXIT_CODE" -eq 0 ] && [ -z "$DIAGNOSTIC_OUTPUT" ]; then
    pass "src/ is clean under the default lint command"
else
    fail "src/ is not clean under the default lint command" \
         "exit=$EXIT_CODE output=$(printf '%s' "$OUTPUT" | tr '\n' ' ' | sed 's/[[:space:]][[:space:]]*/ /g')"
fi

run_test "./bin/mallet --fail-on info src/ exits 0"
EXIT_CODE=0
OUTPUT=$("$CLI" --fail-on info "$PROJECT_DIR/src/" 2>&1) || EXIT_CODE=$?
if [ "$EXIT_CODE" -eq 0 ] &&
   printf '%s\n' "$OUTPUT" | grep -q "No problems found" &&
   ! printf '%s\n' "$OUTPUT" | grep -Eq '(^|[[:space:]])[0-9]+:[0-9]+|[1-9][0-9]* problem(s)? found'; then
    pass "src/ has no info-level diagnostics hidden below the default fail threshold"
else
    fail "src/ still has diagnostics when info is failure-thresholded" \
         "exit=$EXIT_CODE output=$(printf '%s' "$OUTPUT" | tr '\n' ' ' | sed 's/[[:space:]][[:space:]]*/ /g')"
fi

run_test "project config does not disable source cleanup rules"
CONFIG_DISABLE_OUTPUT=$(awk '
    index(tolower($0), "(:disable") {
        line = $0
        normalized = tolower(line)
        while (normalized !~ /\)/ && (getline next_line) > 0) {
            line = line " " next_line
            normalized = normalized " " tolower(next_line)
        }
        if (normalized ~ /(^|[^[:alnum:]*:-]):(needless-let\*|cyclomatic-complexity)([^[:alnum:]*-]|$)/) {
            print line
        }
    }
' "$PROJECT_DIR/.mallet.lisp")
if [ -z "$CONFIG_DISABLE_OUTPUT" ]; then
    pass ".mallet.lisp keeps needless-let* and cyclomatic-complexity enabled"
else
    fail ".mallet.lisp disables a source cleanup rule" \
         "matches=$(printf '%s' "$CONFIG_DISABLE_OUTPUT" | tr '\n' ' ' | sed 's/[[:space:]][[:space:]]*/ /g')"
fi

TMP_DIR=$(mktemp -d "${TMPDIR:-/tmp}/mallet-src-zero-violations.XXXXXX")
trap 'rm -rf "$TMP_DIR"' EXIT
KNOWN_BAD_FILE="$TMP_DIR/known-needless-let-star.lisp"
cat > "$KNOWN_BAD_FILE" <<'EOF'
(defun known-needless-let-star ()
  (let* ((x 1)
         (y 2))
    (+ x y)))
EOF

run_test "./bin/mallet can still detect a known needless-let* violation"
EXIT_CODE=0
OUTPUT=$("$CLI" --none --enable "needless-let*" "$KNOWN_BAD_FILE" 2>&1) || EXIT_CODE=$?
if [ "$EXIT_CODE" -ne 0 ] &&
   printf '%s\n' "$OUTPUT" | grep -Eq 'needless-let\*' &&
   printf '%s\n' "$OUTPUT" | grep -Eq 'warning'; then
    pass "production CLI reports a known needless-let* violation"
else
    fail "production CLI did not report a known needless-let* violation" \
         "exit=$EXIT_CODE output=$(printf '%s' "$OUTPUT" | tr '\n' ' ' | sed 's/[[:space:]][[:space:]]*/ /g')"
fi

run_test "targeted source forms are clean when rules are force-enabled"
EXIT_CODE=0
OUTPUT=$("$CLI" --none \
               --enable "needless-let*" \
               --enable "cyclomatic-complexity:max=25" \
               "$PROJECT_DIR/src/config.lisp" \
               "$PROJECT_DIR/src/init.lisp" \
               "$PROJECT_DIR/src/main.lisp" 2>&1) || EXIT_CODE=$?
SUPPRESSION_OUTPUT=$(grep -En 'mallet:(suppress|disable)[^[:space:]]*[[:space:]]+(:)?(needless-let\*|cyclomatic-complexity)\b|mallet:(suppress|disable)[^[:space:]]*[[:space:]]+(:)?ALL\b' \
    "$PROJECT_DIR/src/config.lisp" \
    "$PROJECT_DIR/src/init.lisp" \
    "$PROJECT_DIR/src/main.lisp" 2>/dev/null || true)
if [ "$EXIT_CODE" -eq 0 ] &&
   ! printf '%s\n' "$OUTPUT" | grep -Eq 'needless-let\*|cyclomatic-complexity' &&
   [ -z "$SUPPRESSION_OUTPUT" ]; then
    pass "flagged source locations are corrected, not hidden by config"
else
    fail "targeted rule run still reports flagged source forms" \
         "exit=$EXIT_CODE output=$(printf '%s' "$OUTPUT" | tr '\n' ' ' | sed 's/[[:space:]][[:space:]]*/ /g') suppressions=$(printf '%s' "$SUPPRESSION_OUTPUT" | tr '\n' ' ' | sed 's/[[:space:]][[:space:]]*/ /g')"
fi

echo ""
echo "Tests run: $TESTS_RUN"
echo "Failures: $TESTS_FAILED"

if [ "$TESTS_FAILED" -eq 0 ]; then
    exit 0
fi

exit 1
