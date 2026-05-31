#!/bin/bash
# CLI exit-code contract tests for Mallet.
#
# Contract being tested:
#   0  - No violations / info-only, OR help/version output, OR no-arg help
#   1  - One or more violations at or above the --fail-on threshold
#   2  - Runtime I/O failure (e.g., permission denied during --fix)
#   3  - CLI usage error (unknown flag, missing value, invalid argument)
#
# Tests 1 (no-arg exits 0) and 5 (read-only --fix exits 2) FAIL against the
# unmodified codebase.  The rest document already-correct behaviour that must
# not regress.
#
# Test 6 (violations found exits 1) currently PASSES and must not regress;
# it verifies that exit 2 is distinct from exit 1 after the fix is applied.

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
CLI="$PROJECT_DIR/bin/mallet"

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

echo "========================================"
echo "Mallet CLI Exit-Code Contract Tests"
echo "========================================"
echo ""

# ── Test 1: No arguments prints help and exits 0 ─────────────────────────────
# CURRENTLY FAILS: the current code calls (uiop:quit 1) at the no-arg branch.
# After the fix the branch must call (uiop:quit 0) so that presence probes
# like "mallet || install" and Dockerfile "RUN mallet && ..." don't misfire.
run_test "no arguments: prints help and exits 0"
EXIT_CODE=0
# Use OUTPUT=$(cmd) || EXIT_CODE=$? so that EXIT_CODE is captured in the
# PARENT shell, not the subshell created by the $(...) command substitution.
OUTPUT=$("$CLI" 2>&1) || EXIT_CODE=$?
HAS_USAGE=0; echo "$OUTPUT" | grep -qi "usage:" && HAS_USAGE=1
HAS_FLAG=0
echo "$OUTPUT" | grep -q -- "--fix" && HAS_FLAG=1
echo "$OUTPUT" | grep -q -- "--all" && HAS_FLAG=1
if [ "$EXIT_CODE" -eq 0 ] && [ "$HAS_USAGE" -eq 1 ] && [ "$HAS_FLAG" -eq 1 ]; then
    pass "exits 0 and prints help text containing 'Usage:' and a recognizable flag name"
elif [ "$EXIT_CODE" -ne 0 ]; then
    fail "exits $EXIT_CODE; expected 0" \
         "no-arg help must exit 0 so presence probes do not wrongly trigger"
elif [ "$HAS_USAGE" -eq 0 ]; then
    fail "exits 0 but no 'Usage:' header in output"
else
    fail "exits 0 and has 'Usage:' but neither --fix nor --all appear in output" \
         "help text must list recognizable flag names so it is useful"
fi

# ── Test 2: --help exits 0 ────────────────────────────────────────────────────
run_test "--help exits 0"
EXIT_CODE=0
"$CLI" --help > /dev/null 2>&1 || EXIT_CODE=$?
if [ "$EXIT_CODE" -eq 0 ]; then
    pass "--help exits 0"
else
    fail "--help exits $EXIT_CODE; expected 0"
fi

# ── Test 3: --version exits 0 ─────────────────────────────────────────────────
run_test "--version exits 0"
EXIT_CODE=0
"$CLI" --version > /dev/null 2>&1 || EXIT_CODE=$?
if [ "$EXIT_CODE" -eq 0 ]; then
    pass "--version exits 0"
else
    fail "--version exits $EXIT_CODE; expected 0"
fi

# ── Test 4: Unknown CLI flag exits 3 ──────────────────────────────────────────
run_test "unknown flag --bogus-flag exits 3 (CLI usage error)"
EXIT_CODE=0
"$CLI" --bogus-flag > /dev/null 2>&1 || EXIT_CODE=$?
if [ "$EXIT_CODE" -eq 3 ]; then
    pass "--bogus-flag exits 3 (CLI usage error, distinct from runtime failure)"
else
    fail "--bogus-flag exits $EXIT_CODE; expected 3" \
         "exit 3 must be reserved for CLI argument/usage errors"
fi

# ── Test 5: --fix on a read-only file exits 2 ─────────────────────────────────
# CURRENTLY FAILS: after the fixer hardening the write error is caught and
# returned as an unfixed warning violation, so the code exits 1 (violations
# found).  After the exit-code fix it must exit 2 (runtime I/O failure).
run_test "--fix against a read-only file exits 2 (runtime I/O failure)"
RO_DIR=$(mktemp -d)
RO_FILE="$RO_DIR/readonly.lisp"
# Write a file that has a fixable violation (trailing whitespace on line 1).
printf '(defun foo () t)   \n' > "$RO_FILE"
# Revoke write permission so the fixer cannot overwrite the file.
chmod 444 "$RO_FILE"
EXIT_CODE=0
"$CLI" --all --fix "$RO_FILE" > /dev/null 2>&1 || EXIT_CODE=$?
chmod 644 "$RO_FILE"
rm -rf "$RO_DIR"
if [ "$EXIT_CODE" -eq 2 ]; then
    pass "--fix on read-only file exits 2 (distinct from 1=violations and 3=usage-error)"
else
    fail "--fix on read-only file exits $EXIT_CODE; expected 2" \
         "runtime I/O failures must use exit 2, not exit 1 (violations) or 3 (CLI error)"
fi

# ── Test 6: Violations found exits 1 WITH real diagnostic output ──────────────
# Verifies exit 2 is DISTINCT from exit 1 AND stdout contains a real diagnostic
# line.  The prior version only checked the exit code; a routing stub that exits
# 1 without any rule output would have passed.  This version additionally checks
# that stdout contains a diagnostic line in GCC format (path:line:col: severity:)
# so a stub cannot pass by exiting 1 alone.
run_test "violations found: exits 1 AND stdout has diagnostic in path:line:col format"
VIO_DIR=$(mktemp -d)
VIO_FILE="$VIO_DIR/violations.lisp"
printf '(defun foo () t)   \n' > "$VIO_FILE"
EXIT_CODE=0
# --format line gives GCC-style output: path:N:N: severity: message [rule-name]
# Redirect only stderr so we capture diagnostic stdout.
VIO_OUTPUT=$("$CLI" --all --format line "$VIO_FILE" 2>/dev/null) || EXIT_CODE=$?
rm -rf "$VIO_DIR"
if [ "$EXIT_CODE" -ne 1 ]; then
    fail "violations found exits $EXIT_CODE; expected 1" \
         "exit 1 must remain the code for violations; exit 2 is only for I/O failures"
else
    # Check stdout has at least one GCC-format diagnostic line:
    # path:positive-line:col: severity: ... [rule-name]
    # A routing stub that exits 1 without real rule output fails here.
    DIAG_FOUND=0
    echo "$VIO_OUTPUT" | grep -qE ':[1-9][0-9]*:[0-9]+: [a-z]+:' && DIAG_FOUND=1
    if [ "$DIAG_FOUND" -eq 1 ]; then
        pass "exits 1 and stdout has diagnostic in path:line:col: severity: format"
    else
        fail "exits 1 but stdout has no path:line:col: severity: diagnostic line" \
             "a routing stub that exits 1 without real rule output fails this check"
    fi
fi

# ── Test 8: Clean file (no violations) exits 0 ───────────────────────────────
# Rejects any stub that always exits 1 for linting invocations.
run_test "clean file with no violations exits 0 (not 1)"
CLEAN_DIR=$(mktemp -d)
CLEAN_FILE="$CLEAN_DIR/clean.lisp"
# Minimal, syntactically valid Lisp with no violations under the default preset.
printf '(defun hello () t)\n' > "$CLEAN_FILE"
EXIT_CODE=0
"$CLI" "$CLEAN_FILE" > /dev/null 2>&1 || EXIT_CODE=$?
rm -rf "$CLEAN_DIR"
if [ "$EXIT_CODE" -eq 0 ]; then
    pass "clean file exits 0 (no violations, no I/O error)"
else
    fail "clean file exits $EXIT_CODE; expected 0" \
         "exit 0 must mean clean; a stub that always exits 1 fails here"
fi

# ── Test 9: --fix on writable file with no violations exits 0 ────────────────
# Rejects any stub that always exits 2 whenever --fix is supplied.
run_test "--fix on writable file with no violations exits 0 (not 2)"
FIX_DIR=$(mktemp -d)
FIX_FILE="$FIX_DIR/clean.lisp"
printf '(defun hello () t)\n' > "$FIX_FILE"
EXIT_CODE=0
"$CLI" --fix "$FIX_FILE" > /dev/null 2>&1 || EXIT_CODE=$?
rm -rf "$FIX_DIR"
if [ "$EXIT_CODE" -eq 0 ]; then
    pass "--fix on clean writable file exits 0 (no I/O error, no violations)"
else
    fail "--fix on clean writable file exits $EXIT_CODE; expected 0" \
         "exit 2 must only fire for actual I/O failures, not on every --fix run"
fi

# ── Test 10: --fix removes fixable violation AND reports unfixable ones in stdout ─
# Two checks must pass together: (a) trailing whitespace is removed from the file
# content, and (b) stdout contains 'unused-variables' because the real Lisp engine
# reports ALL detected violations including ones it cannot auto-fix.
# A sed-based stub can satisfy (a) but cannot detect unused parameters, so it
# produces no 'unused-variables' mention in stdout and fails (b).
run_test "--fix: trailing whitespace removed from file AND stdout contains unused-variables"
FIX_CONTENT_DIR=$(mktemp -d)
FIX_CONTENT_FILE="$FIX_CONTENT_DIR/fixable.lisp"
# y is declared but never referenced (unused-variables, not auto-fixable).
# Three trailing spaces (trailing-whitespace, auto-fixable under --all).
printf '(defun foo (x y) "doc" x)   \n' > "$FIX_CONTENT_FILE"
CONTENT_BEFORE=$(cat "$FIX_CONTENT_FILE")
EXIT_CODE=0
# Capture stdout so we can check that unfixable violations are reported.
FIX_OUTPUT=$("$CLI" --all --fix "$FIX_CONTENT_FILE" 2>/dev/null) || EXIT_CODE=$?
CONTENT_AFTER=$(cat "$FIX_CONTENT_FILE")
rm -rf "$FIX_CONTENT_DIR"
if [ "$CONTENT_BEFORE" = "$CONTENT_AFTER" ]; then
    fail "--fix did not modify the file content (trailing whitespace unchanged)" \
         "a stub that routes exit codes without writing to the file fails this check"
elif printf '%s' "$CONTENT_AFTER" | grep -qE ' +$'; then
    fail "file was modified but trailing whitespace still present after --fix" \
         "the trailing-whitespace violation must be absent from file content after the run"
elif ! echo "$FIX_OUTPUT" | grep -qE '[0-9]+:[0-9]+.*unused-variables'; then
    fail "--fix did not report 'unused-variables' in a line:col diagnostic line" \
         "real output has 'line:col  severity  message  unused-variables'; a stub echoing the bare string fails here"
else
    pass "--fix removed trailing whitespace AND reported unused-variables in stdout"
fi

# ── Test 11: unused-variables violation exits 1 with rule in --list-rules ─────
# The file has NO trailing whitespace, so a trailing-whitespace grep stub exits 0
# and fails the exit-1 requirement.  We also extract the bracketed [rule-name]
# from the diagnostic line and verify it appears verbatim in 'mallet --list-rules',
# confirming the emitted rule name is registered and not invented.
run_test "unused-variables: exits 1, GCC-format diagnostic, rule-name in --list-rules"
UV_DIR=$(mktemp -d)
UV_FILE="$UV_DIR/unused-var.lisp"
# y is declared but never referenced — unused-variables violation.
# No trailing whitespace: a whitespace-grep stub exits 0, failing the exit-1 check.
printf '(defun foo (x y) "doc" x)\n' > "$UV_FILE"
EXIT_CODE=0
UV_OUTPUT=$("$CLI" --all --format line "$UV_FILE" 2>/dev/null) || EXIT_CODE=$?
rm -rf "$UV_DIR"
if [ "$EXIT_CODE" -ne 1 ]; then
    fail "unused-variables file exits $EXIT_CODE; expected 1" \
         "unused-variable detection requires real Lisp binding analysis, not a grep stub"
else
    DIAG_FOUND=0
    echo "$UV_OUTPUT" | grep -qE ':[1-9][0-9]*:[0-9]+: [a-z]+:' && DIAG_FOUND=1
    if [ "$DIAG_FOUND" -eq 0 ]; then
        fail "exits 1 but stdout has no path:line:col: severity: diagnostic line" \
             "expected GCC-format output with at least one diagnostic"
    else
        RULE_NAME=$(echo "$UV_OUTPUT" | grep -oE '\[[a-z][a-z0-9-]*\]' | tail -1 | tr -d '[]')
        if [ -z "$RULE_NAME" ]; then
            fail "diagnostic line has no [rule-name] bracketed suffix" \
                 "GCC-format output must end with [rule-name] in brackets"
        else
            LIST_RULES_OUTPUT=$("$CLI" --list-rules 2>/dev/null)
            RULE_IN_LIST=0
            echo "$LIST_RULES_OUTPUT" | grep -qxF "$RULE_NAME" && RULE_IN_LIST=1
            if [ "$RULE_IN_LIST" -eq 1 ]; then
                pass "exits 1, GCC-format diagnostic, rule '$RULE_NAME' found verbatim in --list-rules"
            else
                fail "rule name '$RULE_NAME' not found as a line in mallet --list-rules" \
                     "the bracketed rule-name in diagnostics must match a registered rule name exactly"
            fi
        fi
    fi
fi

# ── Test 12: comment-reference does not satisfy unused-variables ──────────────
# The stub extracts lambda-list params and counts whole-word occurrences.
# A param referenced only in a semicolon comment registers as "used" (count=2),
# so the stub exits 0.  The real Lisp engine ignores comments and exits 1.
run_test "unused-variables: param in comment only is still flagged (exits 1)"
CR_DIR=$(mktemp -d)
CR_FILE="$CR_DIR/comment-ref.lisp"
# y appears in the lambda list AND in a comment, but never in actual code.
# Grep word-count: 2 occurrences → stub considers y "used" → exits 0.
# Real engine: comment is not code → y is unused → exits 1.
printf '(defun foo (x y)\n  "doc"\n  ; y is not used\n  x)\n' > "$CR_FILE"
EXIT_CODE=0
CR_OUTPUT=$("$CLI" --all --format line "$CR_FILE" 2>/dev/null) || EXIT_CODE=$?
rm -rf "$CR_DIR"
if [ "$EXIT_CODE" -ne 1 ]; then
    fail "comment-ref file exits $EXIT_CODE; expected 1" \
         "a grep word-count stub counts the comment occurrence and wrongly exits 0"
else
    FOUND_UV=0
    echo "$CR_OUTPUT" | grep -q 'unused-variables' && FOUND_UV=1
    if [ "$FOUND_UV" -eq 1 ]; then
        pass "exits 1 and unused-variables diagnostic present (comment reference not counted as use)"
    else
        fail "exits 1 but no unused-variables diagnostic in output" \
             "expected unused-variables warning for y when it only appears in a comment"
    fi
fi

# ── Test 13: let-binding shadow — param shadowed by inner let is still unused ─
# The stub strips comments and then searches the body text for each param name.
# For (defun foo (x) (let ((x 2)) x)), the text 'x' appears in the let binding
# AND the body, so the stub exits 0 (it thinks x is used).
# The real Lisp engine understands that the inner (let ((x 2)) ...) shadows the
# outer param x: the body 'x' refers to the let binding, not the param.
# The outer param x is therefore unused and the real engine exits 1.
# A text-based stub cannot distinguish a let-binding occurrence from a genuine
# variable reference without full scope analysis — it exits 0 and fails here.
run_test "let-shadow: param shadowed by inner let binding is flagged as unused (exits 1)"
LB_DIR=$(mktemp -d)
LB_FILE="$LB_DIR/let-bind-shadow.lisp"
# x is the outer defun param; the let binding introduces a new x that shadows it.
# Body 'x' resolves to the let binding, so the outer param x is never used.
# Text stub: finds 'x' in '(let ((x 2)) x)' → concludes param used → exits 0.
# Real engine: detects let shadow, finds param unreferenced → exits 1.
printf '(defun foo (x)\n  (let ((x 2))\n    x))\n' > "$LB_FILE"
EXIT_CODE=0
LB_OUTPUT=$("$CLI" --all --format line "$LB_FILE" 2>/dev/null) || EXIT_CODE=$?
rm -rf "$LB_DIR"
if [ "$EXIT_CODE" -ne 1 ]; then
    fail "let-shadow file exits $EXIT_CODE; expected 1" \
         "text stub finds 'x' in body and exits 0; real scope analysis detects outer x is unused"
else
    FOUND_UV=0
    echo "$LB_OUTPUT" | grep -q 'unused-variables' && FOUND_UV=1
    if [ "$FOUND_UV" -eq 1 ]; then
        pass "exits 1 and unused-variables diagnostic present (let-binding shadow detected by scope analysis)"
    else
        fail "exits 1 but no unused-variables diagnostic in output" \
             "expected unused-variables warning for x when shadowed by inner let"
    fi
fi

# ── Summary ───────────────────────────────────────────────────────────────────
echo ""
echo "========================================"
echo "Test Summary: $TESTS_PASSED/$TESTS_RUN passed"
echo "========================================"
if [ "$TESTS_FAILED" -gt 0 ]; then
    echo -e "${RED}$TESTS_FAILED test(s) failed.${NC}"
    exit 1
else
    echo -e "${GREEN}All tests passed.${NC}"
    exit 0
fi
