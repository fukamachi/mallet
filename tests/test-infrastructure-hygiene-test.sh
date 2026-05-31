#!/bin/bash
# Focused guards for test-infrastructure hygiene regressions.

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"

fail() {
    echo "FAIL: $1"
    exit 1
}

assert_not_contains() {
    local file="$1"
    local pattern="$2"
    local description="$3"

    if grep -Fq -- "$pattern" "$PROJECT_DIR/$file"; then
        fail "$description"
    fi
}

assert_contains() {
    local file="$1"
    local pattern="$2"
    local description="$3"

    if ! grep -Fq -- "$pattern" "$PROJECT_DIR/$file"; then
        fail "$description"
    fi
}

assert_no_duplicate_flag_invocation() {
    local file="$1"
    local flag="$2"
    local description="$3"

    if ! FLAG="$flag" perl -0ne '
        s/\\[[:blank:]]*\n/ /g;
        for my $line (split /\n/) {
            my $count = () = $line =~ /(^|\s)\Q$ENV{FLAG}\E(?=\s|$)/g;
            exit 1 if 1 < $count;
        }
    ' "$PROJECT_DIR/$file"; then
        fail "$description"
    fi
}

assert_not_contains "tests/init-test.lisp" "/tmp/claude-1000/" \
    "tests/init-test.lisp must not hardcode the Foundry temp directory"
assert_not_contains "tests/init-test.lisp" "#P\"/tmp/\"" \
    "tests/init-test.lisp must not hardcode /tmp as the temp directory base"
assert_contains "tests/init-test.lisp" "(uiop:temporary-directory)" \
    "tests/init-test.lisp must derive the temp directory base from UIOP"
assert_no_duplicate_flag_invocation "tests/cli-format-fix-option-handling-test.sh" "--fix" \
    "CLI format/fix option test must not duplicate --fix"
assert_no_duplicate_flag_invocation "tests/cli-format-fix-option-handling-test.sh" "--fix-dry-run" \
    "CLI format/fix option test must not duplicate --fix-dry-run"
assert_not_contains "tests/rules/coalton-base-test.lisp" "(defmethod base:check-form ((rule aware-test-rule)" \
    "coalton-base tests must not redefine aware-test-rule check-form at runtime"
assert_contains "tests/rules/coalton-base-test.lisp" "coalton-form-aware-test-rule" \
    "coalton-base tests must keep a coalton-form-specific throwaway rule class"
assert_contains "tests/rules/coalton-base-test.lisp" "non-coalton-form-aware-test-rule" \
    "coalton-base tests must keep a non-coalton-form-specific throwaway rule class"
assert_contains "tests/rules/coalton-base-test.lisp" "(not (eq (class-of coalton-rule) (class-of non-coalton-rule)))" \
    "coalton-base tests must assert the throwaway rule classes are distinct"

echo "PASS: test infrastructure hygiene checks"
