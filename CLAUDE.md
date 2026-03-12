# CLAUDE.md

## Project Overview

Mallet is a linter for Common Lisp (SBCL only). Key constraint: **file-based analysis only** — does not load/compile systems, just analyzes source text. Do not propose implementations that require system loading.

Rule classification criteria are in `docs/rule-classification.md`.

## Design Philosophy

Rules must report only definite, unambiguous issues. No subjective style preferences.

**Rule acceptance criteria:**
- Prevents runtime errors or bugs
- Enforces CL best practices with clear rationale
- Detects likely mistakes or anti-patterns
- NOT: subjective preferences, "how I like it" rules, or rules prone to false positives

## Development Commands

```bash
./bin/mallet .                                # Lint (must pass with zero violations)
./bin/mallet --fix .                          # Auto-fix violations

# FASL cache isolation (use ASDF_OUTPUT_TRANSLATIONS, NOT XDG_CACHE_HOME — see critical-rules.md)
_cache="$(pwd)/.cache"
_aot='(:output-translations (t ("'"$_cache"'/" :implementation)) :ignore-inherited-configuration)'

ASDF_OUTPUT_TRANSLATIONS="$_aot" qlot exec sbcl --noinform --non-interactive \
  --eval '(asdf:test-system "mallet")'        # Run all tests
ASDF_OUTPUT_TRANSLATIONS="$_aot" qlot exec sbcl --noinform --non-interactive \
  --eval '(asdf:load-system "mallet/tests")' \
  --eval '(rove:run-suite :mallet/tests/rules/<name>)'  # Run specific test suite
command make                                  # Build standalone executable
```


## ASDF Gotcha

Module names must be unique. When a file and directory share a name, use `:pathname`:
```lisp
(:module "parser-impl" :pathname "parser" :components ...)
```

## Adding New Rules

Every rule needs: (1) unit tests, (2) CLI fixture tests, (3) registration.

### 1. Unit Tests
Create `tests/rules/<rule-name>-test.lisp` (see existing tests for pattern). Add to `mallet.asd`.

### 2. CLI Fixture Tests
- `tests/fixtures/violations/<rule-name>.lisp` — code that triggers violations
- `tests/fixtures/violations/<rule-name>.expected` — expected output:
  ```
  # Format: line:column rule-name severity
  10:0 rule-name warning
  ```
- Optionally: `tests/fixtures/clean/<rule-name>.lisp` — valid code

### 3. Register
- Add case to `make-rule` in `src/rules.lisp`
- Add to preset lists in `src/config.lisp` (`make-default-config` and/or `make-all-config`)
- Export rule class from `src/rules.lisp`

### CLI Option Precedence (highest to lowest)
1. `--enable RULE:option=value`
2. `--disable RULE`
3. `--preset` / `--all` / `--none` (CLI)
4. Config file `:enable` / `:disable`
5. Config file `:extends`
6. Default preset
