# Mallet

[![CI](https://github.com/fukamachi/mallet/actions/workflows/ci.yml/badge.svg)](https://github.com/fukamachi/mallet/actions/workflows/ci.yml)

A Common Lisp linter focused on catching real mistakes without enforcing subjective style preferences.

## Philosophy

Mallet works out of the box. Its default rules detect only clear issues—definite mistakes and widely accepted style problems. This keeps the output useful and free from noise.

Projects differ, and so do their rules. Mallet also provides a growing set of optional checks that you can enable as needed.

A simple start, with room to grow.

## Installation

```bash
git clone https://github.com/fukamachi/mallet.git
cd mallet
make
```

This creates a standalone executable `./mallet`. You can run it directly or move it anywhere you want:

```bash
# Run directly from the build directory
./mallet src

# Or move to a directory in your PATH
mv mallet ~/.local/bin/mallet  # or /usr/local/bin/mallet, ~/bin/mallet, etc.
```

## Usage

### Basic Linting

```bash
# Lint files
mallet src/main.lisp
mallet src/*.lisp
mallet src  # recursively lints .lisp and .asd files

# Enable all rules
mallet -a src
mallet --all src
```

### CLI Rule Configuration

Enable or disable rules from the command line without needing a config file:

```bash
# Enable additional specific rules
mallet --enable cyclomatic-complexity src/
mallet --enable cyclomatic-complexity:max=15 src/

# Enable only specific rules
mallet --none --enable cyclomatic-complexity:max=15 src/

# Enable multiple rules with options
mallet --enable line-length:max=80 --enable cyclomatic-complexity:max=10 src/

# Disable specific rules (override config/preset)
mallet --disable unused-variables tests/

# Fail on warnings and above (useful for CI)
mallet --fail-on warning src/

# Fail on any violation (equivalent to --strict)
mallet --fail-on info src/
```

**Severity levels** (for `--fail-on`):
- `error` - Objectively wrong code (exit 1 by default)
- `warning` - Likely bugs or dangerous patterns
- `info` - Style preferences, metrics, and formatting suggestions

**Exit codes:**
- `0` - No violations at or above the `--fail-on` threshold
- `1` - One or more violations at or above the threshold
- `3` - CLI usage error (unknown flag, missing argument, invalid file)

> **Migration note:** Previous versions used exit code 2 for `:error`-level violations
> and exit code 1 for `:warning`-level violations. Both now exit 1. If your CI pipeline
> distinguished between exit 2 and exit 1, replace that logic with `--fail-on`:
>
> ```bash
> # Old: exit 2 for errors only
> # New equivalent:
> mallet --fail-on error src/   # exit 1 if any :error violation found
>
> # Old: exit 1 for warnings, exit 2 for errors
> # New equivalent (treat warnings as failures too):
> mallet --fail-on warning src/
> ```

### Auto-Fix

Mallet can automatically fix many common violations:

```bash
# Auto-fix violations
mallet --fix src/                 # Fix and write changes
mallet --fix-dry-run src/         # Preview fixes without changing files
mallet -a --fix src/              # Fix with all rules enabled
```

**Fixable rules:**
- `:trailing-whitespace` - Remove trailing whitespace from lines
- `:final-newline` - Ensure files end with a newline
- `:consecutive-blank-lines` - Reduce excessive blank lines
- `:unused-local-nicknames` - Remove unused package nicknames from `defpackage`
- `:unused-imported-symbols` - Remove unused imports from `defpackage`

## Configuration

Create `.mallet.lisp` in your project root:

```lisp
(:mallet-config
 (:extends :default)  ; or :all

 ;; Ignore files/directories (uses glob patterns)
 (:ignore "**/example.lisp"         ; ignore at any level
          "**/*-generated.lisp"     ; ignore generated files
          "vendor/**/*.lisp")       ; ignore vendor directory

 ;; Enable rules with options
 (:enable :line-length :max 100)
 (:enable :consecutive-blank-lines :max 2)

 ;; Enable a rule with a custom severity override
 (:enable :cyclomatic-complexity :severity :warning)

 ;; Disable specific rules
 (:disable :constant-naming)

 ;; Override severity for all rules in a category
 (:set-severity :metrics :info)

 ;; Path-specific overrides
 (:for-paths ("tests")
   (:enable :line-length :max 120)
   (:disable :unused-variables)))
```

**Presets:**

- `:default` - Only universally-accepted rules (catch mistakes, not enforce style)
- `:all` - All rules enabled (for exploration)

**Path overrides:**

Use `:for-paths` to apply different rules to specific directories or files:
- Directory names: `"tests"` → matches `tests/**/*.{lisp,asd}`
- Glob patterns: `"src/**/*.lisp"` → matches pattern exactly

Mallet auto-discovers `.mallet.lisp` by walking up from the current directory.

**Configuration precedence:**

CLI options override config files, which override presets:

```
CLI options (--enable, --disable) > .mallet.lisp > Presets (:default, :all)
```

Example: `mallet --enable line-length:max=80 src/` overrides any `:line-length` settings in `.mallet.lisp`.

## Suppressing Violations

Use `#+mallet` with `declaim` or `declare` to suppress specific rules. The `#+mallet` flag makes suppressions invisible during normal code loading.

**Suppress next form:**
```lisp
#+mallet
(declaim (mallet:suppress-next :if-without-else))
(defun foo () (if x (print "yes")))  ; Suppressed
```

**Suppress region:**
```lisp
#+mallet
(declaim (mallet:disable :line-length))
(defun foo () ...)  ; Rules disabled
(defun bar () ...)  ; Rules disabled
#+mallet
(declaim (mallet:enable :line-length))
```

**Suppress in scope:**
```lisp
(defun foo ()
  #+mallet
  (declare (mallet:suppress :unused-variables))
  (let ((unused 1)) ...))
```

**Suppress all rules:**
```lisp
#+mallet
(declaim (mallet:suppress-next :all))
(defun generated-code () ...)
```

## Rules

See [RULES.md](RULES.md) for the complete list.

## License

MIT License - see [LICENSE](LICENSE)

## Bundled Libraries

Mallet bundles the following Common Lisp libraries for standalone distribution:

- **acclimation** - Internationalization/localization (BSD 2-Clause)
- **alexandria** - Utility library (Public Domain/MIT)
- **cl-ppcre** - Regular expressions (BSD 2-Clause)
- **closer-mop** - Meta-Object Protocol compatibility (MIT)
- **eclector** - Common Lisp reader (BSD 2-Clause)
- **trivial-glob** - Shell-style glob pattern (MIT)

See [THIRD-PARTY-LICENSES](THIRD-PARTY-LICENSES) for full license texts.
