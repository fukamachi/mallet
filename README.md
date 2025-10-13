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

```bash
# Lint files
mallet src/main.lisp
mallet src/*.lisp
mallet src  # recursively lints .lisp and .asd files

# Enable all rules
mallet --all src
```

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
 (:enable :line-length :max-length 100)
 (:enable :consecutive-blank-lines :max-lines 2)

 ;; Disable specific rules
 (:disable :constant-naming)

 ;; Path-specific overrides
 (:for-paths ("tests")
   (:enable :line-length :max-length 120)
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

## Suppressing Violations

Mallet supports suppressing specific violations using Common Lisp's native `declaim` syntax with `#+mallet` feature flags.

### Suppress Next Form

Suppress violations for the immediately following form:

```lisp
;; This function violates if-without-else, but it's suppressed
#+mallet
(declaim (mallet:suppress-next :if-without-else))
(defun conditional-print (x)
  (if x
      (print "yes")))  ; No else clause - but suppressed!
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
