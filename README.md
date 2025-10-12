# Malo

[![CI](https://github.com/fukamachi/malo/actions/workflows/ci.yml/badge.svg)](https://github.com/fukamachi/malo/actions/workflows/ci.yml)

A Common Lisp linter focused on catching real mistakes without enforcing subjective style preferences.

## Philosophy

Malo works out of the box. Its default rules detect only clear issues—definite mistakes and widely accepted style problems. This keeps the output useful and free from noise.

Projects differ, and so do their rules. Malo also provides a growing set of optional checks that you can enable as needed.

A simple start, with room to grow.

## Installation

```bash
git clone https://github.com/fukamachi/malo.git
cd malo
make
```

This creates a standalone executable `./malo`. You can run it directly or move it anywhere you want:

```bash
# Run directly from the build directory
./malo src

# Or move to a directory in your PATH
mv malo ~/.local/bin/malo  # or /usr/local/bin/malo, ~/bin/malo, etc.
```

## Usage

```bash
# Lint files
malo src/main.lisp
malo src/*.lisp
malo src  # recursively lints .lisp and .asd files

# Enable all rules
malo --all src
```

## Configuration

Create `.malo.lisp` in your project root:

```lisp
(:malo-config
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

Malo auto-discovers `.malo.lisp` by walking up from the current directory.

## Rules

See [RULES.md](RULES.md) for the complete list.

## License

MIT License - see [LICENSE](LICENSE)

## Bundled Libraries

Malo bundles the following Common Lisp libraries for standalone distribution:

- **acclimation** - Internationalization/localization (BSD 2-Clause)
- **alexandria** - Utility library (Public Domain/MIT)
- **cl-ppcre** - Regular expressions (BSD 2-Clause)
- **closer-mop** - Meta-Object Protocol compatibility (MIT)
- **eclector** - Common Lisp reader (BSD 2-Clause)
- **trivial-glob** - Shell-style glob pattern (MIT)

See [THIRD-PARTY-LICENSES](THIRD-PARTY-LICENSES) for full license texts.
