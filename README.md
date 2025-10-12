# Malo

[![CI](https://github.com/fukamachi/malo/actions/workflows/ci.yml/badge.svg)](https://github.com/fukamachi/malo/actions/workflows/ci.yml)

A Common Lisp linter focused on catching real mistakes without enforcing subjective style preferences.

## Philosophy

Works immediately. Grows with your needs.

Run Malo without configuration—it catches real mistakes, not style preferences.
Unused variables, wrong patterns, obvious bugs. Zero setup, zero noise.

Need more? Enable additional rules. Teams have different standards. Projects
have different constraints. Malo provides comprehensive rules like ESLint,
but lets you choose what matters.

Good defaults don't require configuration. Good tools don't limit customization.

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
