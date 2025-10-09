# Malo

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

Requires [Qlot](https://github.com/fukamachi/qlot) to install dependencies.

```bash
git clone https://github.com/fukamachi/malo.git
cd malo
qlot install
ln -s $(pwd)/bin/malo /usr/local/bin/malo
```

## Usage

```bash
# Lint files
malo src/main.lisp
malo src/*.lisp
malo src/  # recursively lints .lisp and .asd files

# Enable all rules
malo --all src/
```

## Configuration

Create `.malo.lisp` in your project root:

```lisp
(:malo-config
 (:extends :default)  ; or :all
 (:rules
  (:line-length :enabled t :max-length 100)
  (:unused-imported-symbols :enabled nil)
  (:if-without-else :enabled t)))
```

**Presets:**

- `:default` - Only universally-accepted rules (catch mistakes, not enforce style)
- `:all` - All rules enabled (for exploration)

Malo auto-discovers `.malo.lisp` by walking up from the current directory.

## Rules

See [RULES.md](RULES.md) for the complete list.

## License

MIT License - see [LICENSE](LICENSE)
