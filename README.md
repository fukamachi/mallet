# Malo

A Common Lisp linter focused on catching real mistakes without enforcing subjective style preferences.

## Philosophy

Malo's default preset is deliberately minimal—it only enables rules that catch obvious mistakes or enforce conventions nearly everyone accepts. Why? Because linters that enforce subjective preferences create noise and friction. You shouldn't have to fight your linter over style choices.

The default catches things like unused variables (likely typos), trailing whitespace (universally discouraged), and unused imported symbols (dead dependencies). It skips subjective rules like line length limits, consecutive blank lines, or whether to use `when` vs `if`.

If your team wants stricter rules, enable them in config. The default stays quiet and helpful.

**Rule severity levels** describe the nature of violations, not whether they're in the default:

- **ERROR** - Objectively wrong (defeats language semantics)
- **WARNING** - Likely bugs or dangerous patterns
- **CONVENTION** - Idiom suggestions
- **FORMAT** - Consensus formatting standards
- **INFO** - Code quality suggestions

Any violation causes CI to fail. Teams control strictness by enabling/disabling rules.

## Installation

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

# Config
malo --config .malo.lisp src/

# Output
malo --format json src/
malo --format text src/  # default

# Debug
malo --debug src/
```

## Configuration

Create `.malo.lisp` in your project root:

```lisp
(:malo-config
 (:extends :default)  ; or :all
 (:rules
  (:line-length :enabled t :max-length 100)
  (:unused-variables :enabled t)
  (:if-without-else :enabled nil)))
```

**Presets:**

- `:default` - Only universally-accepted rules (catch mistakes, not enforce style)
- `:all` - All rules enabled (for exploration)

Malo auto-discovers `.malo.lisp` by walking up from the current directory.

## Rules

See [RULES.md](RULES.md) for the complete list.

**Summary:**

- Text rules: formatting (trailing whitespace, tabs, final newline, line length)
- Form rules: control flow, unused code, naming conventions, ASDF definitions

## License

MIT License - see [LICENSE](LICENSE)
