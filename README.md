# Malo

A Common Lisp linter focused on catching real mistakes without enforcing subjective style preferences.

## Philosophy

The default preset enables only universally-accepted rules. It catches obvious mistakes—unused variables, trailing whitespace, unused imports—without enforcing subjective preferences.

This keeps the default useful without being noisy. You catch real mistakes without fighting over style preferences.

Want stricter rules? Enable them in config. Any violation fails CI.

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

## License

MIT License - see [LICENSE](LICENSE)
