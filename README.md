# Malvolio

> A relentless guardian of code integrity for Common Lisp

Malvolio is a sophisticated linter for Common Lisp, inspired by ESLint, that provides fast, file-based analysis with comprehensive rule coverage and customizable configuration.

## Features

- **Fast, File-Based Analysis** - Analyzes source code without loading or compiling systems
- **Customizable Configuration** - S-expression based config with inheritance and rule-specific options
- **Multiple Output Formats** - Human-readable text and machine-readable JSON

## Installation

```bash
# Clone the repository
git clone https://github.com/fukamachi/malvolio.git
cd malvolio

# Install dependencies
qlot install

# Make the CLI executable available
chmod +x bin/malvolio
ln -s $(pwd)/bin/malvolio /usr/local/bin/malvolio

# Run on your code
malvolio src/
```

## Usage

### Command Line

```bash
# Lint a single file
malvolio src/main.lisp

# Lint multiple files
malvolio src/*.lisp

# Lint a directory recursively
malvolio src/

# Use JSON output format
malvolio --format json src/

# Use a custom config file
malvolio --config .malvolio.lisp src/

# Use a built-in preset
malvolio --preset all src/        # Enable all rules
malvolio --preset google src/     # Google style guide

# Show help
malvolio --help

# Show version
malvolio --version
```

### Exit Codes

- `0` - No violations found
- `1` - Warnings found
- `2` - Errors found
- `3` - Fatal error (config error, file not found, etc.)

### Library Usage

```lisp
(ql:quickload :malvolio)

;; Lint a single file
(malvolio:lint-file #p"src/main.lisp")

;; Lint multiple files
(malvolio:lint-files (list #p"src/main.lisp" #p"src/utils.lisp"))

;; Use a custom configuration
(let ((config (malvolio:load-config ".malvolio.lisp")))
  (malvolio:lint-file #p"src/main.lisp" :config config))

;; Create a custom registry
(let ((registry (malvolio:make-registry)))
  (malvolio:register-rule registry :line-length
                          :description "Lines should not exceed 100 characters"
                          :severity :warning
                          :enabled t)
  (malvolio:lint-file #p"src/main.lisp" :registry registry))
```

## Configuration

Malvolio uses S-expression based configuration files (`.malvolio.lisp`). Place this file in your project root, and Malvolio will automatically discover it.

### Example Configuration

```lisp
(:malvolio-config
 ;; Extend from a built-in config
 (:extends :default)  ; Options: :default, :all, :google

 ;; Customize rules
 (:rules
  ;; Enable and configure a disabled rule
  (:line-length
   :enabled t
   :max-length 120)

  ;; Disable a rule
  (:trailing-whitespace
   :enabled nil)

  ;; Change severity
  (:unused-variables
   :severity :error)))
```

### Built-in Configurations

Malvolio provides three built-in presets. Use them via `--preset` on the command line or `:extends` in configuration files:

- **`:default`** - Only universally-accepted rules (trailing whitespace, tabs, unused variables). Quiet output, suitable for all projects.
- **`:all`** - All rules enabled. Useful for exploring what the linter can catch.
- **`:google`** - Google Common Lisp Style Guide compliance (100 character lines, all style rules)

## Rules

Malvolio includes **11 rules** for checking code style, best practices, and potential issues:

- **Text-Level Rules (5)**: `line-length`, `trailing-whitespace`, `no-tabs`, `final-newline`, `consecutive-blank-lines`
- **Token-Level Rules (1)**: `comment-level`
- **Form-Level Rules (5)**: `if-without-else`, `bare-progn-in-if`, `missing-otherwise`, `wrong-otherwise`, `unused-variables`

For detailed documentation of each rule with examples, see [RULES.md](./RULES.md).

## Output Formats

### Text Format (Default)

Human-readable format with color-coded severity:

```
src/main.lisp:
  src/main.lisp:45:0 WARNING: Line exceeds maximum length of 80 (95 characters)
  src/main.lisp:67:0 WARNING: Use 'when' or 'unless' instead of 'if' without else clause
  src/main.lisp:89:0 ERROR: 'ecase' should not have 'otherwise' or 't' clause

✗ Found 3 violations in 1 file.
```

### JSON Format

Machine-readable format for tool integration:

```bash
malvolio --format json src/main.lisp
```

```json
[
  {
    "file": "src/main.lisp",
    "violations": [
      {
        "rule": "line-length",
        "severity": "warning",
        "line": 45,
        "column": 0,
        "message": "Line exceeds maximum length of 80 (95 characters)"
      },
      {
        "rule": "if-without-else",
        "severity": "warning",
        "line": 67,
        "column": 0,
        "message": "Use 'when' or 'unless' instead of 'if' without else clause"
      }
    ]
  }
]
```

## License

MIT License - see [LICENSE](./LICENSE) file for details
