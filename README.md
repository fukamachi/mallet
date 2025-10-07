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
 (:extends :recommended)  ; Options: :recommended, :minimal, :strict

 ;; Customize rules
 (:rules
  ;; Adjust line length limit
  (:line-length
   :enabled t
   :max-length 120)

  ;; Disable a rule
  (:comment-level
   :enabled nil)

  ;; Change severity
  (:unused-variables
   :severity :error)))
```

### Built-in Configurations

- **`:recommended`** (default) - All rules enabled with sensible defaults (80 character lines)
- **`:minimal`** - Only error-severity rules enabled
- **`:strict`** - All rules enabled with warnings promoted to errors
- **`:google`** - Google Common Lisp Style Guide (100 character lines)

## Rules

Malvolio includes rules for checking code style, best practices, and potential issues:

- **Text-Level Rules**: `line-length`
- **Token-Level Rules**: `comment-level`
- **Form-Level Rules**: `if-without-else`, `bare-progn-in-if`, `missing-otherwise`, `wrong-otherwise`, `unused-variables`

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
