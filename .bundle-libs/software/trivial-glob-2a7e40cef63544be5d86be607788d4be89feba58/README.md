# trivial-glob

[![CI](https://github.com/fukamachi/trivial-glob/actions/workflows/ci.yml/badge.svg)](https://github.com/fukamachi/trivial-glob/actions/workflows/ci.yml)

Shell-style glob pattern matching and filesystem globbing for Common Lisp.

**trivial-glob** brings full shell glob syntax to Common Lisp with recursive search (`**`), brace expansion (`{a,b}`), character classes, and pattern-based exclusions.

Use it when you need richer patterns than basic wildcards provided by UIOP.

## Quick Start

```lisp
(ql:quickload :trivial-glob)
(use-package :trivial-glob)

;; Find all Lisp files recursively
(glob "**/*.lisp")

;; Exclude test files
(glob "**/*.lisp" :exclude "*/tests/*.lisp")

;; Test if a string matches a pattern
(glob-match "*.txt" "readme.txt")  ; => T

;; Test if a path matches with special path-aware semantics
(glob-path-match "docs/" #P"/project/docs/api.md")  ; => T
```

## Usage

### Filesystem Globbing

```lisp
(use-package :trivial-glob)

;; Match all text files in current directory
(glob "*.txt")
;; => (#P"/path/to/file1.txt" #P"/path/to/file2.txt" ...)

;; Match all lisp files recursively
(glob "**/*.lisp")
;; => (#P"/path/to/src/main.lisp" #P"/path/to/tests/test.lisp" ...)

;; Brace expansion
(glob "{foo,bar}/*.c")
;; Expands to: foo/*.c and bar/*.c

;; Multiple patterns with braces
(glob "file{1,2,3}.{txt,log}")
;; => (#P"file1.txt" #P"file1.log" #P"file2.txt" ...)

;; Follow symbolic links
(glob "**/*.lisp" :follow-symlinks t)
```

### Excluding Files

```lisp
;; Exclude a single pattern
(glob "*.txt" :exclude "README.txt")

;; Exclude with wildcards
(glob "**/*.lisp" :exclude "*.fasl")

;; Exclude multiple patterns
(glob "**/*.lisp" :exclude '("*/tests/*.lisp"
                             "*/vendor/*.lisp"))

;; Patterns without / match any filename at any depth
(glob "**/*" :exclude "*.log")  ; Excludes all .log files everywhere

;; Patterns with / are automatically matched at any depth
(glob "**/*" :exclude "build/*.log")  ; Excludes build/*.log at any depth
(glob "**/*.lisp" :exclude "generated/*.lisp")  ; Excludes generated/*.lisp anywhere

;; Patterns with */ explicitly match at any directory depth
(glob "**/*.lisp" :exclude "*/generated/*.lisp")  ; Same as above

;; Directory exclusion with trailing / or /**
(glob "**/*" :exclude "build/")  ; Excludes all files in build/ recursively
(glob "**/*" :exclude "build/**")  ; Same as above (equivalent)
(glob "**/*" :exclude "**/build/**")  ; Same as above (explicit)
(glob "**/*.lisp" :exclude "vendor/")  ; Excludes all .lisp files in vendor/
(glob "**/*" :exclude '("build/" "dist/"))  ; Excludes multiple directories

;; Absolute paths match literally
(glob "**/*" :exclude "/tmp/specific/file.txt")  ; Only excludes this exact path
```

### Pattern Matching

```lisp
;; Test if a string matches a pattern
(glob-match "*.txt" "file.txt")  ; => T
(glob-match "test?.c" "test1.c") ; => T
(glob-match "[a-z]*" "hello")    ; => T

;; Case-insensitive matching
(glob-match "*.TXT" "file.txt" :casefold t)  ; => T

;; Pathname mode (/ not matched by wildcards)
(glob-match "*/*" "foo/bar" :pathname t)  ; => T
(glob-match "*" "foo/bar" :pathname t)    ; => NIL

;; Period flag (leading . must be matched explicitly)
(glob-match "*" ".hidden" :period t)      ; => NIL
(glob-match ".*" ".hidden" :period t)     ; => T
```

### Gitignore-Style Path Matching

The `glob-path-match` function implements `.gitignore`-style pattern matching semantics, making it perfect for configuration files, build tools, and any scenario where you need path-based rules:

```lisp
;; Patterns without '/' match any file/directory at any depth
(glob-path-match "*.log" #P"/var/log/app.log")      ; => T
(glob-path-match "build" #P"/src/build/output.txt") ; => T

;; Patterns with '/' are relative and match at any depth
(glob-path-match "src/*.c" #P"/project/src/main.c")     ; => T
(glob-path-match "*/test/*" #P"/app/lib/test/suite.lisp") ; => T

;; Trailing '/' matches directories and their contents recursively
(glob-path-match "vendor/" #P"/project/vendor/lib/foo.lisp") ; => T
(glob-path-match "build/" #P"/src/build/cache/data.tmp")     ; => T

;; Leading '/' makes patterns absolute
(glob-path-match "/tmp/*.log" #P"/tmp/debug.log")   ; => T
(glob-path-match "/tmp/*.log" #P"/var/tmp/debug.log") ; => NIL
```

## Pattern Syntax

### Wildcards

- `*` - Matches zero or more characters (excluding `/` in pathname mode)
- `?` - Matches exactly one character (excluding `/` in pathname mode)
- `**` - Matches zero or more directory levels (recursive)

### Bracket Expressions

- `[abc]` - Matches any character in the set
- `[a-z]` - Matches any character in the range
- `[!abc]` or `[^abc]` - Matches any character NOT in the set

### POSIX Character Classes

- `[:alnum:]` - Alphanumeric characters
- `[:alpha:]` - Alphabetic characters
- `[:digit:]` - Digits (0-9)
- `[:lower:]` - Lowercase letters
- `[:upper:]` - Uppercase letters
- `[:space:]` - Whitespace characters
- `[:blank:]` - Space and tab
- `[:punct:]` - Punctuation characters
- `[:print:]` - Printable characters
- `[:graph:]` - Graphic characters
- `[:cntrl:]` - Control characters
- `[:xdigit:]` - Hexadecimal digits

Example:

```lisp
(glob-match "[[:digit:]]*" "123abc")  ; => T
(glob-match "[[:alpha:]]+.txt" "file.txt")  ; => T
```

### Brace Expansion

```lisp
{foo,bar}    - Expands to: foo, bar
{1..3}       - NOT supported (use {1,2,3} instead)
```

### Escape Sequences

```lisp
\\*  - Matches literal *
\\?  - Matches literal ?
\\[  - Matches literal [
```

## API Reference

### Functions

#### `glob`

```lisp
(glob pathname-or-pattern &key follow-symlinks exclude) => list-of-pathnames
```

Return a list of pathnames matching the glob pattern.

**Arguments:**
- `pathname-or-pattern` - A pathname designator or glob pattern string
- `follow-symlinks` - If true, follow symbolic links during traversal (default: `nil`)
- `exclude` - Pattern or list of patterns to exclude from results. Patterns without `/` match against filename only. Patterns with `/` that don't start with `/` are automatically prefixed with `**/` to match at any directory depth. Patterns ending with `/` or `/**` are treated as directory exclusions and match all files within that directory recursively. Absolute paths (starting with `/`) match literally.

**Returns:** List of pathnames

**Errors:**
- Signals `FILE-ERROR` if the pattern contains wildcards and the base directory does not exist
- Returns `NIL` (no error) if the pattern is a literal path and the file does not exist

#### `glob-match`

```lisp
(glob-match pattern string &key pathname period casefold) => boolean
```

Test whether STRING matches the glob PATTERN.

**Arguments:**
- `pattern` - A glob pattern string
- `string` - The string to test
- `pathname` - If true, `/` characters are not matched by wildcards (default: `nil`)
- `period` - If true, leading `.` must be matched explicitly (default: `nil`)
- `casefold` - If true, perform case-insensitive matching (default: `nil`)

**Returns:** `T` if the string matches, `NIL` otherwise

#### `glob-path-match`

```lisp
(glob-path-match pattern pathname) => boolean
```

Test whether PATHNAME matches the PATTERN with gitignore-style semantics.

This function implements the same pattern matching rules as `.gitignore` files, making it ideal for configuration files, build tools, linters, and any scenario requiring intuitive path-based rules. Unlike basic glob matching, it understands the context of paths and directories.

**Arguments:**
- `pattern` - A pattern string with special path semantics:
  - Patterns with `/` match against the full pathname
  - Patterns without `/` match against just the filename
  - Relative paths are auto-prefixed with `**/` to match at any depth
  - Trailing `/` or `/**` matches directories and all files within recursively
- `pathname` - A pathname designator to test against the pattern

**Returns:** `T` if the pathname matches the pattern, `NIL` otherwise

### Variables

#### `*match-dotfiles*`

```lisp
(defparameter *match-dotfiles* nil)
```

When `NIL` (default), wildcards do not match leading dots in filenames.
When `T`, wildcards match leading dots like any other character.

## License

MIT License
