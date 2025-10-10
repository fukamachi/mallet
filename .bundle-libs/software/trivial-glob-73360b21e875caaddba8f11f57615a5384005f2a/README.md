# trivial-glob

[![CI](https://github.com/fukamachi/trivial-glob/actions/workflows/ci.yml/badge.svg)](https://github.com/fukamachi/trivial-glob/actions/workflows/ci.yml)

Shell-style glob pattern matching and filesystem globbing for Common Lisp.

## Usage

### Basic Glob Matching

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
(glob pathname-or-pattern &key follow-symlinks) => list-of-pathnames
```

Return a list of pathnames matching the glob pattern.

**Arguments:**
- `pathname-or-pattern` - A pathname designator or glob pattern string
- `follow-symlinks` - If true, follow symbolic links during traversal (default: `nil`)

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

### Variables

#### `*match-dotfiles*`

```lisp
(defparameter *match-dotfiles* nil)
```

When `NIL` (default), wildcards do not match leading dots in filenames.
When `T`, wildcards match leading dots like any other character.

## License

MIT License
