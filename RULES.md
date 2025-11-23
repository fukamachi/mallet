# Rules

Rules are organized by severity level. See README.md for severity meanings.

**Suppressing violations**: Any rule can be suppressed using `#+mallet (declaim (mallet:suppress-next :rule-name))`. See the README for details.

## Table of Contents

- [ERROR](#error)
  - [`:wrong-otherwise`](#wrong-otherwise) - `ecase`/`etypecase` with `otherwise`/`t` clause
- [WARNING](#warning)
  - [`:unused-variables`](#unused-variables) - Variables that are never used
  - [`:unused-local-functions`](#unused-local-functions) - Local functions that are never called
  - [`:missing-otherwise`](#missing-otherwise) - `case`/`typecase` without `otherwise` clause
  - [`:mixed-optional-and-key`](#mixed-optional-and-key) - Mixing `&optional` and `&key` parameters
- [CONVENTION](#convention)
  - [`:if-without-else`](#if-without-else) - Use `when`/`unless` instead of `if` without else
  - [`:bare-progn-in-if`](#bare-progn-in-if) - Use `cond` instead of `if` with bare `progn`
  - [`:special-variable-naming`](#special-variable-naming) - Special variables should be named `*foo*`
  - [`:asdf-component-strings`](#asdf-component-strings) - ASDF components should use strings
- [FORMAT](#format)
  - [`:no-tabs`](#no-tabs) - Use spaces instead of tab characters
  - [`:trailing-whitespace`](#trailing-whitespace) - Lines should not have trailing whitespace
  - [`:final-newline`](#final-newline) - Files must end with a newline
- [INFO](#info)
  - [`:line-length`](#line-length) - Lines should not exceed maximum length
  - [`:consecutive-blank-lines`](#consecutive-blank-lines) - Limit consecutive blank lines
  - [`:unused-local-nicknames`](#unused-local-nicknames) - Local nicknames should be used
  - [`:unused-imported-symbols`](#unused-imported-symbols) - Imported symbols should be used or re-exported
  - [`:constant-naming`](#constant-naming) - Constants should be named `+foo+`
  - [`:unused-loop-variables`](#unused-loop-variables) - Loop variables should be used
  - [`:needless-let*`](#needless-let*) - Use `let` when bindings are independent
- [METRICS](#metrics)
  - [`:function-length`](#function-length) - Function exceeds maximum line count
  - [`:cyclomatic-complexity`](#cyclomatic-complexity) - Function has high cyclomatic complexity

## ERROR

### `:wrong-otherwise`

`ecase` and `etypecase` should not have `otherwise` or `t` clauses.

```lisp
;; Bad: defeats exhaustiveness checking
(ecase type
  (:a 1)
  (:b 2)
  (otherwise 0))

;; Good
(ecase type
  (:a 1)
  (:b 2))
```

**Default**: enabled

## WARNING

### `:unused-variables`

Variables should be used or explicitly ignored with `(declare (ignore ...))` or underscore prefix.

```lisp
;; Bad
(defun add (x y)
  (+ x 1))  ; y is unused

;; Good
(defun add (x _y)
  (+ x 1))

(let ((a 1) (b 2))
  (+ a b))  ; both used
```

**Default**: enabled

### `:unused-local-functions`

Local functions defined in `flet` or `labels` should be used.

```lisp
;; Bad
(flet ((helper (x) (* x 2))
       (unused (x) (+ x 1)))
  (helper 10))  ; unused is never called

;; Good
(flet ((helper (x) (* x 2)))
  (helper 10))
```

**Default**: enabled

### `:missing-otherwise`

`case` and `typecase` should have an `otherwise` clause (not `t`).

```lisp
;; Bad
(case type
  (:a 1)
  (:b 2))

;; Good
(case type
  (:a 1)
  (:b 2)
  (otherwise nil))
```

**Default**: disabled

### `:mixed-optional-and-key`

Don't mix `&optional` and `&key` in lambda lists.

```lisp
;; Bad: ambiguous parameter binding
(defun foo (x &optional y &key z)
  ...)

;; Good: use only one
(defun foo (x &key y z)
  ...)
```

**Default**: enabled

## CONVENTION

### `:if-without-else`

Use `when` or `unless` instead of `if` without else.

```lisp
;; Bad
(if condition
    (do-something))

;; Good
(when condition
  (do-something))
```

**Default**: enabled

### `:bare-progn-in-if`

Use `cond` instead of `if` with bare `progn`.

```lisp
;; Bad
(if condition
    (progn
      (do-one)
      (do-two)))

;; Good
(cond
  (condition
   (do-one)
   (do-two)))
```

**Default**: disabled

### `:special-variable-naming`

Special variables should be named `*foo*`.

```lisp
;; Bad
(defvar config nil)

;; Good
(defvar *config* nil)
```

**Default**: disabled

### `:asdf-component-strings`

ASDF systems, components, and dependencies should use strings not symbols.

Applies only to `.asd` files.

```lisp
;; Bad
(defsystem #:my-system
  :depends-on (#:alexandria))

;; Good
(defsystem "my-system"
  :depends-on ("alexandria"))
```

**Default**: enabled

## FORMAT

### `:no-tabs`

Use spaces instead of tab characters.

**Default**: enabled

### `:trailing-whitespace`

Lines should not have trailing whitespace.

**Default**: enabled

### `:final-newline`

Files must end with a newline.

**Default**: enabled

## INFO

### `:line-length`

Lines should not exceed maximum length.

**Options**: `:max` (default: 100)

```lisp
(:line-length :enabled t :max 120)
```

**Default**: disabled

### `:consecutive-blank-lines`

Limit consecutive blank lines.

**Options**: `:max` (default: 2)

```lisp
(:consecutive-blank-lines :enabled t :max 1)
```

**Default**: disabled

### `:unused-local-nicknames`

Local nicknames should be used in the package.

```lisp
;; Bad: unused nickname
(defpackage #:foo
  (:use #:cl)
  (:local-nicknames
   (#:unused #:alexandria)))  ; never used in this package

;; Good: all nicknames used
(defpackage #:foo
  (:use #:cl)
  (:local-nicknames
   (#:a #:alexandria)))

(in-package #:foo)
(a:flatten list)  ; uses the nickname
```

**Default**: enabled

### `:unused-imported-symbols`

Imported symbols should be used or re-exported.

```lisp
;; Bad: imported but never used
(defpackage #:foo
  (:use #:cl)
  (:import-from #:alexandria
                #:flatten
                #:hash-table-keys))  ; never used

;; Good: all imports used or exported
(defpackage #:foo
  (:use #:cl)
  (:import-from #:alexandria
                #:flatten)
  (:export #:flatten))  ; re-exported
```

**Default**: enabled

### `:constant-naming`

Constants should be named `+foo+`.

```lisp
;; Bad
(defconstant pi 3.14159)

;; Good
(defconstant +pi+ 3.14159)
```

**Default**: disabled

### `:unused-loop-variables`

Loop variables should be used within the loop body.

```lisp
;; Bad: loop variable not used
(loop for x in list
      for y in other-list  ; y is unused
      collect x)

;; Good: all loop variables used
(loop for x in list
      for y in other-list
      collect (cons x y))

;; Good: explicitly ignore with underscore
(loop for x in list
      for _y in other-list
      collect x)
```

**Default**: disabled

### `:needless-let*`

Use `let` instead of `let*` when bindings don't depend on each other (including single-binding `let*`).

```lisp
;; Bad: bindings are independent
(let* ((x (foo))
       (y (bar)))
  (list x y))

;; Good: use let for independent bindings
(let ((x (foo))
      (y (bar)))
  (list x y))
```

**Default**: disabled (`:info` severity; included in `:all` preset)

## METRICS

Code quality metrics that measure complexity and size. These are informational and don't indicate bugs or errors.

### `:function-length`

Functions should not exceed maximum line count.

**Options**: `:max` (default: 50)

```lisp
(:function-length :max 100)
```

```lisp
;; Bad: function exceeds 50 lines (default)
(defun very-long-function (data)
  "This function has too many lines."
  (line-1)
  (line-2)
  (line-3)
  ...
  (line-52))

;; Good: function within limit
(defun well-sized-function (data)
  "This function is appropriately sized."
  (process data))
```

**Counting**: Counts only code lines, excluding:
- Line comments (lines starting with `;`)
- Block comments (`#| ... |#` on separate lines)
- Blank lines (lines containing only whitespace)
- Docstrings
- Disabled reader conditionals (`#+nil`, `#+(or)`, `#-(and)`) and their guarded forms

Notes:
- Inline block comments (e.g., `(foo #| comment |# bar)`) are not excluded
- Platform-specific conditionals (e.g., `#+sbcl`, `#+ccl`) count as code
- Nested `flet`/`labels` functions are counted separately

**Default**: disabled

### `:cyclomatic-complexity`

Functions should not exceed maximum cyclomatic complexity.

**Options**:
- `:max` (default: 20) - Maximum allowed complexity
- `:variant` (default: `:standard`) - Calculation variant: `:standard` or `:modified`

```lisp
(:cyclomatic-complexity :max 20)
(:cyclomatic-complexity :max 15 :variant :modified)
```

```lisp
;; Bad: complexity of 19 (OK with default max of 20, shown for illustration)
(defun high-complexity (cmd)
  (cond
    ((string= cmd "start") (start-server))
    ((string= cmd "stop") (stop-server))
    ((string= cmd "restart") (restart-server))
    ((string= cmd "status") (show-status))
    ((string= cmd "config") (show-config))
    ((string= cmd "init") (initialize))
    ((string= cmd "destroy") (destroy))
    ((string= cmd "pause") (pause-server))
    ((string= cmd "resume") (resume-server))
    ((string= cmd "test") (run-tests))
    ((string= cmd "logs") (show-logs))
    ((string= cmd "backup") (backup-data))
    ((string= cmd "restore") (restore-data))
    ((string= cmd "upgrade") (upgrade-system))
    ((string= cmd "downgrade") (downgrade-system))
    ((string= cmd "monitor") (start-monitoring))
    ((string= cmd "alert") (send-alert))
    ((string= cmd "clean") (clean-cache))
    (t (error "Unknown command"))))
  ;; = 1 (base) + 18 (cond clauses, excluding final 't') = 19

;; Good: lower complexity
(defun simple-function (x)
  (if (< x 0)
      'negative
      'positive))
  ;; = 1 (base) + 1 (if) = 2
```

**Complexity Calculation** (`:standard` variant, default):
- **Base**: 1 per function
- **Conditionals**: +1 for each `if`, `when`, `unless`
- **COND**: +1 per clause (excluding final `t`/`otherwise` clause, like if-elseif-else)
- **CASE/TYPECASE**: +1 per clause (excluding final `otherwise`/`t` clause, like switch)
  - `ecase`, `etypecase`, `ccase`, `ctypecase`: all clauses count (no default)
- **Logical operators**: +1 for each `and` or `or` (regardless of argument count)
  - Example: `(and x (or y z))` = +2 (one for `and`, one for `or`)
- **Simple iteration**: +0 for `dotimes`, `dolist` (no conditional branching)
- **DO/DO***: +1 each (has end-test condition, like while loops)
- **LOOP**: +1 per conditional keyword only (`when`, `unless`, `if`, `while`, `until`)
  - Simple loops without conditionals add no complexity
- **Exception handling**: +1 per handler clause (`handler-case`, `restart-case`, etc.)
- **Third-party macros**:
  - **Alexandria**: `if-let`, `when-let`, `when-let*` = +1 each; `xor` = +1 (like `or`)
  - **Alexandria**: `destructuring-case`, `destructuring-ecase` = per clause (like `case`)
  - **Trivia**: `match`, `ematch`, `match*`, `multiple-value-match`, `multiple-value-ematch` = per clause (like `case`)
  - **Trivia**: `if-match`, `when-match`, `unless-match` = +1 each
  - **string-case**: `string-case` = per clause (like `case`)

**`:modified` variant**:
- Same as `:standard`, except:
- **CASE/TYPECASE**: +1 total (entire case statement counts as one decision point)
  - Example: `(case x (a 1) (b 2) (c 3) (otherwise 4))` = +1 (not +3)
- **Third-party case-like macros**: Also +1 total (`destructuring-case`, `match`, `string-case`, etc.)

Nested `flet`/`labels` functions are counted separately.

**Default**: disabled
