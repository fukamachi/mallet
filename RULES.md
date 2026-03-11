# Rules

Rules are organized by severity level. See README.md for severity meanings.

**Suppressing violations**: Any rule can be suppressed using `#+mallet (declaim (mallet:suppress-next :rule-name))`. See the README for details.

## Table of Contents

- [ERROR](#error)
  - [`:wrong-otherwise`](#wrong-otherwise) - `ecase`/`etypecase` with `otherwise`/`t` clause
  - [`:mixed-optional-and-key`](#mixed-optional-and-key) - Mixing `&optional` and `&key` parameters
- [WARNING](#warning)
  - [`:unused-variables`](#unused-variables) - Variables that are never used
  - [`:unused-local-functions`](#unused-local-functions) - Local functions that are never called
  - [`:missing-otherwise`](#missing-otherwise) - `case`/`typecase` without `otherwise` clause
  - [`:eval-usage`](#eval-usage) - Runtime use of `cl:eval`
  - [`:runtime-intern`](#runtime-intern) - Runtime use of symbol-interning functions
  - [`:ignore-errors-usage`](#ignore-errors-usage) - Runtime use of `cl:ignore-errors`
  - [`:no-package-use`](#no-package-use) - Use of `:use` in `defpackage` or `uiop:define-package`
  - [`:needless-let*`](#needless-let*) - Use `let` when bindings are independent
- [CONVENTION](#convention)
  - [`:if-without-else`](#if-without-else) - Use `when`/`unless` instead of `if` without else
  - [`:bare-progn-in-if`](#bare-progn-in-if) - Use `cond` instead of `if` with bare `progn`
  - [`:interned-package-symbol`](#interned-package-symbol) - Use uninterned symbols in package definitions
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
- [METRICS](#metrics)
  - [`:function-length`](#function-length) - Function exceeds maximum line count
  - [`:cyclomatic-complexity`](#cyclomatic-complexity) - Function has high cyclomatic complexity
  - [`:comment-ratio`](#comment-ratio) - Function has too many comments relative to code
- [CLEANLINESS](#cleanliness)
  - [`:stale-suppression`](#stale-suppression) - Suppression directive has no effect

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

### `:eval-usage`

Avoid using `cl:eval` at runtime. Runtime evaluation of arbitrary code is a common source of security vulnerabilities (code injection) and makes programs hard to reason about. This rule detects direct calls as well as indirect invocation via `funcall` and `apply`.

```lisp
;; Bad
(eval user-input)
(funcall #'eval expr)
(apply #'eval forms)

;; Good: use compile-time macros or explicit dispatch instead
(case action
  (:add (+ x y))
  (:sub (- x y)))
```

**Default**: enabled

### `:runtime-intern`

Avoid using symbol-interning functions at runtime. Runtime interning causes symbol table side effects, makes code harder to reason about, and is often a sign that a macro or compile-time mechanism should be used instead. This rule detects direct calls as well as indirect invocation via `funcall` and `apply`.

Detected functions:
- `cl:intern`, `cl:unintern`
- `uiop:intern*`
- `alexandria:symbolicate`, `alexandria:format-symbol`, `alexandria:make-keyword`

```lisp
;; Bad
(intern name :my-package)
(alexandria:symbolicate prefix "-" suffix)
(funcall #'alexandria:make-keyword name)
(apply #'cl:intern args)

;; Good: use compile-time macros or static symbols instead
(defmacro def-accessor (name)
  `(defun ,(alexandria:symbolicate name '-get) () ...))
```

**Exclusions**:
- `defmacro` bodies are skipped entirely (macro expansion code is not runtime)
- `eval-when` bodies are only checked when `:execute` is in the situation list (i.e., the body runs at load/execute time); `eval-when (:compile-toplevel)` bodies are skipped

**Default**: disabled (`:warning` severity; included in `:all` preset)

### `:ignore-errors-usage`

Avoid using `cl:ignore-errors` at runtime. `ignore-errors` silently swallows all errors and returns `nil`, making it very difficult to diagnose problems — the caller cannot tell what went wrong, or whether anything went wrong at all. Use `handler-case` with specific condition types to handle expected errors explicitly.

```lisp
;; Bad: silently swallows all errors
(ignore-errors (parse-config path))
(ignore-errors (connect-to-db host port))

;; Good: handle specific conditions explicitly
(handler-case (parse-config path)
  (file-error (e) (format t "Cannot read config: ~A" e))
  (parse-error (e) (format t "Invalid config: ~A" e)))
```

**Exclusions**:
- `defmacro` bodies are skipped entirely (macro expansion code is not runtime)

**Default**: enabled (`:warning` severity)

### `:no-package-use`

Avoid using `:use` in `cl:defpackage` or `uiop:define-package`. The `:use` option imports all exported symbols from the named package into the current package namespace, which makes it hard to tell which symbols are actually used and creates compatibility risks: if the used package later exports a new symbol that conflicts with one in your package, it will raise an error.

The packages `#:cl`, `#:common-lisp`, `#:coalton`, and `#:coalton-prelude` are exempt from this rule as they are conventional base packages.

```lisp
;; Bad
(defpackage #:my-package
  (:use #:cl #:alexandria)   ; imports all of alexandria's exports
  (:export #:my-function))

;; Good: import only what you need
(defpackage #:my-package
  (:use #:cl)
  (:import-from #:alexandria #:when-let #:if-let)
  (:export #:my-function))
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

### `:interned-package-symbol`

Use uninterned symbols (`#:symbol`) in package definitions instead of keywords (`:symbol`) or bare symbols (`symbol`).

```lisp
;; Bad: keywords
(defpackage :myapp
  (:use :cl)
  (:export :main))
(in-package :myapp)

;; Bad: bare symbols
(defpackage myapp
  (:use cl)
  (:export main))
(in-package myapp)

;; Good: uninterned symbols
(defpackage #:myapp
  (:use #:cl)
  (:export #:main))
(in-package #:myapp)

;; Good: string designators (also acceptable)
(defpackage "MYAPP"
  (:use "CL")
  (:export "MAIN"))
```

**Rationale**:
- Keywords (`:symbol`) unnecessarily intern symbols in the KEYWORD package
- Bare symbols (`symbol`) intern in the current package at read time, which can cause unexpected issues
- Uninterned symbols make package definitions self-contained and independent of load order

**Checked clauses**: Package name, `:use`, `:export`, `:shadow`, `:intern`, `:nicknames`, `:import-from`, `:shadowing-import-from`, `:local-nicknames`, and UIOP-specific clauses (`:mix`, `:reexport`, `:use-reexport`, `:unintern`, `:recycle`).

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

**Default**: enabled (`:warning` severity; included in `:default` preset)

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

### `:comment-ratio`

Functions should not have too many comments relative to code. Useful for catching AI-generated code patterns where functions are padded with excessive inline commentary.

**Category**: `:metrics`
**Default**: disabled (`:info` severity; included in `:all` preset)

**Options**:
- `:max` (default: 0.3) - Maximum allowed comment ratio (0.0–1.0)
- `:min-lines` (default: 5) - Minimum qualifying lines before the rule applies (avoids noise on tiny functions). Counts comment and code lines only; docstring lines are excluded from this count unless `:include-docstrings t` is set.
- `:include-docstrings` (default: `nil`) - Whether to count docstring lines as comments

```lisp
(:comment-ratio :max 0.3)
(:comment-ratio :max 0.2 :min-lines 5)
(:comment-ratio :max 0.3 :include-docstrings t)
```

```lisp
;; Bad: comment ratio of 0.71 (5 comment lines out of 7 non-blank lines)
(defun process (x)
  ;; Check the input
  ;; Validate x first
  ;; Make sure x is positive
  ;; Now do the computation
  ;; Return the result
  (abs x))

;; Good: comment ratio of 0.33 (1 comment line out of 3 non-blank lines)
(defun process (x)
  ;; Ensure positive result
  (abs x))
```

**Ratio formula**: `comment-lines / (comment-lines + code-lines)`

Where:
- **comment-lines**: line comments (`;`), block comment lines (`#| ... |#`), and optionally docstring lines when `:include-docstrings t`
- **code-lines**: all other non-blank, non-docstring lines (docstring lines are excluded by default; set `:include-docstrings t` to count them as comment-lines instead)
- **blank lines**: excluded from both numerator and denominator

Notes:
- Functions with fewer qualifying lines (comment + code) than `:min-lines` are skipped. Docstring lines are not counted toward the threshold unless `:include-docstrings t` is set.
- Nested `flet`/`labels` functions are counted separately

**Default**: disabled

## CLEANLINESS

Rules that detect housekeeping issues in suppression directives. These are opt-in and pair with the inline comment suppression feature.

### `:stale-suppression`

A suppression directive (inline comment or `#+mallet` form) has no effect because no matching violation was found at the suppressed location.

This rule fires when:
- A `; mallet:suppress rule-name` comment is present but the named rule produces no violation on that form.
- A `; mallet:disable` / `; mallet:enable` region contains no violations for the listed rules.

Enable this rule to catch outdated suppression comments left behind after code changes.

```lisp
;; Bad: no violation here, suppression is stale
(let ((x (foo))     ; mallet:suppress needless-let*
      (y (bar)))
  (list x y))

;; Good: violation exists, suppression is meaningful
(let* ((x (foo))    ; mallet:suppress needless-let*
       (y (bar)))
  (list x y))
```

**Category**: `:cleanliness`
**Severity**: `:warning`
**Default**: enabled (`:warning` severity; included in `:default` and `:all` presets)
