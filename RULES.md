# Rules

Rules are organized by category. Each rule shows its severity and default preset status.

**Severity levels**: `error`, `warning`, `info`

**Suppressing violations**: Any rule can be suppressed using `#+mallet (declaim (mallet:suppress-next :rule-name))`. See the README for details.

## Rule List

### [Correctness](#correctness) — Objectively wrong code

| Rule | Description | Severity | Default | Options |
|------|-------------|----------|---------|---------|
| [`:wrong-otherwise`](#wrong-otherwise) | `ecase`/`etypecase` with `otherwise`/`t` clause | error | on | |
| [`:mixed-optional-and-key`](#mixed-optional-and-key) | Mixing `&optional` and `&key` parameters | error | on | |
| [`:asdf-if-feature-keyword`](#asdf-if-feature-keyword) | Feature expressions must use keywords in defsystem | warning | on | |

### [Suspicious](#suspicious) — Likely wrong or dangerous patterns

| Rule | Description | Severity | Default | Options |
|------|-------------|----------|---------|---------|
| [`:eval-usage`](#eval-usage) | Runtime use of `cl:eval` | warning | on | |
| [`:runtime-intern`](#runtime-intern) | Runtime use of symbol-interning functions | warning | off | |
| [`:runtime-unintern`](#runtime-unintern) | Runtime use of `cl:unintern` | warning | off | |

### [Practice](#practice) — Commonly accepted best practices

| Rule | Description | Severity | Default | Options |
|------|-------------|----------|---------|---------|
| [`:no-package-use`](#no-package-use) | Use of `:use` in `defpackage` | warning | on | |
| [`:ignore-errors-usage`](#ignore-errors-usage) | Use of `cl:ignore-errors` | warning | on | |
| [`:allow-other-keys`](#allow-other-keys) | Use of `&allow-other-keys` in lambda lists | warning | on | |
| [`:double-colon-access`](#double-colon-access) | Accessing internal symbols via `::` | warning | on | |
| [`:error-with-string-only`](#error-with-string-only) | Calling `error` with only a format string | warning | off | |
| [`:missing-exported-docstring`](#missing-exported-docstring) | Exported definitions missing docstrings | warning | on | |
| [`:asdf-operate-in-perform`](#asdf-operate-in-perform) | Calling `asdf:operate` inside `:perform` | warning | on | |
| [`:asdf-reader-conditional`](#asdf-reader-conditional) | `#+`/`#-` reader conditionals in defsystem | warning | off | |

### [Cleanliness](#cleanliness) — Dead code and unused definitions

| Rule | Description | Severity | Default | Fix | Options |
|------|-------------|----------|---------|-----|---------|
| [`:unused-variables`](#unused-variables) | Variables that are never used | warning | on | | |
| [`:unused-local-functions`](#unused-local-functions) | Local functions that are never called | warning | on | | |
| [`:unused-local-nicknames`](#unused-local-nicknames) | Local nicknames that are never used | warning | on | yes | |
| [`:unused-imported-symbols`](#unused-imported-symbols) | Imported symbols that are never used | warning | on | yes | |
| [`:unused-loop-variables`](#unused-loop-variables) | Loop variables that are never used | info | off | | |
| [`:stale-suppression`](#stale-suppression) | Suppression directive has no effect | warning | on | | |

### [Style](#style) — Idiomatic patterns and naming

| Rule | Description | Severity | Default | Options |
|------|-------------|----------|---------|---------|
| [`:if-without-else`](#if-without-else) | Use `when`/`unless` instead of `if` without else | warning | on | |
| [`:bare-progn`](#bare-progn) | Use `cond`/`when`/`unless` instead of bare `progn` | info | off | |
| [`:missing-otherwise`](#missing-otherwise) | `case`/`typecase` without `otherwise` clause | info | off | |
| [`:interned-package-symbol`](#interned-package-symbol) | Use uninterned symbols in package definitions | info | off | |
| [`:needless-let*`](#needless-let) | Use `let` when bindings are independent | warning | on | |
| [`:special-variable-naming`](#special-variable-naming) | Special variables should be named `*foo*` | info | off | |
| [`:constant-naming`](#constant-naming) | Constants should be named `+foo+` | info | off | |
| [`:asdf-component-strings`](#asdf-component-strings) | ASDF components should use strings | warning | on | |
| [`:asdf-redundant-package-prefix`](#asdf-redundant-package-prefix) | Redundant package prefixes in `.asd` files | info | on | |
| [`:asdf-secondary-system-name`](#asdf-secondary-system-name) | Secondary systems must use `primary/suffix` name | warning | on | |
| [`:bare-float-literal`](#bare-float-literal) | Float literals should have explicit type markers | info | off | |
| [`:missing-docstring`](#missing-docstring) | Top-level definitions missing docstrings | info | off | |
| [`:redundant-progn`](#redundant-progn) | `progn` with a single body form is redundant | info | on | |

### [Format](#format) — Whitespace and file formatting

| Rule | Description | Severity | Default | Fix | Options |
|------|-------------|----------|---------|-----|---------|
| [`:trailing-whitespace`](#trailing-whitespace) | Lines with trailing whitespace | info | on | yes | |
| [`:no-tabs`](#no-tabs) | Tab characters in source | info | on | | |
| [`:final-newline`](#final-newline) | Files must end with a newline | info | on | yes | |
| [`:closing-paren-on-own-line`](#closing-paren-on-own-line) | Closing parens on their own line | info | on | | |
| [`:line-length`](#line-length) | Lines exceeding maximum length | info | off | | `:max` (100) |
| [`:consecutive-blank-lines`](#consecutive-blank-lines) | Excessive consecutive blank lines | info | off | yes | `:max` (2) |

### [Metrics](#metrics) — Code quality measurements

| Rule | Description | Severity | Default | Options |
|------|-------------|----------|---------|---------|
| [`:function-length`](#function-length) | Function exceeds maximum line count | info | off | `:max` (50) |
| [`:cyclomatic-complexity`](#cyclomatic-complexity) | Function has high cyclomatic complexity | info | off | `:max` (20), `:variant` (standard) |
| [`:comment-ratio`](#comment-ratio) | Function has too many comments | info | off | `:max` (0.3), `:min-lines` (5), `:include-docstrings` (nil) |

## Correctness

Rules that catch objectively wrong code — constructs that defeat language semantics or cause runtime errors.

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

**Severity**: error | **Default**: enabled

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

**Severity**: error | **Default**: enabled

### `:asdf-if-feature-keyword`

Feature expressions in `:if-feature` and `(:feature ...)` dependency forms must use keywords. Plain symbols are evaluated as variables at read time and will likely cause errors. Applies only to `.asd` files. Handles compound feature expressions like `(:and ...)`, `(:or ...)`, and `(:not ...)`.

```lisp
;; Bad: plain symbols instead of keywords
(defsystem "my-system"
  :depends-on ((:feature unix "cl-cffi-gtk"))
  :components ((:file "main" :if-feature sbcl)
               (:file "other" :if-feature (:and unix linux))))

;; Good: keywords
(defsystem "my-system"
  :depends-on ((:feature :unix "cl-cffi-gtk"))
  :components ((:file "main" :if-feature :sbcl)
               (:file "other" :if-feature (:and :unix :linux))))
```

**Severity**: warning | **Default**: enabled

## Suspicious

Rules that detect likely wrong or dangerous patterns — code that probably indicates a bug or a security risk.

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

**Exclusions**:
- `defmacro` bodies are skipped (macro expansion code is not runtime)
- `eval-when` bodies are only checked when `:execute` is in the situation list

**Severity**: warning | **Default**: enabled

### `:runtime-intern`

Avoid using symbol-interning functions at runtime. Runtime interning causes symbol table side effects, makes code harder to reason about, and is often a sign that a macro or compile-time mechanism should be used instead.

Detected functions:
- `cl:intern`, `cl:unintern`
- `uiop:intern*`
- `alexandria:symbolicate`, `alexandria:format-symbol`, `alexandria:make-keyword`

```lisp
;; Bad
(intern name :my-package)
(alexandria:symbolicate prefix "-" suffix)

;; Good: use compile-time macros or static symbols
(defmacro def-accessor (name)
  `(defun ,(alexandria:symbolicate name '-get) () ...))
```

**Exclusions**:
- `defmacro` bodies are skipped (macro expansion code is not runtime)
- `eval-when` bodies are only checked when `:execute` is in the situation list

**Severity**: warning | **Default**: disabled

### `:runtime-unintern`

Avoid calling `cl:unintern` at runtime. `unintern` mutates the live package structure and can break symbol identity across packages. This is a focused companion to `:runtime-intern` that targets only `unintern`.

```lisp
;; Bad
(unintern sym :my-package)
(funcall #'unintern sym)

;; Good: redesign to avoid runtime package mutation
```

**Exclusions**:
- `defmacro` bodies are skipped (macro expansion code is not runtime)

**Severity**: warning | **Default**: disabled

## Practice

Rules that enforce commonly accepted best practices — not wrong, but widely discouraged patterns.

### `:no-package-use`

Avoid using `:use` in `cl:defpackage` or `uiop:define-package`. The `:use` option imports all exported symbols from the named package, which makes it hard to tell which symbols are actually used and risks symbol conflicts when the used package exports new symbols.

The packages `#:cl`, `#:common-lisp`, `#:coalton`, and `#:coalton-prelude` are exempt.

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

**Severity**: warning | **Default**: enabled

### `:ignore-errors-usage`

Avoid using `cl:ignore-errors` at runtime. `ignore-errors` silently swallows all errors and returns `nil`, making it very difficult to diagnose problems. Use `handler-case` with specific condition types instead.

```lisp
;; Bad: silently swallows all errors
(ignore-errors (parse-config path))

;; Good: handle specific conditions explicitly
(handler-case (parse-config path)
  (file-error (e) (format t "Cannot read config: ~A" e))
  (parse-error (e) (format t "Invalid config: ~A" e)))
```

**Exclusions**:
- `defmacro` bodies are skipped (macro expansion code is not runtime)

**Severity**: warning | **Default**: enabled

### `:allow-other-keys`

Avoid `&allow-other-keys` in lambda lists. It disables keyword argument checking, hiding typos and invalid arguments that would otherwise be caught at call sites.

```lisp
;; Bad: silently accepts any keyword
(defun connect (host &key port timeout &allow-other-keys)
  ...)

;; Good: explicit parameters
(defun connect (host &key port timeout)
  ...)
```

**Severity**: warning | **Default**: enabled

### `:double-colon-access`

Avoid accessing internal symbols via `::` package qualifier. Using `::` bypasses package encapsulation, coupling code to package internals and making it fragile against refactoring.

```lisp
;; Bad
(my-lib::internal-function arg)

;; Good: use only exported symbols
(my-lib:public-function arg)
```

**Severity**: warning | **Default**: enabled

### `:error-with-string-only`

Avoid calling `error` with only a format string. Signaling a string creates a `simple-error`, which callers cannot selectively handle with `handler-case`. Define and signal a proper condition type instead.

```lisp
;; Bad: callers can only catch simple-error
(error "connection failed: ~A" host)

;; Good: define a condition type
(define-condition connection-error (error)
  ((host :initarg :host :reader connection-error-host)))
(error 'connection-error :host host)
```

**Severity**: warning | **Default**: disabled

### `:missing-exported-docstring`

Exported definitions should have docstrings. Exported symbols form the public API of a package; without docstrings, users and tools like `describe` have no documentation to show. Applies to `defun`, `defmacro`, `defgeneric`, and `defclass` forms whose name appears in the package's `:export` list. `defmethod` is exempt because methods inherit documentation from the generic function.

```lisp
;; Bad: exported function with no docstring
(defpackage #:my-lib
  (:use #:cl)
  (:export #:connect))
(in-package #:my-lib)

(defun connect (host port)
  (open-connection host port))

;; Good: docstring added
(defun connect (host port)
  "Open a connection to HOST on PORT."
  (open-connection host port))
```

**Severity**: warning | **Default**: enabled

### `:asdf-operate-in-perform`

Do not call `asdf:operate` or related functions inside `:perform` method bodies. These calls can cause infinite loops or unexpected build behavior. Use `symbol-call` instead. Applies only to `.asd` files.

Detected functions: `operate`, `oos`, `load-system`, `test-system`, `clear-system`, `require-system`, `make`, `compile-system` (when qualified with `asdf:` or an ASDF sub-package prefix).

```lisp
;; Bad: direct asdf calls in :perform
(defsystem "my-system"
  :perform (test-op (o c)
    (asdf:load-system "my-system/tests")
    (asdf:test-system "my-system/tests")))

;; Good: use symbol-call
(defsystem "my-system"
  :perform (test-op (o c)
    (symbol-call :asdf :load-system "my-system/tests")
    (symbol-call :asdf :test-system "my-system/tests")))
```

**Severity**: warning | **Default**: enabled

### `:asdf-reader-conditional`

Avoid `#+`/`#-` reader conditionals inside `defsystem` bodies in `.asd` files. Reader conditionals are processed at read time and cannot be controlled by ASDF, making them less portable. Use ASDF's built-in `:if-feature` (component option) and `(:feature ...)` (dependency modifier) instead.

```lisp
;; Bad: reader conditionals in defsystem
(defsystem "my-system"
  :components ((:file "main")
               #+sbcl (:file "sbcl-support")
               #-windows (:file "unix-support")))

;; Good: use :if-feature
(defsystem "my-system"
  :components ((:file "main")
               (:file "sbcl-support" :if-feature :sbcl)
               (:file "unix-support" :if-feature (:not :windows))))
```

**Exclusions**:
- Reader conditionals inside `:perform` bodies are not flagged (they are legitimate runtime CL code)
- Reader conditionals outside `defsystem` forms are ignored
- Comments, string literals, and character literals are not scanned

**Severity**: warning | **Default**: disabled

## Cleanliness

Rules that detect dead code and unused definitions.

### `:unused-variables`

Variables should be used or explicitly ignored with `(declare (ignore ...))` or underscore prefix.

```lisp
;; Bad
(defun add (x y)
  (+ x 1))  ; y is unused

;; Good
(defun add (x _y)
  (+ x 1))
```

**Severity**: warning | **Default**: enabled

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

**Severity**: warning | **Default**: enabled

### `:unused-local-nicknames`

Local nicknames defined in `defpackage` should be used in the package.

```lisp
;; Bad: unused nickname
(defpackage #:foo
  (:use #:cl)
  (:local-nicknames
   (#:unused #:alexandria)))  ; never used

;; Good: all nicknames used
(defpackage #:foo
  (:use #:cl)
  (:local-nicknames
   (#:a #:alexandria)))
```

**Severity**: warning | **Default**: enabled

### `:unused-imported-symbols`

Imported symbols should be used or re-exported.

```lisp
;; Bad: imported but never used
(defpackage #:foo
  (:use #:cl)
  (:import-from #:alexandria
                #:flatten
                #:hash-table-keys))  ; never used

;; Good
(defpackage #:foo
  (:use #:cl)
  (:import-from #:alexandria #:flatten)
  (:export #:flatten))  ; re-exported
```

**Severity**: warning | **Default**: enabled

### `:unused-loop-variables`

Loop variables should be used within the loop body.

```lisp
;; Bad
(loop for x in list
      for y in other-list  ; y is unused
      collect x)

;; Good: explicitly ignore with underscore
(loop for x in list
      for _y in other-list
      collect x)
```

**Severity**: info | **Default**: disabled

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

**Severity**: warning | **Default**: enabled

## Style

Rules for idiomatic patterns and naming conventions.

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

**Severity**: warning | **Default**: enabled

### `:bare-progn`

Use `cond` instead of `if` with bare `progn`. Use `when` instead of `and` with bare `progn` as the last argument. Use `unless` instead of `or` with bare `progn` as the last argument.

```lisp
;; Bad: if with bare progn
(if condition
    (progn
      (do-one)
      (do-two)))

;; Good
(cond
  (condition
   (do-one)
   (do-two)))

;; Bad: and with bare progn as last argument
(and condition
     (progn
       (do-one)
       (do-two)))

;; Good
(when condition
  (do-one)
  (do-two))

;; Bad: or with bare progn as last argument
(or condition
    (progn
      (do-one)
      (do-two)))

;; Good
(unless condition
  (do-one)
  (do-two))
```

**Severity**: info | **Default**: disabled

### `:missing-otherwise`

`case` and `typecase` should have an `otherwise` clause.

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

**Severity**: info | **Default**: disabled

### `:interned-package-symbol`

Use uninterned symbols (`#:symbol`) in package definitions instead of keywords or bare symbols.

```lisp
;; Bad: keywords
(defpackage :myapp
  (:use :cl))

;; Good: uninterned symbols
(defpackage #:myapp
  (:use #:cl))
```

**Severity**: info | **Default**: disabled

### `:needless-let*`

Use `let` instead of `let*` when bindings don't depend on each other (including single-binding `let*`).

```lisp
;; Bad: bindings are independent
(let* ((x (foo))
       (y (bar)))
  (list x y))

;; Good
(let ((x (foo))
      (y (bar)))
  (list x y))
```

**Severity**: warning | **Default**: enabled

### `:special-variable-naming`

Special variables should be named `*foo*`. Applies to `defvar`, `defparameter`, and `sb-ext:defglobal`.

```lisp
;; Bad
(defvar config nil)
(sb-ext:defglobal global-state nil)

;; Good
(defvar *config* nil)
(sb-ext:defglobal *global-state* nil)
```

**Note**: Variables named with `+plus+` convention are also accepted (since `defglobal` is sometimes used for constants).

**Severity**: info | **Default**: disabled

### `:constant-naming`

Constants should be named `+foo+`.

```lisp
;; Bad
(defconstant pi 3.14159)

;; Good
(defconstant +pi+ 3.14159)
```

**Severity**: info | **Default**: disabled

### `:asdf-component-strings`

ASDF systems, components, and dependencies should use strings not symbols. Applies only to `.asd` files.

```lisp
;; Bad
(defsystem #:my-system
  :depends-on (#:alexandria))

;; Good
(defsystem "my-system"
  :depends-on ("alexandria"))
```

**Severity**: warning | **Default**: enabled

### `:asdf-redundant-package-prefix`

Package prefixes `asdf:`, `cl:`, `common-lisp:`, and `uiop:` are redundant in `.asd` files. These files run in the `asdf-user` package which already uses `asdf`, `cl`, and `uiop`, so qualifying symbols with those package names is unnecessary. Sub-package prefixes like `uiop/filesystem:` are also flagged.

```lisp
;; Bad: redundant package prefixes
(asdf:defsystem "my-system"
  :depends-on ("alexandria")
  :description (cl:format nil "tests"))

;; Good: no prefixes needed
(defsystem "my-system"
  :depends-on ("alexandria")
  :description (format nil "tests"))
```

**Severity**: info | **Default**: enabled

### `:asdf-secondary-system-name`

Secondary system names in a `.asd` file must follow the `primary/suffix` convention. In a file named `foo.asd`, systems other than `"foo"` must be named `"foo/something"`. Arbitrary names like `"foo-tests"` or `"bar"` are not allowed by ASDF and may cause issues with system resolution.

```lisp
;; Bad: in my-system.asd
(defsystem "my-system"
  :depends-on ("alexandria"))

(defsystem "my-system-tests"       ; should be "my-system/tests"
  :depends-on ("my-system"))

(defsystem "other-project"          ; unrelated name
  :depends-on ("alexandria"))

;; Good: follows primary/suffix convention
(defsystem "my-system"
  :depends-on ("alexandria"))

(defsystem "my-system/tests"
  :depends-on ("my-system"))
```

**Severity**: warning | **Default**: enabled

### `:bare-float-literal`

Float literals should have explicit type markers (`f`, `d`, `s`, `l`). Without a marker, the type depends on `*read-default-float-format*`, which can vary.

```lisp
;; Bad: depends on *read-default-float-format*
(defvar *threshold* 0.5)

;; Good: explicit double-float
(defvar *threshold* 0.5d0)
```

**Severity**: info | **Default**: disabled

### `:missing-docstring`

Top-level definitions should have docstrings. Applies to `defun`, `defmacro`, `defgeneric`, and `defclass` forms. `defmethod` is exempt because methods inherit documentation from the generic function.

```lisp
;; Bad: no docstring
(defun add (x y)
  (+ x y))

(defclass point ()
  ((x :initarg :x)
   (y :initarg :y)))

;; Good: docstrings present
(defun add (x y)
  "Return the sum of X and Y."
  (+ x y))

(defclass point ()
  ((x :initarg :x)
   (y :initarg :y))
  (:documentation "A two-dimensional point."))
```

**Severity**: info | **Default**: disabled

### `:redundant-progn`

`progn` with a single body form is redundant — the `progn` wrapper can be removed.

```lisp
;; Bad: progn with one form
(progn
  (do-something))

;; Good: no wrapper needed
(do-something)
```

**Exclusions**:
- `progn` wrapping a single `,@splice` (unquote-splicing) is allowed, since the splice may expand to multiple forms

**Severity**: info | **Default**: enabled

## Format

Rules for whitespace and file formatting. These follow strong community consensus (Emacs/SLIME standards).

### `:trailing-whitespace`

Lines should not have trailing whitespace.

**Auto-fixable**: `--fix` removes trailing whitespace.

**Severity**: info | **Default**: enabled

### `:no-tabs`

Use spaces instead of tab characters.

**Severity**: info | **Default**: enabled

### `:final-newline`

Files must end with a newline.

**Auto-fixable**: `--fix` appends a newline.

**Severity**: info | **Default**: enabled

### `:closing-paren-on-own-line`

Closing parentheses should follow the last expression on the same line, not appear alone on their own line. This is the idiomatic Common Lisp style, unlike Algol-family languages.

```lisp
;; Bad
(defun foo ()
  (bar)
  )

;; Good
(defun foo ()
  (bar))
```

**Exception**: Closing parens after a line ending with a comment are allowed, since appending them to the comment line would break the comment.

```lisp
;; OK: previous line ends with a comment
(defvar *alist*
  '((a . 1)
    (b . 2) ; last entry
    ))
```

**Severity**: info | **Default**: enabled

### `:line-length`

Lines should not exceed maximum length.

**Options**: `:max` (default: 100)

```lisp
(:enable :line-length :max 120)
```

**Severity**: info | **Default**: disabled

### `:consecutive-blank-lines`

Limit consecutive blank lines.

**Options**: `:max` (default: 2)

**Auto-fixable**: `--fix` removes excess blank lines.

```lisp
(:enable :consecutive-blank-lines :max 1)
```

**Severity**: info | **Default**: disabled

## Metrics

Code quality metrics that measure complexity and size. These are informational and don't indicate bugs or errors.

### `:function-length`

Functions should not exceed maximum line count.

**Options**: `:max` (default: 50)

**Counting**: Only code lines — excludes comments, blank lines, docstrings, and disabled reader conditionals (`#+nil`, `#+(or)`, `#-(and)`).

Nested `flet`/`labels` functions are counted separately.

**Severity**: info | **Default**: disabled

### `:cyclomatic-complexity`

Functions should not exceed maximum cyclomatic complexity.

**Options**:
- `:max` (default: 20) - Maximum allowed complexity
- `:variant` (default: `:standard`) - `:standard` or `:modified`

**Complexity calculation** (`:standard` variant):
- **Base**: 1 per function
- **Conditionals**: +1 for each `if`, `when`, `unless`
- **COND**: +1 per clause (excluding final `t`/`otherwise`)
- **CASE/TYPECASE**: +1 per clause (excluding final `otherwise`/`t`)
  - `ecase`, `etypecase`, `ccase`, `ctypecase`: all clauses count
- **Logical operators**: +1 for each `and` or `or`
- **DO/DO\***: +1 each (end-test condition)
- **LOOP**: +1 per conditional keyword (`when`, `unless`, `if`, `while`, `until`)
- **Exception handling**: +1 per handler clause
- **Third-party**: Alexandria (`if-let`, `when-let`, etc.), Trivia (`match`, etc.), `string-case`

**`:modified` variant**: Same, except CASE/TYPECASE and case-like macros count as +1 total instead of per-clause.

Nested `flet`/`labels` functions are counted separately.

**Severity**: info | **Default**: disabled

### `:comment-ratio`

Functions should not have too many comments relative to code. Useful for catching AI-generated code padded with excessive inline commentary.

**Options**:
- `:max` (default: 0.3) - Maximum comment ratio (0.0–1.0)
- `:min-lines` (default: 5) - Minimum qualifying lines before the rule applies
- `:include-docstrings` (default: `nil`) - Count docstring lines as comments

**Ratio formula**: `comment-lines / (comment-lines + code-lines)`

Nested `flet`/`labels` functions are counted separately.

Notes:
- Functions with fewer qualifying lines (comment + code) than `:min-lines` are skipped. Docstring lines are not counted toward the threshold unless `:include-docstrings t` is set.

**Severity**: info | **Default**: disabled

