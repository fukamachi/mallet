# Rules

Rules are organized by severity level. See README.md for severity meanings.

**Suppressing violations**: Any rule can be suppressed using `#+mallet (declaim (mallet:suppress-next :rule-name))`. See the README for details.

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

**Options**: `:max-length` (default: 80)

```lisp
(:line-length :enabled t :max-length 100)
```

**Default**: disabled

### `:consecutive-blank-lines`

Limit consecutive blank lines.

**Options**: `:max-consecutive` (default: 2)

```lisp
(:consecutive-blank-lines :enabled t :max-consecutive 1)
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

### `:function-length`

Functions should not exceed maximum line count.

**Options**: `:max-lines` (default: 50)

```lisp
(:function-length :max-lines 100)
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

**Counting**: Lines are counted from the first line of the function definition to the last closing parenthesis. Nested `flet`/`labels` functions are counted separately.

**Default**: disabled

### `:cyclomatic-complexity`

Functions should not exceed maximum cyclomatic complexity.

**Options**: `:max-complexity` (default: 10)

```lisp
(:cyclomatic-complexity :max-complexity 15)
```

```lisp
;; Bad: complexity of 12 (too high with default max of 10)
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
    (t (error "Unknown command"))))

;; Good: lower complexity
(defun simple-function (x)
  (if (< x 0)
      'negative
      'positive))
```

**Complexity Calculation**: Uses modified cyclomatic complexity:
- Base complexity: 1 per function
- +1 for each conditional clause (`if`, `when`, `unless`, `cond` clause, `case` clause, etc.)
- +1 for each logical operator (`and`, `or`)
- +1 for each exception handler clause (`handler-case`, `handler-bind`, etc.)
- +1 for each loop construct (`loop`, `dotimes`, `dolist`, `do`, etc.)

Nested `flet`/`labels` functions are counted separately.

**Default**: disabled
