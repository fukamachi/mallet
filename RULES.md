# Malvolio Rules

This document describes all the rules available in Malvolio.

## Text-Level Rules

### `line-length`

Checks that lines do not exceed a maximum length.

- **Severity**: Warning
- **Options**:
  - `:max-length` - Maximum line length (default: 80)

```lisp
;; Bad: Line exceeds 80 characters
(defun very-long-function-name-that-exceeds-the-maximum-allowed-line-length (x y z) (+ x y z))

;; Good: Line within limit
(defun short-name (x y z)
  (+ x y z))
```

**Configuration**:
```lisp
(:line-length :enabled t :max-length 120)
```

## Token-Level Rules

### `comment-level`

Checks that comments use the appropriate number of semicolons based on context.

- **Severity**: Warning
- **Convention**:
  - 1 semicolon (`;`) - inline comments (after code)
  - 2 semicolons (`;;`) - comments inside function bodies
  - 3 semicolons (`;;;`) - top-level section comments
  - 4 semicolons (`;;;;`) - file-level comments

```lisp
;;;; File-level comment

;;; Section comment
(defun foo (x)
  ;; Comment inside function
  (+ x 1))  ; Inline comment

;; Bad: Wrong level
; Top-level comment with one semicolon
```

## Form-Level Rules

### `if-without-else`

Suggests using `when` or `unless` instead of `if` without an else clause.

- **Severity**: Warning

```lisp
;; Bad: if without else
(if condition
    (do-something))

;; Good: use when
(when condition
  (do-something))

;; Good: if with else
(if condition
    (do-something)
    (do-something-else))
```

### `bare-progn-in-if`

Suggests using `cond` instead of `if` with bare `progn`.

- **Severity**: Warning

```lisp
;; Bad: bare progn in if
(if condition
    (progn
      (do-something)
      (do-another-thing))
    (progn
      (do-else-thing)
      (do-another-else-thing)))

;; Good: use cond
(cond
  (condition
   (do-something)
   (do-another-thing))
  (t
   (do-else-thing)
   (do-another-else-thing)))
```

### `missing-otherwise`

Checks that `case` and `typecase` have an `otherwise` clause.

- **Severity**: Warning

```lisp
;; Bad: case without otherwise
(case type
  (:a 1)
  (:b 2))

;; Bad: using t instead of otherwise
(case type
  (:a 1)
  (:b 2)
  (t 0))

;; Good: has otherwise
(case type
  (:a 1)
  (:b 2)
  (otherwise 0))
```

### `wrong-otherwise`

Checks that `ecase` and `etypecase` do not have `otherwise` or `t` clauses.

- **Severity**: Error

```lisp
;; Bad: ecase with otherwise (defeats exhaustiveness checking)
(ecase type
  (:a 1)
  (:b 2)
  (otherwise 0))

;; Good: ecase without otherwise
(ecase type
  (:a 1)
  (:b 2))
```

### `unused-variables`

Detects unused variables in function parameters, let bindings, and loop variables.

- **Severity**: Warning
- **Respects**:
  - `(declare (ignore var))` declarations
  - Variables prefixed with `_`

```lisp
;; Bad: unused parameter
(defun add (x y)
  (+ x 1))  ; y is unused

;; Good: explicitly ignored
(defun add (x y)
  (declare (ignore y))
  (+ x 1))

;; Good: underscore convention
(defun add (x _y)
  (+ x 1))

;; Bad: unused let binding
(let ((a 1) (b 2))
  (+ a 10))  ; b is unused

;; Bad: unused loop variable
(loop for item in list
      for index from 0
      collect item)  ; index is unused
```
