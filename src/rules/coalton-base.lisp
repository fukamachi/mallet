(defpackage #:mallet/rules/coalton-base
  (:use #:cl)
  (:local-nicknames
   (#:base #:mallet/rules/base))
  (:export #:coalton-symbol-name
           #:coalton-define-p
           #:coalton-define-name
           #:coalton-define-body
           #:coalton-match-p
           #:coalton-match-clauses))
(in-package #:mallet/rules/coalton-base)

;;; Shared symbol name extractor (mirrors coalton.lisp helper)

(defun coalton-symbol-name (sym)
  "Extract the base symbol name from SYM (string or symbol), uppercase.
Returns NIL if SYM is not a recognisable symbol."
  (let ((raw (typecase sym
               (string (base:symbol-name-from-string sym))
               (symbol (symbol-name sym))
               (otherwise nil))))
    (when raw (string-upcase raw))))

;;; coalton-define-p

(defun coalton-define-p (expr)
  "Return T if EXPR is a Coalton function-style define: (define (name args...) body...).
Does NOT match value defines like (define x 42) — the second element must be a cons.
Does NOT match define-type or define-instance; only bare DEFINE is recognised."
  (and (consp expr)
       (let ((head-name (coalton-symbol-name (first expr))))
         (and head-name
              (string-equal head-name "DEFINE")
              (consp (second expr))))))

;;; coalton-define-name

(defun coalton-define-name (expr)
  "Extract the function name from a Coalton function define form.
EXPR must satisfy coalton-define-p.
Returns the name as an uppercase string, or NIL if not a function define."
  (when (coalton-define-p expr)
    (coalton-symbol-name (first (second expr)))))

;;; coalton-define-body

(defun coalton-define-body (expr)
  "Extract the body forms from a Coalton function define form.
EXPR must satisfy coalton-define-p.
Returns the list of body forms (everything after the lambda-list), or NIL."
  (when (coalton-define-p expr)
    (cddr expr)))

;;; coalton-match-p

(defun coalton-match-p (expr)
  "Return T if EXPR is a Coalton match form: (match expr clause...).
Requires at least two elements: the MATCH symbol and the expression being matched.
Matches only the MATCH symbol exactly; does not match MATCH-VECTOR or other variants."
  (and (consp expr)
       (consp (cdr expr)) ; must have at least one more element after 'match'
       (let ((head-name (coalton-symbol-name (first expr))))
         (and head-name
              (string-equal head-name "MATCH")))))

;;; coalton-match-clauses

(defun coalton-wildcard-pattern-p (pattern)
  "Return T if PATTERN is a wildcard (_) in Coalton match syntax."
  (let ((name (coalton-symbol-name pattern)))
    (and name (string= name "_"))))

(defun coalton-match-clauses (expr)
  "Count non-wildcard clauses in a Coalton match form.
EXPR must be a match form: (match expr clause...).
Each clause has the form (pattern body...).
Clauses whose pattern is _ (wildcard) are excluded from the count.
Returns 0 if EXPR is not a match form or has no non-wildcard clauses."
  (if (coalton-match-p expr)
      (let ((clauses (cddr expr))) ; skip 'match' and the matched expression
        (count-if (lambda (clause)
                    (and (consp clause)
                         (not (coalton-wildcard-pattern-p (first clause)))))
                  clauses))
      0))
