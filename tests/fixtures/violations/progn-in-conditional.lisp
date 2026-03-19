;;; Test cases for progn-in-conditional rule

;;; Violations: bare progn in if then/else clauses
(if condition
    (progn
      (do-something)
      (do-another))
    (do-else))

(if condition
    (do-something)
    (progn
      (do-else-1)
      (do-else-2)))

;;; Violations: bare progn as last arg of and/or
(and x (progn (a) (b)))

(or x (progn (a) (b)))

;;; Coalton: progn in if inside coalton-toplevel is also flagged
(coalton-toplevel
  (declare foo (Boolean -> Integer))
  (define (foo x)
    (if x
        (progn
          (bar x)
          (baz x))
        (qux x))))
