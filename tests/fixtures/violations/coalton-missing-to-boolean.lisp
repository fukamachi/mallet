(coalton-toplevel
  (declare is-null (:a -> Boolean))
  (define (is-null x)
    (lisp Boolean (x) (null x)))
  (declare is-zero (Integer -> Boolean))
  (define (is-zero x)
    (lisp Boolean (x) (zerop x))))
