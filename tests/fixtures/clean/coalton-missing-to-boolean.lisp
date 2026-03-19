;;; Clean file for coalton-missing-to-boolean rule
;;; All (lisp Boolean ...) forms use to-boolean

(coalton-toplevel
  (declare is-null (:a -> Boolean))
  (define (is-null x)
    (lisp Boolean (x) (to-boolean (null x))))
  (declare is-zero (Integer -> Boolean))
  (define (is-zero x)
    (lisp Boolean (x) (to-boolean (zerop x))))
  ;; Non-Boolean lisp forms are fine without to-boolean
  (declare get-length (:a -> Integer))
  (define (get-length x)
    (lisp Integer (x) (length x))))
