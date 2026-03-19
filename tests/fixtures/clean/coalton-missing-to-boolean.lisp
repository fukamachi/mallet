;;; Clean file for coalton-missing-to-boolean rule
;;; All (lisp Boolean ...) forms use to-boolean

(coalton-toplevel
  (define (is-null x)
    (lisp Boolean (x) (to-boolean (null x))))
  (define (is-zero x)
    (lisp Boolean (x) (to-boolean (zerop x))))
  ;; Non-Boolean lisp forms are fine without to-boolean
  (define (get-length x)
    (lisp Integer (x) (length x))))
