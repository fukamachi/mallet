;;; Test file for needless-let* rule

;; Single binding let*
(let* ((x 1))
  x)

;; Independent bindings - should use LET
(let* ((a (foo))
       (b (bar)))
  (list a b))

;; Valid: dependent bindings
(let* ((a 1)
       (b (+ a 1)))
  (list a b))

;; Outer scope only - still needless let*
(let ((outer 10))
  (let* ((x (+ outer 1))
         (y (+ outer 2)))
    (list x y)))
