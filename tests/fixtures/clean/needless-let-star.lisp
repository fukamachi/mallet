;;; Clean examples for needless-let* rule

;; Sequential dependency
(let* ((x 1)
       (y (+ x 1)))
  (list x y))

;; Self-shadowing counts as dependency
(let* ((value 10)
       (value (+ value 5)))
  value)

;; Empty bindings are fine
(let* ()
  (do-something))
