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

;; Keyword :let* in ecase clause is not a let* form
(ecase op
  (:let* (do-something))
  (:other (do-other)))

;; Keyword :let* in case clause is not a let* form
(case op
  (:let* (do-something))
  (otherwise nil))
