;; Test fixtures for &allow-other-keys rule

;; Bad: defun with &allow-other-keys
(defun process-data (input &key format &allow-other-keys)
  (list input format))

;; Good: defun with only &key (no allow-other-keys)
(defun process-data-good (input &key format validate)
  (list input format validate))

;; Bad: lambda with &allow-other-keys
(lambda (x &key y &allow-other-keys)
  (list x y))

;; Good: lambda with only &key
(lambda (x &key y)
  (list x y))

;; Bad: defmethod with &allow-other-keys
(defmethod compute ((obj string) &key precision &allow-other-keys)
  (list obj precision))

;; Good: defmethod with only &key
(defmethod compute-good ((obj string) &key precision units)
  (list obj precision units))

;; Bad: defmacro with &allow-other-keys
(defmacro with-context (name &key default &allow-other-keys)
  `(let ((,name ,default))
     ,@body))

;; Good: defmacro with only &key
(defmacro with-context-good (name &key default scope)
  `(let ((,name (or ,default (get-default ,scope))))
     ,@body))

;; Bad: flet with &allow-other-keys
(flet ((helper (a &key b &allow-other-keys)
         (list a b)))
  (helper 1))

;; Good: flet with only &key
(flet ((helper-good (a &key b c)
         (list a b c)))
  (helper-good 1))
