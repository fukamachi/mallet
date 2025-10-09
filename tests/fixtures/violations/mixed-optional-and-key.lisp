;; Test fixtures for mixed &optional and &key rule

;; Bad: defun mixing &optional and &key
(defun process-data (input &optional format &key validate)
  (list input format validate))

;; Good: defun with only &key
(defun process-data-good (input &key format validate)
  (list input format validate))

;; Bad: lambda mixing &optional and &key
(lambda (x &optional y &key z)
  (list x y z))

;; Good: lambda with only &optional
(lambda (x &optional y)
  (list x y))

;; Bad: defmethod mixing &optional and &key
(defmethod compute ((obj string) &optional precision &key units)
  (list obj precision units))

;; Good: defmethod with only &key
(defmethod compute-good ((obj string) &key precision units)
  (list obj precision units))

;; Bad: defmacro mixing &optional and &key
(defmacro with-context (name &optional default &key scope)
  `(let ((,name (or ,default (get-default ,scope))))
     ,@body))

;; Good: defmacro with only &key
(defmacro with-context-good (name &key default scope)
  `(let ((,name (or ,default (get-default ,scope))))
     ,@body))

;; Bad: flet mixing &optional and &key
(flet ((helper (a &optional b &key c)
         (list a b c)))
  (helper 1))

;; Good: flet with only &key
(flet ((helper-good (a &key b c)
         (list a b c)))
  (helper-good 1))
