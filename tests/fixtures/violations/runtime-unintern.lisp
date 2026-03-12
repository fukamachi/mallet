;;; Test file for runtime-unintern rule

;; Bad: direct cl:unintern
(defun remove-symbol (sym pkg)
  (cl:unintern sym pkg))

;; Bad: funcall with #'cl:unintern
(defun remove-via-funcall (sym pkg)
  (funcall #'cl:unintern sym pkg))

;; Bad: unqualified unintern
(defun remove-unqualified (sym)
  (unintern sym))

;; Good: no unintern usage
(defun add (x y)
  (+ x y))

;; Good: defmacro is not flagged (compile-time)
(defmacro with-unintern (sym &body body)
  `(progn (cl:unintern ,sym) ,@body))
