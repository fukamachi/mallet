;;; Test file for unused-variables rule

;; Unused function parameters
(defun add (x y)
  (+ x 1))

;; Unused let binding
(let ((a 1) (b 2))
  (+ a 10))

;; Unused loop variable
(loop for item in list
      for index from 0
      collect item)

;; Valid: all used
(defun multiply (x y)
  (* x y))

;; Valid: explicitly ignored
(defun process (data _options)
  (print data))

;; Valid: declare ignore
(defun compute (x y)
  (declare (ignore y))
  (+ x 1))
