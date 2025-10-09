;; Test fixtures for naming convention rules

;; Bad: special variable without earmuffs
(defvar my-config "default")

;; Good: special variable with earmuffs
(defvar *my-config* "default")

;; Bad: defparameter without earmuffs
(defparameter debug-mode t)

;; Good: defparameter with earmuffs
(defparameter *debug-mode* t)

;; Bad: constant without plus signs
(defconstant pi-value 3.14159)

;; Good: constant with plus signs
(defconstant +pi-value+ 3.14159)

;; Bad: define-constant without plus signs
(alexandria:define-constant max-size 1000)

;; Good: define-constant with plus signs
(alexandria:define-constant +max-size+ 1000)
