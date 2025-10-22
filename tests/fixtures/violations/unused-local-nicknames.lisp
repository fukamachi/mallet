;; Test file for unused-local-nicknames rule

;; Case 1: One unused nickname among multiple
(defpackage #:test1/config
  (:use #:cl)
  (:local-nicknames
   (#:a #:alexandria)  ; Unused
   (#:glob #:trivial-glob))
  (:export #:load-config))
(in-package #:test1/config)

(defun load-config ()
  (glob:glob "*.conf"))

;; Case 2: Multiple unused nicknames
(defpackage #:test2/config
  (:use #:cl)
  (:local-nicknames
   (#:a #:alexandria)  ; Unused
   (#:glob #:trivial-glob)
   (#:unused #:some-package))  ; Unused
  (:export #:load-config))
(in-package #:test2/config)

(defun load-config ()
  (glob:glob "*.conf"))
