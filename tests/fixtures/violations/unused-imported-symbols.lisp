;; Test file for unused-imported-symbols rule

(defpackage #:test-unused-imports
  (:use #:cl)
  (:import-from #:alexandria
                #:hash-table-keys
                #:hash-table-values  ; Unused
                #:hash-table-count)  ; Unused
  (:export #:load-config))
(in-package #:test-unused-imports)

(defun load-config ()
  (hash-table-keys (make-hash-table)))
