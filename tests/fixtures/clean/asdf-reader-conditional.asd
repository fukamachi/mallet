;; Clean fixture for asdf-reader-conditional rule
;; Uses :if-feature and (:feature ...) instead of #+/#-
;; Option-pair reader conditionals (at defsystem plist depth) are permitted.

(defsystem "asdf-reader-conditional"
  ;; Option-pair guards are allowed: no ASDF-native alternative exists for these.
  #+asdf3 :mailto
  #+asdf3 "dev@example.com"
  :depends-on ("alexandria"
               (:feature :unix "cl-cffi-gtk"))
  :around-compile (lambda (next)
                    #+sbcl (configure-sbcl)
                    (funcall next))
  :components ((:file "main")
               (:file "sbcl-support" :if-feature :sbcl)
               (:file "unix-support" :if-feature :unix)))
