;; Clean fixture for asdf-reader-conditional rule
;; Uses :if-feature and (:feature ...) instead of #+/#-

(defsystem "asdf-reader-conditional"
  :depends-on ("alexandria"
               (:feature :unix "cl-cffi-gtk"))
  :around-compile (lambda (next)
                    #+sbcl (configure-sbcl)
                    (funcall next))
  :components ((:file "main")
               (:file "sbcl-support" :if-feature :sbcl)
               (:file "unix-support" :if-feature :unix)))
