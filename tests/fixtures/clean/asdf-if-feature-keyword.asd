;; Clean fixture for asdf-if-feature-keyword rule
;; All feature symbols are proper keywords

(defsystem "asdf-if-feature-keyword"
  :depends-on ("alexandria"
               (:feature :unix "cl-cffi-gtk"))
  :components ((:file "main"
                :if-feature :sbcl)
               (:file "other"
                :if-feature (:and :unix :linux))))
