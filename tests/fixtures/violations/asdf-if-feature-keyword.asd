;; Test fixture for asdf-if-feature-keyword rule
;; Feature symbols in :if-feature and (:feature ...) must be keywords

(defsystem "asdf-if-feature-keyword"
  :depends-on ("alexandria"
               (:feature unix "cl-cffi-gtk"))
  :components ((:file "main"
                :if-feature sbcl)
               (:file "other"
                :if-feature (:and unix linux))))
