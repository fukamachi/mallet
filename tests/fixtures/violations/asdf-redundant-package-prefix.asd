;; Test fixture for asdf-redundant-package-prefix rule
;; .asd files run in asdf-user which already uses asdf, cl, and uiop

;; Bad: redundant asdf: prefix on defsystem
(asdf:defsystem "asdf-redundant-package-prefix"
  :depends-on ("alexandria"))

;; Bad: redundant cl: prefix inside defsystem
(defsystem "asdf-redundant-package-prefix/tests"
  :depends-on ("alexandria")
  :description (cl:format nil "tests"))

;; Bad: redundant uiop: prefix inside defsystem
(defsystem "asdf-redundant-package-prefix/docs"
  :depends-on ("alexandria")
  :description (uiop:getenv "X"))
