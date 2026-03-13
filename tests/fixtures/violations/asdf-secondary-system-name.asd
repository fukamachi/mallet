;; Test fixture for asdf-secondary-system-name rule
;; Secondary systems in foo.asd must be named foo or foo/suffix

(defsystem "asdf-secondary-system-name"
  :depends-on ("alexandria"))

;; Bad: secondary system name doesn't follow primary/suffix convention
(defsystem "asdf-secondary-tests"
  :depends-on ("asdf-secondary-system-name"))

;; Bad: completely unrelated name
(defsystem "other-project"
  :depends-on ("alexandria"))
