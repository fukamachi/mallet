;;; Mallet configuration for test fixtures
;;; Enable all rules so that violation tests can properly detect issues

(:mallet-config
 (:extends :all)
 ;; Each opt-in docstring rule is enabled only for its own fixture files.
 ;; first-match-wins: specific-file overrides must come before the broad disable below.
 (:for-paths ("violations/missing-docstring.lisp"
              "clean/missing-docstring.lisp")
  (:enable :missing-docstring))
 (:for-paths ("violations/missing-package-docstring.lisp"
              "clean/missing-package-docstring.lisp")
  (:enable :missing-package-docstring))
 (:for-paths ("violations/missing-variable-docstring.lisp"
              "clean/missing-variable-docstring.lisp")
  (:enable :missing-variable-docstring))
 (:for-paths ("violations/missing-struct-docstring.lisp"
              "clean/missing-struct-docstring.lisp")
  (:enable :missing-struct-docstring))
 ;; Disable all opt-in docstring rules for every other fixture.
 ;; All four disables are in one block so first-match-wins applies them together.
 (:for-paths ("violations/" "clean/")
  (:disable :missing-docstring)
  (:disable :missing-package-docstring)
  (:disable :missing-variable-docstring)
  (:disable :missing-struct-docstring)))
