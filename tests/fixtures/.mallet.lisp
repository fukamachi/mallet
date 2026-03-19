;;; Mallet configuration for test fixtures
;;; Enable all rules so that violation tests can properly detect issues

(:mallet-config
 (:extends :all)
 ;; missing-docstring is an opt-in :all rule (severity :info).
 ;; Specific override first (first-match wins): keep it enabled for its own fixture.
 ;; Broader override: disable it for all other fixtures to avoid noise.
 (:for-paths ("violations/missing-docstring.lisp"
              "clean/missing-docstring.lisp")
  (:enable :missing-docstring))
 (:for-paths ("violations/" "clean/")
  (:disable :missing-docstring))
 ;; missing-package-docstring is opt-in; enable only for its own fixture files
 (:for-paths ("violations/missing-package-docstring.lisp"
              "clean/missing-package-docstring.lisp")
  (:enable :missing-package-docstring))
 (:for-paths ("violations/" "clean/")
  (:disable :missing-package-docstring))
 ;; missing-variable-docstring is opt-in; enable only for its own fixture files
 (:for-paths ("violations/missing-variable-docstring.lisp"
              "clean/missing-variable-docstring.lisp")
  (:enable :missing-variable-docstring))
 (:for-paths ("violations/" "clean/")
  (:disable :missing-variable-docstring))
 ;; missing-struct-docstring is opt-in; enable only for its own fixture files
 (:for-paths ("violations/missing-struct-docstring.lisp"
              "clean/missing-struct-docstring.lisp")
  (:enable :missing-struct-docstring))
 (:for-paths ("violations/" "clean/")
  (:disable :missing-struct-docstring)))
