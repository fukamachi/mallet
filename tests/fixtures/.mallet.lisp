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
  (:disable :missing-docstring)))
