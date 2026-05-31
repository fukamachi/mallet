(defpackage #:mallet/tests/redundant-stub-guard
  (:use #:cl
        #:rove))
(in-package #:mallet/tests/redundant-stub-guard)

;;; These files already have functional tests that fail against constant-return
;;; stubs. Reintroducing the old stub-guard deftests weakens the suite by adding
;;; redundant smoke tests instead of relying on the functional assertions.

(defparameter *files-with-removed-stub-guards*
  '("rules/coalton-base-test.lisp"
    "rules/coalton-missing-to-boolean-test.lisp"
    "rules/coalton-cyclomatic-complexity-test.lisp"
    "engine-coalton-lisp-test.lisp"
    "engine-coalton-lisp-dispatch-test.lisp"))

(defparameter *forbidden-stub-guard-deftests*
  '(("rules/coalton-base-test.lisp"
     "coalton-define-p-stub-guard"
     "coalton-define-name-stub-guard"
     "coalton-define-body-stub-guard"
     "coalton-match-p-stub-guard"
     "coalton-match-clauses-stub-guard")
    ("rules/coalton-missing-to-boolean-test.lisp"
     "coalton-missing-to-boolean-stub-guard")
    ("rules/coalton-cyclomatic-complexity-test.lisp"
     "cyclomatic-complexity-coalton-stub-guard")
    ("engine-coalton-lisp-test.lisp"
     "extract-lisp-bodies-stub-guard-not-always-nil"
     "extract-lisp-bodies-stub-guard-not-always-list"
     "extract-lisp-bodies-stub-guard-respects-depth"
     "extract-lisp-bodies-stub-guard-skips-atoms"
     "extract-lisp-bodies-stub-guard-uses-posmap")
    ("engine-coalton-lisp-dispatch-test.lisp"
     "stub-guard-dispatch-is-not-no-op"
     "stub-guard-dispatch-uses-correct-rule"
     "stub-guard-no-false-positives-on-pure-coalton")))

(defparameter *required-functional-deftests*
  '(("rules/coalton-base-test.lisp"
     "coalton-define-p-function-define"
     "coalton-define-name-extracts-name"
     "coalton-define-body-extracts-body"
     "coalton-match-p-match-forms"
     "coalton-match-p-non-match-forms"
     "coalton-match-clauses-counts-non-wildcard")
    ("engine-coalton-lisp-test.lisp"
     "extract-lisp-bodies-simple-lisp-form"
     "extract-lisp-bodies-nested-in-define"
     "extract-lisp-bodies-multiple-cons-bodies-single-form")
    ("engine-coalton-lisp-dispatch-test.lisp"
     "cl-rule-fires-inside-lisp-body")))

(defun test-source-pathname (relative-path)
  (merge-pathnames relative-path
                   (merge-pathnames "tests/"
                                    (asdf:system-source-directory "mallet/tests"))))

(defun top-level-deftest-name-lines (relative-path)
  (let ((matches '()))
    (with-open-file (stream (test-source-pathname relative-path))
      (loop for line = (read-line stream nil nil)
            for line-number from 1
            while line
            for trimmed = (string-left-trim '(#\Space #\Tab) line)
            when (and (<= 8 (length trimmed))
                      (string= "(deftest" trimmed :end2 8))
              do (let* ((name-start (position-if-not (lambda (char)
                                                       (member char '(#\Space #\Tab)))
                                                     trimmed
                                                     :start 8))
                        (name-end (and name-start
                                       (or (position-if (lambda (char)
                                                          (member char '(#\Space #\Tab)))
                                                        trimmed
                                                        :start name-start)
                                           (length trimmed)))))
                   (when (and name-start name-end)
                     (push (list (subseq trimmed name-start name-end)
                                 line-number
                                 trimmed)
                           matches)))))
    (nreverse matches)))

(defun top-level-deftest-names (relative-path)
  (mapcar #'first (top-level-deftest-name-lines relative-path)))

(defun top-level-stub-guard-deftest-lines (relative-path)
  (loop for (name line-number line) in (top-level-deftest-name-lines relative-path)
        when (search "stub-guard" name :test #'char-equal)
          collect (format nil "~A:~D: ~A" relative-path line-number line)))

(defun present-forbidden-deftests ()
  (loop for (relative-path . forbidden-names) in *forbidden-stub-guard-deftests*
        for actual-names = (top-level-deftest-names relative-path)
        append (loop for forbidden-name in forbidden-names
                     when (member forbidden-name actual-names :test #'string=)
                       collect (format nil "~A: ~A" relative-path forbidden-name))))

(defun missing-required-deftests ()
  (loop for (relative-path . required-names) in *required-functional-deftests*
        for actual-names = (top-level-deftest-names relative-path)
        append (loop for required-name in required-names
                     unless (member required-name actual-names :test #'string=)
                       collect (format nil "~A: ~A" relative-path required-name))))

(deftest removed-stub-guard-deftests-stay-removed
  (testing "removed stub-guard deftests are absent from the affected test files"
    (let ((matches (loop for relative-path in *files-with-removed-stub-guards*
                         append (top-level-stub-guard-deftest-lines relative-path))))
      (ok (null matches)
          (format nil "Forbidden stub-guard deftests were reintroduced:~%~{~A~%~}"
                  matches))))

  (testing "the specific removed stub-guard deftests stay absent by name"
    (let ((matches (present-forbidden-deftests)))
      (ok (null matches)
          (format nil "Forbidden stub-guard deftests were reintroduced:~%~{~A~%~}"
                  matches))))

  (testing "functional deftests that replace the stub guards remain present"
    (let ((missing (missing-required-deftests)))
      (ok (null missing)
          (format nil "Required functional deftests were removed:~%~{~A~%~}"
                  missing)))))
