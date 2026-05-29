(defpackage #:mallet/tests/public-api-docstrings
  (:use #:cl
        #:rove))
(in-package #:mallet/tests/public-api-docstrings)

;;; Gate: (documentation 'mallet:SYM 'function) returns a non-nil string for
;;; each of the four symbols a library consumer reaches for first.
;;; These tests fail until the symbols are bound as functions with docstrings
;;; in the mallet package.

(defun %violation-fixture (filename)
  (merge-pathnames (concatenate 'string "tests/fixtures/violations/" filename)
                   (asdf:system-source-directory :mallet)))

(deftest lint-file-has-docstring
  (testing "mallet:lint-file documentation is a non-nil string"
    (ok (stringp (documentation 'mallet:lint-file 'function))
        "mallet:lint-file must have a function docstring"))

  (testing "mallet:lint-file docstring describes violations as its return value"
    ;; Both words must appear in the first clause (before any semicolon) so they
    ;; co-occur in the return-value description. A docstring like
    ;; "Return results; violation metadata may be inspected separately."
    ;; contains both words but only in unrelated clauses — that must fail.
    (let* ((doc (string-downcase (or (documentation 'mallet:lint-file 'function) "")))
           (first-clause (let ((semi-pos (position #\; doc)))
                           (if semi-pos (subseq doc 0 semi-pos) doc))))
      (ok (and (search "return" first-clause)
               (search "violation" first-clause))
          "mallet:lint-file docstring must describe violations as the return value in the first clause")))

  (testing "mallet:lint-file actually returns violation objects for a file with known violations"
    (let* ((file (%violation-fixture "closing-paren-on-own-line.lisp"))
           (config (mallet:make-config
                    :rules (list (mallet:make-rule :closing-paren-on-own-line))))
           (violations (mallet:lint-file file :config config)))
      (ok (< 0 (length violations))
          "mallet:lint-file must return at least one violation for closing-paren-on-own-line.lisp")
      (ok (eq :closing-paren-on-own-line
              (mallet:violation-rule (first violations)))
          "returned violations must carry the :closing-paren-on-own-line rule name"))))

(deftest lint-files-has-docstring
  (testing "mallet:lint-files documentation is a non-nil string"
    (ok (stringp (documentation 'mallet:lint-files 'function))
        "mallet:lint-files must have a function docstring"))

  (testing "mallet:lint-files docstring describes violations as its return value"
    ;; Both words must appear in the first clause (before any semicolon) so they
    ;; co-occur in the return-value description. A docstring like
    ;; "Return results; violation metadata may be inspected separately."
    ;; contains both words but only in unrelated clauses — that must fail.
    (let* ((doc (string-downcase (or (documentation 'mallet:lint-files 'function) "")))
           (first-clause (let ((semi-pos (position #\; doc)))
                           (if semi-pos (subseq doc 0 semi-pos) doc))))
      (ok (and (search "return" first-clause)
               (search "violation" first-clause))
          "mallet:lint-files docstring must describe violations as the return value in the first clause")))

  (testing "mallet:lint-files actually returns an alist of (file . violations) for files with known violations"
    (let* ((file (%violation-fixture "closing-paren-on-own-line.lisp"))
           (config (mallet:make-config
                    :rules (list (mallet:make-rule :closing-paren-on-own-line))))
           (result (mallet:lint-files (list file) :config config)))
      (ok (consp result)
          "mallet:lint-files must return a non-empty alist")
      (ok (assoc file result :test #'equal)
          "mallet:lint-files result must contain an entry for the linted file")
      (ok (< 0 (length (cdr (assoc file result :test #'equal))))
          "mallet:lint-files alist entry must contain at least one violation"))))

(deftest make-config-has-docstring
  (testing "mallet:make-config documentation is a non-nil string"
    (ok (stringp (documentation 'mallet:make-config 'function))
        "mallet:make-config must have a function docstring"))

  (testing "mallet:make-config actually returns a non-nil config object"
    (ok (not (null (mallet:make-config)))
        "mallet:make-config with no arguments must return a non-nil config object")))

(deftest load-config-has-docstring
  (testing "mallet:load-config documentation is a non-nil string"
    (ok (stringp (documentation 'mallet:load-config 'function))
        "mallet:load-config must have a function docstring"))

  (testing "mallet:load-config actually returns a non-nil config object from a valid config file"
    (uiop:with-temporary-file (:stream out
                               :pathname path
                               :direction :output
                               :prefix "mallet-pubapi-test"
                               :type "lisp")
      (write-string "(:mallet-config (:enable :closing-paren-on-own-line))" out)
      (force-output out)
      (ok (not (null (mallet:load-config path)))
          "mallet:load-config must return a non-nil config object"))))
