(defpackage #:mallet/tests/public-api-docstrings
  (:use #:cl
        #:rove))
(in-package #:mallet/tests/public-api-docstrings)

;;; Behavioral coverage of the public API entry points a library consumer
;;; reaches for first: lint-file, lint-files, make-config, load-config.

(defun %violation-fixture (filename)
  (merge-pathnames (concatenate 'string "tests/fixtures/violations/" filename)
                   (asdf:system-source-directory :mallet)))

(deftest lint-file-returns-violations
  (testing "mallet:lint-file actually returns violation objects for a file with known violations"
    (let ((file (%violation-fixture "closing-paren-on-own-line.lisp"))
          (config (mallet:make-config
                   :rules (list (mallet:make-rule :closing-paren-on-own-line)))))
      (let ((violations (mallet:lint-file file :config config)))
        (ok (< 0 (length violations))
            "mallet:lint-file must return at least one violation for closing-paren-on-own-line.lisp")
        (ok (eq :closing-paren-on-own-line
                (mallet:violation-rule (first violations)))
            "returned violations must carry the :closing-paren-on-own-line rule name")))))

(deftest lint-files-returns-violations
  (testing "mallet:lint-files actually returns an alist of (file . violations) for files with known violations"
    (let ((file (%violation-fixture "closing-paren-on-own-line.lisp"))
          (config (mallet:make-config
                   :rules (list (mallet:make-rule :closing-paren-on-own-line)))))
      (let ((result (mallet:lint-files (list file) :config config)))
        (ok (consp result)
            "mallet:lint-files must return a non-empty alist")
        (ok (assoc file result :test #'equal)
            "mallet:lint-files result must contain an entry for the linted file")
        (ok (< 0 (length (cdr (assoc file result :test #'equal))))
            "mallet:lint-files alist entry must contain at least one violation")))))

(deftest make-config-returns-config
  (testing "mallet:make-config actually returns a non-nil config object"
    (ok (not (null (mallet:make-config)))
        "mallet:make-config with no arguments must return a non-nil config object")))

(deftest load-config-returns-config
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
