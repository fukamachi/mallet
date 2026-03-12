(defpackage #:mallet/tests/rules/asdf-defsystem
  (:use #:cl
        #:rove)
  (:local-nicknames
   (#:rules #:mallet/rules)
   (#:parser #:mallet/parser)
   (#:violation #:mallet/violation)))
(in-package #:mallet/tests/rules/asdf-defsystem)

;;; Helpers

(defun check-form-rule (rule-class text &optional (filename "test.asd"))
  "Run RULE-CLASS on TEXT parsed from FILENAME."
  (let ((rule (make-instance rule-class))
        (file (uiop:parse-native-namestring filename)))
    (let ((forms (parser:parse-forms text file)))
      (when forms
        (rules:check-form rule (first forms) file)))))

;;; asdf-redundant-package-prefix tests

(deftest asdf-redundant-package-prefix-valid
  (testing "No package prefix — 0 violations"
    (ok (null (check-form-rule 'rules:asdf-redundant-package-prefix-rule
                               "(defsystem \"foo\" :depends-on (\"alexandria\"))"))))

  (testing "Non-redundant prefix — 0 violations"
    (ok (null (check-form-rule 'rules:asdf-redundant-package-prefix-rule
                               "(my-pkg:some-function)"))))

  (testing "Non-.asd file is ignored"
    (ok (null (check-form-rule 'rules:asdf-redundant-package-prefix-rule
                               "(asdf:defsystem \"foo\")"
                               "test.lisp")))))

(deftest asdf-redundant-package-prefix-invalid
  (testing "asdf:defsystem prefix — 1 violation"
    (let ((violations (check-form-rule 'rules:asdf-redundant-package-prefix-rule
                                       "(asdf:defsystem \"foo\")")))
      (ok (= (length violations) 1))
      (ok (eq (violation:violation-rule (first violations))
              :asdf-redundant-package-prefix))
      (ok (search "asdf:" (violation:violation-message (first violations))))))

  (testing "cl:defun prefix — 1 violation"
    (let ((violations (check-form-rule 'rules:asdf-redundant-package-prefix-rule
                                       "(cl:defun my-hook () nil)")))
      (ok (= (length violations) 1))
      (ok (search "cl:" (violation:violation-message (first violations))))))

  (testing "uiop:merge-pathnames prefix — 1 violation"
    (let ((violations (check-form-rule 'rules:asdf-redundant-package-prefix-rule
                                       "(eval (uiop:merge-pathnames \"x\" \"y\"))")))
      (ok (= (length violations) 1))
      (ok (search "uiop:" (violation:violation-message (first violations))))))

  (testing "common-lisp:defvar prefix — 1 violation"
    (let ((violations (check-form-rule 'rules:asdf-redundant-package-prefix-rule
                                       "(common-lisp:defvar *x* nil)")))
      (ok (= (length violations) 1))))

  (testing "uiop/filesystem sub-package — 1 violation"
    (let ((violations (check-form-rule 'rules:asdf-redundant-package-prefix-rule
                                       "(uiop/filesystem:file-exists-p \"foo\")")))
      (ok (= (length violations) 1))))

  (testing "Multiple redundant prefixes — correct count"
    (let ((violations (check-form-rule 'rules:asdf-redundant-package-prefix-rule
                                       "(asdf:defsystem \"foo\" :components ((:file (cl:string \"main\"))))")))
      (ok (= (length violations) 2))))

  (testing "asdf: prefix in argument position — 1 violation"
    (let ((violations (check-form-rule 'rules:asdf-redundant-package-prefix-rule
                                       "(some-func asdf:run-tests)")))
      (ok (= (length violations) 1)))))

;;; asdf-operate-in-perform tests

(deftest asdf-operate-in-perform-valid
  (testing "symbol-call in :perform — 0 violations"
    (ok (null (check-form-rule 'rules:asdf-operate-in-perform-rule
                               "(defsystem \"foo\"
  :perform (test-op (o c) (symbol-call :fiveam '#:run! :foo)))"))))

  (testing "asdf:load-system outside :perform — 0 violations"
    (ok (null (check-form-rule 'rules:asdf-operate-in-perform-rule
                               "(eval-when (:load-toplevel) (asdf:load-system \"bar\"))"))))

  (testing "Non-.asd file is ignored"
    (ok (null (check-form-rule 'rules:asdf-operate-in-perform-rule
                               "(defsystem \"foo\"
  :perform (test-op (o c) (asdf:load-system \"bar\")))"
                               "test.lisp"))))

  (testing "Non-defsystem form is ignored"
    (ok (null (check-form-rule 'rules:asdf-operate-in-perform-rule
                               "(defun foo () (asdf:load-system \"bar\"))")))))

(deftest asdf-operate-in-perform-invalid
  (testing "asdf:load-system in :perform — 1 violation"
    (let ((violations (check-form-rule 'rules:asdf-operate-in-perform-rule
                                       "(defsystem \"foo\"
  :perform (test-op (o c) (asdf:load-system \"bar\")))")))
      (ok (= (length violations) 1))
      (ok (eq (violation:violation-rule (first violations))
              :asdf-operate-in-perform))
      (ok (search "symbol-call" (violation:violation-message (first violations))))))

  (testing "asdf:test-system in :perform — 1 violation"
    (let ((violations (check-form-rule 'rules:asdf-operate-in-perform-rule
                                       "(defsystem \"foo\"
  :perform (test-op (o c) (asdf:test-system \"foo\")))")))
      (ok (= (length violations) 1))
      (ok (search "asdf:test-system" (violation:violation-message (first violations))))))

  (testing "asdf:operate in :perform — 1 violation"
    (let ((violations (check-form-rule 'rules:asdf-operate-in-perform-rule
                                       "(defsystem \"foo\"
  :perform (test-op (o c) (asdf:operate 'asdf:test-op \"foo\")))")))
      (ok (= (length violations) 1))))

  (testing "nested asdf:load-system inside :perform body — detected"
    (let ((violations (check-form-rule 'rules:asdf-operate-in-perform-rule
                                       "(defsystem \"foo\"
  :perform (test-op (o c)
             (let ((x 1))
               (when (> x 0)
                 (asdf:load-system \"bar\")))))")))
      (ok (= (length violations) 1))))

  (testing "asdf:load-system in :perform of nested component — detected"
    (let ((violations (check-form-rule 'rules:asdf-operate-in-perform-rule
                                       "(defsystem \"foo\"
  :components
  ((:module \"mod\"
    :components ((:file \"main\"))
    :perform (test-op (o c) (asdf:load-system \"bar\")))))")))
      (ok (= (length violations) 1)))))

;;; asdf-secondary-system-name tests

(deftest asdf-secondary-system-name-valid
  (testing "Primary system matches filename — 0 violations"
    (ok (null (check-form-rule 'rules:asdf-secondary-system-name-rule
                               "(defsystem \"foo\")"
                               "foo.asd"))))

  (testing "Proper secondary name foo/tests — 0 violations"
    (ok (null (check-form-rule 'rules:asdf-secondary-system-name-rule
                               "(defsystem \"foo/tests\")"
                               "foo.asd"))))

  (testing "Proper secondary name foo/utils — 0 violations"
    (ok (null (check-form-rule 'rules:asdf-secondary-system-name-rule
                               "(defsystem \"foo/utils\")"
                               "foo.asd"))))

  (testing "Case-insensitive match of primary — 0 violations"
    (ok (null (check-form-rule 'rules:asdf-secondary-system-name-rule
                               "(defsystem \"FOO\")"
                               "foo.asd"))))

  (testing "Non-.asd file is ignored"
    (ok (null (check-form-rule 'rules:asdf-secondary-system-name-rule
                               "(defsystem \"bar\")"
                               "foo.lisp")))))

(deftest asdf-secondary-system-name-invalid
  (testing "foo-tests in foo.asd — 1 violation"
    (let ((violations (check-form-rule 'rules:asdf-secondary-system-name-rule
                                       "(defsystem \"foo-tests\")"
                                       "foo.asd")))
      (ok (= (length violations) 1))
      (ok (eq (violation:violation-rule (first violations))
              :asdf-secondary-system-name))
      (ok (search "foo-tests" (violation:violation-message (first violations))))))

  (testing "bar in foo.asd — 1 violation"
    (let ((violations (check-form-rule 'rules:asdf-secondary-system-name-rule
                                       "(defsystem \"bar\")"
                                       "foo.asd")))
      (ok (= (length violations) 1))
      (ok (search "bar" (violation:violation-message (first violations))))))

  (testing "foo/tests does not match bar/tests in bar.asd — 1 violation"
    (let ((violations (check-form-rule 'rules:asdf-secondary-system-name-rule
                                       "(defsystem \"foo/tests\")"
                                       "bar.asd")))
      (ok (= (length violations) 1))))

  (testing "Violation message mentions expected convention"
    (let ((violations (check-form-rule 'rules:asdf-secondary-system-name-rule
                                       "(defsystem \"foo-tests\")"
                                       "foo.asd")))
      (ok (search "foo/suffix" (violation:violation-message (first violations)))))))

;;; asdf-if-feature-keyword tests

(deftest asdf-if-feature-keyword-valid
  (testing "(:feature :sbcl dep) uses keyword — 0 violations"
    (ok (null (check-form-rule 'rules:asdf-if-feature-keyword-rule
                               "(defsystem \"foo\" :depends-on ((:feature :sbcl \"dep\")))"))))

  (testing ":if-feature :sbcl uses keyword — 0 violations"
    (ok (null (check-form-rule 'rules:asdf-if-feature-keyword-rule
                               "(defsystem \"foo\" :components ((:file \"main\" :if-feature :sbcl)))"))))

  (testing ":if-feature (:and :sbcl :unix) compound with keywords — 0 violations"
    (ok (null (check-form-rule 'rules:asdf-if-feature-keyword-rule
                               "(defsystem \"foo\" :components ((:file \"main\" :if-feature (:and :sbcl :unix))))"))))

  (testing ":if-feature (:not :ccl) with keyword — 0 violations"
    (ok (null (check-form-rule 'rules:asdf-if-feature-keyword-rule
                               "(defsystem \"foo\" :components ((:file \"main\" :if-feature (:not :ccl))))"))))

  (testing "Non-.asd file is ignored"
    (ok (null (check-form-rule 'rules:asdf-if-feature-keyword-rule
                               "(defsystem \"foo\" :depends-on ((:feature sbcl \"dep\")))"
                               "test.lisp")))))

(deftest asdf-if-feature-keyword-invalid
  (testing "(:feature sbcl dep) plain symbol — 1 violation"
    (let ((violations (check-form-rule 'rules:asdf-if-feature-keyword-rule
                                       "(defsystem \"foo\" :depends-on ((:feature sbcl \"dep\")))")))
      (ok (= (length violations) 1))
      (ok (eq (violation:violation-rule (first violations))
              :asdf-if-feature-keyword))
      (ok (search "sbcl" (violation:violation-message (first violations))))))

  (testing ":if-feature sbcl plain symbol — 1 violation"
    (let ((violations (check-form-rule 'rules:asdf-if-feature-keyword-rule
                                       "(defsystem \"foo\" :components ((:file \"main\" :if-feature sbcl)))")))
      (ok (= (length violations) 1))
      (ok (search ":sbcl" (violation:violation-message (first violations))))))

  (testing ":if-feature (:and sbcl :unix) — plain symbol in compound — 1 violation"
    (let ((violations (check-form-rule 'rules:asdf-if-feature-keyword-rule
                                       "(defsystem \"foo\" :components ((:file \"main\" :if-feature (:and sbcl :unix))))")))
      (ok (= (length violations) 1))
      (ok (search "sbcl" (violation:violation-message (first violations))))))

  (testing ":if-feature (:or ccl sbcl) — two plain symbols — 2 violations"
    (let ((violations (check-form-rule 'rules:asdf-if-feature-keyword-rule
                                       "(defsystem \"foo\" :components ((:file \"main\" :if-feature (:or ccl sbcl))))")))
      (ok (= (length violations) 2))))

  (testing "plain symbol in nested component :if-feature — detected"
    (let ((violations (check-form-rule 'rules:asdf-if-feature-keyword-rule
                                       "(defsystem \"foo\"
  :components
  ((:module \"mod\"
    :components ((:file \"main\" :if-feature sbcl)))))")))
      (ok (= (length violations) 1)))))
