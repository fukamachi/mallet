(defpackage #:mallet/tests/test-quality-regression
  (:use #:cl
        #:rove))
(in-package #:mallet/tests/test-quality-regression)

(defun test-source-pathname (relative-path)
  (merge-pathnames relative-path
                   (merge-pathnames "tests/"
                                    (asdf:system-source-directory "mallet/tests"))))

(defun file-string (relative-path)
  (with-open-file (stream (test-source-pathname relative-path)
                          :direction :input)
    (let ((string (make-string (file-length stream))))
      (read-sequence string stream)
      string)))

(defun project-file-string (relative-path)
  (with-open-file (stream (merge-pathnames relative-path
                                           (asdf:system-source-directory "mallet/tests"))
                          :direction :input)
    (let ((string (make-string (file-length stream))))
      (read-sequence string stream)
      string)))

(defun source-package (relative-path)
  (or (cdr (assoc relative-path
                  '(("fixer-test.lisp" . "MALLET/TESTS/FIXER")
                    ("rules/unused-variables-test.lisp" . "MALLET/TESTS/RULES/UNUSED-VARIABLES")
                    ("rules/rule-type-system-test.lisp" . "MALLET/TESTS/RULES/RULE-TYPE-SYSTEM")
                    ("rules/missing-docstring-test.lisp" . "MALLET/TESTS/RULES/MISSING-DOCSTRING")
                    ("rules/missing-struct-docstring-test.lisp" . "MALLET/TESTS/RULES/MISSING-STRUCT-DOCSTRING")
                    ("rules/missing-variable-docstring-test.lisp" . "MALLET/TESTS/RULES/MISSING-VARIABLE-DOCSTRING")
                    ("rules/missing-package-docstring-test.lisp" . "MALLET/TESTS/RULES/MISSING-PACKAGE-DOCSTRING"))
                  :test #'string=))
      "MALLET/TESTS/TEST-QUALITY-REGRESSION"))

(defun source-top-level-forms (relative-path)
  (let ((source (file-string relative-path))
        (forms '())
        (position 0))
    (let ((*package* (or (find-package (source-package relative-path))
                         *package*))
          (*read-eval* nil))
      (loop
        (multiple-value-bind (form next-position)
            (read-from-string source nil :eof :start position)
          (when (eq form :eof)
            (return (nreverse forms)))
          (push (list form (subseq source position next-position)) forms)
          (setf position next-position))))))

(defun top-level-form-name (form)
  (when (and (consp form)
             (symbolp (first form))
             (second form)
             (symbolp (second form)))
    (symbol-name (second form))))

(defun top-level-source-matching (relative-path head-symbol name)
  (loop for (form source) in (source-top-level-forms relative-path)
        when (and (consp form)
                  (eq head-symbol (first form))
                  (string= name (top-level-form-name form)))
          collect source))

(defun deftest-sources (relative-path name)
  (top-level-source-matching relative-path 'deftest name))

(defun defmacro-sources (relative-path name)
  (top-level-source-matching relative-path 'defmacro name))

(defun defun-sources (relative-path name)
  (top-level-source-matching relative-path 'defun name))

(defun all-deftest-names (relative-path)
  (loop for entry in (source-top-level-forms relative-path)
        for form = (first entry)
        when (and (consp form)
                  (eq 'deftest (first form)))
          collect (top-level-form-name form)))

(defun missing-markers (source markers)
  (loop for marker in markers
        unless (search marker source :test #'char-equal)
          collect marker))

(defun symbol-named-p (value name)
  (and (symbolp value)
       (string= name (symbol-name value))))

(defun form-contains-symbol-named-p (form name)
  (cond
    ((symbol-named-p form name) t)
    ((consp form)
     (or (form-contains-symbol-named-p (car form) name)
         (form-contains-symbol-named-p (cdr form) name)))
    (t nil)))

(defun form-contains-call-named-p (form name)
  (cond
    ((and (consp form)
          (symbol-named-p (first form) name))
     t)
    ((consp form)
     (or (form-contains-call-named-p (car form) name)
         (form-contains-call-named-p (cdr form) name)))
    (t nil)))

(defun symbol-in-package-named-p (value symbol-name package-name)
  (and (symbol-named-p value symbol-name)
       (symbol-package value)
       (string= package-name (package-name (symbol-package value)))))

(defun form-contains-call-in-package-named-p (form symbol-name package-name)
  (cond
    ((and (consp form)
          (symbol-in-package-named-p (first form) symbol-name package-name))
     t)
    ((consp form)
     (or (form-contains-call-in-package-named-p (car form) symbol-name package-name)
         (form-contains-call-in-package-named-p (cdr form) symbol-name package-name)))
    (t nil)))

(defun form-contains-format-to-t-p (form)
  (cond
    ((and (consp form)
          (symbol-named-p (first form) "FORMAT")
          (eq t (second form)))
     t)
    ((consp form)
     (or (form-contains-format-to-t-p (car form))
         (form-contains-format-to-t-p (cdr form))))
    (t nil)))

(defun deftest-form (relative-path name)
  (loop for (form) in (source-top-level-forms relative-path)
        when (and (consp form)
                  (eq 'deftest (first form))
                  (string= name (top-level-form-name form)))
          do (return form)))

(defun testing-form-named (deftest-form name)
  (labels ((walk (form)
             (cond
               ((and (consp form)
                     (eq 'testing (first form))
                     (string= name (second form)))
                form)
               ((consp form)
                (or (walk (car form))
                    (walk (cdr form))))
               (t nil))))
    (walk deftest-form)))

(defun testing-form-substantive-p (testing-form)
  (and testing-form
       (or (form-contains-call-named-p testing-form "WITH-TEMP-LINT-FILE")
           (and (form-contains-call-named-p testing-form "PARSE-FORMS")
                (form-contains-call-named-p testing-form "CHECK-FORM")))
       (form-contains-symbol-named-p testing-form "VIOLATIONS")
       (form-contains-call-named-p testing-form "OK")
       (or (form-contains-call-named-p testing-form "NULL")
           (form-contains-call-named-p testing-form "LENGTH")
           (form-contains-call-named-p testing-form "SOME")
           (form-contains-call-named-p testing-form "VIOLATION-MESSAGE"))))

(defun missing-substantive-testing-blocks (deftest-form block-names)
  (loop for name in block-names
        for testing-form = (testing-form-named deftest-form name)
        unless (testing-form-substantive-p testing-form)
          collect name))

(deftest unused-variable-suppression-tests-remain-singular-and-substantive
  (testing "unused-variables-suppression is defined once and keeps the broad scenarios"
    (let ((sources (deftest-sources "rules/unused-variables-test.lisp"
                                    "UNUSED-VARIABLES-SUPPRESSION")))
      (ok (= 1 (length sources))
          (format nil "Expected one unused-variables-suppression deftest, found ~D"
                  (length sources)))
      (when (= 1 (length sources))
        (let ((missing (missing-markers
                        (first sources)
                        '("LABELS"
                          "MULTIPLE-VALUE-BIND"
                          "DOLIST"
                          "Partial suppression"))))
          (ok (null missing)
              (format nil "unused-variables-suppression lost scenarios: ~{~A~^, ~}"
                      missing)))
        (let ((weak-blocks (missing-substantive-testing-blocks
                            (deftest-form "rules/unused-variables-test.lisp"
                                          "UNUSED-VARIABLES-SUPPRESSION")
                            '("Suppression in deeply nested LABELS with LET"
                              "Suppression with MULTIPLE-VALUE-BIND and DESTRUCTURING-BIND"
                              "Suppression with DOLIST"
                              "Partial suppression - mix of suppressed and non-suppressed"))))
          (ok (null weak-blocks)
              (format nil "unused-variables-suppression has marker-only/trivial scenarios: ~{~A~^, ~}"
                      weak-blocks))))))

  (testing "unused-loop-variables-suppression is defined once and keeps the broad scenarios"
    (let ((sources (deftest-sources "rules/unused-variables-test.lisp"
                                    "UNUSED-LOOP-VARIABLES-SUPPRESSION")))
      (ok (= 1 (length sources))
          (format nil "Expected one unused-loop-variables-suppression deftest, found ~D"
                  (length sources)))
      (when (= 1 (length sources))
        (let ((missing (missing-markers
                        (first sources)
                        '("Mid-level suppression in nested LOOPs"
                          "LOOP destructuring"
                          "LOOP WITH"
                          "Nested suppression in deeply nested LOOPs"
                          "LOOP inside LET"))))
          (ok (null missing)
              (format nil "unused-loop-variables-suppression lost scenarios: ~{~A~^, ~}"
                      missing)))
        (let ((weak-blocks (missing-substantive-testing-blocks
                            (deftest-form "rules/unused-variables-test.lisp"
                                          "UNUSED-LOOP-VARIABLES-SUPPRESSION")
                            '("Mid-level suppression in nested LOOPs"
                              "Suppression with LOOP destructuring"
                              "Suppression with LOOP WITH variable"
                              "Nested suppression in deeply nested LOOPs"
                              "LOOP inside LET with suppression"))))
          (ok (null weak-blocks)
              (format nil "unused-loop-variables-suppression has marker-only/trivial scenarios: ~{~A~^, ~}"
                      weak-blocks)))))))

(deftest unused-variable-test-helper-remains-singular
  (testing "with-temp-lint-file has one definition"
    (let ((sources (defmacro-sources "rules/unused-variables-test.lisp"
                                     "WITH-TEMP-LINT-FILE")))
      (ok (= 1 (length sources))
          (format nil "Expected one with-temp-lint-file definition, found ~D"
                  (length sources))))))

(deftest apply-fixes-ordering-test-exercises-public-api
  (testing "apply-fixes-ordering-test is absent or calls apply-fixes without in-test sorting"
    (let ((sources (deftest-sources "fixer-test.lisp"
                                    "APPLY-FIXES-ORDERING-TEST")))
      (ok (<= (length sources) 1)
          (format nil "Expected at most one apply-fixes-ordering-test, found ~D"
                  (length sources)))
      (when (= 1 (length sources))
        (let ((source (first sources))
              (form (deftest-form "fixer-test.lisp" "APPLY-FIXES-ORDERING-TEST")))
          (ok (form-contains-call-in-package-named-p form
                                                     "APPLY-FIXES"
                                                     "MALLET/FIXER")
              "apply-fixes-ordering-test must exercise fixer:apply-fixes as a call form")
          (ng (form-contains-call-named-p
               form
               "SORT")
              "apply-fixes-ordering-test must not sort violations inside the test")
          (ng (search "fixer::apply-fix" source :test #'char-equal)
              "apply-fixes-ordering-test must not hand-roll fixes with fixer::apply-fix"))))))

(deftest rule-type-system-tests-avoid-vacuous-slot-roundtrips
  (testing "forbidden vacuous slot/initarg roundtrip deftests are absent"
    (let* ((names (all-deftest-names "rules/rule-type-system-test.lisp"))
           (forbidden '("RULE-SEVERITY-TYPE"
                        "RULE-CATEGORY-SLOT"
                        "VIOLATION-SEVERITY-TYPE"
                        "VIOLATION-CATEGORY-SLOT"))
           (present (intersection names forbidden :test #'string=)))
      (ok (null present)
          (format nil "Forbidden vacuous deftests remain: ~{~A~^, ~}" present)))))

(deftest unused-variable-loop-regressions-do-not-print-debug-output
  (testing "previously affected LOOP regression tests contain no FORMAT T debug calls"
    (dolist (name '("LOOP-NESTED-WITH-HASH-VALUE-BUG"
                    "NESTED-LOOP-SHADOW-BUG"))
      (let ((sources (deftest-sources "rules/unused-variables-test.lisp" name)))
        (ok (= 1 (length sources))
            (format nil "Expected one ~A deftest, found ~D" name (length sources)))
        (when (= 1 (length sources))
          (ng (form-contains-format-to-t-p
               (deftest-form "rules/unused-variables-test.lisp" name))
              (format nil "~A still contains FORMAT T debug output" name)))))))

(defparameter *docstring-rule-test-files*
  '("rules/missing-docstring-test.lisp"
    "rules/missing-struct-docstring-test.lisp"
    "rules/missing-variable-docstring-test.lisp"
    "rules/missing-package-docstring-test.lisp"))

(defparameter *docstring-rule-temp-file-test-files*
  '("rules/missing-docstring-test.lisp"
    "rules/missing-struct-docstring-test.lisp"
    "rules/missing-variable-docstring-test.lisp"))

(defparameter *docstring-rule-required-deftests*
  '(("rules/missing-docstring-test.lisp"
     "MISSING-DOCSTRING-VALID"
     "MISSING-DOCSTRING-INVALID")
    ("rules/missing-struct-docstring-test.lisp"
     "MISSING-STRUCT-DOCSTRING-VALID"
     "MISSING-STRUCT-DOCSTRING-INVALID")
    ("rules/missing-variable-docstring-test.lisp"
     "MISSING-VARIABLE-DOCSTRING-VALID"
     "MISSING-VARIABLE-DOCSTRING-INVALID")
    ("rules/missing-package-docstring-test.lisp"
     "MISSING-PACKAGE-DOCSTRING-VALID"
     "MISSING-PACKAGE-DOCSTRING-INVALID")))

(defun test-lisp-files (&optional (directory (test-source-pathname "")))
  (append
   (mapcar (lambda (file)
             (enough-namestring file (test-source-pathname "")))
           (uiop:directory-files directory "*.lisp"))
   (mapcan #'test-lisp-files
           (uiop:subdirectories directory))))

(defun source-contains-p (relative-path needle)
  (search needle (file-string relative-path) :test #'char=))

(defun source-contains-defun-p (relative-path name)
  (cl-ppcre:scan (format nil "(?i)\\(\\s*defun\\s+~A\\b" name)
                 (file-string relative-path)))

(defun test-files-defining-defun (name)
  (loop for file in (test-lisp-files)
        when (source-contains-defun-p file name)
          collect file))

(defun project-top-level-forms (relative-path)
  (let ((source (project-file-string relative-path))
        (forms '())
        (position 0))
    (let ((*package* (find-package :asdf))
          (*read-eval* nil))
      (loop
        (multiple-value-bind (form next-position)
            (read-from-string source nil :eof :start position)
          (when (eq form :eof)
            (return (nreverse forms)))
          (push form forms)
          (setf position next-position))))))

(defun defsystem-name-p (form name)
  (and (consp form)
       (symbolp (first form))
       (string= "DEFSYSTEM" (symbol-name (first form)))
       (string= name (string (second form)))))

(defun active-defsystem-form (relative-path name)
  (find-if (lambda (form)
             (defsystem-name-p form name))
           (project-top-level-forms relative-path)))

(defun active-asdf-file-components (component-forms)
  (loop for component in component-forms
        when (and (consp component)
                  (eq :file (first component))
                  (stringp (second component)))
          collect (second component)
        when (and (consp component)
                  (getf (cddr component) :components))
          append (active-asdf-file-components (getf (cddr component) :components))))

(defun active-defsystem-file-components (relative-path name)
  (let ((form (active-defsystem-form relative-path name)))
    (when form
      (active-asdf-file-components (getf (cddr form) :components)))))

(defun docstring-rule-deftest-owners (suffix)
  (loop for file in *docstring-rule-test-files*
        when (some (lambda (name)
                     (and (<= (length suffix) (length name))
                          (string= suffix name
                                   :start1 0
                                   :end1 (length suffix)
                                   :start2 (- (length name) (length suffix)))))
                   (all-deftest-names file))
          collect file))

(deftest docstring-test-temp-dir-helpers-remain-shared
  (testing "temp-dir helpers are defined only in the shared docstring test utility"
    (dolist (name '("MAKE-TEMP-DIR" "WRITE-TEMP-FILE" "CLEANUP-TEMP-DIR"))
      (let ((owners (intersection (test-files-defining-defun name)
                                  (cons "docstring-test-utils.lisp"
                                        *docstring-rule-test-files*)
                                  :test #'string=)))
        (ok (equal '("docstring-test-utils.lisp") owners)
            (format nil "~A must be defined only in docstring-test-utils.lisp, found in: ~{~A~^, ~}"
                    name
                    owners)))))

  (testing "exported-only docstring tests exercise the shared helper package"
    (dolist (file *docstring-rule-temp-file-test-files*)
      (ok (source-contains-p file "doc-util:make-temp-dir")
          (format nil "~A does not create temp dirs through the shared utility" file))
      (ok (source-contains-p file "doc-util:write-temp-file")
          (format nil "~A does not write temp files through the shared utility" file))
      (ok (source-contains-p file "doc-util:cleanup-temp-dir")
          (format nil "~A does not clean temp dirs through the shared utility" file)))))

(deftest docstring-generic-behavior-tests-remain-singular
  (testing "message-format coverage exists in exactly one docstring rule file"
    (let ((owners (docstring-rule-deftest-owners "-MESSAGE-FORMAT")))
      (ok (equal '("rules/missing-docstring-test.lisp") owners)
          (format nil "message-format docstring deftests found in: ~{~A~^, ~}" owners))))

  (testing "location coverage exists in exactly one docstring rule file"
    (let ((owners (docstring-rule-deftest-owners "-LOCATION")))
      (ok (equal '("rules/missing-docstring-test.lisp") owners)
          (format nil "location docstring deftests found in: ~{~A~^, ~}" owners))))

  (testing "multiple-form coverage exists in exactly one docstring rule file"
    (let ((owners (docstring-rule-deftest-owners "-MULTIPLE-FORMS")))
      (ok (equal '("rules/missing-docstring-test.lisp") owners)
          (format nil "multiple-form docstring deftests found in: ~{~A~^, ~}" owners)))))

(deftest docstring-rule-files-retain-valid-and-invalid-tests
  (testing "each docstring rule file keeps its rule-specific valid and invalid deftests"
    (dolist (entry *docstring-rule-required-deftests*)
      (destructuring-bind (file &rest required-names) entry
        (let* ((present (all-deftest-names file))
               (missing (set-difference required-names present :test #'string=)))
          (ok (null missing)
              (format nil "~A is missing required deftests: ~{~A~^, ~}"
                      file
                      missing)))))))

(deftest docstring-test-family-remains-registered
  (testing "ASDF registers the shared utility and all docstring rule test files as active components"
    (let ((components (active-defsystem-file-components "mallet.asd" "mallet/tests")))
      (dolist (component '("docstring-test-utils"
                           "missing-docstring-test"
                           "missing-package-docstring-test"
                           "missing-variable-docstring-test"
                           "missing-struct-docstring-test"))
        (ok (member component components :test #'string=)
            (format nil "mallet/tests is missing active ASDF file component ~A" component))))))
