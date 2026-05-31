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

(defun source-package (relative-path)
  (or (cdr (assoc relative-path
                  '(("fixer-test.lisp" . "MALLET/TESTS/FIXER")
                    ("rules/unused-variables-test.lisp" . "MALLET/TESTS/RULES/UNUSED-VARIABLES")
                    ("rules/rule-type-system-test.lisp" . "MALLET/TESTS/RULES/RULE-TYPE-SYSTEM"))
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
