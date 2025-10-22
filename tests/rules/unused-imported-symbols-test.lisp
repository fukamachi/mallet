(defpackage #:mallet/tests/rules/unused-imported-symbols
  (:use #:cl #:rove)
  (:local-nicknames
   (#:rules #:mallet/rules)
   (#:parser #:mallet/parser)
   (#:violation #:mallet/violation)))
(in-package #:mallet/tests/rules/unused-imported-symbols)

(defmacro with-test-file ((tmpfile-var code) &body body)
  "Helper macro to create temporary file with CODE and clean up after."
  `(uiop:with-temporary-file (:stream stream :pathname ,tmpfile-var
                              :type "lisp" :keep t)
     (write-string ,code stream)
     (finish-output stream)
     ,@body))

(deftest unused-imported-symbols-valid
  (testing "Valid: all imported symbols are used"
    (with-test-file (tmpfile "(defpackage #:test-package
                                 (:use #:cl)
                                 (:import-from #:some-package
                                  #:imported-symbol
                                  #:another-symbol)
                                 (:export #:foo))
                               (in-package #:test-package)
                               (defun foo ()
                                 (imported-symbol))
                               (defun bar ()
                                 (another-symbol))")
      (let* ((text (uiop:read-file-string tmpfile))
             (forms (parser:parse-forms text tmpfile))
             (rule (make-instance 'rules:unused-imported-symbols-rule))
             (violations (rules:check-form rule (first forms) tmpfile)))
        (ok (null violations)))))

  (testing "Valid: imported symbol is re-exported"
    (with-test-file (tmpfile "(defpackage #:test-package
                                 (:use #:cl)
                                 (:import-from #:some-package
                                  #:imported-symbol
                                  #:another-symbol)
                                 (:export #:imported-symbol))
                               (in-package #:test-package)
                               (defun foo ()
                                 (another-symbol))")
      (let* ((text (uiop:read-file-string tmpfile))
             (forms (parser:parse-forms text tmpfile))
             (rule (make-instance 'rules:unused-imported-symbols-rule))
             (violations (rules:check-form rule (first forms) tmpfile)))
        ;; Should not report 'imported-symbol' as unused because it's re-exported
        (ok (null violations)))))

  (testing "Valid: package-only file (no code after in-package)"
    (with-test-file (tmpfile "(defpackage #:test-package
                                 (:use #:cl)
                                 (:import-from #:some-package
                                  #:imported-symbol
                                  #:another-symbol)
                                 (:export #:foo))
                               (in-package #:test-package)")
      (let* ((text (uiop:read-file-string tmpfile))
             (forms (parser:parse-forms text tmpfile))
             (rule (make-instance 'rules:unused-imported-symbols-rule))
             (violations (rules:check-form rule (first forms) tmpfile)))
        ;; Should not report violations for package-only files
        (ok (null violations)))))

  (testing "Valid: no imported symbols"
    (with-test-file (tmpfile "(defpackage #:test-package
                                 (:use #:cl)
                                 (:export #:foo))
                               (in-package #:test-package)
                               (defun foo () 42)")
      (let* ((text (uiop:read-file-string tmpfile))
             (forms (parser:parse-forms text tmpfile))
             (rule (make-instance 'rules:unused-imported-symbols-rule))
             (violations (rules:check-form rule (first forms) tmpfile)))
        (ok (null violations))))))

(deftest unused-imported-symbols-invalid
  (testing "Invalid: unused imported symbol"
    (with-test-file (tmpfile "(defpackage #:test-package
                                 (:use #:cl)
                                 (:import-from #:some-package
                                  #:imported-symbol
                                  #:another-symbol)
                                 (:export #:foo))
                               (in-package #:test-package)
                               (defun foo ()
                                 (imported-symbol))")
      (let* ((text (uiop:read-file-string tmpfile))
             (forms (parser:parse-forms text tmpfile))
             (rule (make-instance 'rules:unused-imported-symbols-rule))
             (violations (rules:check-form rule (first forms) tmpfile)))
        ;; Should report 'another-symbol' as unused
        (ok (= (length violations) 1))
        (ok (eq (violation:violation-rule (first violations)) :unused-imported-symbols))
        (ok (search "Imported symbol 'another-symbol'" (violation:violation-message (first violations))))
        (ok (search "some-package" (violation:violation-message (first violations)))))))

  (testing "Invalid: multiple unused imported symbols from one package"
    (with-test-file (tmpfile "(defpackage #:test-package
                                 (:use #:cl)
                                 (:import-from #:some-package
                                  #:imported-symbol
                                  #:another-symbol
                                  #:third-symbol)
                                 (:export #:foo))
                               (in-package #:test-package)
                               (defun foo ()
                                 (imported-symbol))")
      (let* ((text (uiop:read-file-string tmpfile))
             (forms (parser:parse-forms text tmpfile))
             (rule (make-instance 'rules:unused-imported-symbols-rule))
             (violations (rules:check-form rule (first forms) tmpfile)))
        ;; Should report 'another-symbol' and 'third-symbol' as unused
        (ok (= (length violations) 2))
        (ok (every (lambda (v) (eq (violation:violation-rule v) :unused-imported-symbols))
                   violations)))))

  (testing "Invalid: unused imports from multiple packages"
    (with-test-file (tmpfile "(defpackage #:test-package
                                 (:use #:cl)
                                 (:import-from #:some-package
                                  #:imported-symbol
                                  #:another-symbol)
                                 (:import-from #:other-package
                                  #:other-symbol)
                                 (:export #:foo))
                               (in-package #:test-package)
                               (defun foo ()
                                 (imported-symbol))")
      (let* ((text (uiop:read-file-string tmpfile))
             (forms (parser:parse-forms text tmpfile))
             (rule (make-instance 'rules:unused-imported-symbols-rule))
             (violations (rules:check-form rule (first forms) tmpfile)))
        ;; Should report 'another-symbol' and 'other-symbol' as unused
        (ok (= (length violations) 2))
        (ok (every (lambda (v) (eq (violation:violation-rule v) :unused-imported-symbols))
                   violations)))))

  (testing "Invalid: all imported symbols unused (none re-exported)"
    (with-test-file (tmpfile "(defpackage #:test-package
                                 (:use #:cl)
                                 (:import-from #:some-package
                                  #:imported-symbol
                                  #:another-symbol)
                                 (:export #:foo))
                               (in-package #:test-package)
                               (defun foo ()
                                 42)")
      (let* ((text (uiop:read-file-string tmpfile))
             (forms (parser:parse-forms text tmpfile))
             (rule (make-instance 'rules:unused-imported-symbols-rule))
             (violations (rules:check-form rule (first forms) tmpfile)))
        ;; Should report both symbols as unused
        (ok (= (length violations) 2))))))

(deftest unused-imported-symbols-severity
  (testing "Rule has :info severity"
    (let ((rule (make-instance 'rules:unused-imported-symbols-rule)))
      (ok (eq (rules:rule-severity rule) :info)))))

(deftest unused-imported-symbols-autofix
  (testing "Auto-fix makes minimal changes - deletes only the unused symbol line"
    (with-test-file (tmpfile "(defpackage #:test-package
  (:use #:cl)
  (:import-from #:alexandria
                #:hash-table-keys
                #:hash-table-values)
  (:export #:config))
(in-package #:test-package)
(defun config ()
  (hash-table-keys (make-hash-table)))")
      (let* ((text (uiop:read-file-string tmpfile))
             (forms (parser:parse-forms text tmpfile))
             (rule (make-instance 'rules:unused-imported-symbols-rule))
             (violations (rules:check-form rule (first forms) tmpfile)))
        ;; Should report 'hash-table-values' as unused
        (ok (= (length violations) 1))

        ;; Get the fix for the first violation
        (let* ((fix (rules:make-fix rule text tmpfile (first violations)))
               (fix-type (violation:violation-fix-type fix)))
          (ok (not (null fix)))

          ;; Should use either delete-range or replace-form (minimal change)
          ;; replace-form is used when tidying trailing parens
          (ok (or (eq fix-type :delete-range) (eq fix-type :replace-form))
              "Should use minimal deletion strategy")))))

  (testing "Auto-fix preserves comments"
    (with-test-file (tmpfile "(defpackage #:test-package
  (:use #:cl)  ; Common Lisp
  (:import-from #:alexandria
                #:hash-table-keys  ; Used
                #:hash-table-values)  ; Unused
  (:export #:config))  ; Public API
(in-package #:test-package)
(defun config ()
  (hash-table-keys (make-hash-table)))")
      (let* ((text (uiop:read-file-string tmpfile))
             (forms (parser:parse-forms text tmpfile))
             (rule (make-instance 'rules:unused-imported-symbols-rule))
             (violations (rules:check-form rule (first forms) tmpfile))
             (fix (rules:make-fix rule text tmpfile (first violations))))

        (ok (not (null fix)))

        ;; Apply the fix - handle both delete-range and replace-form
        (let* ((lines (uiop:split-string text :separator '(#\Newline)))
               (fix-type (violation:violation-fix-type fix))
               (result
                (cond
                  ((eq fix-type :delete-range)
                   (let* ((start-line (violation:violation-fix-start-line fix))
                          (start-col (violation:violation-fix-start-column fix))
                          (end-line (violation:violation-fix-end-line fix))
                          (end-col (violation:violation-fix-end-column fix))
                          (line-before (subseq (nth (1- start-line) lines) 0 start-col))
                          (line-after (when (< end-line (1+ (length lines)))
                                       (subseq (nth (1- end-line) lines) end-col)))
                          (new-line (concatenate 'string line-before (or line-after "")))
                          (new-lines (append (subseq lines 0 (1- start-line))
                                            (list new-line)
                                            (subseq lines end-line))))
                     (format nil "~{~A~^~%~}" new-lines)))
                  ((eq fix-type :replace-form)
                   (let* ((start-line (violation:violation-fix-start-line fix))
                          (end-line (violation:violation-fix-end-line fix))
                          (replacement (violation:violation-fix-replacement-content fix))
                          (new-lines (append (subseq lines 0 (1- start-line))
                                            (list (string-right-trim '(#\Newline) replacement))
                                            (subseq lines end-line))))
                     (format nil "~{~A~^~%~}" new-lines)))
                  (t
                   (error "Unexpected fix type: ~A" fix-type)))))

          ;; Should preserve comments on other lines
          (ok (search "; Common Lisp" result)
              "Should preserve comment on :use line")
          (ok (search "; Used" result)
              "Should preserve comment on remaining symbol")
          (ok (search "; Public API" result)
              "Should preserve comment on :export line")

          ;; Should not contain the unused symbol
          (ok (not (search "hash-table-values" result))
              "Should remove unused symbol")))))

  (testing "Auto-fix removes :import-from clause when last symbol deleted"
    (with-test-file (tmpfile "(defpackage #:test-package
  (:use #:cl)
  (:import-from #:alexandria
                #:hash-table-keys)
  (:export #:foo))
(in-package #:test-package)
(defun foo () 42)")
      (let* ((text (uiop:read-file-string tmpfile))
             (forms (parser:parse-forms text tmpfile))
             (rule (make-instance 'rules:unused-imported-symbols-rule))
             (violations (rules:check-form rule (first forms) tmpfile)))
        ;; Should report one unused symbol
        (ok (= (length violations) 1))

        ;; Apply fix - should remove the entire :import-from clause
        (let* ((fix (rules:make-fix rule text tmpfile (first violations)))
               (lines (uiop:split-string text :separator '(#\Newline)))
               (start-line (violation:violation-fix-start-line fix))
               (end-line (violation:violation-fix-end-line fix))
               (new-lines (append (subseq lines 0 (1- start-line))
                                 (subseq lines end-line)))
               (result (format nil "~{~A~^~%~}" new-lines)))
          (ok (not (null fix)))

          ;; The clause should be deleted
          (ok (not (search ":import-from" result))
              "Should remove entire :import-from clause")

          ;; Other parts should be preserved
          (ok (search "(:use #:cl)" result)
              "Should preserve :use clause")
          (ok (search "(:export #:foo)" result)
              "Should preserve :export clause")))))

  (testing "Auto-fix preserves trailing close parens"
    (with-test-file (tmpfile "(defpackage #:test-package
  (:use #:cl)
  (:import-from #:alexandria
                #:hash-table-keys
                #:hash-table-values)
  (:export #:config))
(in-package #:test-package)
(defun config ()
  (hash-table-keys (make-hash-table)))")
      (let* ((text (uiop:read-file-string tmpfile))
             (forms (parser:parse-forms text tmpfile))
             (rule (make-instance 'rules:unused-imported-symbols-rule))
             (violations (rules:check-form rule (first forms) tmpfile))
             (fix (rules:make-fix rule text tmpfile (first violations))))
        ;; Should report 'hash-table-values' as unused
        (ok (= (length violations) 1))
        (ok (not (null fix)))

        ;; Apply the fix using replace-form (parens on next line)
        (let* ((lines (uiop:split-string text :separator '(#\Newline)))
               (fix-type (violation:violation-fix-type fix)))
          (cond
            ((eq fix-type :replace-form)
             (let* ((start-line (violation:violation-fix-start-line fix))
                    (end-line (violation:violation-fix-end-line fix))
                    (replacement (violation:violation-fix-replacement-content fix))
                    (new-lines (append (subseq lines 0 (1- start-line))
                                      (list (string-right-trim '(#\Newline) replacement))
                                      (subseq lines end-line)))
                    (result (format nil "~{~A~^~%~}" new-lines)))
               ;; Should preserve the closing parens
               (ok (search "#:hash-table-keys)" result)
                   "Should preserve closing paren after last symbol")))
            (t
             (ok nil "Expected :replace-form fix type")))))))

  (testing "Auto-fix handles multiple unused symbols"
    (with-test-file (tmpfile "(defpackage #:test-package
  (:use #:cl)
  (:import-from #:alexandria
                #:hash-table-keys
                #:hash-table-values
                #:hash-table-count)
  (:export #:config))
(in-package #:test-package)
(defun config ()
  (hash-table-keys (make-hash-table)))")
      (let* ((text (uiop:read-file-string tmpfile))
             (forms (parser:parse-forms text tmpfile))
             (rule (make-instance 'rules:unused-imported-symbols-rule))
             (violations (rules:check-form rule (first forms) tmpfile)))
        ;; Should report 2 unused symbols
        (ok (= (length violations) 2))

        ;; Each should have a fix
        (ok (every (lambda (v) (rules:make-fix rule text tmpfile v))
                   violations)
            "Each violation should have a fix")))))
