(defpackage #:mallet/tests/rules/unused-local-nicknames
  (:use #:cl #:rove)
  (:local-nicknames
   (#:rules #:mallet/rules)
   (#:parser #:mallet/parser)
   (#:violation #:mallet/violation)))
(in-package #:mallet/tests/rules/unused-local-nicknames)

(defmacro with-test-file ((tmpfile-var code) &body body)
  "Helper macro to create temporary file with CODE and clean up after."
  `(uiop:with-temporary-file (:stream stream :pathname ,tmpfile-var
                              :type "lisp" :keep t)
     (write-string ,code stream)
     (finish-output stream)
     ,@body))

(deftest unused-local-nicknames-valid
  (testing "Valid: all local nicknames are used"
    (with-test-file (tmpfile "(defpackage #:test-package
                                 (:use #:cl)
                                 (:local-nicknames
                                  (#:a #:alexandria)
                                  (#:p #:parser))
                                 (:export #:foo))
                               (in-package #:test-package)
                               (defun foo (x)
                                 (a:if-let ((y (+ x 1)))
                                   (p:parse y)
                                   0))")
      (let* ((text (uiop:read-file-string tmpfile))
             (forms (parser:parse-forms text tmpfile))
             (rule (make-instance 'rules:unused-local-nicknames-rule))
             (violations (rules:check-form rule (first forms) tmpfile)))
        (ok (null violations)))))

  (testing "Valid: package-only file (no code after in-package)"
    (with-test-file (tmpfile "(defpackage #:test-package
                                 (:use #:cl)
                                 (:local-nicknames
                                  (#:a #:alexandria)
                                  (#:p #:parser))
                                 (:export #:foo))
                               (in-package #:test-package)")
      (let* ((text (uiop:read-file-string tmpfile))
             (forms (parser:parse-forms text tmpfile))
             (rule (make-instance 'rules:unused-local-nicknames-rule))
             (violations (rules:check-form rule (first forms) tmpfile)))
        ;; Should not report violations for package-only files
        (ok (null violations)))))

  (testing "Valid: no local nicknames defined"
    (with-test-file (tmpfile "(defpackage #:test-package
                                 (:use #:cl)
                                 (:export #:foo))
                               (in-package #:test-package)
                               (defun foo () 42)")
      (let* ((text (uiop:read-file-string tmpfile))
             (forms (parser:parse-forms text tmpfile))
             (rule (make-instance 'rules:unused-local-nicknames-rule))
             (violations (rules:check-form rule (first forms) tmpfile)))
        (ok (null violations))))))

(deftest unused-local-nicknames-invalid
  (testing "Invalid: unused local nickname"
    (with-test-file (tmpfile "(defpackage #:test-package
                                 (:use #:cl)
                                 (:local-nicknames
                                  (#:a #:alexandria)
                                  (#:p #:parser))
                                 (:export #:foo))
                               (in-package #:test-package)
                               (defun foo (x)
                                 (a:if-let ((y (+ x 1)))
                                   y
                                   0))")
      (let* ((text (uiop:read-file-string tmpfile))
             (forms (parser:parse-forms text tmpfile))
             (rule (make-instance 'rules:unused-local-nicknames-rule))
             (violations (rules:check-form rule (first forms) tmpfile)))
        ;; Should report 'p' as unused
        (ok (= (length violations) 1))
        (ok (eq (violation:violation-rule (first violations)) :unused-local-nicknames))
        (ok (search "Local nickname 'p'" (violation:violation-message (first violations))))
        (ok (search "parser" (violation:violation-message (first violations)))))))

  (testing "Invalid: multiple unused local nicknames"
    (with-test-file (tmpfile "(defpackage #:test-package
                                 (:use #:cl)
                                 (:local-nicknames
                                  (#:a #:alexandria)
                                  (#:p #:parser)
                                  (#:u #:uiop))
                                 (:export #:foo))
                               (in-package #:test-package)
                               (defun foo (x)
                                 (a:if-let ((y (+ x 1)))
                                   y
                                   0))")
      (let* ((text (uiop:read-file-string tmpfile))
             (forms (parser:parse-forms text tmpfile))
             (rule (make-instance 'rules:unused-local-nicknames-rule))
             (violations (rules:check-form rule (first forms) tmpfile)))
        ;; Should report 'p' and 'u' as unused
        (ok (= (length violations) 2))
        (ok (every (lambda (v) (eq (violation:violation-rule v) :unused-local-nicknames))
                   violations)))))

  (testing "Invalid: all local nicknames unused"
    (with-test-file (tmpfile "(defpackage #:test-package
                                 (:use #:cl)
                                 (:local-nicknames
                                  (#:a #:alexandria)
                                  (#:p #:parser))
                                 (:export #:foo))
                               (in-package #:test-package)
                               (defun foo (x)
                                 (+ x 1))")
      (let* ((text (uiop:read-file-string tmpfile))
             (forms (parser:parse-forms text tmpfile))
             (rule (make-instance 'rules:unused-local-nicknames-rule))
             (violations (rules:check-form rule (first forms) tmpfile)))
        ;; Should report both 'a' and 'p' as unused
        (ok (= (length violations) 2))))))

(deftest unused-local-nicknames-severity
  (testing "Rule has :info severity"
    (let ((rule (make-instance 'rules:unused-local-nicknames-rule)))
      (ok (eq (rules:rule-severity rule) :info)))))

(deftest unused-local-nicknames-autofix
  (testing "Auto-fix makes minimal changes - deletes only the unused line"
    (with-test-file (tmpfile "(defpackage #:test-package
  (:use #:cl)  ; Use Common Lisp
  (:local-nicknames
   (#:a #:alexandria)  ; Unused
   (#:glob #:trivial-glob))  ; Used for globs
  (:export #:config
           #:make-config))
(in-package #:test-package)
(defun config ()
  (glob:glob \"*.lisp\"))")
      (let* ((text (uiop:read-file-string tmpfile))
             (forms (parser:parse-forms text tmpfile))
             (rule (make-instance 'rules:unused-local-nicknames-rule))
             (violations (rules:check-form rule (first forms) tmpfile)))
        ;; Should report 'a' as unused
        (ok (= (length violations) 1))

        ;; Get the fix for the first violation
        (let* ((fix (rules:make-fix rule text tmpfile (first violations)))
               (fix-type (violation:violation-fix-type fix)))
          (ok (not (null fix)))

          ;; Should use delete-lines (minimal change)
          (ok (eq fix-type :delete-lines)
              "Should use line deletion for minimal change")

          ;; Should delete exactly one line
          (ok (= (violation:violation-fix-start-line fix)
                 (violation:violation-fix-end-line fix))
              "Should delete exactly one line")))))

  (testing "Auto-fix preserves comments"
    (with-test-file (tmpfile "(defpackage #:test-package
  (:use #:cl)  ; Use Common Lisp
  (:local-nicknames
   (#:a #:alexandria)  ; Unused nickname
   (#:glob #:trivial-glob))  ; Used for glob patterns
  (:export #:config))  ; Public API
(in-package #:test-package)
(defun config ()
  (glob:glob \"*.lisp\"))")
      (let* ((text (uiop:read-file-string tmpfile))
             (forms (parser:parse-forms text tmpfile))
             (rule (make-instance 'rules:unused-local-nicknames-rule))
             (violations (rules:check-form rule (first forms) tmpfile))
             (fix (rules:make-fix rule text tmpfile (first violations))))

        ;; Apply the fix by simulating line deletion
        (let* ((lines (uiop:split-string text :separator '(#\Newline)))
               (start-line (violation:violation-fix-start-line fix))
               (end-line (violation:violation-fix-end-line fix))
               (new-lines (append (subseq lines 0 (1- start-line))
                                  (subseq lines end-line)))
               (result (format nil "~{~A~^~%~}" new-lines)))

          ;; Should preserve comments on other lines
          (ok (search "; Use Common Lisp" result)
              "Should preserve comment on :use line")
          (ok (search "; Used for glob patterns" result)
              "Should preserve comment on remaining nickname")
          (ok (search "; Public API" result)
              "Should preserve comment on :export line")

          ;; Should not contain the unused nickname
          (ok (not (search "#:a" result))
              "Should remove unused nickname")
          (ok (not (search "; Unused nickname" result))
              "Should remove comment on deleted line")))))

  (testing "Auto-fix removes :local-nicknames clause when last nickname deleted"
    (with-test-file (tmpfile "(defpackage #:test-package
  (:use #:cl)  ; Use Common Lisp
  (:local-nicknames
   (#:a #:alexandria))
  (:export #:foo))  ; Public API
(in-package #:test-package)
(defun foo () 42)")
      (let* ((text (uiop:read-file-string tmpfile))
             (forms (parser:parse-forms text tmpfile))
             (rule (make-instance 'rules:unused-local-nicknames-rule))
             (violations (rules:check-form rule (first forms) tmpfile)))
        ;; Should report one unused nickname
        (ok (= (length violations) 1))

        ;; Apply fix - should remove the entire :local-nicknames clause
        (let* ((fix (rules:make-fix rule text tmpfile (first violations)))
               (lines (uiop:split-string text :separator '(#\Newline)))
               (start-line (violation:violation-fix-start-line fix))
               (end-line (violation:violation-fix-end-line fix))
               (new-lines (append (subseq lines 0 (1- start-line))
                                  (subseq lines end-line)))
               (result (format nil "~{~A~^~%~}" new-lines)))
          (ok (not (null fix)))

          ;; The clause should be deleted
          (ok (not (search ":local-nicknames" result))
              "Should remove entire :local-nicknames clause")

          ;; Other comments should be preserved
          (ok (search "; Use Common Lisp" result)
              "Should preserve other comments")
          (ok (search "; Public API" result)
              "Should preserve other comments")))))

  (testing "Auto-fix preserves trailing close parens when deleting last item"
    (with-test-file (tmpfile "(defpackage #:test-trailing-paren
  (:use #:cl)
  (:local-nicknames
   (#:glob #:trivial-glob)
   (#:a #:alexandria))
  (:export #:main))
(in-package #:test-trailing-paren)
(defun main ()
  (glob:glob \"*.lisp\"))")
      (let* ((text (uiop:read-file-string tmpfile))
             (forms (parser:parse-forms text tmpfile))
             (rule (make-instance 'rules:unused-local-nicknames-rule))
             (violations (rules:check-form rule (first forms) tmpfile))
             (fix (rules:make-fix rule text tmpfile (first violations))))
        ;; Should report 'a' as unused
        (ok (= (length violations) 1))
        (ok (not (null fix)))

        ;; Apply the fix
        (let* ((lines (uiop:split-string text :separator '(#\Newline)))
               (fix-type (violation:violation-fix-type fix)))
          (cond
            ((eq fix-type :replace-line)
             (let* ((line-num (violation:violation-fix-line-number fix))
                    (replacement (violation:violation-fix-replacement-content fix))
                    (new-lines (append (subseq lines 0 (1- line-num))
                                       (list replacement)
                                       (subseq lines line-num)))
                    (result (format nil "~{~A~^~%~}" new-lines)))
               ;; Should preserve the closing paren
               (ok (search "(:local-nicknames" result)
                   "Should keep :local-nicknames clause")
               (ok (search "(#:glob #:trivial-glob)" result)
                   "Should keep the used nickname")
               (ok (not (search "#:alexandria" result))
                   "Should remove the unused nickname")
               ;; Count parens to ensure they're balanced
               (let ((open-count (count #\( result))
                     (close-count (count #\) result)))
                 (ok (= open-count close-count)
                     "Parens should be balanced"))))
            (t
             (ok nil "Expected :replace-line fix type"))))))))
