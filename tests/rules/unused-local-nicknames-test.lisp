(defpackage #:mallet/tests/rules/unused-local-nicknames
  (:use #:cl #:rove)
  (:local-nicknames
   (#:rules #:mallet/rules)
   (#:parser #:mallet/parser)
   (#:violation #:mallet/violation)
   (#:fixer #:mallet/fixer)))
(in-package #:mallet/tests/rules/unused-local-nicknames)

(defmacro with-test-file ((tmpfile-var code) &body body)
  "Helper macro to create temporary file with CODE and clean up after."
  `(uiop:with-temporary-file (:stream stream :pathname ,tmpfile-var
                              :type "lisp" :keep t)
     (write-string ,code stream)
     (finish-output stream)
     ,@body))

(deftest unused-local-nicknames-valid
  (testing "Valid: all local nicknames are used (defpackage)"
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
        (ok (null violations)))))

  (testing "Valid: all local nicknames are used (uiop:define-package)"
    (with-test-file (tmpfile "(uiop:define-package #:test-package
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
        (ok (= (length violations) 2)))))

  (testing "Invalid: unused local nickname (uiop:define-package)"
    (with-test-file (tmpfile "(uiop:define-package #:test-package
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
        (ok (search "parser" (violation:violation-message (first violations))))))))

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

          ;; Should use either delete-range or replace-form (minimal change)
          ;; replace-form is used when tidying trailing parens
          (ok (or (eq fix-type :delete-range) (eq fix-type :replace-form))
              "Should use minimal deletion strategy")))))

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
                  ((eq fix-type :delete-lines)
                   (let* ((start-line (violation:violation-fix-start-line fix))
                          (end-line (violation:violation-fix-end-line fix))
                          (new-lines (append (subseq lines 0 (1- start-line))
                                            (subseq lines end-line))))
                     (format nil "~{~A~^~%~}" new-lines)))
                  (t
                   (error "Unexpected fix type: ~A" fix-type)))))

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
            ((eq fix-type :replace-form)
             (let* ((start-line (violation:violation-fix-start-line fix))
                    (end-line (violation:violation-fix-end-line fix))
                    (replacement (violation:violation-fix-replacement-content fix))
                    (new-lines (append (subseq lines 0 (1- start-line))
                                      (list (string-right-trim '(#\Newline) replacement))
                                      (subseq lines end-line)))
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
             (ok nil "Expected :replace-form fix type")))))))

  (testing "Auto-fix with multiple unused nicknames - all violations get fixes"
    (with-test-file (tmpfile "(defpackage #:test-multi-unused
  (:use #:cl)
  (:local-nicknames
   (#:a #:alexandria)
   (#:u #:uiop))
  (:export #:foo))
(in-package #:test-multi-unused)
(defun foo () 42)")
      (let* ((text (uiop:read-file-string tmpfile))
             (forms (parser:parse-forms text tmpfile))
             (rule (make-instance 'rules:unused-local-nicknames-rule))
             (violations (rules:check-form rule (first forms) tmpfile)))
        ;; Should report 2 unused nicknames
        (ok (= (length violations) 2))

        ;; All violations should get fixes
        (let ((fixes (mapcar (lambda (v) (rules:make-fix rule text tmpfile v))
                            violations)))
          (ok (every #'identity fixes)
              "All violations should have fixes")

          ;; All fixes should be identical (delete entire :local-nicknames clause)
          (let ((first-fix (first fixes)))
            (ok (eq (violation:violation-fix-type first-fix) :delete-lines))
            (ok (every (lambda (fix)
                        (and (eq (violation:violation-fix-type fix) :delete-lines)
                             (= (violation:violation-fix-start-line fix)
                                (violation:violation-fix-start-line first-fix))
                             (= (violation:violation-fix-end-line fix)
                                (violation:violation-fix-end-line first-fix))))
                      fixes)
                "All fixes should be identical"))))))

  (testing "Auto-fix deduplication - multiple identical fixes applied only once"
    (with-test-file (tmpfile "(defpackage #:test-dedup
  (:use #:cl)
  (:local-nicknames
   (#:a #:alexandria)
   (#:u #:uiop))
  (:export #:foo))
(in-package #:test-dedup)
(defun foo () 42)")
      (let* ((text (uiop:read-file-string tmpfile))
             (forms (parser:parse-forms text tmpfile))
             (rule (make-instance 'rules:unused-local-nicknames-rule))
             (violations (rules:check-form rule (first forms) tmpfile)))
        ;; Should report 2 unused nicknames
        (ok (= (length violations) 2))

        ;; Generate fixes and attach them to violations
        (dolist (v violations)
          (let ((fix (rules:make-fix rule text tmpfile v)))
            (setf (violation:violation-fix v) fix)))

        ;; Apply fixes through the fixer (which has deduplication)
        ;; Test dry-run first
        (let ((fixed-violations (fixer:apply-fixes-to-file tmpfile violations :dry-run t)))
          ;; Both violations should be in fixed list
          (ok (= (length fixed-violations) 2)
              "Both violations should be marked as fixed"))

        ;; Actually apply the fix (not dry-run)
        (fixer:apply-fixes-to-file tmpfile violations :dry-run nil)

        ;; Read result and verify
        (let ((result (uiop:read-file-string tmpfile)))
          ;; Should not have :local-nicknames clause
          (ok (not (search ":local-nicknames" result))
              "Should remove entire :local-nicknames clause")

          ;; Should have other clauses
          (ok (search "(:use #:cl)" result)
              "Should preserve :use clause")
          (ok (search "(:export #:foo)" result)
              "Should preserve :export clause")

          ;; Parens should be balanced
          (let ((open-count (count #\( result))
                (close-count (count #\) result)))
            (ok (= open-count close-count)
                "Parens should be balanced after fix"))

          ;; File should be parseable
          (ok (handler-case
                  (progn
                    (parser:parse-forms result tmpfile)
                    t)
                (error () nil))
              "Fixed file should be parseable")))))

  (testing "Auto-fix with some used and some unused nicknames"
    (with-test-file (tmpfile "(defpackage #:test-partial
  (:use #:cl)
  (:local-nicknames
   (#:a #:alexandria)
   (#:glob #:trivial-glob)
   (#:u #:uiop))
  (:export #:main))
(in-package #:test-partial)
(defun main ()
  (glob:glob \"*.lisp\"))")
      (let* ((text (uiop:read-file-string tmpfile))
             (forms (parser:parse-forms text tmpfile))
             (rule (make-instance 'rules:unused-local-nicknames-rule))
             (violations (rules:check-form rule (first forms) tmpfile)))
        ;; Should report 2 unused nicknames (a and u)
        (ok (= (length violations) 2))

        ;; Generate fixes and attach them to violations
        (let ((fixes (mapcar (lambda (v)
                              (let ((fix (rules:make-fix rule text tmpfile v)))
                                (setf (violation:violation-fix v) fix)
                                fix))
                            violations)))
          (ok (every #'identity fixes)
              "All violations should have fixes")

          ;; Fixes should NOT be delete-lines (since some nicknames are still used)
          (ok (every (lambda (fix)
                      (not (eq (violation:violation-fix-type fix) :delete-lines)))
                    fixes)
              "Should not delete entire clause when some nicknames are used")

          ;; Apply fixes
          (fixer:apply-fixes-to-file tmpfile violations :dry-run nil)

          ;; Read result and verify
          (let ((result (uiop:read-file-string tmpfile)))
            ;; Should still have :local-nicknames clause
            (ok (search ":local-nicknames" result)
                "Should keep :local-nicknames clause")

            ;; Should keep used nickname
            (ok (search "#:glob" result)
                "Should keep used nickname")

            ;; Should remove unused nicknames
            (ok (not (search "#:alexandria" result))
                "Should remove unused nickname 'a'")
            (ok (not (search "#:uiop" result))
                "Should remove unused nickname 'u'")

            ;; Parens should be balanced
            (let ((open-count (count #\( result))
                  (close-count (count #\) result)))
              (ok (= open-count close-count)
                  "Parens should be balanced"))

            ;; File should be parseable
            (ok (handler-case
                    (progn
                      (parser:parse-forms result tmpfile)
                      t)
                  (error () nil))
                "Fixed file should be parseable"))))))

  (testing "Auto-fix works with uiop:define-package"
    (with-test-file (tmpfile "(uiop:define-package #:test-uiop
  (:use #:cl)
  (:local-nicknames
   (#:a #:alexandria)
   (#:glob #:trivial-glob))
  (:export #:main))
(in-package #:test-uiop)
(defun main ()
  (glob:glob \"*.lisp\"))")
      (let* ((text (uiop:read-file-string tmpfile))
             (forms (parser:parse-forms text tmpfile))
             (rule (make-instance 'rules:unused-local-nicknames-rule))
             (violations (rules:check-form rule (first forms) tmpfile)))
        ;; Should report 'a' as unused
        (ok (= (length violations) 1))

        ;; Generate fix and attach it to violation
        (let ((fix (rules:make-fix rule text tmpfile (first violations))))
          (setf (violation:violation-fix (first violations)) fix)
          (ok (not (null fix)))

          ;; Apply fix
          (fixer:apply-fixes-to-file tmpfile violations :dry-run nil)

          ;; Read result and verify
          (let ((result (uiop:read-file-string tmpfile)))
            ;; Should still have uiop:define-package and :local-nicknames
            (ok (search "uiop:define-package" result)
                "Should preserve uiop:define-package")
            (ok (search ":local-nicknames" result)
                "Should keep :local-nicknames clause")

            ;; Should keep used nickname
            (ok (search "#:glob" result)
                "Should keep used nickname")

            ;; Should remove unused nickname
            (ok (not (search "#:alexandria" result))
                "Should remove unused nickname 'a'")

            ;; Parens should be balanced
            (let ((open-count (count #\( result))
                  (close-count (count #\) result)))
              (ok (= open-count close-count)
                  "Parens should be balanced"))

            ;; File should be parseable
            (ok (handler-case
                    (progn
                      (parser:parse-forms result tmpfile)
                      t)
                  (error () nil))
                "Fixed file should be parseable")))))))
