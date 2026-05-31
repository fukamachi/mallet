(defpackage #:mallet/tests/engine-comment-suppression
  (:use #:cl
        #:rove)
  (:local-nicknames
   (#:engine #:mallet/engine)
   (#:config #:mallet/config)
   (#:rules #:mallet/rules)
   (#:violation #:mallet/violation)))
(in-package #:mallet/tests/engine-comment-suppression)

;;; Helper functions

(defun fixture-path (subdir filename)
  (merge-pathnames (concatenate 'string "tests/fixtures/" subdir "/" filename)
                   (asdf:system-source-directory :mallet)))

(defun violations-fixture (filename)
  (fixture-path "violations" filename))

(defun no-violations-fixture (filename)
  (fixture-path "no-violations" filename))

(defun project-path (relative-path)
  (merge-pathnames relative-path
                   (asdf:system-source-directory :mallet)))

(defun source-lines (&rest lines)
  (with-output-to-string (out)
    (dolist (line lines)
      (write-line line out))))

(defun temporary-source-path ()
  (let ((directory (merge-pathnames ".cache/test-sources/"
                                    (asdf:system-source-directory :mallet))))
    (ensure-directories-exist directory)
    (merge-pathnames (format nil "~(~A~).lisp"
                             (gensym "engine-comment-suppression-"))
                     directory)))

(defmacro with-lint-source ((file-var violations-var source config) &body body)
  `(let ((,file-var (temporary-source-path)))
     (unwind-protect
          (progn
            (with-open-file (out ,file-var
                                 :direction :output
                                 :if-exists :supersede
                                 :if-does-not-exist :create)
              (write-string ,source out))
            (let ((,violations-var (engine:lint-file ,file-var :config ,config)))
              ,@body))
       (when (probe-file ,file-var)
         (delete-file ,file-var)))))

(defun defun-name-from-line (line)
  (let ((start (search "(defun" line :test #'char-equal)))
    (when start
      (let* ((name-start (loop for index from (+ start (length "(defun")) below (length line)
                               while (member (char line index) '(#\Space #\Tab))
                               finally (return index)))
             (name-end (or (position-if (lambda (char)
                                           (member char '(#\Space #\Tab #\()))
                                         line
                                         :start name-start)
                           (length line))))
        (when (< name-start name-end)
          (intern (string-upcase (subseq line name-start name-end))))))))

(defun file-lines (file)
  (with-open-file (in file)
    (loop for line = (read-line in nil nil)
          while line
          collect line)))

(defun nearest-defun-name-before-line (file line-number)
  (let* ((lines (coerce (file-lines file) 'vector))
         (last-index (min (1- line-number) (1- (length lines)))))
    (loop for index from last-index downto 0
          for name = (defun-name-from-line (aref lines index))
          when name
            return name)))

(defun violation-defun-names (file violations)
  (mapcar (lambda (violation)
            (nearest-defun-name-before-line file
                                            (violation:violation-line violation)))
          (sort (copy-list violations) #'< :key #'violation:violation-line)))

(defun make-needless-let*-config ()
  (config:make-config
   :rules (list (rules:make-rule :needless-let*))))

(defun make-needless-let*-and-stale-config ()
  (config:make-config
   :rules (list (rules:make-rule :needless-let*)
                (rules:make-rule :stale-suppression))))

(defun make-if-without-else-config ()
  (config:make-config
   :rules (list (rules:make-rule :missing-else))))

(defun make-if-without-else-and-stale-config ()
  (config:make-config
   :rules (list (rules:make-rule :missing-else)
                (rules:make-rule :stale-suppression))))

(defun make-line-length-and-stale-config ()
  (config:make-config
   :rules (list (rules:make-rule :line-length)
                (rules:make-rule :stale-suppression))))

(defun make-double-colon-config ()
  (config:make-config
   :rules (list (rules:make-rule :double-colon-access))))

(defun make-double-colon-and-stale-config ()
  (config:make-config
   :rules (list (rules:make-rule :double-colon-access)
                (rules:make-rule :stale-suppression))))

(defun legacy-engine-integration-name ()
  (concatenate 'string "engine-" "integration-test"))

(defun obsolete-suppression-fixture-names ()
  (list (concatenate 'string "comment-" "suppress.lisp")
        (concatenate 'string "comment-" "disable.lisp")
        (concatenate 'string "comment-" "stale.lisp")
        (concatenate 'string "declaim-" "stale.lisp")))

(defun relevant-repository-file-p (file)
  (member (string-downcase (or (pathname-type file) ""))
          '("asd" "lisp" "sh")
          :test #'string=))

(defun skipped-repository-directory-p (directory)
  (let ((name (car (last (pathname-directory directory)))))
    (member name '(".cache" ".foundry" ".git" ".qlot")
            :test #'string=)))

(defun repository-files ()
  (labels ((walk (directory)
             (append (remove-if-not #'relevant-repository-file-p
                                    (uiop:directory-files directory))
                     (loop for subdir in (uiop:subdirectories directory)
                           unless (skipped-repository-directory-p subdir)
                             append (walk subdir)))))
    (walk (asdf:system-source-directory :mallet))))

(defun file-contains-string-p (file needle)
  (search needle
          (uiop:read-file-string file)
          :test #'char=))

(defun files-containing-string (needle)
  (loop for file in (repository-files)
        when (file-contains-string-p file needle)
          collect file))

;;; Repository-shape regression tests for the merge itself

(deftest legacy-engine-suite-file-removed
  (testing "The legacy engine integration test file is absent"
    (let* ((legacy-name (legacy-engine-integration-name))
           (legacy-file (project-path (format nil "tests/~A.lisp" legacy-name))))
      (ng (probe-file legacy-file)
          "The redundant legacy engine integration test file is not present")))

  (testing "The legacy engine integration test component is absent from mallet.asd"
    (let* ((legacy-name (legacy-engine-integration-name))
           (component-reference (format nil "(:file ~S)" legacy-name))
           (asd-text (uiop:read-file-string (project-path "mallet.asd"))))
      (ng (search component-reference asd-text :test #'char=)
          "mallet.asd no longer registers the redundant legacy test component")
      (ok (null (files-containing-string legacy-name))
          "The redundant legacy test name has no source references"))))

(deftest obsolete-suppression-fixtures-removed
  (testing "Obsolete suppression fixture files are absent"
    (dolist (fixture-name (obsolete-suppression-fixture-names))
      (let ((matching-files (loop for directory in '("tests/fixtures/violations/"
                                                     "tests/fixtures/no-violations/"
                                                     "tests/fixtures/clean/")
                                  for file = (project-path (concatenate 'string directory fixture-name))
                                  when (probe-file file)
                                    collect file)))
        (ok (null matching-files)
            (format nil "Obsolete fixture file ~A is not present" fixture-name)))))

  (testing "Obsolete suppression fixture names have no repository references"
    (dolist (fixture-name (obsolete-suppression-fixture-names))
      (let ((references (files-containing-string fixture-name)))
        (ok (null references)
            (format nil "Obsolete fixture name ~A has no source references" fixture-name))))))

;;; Test 1: Stale suppression — comment with no matching violation

(deftest comment-suppress-stale-no-violation
  (testing "Stale :suppress generates stale-suppression violation"
    (let* ((file (violations-fixture "comment-suppress-stale.lisp"))
           (config (make-needless-let*-and-stale-config))
           (violations (engine:lint-file file :config config)))

      ;; No needless-let* violations (the form uses let, not let*)
      (let ((let*-violations (remove-if-not
                               (lambda (v) (eq :needless-let* (violation:violation-rule v)))
                               violations)))
        (ok (null let*-violations)
            "No needless-let* violations (form uses plain let)"))

      ;; Exactly one stale-suppression violation
      (let ((stale-violations (remove-if-not
                                (lambda (v) (eq :stale-suppression (violation:violation-rule v)))
                                violations)))
        (ok (= 1 (length stale-violations))
            "Exactly 1 stale-suppression violation for unused :needless-let* suppress")
        (when (= 1 (length stale-violations))
          (ok (eq :warning (violation:violation-severity (first stale-violations)))
              "Stale suppression severity is :warning"))))))

(deftest comment-suppress-stale-no-rule-no-violation
  (testing "No stale-suppression violation when rule not in config"
    (let* ((file (violations-fixture "comment-suppress-stale.lisp"))
           (config (make-needless-let*-config)) ; no :stale-suppression rule
           (violations (engine:lint-file file :config config)))

      (let ((stale-violations (remove-if-not
                                (lambda (v) (eq :stale-suppression (violation:violation-rule v)))
                                violations)))
        (ok (null stale-violations)
            "No stale-suppression violations when rule not in config")))))

(deftest comment-suppress-dormant-rule-not-stale
  (testing "Suppression for a rule not in the active rule set is not flagged as stale"
    ;; Regression: a ; mallet:suppress comment targeting a rule that is not
    ;; enabled in the current config should be treated as dormant (a no-op),
    ;; not reported as stale.  Otherwise a suppression written for one preset
    ;; (e.g. :all) generates noise under another preset (e.g. :strict) even
    ;; though the suppressed rule cannot possibly fire.
    (let* ((file (violations-fixture "comment-suppress-stale.lisp"))
           ;; :line-length is active; the file's suppress comment targets
           ;; :needless-let*, which is NOT in this config.
           (config (make-line-length-and-stale-config))
           (violations (engine:lint-file file :config config)))

      (let ((stale-violations (remove-if-not
                                (lambda (v) (eq :stale-suppression (violation:violation-rule v)))
                                violations)))
        (ok (null stale-violations)
            "No stale-suppression for a rule that is not in the active set")))))

;;; Test 2: Negative control — same form WITHOUT trailing comment DOES produce a violation

(deftest comment-suppress-trailing-negative-control
  (testing "needless-let* without trailing suppress comment produces a violation"
    (let* ((file (violations-fixture "needless-let-star.lisp"))
           (config (make-needless-let*-config))
           (violations (engine:lint-file file :config config)))

      (let ((let*-violations (remove-if-not
                               (lambda (v) (eq :needless-let* (violation:violation-rule v)))
                               violations)))
        (ok (>= (length let*-violations) 1)
            "At least 1 needless-let* violation in unsuppressed file")))))

;;; Test 2b: Inline :suppress for missing-else leaves the next form unsuppressed

(deftest comment-suppress-missing-else-inline
  (testing "; mallet:suppress suppresses missing-else for the annotated form only"
    (let ((config (make-if-without-else-and-stale-config))
          (source (source-lines
                   "(defpackage #:test-comment-suppress-missing-else"
                   "  (:use #:cl))"
                   "(in-package #:test-comment-suppress-missing-else)"
                   ""
                   "; mallet:suppress :missing-else"
                   "(defun suppressed-foo (x)"
                   "  (if x"
                   "      (print \"yes\")))"
                   ""
                   "(defun unsuppressed-bar (x)"
                   "  (if x"
                   "      (print \"also yes\")))")))
      (with-lint-source (file violations source config)
        (let ((iwe-violations (remove-if-not
                               (lambda (v) (eq :missing-else (violation:violation-rule v)))
                               violations))
              (stale-violations (remove-if-not
                                 (lambda (v) (eq :stale-suppression (violation:violation-rule v)))
                                 violations)))
          (ok (equal '(unsuppressed-bar)
                     (violation-defun-names file iwe-violations))
              "Only unsuppressed-bar keeps its missing-else violation")
          (ok (null stale-violations)
              "The suppress for suppressed-foo was used and is not stale"))))))

;;; Test 3: Trailing same-line suppression — comment on the same line as the form

(deftest comment-suppress-trailing-no-output
  (testing "Trailing ; mallet:suppress on same line suppresses the form"
    (let* ((file (no-violations-fixture "comment-suppress-trailing.lisp"))
           (config (make-needless-let*-config))
           (violations (engine:lint-file file :config config)))

      (ok (null violations)
          "No violations when needless-let* is suppressed by trailing same-line comment")))

  (testing "Trailing suppress is not reported as stale"
    (let* ((file (no-violations-fixture "comment-suppress-trailing.lisp"))
           (config (make-needless-let*-and-stale-config))
           (violations (engine:lint-file file :config config)))

      (let ((stale-violations (remove-if-not
                                (lambda (v) (eq :stale-suppression (violation:violation-rule v)))
                                violations)))
        (ok (null stale-violations)
            "No stale-suppression violation when trailing suppress was used")))))

;;; Test 4: Active suppression — comment suppresses real violation, no output

(deftest comment-suppress-active-no-output
  (testing "; mallet:suppress :needless-let* suppresses the annotated form"
    (let* ((file (no-violations-fixture "comment-suppress-active.lisp"))
           (config (make-needless-let*-config))
           (violations (engine:lint-file file :config config)))

      (ok (null violations)
          "No violations when needless-let* is suppressed by inline comment")))

  (testing "Suppressed violation is not reported as stale"
    (let* ((file (no-violations-fixture "comment-suppress-active.lisp"))
           (config (make-needless-let*-and-stale-config))
           (violations (engine:lint-file file :config config)))

      ;; The suppress was actually used — it should not be stale
      (let ((stale-violations (remove-if-not
                                (lambda (v) (eq :stale-suppression (violation:violation-rule v)))
                                violations)))
        (ok (null stale-violations)
            "No stale-suppression violation when suppress was used")))))

;;; Test 5: disable/enable region for form-level rule

(deftest comment-disable-enable-region
  (testing "; mallet:disable suppresses until :enable, leaving others intact"
    (let* ((file (violations-fixture "comment-disable-enable.lisp"))
           (config (make-if-without-else-config))
           (violations (engine:lint-file file :config config)))

      (let ((iwe-violations (remove-if-not
                              (lambda (v) (eq :missing-else (violation:violation-rule v)))
                              violations)))
        ;; before-disable and after-enable should both be flagged
        ;; during-disable should be suppressed
        (ok (= 2 (length iwe-violations))
            "Exactly 2 if-without-else violations: before-disable and after-enable")
        (ok (equal '(before-disable after-enable)
                   (violation-defun-names file iwe-violations))
            "before-disable and after-enable keep violations; during-disable is suppressed"))))

  (testing "Violations before disable region are reported normally"
    (let* ((file (violations-fixture "comment-disable-enable.lisp"))
           (config (make-if-without-else-config))
           (violations (engine:lint-file file :config config)))

      (let ((iwe-violations (sort
                              (remove-if-not
                               (lambda (v) (eq :missing-else (violation:violation-rule v)))
                               violations)
                              #'< :key #'violation:violation-line)))
        (when (= 2 (length iwe-violations))
          (ok (< (violation:violation-line (first iwe-violations))
                 (violation:violation-line (second iwe-violations)))
              "First violation precedes second violation in source order"))))))

;;; Test: U-1 — Stale text/token :disable region with no matching violations

(deftest stale-text-disable-region
  (testing "Stale :disable for text/token rule generates stale-suppression violation"
    (let* ((file (violations-fixture "stale-text-disable.lisp"))
           (config (make-line-length-and-stale-config))
           (violations (engine:lint-file file :config config)))

      ;; No line-length violations expected (all lines are short)
      (let ((ll-violations (remove-if-not
                             (lambda (v) (eq :line-length (violation:violation-rule v)))
                             violations)))
        (ok (null ll-violations) "No line-length violations in fixture"))

      ;; Exactly one stale-suppression violation for the unused :disable
      (let ((stale-violations (remove-if-not
                                (lambda (v) (eq :stale-suppression (violation:violation-rule v)))
                                violations)))
        (ok (= 1 (length stale-violations))
            "Exactly 1 stale-suppression violation for unused :line-length disable region")))))

;;; Test: U-2a — Directive-like text in a multi-line docstring is not a real directive

(deftest docstring-multiline-directive-text-not-stale
  (testing "Directive-like text on a continuation line of a docstring is not registered"
    ;; The docstring contains '; mallet:suppress needless-let*' on a new line
    ;; where no quote precedes the semicolon on that line.
    ;; Before the fix this was incorrectly parsed as a real directive and then
    ;; reported as stale-suppression because the directive never matched a violation.
    (let* ((file (no-violations-fixture "docstring-with-directive-text.lisp"))
           (config (make-needless-let*-and-stale-config))
           (violations (engine:lint-file file :config config)))

      ;; No needless-let* violations (no let* in these functions)
      (let ((let*-violations (remove-if-not
                               (lambda (v) (eq :needless-let* (violation:violation-rule v)))
                               violations)))
        (ok (null let*-violations) "No needless-let* violations in clean fixture"))

      ;; No stale-suppression violations (directive-like text is not a real directive)
      (let ((stale-violations (remove-if-not
                                (lambda (v) (eq :stale-suppression (violation:violation-rule v)))
                                violations)))
        (ok (null stale-violations)
            "No stale-suppression violations: docstring text is not a directive")))))

;;; Test: inner-form ; mallet:suppress suppresses a nested violation

(deftest comment-suppress-inner-form
  (testing "; mallet:suppress inside a function body suppresses the nested violation"
    (let* ((file (no-violations-fixture "comment-suppress-inner.lisp"))
           (config (make-needless-let*-config))
           (violations (engine:lint-file file :config config)))

      (ok (null violations)
          "No violations when inner ; mallet:suppress suppresses the nested let*")))

  (testing "Inner suppress is not reported as stale when it matched a violation"
    (let* ((file (no-violations-fixture "comment-suppress-inner.lisp"))
           (config (make-needless-let*-and-stale-config))
           (violations (engine:lint-file file :config config)))

      (let ((stale-violations (remove-if-not
                                (lambda (v) (eq :stale-suppression (violation:violation-rule v)))
                                violations)))
        (ok (null stale-violations)
            "No stale-suppression violation when inner suppress was used")))))

;;; Test: inner-form #+mallet (declaim (mallet:suppress-next ...)) suppresses a nested violation

(deftest declaim-suppress-next-inner-form
  (testing "#+mallet suppress-next declaim inside a function body suppresses the nested violation"
    (let* ((file (no-violations-fixture "declaim-suppress-inner.lisp"))
           (config (make-needless-let*-config))
           (violations (engine:lint-file file :config config)))

      (ok (null violations)
          "No violations when inner suppress-next declaim suppresses the nested let*")))

  (testing "Inner suppress-next declaim is not reported as stale when it matched a violation"
    (let* ((file (no-violations-fixture "declaim-suppress-inner.lisp"))
           (config (make-needless-let*-and-stale-config))
           (violations (engine:lint-file file :config config)))

      (let ((stale-violations (remove-if-not
                                (lambda (v) (eq :stale-suppression (violation:violation-rule v)))
                                violations)))
        (ok (null stale-violations)
            "No stale-suppression violation when inner suppress-next declaim was consumed")))))

;;; Test: top-level #+mallet suppress-next reports stale only when unused

(deftest declaim-suppress-next-stale-when-unused
  (testing "Unused suppress-next is stale while a used suppress-next suppresses its form"
    (let ((config (make-needless-let*-and-stale-config))
          (source (source-lines
                   "(defpackage #:test-declaim-stale"
                   "  (:use #:cl))"
                   "(in-package #:test-declaim-stale)"
                   ""
                   "#+mallet"
                   "(declaim (mallet:suppress-next :needless-let*))"
                   "(defun clean-function (x)"
                   "  (let* ((a (+ x 1))"
                   "         (b (* a 2)))"
                   "    (+ a b)))"
                   ""
                   "#+mallet"
                   "(declaim (mallet:suppress-next :needless-let*))"
                   "(defun suppressed-function ()"
                   "  (let* ((a 1)"
                   "         (b 2))"
                   "    (+ a b)))")))
      (with-lint-source (file violations source config)
        (declare (ignore file))
        (let ((stale-violations (remove-if-not
                                 (lambda (v) (eq :stale-suppression (violation:violation-rule v)))
                                 violations))
              (needless-violations (remove-if-not
                                    (lambda (v) (eq :needless-let* (violation:violation-rule v)))
                                    violations)))
          (ok (= 1 (length stale-violations))
              "Only clean-function's unused suppress-next produces stale-suppression")
          (ok (null needless-violations)
              "suppressed-function's needless-let* violation is consumed by suppress-next"))))))

;;; Test: stale inner-form suppress generates a stale-suppression violation

(deftest comment-suppress-inner-stale
  (testing "Inner ; mallet:suppress with no matching violation is reported as stale"
    (let* ((file (violations-fixture "comment-suppress-inner-stale.lisp"))
           (config (make-needless-let*-and-stale-config))
           (violations (engine:lint-file file :config config)))

      (let ((let*-violations (remove-if-not
                               (lambda (v) (eq :needless-let* (violation:violation-rule v)))
                               violations)))
        (ok (null let*-violations) "No needless-let* violations (form uses plain let)"))

      (let ((stale-violations (remove-if-not
                                (lambda (v) (eq :stale-suppression (violation:violation-rule v)))
                                violations)))
        (ok (= 1 (length stale-violations))
            "Exactly 1 stale-suppression violation for unused inner suppress")))))

;;; Test: U-2 — Suppress comment after the last top-level form

(deftest suppress-after-last-form
  (testing "; mallet:suppress after the last form is reported as stale"
    (let* ((file (violations-fixture "suppress-after-last-form.lisp"))
           (config (make-needless-let*-and-stale-config))
           (violations (engine:lint-file file :config config)))

      ;; No needless-let* violations (the defun body has no let*)
      (let ((let*-violations (remove-if-not
                               (lambda (v) (eq :needless-let* (violation:violation-rule v)))
                               violations)))
        (ok (null let*-violations) "No needless-let* violations in fixture"))

      ;; The dangling suppress comment must be reported as stale
      (let ((stale-violations (remove-if-not
                                (lambda (v) (eq :stale-suppression (violation:violation-rule v)))
                                violations)))
        (ok (= 1 (length stale-violations))
            "Exactly 1 stale-suppression violation for suppress after last form")))))

;;; Test: U-2 positional — suppress AFTER the violating form does not suppress it

(deftest comment-suppress-after-violation-not-suppressed
  (testing "Suppress comment placed after the violating form does not retroactively suppress it"
    (let* ((file (violations-fixture "comment-suppress-after-violation.lisp"))
           (config (make-needless-let*-config))
           (violations (engine:lint-file file :config config)))

      (let ((let*-violations (remove-if-not
                               (lambda (v) (eq :needless-let* (violation:violation-rule v)))
                               violations)))
        (ok (= 1 (length let*-violations))
            "Exactly 1 needless-let* violation (not suppressed by later suppress comment)"))))

  (testing "Suppress placed after a violation with no matching form following it is reported as stale"
    ;; The suppress sits between the let* (before it, not matched due to positional filter)
    ;; and the let (after it, no violation).  It suppresses nothing, so it must be stale.
    (let* ((file (violations-fixture "comment-suppress-after-violation.lisp"))
           (config (make-needless-let*-and-stale-config))
           (violations (engine:lint-file file :config config)))

      (let ((stale-violations (remove-if-not
                                (lambda (v) (eq :stale-suppression (violation:violation-rule v)))
                                violations)))
        (ok (= 1 (length stale-violations))
            "Exactly 1 stale-suppression violation for suppress that matched no violation")))))

;;; Test: disable form-level rule — no false stale-suppression warning

(deftest disable-form-level-rule-no-false-stale
  (testing "; mallet:disable :needless-let* suppresses the form-level violation"
    (let* ((file (no-violations-fixture "disable-form-level-rule.lisp"))
           (config (make-needless-let*-and-stale-config))
           (violations (engine:lint-file file :config config)))

      ;; The disable region should suppress the needless-let* violation
      (let ((let*-violations (remove-if-not
                               (lambda (v) (eq :needless-let* (violation:violation-rule v)))
                               violations)))
        (ok (null let*-violations)
            "No needless-let* violations (suppressed by disable region)"))

      ;; The disable directive is NOT stale — it actively suppressed a violation
      (let ((stale-violations (remove-if-not
                                (lambda (v) (eq :stale-suppression (violation:violation-rule v)))
                                violations)))
        (ok (null stale-violations)
            "No stale-suppression violation for active form-level disable region"))))

  (testing "Stale :disable for text/token rule (regression) still generates stale warning"
    (let* ((file (violations-fixture "stale-text-disable.lisp"))
           (config (make-line-length-and-stale-config))
           (violations (engine:lint-file file :config config)))

      (let ((stale-violations (remove-if-not
                                (lambda (v) (eq :stale-suppression (violation:violation-rule v)))
                                violations)))
        (ok (= 1 (length stale-violations))
            "Stale :disable for text/token rule still generates stale-suppression violation")))))

;;; Test: disable form-level rule :missing-else — no false stale-suppression warning

(deftest disable-missing-else-no-false-stale
  (testing "; mallet:disable :missing-else generates no stale-suppression violation"
    (let* ((file (no-violations-fixture "disable-missing-else.lisp"))
           (config (make-if-without-else-and-stale-config))
           (violations (engine:lint-file file :config config)))

      ;; The disable region should suppress the missing-else violation
      (let ((iwe-violations (remove-if-not
                              (lambda (v) (eq :missing-else (violation:violation-rule v)))
                              violations)))
        (ok (null iwe-violations)
            "No missing-else violations (suppressed by disable region)"))

      ;; Form-level :disable must NOT be registered in text-token-suppression-state,
      ;; so no stale-suppression false positive fires.
      (let ((stale-violations (remove-if-not
                                (lambda (v) (eq :stale-suppression (violation:violation-rule v)))
                                violations)))
        (ok (null stale-violations)
            "No stale-suppression violation for :missing-else form-level disable region")))))

;;; Test: disable :ALL wrapping a form-level violation — no false stale-suppression warning

(deftest disable-all-form-level-no-false-stale
  (testing "; mallet:disable :ALL around a form-level violation generates no stale-suppression"
    (let* ((file (no-violations-fixture "disable-all-form-level.lisp"))
           (config (make-if-without-else-and-stale-config))
           (violations (engine:lint-file file :config config)))

      ;; The :ALL disable region should suppress the missing-else violation
      (let ((iwe-violations (remove-if-not
                              (lambda (v) (eq :missing-else (violation:violation-rule v)))
                              violations)))
        (ok (null iwe-violations)
            "No missing-else violations (suppressed by :ALL disable region)"))

      ;; :ALL is not a real rule object; it must not be registered in text-token-suppression-state.
      (let ((stale-violations (remove-if-not
                                (lambda (v) (eq :stale-suppression (violation:violation-rule v)))
                                violations)))
        (ok (null stale-violations)
            "No stale-suppression violation for :ALL disable region with form-level rule")))))

;;; Test: active text/token :disable region — violation suppressed, no stale warning

(deftest active-text-disable-no-stale
  (testing "Active :disable for text/token rule produces no stale when a violation is suppressed"
    (let* ((file (no-violations-fixture "active-text-disable.lisp"))
           (config (make-line-length-and-stale-config))
           (violations (engine:lint-file file :config config)))

      ;; The disable region suppresses the long-line violation — nothing should escape
      (let ((ll-violations (remove-if-not
                             (lambda (v) (eq :line-length (violation:violation-rule v)))
                             violations)))
        (ok (null ll-violations)
            "No line-length violations (long line is inside disabled region)"))

      ;; The disable directive was used (it suppressed a real violation), so no stale warning
      (let ((stale-violations (remove-if-not
                                (lambda (v) (eq :stale-suppression (violation:violation-rule v)))
                                violations)))
        (ok (null stale-violations)
            "No stale-suppression violation when :line-length disable region was actually used")))))

;;; Test: U-3 — suppress-next declaim as last sub-form does not leak into next top-level form

(deftest suppress-next-last-subform-no-leak
  (testing "suppress-next declaim as last sub-form does not suppress violations in the next defun"
    (let* ((file (violations-fixture "suppress-next-last-subform.lisp"))
           (config (make-needless-let*-config))
           (violations (engine:lint-file file :config config)))

      (let ((let*-violations (remove-if-not
                               (lambda (v) (eq :needless-let* (violation:violation-rule v)))
                               violations)))
        (ok (= 1 (length let*-violations))
            "Exactly 1 needless-let* violation in bar (leaked suppress-next must not silence it")))))

;;; Test: ; mallet:suppress for token-level rules — form-scope semantics

(deftest token-suppress-same-line
  (testing "; mallet:suppress on same line inside the form suppresses the violation"
    (let* ((file (no-violations-fixture "token-suppress-active.lisp"))
           (config (make-double-colon-config))
           (violations (engine:lint-file file :config config)))
      (ok (null violations)
          "No double-colon-access violation when suppress comment is on the same line")))

  (testing "; mallet:suppress on token-level rule does not produce stale-suppression"
    (let* ((file (no-violations-fixture "token-suppress-active.lisp"))
           (config (make-double-colon-and-stale-config))
           (violations (engine:lint-file file :config config)))
      (let ((stale (remove-if-not
                     (lambda (v) (eq :stale-suppression (violation:violation-rule v)))
                     violations)))
        (ok (null stale)
            "No stale-suppression when suppress comment suppressed a real token-level violation"))))

  (testing "; mallet:suppress BEFORE the form suppresses all token violations within it"
    (let* ((file (no-violations-fixture "token-suppress-before-form.lisp"))
           (config (make-double-colon-config))
           (violations (engine:lint-file file :config config)))
      (ok (null violations)
          "No double-colon-access violations when suppress precedes the form")))

  (testing "; mallet:suppress BEFORE the form does not produce stale-suppression"
    (let* ((file (no-violations-fixture "token-suppress-before-form.lisp"))
           (config (make-double-colon-and-stale-config))
           (violations (engine:lint-file file :config config)))
      (let ((stale (remove-if-not
                     (lambda (v) (eq :stale-suppression (violation:violation-rule v)))
                     violations)))
        (ok (null stale)
            "No stale-suppression when before-form suppress suppressed real token violations")))))
