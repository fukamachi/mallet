(defpackage #:mallet/tests/cli-parsing
  (:use #:cl #:rove)
  (:import-from #:mallet
                #:parse-option-value
                #:parse-rule-options
                #:parse-rule-name
                #:parse-rule-spec
                #:should-fail-p
                #:expand-file-args))
(in-package #:mallet/tests/cli-parsing)

;;; Tests for parse-option-value

(deftest parse-option-value-integer
  (testing "Parse integer values"
    (ok (= 15 (parse-option-value "15")))
    (ok (= 100 (parse-option-value "100")))
    (ok (= 0 (parse-option-value "0")))))

(deftest parse-option-value-keyword
  (testing "Parse keyword values"
    (ok (eq :modified (parse-option-value "modified")))
    (ok (eq :standard (parse-option-value "standard")))
    (ok (eq :foo (parse-option-value "foo")))))

(deftest parse-option-value-string
  (testing "Parse string values (fallback)"
    ;; Strings starting with alpha become keywords
    (ok (eq :foo-bar (parse-option-value "foo-bar")))
    ;; Negative numbers and special chars stay as strings
    (ok (string= "-123" (parse-option-value "-123")))))

;;; Tests for parse-rule-options

(deftest parse-rule-options-single
  (testing "Parse single option"
    (let ((result (parse-rule-options "max=15")))
      (ok (equal '(:max 15) result)))))

(deftest parse-rule-options-multiple
  (testing "Parse multiple options"
    (let ((result (parse-rule-options "max=15,variant=modified")))
      (ok (equal '(:max 15 :variant :modified) result)))))

(deftest parse-rule-options-mixed-types
  (testing "Parse mixed option types"
    (let ((result (parse-rule-options "max=100,variant=standard,foo=bar")))
      (ok (equal '(:max 100 :variant :standard :foo :bar) result)))))

(deftest parse-rule-options-invalid
  (testing "Invalid option syntax"
    (ok (signals (parse-rule-options "invalid")
            'mallet/errors:invalid-rule-option))
    (ok (signals (parse-rule-options "max")
            'mallet/errors:invalid-rule-option))))

;;; Tests for parse-rule-name

(deftest parse-rule-name-valid
  (testing "Parse valid rule names"
    (ok (eq :cyclomatic-complexity (parse-rule-name "cyclomatic-complexity")))
    (ok (eq :line-length (parse-rule-name "line-length")))
    (ok (eq :trailing-whitespace (parse-rule-name "trailing-whitespace")))))

(deftest parse-rule-name-invalid
  (testing "Invalid rule name"
    (ok (signals (parse-rule-name "nonexistent-rule")
            'mallet/errors:unknown-rule))))

;;; Tests for parse-rule-spec

(deftest parse-rule-spec-no-options
  (testing "Parse rule spec without options"
    (let ((result (parse-rule-spec "cyclomatic-complexity")))
      (ok (eq :cyclomatic-complexity (car result)))
      (ok (null (cdr result))))))

(deftest parse-rule-spec-with-options
  (testing "Parse rule spec with options"
    (let ((result (parse-rule-spec "cyclomatic-complexity:max=15")))
      (ok (eq :cyclomatic-complexity (car result)))
      (ok (equal '(:max 15) (cdr result))))))

(deftest parse-rule-spec-multiple-options
  (testing "Parse rule spec with multiple options"
    (let ((result (parse-rule-spec "cyclomatic-complexity:max=15,variant=modified")))
      (ok (eq :cyclomatic-complexity (car result)))
      (ok (equal '(:max 15 :variant :modified) (cdr result))))))

;;; Tests for should-fail-p

(deftest should-fail-p-fail-on-error
  (testing "fail-on :error: only fail when there are errors"
    (ok (should-fail-p :error t t t))     ; errors present
    (ok (not (should-fail-p :error nil t t)))  ; no errors, only warnings
    (ok (not (should-fail-p :error nil nil t))) ; no errors, only info
    (ok (not (should-fail-p :error nil nil nil)))))  ; no violations

(deftest should-fail-p-fail-on-warning
  (testing "fail-on :warning: fail when there are errors or warnings"
    (ok (should-fail-p :warning t t t))     ; errors
    (ok (should-fail-p :warning nil t t))   ; warnings
    (ok (not (should-fail-p :warning nil nil t)))  ; only info
    (ok (not (should-fail-p :warning nil nil nil)))))  ; no violations

(deftest should-fail-p-fail-on-info
  (testing "fail-on :info: fail when there are any violations"
    (ok (should-fail-p :info t t t))     ; errors
    (ok (should-fail-p :info nil t t))   ; warnings
    (ok (should-fail-p :info nil nil t)) ; any violations
    (ok (not (should-fail-p :info nil nil nil)))))  ; no violations

;;; Tests for expand-file-args directory exclusion logic

(defun make-test-dir (base &rest parts)
  "Create a directory under BASE from PARTS path components, returning its pathname."
  (let ((path (uiop:ensure-directory-pathname
               (apply #'concatenate 'string base
                      (mapcar (lambda (p) (concatenate 'string p "/")) parts)))))
    (ensure-directories-exist path)
    path))

(defun write-test-lisp-file (dir name)
  "Create an empty .lisp file named NAME under DIR, returning its truename pathname.
Returns truename so comparisons work on macOS where /tmp -> /private/tmp."
  (let ((path (merge-pathnames name dir)))
    (with-open-file (out path :direction :output :if-exists :supersede)
      (write-string ";; test\n" out))
    (truename path)))

(defun cleanup-test-dir (dir)
  "Remove DIR and all its contents."
  (uiop:delete-directory-tree dir :validate t :if-does-not-exist :ignore))

(defun path-names (pathnames)
  "Return a sorted list of file namestrings for comparison."
  (sort (mapcar #'namestring pathnames) #'string<))

(deftest expand-file-args-basic-directory
  (testing "Scanning a plain directory returns all .lisp files"
    (let* ((base (format nil "/tmp/mallet-test-~A/" (random 1000000)))
           (root (make-test-dir base))
           (src (make-test-dir base "src")))
      (unwind-protect
           (let ((f1 (write-test-lisp-file root "top.lisp"))
                 (f2 (write-test-lisp-file src "src.lisp")))
             (let ((result (expand-file-args (list (namestring root)))))
               (ok (member (namestring f1) (path-names result) :test #'string=)
                   "top-level file is included")
               (ok (member (namestring f2) (path-names result) :test #'string=)
                   "file in src/ subdirectory is included")))
        (cleanup-test-dir root)))))

(deftest expand-file-args-excludes-dot-claude-when-scanning-parent
  (testing "Files under .claude/ are excluded when scanning from the parent project root"
    (let* ((base (format nil "/tmp/mallet-test-~A/" (random 1000000)))
           (root (make-test-dir base))
           (src (make-test-dir base "src"))
           (claude (make-test-dir base ".claude"))
           (worktree (make-test-dir base ".claude" "worktrees" "agent-x" "src")))
      (unwind-protect
           (let ((src-file (write-test-lisp-file src "project.lisp"))
                 (claude-file (write-test-lisp-file claude "config.lisp"))
                 (worktree-file (write-test-lisp-file worktree "foo.lisp")))
             (let ((result (expand-file-args (list (namestring root)))))
               (ok (member (namestring src-file) (path-names result) :test #'string=)
                   "src/ file is included")
               (ok (not (member (namestring claude-file) (path-names result) :test #'string=))
                   ".claude/ file is excluded")
               (ok (not (member (namestring worktree-file) (path-names result) :test #'string=))
                   "worktree file under .claude/ is excluded")))
        (cleanup-test-dir root)))))

(deftest expand-file-args-worktree-not-excluded-when-scanning-from-within
  (testing "Files inside a worktree are NOT excluded when scanning from the worktree root"
    ;; The worktree directory itself is under .claude/worktrees/ in the parent project,
    ;; but when mallet is invoked from inside the worktree, files within it must be found.
    (let* ((base (format nil "/tmp/mallet-test-~A/" (random 1000000)))
           ;; Simulate: parent/.claude/worktrees/agent-x/ is the worktree root
           (worktree-root (make-test-dir base ".claude" "worktrees" "agent-x"))
           (worktree-src (make-test-dir base ".claude" "worktrees" "agent-x" "src")))
      (unwind-protect
           (let ((f1 (write-test-lisp-file worktree-root "main.lisp"))
                 (f2 (write-test-lisp-file worktree-src "impl.lisp")))
             ;; Scan from the worktree root (not the parent project root)
             (let ((result (expand-file-args (list (namestring worktree-root)))))
               (ok (member (namestring f1) (path-names result) :test #'string=)
                   "worktree root file is included when scanning from worktree root")
               (ok (member (namestring f2) (path-names result) :test #'string=)
                   "worktree src/ file is included when scanning from worktree root")))
        (cleanup-test-dir (uiop:ensure-directory-pathname
                           (concatenate 'string base ".claude/worktrees/agent-x/")))
        (cleanup-test-dir (uiop:ensure-directory-pathname base))))))

(deftest expand-file-args-excludes-standard-dirs
  (testing "Standard excluded directories like .git, .qlot, target, .cache are skipped"
    (let* ((base (format nil "/tmp/mallet-test-~A/" (random 1000000)))
           (root (make-test-dir base))
           (src (make-test-dir base "src"))
           (git (make-test-dir base ".git"))
           (qlot (make-test-dir base ".qlot"))
           (target (make-test-dir base "target"))
           (cache (make-test-dir base ".cache")))
      (unwind-protect
           (let ((src-file (write-test-lisp-file src "main.lisp"))
                 (git-file (write-test-lisp-file git "hook.lisp"))
                 (qlot-file (write-test-lisp-file qlot "dep.lisp"))
                 (target-file (write-test-lisp-file target "compiled.lisp"))
                 (cache-file (write-test-lisp-file cache "cached.lisp")))
             (let ((result (expand-file-args (list (namestring root)))))
               (ok (member (namestring src-file) (path-names result) :test #'string=)
                   "src/ file is included")
               (ok (not (member (namestring git-file) (path-names result) :test #'string=))
                   ".git/ file is excluded")
               (ok (not (member (namestring qlot-file) (path-names result) :test #'string=))
                   ".qlot/ file is excluded")
               (ok (not (member (namestring target-file) (path-names result) :test #'string=))
                   "target/ file is excluded")
               (ok (not (member (namestring cache-file) (path-names result) :test #'string=))
                   ".cache/ file is excluded")))
        (cleanup-test-dir root)))))

(deftest expand-file-args-single-file
  (testing "A single file path returns just that file"
    (uiop:with-temporary-file (:stream out :pathname path :type "lisp" :keep t)
      (write-string ";; test\n" out)
      (finish-output out)
      (let ((result (expand-file-args (list (namestring path)))))
        (ok (= 1 (length result)) "exactly one file returned")
        (ok (string= (namestring (truename path)) (namestring (first result)))
            "returned file matches input")))))

