(defpackage #:mallet/tests/cli-parsing
  (:use #:cl #:rove)
  (:import-from #:mallet
                #:parse-option-value
                #:parse-rule-options
                #:parse-rule-name
                #:parse-rule-spec
                #:should-fail-p
                #:expand-file-args
                #:handle-preset-option
                #:load-configuration
                #:parse-args
                #:print-help)
  (:local-nicknames
   (#:errors #:mallet/errors)))
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
  (testing "Standard excluded directories like .git, .qlot, .cache are skipped"
    (let* ((base (format nil "/tmp/mallet-test-~A/" (random 1000000)))
           (root (make-test-dir base))
           (src (make-test-dir base "src"))
           (git (make-test-dir base ".git"))
           (qlot (make-test-dir base ".qlot"))
           (cache (make-test-dir base ".cache")))
      (unwind-protect
           (let ((src-file (write-test-lisp-file src "main.lisp"))
                 (git-file (write-test-lisp-file git "hook.lisp"))
                 (qlot-file (write-test-lisp-file qlot "dep.lisp"))
                 (cache-file (write-test-lisp-file cache "cached.lisp")))
             (let ((result (expand-file-args (list (namestring root)))))
               (ok (member (namestring src-file) (path-names result) :test #'string=)
                   "src/ file is included")
               (ok (not (member (namestring git-file) (path-names result) :test #'string=))
                   ".git/ file is excluded")
               (ok (not (member (namestring qlot-file) (path-names result) :test #'string=))
                   ".qlot/ file is excluded")
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

;;; Tests for --strict flag repurposing

(deftest strict-flag-sets-preset
  (testing "--strict sets preset to :strict"
    (multiple-value-bind (format config-path preset debug no-color fix-mode cli-rules fail-on init-mode force files)
        (parse-args '("--strict" "file.lisp"))
      (declare (ignore format config-path debug no-color fix-mode cli-rules files))
      (ok (eq :strict preset) "--strict should set preset to :strict")
      (ok (eq :warning fail-on) "--strict should not change fail-on from :warning default")
      (ok (null init-mode) "--strict should not set init-mode")
      (ok (null force) "--strict should not set force")))

  (testing "--strict does not set fail-on to :info"
    (multiple-value-bind (format config-path preset debug no-color fix-mode cli-rules fail-on init-mode force files)
        (parse-args '("--strict" "file.lisp"))
      (declare (ignore format config-path preset debug no-color fix-mode cli-rules files))
      (ok (not (eq :info fail-on)) "--strict must not alias --fail-on info")
      (ok (null init-mode) "--strict should not set init-mode")
      (ok (null force) "--strict should not set force"))))

;;; Tests for handle-preset-option

(deftest handle-preset-option-built-ins
  (testing "Built-in 'default' returns :default"
    (multiple-value-bind (preset remaining)
        (handle-preset-option '("default" "file.lisp"))
      (ok (eq :default preset))
      (ok (equal '("file.lisp") remaining))))

  (testing "Built-in 'all' returns :all"
    (multiple-value-bind (preset remaining)
        (handle-preset-option '("all"))
      (ok (eq :all preset))
      (ok (null remaining))))

  (testing "Built-in 'none' returns :none"
    (multiple-value-bind (preset remaining)
        (handle-preset-option '("none"))
      (ok (eq :none preset))
      (ok (null remaining)))))

(deftest handle-preset-option-user-defined
  (testing "User-defined name 'strict' returns :strict keyword"
    (multiple-value-bind (preset remaining)
        (handle-preset-option '("strict"))
      (ok (eq :strict preset))
      (ok (null remaining))))

  (testing "User-defined name 'my-ci' returns :my-ci keyword"
    (multiple-value-bind (preset _)
        (handle-preset-option '("my-ci"))
      (declare (ignore _))
      (ok (eq :my-ci preset))))

  (testing "User-defined name is uppercased to keyword"
    (multiple-value-bind (preset _)
        (handle-preset-option '("MY-PRESET"))
      (declare (ignore _))
      (ok (eq :my-preset preset))))

  (testing "Remaining args are returned unchanged"
    (multiple-value-bind (preset remaining)
        (handle-preset-option '("strict" "src/" "--format" "json"))
      (ok (eq :strict preset))
      (ok (equal '("src/" "--format" "json") remaining)))))

(deftest handle-preset-option-errors
  (testing "Missing preset name signals missing-option-value"
    (ok (handler-case
            (progn (handle-preset-option '()) nil)
          (errors:missing-option-value () t))))

  (testing "Missing-option-value for --preset has option name"
    (handler-case
        (handle-preset-option '())
      (errors:missing-option-value (c)
        (ok (search "--preset" (format nil "~A" c)))))))

;;; Tests for load-configuration with user-defined presets

(deftest load-configuration-builtin-no-config
  (testing "Built-in :default with no config file works"
    (let ((cfg (load-configuration nil :default nil)))
      (ok (typep cfg 'mallet/config:config))))

  (testing "Built-in :all with no config file works"
    (let ((cfg (load-configuration nil :all nil)))
      (ok (typep cfg 'mallet/config:config))))

  (testing "Built-in :none with no config file works"
    (let ((cfg (load-configuration nil :none nil)))
      (ok (typep cfg 'mallet/config:config)))))

(deftest load-configuration-user-defined-no-config
  (testing "User-defined preset with no config file signals an error"
    (ok (handler-case
            (progn (load-configuration nil :my-ci nil) nil)
          (error () t))))

  (testing "Error message mentions the preset name"
    (handler-case
        (load-configuration nil :my-preset nil)
      (error (c)
        (ok (search "my-preset" (string-downcase (format nil "~A" c)))))))

  (testing "Error message mentions .mallet.lisp"
    (handler-case
        (load-configuration nil :my-ci nil)
      (error (c)
        (let ((msg (string-downcase (format nil "~A" c))))
          (ok (or (search ".mallet.lisp" msg)
                  (search "no" msg))))))))

;;; Tests for help text mentioning user-defined presets

(deftest print-help-mentions-user-defined-presets
  (testing "Help text mentions user-defined presets"
    (let ((output (with-output-to-string (s)
                    (let ((*standard-output* s))
                      (print-help)))))
      (ok (or (search "user-defined" (string-downcase output))
              (search ".mallet.lisp" (string-downcase output))))))

  (testing "--preset option description is present"
    (let ((output (with-output-to-string (s)
                    (let ((*standard-output* s))
                      (print-help)))))
      (ok (search "--preset" output)))))

;;; Edge case tests for --preset

(deftest handle-preset-option-hyphenated-name
  (testing "Hyphenated preset name 'my-strict-preset' returns correct keyword"
    (multiple-value-bind (preset remaining)
        (handle-preset-option '("my-strict-preset" "src/"))
      (ok (eq :my-strict-preset preset))
      (ok (equal '("src/") remaining)))))

(deftest handle-preset-option-numeric-name
  (testing "Numeric preset name 'v2' returns :v2 keyword"
    (multiple-value-bind (preset remaining)
        (handle-preset-option '("v2"))
      (ok (eq :v2 preset))
      (ok (null remaining)))))

(deftest parse-args-all-alias-sets-preset
  (testing "--all sets preset to :all"
    (multiple-value-bind (format config-path preset)
        (parse-args '("--all" "src/"))
      (declare (ignore format config-path))
      (ok (eq :all preset))))

  (testing "-a sets preset to :all"
    (multiple-value-bind (format config-path preset)
        (parse-args '("-a" "src/"))
      (declare (ignore format config-path))
      (ok (eq :all preset)))))

(deftest parse-args-none-alias-sets-preset
  (testing "--none sets preset to :none"
    (multiple-value-bind (format config-path preset)
        (parse-args '("--none" "src/"))
      (declare (ignore format config-path))
      (ok (eq :none preset)))))

(deftest load-configuration-config-file-with-preset-override
  (testing "Explicit config file with --preset override passes preset to load-config"
    ;; Create a minimal .mallet.lisp config file
    (uiop:with-temporary-file (:stream out :pathname config-path :type "lisp" :keep t)
      (write-string "(:mallet-config (:extends :default))" out)
      (finish-output out)
      (let ((cfg (load-configuration (namestring config-path) :all nil)))
        ;; Should succeed and return a config (preset-override applied by load-config)
        (ok (typep cfg 'mallet/config:config))))))

(deftest load-configuration-error-message-content
  (testing "Error for non-built-in preset without config mentions .mallet.lisp"
    (handler-case
        (load-configuration nil :custom-rules nil)
      (error (c)
        (let ((msg (string-downcase (format nil "~A" c))))
          (ok (search ".mallet.lisp" msg)
              "error message mentions .mallet.lisp"))))))

(deftest load-configuration-nil-preset-defaults-to-default
  (testing "nil preset without config file uses :default built-in"
    (let ((cfg (load-configuration nil nil nil)))
      (ok (typep cfg 'mallet/config:config)
          "returns a valid config even with nil preset"))))

