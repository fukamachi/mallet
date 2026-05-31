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
                #:print-help
                #:violation-rule)
  (:local-nicknames
   (#:errors #:mallet/errors)
   (#:engine #:mallet/engine)
   (#:config #:mallet/config)))
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

(deftest parse-rule-spec-colon-prefixed-name
  (testing "Colon-prefixed rule name resolves to the same result as the bare form"
    (let ((result (parse-rule-spec ":trailing-whitespace")))
      (ok (eq :trailing-whitespace (car result))
          "Keyword matches rule")
      (ok (null (cdr result))
          "No options")))
  (testing "Colon-prefixed and bare forms are equal"
    (ok (equal (parse-rule-spec "trailing-whitespace")
               (parse-rule-spec ":trailing-whitespace"))))
  (testing "Colon-prefixed rule name with options parses like the bare form"
    (ok (equal (parse-rule-spec "cyclomatic-complexity:max=15,variant=modified")
               (parse-rule-spec ":cyclomatic-complexity:max=15,variant=modified")))))

(deftest parse-rule-spec-unknown-rule-error-contains-token
  (testing "Unknown colon-prefixed rule: error message is non-blank and contains the token"
    (handler-case
        (progn (parse-rule-spec ":no-such-rule") (ok nil "Should have signaled unknown-rule"))
      (errors:unknown-rule (c)
        (let ((msg (format nil "~A" c)))
          (ok (plusp (length msg))
              "Error message is non-blank")
          (ok (search "Unknown rule:" msg)
              "Error message contains required 'Unknown rule:' prefix")
          (ok (or (search ":no-such-rule" msg)
                  (search "no-such-rule" msg))
              "Offending token appears in the error message"))))))

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

(defun write-test-source-file (dir name content)
  "Create source file NAME under DIR with CONTENT, returning its truename pathname."
  (let ((path (merge-pathnames name dir)))
    (with-open-file (out path :direction :output :if-exists :supersede)
      (write-string content out))
    (truename path)))

(defun cleanup-test-dir (dir)
  "Remove DIR and all its contents."
  (uiop:delete-directory-tree dir :validate t :if-does-not-exist :ignore))

(defun path-names (pathnames)
  "Return a sorted list of file namestrings for comparison."
  (sort (mapcar #'namestring pathnames) #'string<))

(defun result-violations (results)
  "Return all violations from lint-files RESULTS."
  (loop for (_ . violations) in results
        append violations))

(defun violation-for-file-and-rule-p (violation file rule)
  "Return true when VIOLATION belongs to FILE and RULE."
  (and (equal (namestring (truename file))
              (namestring (truename (mallet:violation-file violation))))
       (eq rule (violation-rule violation))))

(deftest directory-scan-excludes-undocumented-extensions
  (testing "Directory traversal scans only documented .lisp and .asd files, skipping undocumented extensions"
    (let* ((base (format nil "/tmp/mallet-test-~A/" (random 1000000)))
           (root (make-test-dir base)))
      (unwind-protect
           (let* ((lisp-file (write-test-source-file
                              root
                              "main.lisp"
                              (format nil "(defun bad () nil)  ~%")))
                  (asd-file (write-test-source-file
                             root
                             "system.asd"
                             (format nil "(defsystem #:bad-system~%  :components ((:file #:main)))~%")))
                  ;; Undocumented source-like extension: this .cl file carries trailing
                  ;; whitespace, so it WOULD produce a violation if directory traversal
                  ;; scanned it. The supported source extensions are .lisp and .asd.
                  (cl-file (write-test-source-file
                            root
                            "legacy.cl"
                            (format nil "(defun legacy () nil)  ~%")))
                  (config (mallet:make-config
                           :rules (list (mallet:make-rule :trailing-whitespace)
                                        (mallet:make-rule :asdf-component-strings))))
                  (files (expand-file-args (list (namestring root))))
                  (paths (path-names files))
                  (violations (result-violations
                               (mallet:lint-files files :config config))))
             (ok (member (namestring lisp-file) paths :test #'string=)
                 "documented .lisp file is included in directory traversal")
             (ok (member (namestring asd-file) paths :test #'string=)
                 "documented .asd file is included in directory traversal")
             (ok (not (member (namestring cl-file) paths :test #'string=))
                 "undocumented .cl file is excluded from directory traversal")
             (ok (some (lambda (violation)
                         (violation-for-file-and-rule-p
                          violation lisp-file :trailing-whitespace))
                       violations)
                 "trailing-whitespace violation from documented .lisp file is reported")
             (ok (some (lambda (violation)
                         (violation-for-file-and-rule-p
                          violation asd-file :asdf-component-strings))
                       violations)
                 "asdf-component-strings violation from documented .asd file is reported")
             (ok (notany (lambda (violation)
                           (equal (namestring (truename cl-file))
                                  (namestring (truename (mallet:violation-file violation)))))
                         violations)
                 "no violation is reported for the undocumented .cl file (it was skipped)"))
        (cleanup-test-dir root)))))

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

(deftest directory-scan-lints-asd-files
  (testing "Directory traversal includes .asd files and reports ASDF violations"
    (let* ((base (format nil "/tmp/mallet-test-~A/" (random 1000000)))
           (root (make-test-dir base)))
      (unwind-protect
           (let* ((asd-file (write-test-source-file
                             root
                             "system.asd"
                             (format nil "(defsystem #:bad-system~%  :components ((:file #:main)))~%")))
                  (lisp-file (write-test-source-file
                              root
                              "main.lisp"
                              (format nil "(defpackage #:bad-system/main)~%(in-package #:bad-system/main)~%")))
                  (config (mallet:make-config
                           :rules (list (mallet:make-rule :asdf-component-strings))))
                  (files (expand-file-args (list (namestring root))))
                  (violations (result-violations
                               (mallet:lint-files files :config config))))
             (ok (member (namestring lisp-file) (path-names files) :test #'string=)
                 "sibling .lisp file is included in directory traversal")
             (ok (some (lambda (violation)
                         (violation-for-file-and-rule-p
                          violation asd-file :asdf-component-strings))
                       violations)
                 "ASDF component string violation from .asd file is reported"))
        (cleanup-test-dir root)))))

(deftest directory-scan-still-lints-lisp-files
  (testing "Directory traversal still reports violations from sibling .lisp files"
    (let* ((base (format nil "/tmp/mallet-test-~A/" (random 1000000)))
           (root (make-test-dir base)))
      (unwind-protect
           (let* ((asd-file (write-test-source-file
                             root
                             "system.asd"
                             (format nil "(defsystem #:bad-system~%  :components ((:file #:main)))~%")))
                  (lisp-file (write-test-source-file
                              root
                              "main.lisp"
                              (format nil "(defun bad () nil)  ~%")))
                  (config (mallet:make-config
                           :rules (list (mallet:make-rule :trailing-whitespace))))
                  (files (expand-file-args (list (namestring root))))
                  (violations (result-violations
                               (mallet:lint-files files :config config))))
             (ok (member (namestring asd-file) (path-names files) :test #'string=)
                 "sibling .asd file is included in the mixed directory traversal")
             (ok (some (lambda (violation)
                         (violation-for-file-and-rule-p
                          violation lisp-file :trailing-whitespace))
                       violations)
                 "trailing-whitespace violation from .lisp file is reported even with a sibling .asd file"))
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

(deftest handle-preset-option-converts-token-to-keyword
  (dolist (case '(("default" :default ("file.lisp"))
                  ("all" :all nil)
                  ("none" :none nil)
                  ("strict" :strict nil)
                  ("my-ci" :my-ci nil)
                  ("MY-PRESET" :my-preset nil)
                  ("strict" :strict ("src/" "--format" "json"))
                  ("my-strict-preset" :my-strict-preset ("src/"))
                  ("v2" :v2 nil)))
    (destructuring-bind (input-string expected-keyword expected-remaining) case
      (testing (format nil "~A converts to ~S and leaves ~S"
                       input-string
                       expected-keyword
                       expected-remaining)
        (multiple-value-bind (preset remaining)
            (handle-preset-option (cons input-string expected-remaining))
          (ok (eq expected-keyword preset))
          (ok (equal expected-remaining remaining)))))))

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

;;; Tests for mutually exclusive flag detection (F3 / F4)

(deftest parse-args-fix-and-fix-dry-run-are-exclusive
  (testing "--fix followed by --fix-dry-run signals a cli-error"
    (ok (signals (parse-args '("--fix" "--fix-dry-run" "src/"))
                 'errors:cli-error)
        "--fix and --fix-dry-run together must not silently resolve by argument order"))

  (testing "error message for --fix/--fix-dry-run says 'mutually exclusive' and names both flags"
    (ok (handler-case
             (progn (parse-args '("--fix" "--fix-dry-run" "src/")) nil)
           (errors:cli-error (c)
             (let ((msg (format nil "~A" c)))
               (and (search "--fix" msg)
                    (search "--fix-dry-run" msg)
                    (search "mutually exclusive" (string-downcase msg))))))
         "error message must say 'mutually exclusive' and name both --fix and --fix-dry-run"))

  (testing "--fix-dry-run followed by --fix also signals a cli-error"
    (ok (signals (parse-args '("--fix-dry-run" "--fix" "src/"))
                 'errors:cli-error)
        "reversed flag order must also be rejected")))

(deftest parse-args-all-and-none-are-exclusive
  (testing "--all followed by --none signals a cli-error"
    (ok (signals (parse-args '("--all" "--none" "src/"))
                 'errors:cli-error)
        "--all and --none together must not silently resolve by argument order"))

  (testing "error message for --all/--none names both flags"
    (ok (handler-case
             (progn (parse-args '("--all" "--none" "src/")) nil)
           (errors:cli-error (c)
             (let ((msg (format nil "~A" c)))
               (and (search "--all" msg) (search "--none" msg)))))
         "error message must name both --all and --none"))

  (testing "--none followed by --all also signals a cli-error"
    (ok (signals (parse-args '("--none" "--all" "src/"))
                 'errors:cli-error)
        "reversed flag order must also be rejected")))

(deftest parse-args-fix-mode-single-flags
  (testing "--fix alone sets fix-mode to :fix"
    (multiple-value-bind (format config-path preset debug no-color fix-mode)
        (parse-args '("--fix" "src/"))
      (declare (ignore format config-path preset debug no-color))
      (ok (eq :fix fix-mode)
          "--fix alone must not be rejected")))

  (testing "--fix-dry-run alone sets fix-mode to :fix-dry-run"
    (multiple-value-bind (format config-path preset debug no-color fix-mode)
        (parse-args '("--fix-dry-run" "src/"))
      (declare (ignore format config-path preset debug no-color))
      (ok (eq :fix-dry-run fix-mode)
          "--fix-dry-run alone must not be rejected"))))

(deftest parse-args-end-of-options-treats-remaining-args-as-files
  (testing "dash-leading filename after -- is a file argument"
    (multiple-value-bind (format config-path preset debug no-color fix-mode
                          cli-rules fail-on init-mode force files)
        (parse-args '("--" "-dash.lisp"))
      (declare (ignore format config-path preset debug no-color fix-mode
                       cli-rules fail-on init-mode force))
      (ok (equal '("-dash.lisp") files)
          "arguments after -- must be returned as file paths")))

  (testing "--fix after -- is a file argument, not fix mode"
    (multiple-value-bind (format config-path preset debug no-color fix-mode
                          cli-rules fail-on init-mode force files)
        (parse-args '("--" "--fix" "f.lisp"))
      (declare (ignore format config-path preset debug no-color
                       cli-rules fail-on init-mode force))
      (ok (null fix-mode)
          "--fix after -- must not enable fix mode")
      (ok (equal '("--fix" "f.lisp") files)
          "all arguments after -- must be returned as file paths"))))

;;; End-to-end: colon-prefixed rule names actually enable rules and produce violations

(deftest colon-prefixed-enable-rule-lints-file
  (testing "--none --enable :trailing-whitespace reports a trailing-whitespace warning"
    (uiop:with-temporary-file (:stream out :pathname path :type "lisp" :keep t)
      (format out "(defun foo ()  ~%  (+ 1 2))")
      (finish-output out)
      (multiple-value-bind (fmt cfg-path preset dbg no-color fix cli-rules fail-on init force parsed-files)
          (parse-args (list "--none" "--enable" ":trailing-whitespace" (namestring path)))
        (declare (ignore fmt dbg no-color fix fail-on init force parsed-files))
        (let* ((base-config (load-configuration cfg-path preset nil))
               (effective-config (config:apply-cli-overrides base-config cli-rules)))
          (multiple-value-bind (violations ignored-p)
              (engine:lint-file (truename path) :config effective-config)
            (declare (ignore ignored-p))
            (ok (some (lambda (v) (eq :trailing-whitespace (violation-rule v)))
                      violations)
                "trailing-whitespace violation reported under --none --enable :trailing-whitespace"))))))

  (testing "--none --enable :trailing-whitespace and --none --enable trailing-whitespace produce identical violations"
    (uiop:with-temporary-file (:stream out :pathname path :type "lisp" :keep t)
      (format out "(defun foo ()  ~%  (+ 1 2))")
      (finish-output out)
      (flet ((violations-for-enable (spec)
               (multiple-value-bind (fmt cfg-path preset dbg no-color fix cli-rules fail-on init force parsed-files)
                   (parse-args (list "--none" "--enable" spec (namestring path)))
                 (declare (ignore fmt dbg no-color fix fail-on init force parsed-files))
                 (let* ((base-config (load-configuration cfg-path preset nil))
                        (effective-config (config:apply-cli-overrides base-config cli-rules)))
                   (multiple-value-bind (violations ignored-p)
                       (engine:lint-file (truename path) :config effective-config)
                     (declare (ignore ignored-p))
                     violations)))))
        (let ((bare-violations (violations-for-enable "trailing-whitespace"))
              (colon-violations (violations-for-enable ":trailing-whitespace")))
          (ok (= (length bare-violations) (length colon-violations))
              "Same number of violations for bare and colon-prefixed forms")
          (ok (equal (mapcar #'violation-rule bare-violations)
                     (mapcar #'violation-rule colon-violations))
              "Bare and colon-prefixed forms report the same rule violations"))))))

;;; === Exit-code contract: no-arg help path (AC: no args → help → exit 0) ===

(deftest parse-args-no-args-returns-nil-files
  (testing "No arguments returns nil file-args (triggers no-arg help path in main)"
    ;; main checks (null file-args) and calls (print-help) then (uiop:quit 0).
    ;; This unit test verifies parse-args returns nil for files when called with
    ;; no arguments, which is the mechanism that activates that branch.
    (multiple-value-bind (fmt cfg preset dbg nc fix cli-rules fail-on init-mode force files list-rules-mode)
        (parse-args '())
      (declare (ignore fmt cfg preset dbg nc fix cli-rules fail-on init-mode force list-rules-mode))
      (ok (null files)
          "parse-args with empty arg list returns nil file-args"))))

;;; === Exit-code contract: unknown flag → cli-error → exit 3 ===

(deftest parse-args-unknown-flag-signals-cli-error
  (testing "Unknown flag signals cli-error so main exits 3 (usage error)"
    ;; main catches cli-error and calls (uiop:quit 3).  This test verifies the
    ;; signaling mechanism, which is what distinguishes exit 3 from exit 2.
    (ok (handler-case
            (progn (parse-args '("--bogus-flag" "file.lisp")) nil)
          (errors:cli-error () t))
        "--bogus-flag signals cli-error")
    (ok (handler-case
            (progn (parse-args '("--bogus-flag" "file.lisp")) nil)
          (errors:unknown-option () t))
        "--bogus-flag signals specifically unknown-option (a cli-error subtype)")))

;;; Tests for --list-rules flag in parse-args

(deftest parse-args-list-rules-flag
  (testing "--list-rules activates list-rules-mode"
    ;; parse-args must set a list-rules-mode flag (12th return value) rather
    ;; than calling uiop:quit directly, so callers can test this path without
    ;; killing the test runner.
    (multiple-value-bind (format config-path preset debug no-color fix-mode
                          cli-rules fail-on init-mode force files list-rules-mode)
        (parse-args '("--list-rules"))
      (declare (ignore format config-path preset debug no-color fix-mode
                        cli-rules fail-on init-mode force files))
      (ok list-rules-mode "--list-rules must activate list-rules-mode")))

  (testing "list-rules-mode is nil when --list-rules is absent"
    (multiple-value-bind (format config-path preset debug no-color fix-mode
                          cli-rules fail-on init-mode force files list-rules-mode)
        (parse-args '("file.lisp"))
      (declare (ignore format config-path preset debug no-color fix-mode
                        cli-rules fail-on init-mode force files))
      (ok (null list-rules-mode) "list-rules-mode must default to nil"))))
