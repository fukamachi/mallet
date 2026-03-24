(defpackage #:mallet/tests/init
  (:use #:cl #:rove)
  (:local-nicknames
   (#:violation #:mallet/violation)
   (#:init #:mallet/init)))
(in-package #:mallet/tests/init)

;;; Helpers

(defun make-test-dir ()
  "Create a unique temporary directory and return its pathname."
  (let* ((name (format nil "mallet-init-test-~A" (random 1000000)))
         (tmp (or (uiop:getenv "TMPDIR") "/tmp/claude-1000/"))
         (dir (uiop:ensure-directory-pathname
               (merge-pathnames name
                                (uiop:ensure-directory-pathname tmp)))))
    (ensure-directories-exist dir)
    dir))

(defun cleanup-dir (dir)
  (uiop:delete-directory-tree dir :validate t :if-does-not-exist :ignore))

(defun write-lisp-file (dir filename content)
  "Write CONTENT to FILENAME under DIR."
  (let ((path (merge-pathnames filename dir)))
    (ensure-directories-exist (uiop:pathname-directory-pathname path))
    (with-open-file (out path :direction :output :if-exists :supersede)
      (write-string content out))
    (truename path)))

(defun expand-dir (dir)
  "Return all .lisp files under DIR as a list of pathnames."
  (let ((result '()))
    (dolist (f (uiop:directory-files dir "*.lisp"))
      (push f result))
    (nreverse result)))

(defun make-test-violation (rule-name)
  "Create a test violation for RULE-NAME."
  (make-instance 'violation:violation
                 :rule rule-name
                 :file #P"test.lisp"
                 :line 1
                 :column 0
                 :severity :warning
                 :message "test message"))

;;; Tests for aggregate-violations-by-rule

(deftest aggregate-violations-by-rule-empty
  (testing "Empty violations list returns empty list"
    (ok (null (init:aggregate-violations-by-rule '()))
        "No violations returns NIL")))

(deftest aggregate-violations-by-rule-single-rule
  (testing "Single rule aggregates correctly"
    (let* ((v1 (make-test-violation :trailing-whitespace))
           (v2 (make-test-violation :trailing-whitespace))
           (result (init:aggregate-violations-by-rule (list v1 v2))))
      (ok (= 1 (length result)) "One rule in result")
      (ok (eq :trailing-whitespace (caar result)) "Rule name matches input violation rule")
      (ok (= 2 (cdar result)) "Count matches number of violations")))
  (testing "Result reflects actual rule name from violations (not hardcoded)"
    (let* ((v1 (make-test-violation :some-unusual-rule-xyz))
           (v2 (make-test-violation :some-unusual-rule-xyz))
           (v3 (make-test-violation :some-unusual-rule-xyz))
           (result (init:aggregate-violations-by-rule (list v1 v2 v3))))
      (ok (= 1 (length result)) "One rule in result")
      (ok (eq :some-unusual-rule-xyz (caar result)) "Rule name must come from violations, not be hardcoded")
      (ok (= 3 (cdar result)) "Count is 3"))))

(deftest aggregate-violations-by-rule-multiple-rules
  (testing "Multiple rules aggregated and sorted by count descending"
    (let* ((v1 (make-test-violation :missing-else))
           (v2 (make-test-violation :trailing-whitespace))
           (v3 (make-test-violation :trailing-whitespace))
           (v4 (make-test-violation :trailing-whitespace))
           (result (init:aggregate-violations-by-rule (list v1 v2 v3 v4))))
      (ok (= 2 (length result)) "Two rules in result")
      (ok (eq :trailing-whitespace (caar result)) "Highest count rule is first")
      (ok (= 3 (cdar result)) "First rule has count 3")
      (ok (eq :missing-else (caadr result)) "Second rule follows")
      (ok (= 1 (cdadr result)) "Second rule has count 1"))))

;;; Tests for generate-init-config-string

(deftest generate-init-config-string-no-violations
  (testing "No violations generates comment"
    (let ((result (init:generate-init-config-string '() :default)))
      (ok (search "(:extends :default)" result) "Contains extends directive")
      (ok (search "No violations found" result) "Contains no-violations comment")
      ;; Negative: no-violations config must not contain any disable entries
      (ok (not (search "(:disable" result)) "Must not contain any :disable entries")))
  (testing "No violations with :strict preset"
    (let ((result (init:generate-init-config-string '() :strict)))
      (ok (search "(:extends :strict)" result) "Contains strict extends directive")
      ;; Negative: :strict result must not contain :default
      (ok (not (search "(:extends :default)" result)) "Must not contain :default extends"))))

(deftest generate-init-config-string-with-violations
  (testing "With violations generates disable entries sorted by count"
    (let* ((counts '((:trailing-whitespace . 5) (:missing-else . 2)))
           (result (init:generate-init-config-string counts :default)))
      (ok (search "(:extends :default)" result) "Contains extends directive")
      ;; Negative: :default result must not say :strict
      (ok (not (search "(:extends :strict)" result)) "Must not contain :strict extends")
      (ok (search "(:disable :trailing-whitespace)" result)
          "Contains disable for trailing-whitespace")
      (ok (search "(:disable :missing-else)" result)
          "Contains disable for missing-else")
      (let ((pos-tw (search "trailing-whitespace" result))
            (pos-me (search "missing-else" result)))
        (ok (and pos-tw pos-me (< pos-tw pos-me))
            "Higher-count rule appears first"))))
  (testing "Violations with :strict preset uses :strict extends"
    (let* ((counts '((:no-package-use . 3)))
           (result (init:generate-init-config-string counts :strict)))
      (ok (search "(:extends :strict)" result) "Contains strict extends")
      (ok (not (search "(:extends :default)" result)) "Must not contain :default extends")
      (ok (search "(:disable :no-package-use)" result) "Contains disable entry"))))

(deftest generate-init-config-string-strict-preset
  (testing "With :strict preset uses (:extends :strict)"
    (let ((result (init:generate-init-config-string '() :strict)))
      (ok (search "(:extends :strict)" result) "Contains strict extends")
      (ok (not (search "(:extends :default)" result)) "Must not bleed :default"))))

(deftest generate-init-config-string-comment-counts
  (testing "Each disable entry has a violation count comment — count 3"
    (let* ((counts '((:trailing-whitespace . 3)))
           (result (init:generate-init-config-string counts :default)))
      (ok (search "; 3 violations" result) "Contains violation count comment")))
  (testing "Count is dynamic — different count produces different comment"
    (let* ((counts '((:no-tabs . 7)))
           (result (init:generate-init-config-string counts :default)))
      (ok (search "; 7 violations" result) "Contains 7 violations comment")
      (ok (not (search "; 3 violations" result)) "Must not say 3 when count is 7"))))

;;; CLI integration tests for run-init

(deftest run-init-creates-config-file
  (testing "run-init creates .mallet.lisp with no violations for clean code"
    (let* ((dir (make-test-dir))
           (config-path (merge-pathnames ".mallet.lisp" dir)))
      (unwind-protect
           (progn
             (write-lisp-file dir "test.lisp"
                              "(defpackage #:test (:use #:cl))
(in-package #:test)

(defun foo () t)
")
             (let ((files (expand-dir dir)))
               (init:run-init (list (namestring dir)) files
                              :preset :default :force nil))
             (ok (probe-file config-path) ".mallet.lisp was created")
             (let ((content (uiop:read-file-string config-path)))
               (ok (search "(:extends :default)" content)
                   "Config has extends :default")
               ;; Clean code → no disable entries
               (ok (not (search "(:disable" content))
                   "Clean code must not produce any :disable entries")))
        (cleanup-dir dir))))
  (testing "run-init reflects actual violations from scanned files"
    (let* ((dir (make-test-dir))
           (config-path (merge-pathnames ".mallet.lisp" dir)))
      (unwind-protect
           (progn
             ;; Write code without a final newline to trigger :missing-final-newline
             (write-lisp-file dir "bad.lisp"
                              "(defpackage #:test (:use #:cl))
(in-package #:test)
(defun foo () t)")  ; intentionally no trailing newline
             (let ((files (expand-dir dir)))
               (init:run-init (list (namestring dir)) files
                              :preset :default :force nil))
             (ok (probe-file config-path) ".mallet.lisp was created")
             (let ((content (uiop:read-file-string config-path)))
               (ok (search "(:extends :default)" content) "Has extends :default")
               ;; The file has no final newline — :missing-final-newline must be disabled
               (ok (search "(:disable :missing-final-newline)" content)
                   "Missing-final-newline violation must appear as a :disable entry")))
        (cleanup-dir dir))))
  (testing "run-init detects form-based violations requiring AST parsing"
    (let* ((dir (make-test-dir))
           (config-path (merge-pathnames ".mallet.lisp" dir)))
      (unwind-protect
           (progn
             ;; Write code with ignore-errors to trigger :no-ignore-errors (form-based rule, in :default)
             (write-lisp-file dir "risky.lisp"
                              "(defpackage #:test (:use #:cl))
(in-package #:test)

(defun risky-fn ()
  (ignore-errors (do-something)))
")
             (let ((files (expand-dir dir)))
               (init:run-init (list (namestring dir)) files
                              :preset :default :force nil))
             (ok (probe-file config-path) ".mallet.lisp was created")
             (let ((content (uiop:read-file-string config-path)))
               (ok (search "(:extends :default)" content) "Has extends :default")
               ;; :no-ignore-errors is a form-based rule — requires real AST parsing
               (ok (search "(:disable :no-ignore-errors)" content)
                   ":no-ignore-errors must appear — requires real AST parsing, not text scan")))
        (cleanup-dir dir))))
  (testing "run-init does NOT flag (ignore-errors ...) in comments (requires AST, not text scan)"
    (let* ((dir (make-test-dir))
           (config-path (merge-pathnames ".mallet.lisp" dir)))
      (unwind-protect
           (progn
             ;; (ignore-errors ...) appears only in comments and strings — must NOT fire
             (write-lisp-file dir "safe.lisp"
                              "(defpackage #:test (:use #:cl))
(in-package #:test)

;; Do not use (ignore-errors (dangerous-thing)) in production
(defvar *example* \"Use (ignore-errors x) with care\")

(defun safe-fn ()
  t)
")
             (let ((files (expand-dir dir)))
               (init:run-init (list (namestring dir)) files
                              :preset :default :force nil))
             (ok (probe-file config-path) ".mallet.lisp was created")
             (let ((content (uiop:read-file-string config-path)))
               ;; Text-scan stubs would incorrectly flag this — real parser must not
               (ok (not (search "(:disable :no-ignore-errors)" content))
                   "ignore-errors in comments/strings must NOT produce a :disable entry")))
        (cleanup-dir dir))))
  (testing "run-init preset sensitivity: :strict-only rule appears with :strict, absent with :default"
    (let* ((dir (make-test-dir))
           (config-strict (merge-pathnames ".mallet.lisp" dir))
           ;; Code with redundant progn — only caught by :strict, not :default
           ;; Also includes (progn (x)) in a comment — text-scan stub would incorrectly flag it
           (code "(defpackage #:test (:use #:cl))
(in-package #:test)

;; Avoid (progn (single)) patterns
(defvar *note* \"(progn (one)) is redundant\")

(defun foo ()
  (progn (bar)))
"))
      (unwind-protect
           (progn
             (write-lisp-file dir "code.lisp" code)
             (let ((files (expand-dir dir)))
               ;; Run with :default — :redundant-progn is NOT in :default, should not appear
               (init:run-init (list (namestring dir)) files
                              :preset :default :force nil)
               (let ((content (uiop:read-file-string config-strict)))
                 (ok (not (search "(:disable :redundant-progn)" content))
                     ":redundant-progn must NOT appear when using :default preset"))
               ;; Now re-run with :strict — :redundant-progn IS in :strict, must appear
               (init:run-init (list (namestring dir)) files
                              :preset :strict :force t)
               (let ((content (uiop:read-file-string config-strict)))
                 (ok (search "(:extends :strict)" content) "Has extends :strict")
                 (ok (search "(:disable :redundant-progn)" content)
                     ":redundant-progn must appear when using :strict preset"))))
        (cleanup-dir dir)))))

(deftest run-init-errors-if-exists
  (testing "run-init signals error if .mallet.lisp already exists and force is nil"
    (let* ((dir (make-test-dir))
           (config-path (merge-pathnames ".mallet.lisp" dir)))
      (unwind-protect
           (progn
             (let ((f (write-lisp-file dir "test.lisp" "(defun foo () t)")))
               (with-open-file (out config-path :direction :output :if-exists :supersede)
                 (write-string "(:mallet-config (:extends :default))" out))
               (ok (signals (init:run-init (list (namestring dir)) (list f)
                                 :preset :default :force nil)
                       'error)
                   "Signals error when config already exists")))
        (cleanup-dir dir)))))

(deftest run-init-force-overwrites
  (testing "run-init with :force t overwrites existing .mallet.lisp"
    (let* ((dir (make-test-dir))
           (config-path (merge-pathnames ".mallet.lisp" dir)))
      (unwind-protect
           (progn
             (let ((f (write-lisp-file dir "test.lisp"
                                       "(defpackage #:test (:use #:cl))
(in-package #:test)
")))
               (with-open-file (out config-path :direction :output :if-exists :supersede)
                 (write-string "old content" out))
               (init:run-init (list (namestring dir)) (list f)
                              :preset :default :force t)
               (let ((content (uiop:read-file-string config-path)))
                 (ok (search "(:extends :default)" content)
                     "Config was overwritten with correct content"))))
        (cleanup-dir dir)))))
