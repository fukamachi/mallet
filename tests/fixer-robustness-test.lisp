(defpackage #:mallet/tests/fixer-robustness
  (:use #:cl
        #:rove)
  (:local-nicknames
   (#:fixer #:mallet/fixer)
   (#:violation #:mallet/violation)))
(in-package #:mallet/tests/fixer-robustness)

;;; --- Helpers ---

(defun make-replace-violation (file line new-content)
  (make-instance 'violation:violation
                 :rule :trailing-whitespace
                 :file file
                 :line line
                 :column 0
                 :severity :info
                 :message "trailing whitespace"
                 :fix (violation:make-violation-fix
                       :type :replace-line
                       :line-number line
                       :replacement-content new-content)))

(defun chmod-file-if-present (path mode)
  "Set PATH permissions when possible, ignoring missing-file cleanup failures."
  (handler-case
      (sb-posix:chmod path mode)
    (sb-posix:syscall-error () nil)))

(defun delete-file-if-present (path)
  "Delete PATH when present, ignoring cleanup failures."
  (handler-case
      (delete-file path)
    (file-error () nil)))

(defun remove-directory-if-present (path)
  "Remove PATH when present, ignoring cleanup failures."
  (handler-case
      (sb-posix:rmdir path)
    (sb-posix:syscall-error () nil)))

;;; === AC1: Basic correctness - trailing whitespace fix leaves other content intact ===
;;;
;;; This is a regression test: the atomic-write refactoring must not corrupt the
;;; file content.  Four-line file; lines 1 and 3 have trailing spaces; lines 2
;;; and 4 must be byte-identical after the fix.

(deftest fixer-basic-fix-leaves-other-content-unchanged-test
  (testing "Applying --fix removes only trailing whitespace; all other bytes unchanged"
    ;; One violation on line 1 only; lines 2-4 must survive bit-for-bit.
    (let ((test-file (merge-pathnames "mallet-basic-fix-ac1-test.lisp" (uiop:temporary-directory)))
          (original (format nil "first line   ~%unchanged two~%unchanged three~%unchanged four~%"))
          (expected (format nil "first line~%unchanged two~%unchanged three~%unchanged four~%")))
      (unwind-protect
          (progn
            (with-open-file (s test-file :direction :output
                                          :if-exists :supersede
                                          :if-does-not-exist :create)
              (write-string original s))
            (fixer:apply-fixes
             (list (make-replace-violation test-file 1 "first line")))
            (ok (string= (uiop:read-file-string test-file) expected)
                "Trailing whitespace removed from line 1; lines 2-4 byte-identical"))
        (delete-file-if-present test-file)))))

(deftest fixer-atomic-write-preserves-permissions-test
  (testing "Atomic write preserves existing file permission bits"
    (let ((test-file (merge-pathnames "mallet-permission-preserve-test.lisp"
                                      (uiop:temporary-directory)))
          (original "line with trailing   ")
          (expected (format nil "line with trailing~%")))
      (unwind-protect
          (progn
            (with-open-file (s test-file :direction :output
                                         :if-exists :supersede
                                         :if-does-not-exist :create)
              (write-string original s))
            (sb-posix:chmod test-file #o755)
            (fixer:apply-fixes
             (list (make-replace-violation test-file 1 "line with trailing")))
            (ok (string= (uiop:read-file-string test-file) expected)
                "File content was fixed")
            (ok (= #o755 (logand (sb-posix:stat-mode (sb-posix:stat test-file))
                                  #o7777))
                "File permission bits were preserved after atomic rename"))
        (chmod-file-if-present test-file #o644)
        (delete-file-if-present test-file)))))

;;; === AC2: Atomic write - original intact on failure ===
;;;
;;; An atomic implementation writes to a temp file then renames.  Creating a
;;; temp file in the target directory requires directory write permission.
;;; The current non-atomic implementation opens the existing target file with
;;; O_TRUNC (needs only file write permission, not directory write).
;;;
;;; Setting chmod 555 on the directory therefore distinguishes the two:
;;;   non-atomic -> write succeeds  -> file has new content -> assertion FAILS
;;;   atomic     -> temp create fails -> original intact    -> assertion PASSES

(deftest fixer-atomic-write-test
  (testing "Original file is intact when write fails due to non-writable directory"
    (let* ((test-dir (merge-pathnames "mallet-atomic-test-dir/" (uiop:temporary-directory)))
           (test-file (merge-pathnames "target.lisp" test-dir))
           (original "original content"))
      (unwind-protect
          (progn
            ;; Clean up any leftover state from prior run.
            (chmod-file-if-present test-dir #o755)
            (delete-file-if-present test-file)
            (remove-directory-if-present test-dir)

            (ensure-directories-exist test-dir)
            (with-open-file (s test-file :direction :output
                                          :if-exists :supersede
                                          :if-does-not-exist :create)
              (write-string original s))

            ;; Remove directory write permission.
            (sb-posix:chmod test-dir #o555)

            ;; Attempt fix.  With atomic write the temp-file create fails.
            ;; With non-atomic write (O_TRUNC) the file is silently overwritten.
            (handler-case
                (fixer:apply-fixes-to-file
                 test-file
                 (list (make-replace-violation test-file 1 "modified content"))
                 :dry-run nil)
              (error () nil))

            ;; Restore before reading.
            (sb-posix:chmod test-dir #o755)

            (ok (string= (uiop:read-file-string test-file) original)
                "Original file content is fully intact after failed write"))
        ;; Cleanup
        (chmod-file-if-present test-dir #o755)
        (delete-file-if-present test-file)
        (remove-directory-if-present test-dir)))))

;;; === AC3: Multi-file --fix continues past a read-only file ===

(deftest fixer-continues-past-write-error-test
  (testing "apply-fixes continues fixing writable files when another file cannot be written"
    (let ((writable (merge-pathnames "mallet-test-robustness-writable.lisp" (uiop:temporary-directory)))
          (readonly (merge-pathnames "mallet-test-robustness-readonly.lisp" (uiop:temporary-directory)))
          (original "line with trailing   "))
      (unwind-protect
          (progn
            (dolist (p (list writable readonly))
              (chmod-file-if-present p #o644)
              (with-open-file (s p :direction :output :if-exists :supersede
                                   :if-does-not-exist :create)
                (write-string original s)))
            (sb-posix:chmod readonly #o444)

            (let ((violations
                    (list (make-replace-violation writable 1 "line with trailing")
                          (make-replace-violation readonly 1 "line with trailing")))
                  (error-raised nil)
                  (result-unfixed nil)
                  (result-write-error-p nil))
              (handler-case
                  (multiple-value-bind (fixed-count fixed-violations unfixed-violations write-error-p)
                      (fixer:apply-fixes violations)
                    (declare (ignore fixed-count fixed-violations))
                    (setf result-unfixed unfixed-violations
                          result-write-error-p write-error-p))
                (error () (setf error-raised t)))

              ;; Must not propagate an unhandled error.
              (ok (not error-raised)
                  "apply-fixes did not abort with an unhandled error")

              ;; Writable file must be fixed.
              (ok (string= (uiop:read-file-string writable)
                            (format nil "line with trailing~%"))
                  "Writable file was fixed despite read-only peer")

              ;; Read-only file must be unchanged.
              (ok (string= (uiop:read-file-string readonly) original)
                  "Read-only file was not modified")

              ;; The failed file's violations must surface as unfixed so the
              ;; caller can set a non-zero exit code.  A stub that silently
              ;; swallows the error and claims success would fail this check.
              (ok (plusp (length result-unfixed))
                  "Write-error violation recorded as unfixed, enabling non-zero exit")

              ;; write-error-p must be T so callers can distinguish write
              ;; failures from clean runs without inspecting unfixed-violations.
              (ok result-write-error-p
                  "write-error-p is T when a file write fails")))
        (chmod-file-if-present readonly #o644)
        (delete-file-if-present writable)
        (delete-file-if-present readonly)))))

;;; === AC3-EXIT: Write failure causes exit 2 (not exit 1) ===
;;;
;;; AC3 requires "finishes with a non-zero exit."  The default --fail-on is
;;; :warning, so `should-fail-p :warning has-errors has-warnings _` fires only
;;; when has-errors or has-warnings is non-nil.  A stub that re-adds the
;;; original :info violations to unfixed-violations satisfies the
;;; "unfixed non-empty" gate in fixer-continues-past-write-error-test but still
;;; exits 0 under the default threshold.  This test closes that gap by calling
;;; process-fix-mode directly and checking (or has-errors has-warnings).
;;;
;;; However, the exit-code contract requires exit 2 for I/O failures, distinct
;;; from exit 1 (plain violations).  A stub that surfaces write errors only as
;;; ordinary :warning violations passes the non-zero check but exits 1, not 2.
;;; The io-error-p assertion below checks the 4th return value that
;;; process-fix-mode must return so main can select exit 2 instead of exit 1.

(deftest fixer-write-error-exits-nonzero-test
  (testing "Write failure causes process-fix-mode to return non-zero exit indication"
    (let* ((readonly (merge-pathnames "mallet-test-exit-nonzero-readonly.lisp" (uiop:temporary-directory)))
           (violations (list (make-replace-violation readonly 1 "fixed")))
           (exit-nonzero nil)
           (io-error-p nil))
      (unwind-protect
          (progn
            (chmod-file-if-present readonly #o644)
            (with-open-file (s readonly :direction :output :if-exists :supersede
                                         :if-does-not-exist :create)
              (write-string "content" s))
            (sb-posix:chmod readonly #o444)

            ;; Call process-fix-mode, capturing stdout so test output is clean.
            ;; With current non-atomic code this raises an unhandled error;
            ;; handler-case catches it and exit-nonzero stays nil → FAILS.
            ;; With implemented error handling, process-fix-mode returns values
            ;; that reflect the write failure with :error or :warning severity,
            ;; making should-fail-p :warning return t → exit-nonzero t → PASSES.
            ;; The 4th return value (io-error-p) must also be true so that main
            ;; can exit 2 instead of exit 1 when an I/O failure occurs.
            (with-output-to-string (*standard-output*)
              (handler-case
                  (multiple-value-bind (has-errors has-warnings has-any io-error)
                      (mallet::process-fix-mode violations :fix :text)
                    (declare (ignore has-any))
                    ;; Default --fail-on is :warning: non-zero exit fires only
                    ;; when has-errors or has-warnings is true.  :info-severity
                    ;; violations alone are NOT enough.
                    (setf exit-nonzero (or has-errors has-warnings))
                    (setf io-error-p io-error))
                (error () nil))))

        (chmod-file-if-present readonly #o644)
        (delete-file-if-present readonly))

      (ok exit-nonzero
          "Write failure causes non-zero exit under default --fail-on warning")
      ;; Stronger assertion: process-fix-mode must set io-error-p so main can
      ;; exit 2 (I/O failure) rather than 1 (plain violations).  A stub that
      ;; only surfaces write errors as :warning violations passes the non-zero
      ;; check but still exits 1 — this assertion closes that gap.
      (ok io-error-p
          "process-fix-mode returns io-error-p=t so main exits 2 (I/O failure), not 1 (violations)")))

  ;; Companion: io-error-p must be NIL when the write succeeds.
  ;; Without this check a stub that always returns io-error=T in :fix mode
  ;; satisfies the failure-case assertion above while bypassing real error
  ;; detection.  Both the failure path (io-error=T) and the success path
  ;; (io-error=NIL) must hold before the gate is closed.
  (testing "Successful write causes process-fix-mode to return io-error-p=nil"
    (let* ((writable (merge-pathnames "mallet-test-exit-nonzero-writable.lisp"
                                      (uiop:temporary-directory)))
           (violations (list (make-replace-violation writable 1 "fixed")))
           (success-io-error-p :unset))
      (unwind-protect
          (progn
            (ignore-errors (sb-posix:chmod writable #o644))
            (with-open-file (s writable :direction :output :if-exists :supersede
                                         :if-does-not-exist :create)
              (write-string "content" s))
            (with-output-to-string (*standard-output*)
              (handler-case
                  (multiple-value-bind (has-errors has-warnings has-any io-error)
                      (mallet::process-fix-mode violations :fix :text)
                    (declare (ignore has-errors has-warnings has-any))
                    (setf success-io-error-p io-error))
                (error () (setf success-io-error-p :error)))))
        (ignore-errors (delete-file writable)))
      (ok (not success-io-error-p)
          "Successful write returns io-error-p=nil; a stub that always returns T fails here"))))

;;; === AC4: Error message uses plain pathname, not #P or #<SB- ===

(deftest fixer-write-error-message-format-test
  (testing "Error for unwritable file uses plain pathname string, not #P or #<SB- notation"
    (let ((readonly (merge-pathnames "mallet-test-errmsg-readonly.lisp" (uiop:temporary-directory))))
      (unwind-protect
          (progn
            (chmod-file-if-present readonly #o644)
            (with-open-file (s readonly :direction :output :if-exists :supersede
                                         :if-does-not-exist :create)
              (write-string "content" s))
            (sb-posix:chmod readonly #o444)

            (let* ((v (make-replace-violation readonly 1 "fixed"))
                   (err-str
                     (with-output-to-string (*error-output*)
                       (handler-case (fixer:apply-fixes (list v)) (error () nil)))))
              ;; Plain pathname must appear in the error output.
              (ok (search (namestring readonly) err-str)
                  "Error output contains the plain pathname string")
              ;; Must NOT use #P pathname-literal notation.
              (ok (not (search "#P" err-str))
                  "Error output does not contain #P notation")
              ;; Must NOT use SBCL internal stream-object notation.
              (ok (not (search "#<SB-" err-str))
                  "Error output does not contain #<SB- notation")))
        (chmod-file-if-present readonly #o644)
        (delete-file-if-present readonly)))))

;;; === AC5: Output is in pathname-sorted order and deterministic across runs ===
;;;
;;; Insert violations in FORWARD alphabetical order (aaa…hhh).  apply-fixes
;;; currently uses maphash then nreverse, which together accidentally produce
;;; forward order when input is REVERSE — but produce WRONG (reverse) order
;;; when input is forward.  Using forward input ensures a maphash+nreverse
;;; implementation fails the strict-order assertion, while a correct sorted
;;; implementation always passes.

(deftest fix-mode-sorted-output-test
  (testing "process-fix-mode outputs violations in pathname-sorted order"
    (let* ((names '("aaa" "bbb" "ccc" "ddd" "eee" "fff" "ggg" "hhh"))
           (files (mapcar (lambda (n)
                            (merge-pathnames
                             (make-pathname :name (format nil "mallet-sort-~A" n)
                                           :type "lisp")
                             (uiop:temporary-directory)))
                          names)))
      (unwind-protect
          (progn
            (dolist (p files)
              (with-open-file (s p :direction :output :if-exists :supersede
                                   :if-does-not-exist :create)
                (write-string "trailing   " s)))

            ;; Violations in FORWARD alphabetical order.  maphash+nreverse
            ;; reverses them, producing wrong output — caught by the strict
            ;; position check below.
            (let* ((violations
                     (mapcar (lambda (p) (make-replace-violation p 1 "trailing"))
                             files))  ; forward order: aaa bbb ... hhh
                   ;; Run once — check sorted order.
                   (output1
                     (with-output-to-string (*standard-output*)
                       (mallet::process-fix-mode violations :fix-dry-run :text)))
                   ;; Run again with the same violations — must be byte-identical.
                   (output2
                     (with-output-to-string (*standard-output*)
                       (mallet::process-fix-mode violations :fix-dry-run :text))))
              ;; Collect positions of each filename in the first output.
              (let ((positions
                      (mapcar (lambda (p) (search (namestring p) output1)) files)))
                (ok (every #'integerp positions)
                    "All filenames appear in the output")
                ;; Strict ascending position order = alphabetical file order.
                (ok (apply #'< positions)
                    "Files appear in pathname-sorted (alphabetical) order")
                (ok (string= output1 output2)
                    "Two repeated runs produce byte-identical output ordering")))))
        (dolist (p files)
          (delete-file-if-present p)))))
