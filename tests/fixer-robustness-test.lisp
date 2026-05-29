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

;;; === AC1: Basic correctness - trailing whitespace fix leaves other content intact ===
;;;
;;; This is a regression test: the atomic-write refactoring must not corrupt the
;;; file content.  Four-line file; lines 1 and 3 have trailing spaces; lines 2
;;; and 4 must be byte-identical after the fix.

(deftest fixer-basic-fix-leaves-other-content-unchanged-test
  (testing "Applying --fix removes only trailing whitespace; all other bytes unchanged"
    ;; One violation on line 1 only; lines 2-4 must survive bit-for-bit.
    (let* ((test-file (merge-pathnames "mallet-basic-fix-ac1-test.lisp" (uiop:temporary-directory)))
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
        (ignore-errors (delete-file test-file))))))

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
            (ignore-errors (sb-posix:chmod test-dir #o755))
            (ignore-errors (delete-file test-file))
            (ignore-errors (sb-posix:rmdir test-dir))

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
        (ignore-errors (sb-posix:chmod test-dir #o755))
        (ignore-errors (delete-file test-file))
        (ignore-errors (sb-posix:rmdir test-dir))))))

;;; === AC3: Multi-file --fix continues past a read-only file ===

(deftest fixer-continues-past-write-error-test
  (testing "apply-fixes continues fixing writable files when another file cannot be written"
    (let ((writable (merge-pathnames "mallet-test-robustness-writable.lisp" (uiop:temporary-directory)))
          (readonly (merge-pathnames "mallet-test-robustness-readonly.lisp" (uiop:temporary-directory)))
          (original "line with trailing   "))
      (unwind-protect
          (progn
            (dolist (p (list writable readonly))
              (ignore-errors (sb-posix:chmod p #o644))
              (with-open-file (s p :direction :output :if-exists :supersede
                                   :if-does-not-exist :create)
                (write-string original s)))
            (sb-posix:chmod readonly #o444)

            (let ((violations
                    (list (make-replace-violation writable 1 "line with trailing")
                          (make-replace-violation readonly 1 "line with trailing")))
                  (error-raised nil)
                  (result-unfixed nil))
              (handler-case
                  (multiple-value-bind (fixed-count fixed-violations unfixed-violations)
                      (fixer:apply-fixes violations)
                    (declare (ignore fixed-count fixed-violations))
                    (setf result-unfixed unfixed-violations))
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
                  "Write-error violation recorded as unfixed, enabling non-zero exit")))
        (ignore-errors (sb-posix:chmod readonly #o644))
        (ignore-errors (delete-file writable))
        (ignore-errors (delete-file readonly))))))

;;; === AC3-EXIT: Write failure causes non-zero exit under default --fail-on ===
;;;
;;; AC3 requires "finishes with a non-zero exit."  The default --fail-on is
;;; :warning, so `should-fail-p :warning has-errors has-warnings _` fires only
;;; when has-errors or has-warnings is non-nil.  A stub that re-adds the
;;; original :info violations to unfixed-violations satisfies the
;;; "unfixed non-empty" gate in fixer-continues-past-write-error-test but still
;;; exits 0 under the default threshold.  This test closes that gap by calling
;;; process-fix-mode directly and checking (or has-errors has-warnings).

(deftest fixer-write-error-exits-nonzero-test
  (testing "Write failure causes process-fix-mode to return non-zero exit indication"
    (let* ((readonly (merge-pathnames "mallet-test-exit-nonzero-readonly.lisp" (uiop:temporary-directory)))
           (violations (list (make-replace-violation readonly 1 "fixed")))
           (exit-nonzero nil))
      (unwind-protect
          (progn
            (ignore-errors (sb-posix:chmod readonly #o644))
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
            (with-output-to-string (*standard-output*)
              (handler-case
                  (multiple-value-bind (has-errors has-warnings has-any)
                      (mallet::process-fix-mode violations :fix :text)
                    (declare (ignore has-any))
                    ;; Default --fail-on is :warning: non-zero exit fires only
                    ;; when has-errors or has-warnings is true.  :info-severity
                    ;; violations alone are NOT enough.
                    (setf exit-nonzero (or has-errors has-warnings)))
                (error () nil))))

        (ignore-errors (sb-posix:chmod readonly #o644))
        (ignore-errors (delete-file readonly)))

      (ok exit-nonzero
          "Write failure causes non-zero exit under default --fail-on warning"))))

;;; === AC4: Error message uses plain pathname, not #P or #<SB- ===

(deftest fixer-write-error-message-format-test
  (testing "Error for unwritable file uses plain pathname string, not #P or #<SB- notation"
    (let ((readonly (merge-pathnames "mallet-test-errmsg-readonly.lisp" (uiop:temporary-directory))))
      (unwind-protect
          (progn
            (ignore-errors (sb-posix:chmod readonly #o644))
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
        (ignore-errors (sb-posix:chmod readonly #o644))
        (ignore-errors (delete-file readonly))))))

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
          (ignore-errors (delete-file p))))))
