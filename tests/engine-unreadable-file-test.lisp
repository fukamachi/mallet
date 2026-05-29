(defpackage #:mallet/tests/engine-unreadable-file
  (:use #:cl #:rove)
  (:local-nicknames
   (#:engine #:mallet/engine)
   (#:violation #:mallet/violation)
   (#:config #:mallet/config)))
(in-package #:mallet/tests/engine-unreadable-file)

;;; --- helpers ---

(defun make-test-dir ()
  (let* ((name (format nil "mallet-utf8-test-~A" (random 1000000)))
         (tmp (uiop:ensure-directory-pathname
               (or (uiop:getenv "TMPDIR") "/tmp/claude-1000/")))
         (dir (uiop:ensure-directory-pathname (merge-pathnames name tmp))))
    (ensure-directories-exist dir)
    dir))

(defun cleanup-dir (dir)
  (uiop:delete-directory-tree dir :validate t :if-does-not-exist :ignore))

(defun write-bytes-to-file (path bytes)
  "Write BYTES (list of integers 0-255) to PATH as raw binary."
  (with-open-file (out path
                       :direction :output
                       :element-type '(unsigned-byte 8)
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (dolist (byte bytes)
      (write-byte byte out))))

(defun write-text-to-file (path text)
  "Write TEXT string to PATH."
  (with-open-file (out path
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (write-string text out)))

;;; --- tests ---

(deftest lint-file-does-not-abort-on-non-utf8
  (testing "lint-file returns violations instead of signaling for a non-UTF-8 encoded file"
    (let* ((dir (make-test-dir))
           (bad (merge-pathnames "bad.lisp" dir)))
      (unwind-protect
          (progn
            ;; C0 80: invalid overlong UTF-8 two-byte sequence — the bytes from the bug report
            (write-bytes-to-file bad '(#xC0 #x80))
            (let ((result (engine:lint-file bad)))
              (ok (listp result)
                  "lint-file returns a list, not an error condition")
              (ok (not (null result))
                  "violation list is non-empty — the unreadable file is reported as a violation")))
        (cleanup-dir dir)))))

(deftest lint-file-non-utf8-message-is-human-readable
  (testing "violation for a non-UTF-8 file is correctly structured and has a human-readable message"
    (let* ((dir (make-test-dir))
           (bad (merge-pathnames "undecodable.lisp" dir)))
      (unwind-protect
          (progn
            (write-bytes-to-file bad '(#xC0 #x80))
            (let* ((violations (engine:lint-file bad))
                   (v (first violations)))
              (ok v "At least one violation returned for the unreadable file")
              (when v
                ;; AC2: violation must be attributed to the bad file ("naming that file")
                (ok (equal (violation:violation-file v) bad)
                    "Violation :file slot equals the unreadable file path")
                ;; Rule must identify this as a file-read failure (positive identity check —
                ;; empty-string messages, stubs, etc. cannot satisfy this)
                (ok (eq (violation:violation-rule v) :file-read-error)
                    "Violation rule is :file-read-error")
                ;; AC3: severity must be :error so the exit code is non-zero.
                ;; A stub returning :info or :warning would pass the message checks but
                ;; the overall run would exit 0 under the default --fail-on threshold.
                (ok (eq (violation:violation-severity v) :error)
                    "Violation severity is :error (required for non-zero exit on unreadable file)")
                (let ((msg (violation:violation-message v)))
                  (ok (stringp msg)
                      "Violation message is a plain string")
                  ;; Message must be substantively non-empty — a single character or empty
                  ;; string is not a human-readable description.
                  (ok (plusp (length msg))
                      "Violation message is non-empty")
                  ;; The acceptance criterion: message must not contain these Lisp printer
                  ;; artifacts, which would expose raw pathnames or SBCL stream internals
                  ;; to the user instead of a human-readable description.
                  (ng (search "#P\"" msg)
                      "Message does not contain raw #P\"...\" pathname literal")
                  (ng (search "#<SB-" msg)
                      "Message does not contain #<SB- SBCL internal object representation")))))
        (cleanup-dir dir)))))

(deftest lint-files-continues-after-unreadable-file
  (testing "lint-files processes all remaining valid files after encountering an unreadable one"
    (let* ((dir (make-test-dir))
           ;; Alphabetically first so it is encountered before the valid files
           (bad   (merge-pathnames "a-bad.lisp" dir))
           (good1 (merge-pathnames "b-good.lisp" dir))
           (good2 (merge-pathnames "c-good.lisp" dir)))
      (unwind-protect
          (progn
            (write-bytes-to-file bad '(#xC0 #x80))
            (write-text-to-file good1 "(defun foo () nil)")
            (write-text-to-file good2 "(defun bar () nil)")
            (let* (;; Empty rule set so valid files produce no violations of their own
                   (cfg (config:make-config :rules '()))
                   (results (engine:lint-files (list bad good1 good2) :config cfg)))
              ;; All three files must appear in results; the run must not abort on bad.lisp
              (ok (= 3 (length results))
                  "All 3 files appear in results — scan did not abort on the unreadable file")
              ;; The bad file should have at least one violation (the read-error)
              (let ((bad-entry (find bad results :key #'car :test #'equal)))
                (ok bad-entry "a-bad.lisp appears in results")
                (when bad-entry
                  (ok (not (null (cdr bad-entry)))
                      "a-bad.lisp has at least one violation (the decode error)")))
              ;; Both valid files must be present — the scan continued past the bad one
              (ok (find good1 results :key #'car :test #'equal)
                  "b-good.lisp is in results despite a-bad.lisp being unreadable")
              (ok (find good2 results :key #'car :test #'equal)
                  "c-good.lisp is in results despite a-bad.lisp being unreadable")))
        (cleanup-dir dir)))))
