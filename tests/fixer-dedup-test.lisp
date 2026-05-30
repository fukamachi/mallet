(defpackage #:mallet/tests/fixer-dedup
  (:use #:cl
        #:rove)
  (:local-nicknames
   (#:fixer #:mallet/fixer)
   (#:violation #:mallet/violation)))
(in-package #:mallet/tests/fixer-dedup)

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

(defun delete-file-if-present (path)
  (handler-case
      (delete-file path)
    (file-error () nil)))

;;; === AC1 + AC3: All :replace-line fixes on distinct lines are applied ===
;;;
;;; Regression for fix-key omitting line-number: every :replace-line fix
;;; collapsed to (:replace-line nil nil nil nil), so only the first was kept
;;; and the remaining lines were silently left dirty.

(deftest replace-line-dedup-all-fixes-applied-test
  (testing "All :replace-line fixes on distinct lines are applied"
    (let ((test-file (merge-pathnames "mallet-dedup-all-lines-test.lisp"
                                      (uiop:temporary-directory))))
      (unwind-protect
          (progn
            (with-open-file (s test-file :direction :output
                                          :if-exists :supersede
                                          :if-does-not-exist :create)
              (write-string (format nil "(defun f1 () nil)   ~%(defun f2 () nil)   ~%(defun f3 () nil)   ~%") s))
            (fixer:apply-fixes
             (list (make-replace-violation test-file 1 "(defun f1 () nil)")
                   (make-replace-violation test-file 2 "(defun f2 () nil)")
                   (make-replace-violation test-file 3 "(defun f3 () nil)")))
            (let ((lines (uiop:read-file-lines test-file)))
              ;; One (ok ...) per targeted line — AC3 requirement.
              (ok (string= (nth 0 lines) "(defun f1 () nil)")
                  "Line 1: trailing whitespace removed")
              (ok (string= (nth 1 lines) "(defun f2 () nil)")
                  "Line 2: trailing whitespace removed")
              (ok (string= (nth 2 lines) "(defun f3 () nil)")
                  "Line 3: trailing whitespace removed")))
        (delete-file-if-present test-file)))))

;;; === AC2: Only targeted lines are modified; untargeted lines unchanged ===

(deftest replace-line-dedup-selective-fix-test
  (testing "Targeted lines 1 and 5 are fixed; lines 2-4 are byte-identical to original"
    (let* ((test-file (merge-pathnames "mallet-dedup-selective-test.lisp"
                                       (uiop:temporary-directory)))
           (line2 "(defun f2 () nil)")
           (line3 "(defun f3 () nil)")
           (line4 "(defun f4 () nil)"))
      (unwind-protect
          (progn
            (with-open-file (s test-file :direction :output
                                          :if-exists :supersede
                                          :if-does-not-exist :create)
              (write-string (format nil "(defun f1 () nil)   ~%~A~%~A~%~A~%(defun f5 () nil)   ~%"
                                    line2 line3 line4)
                            s))
            (fixer:apply-fixes
             (list (make-replace-violation test-file 1 "(defun f1 () nil)")
                   (make-replace-violation test-file 5 "(defun f5 () nil)")))
            (let ((lines (uiop:read-file-lines test-file)))
              ;; Targeted lines must be fixed.
              (ok (string= (nth 0 lines) "(defun f1 () nil)")
                  "Line 1: trailing whitespace removed")
              (ok (string= (nth 4 lines) "(defun f5 () nil)")
                  "Line 5: trailing whitespace removed")
              ;; Untargeted lines must be byte-identical to original.
              (ok (string= (nth 1 lines) line2) "Line 2: unchanged")
              (ok (string= (nth 2 lines) line3) "Line 3: unchanged")
              (ok (string= (nth 3 lines) line4) "Line 4: unchanged")))
        (delete-file-if-present test-file)))))

;;; === AC4: True duplicates (same line + type + replacement) are applied once ===
;;;
;;; After adding line-number to fix-key, two violations for the same line with
;;; the same replacement still collide in the seen-fixes table and are deduped
;;; to one application.  This test guards against removing dedup entirely.

(deftest replace-line-true-duplicate-dedup-test
  (testing "Two identical :replace-line fixes on the same line are applied exactly once"
    (let ((test-file (merge-pathnames "mallet-dedup-duplicate-test.lisp"
                                      (uiop:temporary-directory))))
      (unwind-protect
          (progn
            (with-open-file (s test-file :direction :output
                                          :if-exists :supersede
                                          :if-does-not-exist :create)
              (write-string (format nil "line with trailing   ~%second line~%") s))
            (fixer:apply-fixes
             (list (make-replace-violation test-file 1 "line with trailing")
                   (make-replace-violation test-file 1 "line with trailing")))
            (let ((lines (uiop:read-file-lines test-file)))
              ;; The targeted line must be fixed.
              (ok (string= (nth 0 lines) "line with trailing")
                  "Line 1: trailing whitespace removed")
              ;; The untargeted line must be intact.
              (ok (string= (nth 1 lines) "second line")
                  "Line 2: untouched by deduped fix")))
        (delete-file-if-present test-file)))))
