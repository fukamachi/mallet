(defpackage #:mallet/tests/suppression-integration
  (:use #:cl
        #:rove)
  (:local-nicknames
   (#:engine #:mallet/engine)
   (#:config #:mallet/config)
   (#:violation #:mallet/violation)))
(in-package #:mallet/tests/suppression-integration)

(deftest suppress-next-integration
  (testing "suppress-next actually suppresses violations in real file"
    (let* ((test-file (merge-pathnames
                       "tests/fixtures/violations/suppress-next.lisp"
                       (asdf:system-source-directory :mallet)))
           ;; Create config with if-without-else rule enabled
           (config (config:make-config
                    :rules (list (mallet/rules:make-rule :if-without-else))))
           (violations (engine:lint-file test-file :config config)))

      ;; Debug: print violations count
      (format t "~%Got ~D violation(s)~%" (length violations))
      (when violations
        (dolist (v violations)
          (format t "  Line ~D: ~A~%"
                  (violation:violation-line v)
                  (violation:violation-rule v))))

      ;; Should have exactly 1 violation (the second function without suppression)
      ;; The first function (foo) is suppressed
      (ok (= (length violations) 1) "Should have exactly 1 violation")

      ;; The violation should be for the second function (bar)
      (when (>= (length violations) 1)
        (let ((v (first violations)))
          (ok (eq (violation:violation-rule v) :if-without-else))
          ;; After migration, violations are reported at the defun line (15) instead of the if line (16)
          (ok (= (violation:violation-line v) 15) "Should be line 15 (bar function)"))))))
