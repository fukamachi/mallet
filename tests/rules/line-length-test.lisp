(defpackage #:malvolio/tests/rules/line-length
  (:use #:cl
        #:rove)
  (:local-nicknames
   (#:rules #:malvolio/rules)
   (#:violation #:malvolio/violation)
   (#:ppcre #:cl-ppcre)))
(in-package #:malvolio/tests/rules/line-length)

(deftest line-length-within-limit
  (testing "Line within default limit (80 chars)"
    (let ((text (make-string 79 :initial-element #\a))
          (file #p"/tmp/test.lisp")
          (rule (make-instance 'rules:line-length-rule)))
      (let ((violations (rules:check-text rule text file)))
        (ok (null violations)))))

  (testing "Empty line"
    (let ((text "")
          (file #p"/tmp/test.lisp")
          (rule (make-instance 'rules:line-length-rule)))
      (let ((violations (rules:check-text rule text file)))
        (ok (null violations))))))

(deftest line-length-exceeds-limit
  (testing "Line exceeding default limit (80 chars)"
    (let ((text (make-string 81 :initial-element #\a))
          (file #p"/tmp/test.lisp")
          (rule (make-instance 'rules:line-length-rule)))
      (let ((violations (rules:check-text rule text file)))
        (ok (= 1 (length violations)))
        (let ((v (first violations)))
          (ok (eq :line-length (violation:violation-rule v)))
          (ok (eq 1 (violation:violation-line v)))
          (ok (ppcre:scan "exceeds.*80" (violation:violation-message v)))))))

  (testing "Multiple lines, one exceeding"
    (let ((text (format nil "~A~%~A~%~A"
                       (make-string 50 :initial-element #\a)
                       (make-string 100 :initial-element #\b)
                       (make-string 50 :initial-element #\c)))
          (file #p"/tmp/test.lisp")
          (rule (make-instance 'rules:line-length-rule)))
      (let ((violations (rules:check-text rule text file)))
        (ok (= 1 (length violations)))
        (let ((v (first violations)))
          (ok (eq 2 (violation:violation-line v)))))))

  (testing "Multiple lines exceeding"
    (let ((text (format nil "~A~%~A~%~A"
                       (make-string 100 :initial-element #\a)
                       (make-string 50 :initial-element #\b)
                       (make-string 90 :initial-element #\c)))
          (file #p"/tmp/test.lisp")
          (rule (make-instance 'rules:line-length-rule)))
      (let ((violations (rules:check-text rule text file)))
        (ok (= 2 (length violations)))
        (ok (eq 1 (violation:violation-line (first violations))))
        (ok (eq 3 (violation:violation-line (second violations))))))))

(deftest line-length-custom-limit
  (testing "Custom limit 120 characters"
    (let ((text (make-string 100 :initial-element #\a))
          (file #p"/tmp/test.lisp")
          (rule (make-instance 'rules:line-length-rule :max-length 120)))
      (let ((violations (rules:check-text rule text file)))
        (ok (null violations)))))

  (testing "Exceeding custom limit"
    (let ((text (make-string 121 :initial-element #\a))
          (file #p"/tmp/test.lisp")
          (rule (make-instance 'rules:line-length-rule :max-length 120)))
      (let ((violations (rules:check-text rule text file)))
        (ok (= 1 (length violations)))
        (let ((v (first violations)))
          (ok (ppcre:scan "exceeds.*120" (violation:violation-message v))))))))
