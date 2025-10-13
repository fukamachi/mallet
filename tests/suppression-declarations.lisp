(defpackage #:mallet/tests/suppression-declarations
  (:use #:cl
        #:rove)
  (:local-nicknames
   (#:suppression #:mallet/suppression)))
(in-package #:mallet/tests/suppression-declarations)

;;; Setup: Ensure mallet package exists for tests
(suppression:ensure-mallet-package-exists)

(deftest mallet-declaim-recognition
  (testing "Recognize mallet declaim forms"
    ;; Valid mallet declaim
    (let ((form `(declaim (,(intern "DISABLE" "MALLET") :line-length :if-without-else))))
      (ok (suppression:mallet-declaim-p form)))

    ;; Another valid mallet declaim
    (let ((form `(declaim (,(intern "SUPPRESS-NEXT" "MALLET") :line-length))))
      (ok (suppression:mallet-declaim-p form)))

    ;; Not a declaim
    (ng (suppression:mallet-declaim-p '(defun foo ())))

    ;; Regular declaim (not mallet)
    (ng (suppression:mallet-declaim-p '(declaim (optimize (speed 3)))))))

(deftest mallet-declaration-recognition
  (testing "Recognize individual mallet declarations (for declaim)"
    ;; Valid mallet declarations for declaim
    (ok (suppression:mallet-declaration-p `(,(intern "DISABLE" "MALLET") :line-length)))
    (ok (suppression:mallet-declaration-p `(,(intern "ENABLE" "MALLET") :line-length)))
    (ok (suppression:mallet-declaration-p `(,(intern "SUPPRESS-NEXT" "MALLET") :line-length)))
    (ok (suppression:mallet-declaration-p `(,(intern "SUPPRESS-FUNCTION" "MALLET") :line-length foo bar)))

    ;; Note: SUPPRESS is used in declare forms, not declaim, so not checked here

    ;; Invalid - not a mallet declaration
    (ng (suppression:mallet-declaration-p '(optimize (speed 3))))
    (ng (suppression:mallet-declaration-p '(inline foo bar)))))

(deftest parse-mallet-declaration-disable-enable
  (testing "Parse disable/enable declarations"
    (let ((decl `(,(intern "DISABLE" "MALLET") :line-length :if-without-else)))
      (multiple-value-bind (type rules function-names)
          (suppression:parse-mallet-declaration decl)
        (ok (eq type :disable))
        (ok (equal rules '(:line-length :if-without-else)))
        (ok (null function-names))))

    (let ((decl `(,(intern "ENABLE" "MALLET") :line-length)))
      (multiple-value-bind (type rules function-names)
          (suppression:parse-mallet-declaration decl)
        (ok (eq type :enable))
        (ok (equal rules '(:line-length)))
        (ok (null function-names))))))

(deftest parse-mallet-declaration-suppress-next
  (testing "Parse suppress-next declarations"
    (let ((decl `(,(intern "SUPPRESS-NEXT" "MALLET") :line-length :if-without-else)))
      (multiple-value-bind (type rules function-names)
          (suppression:parse-mallet-declaration decl)
        (ok (eq type :suppress-next))
        (ok (equal rules '(:line-length :if-without-else)))
        (ok (null function-names))))))

(deftest parse-mallet-declaration-suppress-function
  (testing "Parse suppress-function declarations"
    (let ((decl `(,(intern "SUPPRESS-FUNCTION" "MALLET") :line-length factorial fibonacci)))
      (multiple-value-bind (type rules function-names)
          (suppression:parse-mallet-declaration decl)
        (ok (eq type :suppress-function))
        (ok (equal rules '(:line-length)))
        (ok (equal function-names '(factorial fibonacci)))))))

(deftest parse-mallet-declaration-all
  (testing "Parse declarations with :all"
    (let ((decl `(,(intern "DISABLE" "MALLET") :all)))
      (multiple-value-bind (type rules function-names)
          (suppression:parse-mallet-declaration decl)
        (ok (eq type :disable))
        (ok (equal rules '(:all)))
        (ok (null function-names))))))

(deftest extract-mallet-declare
  (testing "Extract mallet:suppress from declare forms"
    ;; Valid mallet suppress in declare
    (let ((form `(declare (,(intern "SUPPRESS" "MALLET") :line-length :unused-variables))))
      (let ((rules (suppression:extract-mallet-declare form)))
        (ok (equal rules '(:line-length :unused-variables)))))

    ;; Multiple declarations, one is mallet
    (let ((form `(declare (optimize (speed 3))
                          (,(intern "SUPPRESS" "MALLET") :line-length)
                          (ignore x))))
      (let ((rules (suppression:extract-mallet-declare form)))
        (ok (equal rules '(:line-length)))))

    ;; No mallet declarations
    (let ((form '(declare (optimize (speed 3)))))
      (ok (null (suppression:extract-mallet-declare form))))

    ;; Not a declare form
    (ok (null (suppression:extract-mallet-declare '(defun foo ()))))))
