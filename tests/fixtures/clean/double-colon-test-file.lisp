;;; Test file using rove - double-colon-access is skipped for test files
;;; that use a known test framework in their defpackage.

(defpackage #:my-package/test
  (:use #:cl #:rove))
(in-package #:my-package/test)

;;; In test files, :: access is allowed for white-box testing
(deftest test-internal-access
  (testing "can access internals in tests"
    (ok (some-library::internal-function 42))
    (ok (some-library::*internal-var*))))
