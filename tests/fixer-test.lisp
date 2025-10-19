(defpackage #:mallet/tests/fixer
  (:use #:cl
        #:rove)
  (:local-nicknames
   (#:fixer #:mallet/fixer)
   (#:violation #:mallet/violation)))
(in-package #:mallet/tests/fixer)

(deftest apply-replace-line-test
  (testing "Replace single line"
    (let ((text "line 1
line 2
line 3
"))
      (let ((fix (violation:make-violation-fix
                  :type :replace-line
                  :line-number 2
                  :replacement-content "line 2")))
        (let ((result (fixer::apply-fix text fix)))
          (ok (string= result "line 1
line 2
line 3
"))))))

  (testing "Replace first line"
    (let ((text "old first line
second line
"))
      (let ((fix (violation:make-violation-fix
                  :type :replace-line
                  :line-number 1
                  :replacement-content "new first line")))
        (let ((result (fixer::apply-fix text fix)))
          (ok (string= result "new first line
second line
"))))))

  (testing "Replace last line"
    (let ((text "first line
old last line
"))
      (let ((fix (violation:make-violation-fix
                  :type :replace-line
                  :line-number 2
                  :replacement-content "new last line")))
        (let ((result (fixer::apply-fix text fix)))
          (ok (string= result "first line
new last line
")))))))

(deftest apply-append-to-file-test
  (testing "Append newline to file"
    (let ((text "line 1
line 2"))
      (let ((fix (violation:make-violation-fix
                  :type :append-to-file
                  :appended-content (string #\Newline))))
        (let ((result (fixer::apply-fix text fix)))
          (ok (string= result "line 1
line 2
"))))))

  (testing "Append to empty file"
    (let ((text ""))
      (let ((fix (violation:make-violation-fix
                  :type :append-to-file
                  :appended-content "content")))
        (let ((result (fixer::apply-fix text fix)))
          (ok (string= result "content")))))))

(deftest apply-delete-lines-test
  (testing "Delete single line"
    (let ((text "line 1
line 2
line 3
line 4
"))
      (let ((fix (violation:make-violation-fix
                  :type :delete-lines
                  :start-line 2
                  :end-line 2)))
        (let ((result (fixer::apply-fix text fix)))
          (ok (string= result "line 1
line 3
line 4
"))))))

  (testing "Delete multiple consecutive lines"
    (let ((text "line 1
blank 1
blank 2
blank 3
line 2
"))
      (let ((fix (violation:make-violation-fix
                  :type :delete-lines
                  :start-line 2
                  :end-line 4)))
        (let ((result (fixer::apply-fix text fix)))
          (ok (string= result "line 1
line 2
"))))))

  (testing "Delete first line"
    (let ((text "delete me
keep me
"))
      (let ((fix (violation:make-violation-fix
                  :type :delete-lines
                  :start-line 1
                  :end-line 1)))
        (let ((result (fixer::apply-fix text fix)))
          (ok (string= result "keep me
")))))))

(deftest apply-fixes-ordering-test
  (testing "Multiple fixes applied bottom-to-top"
    (let* ((text "line 1
line 2
line 3
")
           (v1 (make-instance 'violation:violation
                              :rule :trailing-whitespace
                              :file #P"/tmp/test.lisp"
                              :line 1
                              :column 0
                              :severity :format
                              :message "trailing"
                              :fix (violation:make-violation-fix
                                    :type :replace-line
                                    :line-number 1
                                    :replacement-content "line 1")))
           (v2 (make-instance 'violation:violation
                              :rule :trailing-whitespace
                              :file #P"/tmp/test.lisp"
                              :line 3
                              :column 0
                              :severity :format
                              :message "trailing"
                              :fix (violation:make-violation-fix
                                    :type :replace-line
                                    :line-number 3
                                    :replacement-content "line 3"))))

      ;; Apply fixes (should sort by line number descending)
      (let ((fixed-text text))
        ;; Simulate what fixer does: sort bottom-to-top, apply fixes
        (dolist (v (sort (list v1 v2) #'> :key #'violation:violation-line))
          (setf fixed-text (fixer::apply-fix fixed-text (violation:violation-fix v))))

        (ok (string= fixed-text "line 1
line 2
line 3
"))))))

(deftest apply-fixes-with-unfixable-test
  (testing "Separates fixable and unfixable violations"
    (let* ((fixable (make-instance 'violation:violation
                                   :rule :trailing-whitespace
                                   :file #P"/tmp/test.lisp"
                                   :line 1
                                   :column 0
                                   :severity :format
                                   :message "fixable"
                                   :fix (violation:make-violation-fix
                                         :type :replace-line
                                         :line-number 1
                                         :replacement-content "fixed")))
           (unfixable (make-instance 'violation:violation
                                     :rule :unused-variables
                                     :file #P"/tmp/test.lisp"
                                     :line 2
                                     :column 0
                                     :severity :warning
                                     :message "not fixable"
                                     :fix nil)))

      (multiple-value-bind (count fixed unfixed)
          (fixer:apply-fixes (list fixable unfixable) :dry-run t)
        (ok (= count 1))
        (ok (= (length fixed) 1))
        (ok (= (length unfixed) 1))
        (ok (eq (first fixed) fixable))
        (ok (eq (first unfixed) unfixable))))))
