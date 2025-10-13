(defpackage #:mallet/tests/config-ignore
  (:use #:cl
        #:rove)
  (:local-nicknames
   (#:config #:mallet/config)))
(in-package #:mallet/tests/config-ignore)

(deftest parse-ignore-directive
  (testing "Parse single ignore pattern"
    (let ((cfg (config:parse-config
                '(:mallet-config
                  (:extends :default)
                  (:ignore "vendor/")))))
      (ok (equal (config:config-ignore cfg) '("vendor/")))))

  (testing "Parse multiple ignore patterns"
    (let ((cfg (config:parse-config
                '(:mallet-config
                  (:extends :default)
                  (:ignore "vendor/" "build/" "**/*-generated.lisp")))))
      (ok (equal (config:config-ignore cfg)
                 '("vendor/" "build/" "**/*-generated.lisp")))))

  (testing "Empty ignore list by default"
    (let ((cfg (config:parse-config '(:mallet-config (:extends :default)))))
      (ok (null (config:config-ignore cfg))))))

(deftest file-ignored-p-basic
  (testing "Match wildcard pattern"
    (let ((cfg (config:parse-config
                '(:mallet-config
                  (:extends :default)
                  (:ignore "**/*-generated.lisp")))))
      (ok (config:file-ignored-p cfg #P"/project/src/foo-generated.lisp"))
      (ok (config:file-ignored-p cfg #P"/project/build/bar-generated.lisp"))
      (ok (not (config:file-ignored-p cfg #P"/project/src/main.lisp")))))

  (testing "No ignore patterns - nothing ignored"
    (let ((cfg (config:parse-config '(:mallet-config (:extends :default)))))
      (ok (not (config:file-ignored-p cfg #P"/project/anything.lisp"))))))

;; Note: merge-configs was removed during refactoring
;; Merging is now handled internally by parse-config when using :extends
