;; Clean fixture for asdf-operate-in-perform rule
;; Uses symbol-call instead of direct asdf:operate calls

(defsystem "asdf-operate-in-perform"
  :depends-on ("alexandria")
  :components ((:file "main"))
  :perform (test-op (o c)
    (symbol-call :asdf :load-system "asdf-operate-in-perform/tests")
    (symbol-call :asdf :test-system "asdf-operate-in-perform/tests")))
