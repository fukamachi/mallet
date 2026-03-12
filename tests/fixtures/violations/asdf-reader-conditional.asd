;; Test fixture for asdf-reader-conditional rule
;; #+/#- reader conditionals inside defsystem bodies are not portable

(defsystem "asdf-reader-conditional"
  :depends-on ("alexandria")
  :components ((:file "main")
               #+sbcl (:file "sbcl-support")
               #-windows (:file "unix-support")))
