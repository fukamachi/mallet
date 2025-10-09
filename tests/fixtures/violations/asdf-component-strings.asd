;; Test fixtures for ASDF component strings rule

;; Bad: system name as symbol
(defsystem #:my-project
  :depends-on ("alexandria"))

;; Good: system name as string
(defsystem "my-project-good"
  :depends-on ("alexandria"))

;; Bad: dependencies as symbols
(defsystem "another-project"
  :depends-on (#:alexandria
               #:cl-ppcre))

;; Good: dependencies as strings
(defsystem "another-project-good"
  :depends-on ("alexandria"
               "cl-ppcre"))

;; Bad: component names as symbols
(defsystem "project-with-components"
  :components ((:file #:main)
               (:file #:utils)))

;; Good: component names as strings
(defsystem "project-with-components-good"
  :components ((:file "main")
               (:file "utils")))

;; Bad: mixed - some symbols, some strings
(defsystem #:mixed-project
  :depends-on (#:alexandria
               "cl-ppcre")
  :components ((:file #:main)
               (:file "utils")))
