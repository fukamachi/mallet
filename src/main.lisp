(defpackage #:malvolio
  (:use #:cl
        #:malvolio/violation
        #:malvolio/parser
        #:malvolio/rules)
  (:local-nicknames
   (#:a #:alexandria))
  (:export #:main
           #:lint-file
           #:lint-files
           ;; Public API for library use
           #:*rule-registry*
           #:make-registry
           #:register-rule
           #:find-rule
           #:list-rules
           #:enable-rule
           #:disable-rule
           ;; Configuration
           #:load-config
           #:make-config
           ;; Core classes (for extensibility)
           #:rule
           #:rule-name
           #:rule-description
           #:rule-severity
           #:rule-type
           #:rule-enabled-p
           #:config
           ;; Re-exported from malvolio/violation
           #:violation
           #:violation-rule
           #:violation-file
           #:violation-line
           #:violation-column
           #:violation-severity
           #:violation-message
           #:violation-fix
           ;; Re-exported from malvolio/parser
           #:token
           #:token-type
           #:token-value
           #:token-line
           #:token-column
           #:token-file
           #:token-raw
           #:tokenize
           #:form
           #:form-expr
           #:form-line
           #:form-column
           #:form-end-line
           #:form-end-column
           #:form-file
           #:form-source
           #:parse-forms
           #:analyze-text))
(in-package #:malvolio)
