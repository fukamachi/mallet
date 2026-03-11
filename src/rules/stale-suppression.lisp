(defpackage #:mallet/rules/stale-suppression
  (:use #:cl)
  (:local-nicknames
   (#:base #:mallet/rules/base))
  (:export #:stale-suppression-rule
           #:make-stale-suppression-message))
(in-package #:mallet/rules/stale-suppression)

;;; Stale suppression rule

(defclass stale-suppression-rule (base:rule)
  ()
  (:default-initargs
   :name :stale-suppression
   :description "Suppression comment has no effect (no matching violation found)"
   :severity :warning
   :category :cleanliness
   :type :form)
  (:documentation "Rule for stale suppression violations.
Violations are created directly by the engine when an inline suppression comment
does not suppress any actual violation. This rule class exists for metadata only
(category, severity lookup) — check-form is a no-op."))

(defmethod base:check-form ((rule stale-suppression-rule) form file)
  "No-op: engine creates stale-suppression violations directly."
  (declare (ignore form file))
  nil)

;;; Violation message helper

(defun make-stale-suppression-message (rule-name &optional reason)
  "Return the standard stale-suppression violation message for RULE-NAME.
RULE-NAME is typically a keyword symbol (e.g. :unused-variables).
Optional REASON string is appended when present."
  (let ((base (format nil "suppression for ~S has no effect (no matching violation found)"
                      rule-name)))
    (if reason
        (format nil "~A (reason: \"~A\")" base reason)
        base)))
