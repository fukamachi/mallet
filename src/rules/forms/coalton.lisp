(defpackage #:mallet/rules/forms/coalton
  (:use #:cl)
  (:local-nicknames
   (#:base #:mallet/rules/base)
   (#:parser #:mallet/parser)
   (#:violation #:mallet/violation))
  (:export #:coalton-missing-declare-rule))
(in-package #:mallet/rules/forms/coalton)

;;; coalton-missing-declare rule

(defclass coalton-missing-declare-rule (base:coalton-rule)
  ()
  (:default-initargs
   :name :coalton-missing-declare
   :description "Coalton function define is missing a preceding declare type signature"
   :severity :warning
   :category :practice
   :type :form)
  (:documentation "Warns when a function (define (fn args...) body) inside
coalton-toplevel lacks a preceding (declare (fn type...)) type signature.
Value defines like (define x 42) are not flagged."))

;;; Helpers

(defun coalton-symbol-name (head)
  "Extract the base symbol name from HEAD (string or symbol), uppercase.
Returns NIL if HEAD is not a recognisable symbol."
  (let ((raw (typecase head
               (string (base:symbol-name-from-string head))
               (symbol (symbol-name head))
               (otherwise nil))))
    (when raw (string-upcase raw))))

(defun declare-defined-name (head-name sub-form)
  "If HEAD-NAME is \"DECLARE\" and SUB-FORM is (declare (fn-name ...)),
return the declared name as an uppercase string. Otherwise return NIL."
  (when (string-equal head-name "DECLARE")
    (let ((sig (second sub-form)))
      (when (consp sig)
        (coalton-symbol-name (first sig))))))

(defun function-define-name (head-name sub-form)
  "If HEAD-NAME is \"DEFINE\" and SUB-FORM is (define (fn-name ...) body),
return the defined name as an uppercase string. Otherwise return NIL.
Does NOT match value defines like (define x 42) or (define-type ...) forms."
  (when (string-equal head-name "DEFINE")
    (let ((second-elem (second sub-form)))
      ;; Only flag the function form: (define (name args...) body)
      ;; Value defines have an atom as second element — skip those.
      (when (consp second-elem)
        (coalton-symbol-name (first second-elem))))))

;;; Rule implementation

(defmethod base:check-form ((rule coalton-missing-declare-rule) form file)
  "Check that every function define inside the coalton-toplevel body has a
preceding declare with a matching name."
  (check-type form parser:form)
  (check-type file pathname)

  (let* ((expr (parser:form-expr form))
         (position-map (parser:form-position-map form))
         (fallback-line (parser:form-line form))
         (fallback-column (parser:form-column form))
         (body (when (consp expr) (rest expr)))
         (declared-names (make-hash-table :test #'equal))
         (violations '()))

    (dolist (sub-form body)
      (when (consp sub-form)
        ;; Compute head name once; used for both declare and define checks.
        (let* ((head-name (coalton-symbol-name (first sub-form)))
               (decl-name (declare-defined-name head-name sub-form))
               (def-name  (function-define-name head-name sub-form)))
          (when decl-name
            (setf (gethash decl-name declared-names) t))
          (when (and def-name (not (gethash def-name declared-names)))
            (multiple-value-bind (actual-line actual-column)
                (base:find-actual-position sub-form position-map
                                          fallback-line fallback-column)
              (push (make-instance 'violation:violation
                                   :rule :coalton-missing-declare
                                   :file file
                                   :line actual-line
                                   :column actual-column
                                   :severity (base:rule-severity rule)
                                   :category (base:rule-category rule)
                                   :message (format nil
                                                    "Function ~A is missing a preceding declare type signature"
                                                    def-name))
                    violations))))))

    (nreverse violations)))
