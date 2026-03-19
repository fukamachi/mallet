(defpackage #:mallet/rules/forms/asdf
  (:use #:cl)
  (:local-nicknames
   (#:base #:mallet/rules/base)
   (#:violation #:mallet/violation)
   (#:parser #:mallet/parser)
   (#:utils #:mallet/utils))
  (:export #:asdf-component-strings-rule))
(in-package #:mallet/rules/forms/asdf)

;;; ASDF Component Strings Rule

(defclass asdf-component-strings-rule (base:rule)
  ()
  (:default-initargs
   :name :asdf-component-strings
   :description "ASDF components, systems, and dependencies should use strings not symbols"
   :severity :warning
   :category :practice
   :type :form
   :file-types '(:asd))
  (:documentation "Rule to check that ASDF defsystem uses strings for component names,
system names, and dependencies rather than symbols or keywords."))

(defmethod base:check-form ((rule asdf-component-strings-rule) form file)
  "Check that defsystem uses strings for names."
  ;; Only check .asd files
  (when (string-equal (pathname-type file) "asd")
    (let ((expr (parser:form-expr form)))
      (when (and (consp expr)
                 (stringp (first expr))
                 (base:symbol-matches-p (first expr) "DEFSYSTEM"))
        (check-defsystem-source form file (base:rule-severity rule))))))

;;; Helper functions

(defun source-offset (source form-line form-column target-line target-column)
  "Convert absolute (TARGET-LINE, TARGET-COLUMN) to a character offset in SOURCE.
SOURCE starts at (FORM-LINE, FORM-COLUMN). Returns offset or NIL if out of range."
  (let ((offset 0)
        (current-line form-line))
    (loop while (< current-line target-line)
          do (let ((nl (position #\Newline source :start offset)))
               (unless nl
                 (return-from source-offset nil))
               (setf offset (1+ nl))
               (incf current-line)))
    (let ((result (if (= target-line form-line)
                      (+ offset (- target-column form-column))
                      (+ offset target-column))))
      (when (and (>= result 0) (< result (length source)))
        result))))

(defun source-char-at (value form)
  "Return the character at VALUE's position in the form source, or NIL.
Uses the position-map to find where VALUE appears in the raw source text."
  (let ((position-map (parser:form-position-map form))
        (source (parser:form-source form)))
    (when (and position-map source)
      (let ((pos (gethash value position-map)))
        (when pos
          (let ((offset (source-offset source
                                       (parser:form-line form)
                                       (parser:form-column form)
                                       (car pos) (cdr pos))))
            (when offset
              (char source offset))))))))

(defun value-is-symbol-p (value form)
  "Check if VALUE (a string from the parsed expr) was written as a symbol in source.
Uses the position-map for precise, scoped detection."
  (and (stringp value)
       (or
        (utils:keyword-string-p value)
        (and (find #\: value :test #'char=)
             (not (utils:keyword-string-p value)))
        (let ((ch (source-char-at value form)))
          (and ch (not (char= ch #\")))))))

(defun extract-symbol-name (str)
  "Extract the base name from a symbol string for display.
Examples: ':ALEXANDRIA' -> 'alexandria', 'CURRENT:FOO' -> 'foo'
Handles non-string inputs (like Eclector objects) gracefully by returning a default value."
  (if (not (stringp str))
      (if (and (consp str) (symbolp (first str)))
          (format nil "~A" (first str))
          (format nil "~A" str))
      (cond
        ((utils:keyword-string-p str)
         (string-downcase (subseq str 1)))
        ;; Uninterned: "#:FOO" -> "foo"
        ((and (> (length str) 1) (char= (char str 0) #\#) (char= (char str 1) #\:))
         (string-downcase (subseq str 2)))
        ;; Package-qualified: "CURRENT:FOO" -> "foo"
        ((find #\: str :test #'char=)
         (string-downcase (subseq str (1+ (position #\: str :from-end t :test #'char=)))))
        (t (string-downcase str)))))

(defun format-symbol-for-message (value form)
  "Format VALUE as it appeared in source for error messages.
Uses position-map to determine the original notation."
  (let ((name (extract-symbol-name value)))
    (cond
      ((utils:keyword-string-p value)
       (format nil ":~A" name))
      (t
       (let ((ch (source-char-at value form)))
         (if (and ch (char= ch #\#))
             (format nil "#:~A" name)
             name))))))

(defun create-violation (value form file severity message)
  "Create a violation for VALUE, using position-map for its location."
  (let ((position-map (parser:form-position-map form)))
    (multiple-value-bind (line column)
        (parser:find-position value position-map
                              (parser:form-line form)
                              (parser:form-column form))
      (make-instance 'violation:violation
                     :rule :asdf-component-strings
                     :file file
                     :line line
                     :column column
                     :severity severity
                     :message message))))

(defun check-dependency (dep form file severity)
  "Check a single dependency DEP for symbol usage.
Handles: simple-name | (:feature expr dep) | (:version name ver) | (:require name)"
  (cond
    ((stringp dep)
     (when (value-is-symbol-p dep form)
       (list (create-violation dep form file severity
                               (format nil "Dependency should be string \"~A\", instead of symbol ~A"
                                       (extract-symbol-name dep)
                                       (format-symbol-for-message dep form))))))

    ((consp dep)
     (when (>= (length dep) 2)
       (let ((dep-type (first dep)))
         (when (stringp dep-type)
           (let ((type-name (base:symbol-name-from-string dep-type)))
             (cond
               ((string-equal type-name "VERSION")
                (when (>= (length dep) 2)
                  (let ((sys-name (second dep)))
                    (when (value-is-symbol-p sys-name form)
                      (list (create-violation sys-name form file severity
                                              (format nil "System name should be string \"~A\", instead of symbol ~A"
                                                      (extract-symbol-name sys-name)
                                                      (format-symbol-for-message sys-name form))))))))

               ((string-equal type-name "REQUIRE")
                (when (>= (length dep) 2)
                  (let ((mod-name (second dep)))
                    (when (value-is-symbol-p mod-name form)
                      (list (create-violation mod-name form file severity
                                              (format nil "Module name should be string \"~A\", instead of symbol ~A"
                                                      (extract-symbol-name mod-name)
                                                      (format-symbol-for-message mod-name form))))))))

               ((string-equal type-name "FEATURE")
                (when (>= (length dep) 3)
                  (check-dependency (third dep) form file severity)))

               (t nil)))))))

    (t nil)))

(defun check-in-order-to-list (in-order-to-clauses form file severity)
  "Check :in-order-to clauses for symbol usage in system names.
Structure: ((operation (operation system-name) ...) ...)"
  (let ((violations '()))
    (dolist (clause in-order-to-clauses)
      (when (consp clause)
        (dolist (dependency (rest clause))
          (when (and (consp dependency)
                     (>= (length dependency) 2))
            (let ((system-name (second dependency)))
              (when (value-is-symbol-p system-name form)
                (let ((sym-name (extract-symbol-name system-name)))
                  (push (create-violation system-name form file severity
                                          (format nil "System name should be string \"~A\", instead of symbol ~A"
                                                  sym-name
                                                  (format-symbol-for-message system-name form)))
                        violations))))))))
    violations))

(defun check-components-list (components form file severity)
  "Check a list of component definitions recursively, returning violations.
Component-def structure: (component-type component-name option*)
- First element (component-type) can be any keyword (valid)
- Second element (component-name) should be a string
- Components can have :depends-on and nested :components"
  (let ((violations '()))
    (dolist (component-def components)
      (when (consp component-def)
        (when (>= (length component-def) 2)
          (let ((component-name (second component-def)))
            (when (value-is-symbol-p component-name form)
              (let ((sym-name (extract-symbol-name component-name)))
                (push (create-violation component-name form file severity
                                        (format nil "Component name should be string \"~A\", instead of symbol ~A"
                                                sym-name
                                                (format-symbol-for-message component-name form)))
                      violations)))))

        (loop for (key value) on (cddr component-def) by #'cddr
              when (stringp key)
                do (let ((key-name (base:symbol-name-from-string key)))
                     (cond
                       ((string-equal key-name "COMPONENTS")
                        (when (consp value)
                          (setf violations (nconc violations
                                                  (check-components-list value form file severity)))))
                       ((string-equal key-name "DEPENDS-ON")
                        (when (consp value)
                          (dolist (dep value)
                            (let ((dep-violations (check-dependency dep form file severity)))
                              (when dep-violations
                                (setf violations (nconc violations dep-violations))))))))))))
    violations))

(defun check-defsystem-source (form file severity)
  "Check a defsystem form for symbol usage instead of strings.
Parses the semantic structure: system name, :depends-on dependencies, :components definitions."
  (let ((violations '())
        (expr (parser:form-expr form)))

    (when (>= (length expr) 2)
      (let ((system-name (second expr)))
        (when (value-is-symbol-p system-name form)
          (let ((sym-name (extract-symbol-name system-name)))
            (push (create-violation system-name form file severity
                                    (format nil "System name should be string \"~A\", instead of symbol ~A"
                                            sym-name
                                            (format-symbol-for-message system-name form)))
                  violations)))))

    (loop for (key value) on (cddr expr) by #'cddr
          when (stringp key)
            do (let ((key-name (base:symbol-name-from-string key)))
                 (cond
                   ((string-equal key-name "DEPENDS-ON")
                    (when (consp value)
                      (dolist (dep value)
                        (let ((dep-violations (check-dependency dep form file severity)))
                          (when dep-violations
                            (setf violations (nconc violations dep-violations)))))))

                   ((string-equal key-name "COMPONENTS")
                    (when (consp value)
                      (setf violations (nconc violations
                                              (check-components-list value form file severity)))))

                   ((string-equal key-name "IN-ORDER-TO")
                    (when (consp value)
                      (setf violations (nconc violations
                                              (check-in-order-to-list value form file severity))))))))

    (nreverse violations)))
