(defpackage #:mallet/rules/forms/asdf
  (:use #:cl)
  (:local-nicknames
   (#:a #:alexandria)
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
   :severity :convention
   :type :form))

(defmethod base:check-form ((rule asdf-component-strings-rule) form file)
  "Check that defsystem uses strings for names."
  ;; Only check .asd files
  (when (string-equal (pathname-type file) "asd")
    (let ((expr (parser:form-expr form)))
      (when (and (consp expr)
                 (stringp (first expr)))
        (let ((operator (base:symbol-name-from-string (first expr))))
          (when (base:symbol-matches-p operator "DEFSYSTEM")
            (check-defsystem-source form file (base:rule-severity rule))))))))

;;; Helper functions

(defun looks-like-symbol-p (str &optional source)
  "Check if STR looks like it was parsed from a symbol (not a string literal).
The parser converts symbols to strings, so we check for patterns like:
  - ':ALEXANDRIA' (keyword)
  - 'CURRENT:FOO' (package-qualified symbol)
For plain strings like 'main', check SOURCE for '#:' pattern if provided.
Returns NIL for non-string inputs (like Eclector objects)."
  (and (stringp str)
       (or
        ;; Keyword: starts with ":"
        (utils:keyword-string-p str)
        ;; Package-qualified: has ":" but not at start (e.g., "CURRENT:FOO")
        (and (find #\: str :test #'char=)  ; Be explicit about test function
             (not (utils:keyword-string-p str)))
        ;; Plain string - check source for #: pattern if available
        (and source
             (stringp source)
             (let* ((name (extract-symbol-name str))
                    (pattern (format nil "#:~A" name)))
               (search pattern source :test #'char-equal))))))

(defun extract-symbol-name (str)
  "Extract the base name from a symbol string for display.
Examples: ':ALEXANDRIA' -> 'alexandria', 'CURRENT:FOO' -> 'foo'
Handles non-string inputs (like Eclector objects) gracefully by returning a default value."
  (if (not (stringp str))
      ;; Not a string - try to get a reasonable representation
      (if (and (consp str) (symbolp (first str)))
          (format nil "~A" (first str))  ; Best effort for Eclector objects
          (format nil "~A" str))          ; Last resort - princ it
      ;; Normal string processing
      (cond
        ;; Keyword: ":ALEXANDRIA" -> "alexandria"
        ((utils:keyword-string-p str)
         (string-downcase (subseq str 1)))
        ;; Uninterned: "#:FOO" -> "foo"
        ((and (> (length str) 1) (char= (char str 0) #\#) (char= (char str 1) #\:))
         (string-downcase (subseq str 2)))
        ;; Package-qualified: "CURRENT:FOO" -> "foo"
        ((find #\: str :test #'char=)
         (string-downcase (subseq str (1+ (position #\: str :from-end t :test #'char=)))))
        ;; Plain: "FOO" -> "foo"
        (t (string-downcase str)))))

(defun format-symbol-for-message (str source)
  "Format STR as it appeared in SOURCE for error messages.
Examples: 'my-system' with '#:my-system' in source -> '#:my-system'
          ':alexandria' -> ':alexandria'
          'CURRENT:foo' -> 'foo'"
  (let ((name (extract-symbol-name str)))
    (cond
      ;; Check if it's a keyword in the parsed string
      ((utils:keyword-string-p str)
       (format nil ":~A" name))
      ;; Check source for #: pattern
      ((and source (stringp source)
            (search (format nil "#:~A" name) source :test #'char-equal))
       (format nil "#:~A" name))
      ;; Plain symbol (including package-qualified ones) - just show the name
      (t name))))

(defun find-symbol-in-source (symbol-str source start-offset)
  "Find SYMBOL-STR in SOURCE starting from START-OFFSET.
Returns (values line column) relative to start of SOURCE, or (values 0 0) if not found."
  (let* ((symbol-name (extract-symbol-name symbol-str))
         ;; Try different patterns the symbol might appear as
         (patterns (list
                    (format nil ":~A" symbol-name)  ; :alexandria
                    (format nil "#:~A" symbol-name) ; #:foo
                    symbol-name)))                   ; bare symbol
    (dolist (pattern patterns)
      (let ((pos (search pattern source :start2 start-offset :test #'char-equal)))
        (when pos
          ;; Calculate line and column
          (let ((line 0)
                (column 0))
            (loop for i from 0 below pos
                  for char = (char source i)
                  do (if (char= char #\Newline)
                         (progn (incf line) (setf column 0))
                         (incf column)))
            (return-from find-symbol-in-source (values line column))))))
    ;; Not found - return 0, 0
    (values 0 0)))

(defun create-violation (symbol-value form file severity message)
  "Create a violation for SYMBOL-VALUE, finding its position in source."
  (let ((source (parser:form-source form))
        (base-line (parser:form-line form))
        (base-column (parser:form-column form)))
    (multiple-value-bind (rel-line rel-column)
        (if source
            (find-symbol-in-source symbol-value source 0)
            (values 0 0))
      (make-instance 'violation:violation
                     :rule :asdf-component-strings
                     :file file
                     :line (+ base-line rel-line)
                     :column (if (zerop rel-line)
                                 (+ base-column rel-column)
                                 rel-column)
                     :severity severity
                     :message message))))

(defun check-dependency (dep form file severity)
  "Check a single dependency DEP for symbol usage.
Handles: simple-name | (:feature expr dep) | (:version name ver) | (:require name)"
  (let ((source (parser:form-source form)))
    (cond
      ;; Simple dependency - just a name string
      ((stringp dep)
       (when (looks-like-symbol-p dep source)
         (list (create-violation dep form file severity
                                 (format nil "Dependency should be string \"~A\", instead of symbol ~A"
                                         (extract-symbol-name dep)
                                         (format-symbol-for-message dep source))))))

      ;; Complex dependency - list form
      ((consp dep)
       (when (>= (length dep) 2)
         (let ((dep-type (first dep)))
           (when (stringp dep-type)
             (let ((type-name (base:symbol-name-from-string dep-type)))
               (cond
                 ;; (:version "system" "1.0") - check system name (2nd element)
                 ((string-equal type-name "VERSION")
                  (when (>= (length dep) 2)
                    (let ((sys-name (second dep)))
                      (when (looks-like-symbol-p sys-name source)
                        (list (create-violation sys-name form file severity
                                                (format nil "System name should be string \"~A\", instead of symbol ~A"
                                                        (extract-symbol-name sys-name)
                                                        (format-symbol-for-message sys-name source))))))))

                 ;; (:require "module") - check module name (2nd element)
                 ((string-equal type-name "REQUIRE")
                  (when (>= (length dep) 2)
                    (let ((mod-name (second dep)))
                      (when (looks-like-symbol-p mod-name source)
                        (list (create-violation mod-name form file severity
                                                (format nil "Module name should be string \"~A\", instead of symbol ~A"
                                                        (extract-symbol-name mod-name)
                                                        (format-symbol-for-message mod-name source))))))))

                 ;; (:feature expr dependency-def) - recursively check dependency-def (3rd element)
                 ((string-equal type-name "FEATURE")
                  (when (>= (length dep) 3)
                    (check-dependency (third dep) form file severity)))

                 ;; Unknown list form - don't flag
                 (t nil)))))))

      ;; Unknown form
      (t nil))))

(defun check-in-order-to-list (in-order-to-clauses form file severity)
  "Check :in-order-to clauses for symbol usage in system names.
Structure: ((operation (operation system-name) ...) ...)"
  (let ((violations '())
        (source (parser:form-source form)))
    (dolist (clause in-order-to-clauses)
      (when (consp clause)
        ;; Each clause: (operation (operation system-name) ...)
        ;; Skip first element (operation), check rest (dependencies)
        (dolist (dependency (rest clause))
          (when (and (consp dependency)
                     (>= (length dependency) 2))
            (let ((system-name (second dependency)))
              (when (looks-like-symbol-p system-name source)
                (let ((sym-name (extract-symbol-name system-name)))
                  (push (create-violation system-name form file severity
                                          (format nil "System name should be string \"~A\", instead of symbol ~A"
                                                  sym-name
                                                  (format-symbol-for-message system-name source)))
                        violations))))))))
    violations))

(defun check-components-list (components form file severity)
  "Check a list of component definitions recursively, returning violations.
Component-def structure: (component-type component-name option*)
- First element (component-type) can be any keyword (valid)
- Second element (component-name) should be a string
- Components can have :depends-on and nested :components"
  (let ((violations '())
        (source (parser:form-source form)))
    (dolist (component-def components)
      (when (consp component-def)
        ;; Check component name (second element) - should be string
        (when (>= (length component-def) 2)
          (let ((component-name (second component-def)))
            (when (looks-like-symbol-p component-name source)
              (let ((sym-name (extract-symbol-name component-name)))
                (push (create-violation component-name form file severity
                                        (format nil "Component name should be string \"~A\", instead of symbol ~A"
                                                sym-name
                                                (format-symbol-for-message component-name source)))
                      violations)))))

        ;; Check component options (starting at position 2)
        ;; Options: :depends-on, :components, etc.
        (loop for (key value) on (cddr component-def) by #'cddr
              when (stringp key)
                do (let ((key-name (base:symbol-name-from-string key)))
                     (cond
                       ;; Nested :components (in :module components)
                       ((string-equal key-name "COMPONENTS")
                        (when (consp value)
                          (setf violations (nconc violations
                                                  (check-components-list value form file severity)))))
                       ;; Component :depends-on
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
        (expr (parser:form-expr form))
        (source (parser:form-source form)))

    ;; Check system name (second element of defsystem form)
    (when (>= (length expr) 2)
      (let ((system-name (second expr)))
        (when (looks-like-symbol-p system-name source)
          (let ((sym-name (extract-symbol-name system-name)))
            (push (create-violation system-name form file severity
                                    (format nil "System name should be string \"~A\", instead of symbol ~A"
                                            sym-name
                                            (format-symbol-for-message system-name source)))
                  violations)))))

    ;; Parse options (everything after system name)
    ;; Structure: (defsystem system-name :option1 value1 :option2 value2 ...)
    (loop for (key value) on (cddr expr) by #'cddr
          when (stringp key)
            do (let ((key-name (base:symbol-name-from-string key)))
                 (cond
                   ;; Check :depends-on - dependencies can be simple names or complex forms
                   ((string-equal key-name "DEPENDS-ON")
                    (when (consp value)
                      (dolist (dep value)
                        (let ((dep-violations (check-dependency dep form file severity)))
                          (when dep-violations
                            (setf violations (nconc violations dep-violations)))))))

                   ;; Check :components - validate component definitions
                   ((string-equal key-name "COMPONENTS")
                    (when (consp value)
                      (setf violations (nconc violations
                                              (check-components-list value form file severity)))))

                   ;; Check :in-order-to - system names in dependencies
                   ((string-equal key-name "IN-ORDER-TO")
                    (when (consp value)
                      (setf violations (nconc violations
                                              (check-in-order-to-list value form file severity))))))))

    (nreverse violations)))
