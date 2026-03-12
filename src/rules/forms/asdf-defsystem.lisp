(defpackage #:mallet/rules/forms/asdf-defsystem
  (:use #:cl)
  (:local-nicknames
   (#:base #:mallet/rules/base)
   (#:violation #:mallet/violation)
   (#:parser #:mallet/parser)
   (#:utils #:mallet/utils))
  (:export #:asdf-redundant-package-prefix-rule
           #:asdf-operate-in-perform-rule
           #:asdf-secondary-system-name-rule
           #:asdf-if-feature-keyword-rule))
(in-package #:mallet/rules/forms/asdf-defsystem)

;;; Shared helpers

(defun make-violation-at-value (value form file rule-name severity message)
  "Create a violation for VALUE at its position in FORM."
  (let ((position-map (parser:form-position-map form)))
    (multiple-value-bind (line column)
        (parser:find-position value position-map
                              (parser:form-line form)
                              (parser:form-column form))
      (make-instance 'violation:violation
                     :rule rule-name
                     :file file
                     :line line
                     :column column
                     :severity severity
                     :message message))))

;;; Rule: asdf-redundant-package-prefix

(defclass asdf-redundant-package-prefix-rule (base:rule)
  ()
  (:default-initargs
   :name :asdf-redundant-package-prefix
   :description "Package prefixes asdf:, cl:, common-lisp:, uiop: are redundant in .asd files"
   :severity :info
   :category :style
   :type :form
   :file-types '(:asd))
  (:documentation "Rule to detect redundant package prefixes in .asd files.
.asd files run in the asdf-user package which already uses asdf, cl, and uiop,
so qualifying symbols with those package names is unnecessary."))

(defparameter *redundant-packages*
  '("ASDF" "CL" "COMMON-LISP" "UIOP")
  "Package names that are redundant in .asd files.")

(defun redundant-package-prefix-p (str)
  "Return T if STR is a package-qualified symbol with a redundant prefix.
Handles asdf, cl, common-lisp, uiop, and their sub-packages (uiop/...)."
  (when (and (stringp str)
             (find #\: str :test #'char=)
             (not (utils:keyword-string-p str)))
    (let* ((colon-pos (position #\: str :test #'char=))
           (pkg-part (string-upcase (subseq str 0 colon-pos))))
      ;; Check exact match or sub-package (ASDF/... or UIOP/...)
      (or (member pkg-part *redundant-packages* :test #'string=)
          ;; Sub-packages like ASDF/COMPONENT, UIOP/FILESYSTEM etc.
          (let ((slash-pos (position #\/ pkg-part :test #'char=)))
            (when slash-pos
              (member (subseq pkg-part 0 slash-pos) *redundant-packages* :test #'string=)))))))

(defun extract-package-name (str)
  "Extract the package prefix from a qualified symbol string."
  (let ((colon-pos (position #\: str :test #'char=)))
    (when colon-pos
      (string-downcase (subseq str 0 colon-pos)))))

(defun collect-redundant-prefix-violations (expr form file severity visited)
  "Walk EXPR recursively, collecting violations for redundant package prefixes.
Checks every symbol position (not just heads) for redundant package prefixes."
  (let ((violations '()))
    (labels ((check-atom (a)
               (when (redundant-package-prefix-p a)
                 (push (make-violation-at-value
                        a form file :asdf-redundant-package-prefix severity
                        (format nil "Redundant package prefix ~A: is unnecessary in .asd files"
                                (extract-package-name a)))
                       violations)))
             (walk (e)
               (cond
                 ((stringp e)
                  (check-atom e))
                 ((and (consp e)
                       (not (gethash e visited)))
                  (setf (gethash e visited) t)
                  (dolist (subexpr e)
                    (walk subexpr))))))
      (walk expr))
    violations))

(defmethod base:check-form ((rule asdf-redundant-package-prefix-rule) form file)
  "Check all forms in .asd files for redundant package prefixes."
  (when (string-equal (pathname-type file) "asd")
    (let ((expr (parser:form-expr form))
          (visited (make-hash-table :test 'eq)))
      (nreverse (collect-redundant-prefix-violations expr form file
                                                     (base:rule-severity rule)
                                                     visited)))))

;;; Rule: asdf-operate-in-perform

(defclass asdf-operate-in-perform-rule (base:rule)
  ()
  (:default-initargs
   :name :asdf-operate-in-perform
   :description "Do not call asdf:operate or related functions inside :perform bodies"
   :severity :warning
   :category :practice
   :type :form
   :file-types '(:asd))
  (:documentation "Rule to detect asdf operate calls inside :perform method bodies.
Calling asdf:load-system, asdf:test-system, etc. inside :perform can cause
infinite loops or unexpected behavior. Use symbol-call instead."))

(defparameter *forbidden-operate-functions*
  '("OPERATE" "OOS" "LOAD-SYSTEM" "TEST-SYSTEM" "CLEAR-SYSTEM"
    "REQUIRE-SYSTEM" "MAKE" "COMPILE-SYSTEM")
  "ASDF function names that should not be called inside :perform.")

(defun forbidden-operate-call-p (str)
  "Return T if STR is a call to a forbidden asdf operate function."
  (when (and (stringp str)
             (find #\: str :test #'char=)
             (not (utils:keyword-string-p str)))
    (let* ((colon-pos (position #\: str :test #'char=))
           (pkg-part (string-upcase (subseq str 0 colon-pos)))
           (name-part (string-upcase (subseq str (1+ colon-pos)))))
      (and (or (string= pkg-part "ASDF")
               ;; sub-package like asdf/operate
               (let ((slash-pos (position #\/ pkg-part :test #'char=)))
                 (and slash-pos
                      (string= (subseq pkg-part 0 slash-pos) "ASDF"))))
           (member name-part *forbidden-operate-functions* :test #'string=)))))

(defun collect-operate-violations-in-body (body form file severity visited)
  "Walk BODY expressions, collecting violations for forbidden operate calls."
  (let ((violations '()))
    (labels ((walk (e)
               (when (and (consp e)
                          (not (gethash e visited)))
                 (setf (gethash e visited) t)
                 (let ((head (first e)))
                   (when (forbidden-operate-call-p head)
                     (push (make-violation-at-value
                            head form file :asdf-operate-in-perform severity
                            (format nil "Do not call ~A inside :perform; use symbol-call instead"
                                    head))
                           violations))
                   (dolist (subexpr (rest e))
                     (walk subexpr))))))
      (dolist (expr body)
        (walk expr)))
    violations))

(defun perform-body-forms (perform-clause)
  "Extract the body forms from a :perform clause.
Structure: (:perform (op-type (o c) body-forms...))"
  (when (and (consp perform-clause)
             (>= (length perform-clause) 2))
    (let ((method-form (second perform-clause)))
      ;; method-form is like (test-op (o c) body...)
      (when (and (consp method-form)
                 (>= (length method-form) 3))
        (cddr method-form)))))

(defun collect-perform-violations (expr form file severity visited-outer)
  "Collect violations from :perform clauses in EXPR (a plist portion of defsystem)."
  (let ((violations '()))
    (loop for (key value) on expr by #'cddr
          when (and (stringp key)
                    (string-equal key ":perform"))
            do (let ((body (perform-body-forms (list key value))))
                 (when body
                   (let ((inner-visited (make-hash-table :test 'eq)))
                     (setf violations
                           (nconc violations
                                  (collect-operate-violations-in-body
                                   body form file severity inner-visited))))))
          when (and (stringp key)
                    (string-equal (utils:symbol-name-from-string key) "COMPONENTS")
                    (consp value))
            do (dolist (component value)
                 (when (and (consp component)
                            (not (gethash component visited-outer)))
                   (setf (gethash component visited-outer) t)
                   (setf violations
                         (nconc violations
                                (collect-perform-violations (cddr component)
                                                            form file severity
                                                            visited-outer))))))
    violations))

(defmethod base:check-form ((rule asdf-operate-in-perform-rule) form file)
  "Check defsystem forms for asdf:operate calls inside :perform bodies."
  (when (string-equal (pathname-type file) "asd")
    (let ((expr (parser:form-expr form)))
      (when (and (consp expr)
                 (stringp (first expr))
                 (base:symbol-matches-p (first expr) "DEFSYSTEM"))
        (let ((visited (make-hash-table :test 'eq)))
          (nreverse
           (collect-perform-violations (cddr expr) form file
                                       (base:rule-severity rule) visited)))))))

;;; Rule: asdf-secondary-system-name

(defclass asdf-secondary-system-name-rule (base:rule)
  ()
  (:default-initargs
   :name :asdf-secondary-system-name
   :description "Secondary system names must follow the primary/suffix convention"
   :severity :warning
   :category :style
   :type :form
   :file-types '(:asd))
  (:documentation "Rule to detect secondary system names that don't follow the
primary/suffix convention. In a foo.asd file, systems other than 'foo' must be
named 'foo/something'. Arbitrary names like 'foo-tests' or 'bar' are not allowed
by ASDF and may cause issues with system resolution."))

(defun asd-file-primary-name (file)
  "Return the primary system name from FILE (the .asd filename without extension).
Returns a lowercase string."
  (string-downcase (pathname-name file)))

(defun secondary-system-name-valid-p (system-name primary-name)
  "Return T if SYSTEM-NAME is valid for a file with PRIMARY-NAME.
Valid: exactly matches primary-name, or starts with 'primary-name/'."
  (let ((lower-system (string-downcase (utils:symbol-name-from-string system-name)))
        (lower-primary (string-downcase primary-name)))
    (or (string= lower-system lower-primary)
        (and (> (length lower-system) (1+ (length lower-primary)))
             (string= (subseq lower-system 0 (1+ (length lower-primary)))
                      (concatenate 'string lower-primary "/"))))))

(defmethod base:check-form ((rule asdf-secondary-system-name-rule) form file)
  "Check defsystem names follow the primary/suffix convention."
  (when (string-equal (pathname-type file) "asd")
    (let ((expr (parser:form-expr form)))
      (when (and (consp expr)
                 (stringp (first expr))
                 (base:symbol-matches-p (first expr) "DEFSYSTEM")
                 (>= (length expr) 2))
        (let ((system-name (second expr))
              (primary-name (asd-file-primary-name file)))
          (when (and (stringp system-name)
                     (not (secondary-system-name-valid-p system-name primary-name)))
            (let ((display-name (string-downcase
                                 (utils:symbol-name-from-string system-name))))
              (list (make-violation-at-value
                     system-name form file :asdf-secondary-system-name
                     (base:rule-severity rule)
                     (format nil
                             "Secondary system ~S should be named ~S or follow ~S convention"
                             display-name
                             primary-name
                             (concatenate 'string primary-name "/suffix")))))))))))

;;; Rule: asdf-if-feature-keyword

(defclass asdf-if-feature-keyword-rule (base:rule)
  ()
  (:default-initargs
   :name :asdf-if-feature-keyword
   :description "Feature expressions in :if-feature and (:feature ...) must use keywords"
   :severity :warning
   :category :correctness
   :type :form
   :file-types '(:asd))
  (:documentation "Rule to detect plain symbols instead of keywords in feature expressions.
Feature expressions like :if-feature and (:feature ...) in defsystem must use
keywords (e.g., :sbcl, :unix) not plain symbols (e.g., sbcl, unix).
Plain symbols are evaluated as variables and will likely cause errors."))

(defun feature-symbol-valid-p (sym)
  "Return T if SYM is a valid feature symbol (keyword string or compound list)."
  (or (utils:keyword-string-p sym)
      (consp sym)))

(defun collect-feature-expr-violations (feat-expr form file severity violations)
  "Recursively check FEAT-EXPR for non-keyword feature symbols.
Handles compound expressions: (:and ...), (:or ...), (:not ...).
Pushes violations onto the VIOLATIONS list (by side effect via nconc).
Returns the (possibly extended) violations list."
  (cond
    ;; NIL or non-string atom - skip
    ((null feat-expr) violations)

    ;; Compound expression - recurse into sub-expressions
    ((consp feat-expr)
     (let ((head (first feat-expr)))
       (when (and (stringp head)
                  (or (string-equal head ":and")
                      (string-equal head ":or")
                      (string-equal head ":not")))
         (dolist (sub (rest feat-expr))
           (setf violations
                 (collect-feature-expr-violations sub form file severity violations))))
       violations))

    ;; String - check if it's a keyword
    ((stringp feat-expr)
     (if (feature-symbol-valid-p feat-expr)
         violations
         (append violations
                 (list (make-violation-at-value
                        feat-expr form file :asdf-if-feature-keyword severity
                        (format nil
                                "Feature symbol ~S should be a keyword (e.g., :~A)"
                                (string-downcase (utils:symbol-name-from-string feat-expr))
                                (string-downcase (utils:symbol-name-from-string feat-expr))))))))

    (t violations)))

(defun check-if-feature-in-plist (plist form file severity)
  "Check :if-feature values in PLIST for non-keyword feature symbols."
  (let ((violations '()))
    (loop for (key value) on plist by #'cddr
          when (and (stringp key)
                    (string-equal key ":if-feature"))
            do (setf violations
                     (collect-feature-expr-violations value form file severity violations)))
    violations))

(defun check-feature-in-depends-on (deps form file severity)
  "Check (:feature ...) forms in DEPS list for non-keyword feature symbols."
  (let ((violations '()))
    (dolist (dep deps)
      (when (and (consp dep)
                 (>= (length dep) 3)
                 (stringp (first dep))
                 (string-equal (utils:symbol-name-from-string (first dep)) "FEATURE"))
        ;; (:feature feat-expr dep) - feat-expr is second element
        (let ((feat-expr (second dep)))
          (setf violations
                (collect-feature-expr-violations feat-expr form file severity violations)))))
    violations))

(defun check-if-feature-in-components (components form file severity)
  "Check :if-feature in COMPONENTS list (recursively)."
  (let ((violations '()))
    (dolist (component components)
      (when (consp component)
        ;; Check :if-feature in component plist (cddr is the plist)
        (setf violations
              (nconc violations
                     (check-if-feature-in-plist (cddr component) form file severity)))
        ;; Recurse into nested :components
        (loop for (key value) on (cddr component) by #'cddr
              when (and (stringp key)
                        (string-equal (utils:symbol-name-from-string key) "COMPONENTS")
                        (consp value))
                do (setf violations
                         (nconc violations
                                (check-if-feature-in-components value form file severity))))))
    violations))

(defmethod base:check-form ((rule asdf-if-feature-keyword-rule) form file)
  "Check defsystem forms for non-keyword feature symbols."
  (when (string-equal (pathname-type file) "asd")
    (let ((expr (parser:form-expr form)))
      (when (and (consp expr)
                 (stringp (first expr))
                 (base:symbol-matches-p (first expr) "DEFSYSTEM"))
        (let ((plist (cddr expr))
              (violations '()))
          ;; Check :if-feature in top-level defsystem plist
          (setf violations
                (nconc violations
                       (check-if-feature-in-plist plist form file
                                                  (base:rule-severity rule))))
          ;; Check (:feature ...) in :depends-on
          (loop for (key value) on plist by #'cddr
                when (and (stringp key)
                          (string-equal (utils:symbol-name-from-string key) "DEPENDS-ON")
                          (consp value))
                  do (setf violations
                           (nconc violations
                                  (check-feature-in-depends-on value form file
                                                               (base:rule-severity rule)))))
          ;; Check :if-feature in :components (recursively)
          (loop for (key value) on plist by #'cddr
                when (and (stringp key)
                          (string-equal (utils:symbol-name-from-string key) "COMPONENTS")
                          (consp value))
                  do (setf violations
                           (nconc violations
                                  (check-if-feature-in-components value form file
                                                                   (base:rule-severity rule)))))
          violations)))))
