(defpackage #:mallet/suppression
  (:use #:cl)
  (:local-nicknames
   (#:utils #:mallet/utils))
  (:export #:suppression-state
           #:make-suppression-state
           #:region-disabled-rules
           #:scope-stack
           #:function-suppressions
           #:next-form-suppressions
           #:push-scope-suppression
           #:pop-scope-suppression
           #:add-function-suppression
           #:set-region-disabled
           #:enable-region-rules
           #:set-next-form-suppression
           #:consume-next-form-suppression
           #:rule-suppressed-p
           #:ensure-mallet-package-exists
           ;; Declaration recognition
           #:mallet-declaim-p
           #:mallet-declaration-p
           #:parse-mallet-declaration
           #:extract-mallet-declare
           ;; Form processing
           #:update-suppression-for-declaim
           #:mallet-suppress-next-p
           #:extract-suppress-next-rules
           ;; Comment directive parsing
           #:parse-comment-directives
           ;; Suppression registration and stale detection
           #:register-suppression
           #:mark-suppression-used
           #:collect-stale-suppressions
           ;; State accessors needed by engine for lifecycle management
           #:registered-suppressions
           #:used-suppression-ids))
(in-package #:mallet/suppression)

(defclass suppression-state ()
  ((region-disabled-rules
    :initform nil
    :accessor region-disabled-rules
    :documentation "Rules disabled for current region (via disable/enable)")
   (next-form-suppressions
    :initform nil
    :accessor next-form-suppressions
    :documentation "Rules to suppress for next form only (via suppress-next)")
   (function-suppressions
    :initform (make-hash-table :test 'equal)
    :accessor function-suppressions
    :documentation "Hash table: function-name -> list of suppressed rules")
   (scope-stack
    :initform nil
    :accessor scope-stack
    :documentation "Stack of scope-level suppressions (via declare)")
   (registered-suppressions
    :initform nil
    :accessor registered-suppressions
    :documentation "Alist of registered suppression entries: ((ID :line LINE :rules RULES :reason REASON :type TYPE) ...)
    Each entry is a cons of (INTEGER-ID . PLIST) as created by REGISTER-SUPPRESSION.")
   (used-suppression-ids
    :initform (make-hash-table :test 'eql)
    :accessor used-suppression-ids
    :documentation "Hash table (test 'eql) used as a set of used suppression IDs")
   (next-suppression-id
    :initform 0
    :accessor next-suppression-id
    :documentation "Counter for generating unique suppression IDs"))
  (:documentation "Tracks suppression state during parsing of a single file"))

(defun make-suppression-state ()
  "Create a new suppression state for linting a file."
  (make-instance 'suppression-state))

;;; Scope Stack Management

(defun push-scope-suppression (state rules)
  "Push a new scope-level suppression onto the stack."
  (check-type state suppression-state)
  (check-type rules list)
  (push rules (scope-stack state)))

(defun pop-scope-suppression (state)
  "Pop the current scope-level suppression."
  (check-type state suppression-state)
  (pop (scope-stack state)))

;;; Region-Based Suppression

(defun set-region-disabled (state rules)
  "Add rules to the currently disabled set for the region."
  (check-type state suppression-state)
  (check-type rules list)
  (setf (region-disabled-rules state)
        (union (region-disabled-rules state) rules :test #'eq)))

(defun enable-region-rules (state rules)
  "Remove rules from the currently disabled set for the region."
  (check-type state suppression-state)
  (check-type rules list)
  (setf (region-disabled-rules state)
        (set-difference (region-disabled-rules state) rules :test #'eq)))

;;; Function-Specific Suppression

(defun add-function-suppression (state function-name rules)
  "Add suppression for a specific function."
  (check-type state suppression-state)
  (check-type rules list)
  (let ((existing (gethash function-name (function-suppressions state))))
    (setf (gethash function-name (function-suppressions state))
          (union existing rules :test #'eq))))

;;; Next-Form Suppression

(defun set-next-form-suppression (state rules)
  "Set rules to suppress for the next form only."
  (check-type state suppression-state)
  (check-type rules list)
  (setf (next-form-suppressions state) rules))

(defun consume-next-form-suppression (state)
  "Get and clear next-form suppression (one-time use)."
  (check-type state suppression-state)
  (let ((rules (next-form-suppressions state)))
    (setf (next-form-suppressions state) nil)
    rules))

;;; Suppression Registration and Stale Detection

(defun register-suppression (state line rules reason type)
  "Register an inline suppression. Returns a unique integer ID.

   LINE: source line number where the suppression was found
   RULES: list of rule keywords (or '(:all))
   REASON: optional reason string or NIL
   TYPE: suppression type keyword (e.g., :inline-comment)

   The entry is stored as (ID :line LINE :rules RULES :reason REASON :type TYPE)."
  (check-type state suppression-state)
  (check-type rules list)
  (let ((id (incf (next-suppression-id state))))
    (push (cons id (list :line line :rules rules :reason reason :type type))
          (registered-suppressions state))
    id))

(defun mark-suppression-used (state id)
  "Mark the suppression with the given ID as used."
  (check-type state suppression-state)
  (setf (gethash id (used-suppression-ids state)) t))

(defun collect-stale-suppressions (state)
  "Return list of registered suppression entries that were never used.

   Each entry is a cons (ID . PLIST) where PLIST has :line, :rules, :reason, :type.
   Entries are returned in reverse-registration order (newest first), because
   suppressions are stored newest-first via PUSH."
  (check-type state suppression-state)
  (remove-if (lambda (entry)
               (gethash (car entry) (used-suppression-ids state)))
             (registered-suppressions state)))

;;; Suppression Checking

(defun rule-suppressed-p (state rule-name &key form-type function-name)
  "Check if a rule is currently suppressed.

   Returns the suppression ID (integer) when matched by a registered suppression,
   T when matched by region/function/scope suppression, or NIL when not suppressed.
   As a side effect, marks the first matching registered suppression as used.
   When multiple registered suppressions match the same rule, only the most recently
   registered one is consumed (suppressions are stored newest-first internally).

   form-type: :top-level, :function-body, :lexical-scope
   function-name: If form-type is :function-body, the function name"
  (check-type state suppression-state)
  (or
   ;; 1. Check registered suppressions (inline comment suppressions)
   (loop for entry in (registered-suppressions state)
         when (let ((rules (getf (cdr entry) :rules)))
                (or (member rule-name rules :test #'eq)
                    (member :all rules :test #'eq)))
           do (mark-suppression-used state (car entry))
              (return (car entry)))

   ;; 2. Check region-level suppression
   (and (or (member rule-name (region-disabled-rules state) :test #'eq)
            (member :all (region-disabled-rules state) :test #'eq))
        t)

   ;; 3. Check function-specific suppression
   (when (and function-name (eq form-type :function-body))
     (let ((suppressed (gethash function-name (function-suppressions state))))
       (and (or (member rule-name suppressed :test #'eq)
                (member :all suppressed :test #'eq))
            t)))

   ;; 4. Check scope stack (any parent scope)
   (and (some (lambda (scope-rules)
                (or (member rule-name scope-rules :test #'eq)
                    (member :all scope-rules :test #'eq)))
              (scope-stack state))
        t)))

;;; Declaration Recognition

(defun mallet-declaim-p (form)
  "Check if FORM is a mallet declaim.

   Returns T if FORM is a (declaim ...) form containing at least one mallet declaration."
  (and (consp form)
       ;; Handle both actual symbols and string representations from parser
       (or (eq (first form) 'declaim)
           (and (stringp (first form))
                (or (string= (first form) "declaim")
                    (string= (first form) "CURRENT:declaim")
                    (string= (first form) "CL:declaim"))))
       (some #'mallet-declaration-p (rest form))))

(defun mallet-declaration-p (declaration)
  "Check if DECLARATION is a mallet declaration.

   Returns T if DECLARATION is a form like (mallet:disable ...) or (mallet:enable ...)."
  (when (consp declaration)
    (let ((first-elem (first declaration)))
      (cond
        ;; Handle actual symbols (from test environment)
        ((symbolp first-elem)
         (let ((pkg (symbol-package first-elem)))
           (and pkg
                (string= (package-name pkg) "MALLET")
                (member (symbol-name first-elem)
                        '("DISABLE" "ENABLE" "SUPPRESS-NEXT" "SUPPRESS-FUNCTION")
                        :test #'string=))))
        ;; Handle string representations from parser (e.g., "MALLET:suppress-next")
        ((stringp first-elem)
         (and (or (search "MALLET:disable" first-elem :test #'char-equal)
                  (search "MALLET:enable" first-elem :test #'char-equal)
                  (search "MALLET:suppress-next" first-elem :test #'char-equal)
                  (search "MALLET:suppress-function" first-elem :test #'char-equal))
              t))))))

(defun parse-mallet-declaration (declaration)
  "Parse a mallet declaration and return (values type rules function-names).

   TYPE is one of: :disable, :enable, :suppress-next, :suppress-function
   RULES is a list of rule keywords (or (:all))
   FUNCTION-NAMES is a list of function names (only for :suppress-function)"
  (check-type declaration cons)
  (let* ((type-elem (first declaration))
         (args (rest declaration))
         ;; Extract type name from symbol or string
         (type-name (cond
                      ((symbolp type-elem) (symbol-name type-elem))
                      ((stringp type-elem)
                       ;; Handle "MALLET:suppress-next" -> "SUPPRESS-NEXT"
                       (let ((colon-pos (position #\: type-elem :from-end t :test #'char=)))
                         (if colon-pos
                             (subseq type-elem (1+ colon-pos))
                             type-elem)))
                      ;; Handle Eclector objects or other non-string/non-symbol forms
                      ((and (consp type-elem) (symbolp (first type-elem)))
                       (format nil "~A" (first type-elem)))
                      (t (error "Invalid type element: ~A" type-elem))))
         (type (intern (string-upcase type-name) :keyword)))
    (ecase type
      ((:disable :enable :suppress-next)
       ;; (mallet:disable rule1 rule2 ...)
       ;; Convert all args to keywords
       (values type
               (loop for arg in args
                     collect (cond
                               ;; Already a keyword - return as-is
                               ((keywordp arg) arg)
                               ;; Symbol - intern in keyword package
                               ((symbolp arg)
                                (intern (symbol-name arg) :keyword))
                               ;; String from parser - convert to keyword
                               ;; Strip leading colon if present
                               ((stringp arg)
                                (let ((name (string-upcase arg)))
                                  (intern (if (utils:keyword-string-p name)
                                              (subseq name 1)
                                              name)
                                          :keyword)))
                               (t arg)))
               nil))

      (:suppress-function
       ;; (mallet:suppress-function rule fn1 fn2 ...)
       ;; First arg is rule (convert to keyword), rest are function names (keep as-is)
       (let ((rule-arg (first args))
             (fn-names (rest args)))
         (values type
                 (list (cond
                         ;; Already a keyword - return as-is
                         ((keywordp rule-arg) rule-arg)
                         ;; Symbol - intern in keyword package
                         ((symbolp rule-arg)
                          (intern (symbol-name rule-arg) :keyword))
                         ;; String from parser - convert to keyword
                         ((stringp rule-arg)
                          (let ((name (string-upcase rule-arg)))
                            (intern (if (utils:keyword-string-p name)
                                        (subseq name 1)
                                        name)
                                    :keyword)))
                         (t rule-arg)))
                 fn-names))))))

(defun extract-mallet-declare (form)
  "Extract mallet:suppress declarations from a DECLARE form.

   Returns list of rule keywords, or NIL if no mallet declarations.
   Example: (declare (mallet:suppress line-length unused-variables))
   Returns: (:line-length :unused-variables)"
  (when (and (consp form) (eq (first form) 'declare))
    (loop for declaration in (rest form)
          when (and (consp declaration)
                    (symbolp (first declaration))
                    (let ((pkg (symbol-package (first declaration))))
                      (and pkg (string= (package-name pkg) "MALLET")))
                    (string= (symbol-name (first declaration)) "SUPPRESS"))
            append (rest declaration))))

;;; Form Processing Helpers

(defun mallet-suppress-next-p (declaim-form)
  "Check if DECLAIM-FORM contains a suppress-next declaration."
  (when (and (consp declaim-form)
             (or (eq (first declaim-form) 'declaim)
                 (and (stringp (first declaim-form))
                      (or (string= (first declaim-form) "declaim")
                          (string= (first declaim-form) "CURRENT:declaim")
                          (string= (first declaim-form) "CL:declaim")))))
    (some (lambda (declaration)
            (when (consp declaration)
              (let ((first-elem (first declaration)))
                (cond
                  ((symbolp first-elem)
                   (let ((pkg (symbol-package first-elem)))
                     (and pkg
                          (string= (package-name pkg) "MALLET")
                          (string= (symbol-name first-elem) "SUPPRESS-NEXT"))))
                  ((stringp first-elem)
                   (search "MALLET:suppress-next" first-elem :test #'char-equal))))))
          (rest declaim-form))))

(defun extract-suppress-next-rules (declaim-form)
  "Extract rules from a suppress-next declaration.

   Returns a list of rule keywords to suppress for the next form."
  (loop for declaration in (rest declaim-form)
        when (and (consp declaration)
                  (let ((first-elem (first declaration)))
                    (cond
                      ((symbolp first-elem)
                       (let ((pkg (symbol-package first-elem)))
                         (and pkg
                              (string= (package-name pkg) "MALLET")
                              (string= (symbol-name first-elem) "SUPPRESS-NEXT"))))
                      ((stringp first-elem)
                       (search "MALLET:suppress-next" first-elem :test #'char-equal)))))
          append (loop for arg in (rest declaration)
                       collect (cond
                                 ;; Already a keyword - return as-is
                                 ((keywordp arg) arg)
                                 ;; Symbol - intern in keyword package
                                 ((symbolp arg)
                                  (intern (symbol-name arg) :keyword))
                                 ;; String from parser - convert to keyword
                                 ;; Strip leading colon if present
                                 ((stringp arg)
                                  (let ((name (string-upcase arg)))
                                    (intern (if (utils:keyword-string-p name)
                                                (subseq name 1)
                                                name)
                                            :keyword)))
                                 (t arg)))))

(defun update-suppression-for-declaim (declaim-form state)
  "Update SUPPRESSION-STATE for disable/enable/suppress-function declarations.

   Does NOT handle suppress-next (that's handled separately in the main loop).
   Returns the updated state."
  (loop for declaration in (rest declaim-form)
        when (mallet-declaration-p declaration)
          do (multiple-value-bind (type rules function-names)
                 (parse-mallet-declaration declaration)
               (ecase type
                 (:disable
                  (set-region-disabled state rules))

                 (:enable
                  (enable-region-rules state rules))

                 (:suppress-function
                  (loop for fn-name in function-names
                        do (add-function-suppression state fn-name rules)))

                 ;; suppress-next is handled separately in process-top-level-forms
                 (:suppress-next
                  nil))))
  state)

;;; Stub Package Creation

(defun ensure-mallet-package-exists ()
  "Ensure mallet package has symbols for reading #+mallet declarations.

   This ensures the mallet package exports symbols for suppression declarations.
   The package may already exist (as the main Mallet linter package), so we just
   ensure the necessary symbols are interned and exported."
  (let ((pkg (or (find-package '#:mallet)
                 (make-package '#:mallet :use '()))))
    ;; Intern and export the declaration symbols
    ;; Use dolist to intern each symbol and export it
    (dolist (symbol-name '("DISABLE" "ENABLE" "SUPPRESS" "SUPPRESS-NEXT" "SUPPRESS-FUNCTION"))
      (let ((sym (intern symbol-name pkg)))
        (export sym pkg)))
    pkg))

;;; Comment Directive Parsing

(defun parse-comment-directives (source-text)
  "Parse mallet comment directives from SOURCE-TEXT.

   Scans each line for patterns like:
     ; mallet:suppress rule1 rule2 -- optional reason
     ;; mallet:disable rule1
     ;;; mallet:enable rule1
     (form ...) ; mallet:suppress rule1   (trailing same-line comment)

   Returns a list of (line-number type rules reason) sorted by line-number, where:
   - line-number is 1-based
   - type is :suppress, :disable, or :enable
   - rules is a list of keyword symbols (e.g., :needless-let*)
   - reason is a string or NIL (parsed from text after '--')"
  (let ((result nil)
        (line-number 0))
    (with-input-from-string (stream source-text)
      (loop for line = (read-line stream nil nil)
            while line
            do (incf line-number)
               (multiple-value-bind (matched groups)
                   (cl-ppcre:scan-to-strings
                    ";+\\s*mallet:(suppress|disable|enable)\\s*(.*)"
                    line)
                 (when matched
                   (let* ((type-str (aref groups 0))
                          (rest-str (string-trim " " (aref groups 1)))
                          (type (intern (string-upcase type-str) :keyword))
                          (reason nil)
                          (rules-str rest-str))
                     ;; Split out optional reason after " -- " (space-bounded to avoid
                     ;; matching "--" embedded within rule names like "my--rule")
                     (let ((sep-pos (cl-ppcre:scan "\\s+--(?:\\s|$)" rules-str)))
                       (when sep-pos
                         (let ((reason-part (string-trim " "
                                              (cl-ppcre:regex-replace "^.*?\\s+--\\s*" rules-str ""))))
                           (setf reason (if (string= reason-part "") nil reason-part)))
                         (setf rules-str (string-trim " " (subseq rules-str 0 sep-pos)))))
                     ;; Split rules by whitespace, filtering empty strings
                     (let ((rule-strings
                             (remove-if #'(lambda (s) (string= s ""))
                                        (cl-ppcre:split "\\s+" rules-str))))
                       ;; Silently ignore directives with no rules
                       (when rule-strings
                         (let ((rules (mapcar (lambda (r)
                                               (let ((name (string-upcase r)))
                                                 (intern (if (utils:keyword-string-p name)
                                                             (subseq name 1)
                                                             name)
                                                         :keyword)))
                                             rule-strings)))
                           (push (list line-number type rules reason) result)))))))))
    (sort result #'< :key #'first)))
