(defpackage #:mallet/parser/loop
  (:use #:cl)
  (:local-nicknames
   (#:a #:alexandria)
   (#:utils #:mallet/utils))
  (:export
   #:loop-binding
   #:loop-binding-pattern
   #:loop-binding-init-form
   #:loop-binding-is-parallel
   #:parse-loop-clauses))
(in-package #:mallet/parser/loop)

;;; LOOP Clause Parser
;;;
;;; This module parses LOOP clauses to extract variable bindings and their
;;; initialization forms, handling parallelism (AND) and destructuring.
;;;
;;; Key challenges:
;;; 1. LOOP keywords are position-dependent: 'for' can be a keyword or a variable
;;;    Example: (loop for i from 0 to for) - second 'for' is a variable reference
;;; 2. Some clauses take multiple forms: initially, finally, do, doing
;;;    Example: (loop initially (setf x 1) (print "start") ...)
;;; 3. Destructuring patterns can be dotted pairs or proper lists
;;;    Example: (loop for (name . node) in items ...)
;;; 4. AND creates parallel bindings like LET (vs sequential like LET*)
;;;    Example: (loop for x = 1 and y = x ...) - y cannot reference x

(defstruct loop-binding
  "Information about a LOOP variable binding.
PATTERN: The variable pattern (string or cons for destructuring)
INIT-FORM: The initialization form/clause (list of expressions)
IS-PARALLEL: T if connected to PREVIOUS binding with AND (parallel like LET),
             NIL if sequential (can reference previous bindings like LET*)"
  pattern
  init-form
  is-parallel)

(defun loop-keyword-p (string)
  "Check if STRING is a LOOP clause keyword (position-independent check).
This does NOT mean the string will be treated as a keyword in context -
LOOP keywords are position-dependent. Use this only for parsing decisions."
  (member (utils:symbol-name-from-string string)
          '("NAMED" "WITH" "FOR" "AS" "AND"
            "INITIALLY" "FINALLY"
            "DO" "DOING" "RETURN"
            "COLLECT" "COLLECTING" "APPEND" "APPENDING"
            "NCONC" "NCONCING" "COUNT" "COUNTING"
            "SUM" "SUMMING" "MAXIMIZE" "MAXIMIZING"
            "MINIMIZE" "MINIMIZING"
            "IF" "WHEN" "UNLESS" "WHILE" "UNTIL" "REPEAT"
            "ALWAYS" "NEVER" "THEREIS"
            ;; Iteration keywords (used in binding clauses)
            "FROM" "TO" "BELOW" "ABOVE" "BY"
            "IN" "ON" "=" "THEN"
            "ACROSS" "BEING" "THE" "EACH"
            "OF" "OF-TYPE"
            ;; Control flow
            "IT" "ELSE" "END")
          :test #'string-equal))

(defun binding-keyword-p (string)
  "Check if STRING is a binding clause keyword (FOR, AS, WITH, AND)."
  (member (utils:symbol-name-from-string string)
          '("FOR" "AS" "WITH" "AND")
          :test #'string-equal))

(defun multi-form-keyword-p (string)
  "Check if STRING is a keyword that can take multiple forms (INITIALLY, FINALLY, DO, DOING)."
  (member (utils:symbol-name-from-string string)
          '("INITIALLY" "FINALLY" "DO" "DOING")
          :test #'string-equal))

(defun accumulation-keyword-p (string)
  "Check if STRING is an accumulation clause keyword (COLLECT, APPEND, etc.).
These can be connected with AND without creating variable bindings."
  (member (utils:symbol-name-from-string string)
          '("COLLECT" "COLLECTING" "APPEND" "APPENDING"
            "NCONC" "NCONCING" "COUNT" "COUNTING"
            "SUM" "SUMMING" "MAXIMIZE" "MAXIMIZING"
            "MINIMIZE" "MINIMIZING")
          :test #'string-equal))

(defun clause-keyword-p (string)
  "Check if STRING starts a new LOOP clause.
This excludes iteration sub-keywords like FROM, TO, IN, etc. which are part of FOR/AS/WITH clauses."
  (member (utils:symbol-name-from-string string)
          '("NAMED" "WITH" "FOR" "AS" "AND"
            "INITIALLY" "FINALLY"
            "DO" "DOING" "RETURN"
            "COLLECT" "COLLECTING" "APPEND" "APPENDING"
            "NCONC" "NCONCING" "COUNT" "COUNTING"
            "SUM" "SUMMING" "MAXIMIZE" "MAXIMIZING"
            "MINIMIZE" "MINIMIZING"
            "IF" "WHEN" "UNLESS" "WHILE" "UNTIL" "REPEAT"
            "ALWAYS" "NEVER" "THEREIS")
          :test #'string-equal))

(defun strip-package-prefix (form)
  "Recursively strip package prefixes from strings in FORM.
Handles strings, cons structures, and lists.
Preserves the original case from source code for better error messages."
  (cond
    ((null form) nil)
    ((stringp form)
     (utils:symbol-name-from-string form))
    ((consp form)
     (cons (strip-package-prefix (car form))
           (strip-package-prefix (cdr form))))
    (t form)))

(defun handle-and-clause (clauses i prev-clause bindings var-positions)
  "Handle AND clause in LOOP. Returns (values new-i bindings var-positions)."
  (cond
    ((and prev-clause
          (member (utils:symbol-name-from-string prev-clause)
                  '("FOR" "AS" "WITH")
                  :test #'string-equal)
          (< (1+ i) (length clauses)))
     ;; Previous was FOR/AS/WITH, this AND creates a parallel binding
     (let ((next-elem (nth (1+ i) clauses)))
       (when next-elem
         ;; Mark this position as a variable binding
         (push (1+ i) var-positions)
         (incf i)  ; Move to variable position
         (incf i)  ; Move past variable to start of init clause

         ;; Collect the init clause
         (let ((init-forms '()))
           (loop while (and (< i (length clauses))
                            (let ((next (nth i clauses)))
                              (not (and (stringp next)
                                        (clause-keyword-p next)))))
                 do (push (nth i clauses) init-forms)
                    (incf i))

           ;; Create loop-binding struct with is-parallel = T
           (push (make-loop-binding
                  :pattern (strip-package-prefix next-elem)
                  :init-form (strip-package-prefix (nreverse init-forms))
                  :is-parallel t)
                 bindings))
         ;; Adjust i (it's already at the next keyword)
         (decf i)))
     (values i bindings var-positions))
    (t
     ;; Previous was not FOR/AS/WITH, AND is just a connector
     (values (1+ i) bindings var-positions))))

(defun handle-binding-clause (clause clauses i bindings var-positions)
  "Handle FOR/AS/WITH clause in LOOP. Returns (values new-i bindings var-positions new-prev-clause)."
  (when (< (1+ i) (length clauses))
    (let ((var-or-pattern (nth (1+ i) clauses)))
      (when var-or-pattern
        ;; Mark this position as a variable binding
        (push (1+ i) var-positions)
        (incf i)  ; Move to variable position
        (incf i)  ; Move past variable to start of init clause

        ;; Collect the init clause
        (let ((init-forms '()))
          (loop while (and (< i (length clauses))
                           (let ((next (nth i clauses)))
                             (not (and (stringp next)
                                       (clause-keyword-p next)))))
                do (push (nth i clauses) init-forms)
                   (incf i))

          ;; Create loop-binding struct (sequential binding)
          (push (make-loop-binding
                 :pattern (strip-package-prefix var-or-pattern)
                 :init-form (strip-package-prefix (nreverse init-forms))
                 :is-parallel nil)
                bindings))
        ;; Adjust i (it's already at the next keyword)
        (decf i))))
  (values i bindings var-positions clause))

(defun handle-multi-form-clause (clauses i)
  "Handle INITIALLY/FINALLY/DO/DOING clause. Returns new-i."
  (incf i)
  ;; Skip all forms until next clause-starting keyword
  (loop while (and (< i (length clauses))
                   (let ((next (nth i clauses)))
                     (not (and (stringp next)
                               (clause-keyword-p next)))))
        do (incf i))
  (decf i)
  i)

(defun parse-loop-clauses (clauses)
  "Parse LOOP clauses and extract variable bindings with parallelism info.
Returns (values loop-bindings body-clauses)."
  ;; Guard: if clauses is not a proper list, return empty bindings
  (unless (a:proper-list-p clauses)
    (return-from parse-loop-clauses (values '() '())))

  ;; Check for simple form-based LOOP: (loop <form>)
  (when (and (= (length clauses) 1)
             (consp (first clauses))
             (not (stringp (first clauses))))
    (return-from parse-loop-clauses (values '() clauses)))

  (let ((bindings '())
        (var-positions '())
        (prev-clause nil)
        (i 0))
    ;; Collect all variable bindings and their positions
    (loop while (< i (length clauses))
          for clause = (nth i clauses)
          do (cond
               ;; AND - creates a parallel binding when connecting FOR/AS/WITH
               ((and (stringp clause)
                     (string-equal (utils:symbol-name-from-string clause) "AND"))
                (multiple-value-setq (i bindings var-positions)
                  (handle-and-clause clauses i prev-clause bindings var-positions)))

               ;; FOR/AS/WITH - binding clauses (always sequential)
               ((and (stringp clause)
                     (binding-keyword-p clause)
                     (not (string-equal (utils:symbol-name-from-string clause) "AND")))
                (multiple-value-setq (i bindings var-positions prev-clause)
                  (handle-binding-clause clause clauses i bindings var-positions)))

               ;; Multi-form keywords (INITIALLY, FINALLY, DO, DOING)
               ((and (stringp clause)
                     (multi-form-keyword-p clause))
                (setf prev-clause clause)
                (setf i (handle-multi-form-clause clauses i)))

               ;; Other clause keywords
               ((and (stringp clause)
                     (clause-keyword-p clause)
                     (not (binding-keyword-p clause)))
                (setf prev-clause clause))

               (t nil))
             (incf i))

    ;; Extract searchable expressions from clauses
    (let ((expressions '()))
      ;; Collect init-forms from bindings
      (dolist (binding bindings)
        (dolist (init-form (loop-binding-init-form binding))
          (when (consp init-form)
            (push init-form expressions))))

      ;; Collect expressions from other clauses
      (loop for j from 0 below (length clauses)
            for elem = (nth j clauses)
            unless (member j var-positions)
              do (cond
                   ((consp elem)
                    (push elem expressions))
                   ((and (stringp elem)
                         (not (loop-keyword-p elem)))
                    (push elem expressions))))

      (values (nreverse bindings) (nreverse expressions)))))


