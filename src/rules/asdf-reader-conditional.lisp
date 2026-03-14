(defpackage #:mallet/rules/asdf-reader-conditional
  (:use #:cl)
  (:local-nicknames
   (#:base #:mallet/rules/base)
   (#:violation #:mallet/violation))
  (:export #:asdf-reader-conditional-rule))
(in-package #:mallet/rules/asdf-reader-conditional)

;;; ASDF Reader Conditional Rule

(defclass asdf-reader-conditional-rule (base:rule)
  ()
  (:default-initargs
   :name :asdf-reader-conditional
   :description "Avoid #+/#- reader conditionals in defsystem; use :if-feature or :feature instead"
   :severity :info
   :category :practice
   :type :text
   :file-types '(:asd))
  (:documentation "Rule to detect #+/#- reader conditionals inside defsystem bodies in .asd files.

Reader conditionals are processed at read time and cannot be controlled by ASDF, making them
less portable than ASDF's built-in :if-feature (component option) and :feature (dependency
modifier).  Use these ASDF mechanisms instead.

Exclusions: reader conditionals inside :perform and :around-compile bodies are legitimate
runtime CL code and are not flagged.  Reader conditionals at the top-level option-plist depth
of a defsystem form (e.g., #+asdf3 :mailto #+asdf3 \"email\") guard keyword-value pairs for
which there is no ASDF-native alternative; these are also not flagged.  Reader conditionals
outside any defsystem form are ignored.
Comments, string literals, and character literals are not scanned."))

;;; Text scanner state

(defstruct scan-state
  "Mutable state carried across characters (and lines) during text scanning."
  ;; String/comment/char literal tracking
  (in-string nil :type boolean)
  (escape-next nil :type boolean)
  (in-char nil :type boolean)   ; T after #\ — consuming character name
  ;; Block comment tracking (#| ... |# nesting)
  (block-comment-depth 0 :type integer)
  (prev-char-bar nil :type boolean)  ; T when previous char was `|` (for detecting |#)
  ;; Paren depth (global)
  (paren-depth 0 :type integer)
  ;; defsystem tracking
  ;; Once we are inside a defsystem body, in-defsystem = T.
  ;; defsystem-start-depth is the paren-depth of the OUTER context (the depth before
  ;; the opening `(` of the defsystem form).  When paren-depth drops to that value,
  ;; we have exited the defsystem.
  (in-defsystem nil :type boolean)
  (defsystem-start-depth 0 :type integer)
  ;; :perform/:around-compile body tracking
  ;; We use a list of depths.  Each entry is the paren-depth at the moment we saw the
  ;; opening `(` of the :perform/:around-compile argument list.  While perform-depths
  ;; is non-nil, we are inside at least one such body.
  ;; perform-pending: T means we just saw `:perform` or `:around-compile` and are
  ;; waiting for its `(`
  (perform-pending nil :type boolean)
  (perform-depths nil :type list)
  ;; Token accumulation buffer (reused across characters)
  (token-buf (make-array 20 :element-type 'character :adjustable t :fill-pointer 0)
              :type (vector character)))

(defun in-perform-p (state)
  "Return T if we are currently inside a :perform or :around-compile body."
  (scan-state-perform-depths state))

(defun at-defsystem-option-plist-p (state)
  "Return T if we are at the top-level option-plist depth of a defsystem form.

At this depth, reader conditionals guard keyword-value pairs such as:
  #+asdf3 :mailto #+asdf3 \"dev@example.com\"
There is no ASDF-native alternative for these option-pair guards, so they must
not be flagged.  Deeper depths (inside subforms like :depends-on or :components)
still have ASDF alternatives and should be flagged."
  (= (scan-state-paren-depth state)
     (1+ (scan-state-defsystem-start-depth state))))

(defun maybe-pop-performs (state)
  "Remove any :perform/:around-compile depth entries for bodies we have exited."
  (loop while (and (scan-state-perform-depths state)
                   (<= (scan-state-paren-depth state)
                       (first (scan-state-perform-depths state))))
        do (pop (scan-state-perform-depths state))))

(defun flush-token-buf (state)
  "Return accumulated token string and reset the buffer."
  (let* ((buf (scan-state-token-buf state))
         (tok (subseq buf 0 (fill-pointer buf))))
    (setf (fill-pointer buf) 0)
    tok))

(defun push-char-to-token (state ch)
  "Append CH to the token accumulation buffer."
  (let ((buf (scan-state-token-buf state)))
    (when (= (fill-pointer buf) (array-dimension buf 0))
      (adjust-array buf (* 2 (array-dimension buf 0))))
    (vector-push ch buf)))

(defun name-matches-defsystem-p (name)
  "Return T if NAME (already extracted, case-insensitively) equals `defsystem`."
  (string-equal name "defsystem"))

(defun token-defsystem-p (tok)
  "Return T if TOK is a `defsystem` symbol (possibly package-qualified)."
  (when (and (plusp (length tok))
             ;; Must not be a keyword (keywords start with :)
             (not (char= (char tok 0) #\:)))
    (let* ((colon-pos (position #\: tok :from-end t))
           (name (if colon-pos (subseq tok (1+ colon-pos)) tok)))
      (name-matches-defsystem-p name))))

(defun token-runtime-body-keyword-p (tok)
  "Return T if TOK is a keyword that introduces a runtime CL body (:perform or :around-compile)."
  (or (string-equal tok ":perform")
      (string-equal tok ":around-compile")))

(defun process-token (state tok)
  "Handle a completed token TOK, updating STATE accordingly."
  (when (plusp (length tok))
    (cond
      ;; If we see `defsystem` while not yet inside one, enter defsystem context.
      ;; At this point the opening `(` has already been processed, so paren-depth
      ;; is already 1 higher than before that `(`.  The defsystem form's outer
      ;; depth is therefore (paren-depth - 1).
      ((and (not (scan-state-in-defsystem state))
            (token-defsystem-p tok))
       (setf (scan-state-in-defsystem state) t)
       (setf (scan-state-defsystem-start-depth state)
             (1- (scan-state-paren-depth state))))

      ;; `:perform` or `:around-compile` inside a defsystem: mark that the next `(`
      ;; opens a runtime body that should be excluded from scanning
      ((and (scan-state-in-defsystem state)
            (not (scan-state-perform-pending state))
            (token-runtime-body-keyword-p tok))
       (setf (scan-state-perform-pending state) t))

      (t nil))))

; mallet:suppress cyclomatic-complexity
(defun scan-line (state line line-number file rule violations)
  "Scan all characters in LINE, updating STATE and collecting violations.
Returns updated violations list."
  (let ((len (length line))
        (i 0))
    (loop while (< i len) do
      (let ((ch (char line i)))
        (cond
          ;; --- inside block comment (#| ... |#) ---
          ((plusp (scan-state-block-comment-depth state))
           (cond
             ;; Check for nested #|
             ((and (char= ch #\#)
                   (< (1+ i) len)
                   (char= (char line (1+ i)) #\|))
              (incf (scan-state-block-comment-depth state))
              (setf (scan-state-prev-char-bar state) nil)
              (incf i))  ; skip past |
             ;; Check for closing |#
             ((and (scan-state-prev-char-bar state)
                   (char= ch #\#))
              (decf (scan-state-block-comment-depth state))
              (setf (scan-state-prev-char-bar state) nil))
             ;; Track | for |# detection
             (t
              (setf (scan-state-prev-char-bar state) (char= ch #\|)))))

          ;; --- escape sequence ---
          ((scan-state-escape-next state)
           (setf (scan-state-escape-next state) nil))

          ;; --- character literal name (after #\) ---
          ((scan-state-in-char state)
           (unless (alphanumericp ch)
             ;; Non-alphanumeric ends the character name; re-process this char
             (setf (scan-state-in-char state) nil)
             (decf i)))  ; will be re-incremented at end of loop

          ;; --- inside string ---
          ((scan-state-in-string state)
           (cond
             ((char= ch #\\) (setf (scan-state-escape-next state) t))
             ((char= ch #\") (setf (scan-state-in-string state) nil))
             (t nil))
           (flush-token-buf state))

          ;; --- semicolon: start of comment ---
          ((char= ch #\;)
           (process-token state (flush-token-buf state))
           (return))  ; skip rest of line

          ;; --- opening string ---
          ((char= ch #\")
           (process-token state (flush-token-buf state))
           (setf (scan-state-in-string state) t))

          ;; --- `#` dispatch ---
          ((char= ch #\#)
           (process-token state (flush-token-buf state))
           (let ((next (when (< (1+ i) len) (char line (1+ i)))))
             (cond
               ;; #| — block comment start
               ((and next (char= next #\|))
                (incf (scan-state-block-comment-depth state))
                (setf (scan-state-prev-char-bar state) nil)
                (incf i))   ; advance past `|`

               ;; #\ — character literal; consume the full character name
               ((and next (char= next #\\))
                (setf (scan-state-in-char state) t)
                (incf i))   ; advance past `\`; in-char will consume alphanumeric chars

               ;; #+ or #- — reader conditional
               ((and next (or (char= next #\+) (char= next #\-)))
                (when (and (scan-state-in-defsystem state)
                           (not (in-perform-p state))
                           (not (at-defsystem-option-plist-p state)))
                  (push (make-instance 'violation:violation
                                       :rule :asdf-reader-conditional
                                       :file file
                                       :line line-number
                                       :column i
                                       :severity (base:rule-severity rule)
                                       :message (format nil "Reader conditional #~A found in defsystem; use :if-feature or (:feature ...) instead"
                                                        (char line (1+ i))))
                        violations))
                (incf i))   ; advance past `+` or `-`

               ;; Other # — not our concern, don't accumulate
               (t nil))))

          ;; --- opening paren ---
          ((char= ch #\()
           (process-token state (flush-token-buf state))
           (when (scan-state-perform-pending state)
             (setf (scan-state-perform-pending state) nil)
             (push (scan-state-paren-depth state)
                   (scan-state-perform-depths state)))
           (incf (scan-state-paren-depth state)))

          ;; --- closing paren ---
          ((char= ch #\))
           (process-token state (flush-token-buf state))
           (decf (scan-state-paren-depth state))
           (maybe-pop-performs state)
           (when (and (scan-state-in-defsystem state)
                      (= (scan-state-paren-depth state)
                         (scan-state-defsystem-start-depth state)))
             (setf (scan-state-in-defsystem state) nil)
             (setf (scan-state-perform-pending state) nil)
             (setf (scan-state-perform-depths state) nil)))

          ;; --- whitespace ---
          ((member ch '(#\Space #\Tab #\Return))
           (process-token state (flush-token-buf state)))

          ;; --- regular character: accumulate token ---
          (t
           (push-char-to-token state ch))))
      (incf i))

    ;; End of line: flush any trailing token
    (process-token state (flush-token-buf state))
    ;; escape-next does not persist across newlines in CL (no line-continuation in strings)
    (setf (scan-state-escape-next state) nil))
  violations)

;;; Rule method

(defmethod base:check-text ((rule asdf-reader-conditional-rule) text file)
  "Scan TEXT for #+/#- reader conditionals inside defsystem bodies.
Only processes .asd files; skips comments, strings, character literals,
and :perform/:around-compile bodies."
  (check-type text string)
  (check-type file pathname)

  ;; Only check .asd files
  (when (string-equal (pathname-type file) "asd")
    (let ((state (make-scan-state))
          (violations '()))

      (with-input-from-string (stream text)
        (loop for line-number from 1
              for line = (read-line stream nil nil)
              while line
              do (setf violations
                       (scan-line state line line-number file rule violations))))

      (nreverse violations))))
