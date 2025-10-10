(defpackage #:trivial-glob/pattern
  (:use #:cl)
  (:export
   #:match-pattern
   #:*match-dotfiles*))
(in-package #:trivial-glob/pattern)

(defvar *match-dotfiles* nil
  "When NIL, wildcards do not match leading dots in filenames.
When T, wildcards match leading dots like any other character.")

(defun char-matches-p (pattern-char string-char casefold)
  "Check if a single character matches, with optional case-folding."
  (if casefold
      (char-equal pattern-char string-char)
      (char= pattern-char string-char)))

(defun char-in-class-p (char class-name)
  "Check if CHAR matches the POSIX character class CLASS-NAME."
  (cond
    ((string= class-name "alnum") (alphanumericp char))
    ((string= class-name "alpha") (alpha-char-p char))
    ((string= class-name "digit") (digit-char-p char))
    ((string= class-name "lower") (lower-case-p char))
    ((string= class-name "upper") (upper-case-p char))
    ((string= class-name "space") (member char '(#\Space #\Tab #\Newline #\Return #\Linefeed #\Page)))
    ((string= class-name "blank") (or (char= char #\Space) (char= char #\Tab)))
    ((string= class-name "punct")
     (and (graphic-char-p char)
          (not (alphanumericp char))
          (not (char= char #\Space))))
    ((string= class-name "print") (or (graphic-char-p char) (char= char #\Space)))
    ((string= class-name "graph") (graphic-char-p char))
    ((string= class-name "cntrl")
     (or (< (char-code char) 32)
         (= (char-code char) 127)))
    ((string= class-name "xdigit")
     (or (digit-char-p char)
         (and (char<= #\a char) (char<= char #\f))
         (and (char<= #\A char) (char<= char #\F))))
    (t nil)))

(defun find-bracket-close (pattern start end)
  "Find the closing ] for a bracket expression, skipping character classes.
START should point to the character after the opening [.
Returns the position of the closing ] or NIL if not found."
  (loop with i = start
        while (< i end)
        do (cond
             ;; Character class [:name:] - skip past it
             ((and (< (+ i 3) end)
                   (char= (char pattern i) #\[)
                   (char= (char pattern (1+ i)) #\:))
              (let ((class-end (search ":]" pattern :start2 (+ i 2) :end2 end)))
                (if class-end
                    (setf i (+ class-end 2))
                    (incf i))))
             ;; Found closing bracket
             ((char= (char pattern i) #\])
              (return i))
             ;; Regular character, keep going
             (t
              (incf i)))))

(defun char-in-bracket-p (char bracket-content casefold)
  "Check if CHAR matches the bracket expression BRACKET-CONTENT.
BRACKET-CONTENT should be the content between [ and ], e.g., 'a-z' or '!abc'."
  (let ((negated nil)
        (content bracket-content))
    ;; Check for negation
    (when (and (> (length content) 0)
               (or (char= (char content 0) #\!)
                   (char= (char content 0) #\^)))
      (setf negated t)
      (setf content (subseq content 1)))

    ;; Check if char matches any character or range in the bracket
    (let ((matched nil))
      (loop with i = 0
            while (< i (length content))
            do (cond
                 ;; Character class like [:alnum:] (check for [: pattern)
                 ((and (< (+ i 4) (length content))
                       (char= (char content i) #\[)
                       (char= (char content (1+ i)) #\:))
                  ;; Find the closing :]
                  (let ((close-pos (search ":]" content :start2 (+ i 2))))
                    (if close-pos
                        (let ((class-name (subseq content (+ i 2) close-pos)))
                          (when (char-in-class-p char class-name)
                            (setf matched t)
                            (return))
                          (setf i (+ close-pos 2)))
                        ;; No closing :], treat [ as regular character
                        (progn
                          (when (char-matches-p (char content i) char casefold)
                            (setf matched t)
                            (return))
                          (incf i)))))

                 ;; Range like a-z
                 ((and (< (+ i 2) (length content))
                       (char= (char content (1+ i)) #\-))
                  (let ((start (char content i))
                        (end (char content (+ i 2))))
                    (when (if casefold
                              (and (char<= start (char-upcase char))
                                   (char<= (char-upcase char) end))
                              (and (char<= start char)
                                   (char<= char end)))
                      (setf matched t)
                      (return)))
                  (incf i 3))

                 ;; Individual character
                 (t
                  (when (char-matches-p (char content i) char casefold)
                    (setf matched t)
                    (return))
                  (incf i))))
      ;; Apply negation if needed
      (if negated (not matched) matched))))

(defun match-glob-internal (pattern pstart pend string sstart send
                            &key pathname period casefold)
  "Internal recursive glob matcher.
Returns T if pattern[pstart:pend] matches string[sstart:send]."
  (cond
    ;; Both exhausted - match
    ((and (>= pstart pend) (>= sstart send))
     t)

    ;; Pattern exhausted but string remains - no match
    ((>= pstart pend)
     nil)

    ;; String exhausted but pattern has only * left - match
    ((>= sstart send)
     (and (< pstart pend)
          (char= (char pattern pstart) #\*)
          (match-glob-internal pattern (1+ pstart) pend string sstart send
                               :pathname pathname :period period :casefold casefold)))

    ;; Match current pattern character
    (t
     (let ((pchar (char pattern pstart))
           (schar (char string sstart)))
       (case pchar
         (#\*
          ;; * matches zero or more characters (excluding / in pathname mode)
          (or
           ;; Try matching zero characters (skip the *)
           (match-glob-internal pattern (1+ pstart) pend string sstart send
                                :pathname pathname :period period :casefold casefold)
           ;; Try matching one or more characters
           (and (not (and pathname (char= schar #\/)))
                (match-glob-internal pattern pstart pend string (1+ sstart) send
                                     :pathname pathname :period period :casefold casefold))))

         (#\?
          ;; ? matches exactly one character (excluding / in pathname mode)
          (and (not (and pathname (char= schar #\/)))
               (match-glob-internal pattern (1+ pstart) pend string (1+ sstart) send
                                    :pathname pathname :period period :casefold casefold)))

         (#\[
          ;; Bracket expression
          (let ((close-pos (find-bracket-close pattern (1+ pstart) pend)))
            (if close-pos
                (let ((bracket-content (subseq pattern (1+ pstart) close-pos)))
                  (and (char-in-bracket-p schar bracket-content casefold)
                       (match-glob-internal pattern (1+ close-pos) pend
                                            string (1+ sstart) send
                                            :pathname pathname :period period :casefold casefold)))
                ;; No closing bracket - treat [ as literal
                (and (char-matches-p pchar schar casefold)
                     (match-glob-internal pattern (1+ pstart) pend
                                          string (1+ sstart) send
                                          :pathname pathname :period period :casefold casefold)))))

         (#\\
          ;; Escape sequence - match next character literally
          (when (< (1+ pstart) pend)
            (let ((escaped-char (char pattern (1+ pstart))))
              (and (char-matches-p escaped-char schar casefold)
                   (match-glob-internal pattern (+ pstart 2) pend
                                        string (1+ sstart) send
                                        :pathname pathname :period period :casefold casefold)))))

         (otherwise
          ;; Literal character
          (and (char-matches-p pchar schar casefold)
               (match-glob-internal pattern (1+ pstart) pend
                                    string (1+ sstart) send
                                    :pathname pathname :period period :casefold casefold))))))))

(defun match-pattern (pattern string &key pathname period casefold)
  "Test whether STRING matches the glob PATTERN.

PATHNAME - If true, '/' characters are not matched by wildcards.
PERIOD - If true, leading '.' must be matched explicitly.
CASEFOLD - If true, perform case-insensitive matching.

Returns T if the string matches, NIL otherwise."
  ;; Handle period flag: if true and string starts with '.',
  ;; pattern must also start with '.'
  (when (and period
             (> (length string) 0)
             (char= (char string 0) #\.)
             (or (= (length pattern) 0)
                 (char/= (char pattern 0) #\.)))
    (return-from match-pattern nil))

  (match-glob-internal pattern 0 (length pattern)
                       string 0 (length string)
                       :pathname pathname
                       :period period
                       :casefold casefold))
