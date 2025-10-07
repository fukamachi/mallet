(in-package #:malvolio/parser)

(defun count-newlines (text start end)
  "Count newlines in TEXT from START to END."
  (loop for i from start below end
        count (char= (char text i) #\Newline)))

(defun find-column (text pos)
  "Find column number (0-based) for position POS in TEXT."
  (if (zerop pos)
      0
      (loop for i downfrom (1- pos) downto 0
            until (char= (char text i) #\Newline)
            count 1)))

(defun find-form-end (text start)
  "Find the end position of a form starting at START.
This is a simple heuristic based on balanced parentheses and whitespace."
  (let ((len (length text))
        (depth 0)
        (in-string nil)
        (in-comment nil)
        (pos start))

    ;; Skip leading whitespace and comments
    (loop while (and (< pos len)
                     (or (member (char text pos) '(#\Space #\Tab #\Newline))
                         (char= (char text pos) #\;)))
          do (cond
               ((char= (char text pos) #\;)
                ;; Skip to end of line
                (loop while (and (< pos len)
                                 (char/= (char text pos) #\Newline))
                      do (incf pos))
                (when (< pos len) (incf pos)))
               (t (incf pos))))

    (when (>= pos len)
      (return-from find-form-end len))

    ;; Now find the end of the form
    (let ((start-char (char text pos)))
      (cond
        ;; List form
        ((char= start-char #\()
         (setf depth 1)
         (incf pos)
         (loop while (and (< pos len) (> depth 0))
               do (let ((ch (char text pos)))
                    (cond
                      ;; Handle strings
                      ((and (char= ch #\") (not in-string))
                       (setf in-string t)
                       (incf pos))
                      ((and (char= ch #\") in-string)
                       (setf in-string nil)
                       (incf pos))
                      ;; Handle escape in strings
                      ((and (char= ch #\\) in-string)
                       (incf pos 2))
                      ;; Handle parens when not in string
                      ((and (char= ch #\() (not in-string))
                       (incf depth)
                       (incf pos))
                      ((and (char= ch #\)) (not in-string))
                       (decf depth)
                       (incf pos))
                      (t (incf pos)))))
         pos)

        ;; String
        ((char= start-char #\")
         (incf pos)
         (loop while (and (< pos len)
                          (or (char/= (char text pos) #\")
                              (and (> pos 0)
                                   (char= (char text (1- pos)) #\\))))
               do (incf pos))
         (when (< pos len) (incf pos))
         pos)

        ;; Quoted form
        ((or (char= start-char #\')
             (char= start-char #\`)
             (char= start-char #\,))
         (let ((next-pos (1+ pos)))
           (when (and (char= start-char #\,)
                      (< next-pos len)
                      (char= (char text next-pos) #\@))
             (incf next-pos))
           (find-form-end text next-pos)))

        ;; Atom (symbol or number)
        (t
         (loop while (and (< pos len)
                          (not (member (char text pos) '(#\Space #\Tab #\Newline
                                                         #\( #\) #\; #\"))))
               do (incf pos))
         pos)))))

(defun skip-whitespace-and-comments (text pos)
  "Skip whitespace and comments starting at POS, returning new position."
  (let ((len (length text)))
    (loop while (< pos len) do
      (let ((ch (char text pos)))
        (cond
          ;; Whitespace
          ((member ch '(#\Space #\Tab #\Newline #\Return))
           (incf pos))
          ;; Comment
          ((char= ch #\;)
           ;; Skip to end of line
           (loop while (and (< pos len)
                            (char/= (char text pos) #\Newline))
                 do (incf pos))
           (when (< pos len) (incf pos)))
          ;; Not whitespace or comment, stop
          (t (loop-finish))))
          finally (return pos))))

(defun parse-forms (text file)
  "Parse forms from TEXT in FILE using SBCL reader with source tracking.
Returns a list of FORM objects."
  (check-type text string)
  (check-type file pathname)

  (let ((forms '())
        (stream (make-string-input-stream text))
        (pos 0)
        (len (length text)))

    (loop while (< pos len) do
      ;; Skip whitespace and comments first
      (setf pos (skip-whitespace-and-comments text pos))
      (when (>= pos len)
        (return))

      (handler-case
          (let* ((start-pos pos)
                 (start-line (1+ (count-newlines text 0 pos)))
                 (start-column (find-column text pos)))

            ;; Try to read a form
            (file-position stream pos)
            (let ((expr (read stream nil :eof)))
              (when (eq expr :eof)
                (return))

              ;; Find actual end of form in source
              (let* ((end-pos (find-form-end text start-pos))
                     (source (subseq text start-pos end-pos))
                     (newlines (count-newlines text start-pos end-pos))
                     (end-line (+ start-line newlines))
                     (end-column (if (zerop newlines)
                                     (+ start-column (- end-pos start-pos))
                                     (find-column text end-pos))))

                ;; Create form object
                (push (make-instance 'form
                                     :expr expr
                                     :file file
                                     :line start-line
                                     :column start-column
                                     :end-line end-line
                                     :end-column end-column
                                     :source (string-trim '(#\Space #\Tab #\Newline)
                                                          source))
                      forms)

                ;; Update position
                (setf pos end-pos))))

        (end-of-file ()
          (return))
        (error (e)
          (error "Parse error at position ~A: ~A" pos e))))

    (nreverse forms)))
