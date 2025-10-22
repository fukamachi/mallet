(defpackage #:mallet/fixer
  (:use #:cl)
  (:local-nicknames
   (#:violation #:mallet/violation))
  (:export #:apply-fixes
           #:apply-fixes-to-file))
(in-package #:mallet/fixer)

(defun apply-fixes (violations &key dry-run)
  "Apply fixes from VIOLATIONS to their respective files.

VIOLATIONS - List of violation objects (may include unfixable violations)
DRY-RUN - If T, don't write files, just return what would be fixed

Returns (values fixed-count fixed-violations unfixed-violations)
  - fixed-count: Number of violations that were fixed
  - fixed-violations: List of violations that were fixed
  - unfixed-violations: List of violations that couldn't be fixed"
  (check-type violations list)

  ;; Group violations by file
  (let ((by-file (make-hash-table :test 'equal))
        (fixed-violations '())
        (unfixed-violations '()))

    ;; Group violations by file pathname
    (dolist (v violations)
      (let ((file (violation:violation-file v))
            (fix (violation:violation-fix v)))
        (if fix
            (push v (gethash file by-file))
            (push v unfixed-violations))))

    ;; Apply fixes file by file
    (maphash (lambda (file file-violations)
               (let ((fixed (apply-fixes-to-file file file-violations :dry-run dry-run)))
                 (setf fixed-violations (nconc fixed-violations fixed))))
             by-file)

    (values (length fixed-violations)
            (nreverse fixed-violations)
            (nreverse unfixed-violations))))

(defun apply-fixes-to-file (file violations &key dry-run)
  "Apply fixes from VIOLATIONS to FILE.

FILE - Pathname to fix
VIOLATIONS - List of violations for this file (all must have fix metadata)
DRY-RUN - If T, don't write file

Returns list of violations that were successfully fixed."
  (check-type file pathname)
  (check-type violations list)

  ;; Read file content
  (let ((text (uiop:read-file-string file)))

    ;; Sort violations by line number (bottom to top) to preserve line numbers
    (let ((sorted (sort (copy-list violations) #'>
                        :key #'violation:violation-line)))

      ;; Deduplicate identical fixes (e.g., multiple violations deleting same clause)
      (let ((seen-fixes (make-hash-table :test 'equal))
            (unique-fixes '()))
        (dolist (v sorted)
          (let ((fix (violation:violation-fix v)))
            (when fix
              (let ((fix-key (fix-key fix)))
                (unless (gethash fix-key seen-fixes)
                  (setf (gethash fix-key seen-fixes) t)
                  (push fix unique-fixes))))))

        ;; Apply each unique fix
        (dolist (fix (nreverse unique-fixes))
          (setf text (apply-fix text fix))))

      ;; Write fixed content back to file (unless dry-run)
      (unless dry-run
        (with-open-file (out file
                             :direction :output
                             :if-exists :supersede
                             :if-does-not-exist :create)
          (write-string text out)))

      ;; Return list of fixed violations
      (remove-if-not #'violation:violation-fix violations))))

(defun fix-key (fix)
  "Generate a unique key for a fix for deduplication purposes.
Two fixes with the same key are considered identical and only one should be applied."
  (list (violation:violation-fix-type fix)
        (violation:violation-fix-start-line fix)
        (violation:violation-fix-end-line fix)
        (violation:violation-fix-start-column fix)
        (violation:violation-fix-end-column fix)))

(defun apply-fix (text fix)
  "Apply a single FIX to TEXT, returning modified TEXT.

TEXT - The file content as a string
FIX - A violation-fix struct

Returns the modified text."
  (check-type text string)
  (check-type fix violation:violation-fix)

  (ecase (violation:violation-fix-type fix)
    (:replace-line
     (apply-replace-line text
                         (violation:violation-fix-line-number fix)
                         (violation:violation-fix-replacement-content fix)))

    (:append-to-file
     (apply-append-to-file text
                           (violation:violation-fix-appended-content fix)))

    (:delete-lines
     (apply-delete-lines text
                         (violation:violation-fix-start-line fix)
                         (violation:violation-fix-end-line fix)))

    (:delete-range
     (apply-delete-range text
                         (violation:violation-fix-start-line fix)
                         (violation:violation-fix-start-column fix)
                         (violation:violation-fix-end-line fix)
                         (violation:violation-fix-end-column fix)))

    (:replace-form
     (apply-replace-form text
                         (violation:violation-fix-start-line fix)
                         (violation:violation-fix-end-line fix)
                         (violation:violation-fix-replacement-content fix)))))

(defun apply-replace-line (text line-number new-content)
  "Replace line LINE-NUMBER in TEXT with NEW-CONTENT.

LINE-NUMBER - 1-indexed line number
NEW-CONTENT - Replacement content (without trailing newline)

Returns modified text."
  (check-type text string)
  (check-type line-number (integer 1))
  (check-type new-content string)

  (with-output-to-string (out)
    (with-input-from-string (in text)
      (loop for current-line from 1
            for line = (read-line in nil nil)
            while line
            do (if (= current-line line-number)
                   (write-line new-content out)
                   (write-line line out))))))

(defun apply-append-to-file (text content)
  "Append CONTENT to end of TEXT.

CONTENT - Content to append

Returns modified text."
  (check-type text string)
  (check-type content string)

  (concatenate 'string text content))

(defun apply-delete-lines (text start-line end-line)
  "Delete lines START-LINE through END-LINE (inclusive) from TEXT.

START-LINE - 1-indexed starting line
END-LINE - 1-indexed ending line

Returns modified text."
  (check-type text string)
  (check-type start-line (integer 1))
  (check-type end-line (integer 1))

  (when (< end-line start-line)
    (error "end-line (~A) must be >= start-line (~A)" end-line start-line))

  (with-output-to-string (out)
    (with-input-from-string (in text)
      (loop for current-line from 1
            for line = (read-line in nil nil)
            while line
            do (unless (and (<= start-line current-line)
                            (<= current-line end-line))
                 (write-line line out))))))

(defun apply-delete-range (text start-line start-column end-line end-column)
  "Delete a precise character range from TEXT.

START-LINE - 1-indexed starting line
START-COLUMN - 0-indexed starting column
END-LINE - 1-indexed ending line
END-COLUMN - 0-indexed ending column (exclusive)

Returns modified text with the range deleted."
  (check-type text string)
  (check-type start-line (integer 1))
  (check-type start-column (integer 0))
  (check-type end-line (integer 1))
  (check-type end-column (integer 0))

  (with-output-to-string (out)
    (with-input-from-string (in text)
      (loop for current-line from 1
            for line = (read-line in nil nil)
            while line
            do (cond
                 ;; Before the range: write line as-is
                 ((< current-line start-line)
                  (write-line line out))

                 ;; Single line deletion
                 ((= start-line end-line current-line)
                  (let ((before (subseq line 0 start-column))
                        (after (if (< end-column (length line))
                                   (subseq line end-column)
                                   "")))
                    (write-line (concatenate 'string before after) out)))

                 ;; Start line of multi-line deletion
                 ((= current-line start-line)
                  (write-string (subseq line 0 start-column) out))

                 ;; End line of multi-line deletion
                 ((= current-line end-line)
                  (let ((after (if (< end-column (length line))
                                   (subseq line end-column)
                                   "")))
                    (write-line after out)))

                 ;; Middle lines of multi-line deletion: skip
                 ((and (> current-line start-line)
                       (< current-line end-line))
                  nil)

                 ;; After the range: write line as-is
                 ((> current-line end-line)
                  (write-line line out)))))))

(defun apply-replace-form (text start-line end-line replacement-content)
  "Replace lines START-LINE through END-LINE in TEXT with REPLACEMENT-CONTENT.

START-LINE - 1-indexed starting line
END-LINE - 1-indexed ending line
REPLACEMENT-CONTENT - New content to insert

Returns modified text.

Note: REPLACEMENT-CONTENT should include its own trailing newline.
If it doesn't end with a newline, one will be added automatically to prevent
the next line from being concatenated."
  (check-type text string)
  (check-type start-line (integer 1))
  (check-type end-line (integer 1))
  (check-type replacement-content string)

  (when (< end-line start-line)
    (error "end-line (~A) must be >= start-line (~A)" end-line start-line))

  (with-output-to-string (out)
    (with-input-from-string (in text)
      (loop for current-line from 1
            for line = (read-line in nil nil)
            while line
            do (cond
                 ;; Before the form: write line as-is
                 ((< current-line start-line)
                  (write-line line out))
                 ;; At start of form: write replacement
                 ((= current-line start-line)
                  ;; Ensure replacement ends with newline to avoid concatenation
                  (if (and (> (length replacement-content) 0)
                           (char= (char replacement-content (1- (length replacement-content))) #\Newline))
                      (write-string replacement-content out)
                      (write-line replacement-content out)))
                 ;; Inside or at end of form: skip
                 ((<= current-line end-line)
                  nil)
                 ;; After the form: write line as-is
                 (t
                  (write-line line out)))))))
