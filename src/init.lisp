(defpackage #:mallet/init
  (:use #:cl)
  (:local-nicknames
   (#:engine #:mallet/engine)
   (#:config #:mallet/config)
   (#:errors #:mallet/errors)
   (#:violation #:mallet/violation))
  (:export #:aggregate-violations-by-rule
           #:generate-init-config-string
           #:run-init))
(in-package #:mallet/init)

(defun aggregate-violations-by-rule (violations)
  "Count violations per rule name and return an alist of (rule-name . count)
sorted by count descending."
  (let ((counts (make-hash-table :test 'eq)))
    (dolist (v violations)
      (let ((rule-name (violation:violation-rule v)))
        (setf (gethash rule-name counts)
              (1+ (gethash rule-name counts 0)))))
    (let ((result '()))
      (maphash (lambda (k v)
                 (push (cons k v) result))
               counts)
      (sort result #'> :key #'cdr))))

(defun generate-init-config-string (rule-counts preset)
  "Generate the content of a .mallet.lisp suppression config.

RULE-COUNTS is an alist of (rule-name . count) sorted by count descending.
PRESET is a keyword (:default, :strict, etc.)."
  (with-output-to-string (out)
    (format out "(:mallet-config~%")
    (format out " (:extends ~(~S~))" preset)
    (cond
      ((null rule-counts)
       (format out " ; No violations found~%"))
      (t
       (format out "~%")
       (dolist (entry rule-counts)
         (let ((rule-name (car entry))
               (count (cdr entry)))
           (format out " (:disable ~(~S~)) ; ~D violation~:P~%"
                   rule-name count)))))
    (format out ")~%")))

(defun collect-all-violations (files preset)
  "Run the linting engine on FILES (list of pathnames) using PRESET and return all violations."
  (let* ((base-config (config:get-built-in-config preset))
         (all-violations '()))
    (dolist (file files)
      (multiple-value-bind (violations ignored-p)
          (engine:lint-file file :config base-config)
        (unless ignored-p
          (setf all-violations (nconc all-violations violations)))))
    all-violations))

(defun find-output-path (files file-args)
  "Determine where to write the .mallet.lisp config file.
Tries to find project root from FILES or FILE-ARGS; falls back to first directory."
  (let* ((first-file (first files))
         (first-arg (first file-args))
         ;; Start from the first file's directory, or parse the first arg
         (dir (cond
                (first-file
                 (uiop:pathname-directory-pathname first-file))
                (first-arg
                 (let ((path (uiop:parse-native-namestring first-arg)))
                   (if (uiop:directory-exists-p path)
                       (uiop:ensure-directory-pathname (truename path))
                       (uiop:pathname-directory-pathname path))))
                (t
                 (uiop:ensure-directory-pathname
                  *default-pathname-defaults*))))
         (project-root (when dir (config:find-project-root dir))))
    (merge-pathnames ".mallet.lisp"
                     (or project-root dir))))

(defun run-init (file-args files &key (preset :default) (force nil))
  "Generate a .mallet.lisp suppression config for gradual legacy adoption.

FILE-ARGS is a list of original file/directory path strings (used for project root detection).
FILES is a list of expanded pathnames to lint.
PRESET is the base preset keyword (:default or :strict).
FORCE, if true, overwrites an existing .mallet.lisp without error."
  (let ((output-path (find-output-path files file-args)))
    ;; Check if config already exists
    (when (and (probe-file output-path) (not force))
      (error 'errors:file-already-exists :path output-path))

    ;; Run linting to collect violations
    (let* ((violations (collect-all-violations files preset))
           (rule-counts (aggregate-violations-by-rule violations))
           (config-str (generate-init-config-string rule-counts preset)))

      ;; Write config file
      (with-open-file (out output-path
                           :direction :output
                           :if-exists :supersede
                           :if-does-not-exist :create)
        (write-string config-str out))

      (format t "Wrote ~A~%" output-path)
      (when rule-counts
        (format t "Suppressed ~D rule~:P with ~D violation~:P total.~%"
                (length rule-counts)
                (reduce #'+ rule-counts :key #'cdr))))))
