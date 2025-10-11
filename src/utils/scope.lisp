(defpackage #:malo/utils/scope
  (:use #:cl)
  (:export #:*scope-stack*
           #:push-scope
           #:pop-scope
           #:record-variable
           #:lookup-variable-position
           #:with-new-scope))
(in-package #:malo/utils/scope)

;;; Scope Stack - proper lexical scope management for variable tracking
;;;
;;; This module provides a scope stack mechanism for tracking variable positions
;;; in nested lexical scopes. It solves the problem of multiple scopes using the
;;; same variable name by maintaining separate hash tables for each scope level.
;;;
;;; Example:
;;;   (let ((*scope-stack* nil))
;;;     (with-new-scope
;;;       (record-variable "a" 1 0)
;;;       (with-new-scope
;;;         (record-variable "a" 5 0)  ; different scope, same name
;;;         (lookup-variable-position "a"))  ; returns (5 . 0)
;;;       (lookup-variable-position "a")))  ; returns (1 . 0)

(defvar *scope-stack* nil
  "Stack of scope hash tables for tracking variable positions.
Each scope is a hash table mapping variable names (strings) to (line . column) positions.
The stack grows as we enter nested scopes and shrinks as we exit them.
This allows multiple scopes to have variables with the same name without confusion.")

(defun push-scope ()
  "Create and push a new scope onto the scope stack.
Returns the new scope hash table."
  (let ((new-scope (make-hash-table :test 'equal)))
    (push new-scope *scope-stack*)
    new-scope))

(defun pop-scope ()
  "Pop the current scope from the scope stack.
Returns the popped scope hash table."
  (pop *scope-stack*))

(defun record-variable (var-name line column)
  "Record a variable binding in the current (topmost) scope with its position.
VAR-NAME should be a string (symbol name).
LINE and COLUMN are the source position (1-based line, 0-based column).
Returns the recorded position as (LINE . COLUMN)."
  (check-type var-name string)
  (check-type line (integer 1))
  (check-type column (integer 0))

  (if *scope-stack*
      (let ((current-scope (first *scope-stack*))
            (pos (cons line column)))
        (setf (gethash var-name current-scope) pos)
        pos)
      (error "Cannot record variable: no scope active")))

(defun lookup-variable-position (var-name)
  "Look up a variable's position, searching from innermost to outermost scope.
Returns (values line column) if found, or (values nil nil) if not found."
  (check-type var-name string)

  (loop for scope in *scope-stack*
        for pos = (gethash var-name scope)
        when pos
          return (values (car pos) (cdr pos))
        finally (return (values nil nil))))

(defmacro with-new-scope (&body body)
  "Execute BODY with a new scope pushed onto the scope stack.
The scope is automatically popped when exiting, even on non-local exits."
  `(progn
     (push-scope)
     (unwind-protect
          (progn ,@body)
       (pop-scope))))
