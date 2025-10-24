;;; Test fixture for cyclomatic-complexity rule

;; Simple function - complexity 1 (OK)
(defun simple (x)
  (+ x 1))

;; Medium complexity - complexity 5 (OK with default 10)
(defun medium (x)
  (cond
    ((< x 0) 'negative)
    ((= x 0) 'zero)
    ((> x 0) 'positive)
    ((> x 100) 'large)
    (t 'other)))

;; High complexity - complexity 12 (VIOLATION with default 10)
(defun high-complexity (cmd)
  (cond
    ((string= cmd "start") (start-server))
    ((string= cmd "stop") (stop-server))
    ((string= cmd "restart") (restart-server))
    ((string= cmd "status") (show-status))
    ((string= cmd "config") (show-config))
    ((string= cmd "init") (initialize))
    ((string= cmd "destroy") (destroy))
    ((string= cmd "pause") (pause-server))
    ((string= cmd "resume") (resume-server))
    ((string= cmd "test") (run-tests))
    (t (error "Unknown command"))))

;; Another simple function
(defun add (a b)
  (+ a b))
