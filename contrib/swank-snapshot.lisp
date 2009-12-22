
(defpackage swank-snapshot
  (:use cl)
  (:export restore-snapshot save-snapshot)
  (:import-from swank defslimefun))
(in-package swank-snapshot)

(defslimefun save-snapshot (image-file)
  (swank-backend:save-image image-file 
			    (let ((c swank::*emacs-connection*))
			      (lambda () (resurrect c))))
  t)

(defslimefun restore-snapshot (image-file)
  (let* ((conn swank::*emacs-connection*)
	 (stream (swank::connection.socket-io conn))
	 (clone (swank-backend:dup (swank-backend:socket-fd stream)))
	 (style (swank::connection.communication-style conn))
	 (args (list "--swank-fd" (format nil "~d" clone)
		     "--swank-style" (format nil "~s" style))))
    (swank::close-connection conn nil nil)
    (swank-backend:exec-image image-file args)))

(in-package :swank)

(defun swank-snapshot::resurrect (old-connection)
  (setq *log-output* nil)
  (init-log-output)
  (clear-event-history)
  (setq *connections* (delete old-connection *connections*))
  (format *error-output* "args: ~s~%" (command-line-args))
  (let* ((fd (read-command-line-arg "--swank-fd"))
	 (style (read-command-line-arg "--swank-style")))
    (format *error-output* "fd=~s style=~s~%" fd style)
    (let ((connection (create-connection (make-fd-stream fd :default) style)))
      (run-hook *new-connection-hook* connection)
      (push connection *connections*)
      (serve-requests connection)
      (simple-repl))))

(defun read-command-line-arg (name)
  (let* ((args (command-line-args))
	 (pos (position name args :test #'equal)))
    (read-from-string (elt args (1+ pos)))))

(in-package :swank-snapshot)
