;;; swank-mrepl.lisp
;;
;; Licence: public domain

(in-package :swank)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((api '(
	       *emacs-connection*
	       channel 
	       channel-id
               channel-thread
	       define-channel-method
	       defslyfun 
	       destructure-case
	       log-event
	       process-requests
	       send-to-remote-channel
	       use-threads-p
	       wait-for-event
	       with-bindings
	       with-connection
	       with-top-level-restart
	       with-sly-interrupts
	       )))
    (eval `(defpackage #:swank-api
	     (:use)
	     (:import-from #:swank . ,api)
	     (:export . ,api)))))

(defpackage :swank-mrepl
  (:use :cl :swank-api)
  (:export #:create-mrepl))

(in-package :swank-mrepl)

(defclass listener-channel (channel)
  ((remote :initarg :remote)
   (env :initarg :env)
   (mode :initform :eval)
   (tag :initform nil)))

(defmethod initialize-instance :after ((channel listener-channel) &rest initargs)
  ;; FIXME: fugly, but I need this to be able to name the thread
  ;; according to the channel.
  (setf (slot-value channel 'swank::thread)
        (if (use-threads-p)
            (spawn-listener-thread *emacs-connection* channel)
            nil)))

(defun package-prompt (package)
  (reduce (lambda (x y) (if (<= (length x) (length y)) x y))
	  (cons (package-name package) (package-nicknames package))))

(defslyfun create-mrepl (remote)
  (let* ((pkg *package*)
         (ch (make-instance 'listener-channel :remote remote :thread nil))
         (thread (channel-thread ch)))
    (setf (slot-value ch 'env) (initial-listener-env ch))
    (when thread
      (swank-backend:send thread `(:serve-channel ,ch)))
    (list (channel-id ch)
	  (swank-backend:thread-id (or thread (swank-backend:current-thread)))
	  (package-name pkg)
	  (package-prompt pkg))))

(defun initial-listener-env (listener)
  `((*package* . ,*package*)
    (*standard-output* . ,(make-listener-output-stream listener))
    (*standard-input* . ,(make-listener-input-stream listener))
    (*) (**) (***)
    (/) (//) (///)
    (+) (++) (+++)))

(defun drop-unprocessed-events (channel)
  (with-slots (mode) channel
    (let ((old-mode mode))
      (setf mode :drop)
      (unwind-protect
	   (process-requests t)
	(setf mode old-mode)))
    (send-prompt channel)))

(defun spawn-listener-thread (connection channel)
  "Spawn a listener thread for CONNECTION and CHANNEL."
  (swank-backend:spawn 
   (lambda ()
     (with-connection (connection)
       (destructure-case (swank-backend:receive)
	 ((:serve-channel c)
          (assert (eq c channel))
	  (loop
	   (with-top-level-restart (connection (drop-unprocessed-events channel))
	     (process-requests nil)))))))
   :name (format nil "sly-mrepl-listener-ch-~a" (channel-id channel))))

(define-channel-method :process ((c listener-channel) string)
  (log-event ":process ~s~%" string)
  (with-slots (mode remote) c
    (ecase mode
      (:eval (mrepl-eval c string))
      (:read (mrepl-read c string))
      (:drop))))

(defun mrepl-eval (channel string)
  (with-slots (remote env) channel
    (let ((aborted t))
      (with-bindings env
	(unwind-protect
	     (let ((result (with-sly-interrupts (read-eval-print string))))
	       (send-to-remote-channel remote `(:write-result ,result))
	       (setq aborted nil))
          (when /
            (setq *** **  ** *  * (car /)
                  /// //  // /  
                  +++ ++  ++ + ))
	  (setf env (loop for (sym) in env
			  collect (cons sym (symbol-value sym))))
	  (cond (aborted
		 (send-to-remote-channel remote `(:evaluation-aborted)))
		(t
		 (send-prompt channel))))))))

(defun send-prompt (channel)
  (with-slots (env remote) channel
    (let ((pkg (or (cdr (assoc '*package* env)) *package*))
	  (out (cdr (assoc '*standard-output* env)))
	  (in (cdr (assoc '*standard-input* env))))
      (when out (force-output out))
      (when in (clear-input in))
      (send-to-remote-channel remote `(:prompt ,(package-name pkg)
					       ,(package-prompt pkg))))))
  
(defun mrepl-read (channel string)
  (with-slots (tag) channel
    (assert tag)
    (throw tag string)))

(defun read-eval-print (string)
  (with-input-from-string (in string)
    (setq / ())
    (loop for form = (read in nil in)
          until (eq form in)
          do
             (setq / (multiple-value-list (eval (setq + form)))))
    (force-output)
    (if /
	(format nil "~{~s~%~}" /) 
	"; No values")))

(defun make-listener-output-stream (channel)
  (let ((remote (slot-value channel 'remote)))
    (swank-backend:make-output-stream 
     (lambda (string)
       (send-to-remote-channel remote `(:write-string ,string))))))

(defun make-listener-input-stream (channel)
  (swank-backend:make-input-stream (lambda () (read-input channel))))

(defun set-mode (channel new-mode)
  (with-slots (mode remote) channel
    (unless (eq mode new-mode)
      (send-to-remote-channel remote `(:set-read-mode ,new-mode)))
    (setf mode new-mode)))

(defun read-input (channel)
  (with-slots (mode tag remote) channel
    (force-output)
    (let ((old-mode mode)
	  (old-tag tag))
      (setf tag (cons nil nil))
      (set-mode channel :read)
      (unwind-protect 
	   (catch tag (process-requests nil))
	(setf tag old-tag)
	(set-mode channel old-mode)))))

(provide :swank-mrepl)
