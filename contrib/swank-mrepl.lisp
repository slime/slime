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
               close-channel
               define-channel-method
               defslyfun
               destructure-case
               find-channel
               log-event
               process-requests
               send-to-remote-channel
               use-threads-p
               wait-for-event
               with-bindings
               with-connection
               with-top-level-restart
               with-sly-interrupts
               stop-processing
               with-buffer-syntax
               with-retry-restart
               )))
    (eval `(defpackage #:swank-api
             (:use)
             (:import-from #:swank . ,api)
             (:export . ,api)))))

(defpackage :swank-mrepl
  (:use :cl :swank-api)
  (:export #:create-mrepl
           #:listener-save-value
           #:eval-in-mrepl))

(in-package :swank-mrepl)

(defclass listener-channel (channel)
  ((remote     :initarg  :remote :accessor remote)
   (env        :initarg  :env    :accessor env)
   (mode       :initform :eval   :accessor channel-mode)
   (tag        :initform nil)
   (out                          :reader out)
   (in                           :reader in)))

(defmethod initialize-instance :after ((channel listener-channel)
                                       &rest initargs)
  (declare (ignore initargs))
  ;; FIXME: fugly, but I need this to be able to name the thread
  ;; according to the channel.
  (setf (slot-value channel 'swank::thread)
        (if (use-threads-p)
            (spawn-listener-thread *emacs-connection* channel)
            nil))
  (setf (slot-value channel 'out)
        (or (and *use-dedicated-output-stream*
                 (open-dedicated-output-stream channel))
            (make-listener-output-stream channel)))
  (setf (slot-value channel 'in) (make-listener-input-stream channel)))

(defun package-prompt (package)
  (reduce (lambda (x y) (if (<= (length x) (length y)) x y))
          (cons (package-name package) (package-nicknames package))))

(defslyfun create-mrepl (remote)
  (let* ((pkg *package*)
         (ch (make-instance
              'listener-channel
              :remote remote :thread nil
              :name (format nil "mrepl listener for remote ~a" remote)))
         (thread (channel-thread ch)))

    (setf (slot-value ch 'env) (initial-listener-env ch))

    (when thread
      (swank-backend:send thread `(:serve-channel ,ch)))
    (list (channel-id ch)
          (swank-backend:thread-id (or thread (swank-backend:current-thread)))
          (package-name pkg)
          (package-prompt pkg))))

(defslyfun eval-in-mrepl (remote string)
  "Like MREPL-EVAL, but not run in channel's thread."
  (mrepl-eval (find-channel remote) string))

(defvar *history* nil)

(defun initial-listener-env (channel)
  (let* ((out (out channel))
         (in (in channel))
         (io (make-two-way-stream in out)))
    `((cl:*package* . ,*package*)
      (cl:*standard-output* . ,out)
      (cl:*standard-input*  . ,in)
      (cl:*trace-output*    . ,out)
      (cl:*error-output*    . ,out)
      (cl:*debug-io*        . ,io)
      (cl:*query-io*        . ,io)
      (cl:*terminal-io*             . ,io)

      (*) (**) (***)
      (/) (//) (///)
      (+) (++) (+++)

      (*history* . ,(make-array 40 :fill-pointer 0
                                   :adjustable t)))))

(defun drop-unprocessed-events (channel)
  "Empty CHANNEL of events, then send prompt to Emacs."
  (with-slots (mode) channel
    (let ((old-mode mode))
      (setf mode :drop)
      (unwind-protect
           (process-requests t)
        (setf mode old-mode)))
    (with-bindings (env channel)
      (send-prompt channel))))

(defun spawn-listener-thread (connection channel)
  "Spawn a listener thread for CONNECTION and CHANNEL."
  (swank-backend:spawn
   (lambda ()
     (with-connection (connection)
       (destructure-case
        (swank-backend:receive)
        ((:serve-channel c)
         (assert (eq c channel))
         (loop
           (with-top-level-restart (connection
                                    (drop-unprocessed-events channel))
             (when (eq (process-requests nil)
                       'listener-teardown)
               (return))))))))
   :name (format nil "sly-mrepl-listener-ch-~a" (channel-id channel))))

(define-channel-method :process ((c listener-channel) string)
  (log-event ":process ~s~%" string)
  (ecase (channel-mode c)
      (:eval (mrepl-eval c string))
      (:read (mrepl-read c string))
      (:drop)))

(define-channel-method :inspect ((c listener-channel) object-idx value-idx)
  (log-event ":inspect ~s~%" object-idx)
  (with-slots (remote env) c
    (with-bindings env
      (send-to-remote-channel
       remote
       `(:inspect-result
         ,(swank::inspect-object (nth value-idx
                                      (aref *history* object-idx))))))))

(define-channel-method :teardown ((c listener-channel))
  (log-event ":teardown~%")
  (close-channel c)
  (throw 'stop-processing 'listener-teardown))

(defvar *listener-saved-value* nil)

(defslyfun listener-save-value (slyfun &rest args)
  "Apply SLYFUN to ARGS and save the value.
 The saved value should be visible to all threads and retrieved via a
 :PRODUCE-SAVED-VALUE message."
  (setq *listener-saved-value* (apply slyfun args))
  t)

(define-channel-method :sync-package-and-default-directory ((c listener-channel)
                                                            package-name
                                                            directory)

  (mrepl-eval
   c (let ((*package* (find-package :keyword)))
       (write-to-string
        `(progn
           (let ((package (swank::guess-package ,package-name)))
             (swank:set-default-directory ,directory)
             (and package (setq *package* package))
             (list *package* *default-pathname-defaults*)))))))

(define-channel-method :produce-saved-value ((c listener-channel))
  (mrepl-eval c (let ((*package* (find-package :keyword)))
                  (write-to-string '*listener-saved-value*))))


(defun mrepl-eval (channel string)
  (with-slots (remote env) channel
    (let ((aborted t))
      (with-bindings env
        (let (results)
          (unwind-protect
               (handler-bind
                   ((error #'(lambda (err)
                               (setq aborted err))))
                 (setq results (with-sly-interrupts (read-eval string))
                       aborted nil))
            (flush-streams channel)
            (cond (aborted
                   (send-to-remote-channel remote
                                           `(:evaluation-aborted
                                             ,(prin1-to-string aborted))))
                  (t
                   (setq /// //  // /  / results
                         *** **  ** *  * (car results)
                         +++ ++  ++ + )
                   (vector-push-extend results *history*)
                   (send-to-remote-channel
                    remote
                    `(:write-values ,(mapcar #'swank::to-line
                                             results)))
                   (send-prompt channel)))
            (loop for binding in env
                  do (setf (cdr binding) (symbol-value (car binding))))))))))

(defun flush-streams (channel)
  (with-slots (in out) channel
    (force-output out)
    (clear-input in)))

(defun send-prompt (channel)
  (with-slots (remote) channel
    (send-to-remote-channel remote `(:prompt ,(package-name *package*)
                                             ,(package-prompt *package*)))))

(defun mrepl-read (channel string)
  (with-slots (tag) channel
    (assert tag)
    (throw tag string)))

(defun read-eval (string)
  (with-retry-restart (:msg "Retry SLY mREPL evaluation request.")
    (with-input-from-string (in string)
      (loop with values
            for form = (read in nil in)
            until (eq form in)
            do
               (setq values (multiple-value-list (eval (setq + form))))
            finally
               (return values)))))

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


;;; Dedicated output stream
;;;
(defparameter *use-dedicated-output-stream* t
  "When T, dedicate a second stream for sending output to Emacs.")

(defparameter *dedicated-output-stream-port* 0
  "Which port we should use for the dedicated output stream.")

(defparameter *dedicated-output-stream-buffering*
  (if (eq swank:*communication-style* :spawn) t nil)
  "The buffering scheme that should be used for the output stream.
Valid values are nil, t, :line")

(defun open-dedicated-output-stream (channel)
  "Establish a dedicated output connection to Emacs.

Notify Emacs's CHANNEL that a socket is listening at a local ephemeral
port. This is an optimized way for Lisp to deliver output to Emacs."
  (let ((socket (swank-backend:create-socket swank::*loopback-interface*
                                             *dedicated-output-stream-port*))
        ;; HACK: hardcoded coding system
        (ef (swank::find-external-format-or-lose "utf-8")))
    (unwind-protect
         (let ((port (swank-backend:local-port socket)))
           (send-to-remote-channel (remote channel)
                                   `(:open-dedicated-output-stream ,port nil))
           (let ((dedicated (swank-backend:accept-connection
                             socket
                             :external-format ef
                             :buffering *dedicated-output-stream-buffering*
                             :timeout 30)))
             (swank:authenticate-client dedicated)
             (swank-backend:close-socket socket)
             (setf socket nil)
             dedicated))
      (when socket
        (swank-backend:close-socket socket)))))



(provide :swank-mrepl)
