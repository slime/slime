;;; swank-repl.lisp --- Server side part of the Lisp listener.
;;
;; License: public domain

(in-package :swank)

(defvar *use-dedicated-output-stream* nil
  "When T swank will attempt to create a second connection to Emacs
which is used just to send output.")

(defvar *dedicated-output-stream-port* 0
  "Which port we should use for the dedicated output stream.")

(defvar *dedicated-output-stream-buffering*
  (if (eq *communication-style* :spawn) t nil)
  "The buffering scheme that should be used for the output stream.
Valid values are nil, t, :line")

(defun open-streams (connection properties)
  "Return the 5 streams for IO redirection:
DEDICATED-OUTPUT INPUT OUTPUT IO REPL-RESULTS"
  (let* ((input-fn
          (lambda ()
            (with-connection (connection)
              (with-simple-restart (abort-read
                                    "Abort reading input from Emacs.")
                (read-user-input-from-emacs)))))
         (dedicated-output (if *use-dedicated-output-stream*
                               (open-dedicated-output-stream
                                connection
                                (getf properties :coding-system))))
         (in (make-input-stream input-fn))
         (out (or dedicated-output
                  (make-output-stream (make-output-function connection))))
         (io (make-two-way-stream in out))
         (repl-results (make-output-stream-for-target connection
                                                      :repl-result)))
    (typecase connection
      (multithreaded-connection
       (setf (mconn.auto-flush-thread connection)
	     (spawn (lambda () (auto-flush-loop out))
		    :name "auto-flush-thread"))))
    (values dedicated-output in out io repl-results)))

(defun make-output-function (connection)
  "Create function to send user output to Emacs."
  (lambda (string)
    (with-connection (connection)
      (send-to-emacs `(:write-string ,string)))))

(defun make-output-function-for-target (connection target)
  "Create a function to send user output to a specific TARGET in Emacs."
  (lambda (string)
    (with-connection (connection)
      (with-simple-restart
          (abort "Abort sending output to Emacs.")
        (send-to-emacs `(:write-string ,string ,target))))))

(defun make-output-stream-for-target (connection target)
  "Create a stream that sends output to a specific TARGET in Emacs."
  (make-output-stream (make-output-function-for-target connection target)))

(defun open-dedicated-output-stream (connection coding-system)
  "Open a dedicated output connection to the Emacs on SOCKET-IO.
Return an output stream suitable for writing program output.

This is an optimized way for Lisp to deliver output to Emacs."
  (let ((socket (create-socket *loopback-interface*
                               *dedicated-output-stream-port*))
        (ef (find-external-format-or-lose coding-system)))
    (unwind-protect
         (let ((port (local-port socket)))
           (encode-message `(:open-dedicated-output-stream ,port
                                                           ,coding-system)
                           (connection.socket-io connection))
           (let ((dedicated (accept-connection
                             socket
                             :external-format ef
                             :buffering *dedicated-output-stream-buffering*
                             :timeout 30)))
             (authenticate-client dedicated)
             (close-socket socket)
             (setf socket nil)
             dedicated))
      (when socket
        (close-socket socket)))))

(defun find-repl-thread (connection)
  (cond ((not (use-threads-p))
         (current-thread))
        (t
         (let ((thread (mconn.repl-thread connection)))
           (cond ((not thread) nil)
                 ((thread-alive-p thread) thread)
                 (t
                  (setf (mconn.repl-thread connection)
                        (spawn-repl-thread connection "new-repl-thread"))))))))

(defun spawn-repl-thread (connection name)
  (spawn (lambda ()
           (with-bindings *default-worker-thread-bindings*
             (repl-loop connection)))
         :name name))

(defun repl-loop (connection)
  (handle-requests connection))

;;;;; Redirection during requests
;;;
;;; We always redirect the standard streams to Emacs while evaluating
;;; an RPC. This is done with simple dynamic bindings.

(defslimefun create-repl (target &key coding-system)
  (assert (eq target nil))
  (let ((conn *emacs-connection*))
    (initialize-streams-for-connection conn `(:coding-system ,coding-system))
    (with-struct* (connection. @ conn)
      (setf (@ env)
            `((*standard-output* . ,(@ user-output))
              (*standard-input*  . ,(@ user-input))
              (*trace-output*    . ,(or (@ trace-output) (@ user-output)))
              (*error-output*    . ,(@ user-output))
              (*debug-io*        . ,(@ user-io))
              (*query-io*        . ,(@ user-io))
              (*terminal-io*     . ,(@ user-io))))
      (maybe-redirect-global-io conn)
      (typecase conn
	(multithreaded-connection
	 (setf (mconn.repl-thread conn) 
	       (spawn-repl-thread conn "repl-thread"))))
      (list (package-name *package*)
            (package-string-for-prompt *package*)))))

(defun initialize-streams-for-connection (connection properties)
  (multiple-value-bind (dedicated in out io repl-results)
      (open-streams connection properties)
    (setf (connection.dedicated-output connection) dedicated
          (connection.user-io connection)          io
          (connection.user-output connection)      out
          (connection.user-input connection)       in
          (connection.repl-results connection)     repl-results)
    connection))

(defun read-user-input-from-emacs ()
  (let ((tag (make-tag)))
    (force-output)
    (send-to-emacs `(:read-string ,(current-thread-id) ,tag))
    (let ((ok nil))
      (unwind-protect
           (prog1 (caddr (wait-for-event `(:emacs-return-string ,tag value)))
             (setq ok t))
        (unless ok
          (send-to-emacs `(:read-aborted ,(current-thread-id) ,tag)))))))

;;;;; Listener eval

(defvar *listener-eval-function* 'repl-eval)

(defslimefun listener-eval (string)
  (funcall *listener-eval-function* string))

(defvar *send-repl-results-function* 'send-repl-results-to-emacs)

(defun repl-eval (string)
  (clear-user-input)
  (with-buffer-syntax ()
    (with-retry-restart (:msg "Retry SLIME REPL evaluation request.")
      (track-package
       (lambda ()
         (multiple-value-bind (values last-form) (eval-region string)
           (setq *** **  ** *  * (car values)
                 /// //  // /  / values
                 +++ ++  ++ +  + last-form)
           (funcall *send-repl-results-function* values))))))
  nil)

(defslimefun clear-repl-variables ()
  (let ((variables '(*** ** * /// // / +++ ++ +)))
    (loop for variable in variables
          do (setf (symbol-value variable) nil))))

(defun track-package (fun)
  (let ((p *package*))
    (unwind-protect (funcall fun)
      (unless (eq *package* p)
        (send-to-emacs (list :new-package (package-name *package*)
                             (package-string-for-prompt *package*)))))))

(defun send-repl-results-to-emacs (values)
  (finish-output)
  (if (null values)
      (send-to-emacs `(:write-string "; No value" :repl-result))
      (dolist (v values)
        (send-to-emacs `(:write-string ,(cat (prin1-to-string v) #\newline)
                                       :repl-result)))))

(defslimefun redirect-trace-output (target)
  (setf (connection.trace-output *emacs-connection*)
        (make-output-stream-for-target *emacs-connection* target))
  nil)

