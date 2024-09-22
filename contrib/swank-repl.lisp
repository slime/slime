;;; swank-repl.lisp --- Server side part of the Lisp listener.
;;
;; License: public domain
(in-package swank)

(defpackage swank-repl
  (:use cl swank/backend)
  (:export *send-repl-results-function*)
  (:import-from
   swank

   *default-worker-thread-bindings*

   *loopback-interface*

   add-hook
   *connection-closed-hook*

   eval-region
   with-buffer-syntax

   connection
   connection.socket-io
   connection.repl-results
   connection.user-input
   connection.user-output
   connection.user-io
   connection.trace-output
   connection.env

   multithreaded-connection
   mconn.active-threads
   mconn.repl-thread
   mconn.auto-flush-thread
   use-threads-p

   *emacs-connection*
   default-connection
   with-connection

   send-to-emacs
   *communication-style*
   handle-requests
   wait-for-event
   make-tag
   thread-for-evaluation
   socket-quest

   authenticate-client
   encode-message

   auto-flush-loop
   clear-user-input

   current-thread-id
   cat
   with-struct*
   with-retry-restart
   with-bindings

   package-string-for-prompt
   find-external-format-or-lose

   defslimefun

   ;; FIXME: those should be exported from swank-repl only, but how to
   ;; do that whithout breaking init files?
   *globally-redirect-io*))

(in-package swank-repl)

(defvar *globally-redirect-io* :started-from-emacs
  "When T globally redirect all standard streams to Emacs.
When :STARTED-FROM-EMACS redirect when launched by M-x slime")

(defun globally-redirect-io-p ()
  (case *globally-redirect-io*
    ((t) t)
    (:started-from-emacs swank-loader:*started-from-emacs*)))

(defun make-output-function (connection)
  "Create function to send user output to Emacs."
  (lambda (string)
    (with-connection (connection)
      (send-to-emacs `(:write-string ,string nil ,(current-thread-id)))
      ;; Wait for Emacs to finish writing, otherwise on continuous
      ;; output its input buffer will fill up and nothing else will be
      ;; processed, most importantly an interrupt-thread request.
      (wait-for-event `(:write-done)))))

(defun repl-loop (connection)
  (unwind-protect
       (handle-requests connection)
    (when (typep connection 'multithreaded-connection)
      (setf (mconn.repl-thread connection)
            'aborted))))

(defun spawn-repl-thread (connection name)
  (spawn (lambda ()
           (with-bindings *default-worker-thread-bindings*
             (repl-loop connection)))
         :name name))

(defun find-repl-thread (connection)
  (cond ((not (use-threads-p))
         (current-thread))
        (t
         (let ((thread (mconn.repl-thread connection)))
           (cond ((not thread) nil)
                 ((and (not (eq thread 'aborted))
                       (thread-alive-p thread))
                  thread)
                 (t
                  (setf (mconn.repl-thread connection)
                        (spawn-repl-thread connection "new-repl-thread"))))))))

(defmethod thread-for-evaluation ((connection multithreaded-connection)
				  (id (eql :find-existing)))
  (or (car (mconn.active-threads connection))
      (find-repl-thread connection)))

(defmethod thread-for-evaluation ((connection multithreaded-connection)
				  (id (eql :repl-thread)))
  (find-repl-thread connection))

(defun read-user-input-from-emacs ()
  (let ((tag (make-tag)))
    (really-finish-output *standard-output*)
    (send-to-emacs `(:read-string ,(current-thread-id) ,tag))
    (let ((ok nil))
      (unwind-protect
           (prog1 (caddr (wait-for-event `(:emacs-return-string ,tag value)))
             (swank/gray::reset-stream-line-column (connection.user-output *emacs-connection*))
             (setq ok t))
        (unless ok
          (send-to-emacs `(:read-aborted ,(current-thread-id) ,tag)))))))

(defun open-streams (connection)
  "Return the 4 streams for IO redirection:
INPUT OUTPUT IO REPL-RESULTS"
  (let* ((input-fn
           (lambda ()
             (with-connection (connection)
               (with-simple-restart (abort-read
                                     "Abort reading input from Emacs.")
                 (read-user-input-from-emacs)))))
         (in (make-input-stream input-fn))
         (out (make-output-stream (make-output-function connection)))
         (io (make-two-way-stream in out))
         (repl-results (swank:make-output-stream-for-target connection
                                                            :repl-result)))
    (typecase connection
      (multithreaded-connection
       (setf (mconn.auto-flush-thread connection)
             (make-auto-flush-thread out))))
    (values in out io repl-results)))

(defun initialize-streams-for-connection (connection)
  (multiple-value-bind (in out io repl-results)
      (open-streams connection)
    (setf (connection.user-io connection)          io
          (connection.user-output connection)      out
          (connection.user-input connection)       in
          (connection.repl-results connection)     repl-results)
    connection))

;;;;; Redirection during requests
;;;
;;; We always redirect the standard streams to Emacs while evaluating
;;; an RPC. This is done with simple dynamic bindings.

;;;;; Global redirection setup

(defvar *standard-output-streams*
  '(*standard-output* *error-output* *trace-output*)
  "The symbols naming standard output streams.")

(defvar *standard-input-streams*
  '(*standard-input*)
  "The symbols naming standard input streams.")

(defvar *standard-io-streams*
  '(*debug-io* *query-io* *terminal-io*)
  "The symbols naming standard io streams.")

(defun prefixed-var (prefix variable-symbol)
  "(PREFIXED-VAR \"FOO\" '*BAR*) => SWANK::*FOO-BAR*"
  (let ((basename (subseq (symbol-name variable-symbol) 1)))
    (intern (format nil "*~A-~A" (string prefix) basename) :swank)))

(defun globally-redirect-io-to-connection (connection)
  "Set the standard I/O streams to redirect to CONNECTION.
Assigns *CURRENT-<STREAM>* for all standard streams."
  (dolist (o *standard-output-streams*)
    (set (prefixed-var '#:current o)
         (connection.user-output connection)))
  ;; FIXME: If we redirect standard input to Emacs then we get the
  ;; regular Lisp top-level trying to read from our REPL.
  ;;
  ;; Perhaps the ideal would be for the real top-level to run in a
  ;; thread with local bindings for all the standard streams. Failing
  ;; that we probably would like to inhibit it from reading while
  ;; Emacs is connected.
  ;;
  ;; Meanwhile we just leave *standard-input* alone.
  #+NIL
  (dolist (i *standard-input-streams*)
    (set (prefixed-var '#:current i)
         (connection.user-input connection)))
  (dolist (io *standard-io-streams*)
    (set (prefixed-var '#:current io)
         (connection.user-io connection))))

;;;;; Global redirection hooks

(defvar *global-stdio-connection* nil
  "The connection to which standard I/O streams are globally redirected.
NIL if streams are not globally redirected.")

(defvar *saved-global-streams* '()
  "A plist to save and restore redirected stream objects.
E.g. the value for '*standard-output* holds the stream object
for *standard-output* before we install our redirection.")

(defun revert-global-io-redirection ()
  "Set *CURRENT-<STREAM>* to *REAL-<STREAM>* for all standard streams."
  (dolist (stream-var (append *standard-output-streams*
                              *standard-input-streams*
                              *standard-io-streams*))
    (set (prefixed-var '#:current stream-var)
         (getf *saved-global-streams* stream-var))))

(defun setup-stream-indirection (stream-var &optional stream)
  "Setup redirection scaffolding for a global stream variable.
Supposing (for example) STREAM-VAR is *STANDARD-INPUT*, this macro:

1. Saves the value of *STANDARD-INPUT* in `*SAVED-GLOBAL-STREAMS*'.

2. Creates *CURRENT-STANDARD-INPUT*, initially with the same value as
*STANDARD-INPUT*.

3. Assigns *STANDARD-INPUT* to a synonym stream pointing to
*CURRENT-STANDARD-INPUT*.

This has the effect of making *CURRENT-STANDARD-INPUT* contain the
effective global value for *STANDARD-INPUT*. This way we can assign
the effective global value even when *STANDARD-INPUT* is shadowed by a
dynamic binding."
  (let ((current-stream-var (prefixed-var '#:current stream-var))
        (stream (or stream (symbol-value stream-var))))
    ;; Save the real stream value for the future.
    (setf (getf *saved-global-streams* stream-var) stream)
    ;; Define a new variable for the effective stream.
    ;; This can be reassigned.
    (proclaim `(special ,current-stream-var))
    (set current-stream-var stream)
    ;; Assign the real binding as a synonym for the current one.
    (let ((stream (make-synonym-stream current-stream-var)))
      (set stream-var stream)
      (set-default-initial-binding stream-var `(quote ,stream)))))

(defun init-global-stream-redirection ()
  (when (globally-redirect-io-p)
    (cond (*saved-global-streams*
           (warn "Streams already redirected."))
          (t
           (mapc #'setup-stream-indirection
                 (append *standard-output-streams*
                         *standard-input-streams*
                         *standard-io-streams*))))))

(defun maybe-redirect-global-io (connection)
  "Consider globally redirecting to CONNECTION."
  (when (and (globally-redirect-io-p) (null *global-stdio-connection*)
             (connection.user-io connection))
    (unless *saved-global-streams*
      (init-global-stream-redirection))
    (setq *global-stdio-connection* connection)
    (globally-redirect-io-to-connection connection)))

(defslimefun create-repl (target)
  (assert (eq target nil))
  (let ((conn *emacs-connection*))
    (initialize-streams-for-connection conn)
    (with-struct* (connection. @ conn)
      (setf (@ env)
	    `((*standard-input*  . ,(@ user-input))
	      ,@(unless (globally-redirect-io-p)
		  `((*standard-output* . ,(@ user-output))
		    (*trace-output*    . ,(or (@ trace-output) (@ user-output)))
		    (*error-output*    . ,(@ user-output))
		    (*debug-io*        . ,(@ user-io))
		    (*query-io*        . ,(@ user-io))
		    (*terminal-io*     . ,(@ user-io))))))
      (add-hook *connection-closed-hook* 'update-redirection-after-close)
      (typecase conn
	(multithreaded-connection
         (cond (swank::*main-thread*
                (send swank::*main-thread*
                      (list :run-on-main-thread
                            (lambda ()
                              (maybe-redirect-global-io conn)
                              (shiftf (mconn.repl-thread conn)
                                      swank::*main-thread* nil)
                              (swank::with-io-redirection (conn)
                                (with-bindings *default-worker-thread-bindings*
                                  (repl-loop conn)))))))
               (t
                (maybe-redirect-global-io conn)
	        (setf (mconn.repl-thread conn)
	              (spawn-repl-thread conn "repl-thread")))))
        (t
         (maybe-redirect-global-io conn)))
      (list (package-name *package*)
            (package-string-for-prompt *package*)))))

;;;;; Listener eval

(defvar *listener-eval-function* 'repl-eval)

(defvar *listener-saved-value* nil)

(defslimefun listener-save-value (slimefun &rest args)
  "Apply SLIMEFUN to ARGS and save the value.
The saved value should be visible to all threads and retrieved via
LISTENER-GET-VALUE."
  (setq *listener-saved-value* (apply slimefun args))
  t)

(defslimefun listener-eval (string &key (window-width nil window-width-p))
  (swank/gray::reset-stream-line-column (connection.user-output *emacs-connection*))
  (if window-width-p
      (let ((*print-right-margin* window-width))
        (funcall *listener-eval-function* string))
      (funcall *listener-eval-function* string)))

(defslimefun listener-get-value ()
  "Get the last value saved by LISTENER-SAVE-VALUE.
The value should be produced as if it were requested through
LISTENER-EVAL directly, so that spacial variables *, etc are set."
  (listener-eval (let ((*package* (find-package :keyword)))
                   (write-to-string '*listener-saved-value*))))

(defslimefun clear-repl-variables ()
  (let ((variables '(*** ** * /// // / +++ ++ +)))
    (loop for variable in variables
       do (setf (symbol-value variable) nil))))

(defvar *send-repl-results-function* 'send-repl-results-to-emacs)

(defun track-package (fun)
  (let ((p *package*))
    (unwind-protect (funcall fun)
      (unless (eq *package* p)
        (send-to-emacs (list :new-package (package-name *package*)
                             (package-string-for-prompt *package*)))))))

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

(defun send-repl-results-to-emacs (values)
  (really-finish-output *standard-output*)
  (if (null values)
      (send-to-emacs `(:write-string "; No value" :repl-result))
      (dolist (v values)
        (send-to-emacs `(:write-string ,(cat (prin1-to-string v) #\newline)
                                       :repl-result)))))

(defslimefun redirect-trace-output (target)
  (setf (connection.trace-output *emacs-connection*)
        (swank:make-output-stream-for-target *emacs-connection* target))
  nil)

;;;; IO to Emacs
;;;
;;; This code handles redirection of the standard I/O streams
;;; (`*standard-output*', etc) into Emacs. The `connection' structure
;;; contains the appropriate streams, so all we have to do is make the
;;; right bindings.

;;;;; Global I/O redirection framework
;;;
;;; Optionally, the top-level global bindings of the standard streams
;;; can be assigned to be redirected to Emacs. When Emacs connects we
;;; redirect the streams into the connection, and they keep going into
;;; that connection even if more are established. If the connection
;;; handling the streams closes then another is chosen, or if there
;;; are no connections then we revert to the original (real) streams.
;;;
;;; It is slightly tricky to assign the global values of standard
;;; streams because they are often shadowed by dynamic bindings. We
;;; solve this problem by introducing an extra indirection via synonym
;;; streams, so that *STANDARD-INPUT* is a synonym stream to
;;; *CURRENT-STANDARD-INPUT*, etc. We never shadow the "current"
;;; variables, so they can always be assigned to affect a global
;;; change.



(defun update-redirection-after-close (closed-connection)
  "Update redirection after a connection closes."
  (check-type closed-connection connection)
  (when (eq *global-stdio-connection* closed-connection)
    (if (and (default-connection) (globally-redirect-io-p))
        ;; Redirect to another connection.
        (globally-redirect-io-to-connection (default-connection))
        ;; No more connections, revert to the real streams.
        (progn (revert-global-io-redirection)
               (setq *global-stdio-connection* nil)))))

(provide :swank-repl)
