;;; swank-mrepl.lisp
;;
;; Licence: public domain

(defpackage :swank-mrepl
  (:use :cl :swank-api)
  (:export #:create-mrepl
           #:globally-save-object
           #:eval-in-mrepl))

(in-package :swank-mrepl)

(defclass mrepl (channel listener)
  ((remote-id     :initarg  :remote-id :accessor mrepl-remote-id)
   (mode       :initform :eval   :accessor mrepl-mode)
   (tag        :initform nil))
  (:documentation "A listener implemented in terms of a channel.")
  (:default-initargs
   :make-out-stream #'make-mrepl-output-stream
   :make-in-stream #'make-mrepl-input-stream
   :initial-env `((cl:*package* . ,*package*)
                  (*) (**) (***)
                  (/) (//) (///)
                  (+) (++) (+++)
                  (*history* . ,(make-array 40 :fill-pointer 0
                                               :adjustable t)))))

(defun package-prompt (package)
  (reduce (lambda (x y) (if (<= (length x) (length y)) x y))
          (cons (package-name package) (package-nicknames package))))

(defslyfun create-mrepl (remote-id)
  (let* ((pkg *package*)
         (ch (make-instance
              'mrepl
              :remote-id remote-id
              :name (format nil "mrepl-remote-~a" remote-id)))
         (thread (channel-thread ch)))

    (when thread
      (swank-backend:send thread `(:serve-channel ,ch)))
    (list (channel-id ch)
          (swank-backend:thread-id (or thread (swank-backend:current-thread)))
          (package-name pkg)
          (package-prompt pkg))))

(defslyfun eval-in-mrepl (remote-id string)
  "Like MREPL-EVAL, but not run in channel's thread."
  (mrepl-eval (find-channel remote-id) string))

(defvar *history* nil)

(defmethod swank::drop-unprocessed-events :after (repl)
  "Empty REPL of events, then send prompt to Emacs."
  (with-slots (mode) repl
    (let ((old-mode mode))
      (setf mode :drop)
      (unwind-protect
           (process-requests t)
        (setf mode old-mode)))
    (with-listener repl
      (send-prompt repl))))

(define-channel-method :process ((c mrepl) string)
  (ecase (mrepl-mode c)
    (:eval (mrepl-eval c string))
    (:read (mrepl-read c string))
    (:drop)))

(defun mrepl-get-object-from-history (entry-idx value-idx)
  (nth value-idx (aref *history* entry-idx)))

(define-channel-method :inspect-object ((r mrepl) entry-idx value-idx)
  (with-listener r
    (send-to-remote-channel
       (mrepl-remote-id r)
       `(:inspect-object
         ,(swank::inspect-object (mrepl-get-object-from-history entry-idx value-idx))))))

(define-channel-method :teardown ((r mrepl))
  (close-channel r)
  (throw 'stop-processing 'listener-teardown))

(defun copy-values-to-repl (repl values)
  ;; FIXME: Notice some duplication to MREPL-EVAL.
  (setq /// //  // /  / values
        *** **  ** *  * (car values))
  (vector-push-extend values *history*)
  (send-to-remote-channel (mrepl-remote-id repl)
                          `(:copy-to-repl ,(mapcar #'swank::to-line values)))
  (send-prompt repl))

(define-channel-method :sync-package-and-default-directory ((r mrepl)
                                                            package-name
                                                            directory)
  (with-listener r
    (let ((package (swank::guess-package package-name)))
      (swank:set-default-directory directory)
      (and package (setq *package* package))
      (copy-values-to-repl r (list *package* *default-pathname-defaults*)))))

(defvar *saved-object* nil)

(defslyfun globally-save-object (slyfun &rest args)
  "Apply SLYFUN to ARGS and save the value.
 The saved value should be visible to all threads and retrieved via a
 :COPY-TO-REPL message."
  (setq *saved-object* (apply slyfun args))
  t)

(define-channel-method :copy-to-repl ((r mrepl) &optional object-indexes)
  (with-listener r
    ;; FIXME: Notice some duplication to MREPL-EVAL.
    (let ((object (if object-indexes
                      (mrepl-get-object-from-history (first object-indexes) (second object-indexes))
                      *saved-object*)))
      (copy-values-to-repl r (list object)))))


(defun mrepl-eval (repl string)
  (let ((aborted t)
        (results))
    (with-listener repl
      (unwind-protect
           (handler-bind
               ((error #'(lambda (err)
                           (setq aborted err))))
             (setq results (with-sly-interrupts (read-eval string))
                   aborted nil))
        (flush-listener-streams repl)
        (cond (aborted
               (send-to-remote-channel (mrepl-remote-id repl)
                                       `(:evaluation-aborted
                                         ,(prin1-to-string aborted))))
              (t
               (setq /// //  // /  / results
                     *** **  ** *  * (car results)
                     +++ ++  ++ + )
               (vector-push-extend results *history*)
               (send-to-remote-channel
                (mrepl-remote-id repl)
                `(:write-values ,(mapcar #'swank::to-line
                                         results)))
               (send-prompt repl)))))))

(defun send-prompt (repl)
  (send-to-remote-channel (mrepl-remote-id repl)
                          `(:prompt ,(package-name *package*)
                                    ,(package-prompt *package*))))

(defun mrepl-read (repl string)
  (with-slots (tag) repl
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

(defun make-mrepl-output-stream (repl)
  (or (and *use-dedicated-output-stream*
           (open-dedicated-output-stream repl))
      (swank-backend:make-output-stream
       (lambda (string)
         (send-to-remote-channel (mrepl-remote-id repl) `(:write-string ,string))))))

(defun make-mrepl-input-stream (repl)
  (swank-backend:make-input-stream
   (lambda () (read-input repl))))

(defun set-mode (repl new-mode)
  (with-slots (mode remote-id) repl
    (unless (eq mode new-mode)
      (send-to-remote-channel remote-id `(:set-read-mode ,new-mode)))
    (setf mode new-mode)))

(defun read-input (repl)
  (with-slots (mode tag remote-id) repl
    (force-output)
    (let ((old-mode mode)
          (old-tag tag))
      (setf tag (cons nil nil))
      (set-mode repl :read)
      (unwind-protect
           (catch tag (process-requests nil))
        (setf tag old-tag)
        (set-mode repl old-mode)))))


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

(defun open-dedicated-output-stream (repl)
  "Establish a dedicated output connection to Emacs.

Notify Emacs's REPL that a socket is listening at a local ephemeral
port. This is an optimized way for Lisp to deliver output to Emacs."
  (let ((socket (swank-backend:create-socket swank::*loopback-interface*
                                             *dedicated-output-stream-port*))
        ;; HACK: hardcoded coding system
        (ef (swank::find-external-format-or-lose "utf-8")))
    (unwind-protect
         (let ((port (swank-backend:local-port socket)))
           (send-to-remote-channel (mrepl-remote-id repl)
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
