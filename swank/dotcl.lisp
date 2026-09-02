;;;; -*- indent-tabs-mode: nil -*-
;;;
;;; swank-dotcl.lisp --- SLIME backend for dotcl
;;;
;;; This code has been placed in the Public Domain.  All warranties are
;;; disclaimed.
;;;
;;; dotcl is a Common Lisp implementation on .NET/CLR.  The facilities this
;;; backend needs are contrib modules shipped inside a dotcl release, pulled
;;; in by the system definition rather than from here:
;;;
;;;   dotcl-socket   sockets                   dotcl-thread   threads and locks
;;;   dotcl-gray     Gray streams
;;;
;;; The Gray streams package must exist before swank/gray.lisp is compiled, which
;;; is why this file is loaded ahead of it.

(defpackage swank/dotcl
  (:use cl swank/backend))

(in-package swank/dotcl)

;; The loader compiles this file directly, so the contrib modules are pulled
;; in here rather than through a system definition.
(require "dotcl-socket")
(require "dotcl-thread")
(require "dotcl-gray")

;;;; Connection info

(defimplementation gray-package-name ()
  "DOTCL-GRAY")

(defimplementation lisp-implementation-type-name ()
  "dotcl")

(defimplementation lisp-implementation-program ()
  "dotcl")

(defimplementation getpid ()
  0) ; TODO: implement via .NET Process.GetCurrentProcess().Id

(defimplementation command-line-args ()
  nil)

(defimplementation quit-lisp ()
  #+ignore (ext:quit)) ; TODO

;;;; Communication style

(defimplementation preferred-communication-style ()
  :spawn)

;;;; UTF-8 encoding (via System.Text.Encoding.UTF8)

(defun %utf8-encoding ()
  (dotnet:static "System.Text.Encoding" "get_UTF8"))

(defimplementation string-to-utf8 (string)
  "Encode STRING to a CL (unsigned-byte 8) vector via .NET's UTF-8 encoder."
  (let* ((bytes (dotnet:invoke (%utf8-encoding) "GetBytes" string))
         (n     (dotnet:invoke bytes "get_Length"))
         (out   (make-array n :element-type '(unsigned-byte 8))))
    (dotimes (i n out)
      ;; D1468: a Byte[] element comes back as a real CL integer, no unboxing needed.
      (setf (aref out i) (aref bytes i)))))

(defimplementation utf8-to-string (octets)
  "Decode a CL (unsigned-byte 8) sequence of UTF-8 OCTETS to a string via .NET."
  (let* ((n     (length octets))
         (bytes (dotnet:make-array "System.Byte" n)))
    (dotimes (i n)
      ;; D1469: a CL Fixnum stores straight into a Byte[] element.
      (setf (aref bytes i) (elt octets i)))
    (dotnet:invoke (%utf8-encoding) "GetString" bytes)))

;;;; TCP Server (using dotcl-socket)

(defimplementation create-socket (host port &key backlog)
  (dotcl-socket:make-server-socket host port :backlog (or backlog 5)))

(defimplementation local-port (socket)
  (dotcl-socket:local-port socket))

(defimplementation close-socket (socket)
  (dotcl-socket:socket-close socket))

(defimplementation accept-connection (socket &key
                                      external-format
                                      buffering timeout)
  (declare (ignore external-format buffering timeout))
  (dotcl-socket:socket-accept socket))

;;;; Threading (using dotcl-thread)

(defvar *thread-id-counter* 0)
(defvar *thread-id-lock* (dotcl-thread:make-lock "thread-id-lock"))
(defvar *thread-id-map* (make-hash-table :test 'eq))
(defvar *thread-id-reverse* (make-hash-table :test 'eql))

(defun next-thread-id ()
  (dotcl-thread:with-lock-held (*thread-id-lock*)
    (incf *thread-id-counter*)))

(defun ensure-thread-id (thread)
  (or (gethash thread *thread-id-map*)
      (dotcl-thread:with-lock-held (*thread-id-lock*)
        (or (gethash thread *thread-id-map*)
            (let ((id (incf *thread-id-counter*)))
              (setf (gethash thread *thread-id-map*) id)
              (setf (gethash id *thread-id-reverse*) thread)
              id)))))

(defimplementation spawn (fn &key name)
  (let ((thread (dotcl-thread:make-thread fn :name (or name "anonymous"))))
    (ensure-thread-id thread)
    thread))

(defimplementation thread-id (thread)
  (ensure-thread-id thread))

(defimplementation find-thread (id)
  (gethash id *thread-id-reverse*))

(defimplementation thread-name (thread)
  (princ-to-string (dotcl-thread:thread-name thread)))

(defimplementation thread-status (thread)
  (if (dotcl-thread:thread-alive-p thread)
      "Running"
      "Stopped"))

(defimplementation current-thread ()
  (dotcl-thread:current-thread))

(defimplementation all-threads ()
  ;; Return known threads (no .NET API to list all managed threads)
  (let ((threads nil))
    (maphash (lambda (thread id)
               (declare (ignore id))
               (when (dotcl-thread:thread-alive-p thread)
                 (push thread threads)))
             *thread-id-map*)
    threads))

(defimplementation thread-alive-p (thread)
  (dotcl-thread:thread-alive-p thread))

(defimplementation interrupt-thread (thread fn)
  (declare (ignore thread fn))
  ;; TODO: .NET Thread.Interrupt or similar
  nil)

(defimplementation kill-thread (thread)
  (dotcl-thread:destroy-thread thread))

;;;; Mailbox (per-thread message queue)

(defvar *mailbox-lock* (dotcl-thread:make-lock "mailbox-lock"))
(defvar *mailboxes* (make-hash-table :test 'eq))

(defstruct (mailbox (:conc-name mailbox.))
  thread
  (lock (dotcl-thread:make-lock "mbox"))
  (queue nil :type list))

(defun mailbox (thread)
  "Return THREAD's mailbox, creating one if needed."
  (dotcl-thread:with-lock-held (*mailbox-lock*)
    (or (gethash thread *mailboxes*)
        (setf (gethash thread *mailboxes*)
              (make-mailbox :thread thread)))))

(defimplementation send (thread message)
  (let* ((mbox (mailbox thread))
         (lock (mailbox.lock mbox)))
    (dotcl-thread:with-lock-held (lock)
      (setf (mailbox.queue mbox)
            (nconc (mailbox.queue mbox) (list message))))))

(defimplementation receive-if (test &optional timeout)
  (let* ((mbox (mailbox (current-thread)))
         (lock (mailbox.lock mbox)))
    (assert (or (not timeout) (eq timeout t)))
    (loop
      (check-slime-interrupts)
      (dotcl-thread:with-lock-held (lock)
        (let* ((q (mailbox.queue mbox))
               (tail (member-if test q)))
          (when tail
            (setf (mailbox.queue mbox) (nconc (ldiff q tail) (cdr tail)))
            (return (car tail))))
        (when (eq timeout t) (return (values nil t))))
      ;; No condition variable yet — busy-wait with sleep
      (sleep 0.02))))

(defimplementation wake-thread (thread)
  ;; No condition variable — receive-if polls
  (declare (ignore thread))
  nil)

;;;; Locks

(defimplementation make-lock (&key name)
  (dotcl-thread:make-lock (or name "anonymous")))

(defimplementation call-with-lock-held (lock function)
  (dotcl-thread:with-lock-held (lock)
    (funcall function)))

;;;; Compilation (stubs)

(defimplementation call-with-compilation-hooks (func)
  (funcall func))

(defimplementation swank-compile-string (string &key buffer position filename
                                         line column policy)
  (declare (ignore buffer position filename line column policy))
  (handler-case
      (progn (eval (read-from-string string))
             t)
    (error (c)
      (list :message (princ-to-string c)))))

(defimplementation swank-compile-file (input-file output-file load-p
                                       external-format &key policy)
  (declare (ignore output-file external-format policy))
  (when load-p
    (load input-file)))

(defimplementation find-external-format (coding-system)
  (declare (ignore coding-system))
  :default)

(defimplementation guess-external-format (pathname)
  (declare (ignore pathname))
  :default)

;;;; Introspection (minimal)

(defimplementation arglist (name)
  (multiple-value-bind (arglist foundp)
      (dotcl:function-lambda-list name)
    (if foundp arglist :not-available)))

(defimplementation function-name (function)
  (declare (ignore function))
  nil)

(defimplementation valid-function-name-p (form)
  (or (symbolp form)
      (and (consp form) (eq (car form) 'setf) (symbolp (cadr form)))))

(defimplementation type-specifier-p (symbol)
  (declare (ignore symbol))
  nil)

(defimplementation describe-symbol-for-emacs (symbol)
  (let ((result nil))
    (when (fboundp symbol)
      (push (cons :function (documentation symbol 'function)) result))
    (when (boundp symbol)
      (push (cons :variable (documentation symbol 'variable)) result))
    result))

(defimplementation describe-definition (name type)
  (declare (ignore type))
  (format nil "~S" (if (fboundp name) (symbol-function name) name)))

(defimplementation find-definitions (name)
  (declare (ignore name))
  nil)

(defimplementation find-source-location (object)
  (declare (ignore object))
  nil)

;;;; Debugging (stubs)

(defimplementation call-with-debugging-environment (debugger-loop-fn)
  (funcall debugger-loop-fn))

(defimplementation call-with-debugger-hook (hook fun)
  (let ((*debugger-hook* hook))
    (funcall fun)))

(defimplementation install-debugger-globally (function)
  (setf *debugger-hook* function))

(defimplementation compute-backtrace (start end)
  (declare (ignore start end))
  nil)

(defimplementation print-frame (frame stream)
  (format stream "~A" frame))

(defimplementation frame-source-location (frame-number)
  (declare (ignore frame-number))
  nil)

(defimplementation frame-catch-tags (frame-number)
  (declare (ignore frame-number))
  nil)

(defimplementation frame-locals (frame-number)
  (declare (ignore frame-number))
  nil)

(defimplementation frame-var-value (frame-number var-id)
  (declare (ignore frame-number var-id))
  nil)

(defimplementation eval-in-frame (form frame-number)
  (declare (ignore frame-number))
  (eval form))

(defimplementation frame-call (frame-number)
  (declare (ignore frame-number))
  nil)

(defimplementation print-condition (condition stream)
  (format stream "~A" condition))

(defimplementation condition-extras (condition)
  (declare (ignore condition))
  nil)

;;;; Misc

(defimplementation call-with-syntax-hooks (fn)
  (funcall fn))

(defimplementation package-local-nicknames (package)
  (declare (ignore package))
  nil)

(defimplementation set-stream-timeout (stream timeout)
  (declare (ignore stream timeout))
  nil)

(defimplementation default-directory ()
  (namestring *default-pathname-defaults*))

(defimplementation set-default-directory (directory)
  (setf *default-pathname-defaults* (pathname directory))
  (default-directory))

(defimplementation macroexpand-all (form &optional env)
  (declare (ignore env))
  (macroexpand form))

;;; Register the current thread so swank knows about it
(ensure-thread-id (dotcl-thread:current-thread))
