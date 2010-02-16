;;;; -*- indent-tabs-mode: nil -*-
;;;
;;; swank-ecl.lisp --- SLIME backend for ECL.
;;;
;;; This code has been placed in the Public Domain.  All warranties
;;; are disclaimed.
;;;

;;; Administrivia

(in-package :swank-backend)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((version (find-symbol "+ECL-VERSION-NUMBER+" :EXT)))
    (when (or (not version) (< (symbol-value version) 100201))
      (error "~&IMPORTANT:~%  ~
              The version of ECL you're using (~A) is too old.~%  ~
              Please upgrade to at least 10.2.1.~%  ~
              Sorry for the inconvenience.~%~%"
             (lisp-implementation-version)))))

(declaim (optimize (debug 3)))

;;; Swank-mop

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import-from :gray *gray-stream-symbols* :swank-backend)

  (import-swank-mop-symbols :clos
    '(:eql-specializer
      :eql-specializer-object
      :generic-function-declarations
      :specializer-direct-methods
      :compute-applicable-methods-using-classes)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (probe-file "sys:serve-event.fas")
    (require :serve-event)
    (pushnew :serve-event *features*)))


;;;; TCP Server

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require 'sockets))

(defun resolve-hostname (name)
  (car (sb-bsd-sockets:host-ent-addresses
        (sb-bsd-sockets:get-host-by-name name))))

(defimplementation create-socket (host port)
  (let ((socket (make-instance 'sb-bsd-sockets:inet-socket
			       :type :stream
			       :protocol :tcp)))
    (setf (sb-bsd-sockets:sockopt-reuse-address socket) t)
    (sb-bsd-sockets:socket-bind socket (resolve-hostname host) port)
    (sb-bsd-sockets:socket-listen socket 5)
    socket))

(defimplementation local-port (socket)
  (nth-value 1 (sb-bsd-sockets:socket-name socket)))

(defimplementation close-socket (socket)
  (when (eq (preferred-communication-style) :fd-handler)
    (remove-fd-handlers socket))
  (sb-bsd-sockets:socket-close socket))

(defimplementation accept-connection (socket
                                      &key external-format
                                      buffering timeout)
  (declare (ignore buffering timeout external-format))
  (sb-bsd-sockets:socket-make-stream (accept socket)
                                     :output t
                                     :input t
                                     :element-type 'base-char))
(defun accept (socket)
  "Like socket-accept, but retry on EAGAIN."
  (loop (handler-case
            (return (sb-bsd-sockets:socket-accept socket))
          (sb-bsd-sockets:interrupted-error ()))))

(defimplementation preferred-communication-style ()
  ;; ECL on Windows does not provide condition-variables
  (or #+ (and threads (not win32) (not win64)) :spawn
      #+serve-event :fd-handler
      nil))

(defvar *external-format-to-coding-system*
  '((:iso-8859-1
     "latin-1" "latin-1-unix" "iso-latin-1-unix" 
     "iso-8859-1" "iso-8859-1-unix")
    (:utf-8 "utf-8" "utf-8-unix")))

(defimplementation find-external-format (coding-system)
  (car (rassoc-if (lambda (x) (member coding-system x :test #'equal))
                  *external-format-to-coding-system*)))


;;;; Unix signals

(defvar *original-sigint-handler* #'si:terminal-interrupt)

(defimplementation install-sigint-handler (handler)
  (declare (function handler))
  (let ((old-handler (symbol-function 'si:terminal-interrupt)))
    (setf (symbol-function 'si:terminal-interrupt)
          (if (eq handler *original-sigint-handler*)
              handler
              (lambda (&rest args)
                (declare (ignore args))
                (funcall handler)
                (continue))))
    old-handler))


(defimplementation getpid ()
  (si:getpid))

(defimplementation set-default-directory (directory)
  (ext:chdir (namestring directory))  ; adapts *DEFAULT-PATHNAME-DEFAULTS*.
  (default-directory))

(defimplementation default-directory ()
  (namestring (ext:getcwd)))

(defimplementation quit-lisp ()
  (ext:quit))


;;;; Serve Event Handlers

;;; FIXME: verify this is correct implementation

#+serve-event
(progn
  
(defun socket-fd (socket)
  (etypecase socket
    (fixnum socket)
    (sb-bsd-sockets:socket (sb-bsd-sockets:socket-file-descriptor socket))
    (file-stream (si:file-stream-fd socket))))

(defvar *descriptor-handlers* (make-hash-table :test 'eql))

(defimplementation add-fd-handler (socket fun)
  (let* ((fd (socket-fd socket))
         (handler (gethash fd *descriptor-handlers*)))
    (when handler
      (serve-event:remove-fd-handler handler))
    (setf (gethash fd *descriptor-handlers*)
          (serve-event:add-fd-handler fd :input #'(lambda (x)
                                                    (declare (ignore x))
                                                    (funcall fun))))
    (serve-event:serve-event)))

(defimplementation remove-fd-handlers (socket)
  (let ((handler (gethash (socket-fd socket) *descriptor-handlers*)))
    (when handler
      (serve-event:remove-fd-handler handler))))

(defimplementation wait-for-input (streams &optional timeout)
  (assert (member timeout '(nil t)))
  (loop
     (let ((ready (remove-if-not #'listen streams)))
       (when ready (return ready)))
     ;; (when timeout (return nil))
     (when (check-slime-interrupts) (return :interrupt))
     (serve-event:serve-event)))

) ; #+serve-event (progn ...


;;;; Compilation

(defvar *buffer-name* nil)
(defvar *buffer-start-position*)

(defun signal-compiler-condition (&rest args)
  (signal (apply #'make-condition 'compiler-condition args)))

(defun handle-compiler-message (condition)
  ;; ECL emits lots of noise in compiler-notes, like "Invoking
  ;; external command".
  (unless (typep condition 'c::compiler-note)
    (signal-compiler-condition
     :original-condition condition
     :message (format nil "~A" condition)
     :severity (etypecase condition
                 (c:compiler-fatal-error :error)
                 (c:compiler-error :error)
                 (error            :error)
                 (style-warning    :style-warning)
                 (warning          :warning))
     :location (condition-location condition))))

(defun condition-location (condition)
  (let ((file     (c:compiler-message-file condition))
        (position (c:compiler-message-file-position condition)))
    (if (and position (not (minusp position)))
        (if *buffer-name*
            (make-location `(:buffer ,*buffer-name*)
                           `(:offset ,*buffer-start-position* ,position)
                           `(:align t))
            (make-location `(:file ,(namestring file))
                           `(:position ,(1+ position))
                           `(:align t)))
        (make-error-location "No location found."))))

(defimplementation call-with-compilation-hooks (function)
  (handler-bind ((c:compiler-message #'handle-compiler-message))
    (funcall function)))

(defimplementation swank-compile-file (input-file output-file
                                       load-p external-format)
  (declare (ignore external-format))
  (with-compilation-hooks ()
    (compile-file input-file :output-file output-file :load load-p)))

(defimplementation swank-compile-string (string &key buffer position filename
                                         policy)
  (declare (ignore filename policy))
  (with-compilation-hooks ()
    (let ((*buffer-name* buffer)
          (*buffer-start-position* position))
      (with-input-from-string (s string)
        (not (nth-value 2 (compile-from-stream s :load t)))))))

(defun compile-from-stream (stream &rest args)
  (let ((file (si::mkstemp "TMP:ECLXXXXXX")))
    (with-open-file (s file :direction :output :if-exists :overwrite)
      (do ((line (read-line stream nil) (read-line stream nil)))
	  ((not line))
	(write-line line s)))
    (unwind-protect
         (apply #'compile-file file args)
      (delete-file file))))


;;;; Documentation

(defun grovel-docstring-for-arglist (name type)
  (flet ((compute-arglist-offset (docstring)
           (when docstring
             (let ((pos1 (search "Args: " docstring)))
               (if pos1
                   (+ pos1 6)
                   (let ((pos2 (search "Syntax: " docstring)))
                     (when pos2
                       (+ pos2 8))))))))
    (let* ((docstring (si::get-documentation name type))
           (pos (compute-arglist-offset docstring)))
      (if pos
          (multiple-value-bind (arglist errorp)
              (ignore-errors
                (values (read-from-string docstring t nil :start pos)))
            (if (or errorp (not (listp arglist)))
                :not-available
                ; ECL for some reason includes macro name at the first place
                (if (or (macro-function name)
                        (special-operator-p name)) 
                    (cdr arglist)
                    arglist)))
          :not-available ))))

(defimplementation arglist (name)
  (cond ((and (symbolp name) (special-operator-p name))
         (grovel-docstring-for-arglist name 'function))
        ((and (symbolp name) (macro-function name))
         (grovel-docstring-for-arglist name 'function))
        ((or (functionp name) (fboundp name))
         (multiple-value-bind (name fndef)
             (if (functionp name)
                 (values (function-name name) name)
                 (values name (fdefinition name)))
           (typecase fndef
             (generic-function
              (clos::generic-function-lambda-list fndef))
             (compiled-function
              (grovel-docstring-for-arglist name 'function))
             (function
              (let ((fle (function-lambda-expression fndef)))
                (case (car fle)
                  (si:lambda-block (caddr fle))
                  (t               :not-available)))))))
        (t :not-available)))

(defimplementation function-name (f)
  (typecase f
    (generic-function (clos:generic-function-name f))
    (function (si:compiled-function-name f))))

;; FIXME
;; (defimplementation macroexpand-all (form))

(defimplementation describe-symbol-for-emacs (symbol)
  (let ((result '()))
    (dolist (type '(:VARIABLE :FUNCTION :CLASS))
      (let ((doc (describe-definition symbol type)))
        (when doc
          (setf result (list* type doc result)))))
    result))

(defimplementation describe-definition (name type)
  (case type
    (:variable (documentation name 'variable))
    (:function (documentation name 'function))
    (:class (documentation name 'class))
    (t nil)))

;;; Debugging

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import
   '(si::*break-env*
     si::*ihs-top*
     si::*ihs-current*
     si::*ihs-base*
     si::*frs-base*
     si::*frs-top*
     si::*tpl-commands*
     si::*tpl-level*
     si::frs-top
     si::ihs-top
     si::ihs-fun
     si::ihs-env
     si::sch-frs-base
     si::set-break-env
     si::set-current-ihs
     si::tpl-commands)))

(defun make-invoke-debugger-hook (hook)
  (when hook
    #'(lambda (condition old-hook)
        ;; Regard *debugger-hook* if set by user.
        (if *debugger-hook*
            nil         ; decline, *DEBUGGER-HOOK* will be tried next.
            (funcall hook condition old-hook)))))

(defimplementation install-debugger-globally (function)
  (setq *debugger-hook* function)
  (setq ext:*invoke-debugger-hook* (make-invoke-debugger-hook function)))

(defimplementation call-with-debugger-hook (hook fun)
  (let ((*debugger-hook* hook)
        (ext:*invoke-debugger-hook* (make-invoke-debugger-hook hook))
        (*ihs-base* (ihs-top)))
    (funcall fun)))

(defvar *backtrace* '())

(defun in-swank-package-p (x)
  (and
   (symbolp x)
   (member (symbol-package x)
           (list #.(find-package :swank)
                 #.(find-package :swank-backend)
                 #.(ignore-errors (find-package :swank-mop))
                 #.(ignore-errors (find-package :swank-loader))))
   t))

(defun is-swank-source-p (name)
  (setf name (pathname name))
  (pathname-match-p
   name
   (make-pathname :defaults swank-loader::*source-directory*
                  :name (pathname-name name)
                  :type (pathname-type name)
                  :version (pathname-version name))))

(defun is-ignorable-fun-p (x)
  (or
   (in-swank-package-p (frame-name x))
   (multiple-value-bind (file position)
       (ignore-errors (si::bc-file (car x)))
     (declare (ignore position))
     (if file (is-swank-source-p file)))))

(defimplementation call-with-debugging-environment (debugger-loop-fn)
  (declare (type function debugger-loop-fn))
  (let* ((*tpl-commands* si::tpl-commands)
         (*ihs-top* (ihs-top))
         (*ihs-current* *ihs-top*)
         (*frs-base* (or (sch-frs-base *frs-top* *ihs-base*) (1+ (frs-top))))
         (*frs-top* (frs-top))
         (*read-suppress* nil)
         (*tpl-level* (1+ *tpl-level*))
         (*backtrace* (loop for ihs from 0 below *ihs-top*
                            collect (list (si::ihs-fun ihs)
                                          (si::ihs-env ihs)
                                          nil))))
    (declare (special *ihs-current*))
    (loop for f from *frs-base* until *frs-top*
          do (let ((i (- (si::frs-ihs f) *ihs-base* 1)))
               (when (plusp i)
                 (let* ((x (elt *backtrace* i))
                        (name (si::frs-tag f)))
                   (unless (si::fixnump name)
                     (push name (third x)))))))
    (setf *backtrace* (remove-if #'is-ignorable-fun-p (nreverse *backtrace*)))
    (set-break-env)
    (set-current-ihs)
    (let ((*ihs-base* *ihs-top*))
      (funcall debugger-loop-fn))))

(defimplementation compute-backtrace (start end)
  (when (numberp end)
    (setf end (min end (length *backtrace*))))
  (loop for f in (subseq *backtrace* start end)
        collect f))

(defun frame-name (frame)
  (let ((x (first frame)))
    (if (symbolp x)
      x
      (function-name x))))

(defun function-position (fun)
  (multiple-value-bind (file position)
      (si::bc-file fun)
    (and file (make-location `(:file ,file) `(:position ,position)))))

(defun frame-function (frame)
  (let* ((x (first frame))
         fun position)
    (etypecase x
      (symbol (and (fboundp x)
                   (setf fun (fdefinition x)
                         position (function-position fun))))
      (function (setf fun x position (function-position x))))
    (values fun position)))

(defun frame-decode-env (frame)
  (let ((functions '())
        (blocks '())
        (variables '()))
    (setf frame (si::decode-ihs-env (second frame)))
    (dolist (record frame)
      (let* ((record0 (car record))
	     (record1 (cdr record)))
	(cond ((or (symbolp record0) (stringp record0))
	       (setq variables (acons record0 record1 variables)))
	      ((not (si::fixnump record0))
	       (push record1 functions))
	      ((symbolp record1)
	       (push record1 blocks))
	      (t
	       ))))
    (values functions blocks variables)))

(defimplementation print-frame (frame stream)
  (format stream "~A" (first frame)))

(defimplementation frame-source-location (frame-number)
  (nth-value 1 (frame-function (elt *backtrace* frame-number))))

(defimplementation frame-catch-tags (frame-number)
  (third (elt *backtrace* frame-number)))

(defimplementation frame-locals (frame-number)
  (loop for (name . value) in (nth-value 2 (frame-decode-env (elt *backtrace* frame-number)))
        with i = 0
        collect (list :name name :id (prog1 i (incf i)) :value value)))

(defimplementation frame-var-value (frame-number var-id)
  (elt (nth-value 2 (frame-decode-env (elt *backtrace* frame-number)))
       var-id))

(defimplementation disassemble-frame (frame-number)
  (let ((fun (frame-fun (elt *backtrace* frame-number))))
    (disassemble fun)))

(defimplementation eval-in-frame (form frame-number)
  (let ((env (second (elt *backtrace* frame-number))))
    (si:eval-with-env form env)))

;;;; Inspector

(defmethod emacs-inspect ((o t))
  ; ecl clos support leaves some to be desired
  (cond
    ((streamp o)
     (list*
      (format nil "~S is an ordinary stream~%" o)
      (append
       (list
        "Open for "
        (cond
          ((ignore-errors (interactive-stream-p o)) "Interactive")
          ((and (input-stream-p o) (output-stream-p o)) "Input and output")
          ((input-stream-p o) "Input")
          ((output-stream-p o) "Output"))
        `(:newline) `(:newline))
       (label-value-line*
        ("Element type" (stream-element-type o))
        ("External format" (stream-external-format o)))
       (ignore-errors (label-value-line*
                       ("Broadcast streams" (broadcast-stream-streams o))))
       (ignore-errors (label-value-line*
                       ("Concatenated streams" (concatenated-stream-streams o))))
       (ignore-errors (label-value-line*
                       ("Echo input stream" (echo-stream-input-stream o))))
       (ignore-errors (label-value-line*
                       ("Echo output stream" (echo-stream-output-stream o))))
       (ignore-errors (label-value-line*
                       ("Output String" (get-output-stream-string o))))
       (ignore-errors (label-value-line*
                       ("Synonym symbol" (synonym-stream-symbol o))))
       (ignore-errors (label-value-line*
                       ("Input stream" (two-way-stream-input-stream o))))
       (ignore-errors (label-value-line*
                       ("Output stream" (two-way-stream-output-stream o)))))))
    ((si:instancep o)
     (let* ((cl (si:instance-class o))
            (slots (clos:class-slots cl)))
       (list* (format nil "~S is an instance of class ~A~%"
                      o (clos::class-name cl))
               (loop for x in slots append
                    (let* ((name (clos:slot-definition-name x))
                           (value (clos::slot-value o name)))
                      (list
                       (format nil "~S: " name)
                       `(:value ,value)
                       `(:newline)))))))))

;;;; Definitions

(defimplementation find-definitions (name)
  (if (fboundp name)
      (let ((tmp (find-source-location (symbol-function name))))
        `(((defun ,name) ,tmp)))))

(defimplementation find-source-location (obj)
  (or
   (typecase obj
     (function
      (multiple-value-bind (file pos) (ignore-errors (si::bc-file obj))
        (if (and file pos) 
            (make-location
              `(:file ,(namestring file))
              `(:position ,pos)
              `(:snippet
                ,(with-open-file (s file)
                   (file-position s pos)
                   (skip-comments-and-whitespace s)
                   (read-snippet s))))))))
   `(:error ,(format nil "Source definition of ~S not found" obj))))

;;;; Profiling

#+profile
(progn

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require 'profile))

(defimplementation profile (fname)
  (when fname (eval `(profile:profile ,fname))))

(defimplementation unprofile (fname)
  (when fname (eval `(profile:unprofile ,fname))))

(defimplementation unprofile-all ()
  (profile:unprofile-all)
  "All functions unprofiled.")

(defimplementation profile-report ()
  (profile:report))

(defimplementation profile-reset ()
  (profile:reset)
  "Reset profiling counters.")

(defimplementation profiled-functions ()
  (profile:profile))

(defimplementation profile-package (package callers methods)
  (declare (ignore callers methods))
  (eval `(profile:profile ,(package-name (find-package package)))))
)                                       ; progn

;;;; Threads

#+threads
(progn
  (defvar *thread-id-counter* 0)

  (defparameter *thread-id-map* (make-hash-table))

  (defvar *thread-id-map-lock*
    (mp:make-lock :name "thread id map lock"))

  (defimplementation spawn (fn &key name)
    (mp:process-run-function name fn))

  (defimplementation thread-id (target-thread)
    (block thread-id
      (mp:with-lock (*thread-id-map-lock*)
        ;; Does TARGET-THREAD have an id already?
        (maphash (lambda (id thread-pointer)
                   (let ((thread (si:weak-pointer-value thread-pointer)))
                     (cond ((not thread)
                            (remhash id *thread-id-map*))
                           ((eq thread target-thread)
                            (return-from thread-id id)))))
                 *thread-id-map*)
        ;; TARGET-THREAD not found in *THREAD-ID-MAP*
        (let ((id (incf *thread-id-counter*))
              (thread-pointer (si:make-weak-pointer target-thread)))
          (setf (gethash id *thread-id-map*) thread-pointer)
          id))))

  (defimplementation find-thread (id)
    (mp:with-lock (*thread-id-map-lock*)
      (let* ((thread-pointer (gethash id *thread-id-map*))
             (thread (and thread-pointer (si:weak-pointer-value thread-pointer))))
        (unless thread
          (remhash id *thread-id-map*))
        thread)))

  (defimplementation thread-name (thread)
    (mp:process-name thread))

  (defimplementation thread-status (thread)
    (if (mp:process-active-p thread)
        "RUNNING"
        "STOPPED"))

  (defimplementation make-lock (&key name)
    (mp:make-lock :name name))

  (defimplementation call-with-lock-held (lock function)
    (declare (type function function))
    (mp:with-lock (lock) (funcall function)))

  (defimplementation current-thread ()
    mp:*current-process*)

  (defimplementation all-threads ()
    (mp:all-processes))

  (defimplementation interrupt-thread (thread fn)
    (mp:interrupt-process thread fn))

  (defimplementation kill-thread (thread)
    (mp:process-kill thread))

  (defimplementation thread-alive-p (thread)
    (mp:process-active-p thread))

  (defvar *mailbox-lock* (mp:make-lock :name "mailbox lock"))
  (defvar *mailboxes* (list))
  (declaim (type list *mailboxes*))

  (defstruct (mailbox (:conc-name mailbox.))
    thread
    (mutex (mp:make-lock))
    (cvar  (mp:make-condition-variable))
    (queue '() :type list))

  (defun mailbox (thread)
    "Return THREAD's mailbox."
    (mp:with-lock (*mailbox-lock*)
      (or (find thread *mailboxes* :key #'mailbox.thread)
          (let ((mb (make-mailbox :thread thread)))
            (push mb *mailboxes*)
            mb))))

  (defimplementation send (thread message)
    (let* ((mbox (mailbox thread))
           (mutex (mailbox.mutex mbox)))
      (mp:with-lock (mutex)
        (setf (mailbox.queue mbox)
              (nconc (mailbox.queue mbox) (list message)))
        (mp:condition-variable-broadcast (mailbox.cvar mbox)))))

  (defimplementation receive-if (test &optional timeout)
    (let* ((mbox (mailbox (current-thread)))
           (mutex (mailbox.mutex mbox)))
      (assert (or (not timeout) (eq timeout t)))
      (loop
         (check-slime-interrupts)
         (mp:with-lock (mutex)
           (let* ((q (mailbox.queue mbox))
                  (tail (member-if test q)))
             (when tail
               (setf (mailbox.queue mbox) (nconc (ldiff q tail) (cdr tail)))
               (return (car tail))))
           (when (eq timeout t) (return (values nil t)))
           (mp:condition-variable-timedwait (mailbox.cvar mbox)
                                            mutex
                                            0.2)))))

  ) ; #+threads (progn ...
