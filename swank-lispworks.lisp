;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; swank-lispworks.lisp --- LispWorks specific code for SLIME. 
;;;
;;; Created 2003, Helmut Eller
;;;
;;; This code has been placed in the Public Domain.  All warranties
;;; are disclaimed.
;;;

(in-package :swank-backend)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "comm"))

(import
 '(stream:fundamental-character-output-stream
   stream:stream-write-char
   stream:stream-force-output
   stream:fundamental-character-input-stream
   stream:stream-read-char
   stream:stream-listen
   stream:stream-unread-char
   stream:stream-clear-input
   stream:stream-line-column
   ))

;;; TCP server

(defimplementation preferred-communication-style ()
  :spawn)

(defun socket-fd (socket)
  (etypecase socket
    (fixnum socket)
    (comm:socket-stream (comm:socket-stream-socket socket))))

(defimplementation create-socket (host port)
  (multiple-value-bind (socket where errno)
      #-lispworks4.1(comm::create-tcp-socket-for-service port :address host)
      #+lispworks4.1(comm::create-tcp-socket-for-service port)
    (cond (socket socket)
          (t (error 'network-error 
              :format-control "~A failed: ~A (~D)"
              :format-arguments (list where 
                                      (list #+unix (lw:get-unix-error errno))
                                      errno))))))

(defimplementation local-port (socket)
  (nth-value 1 (comm:get-socket-address (socket-fd socket))))

(defimplementation close-socket (socket)
  (comm::close-socket (socket-fd socket)))

(defimplementation accept-connection (socket)
  (let ((fd (comm::get-fd-from-socket socket)))
    (assert (/= fd -1))
    (make-instance 'comm:socket-stream :socket fd :direction :io 
                   :element-type 'base-char)))

(defun set-sigint-handler ()
  ;; Set SIGINT handler on Swank request handler thread.
  #-win32
  (sys::set-signal-handler +sigint+ 
                           (make-sigint-handler mp:*current-process*)))

(defimplementation emacs-connected (stream)
  (declare (ignore stream))
  (set-sigint-handler)
  (let ((lw:*handle-warn-on-redefinition* :warn))
    (defmethod stream:stream-soft-force-output  ((o comm:socket-stream))
      (force-output o))
    (defmethod stream:stream-soft-force-output ((o slime-output-stream))
      (force-output o))
    (defmethod env-internals:environment-display-notifier 
        (env &key restarts condition)
      (declare (ignore restarts))
      (funcall (find-symbol (string :swank-debugger-hook) :swank)
               condition *debugger-hook*))))

;;; Unix signals

(defun sigint-handler ()
  (with-simple-restart  (continue "Continue from SIGINT handler.")
    (invoke-debugger "SIGINT")))

(defun make-sigint-handler (process)
  (lambda (&rest args)
    (declare (ignore args))
    (mp:process-interrupt process #'sigint-handler)))

(defimplementation call-without-interrupts (fn)
  (lw:without-interrupts (funcall fn)))

(defimplementation getpid ()
  #+win32 (win32:get-current-process-id)
  #-win32 (system::getpid))

(defimplementation lisp-implementation-type-name ()
  "lispworks")

(defimplementation set-default-directory (directory)
  (namestring (hcl:change-directory directory)))

;;;; Documentation

(defimplementation arglist (symbol)
  (let ((arglist (lw:function-lambda-list symbol)))
    (etypecase arglist
      ((member :dont-know) 
       :not-available)
      (list
       arglist))))

(defimplementation macroexpand-all (form)
  (walker:walk-form form))

(defun generic-function-p (object)
  (typep object 'generic-function))

(defimplementation describe-symbol-for-emacs (symbol)
  "Return a plist describing SYMBOL.
Return NIL if the symbol is unbound."
  (let ((result '()))
    (labels ((first-line (string) 
               (let ((pos (position #\newline string)))
                 (if (null pos) string (subseq string 0 pos))))
             (doc (kind &optional (sym symbol))
               (let ((string (documentation sym kind)))
                 (if string 
                     (first-line string)
                     :not-documented)))
             (maybe-push (property value)
               (when value
                 (setf result (list* property value result)))))
      (maybe-push
       :variable (when (boundp symbol)
                   (doc 'variable)))
      (maybe-push
       :generic-function (if (and (fboundp symbol)
                                  (generic-function-p (fdefinition symbol)))
                             (doc 'function)))
      (maybe-push
       :function (if (and (fboundp symbol)
                          (not (generic-function-p (fdefinition symbol))))
                     (doc 'function)))
      (maybe-push
       :class (if (find-class symbol nil) 
                  (doc 'class)))
      result)))

(defimplementation describe-definition (symbol type)
  (ecase type
    (:variable (describe-symbol symbol))
    (:class (describe (find-class symbol)))
    ((:function :generic-function) (describe-function symbol))))

(defun describe-function (symbol)
  (cond ((fboundp symbol)
         (format t "~%(~A~{ ~A~})~%~%~:[(not documented)~;~:*~A~]~%"
                 (string-downcase symbol)
                 (mapcar #'string-upcase 
                         (lispworks:function-lambda-list symbol))
                 (documentation symbol 'function))
         (describe (fdefinition symbol)))
        (t (format t "~S is not fbound" symbol))))

(defun describe-symbol (sym)
  (format t "~A is a symbol in package ~A." sym (symbol-package sym))
  (when (boundp sym)
    (format t "~%~%Value: ~A" (symbol-value sym)))
  (let ((doc (documentation sym 'variable)))
    (when doc 
      (format t "~%~%Variable documentation:~%~A"  doc)))
  (when (fboundp sym)
    (describe-function sym)))

;;; Debugging

(defvar *sldb-top-frame*)

(defun interesting-frame-p (frame)
  (cond ((or (dbg::call-frame-p frame)
             (dbg::derived-call-frame-p frame)
             (dbg::foreign-frame-p frame)
             (dbg::interpreted-call-frame-p frame))
         t)
        ((dbg::catch-frame-p frame) dbg:*print-catch-frames*)
        ((dbg::binding-frame-p frame) dbg:*print-binding-frames*)
        ((dbg::handler-frame-p frame) dbg:*print-handler-frames*)
        ((dbg::restart-frame-p frame) dbg:*print-restart-frames*)
        ((dbg::open-frame-p frame) dbg:*print-open-frames*)
        (t nil)))

(defun nth-next-frame (frame n)
  "Unwind FRAME N times."
  (do ((frame frame (dbg::frame-next frame))
       (i n (if (interesting-frame-p frame) (1- i) i)))
      ((and (interesting-frame-p frame) (zerop i)) frame)
    (assert frame)))

(defun nth-frame (index)
  (nth-next-frame *sldb-top-frame* index))
           
(defun find-top-frame ()
  "Return the most suitable top-frame for the debugger."
  (do ((frame (dbg::debugger-stack-current-frame dbg::*debugger-stack*)
              (nth-next-frame frame 1)))
      ((and (dbg::call-frame-p frame)
            (eq (dbg::call-frame-function-name frame) 
                'invoke-debugger))
       (nth-next-frame frame 1))))
  
(defimplementation call-with-debugging-environment (fn)
  (dbg::with-debugger-stack ()
    (let ((*sldb-top-frame* (find-top-frame)))
      (funcall fn))))

(defimplementation compute-backtrace (start end)
  (let ((end (or end most-positive-fixnum))
	(backtrace '()))
    (do ((frame (nth-frame start) (dbg::frame-next frame))
	 (i start))
	((or (not frame) (= i end)) (nreverse backtrace))
      (when (interesting-frame-p frame)
	(incf i)
	(push frame backtrace)))))

(defun frame-actual-args (frame)
  (mapcar (lambda (arg)
            (handler-case (dbg::dbg-eval arg frame)
              (error (format nil "<~A>" arg))))
          (dbg::call-frame-arglist frame)))

(defimplementation print-frame (frame stream)
  (cond ((dbg::call-frame-p frame)
         (format stream "~S ~S"
                 (dbg::call-frame-function-name frame)
                 (frame-actual-args frame)))
        (t (princ frame stream))))

(defimplementation frame-locals (n)
  (let ((frame (nth-frame n)))
    (if (dbg::call-frame-p frame)
	(destructuring-bind (vars with)
	    (dbg::frame-locals-format-list frame #'list 75 0)
	  (declare (ignore with))
          (mapcar (lambda (var)
                    (destructuring-bind (name value symbol location) var
                      (declare (ignore name location))
                      (list :name symbol :id 0
                            :value value)))
                  vars)))))

(defimplementation frame-catch-tags (index)
  (declare (ignore index))
  nil)

(defimplementation frame-source-location-for-emacs (frame)
  (let ((frame (nth-frame frame)))
    (if (dbg::call-frame-p frame)
	(let ((name (dbg::call-frame-function-name frame)))
	  (if name
              (function-name-location name))))))

(defimplementation eval-in-frame (form frame-number)
  (let ((frame (nth-frame frame-number)))
    (dbg::dbg-eval form frame)))

(defimplementation return-from-frame (frame-number form)
  (let* ((frame (nth-frame frame-number))
         (return-frame (dbg::find-frame-for-return frame)))
    (dbg::dbg-return-from-call-frame frame form return-frame 
                                     dbg::*debugger-stack*)))

(defimplementation restart-frame (frame-number)
  (let ((frame (nth-frame frame-number)))
    (dbg::restart-frame frame :same-args t)))

;;; Definition finding

(defun function-name-location (name)
  (let ((defs (find-definitions name)))
    (cond (defs (cadr (first defs)))
          (t (list :error (format nil "Source location not available for: ~S" 
                                  name))))))

(defimplementation find-definitions (name)
  (let ((locations (dspec:find-name-locations dspec:*dspec-classes* name)))
    (loop for (dspec location) in locations
          collect (list dspec (make-dspec-location dspec location)))))

;;; Compilation 

(defmacro with-swank-compilation-unit ((location &rest options) &body body)
  (lw:rebinding (location)
    `(let ((compiler::*error-database* '()))
       (with-compilation-unit ,options
         ,@body
         (signal-error-data-base compiler::*error-database* ,location)
         (signal-undefined-functions compiler::*unknown-functions* ,location)))))

(defimplementation swank-compile-file (filename load-p)
  (with-swank-compilation-unit (filename)
    (compile-file filename :load load-p)))

(defun map-error-database (database fn)
  (loop for (filename . defs) in database do
	(loop for (dspec . conditions) in defs do
	      (dolist (c conditions) 
		(funcall fn filename dspec c)))))

(defun lispworks-severity (condition)
  (cond ((not condition) :warning)
	(t (etypecase condition
	     (error :error)
	     (style-warning :warning)
	     (warning :warning)))))

(defun signal-compiler-condition (message location condition)
  (check-type message string)
  (signal 
   (make-instance 'compiler-condition :message message 
		  :severity (lispworks-severity condition) 
		  :location location
		  :original-condition condition)))

(defun compile-from-temp-file (string filename)
  (unwind-protect
       (progn
	 (with-open-file (s filename :direction :output :if-exists :supersede)
	   (write-string string s)
	   (finish-output s))
	 (let ((binary-filename (compile-file filename :load t)))
           (when binary-filename
             (delete-file binary-filename))))
    (delete-file filename)))

(defun dspec-buffer-position (dspec offset)
  (etypecase dspec
    (cons (let ((name (dspec:dspec-primary-name dspec)))
            (typecase name
              ((or symbol string) 
               (list :function-name (string name)))
              (t (list :position offset)))))
    (null (list :position offset))
    (symbol (list :function-name (string dspec)))))

(defmacro with-fairly-standard-io-syntax (&body body)
  "Like WITH-STANDARD-IO-SYNTAX but preserve *PACKAGE* and *READTABLE*."
  (let ((package (gensym))
        (readtable (gensym)))
    `(let ((,package *package*)
           (,readtable *readtable*))
      (with-standard-io-syntax
        (let ((*package* ,package)
              (*readtable* ,readtable))
          ,@body)))))

#-(or lispworks-4.1 lispworks-4.2)      ; no dspec:parse-form-dspec prior to 4.3
(defun dspec-stream-position (stream dspec)
  (with-fairly-standard-io-syntax
    (loop (let* ((pos (file-position stream))
                 (form (read stream nil '#1=#:eof)))
            (when (eq form '#1#)
              (return nil))
            (labels ((check-dspec (form)
                       (when (consp form)
                         (let ((operator (car form)))
                           (case operator
                             ((progn)
                              (mapcar #'check-dspec
                                      (cdr form)))
                             ((eval-when locally macrolet symbol-macrolet)
                              (mapcar #'check-dspec
                                      (cddr form)))
                             ((in-package)
                              (let ((package (find-package (second form))))
                                (when package
                                  (setq *package* package))))
                             (otherwise
                              (let ((form-dspec (dspec:parse-form-dspec form)))
                                (when (dspec:dspec-equal dspec form-dspec)
                                  (return pos)))))))))
              (check-dspec form))))))

(defun emacs-buffer-location-p (location)
  (and (consp location)
       (eq (car location) :emacs-buffer)))

(defun make-dspec-location (dspec location)
  (flet ((filename (pathname)
           (multiple-value-bind (truename condition)
               (ignore-errors (truename pathname))
             (cond (condition 
                    (return-from make-dspec-location
                      (list :error (format nil "~A" condition))))
                   (t (namestring truename)))))
         (function-name (dspec)
           (etypecase dspec
             (symbol (symbol-name dspec))
             (cons (string (dspec:dspec-primary-name dspec))))))
    (etypecase location
      ((or pathname string)
       (let ((checked-filename (filename location)))
         (make-location `(:file ,checked-filename)
                        #+(or lispworks-4.1 lispworks-4.2)
                        (dspec-buffer-position dspec 1)
                        #-(or lispworks-4.1 lispworks-4.2)
                        (with-open-file (stream checked-filename)
                          (let ((position (dspec-stream-position stream dspec)))
                            (if position
                                (list :position (1+ position) t)
                              (dspec-buffer-position dspec 1)))))))
      (symbol `(:error ,(format nil "Cannot resolve location: ~S" location)))
      ((satisfies emacs-buffer-location-p)
       (destructuring-bind (_ buffer offset string) location
         (declare (ignore _ string))
         (make-location `(:buffer ,buffer)
                        (dspec-buffer-position dspec offset)))))))

(defun make-dspec-progenitor-location (dspec location)
  (let ((canon-dspec (dspec:canonicalize-dspec dspec)))
    (make-dspec-location
     (if canon-dspec
         (if (dspec:local-dspec-p canon-dspec)
             (dspec:dspec-progenitor canon-dspec)
           canon-dspec)
       nil)
     location)))

(defun signal-error-data-base (database location)
  (map-error-database 
   database
   (lambda (filename dspec condition)
     (declare (ignore filename))
     (signal-compiler-condition
      (format nil "~A" condition)
      (make-dspec-progenitor-location dspec location)
      condition))))

(defun signal-undefined-functions (htab filename)
  (maphash (lambda (unfun dspecs)
	     (dolist (dspec dspecs)
	       (signal-compiler-condition 
		(format nil "Undefined function ~A" unfun)
		(make-dspec-progenitor-location dspec filename)
		nil)))
	   htab))

(defimplementation swank-compile-string (string &key buffer position)
  (assert buffer)
  (assert position)
  (let* ((location (list :emacs-buffer buffer position string))
         (tmpname (hcl:make-temp-file nil "lisp")))
    (with-swank-compilation-unit (location)
      (compile-from-temp-file 
       (format nil "~S~%~A" `(eval-when (:compile-toplevel)
                              (setq dspec::*location* (list ,@location)))
               string)
       tmpname))))

;;; xref

(defmacro defxref (name function)
  `(defimplementation ,name (name)
    (xref-results (,function name))))

(defxref who-calls      hcl:who-calls)
(defxref who-macroexpands hcl:who-calls) ; macros are in the calls table too
(defxref list-callees   hcl:calls-who)
(defxref list-callers   list-callers-internal)

(defun list-callers-internal (name)
  (let ((callers (make-array 100
                             :fill-pointer 0
                             :adjustable t)))
    (hcl:sweep-all-objects
     #'(lambda (object)
         (when (and #+Harlequin-PC-Lisp (low:compiled-code-p object)
                    #-Harlequin-PC-Lisp (sys::callablep object)
                    (system::find-constant$funcallable name object))
           (vector-push-extend object callers))))
    ;; Delay dspec:object-dspec until after sweep-all-objects
    ;; to reduce allocation problems.
    (loop for object across callers
          collect (if (symbolp object)
		      (list 'function object)
                      (dspec:object-dspec object)))))

;; only for lispworks 4.2 and above
#-lispworks4.1
(progn
  (defxref who-references hcl:who-references)
  (defxref who-binds      hcl:who-binds)
  (defxref who-sets       hcl:who-sets))

(defimplementation who-specializes (classname)
  (let ((methods (clos:class-direct-methods (find-class classname))))
    (xref-results (mapcar #'dspec:object-dspec methods))))

(defun xref-results (dspecs)
  (loop for dspec in dspecs
        nconc (loop for (dspec location) 
                    in (dspec:dspec-definition-locations dspec)
                    collect (list dspec 
                                  (make-dspec-location dspec location)))))
;;; Inspector

(defmethod inspected-parts (o)
  (multiple-value-bind (names values _getter _setter type)
      (lw:get-inspector-values o nil)
    (declare (ignore _getter _setter))
    (values (format nil "~A~%   is a ~A" o type)
            (mapcar #'cons names values))))

;;; Miscellaneous

(defimplementation quit-lisp ()
  (lispworks:quit))

;;; Multithreading

(defimplementation startup-multiprocessing ()
  (mp:initialize-multiprocessing))

(defimplementation spawn (fn &key name)
  (let ((mp:*process-initial-bindings* 
         (remove (find-package :cl) 
                 mp:*process-initial-bindings*
                 :key (lambda (x) (symbol-package (car x))))))
    (mp:process-run-function name () fn)))

(defimplementation thread-name (thread)
  (mp:process-name thread))

(defimplementation thread-status (thread)
  (format nil "~A ~D" 
          (mp:process-whostate thread)
          (mp:process-priority thread)))

(defimplementation make-lock (&key name)
  (mp:make-lock :name name))

(defimplementation call-with-lock-held (lock function)
  (mp:with-lock (lock) (funcall function)))

(defimplementation current-thread ()
  mp:*current-process*)

(defimplementation all-threads ()
  (mp:list-all-processes))

(defimplementation interrupt-thread (thread fn)
  (mp:process-interrupt thread fn))

(defimplementation kill-thread (thread)
  (mp:process-kill thread))

(defimplementation thread-alive-p (thread)
  (mp:process-alive-p thread))

(defvar *mailbox-lock* (mp:make-lock))

(defun mailbox (thread)
  (mp:with-lock (*mailbox-lock*)
    (or (getf (mp:process-plist thread) 'mailbox)
        (setf (getf (mp:process-plist thread) 'mailbox)
              (mp:make-mailbox)))))

(defimplementation receive ()
  (mp:mailbox-read (mailbox mp:*current-process*)))

(defimplementation send (thread object)
  (mp:mailbox-send (mailbox thread) object))

