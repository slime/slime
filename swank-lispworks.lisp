;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; swank-lispworks.lisp --- LispWorks specific code for SLIME. 
;;;
;;; Created 2003, Helmut Eller
;;;
;;; This code has been placed in the Public Domain.  All warranties
;;; are disclaimed.
;;;

(in-package :swank)

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

(setq *swank-in-background* :spawn)

(defun socket-fd (socket)
  (etypecase socket
    (fixnum socket)
    (comm:socket-stream (comm:socket-stream-socket socket))))

(defimplementation create-socket (host port)
  (multiple-value-bind (socket where errno)
      (comm::create-tcp-socket-for-service port :address host)
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

(defimplementation emacs-connected ()
  ;; Set SIGINT handler on Swank request handler thread.
  #-win32 
  (sys:set-signal-handler +sigint+ (make-sigint-handler mp:*current-process*)))

;;; Unix signals

(defun sigint-handler ()
  (with-simple-restart  (continue "Continue from SIGINT handler.")
    (invoke-debugger "SIGINT")))

(defun make-sigint-handler (process)
  (lambda (&rest args)
    (declare (ignore args))
    (mp:process-interrupt process #'sigint-handler)))

(defmethod call-without-interrupts (fn)
  (lispworks:without-interrupts (funcall fn)))

(defimplementation getpid ()
  #+win32 (win32:get-current-process-id)
  #-win32 (system::getpid))

(defimplementation lisp-implementation-type-name ()
  "lispworks")

(defimplementation arglist-string (fname)
  (format-arglist fname 
                  (lambda (symbol)
                    (let ((arglist (lw:function-lambda-list symbol)))
                      (etypecase arglist
                        ((member :dont-know)
                         (error "<arglist-unavailable>"))
                        (cons arglist))))))

(defimplementation macroexpand-all (form)
  (walker:walk-form form))

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
       :function (if (fboundp symbol)
                     (doc 'function)))
      (maybe-push
       :class (if (find-class symbol nil) 
                  (doc 'class)))
      (if result
          (list* :designator (to-string symbol) result)))))

(defimplementation describe-definition (symbol-name type)
  (case type
    ;; FIXME: This should cover all types returned by
    ;; DESCRIBE-SYMBOL-FOR-EMACS.
    (:function (describe-function symbol-name))))

(defun describe-function (symbol-name)
  (with-output-to-string (*standard-output*)
    (let ((sym (from-string symbol-name)))
      (cond ((fboundp sym)
             (format t "~%(~A~{ ~A~})~%~%~:[(not documented)~;~:*~A~]~%"
                     (string-downcase sym)
                     (mapcar #'string-upcase 
                             (lispworks:function-lambda-list sym))
                     (documentation sym 'function))
             (describe (symbol-function sym)))
            (t (format t "~S is not fbound" sym))))))

#+(or)
(defimplementation describe-object ((sym symbol) *standard-output*)
  (format t "~A is a symbol in package ~A." sym (symbol-package sym))
  (when (boundp sym)
    (format t "~%~%Value: ~A" (symbol-value sym)))
  (let ((doc (documentation sym 'variable)))
    (when doc 
      (format t "~%~%Variable documentation:~%~A"  doc)))
  (when (fboundp sym)
    (format t "~%~%(~A~{ ~A~})" 
	    (string-downcase sym)
	    (mapcar #'string-upcase 
		    (lispworks:function-lambda-list sym))))
  (let ((doc (documentation sym 'function)))
    (when doc (format t "~%~%~A~%"  doc))))

;;; Debugging

(defvar *sldb-restarts*)
(defvar *sldb-top-frame*)

(defslimefun sldb-abort ()
  (invoke-restart (find 'abort *sldb-restarts* :key #'restart-name)))

(defimplementation call-with-debugging-environment (fn)
  (dbg::with-debugger-stack ()
    (let ((*sldb-restarts* (compute-restarts *swank-debugger-condition*))
          (*sldb-top-frame* (dbg::debugger-stack-current-frame 
                             dbg::*debugger-stack*)))
      (funcall fn))))

(defun format-restarts-for-emacs ()
  (loop for restart in *sldb-restarts*
        collect (list (princ-to-string (restart-name restart))
                      (princ-to-string restart))))

(defun interesting-frame-p (frame)
  (or (dbg::call-frame-p frame)
      ;;(dbg::catch-frame-p frame)
      ))

(defun nth-frame (index)
  (do ((frame *sldb-top-frame* (dbg::frame-next frame))
       (i index (if (interesting-frame-p frame) (1- i) i)))
      ((and (interesting-frame-p frame) (zerop i)) frame)
    (assert frame)))

(defun compute-backtrace (start end)
  (let ((end (or end most-positive-fixnum))
	(backtrace '()))
    (do ((frame (nth-frame start) (dbg::frame-next frame))
	 (i start))
	((or (not frame) (= i end)) (nreverse backtrace))
      (when (interesting-frame-p frame)
	(incf i)
	(push frame backtrace)))))

(defimplementation backtrace (start end)
  (flet ((format-frame (f i)
           (print-with-frame-label
            i (lambda (s)
	       (cond ((dbg::call-frame-p f)
                      (format s "~A ~A"
                              (dbg::call-frame-function-name f)
                              (dbg::call-frame-arglist f)))
                     (t (princ f s)))))))
    (loop for i from start
	  for f in (compute-backtrace start end)
	  collect (list i (format-frame f i)))))

(defimplementation debugger-info-for-emacs (start end)
  (list (debugger-condition-for-emacs)
        (format-restarts-for-emacs)
        (backtrace start end)))

(defun nth-restart (index)
  (nth index *sldb-restarts*))

(defslimefun invoke-nth-restart (index)
  (invoke-restart-interactively (nth-restart index)))

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
	(let ((func (dbg::call-frame-function-name frame)))
	  (if func 
	      (name-source-location func))))))

(defimplementation eval-in-frame (form frame-number)
  (let ((frame (nth-frame frame-number)))
    (dbg::dbg-eval form frame)))

(defimplementation return-from-frame (frame-number form)
  (let* ((frame (nth-frame frame-number))
         (return-frame (dbg::find-frame-for-return frame))
         (form (from-string form)))
    (dbg::dbg-return-from-call-frame frame form return-frame 
                                     dbg::*debugger-stack*)))

(defimplementation restart-frame (frame-number)
  (let ((frame (nth-frame frame-number)))
    (dbg::restart-frame frame :same-args t)))

;;; Definition finding

(defun name-source-location (name)
  (first (name-source-locations name)))

(defun name-source-locations (name)
  (let ((locations (dspec:find-name-locations dspec:*dspec-classes* name)))
    (cond ((not locations) 
	   (list :error (format nil "Cannot find source for ~S" name)))
	  (t
           (loop for (dspec location) in locations
                 collect (make-dspec-location dspec location))))))

(defimplementation find-function-locations (fname)
  (name-source-locations (from-string fname)))

;;; Compilation 

(defimplementation compile-file-for-emacs (filename load-p)
  (let ((compiler::*error-database* '()))
    (with-compilation-unit ()
      (compile-file filename :load load-p)
      (signal-error-data-base compiler::*error-database* filename)
      (signal-undefined-functions compiler::*unknown-functions* filename))))

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

;; XXX handle all cases in dspec:*dspec-classes*
(defun dspec-buffer-position (dspec)
  (etypecase dspec
    (cons (ecase (car dspec)
            ((defun defmacro defgeneric defvar defstruct
                    method structure package)
             `(:function-name ,(symbol-name (cadr dspec))))
            ;; XXX this isn't quite right
            (lw:top-level-form `(:source-path ,(cdr dspec) nil))))
    (symbol `(:function-name ,(symbol-name dspec)))))

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
       (make-location `(:file ,(filename location))
                      (dspec-buffer-position dspec)))
      ((member :listener)
       `(:error ,(format nil "Function defined in listener: ~S" dspec)))
      ((member :unknown)
       `(:error ,(format nil "Function location unkown: ~S" dspec)))
      ((satisfies emacs-buffer-location-p)
       (destructuring-bind (_ buffer offset string) location
         (declare (ignore _ offset string))
         (make-location `(:buffer ,buffer)
                        (dspec-buffer-position dspec)))))))

(defun signal-error-data-base (database location)
  (map-error-database 
   database
   (lambda (filename dspec condition)
     (declare (ignore filename))
     (signal-compiler-condition
      (format nil "~A" condition)
      (make-dspec-location dspec location)
      condition))))

(defun signal-undefined-functions (htab filename)
  (maphash (lambda (unfun dspecs)
	     (dolist (dspec dspecs)
	       (signal-compiler-condition 
		(format nil "Undefined function ~A" unfun)
		(make-dspec-location dspec filename)
		nil)))
	   htab))

(defimplementation compile-string-for-emacs (string &key buffer position)
  (assert buffer)
  (assert position)
  (let* ((*package* *buffer-package*)
         (location (list :emacs-buffer buffer position string))
         (compiler::*error-database* '())
         (tmpname (hcl:make-temp-file nil "lisp")))
    (with-compilation-unit ()
      (compile-from-temp-file 
       (with-standard-io-syntax 
         (format nil "~S~%~A" `(eval-when (:compile-toplevel)
                                (setq dspec::*location* (list ,@location)))
                 string))
       tmpname)
      (signal-error-data-base compiler::*error-database* location)
      (signal-undefined-functions compiler::*unknown-functions* location))))

;;; xref

(defun lookup-xrefs (finder name)
  (xref-results-for-emacs (funcall finder (from-string name))))

(defimplementation who-calls (function-name)
  (lookup-xrefs #'hcl:who-calls function-name))

(defimplementation who-references (variable)
  (lookup-xrefs #'hcl:who-references variable))

(defimplementation who-binds (variable)
  (lookup-xrefs #'hcl:who-binds variable))

(defimplementation who-sets (variable)
  (lookup-xrefs #'hcl:who-sets variable))

(defun xref-results-for-emacs (dspecs)
  (let ((xrefs '()))
    (dolist (dspec dspecs)
      (loop for (dspec location) in (dspec:find-dspec-locations dspec)
            do (push (cons (to-string dspec)
                           (make-dspec-location dspec location))
                     xrefs)))
    (group-xrefs xrefs)))

(defimplementation list-callers (symbol-name)
  (lookup-xrefs #'hcl:who-calls symbol-name))

(defimplementation list-callees (symbol-name)
  (lookup-xrefs #'hcl:calls-who symbol-name))

;;; Inspector

(defmethod inspected-parts (o)
  (multiple-value-bind (names values _getter _setter type)
      (lw:get-inspector-values o nil)
    (declare (ignore _getter _setter))
    (values (format nil "~A~%   is a ~A" o type)
            (mapcar (lambda (name value)
                      (cons (princ-to-string name) value))
                    names values))))

;;; Multithreading

(defimplementation startup-multiprocessing ()
  (mp:initialize-multiprocessing))

(defimplementation spawn (fn &key name)
  (mp:process-run-function name () fn))

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

