;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; swank-lispworks.lisp --- LispWorks specific code for SLIME. 
;;;
;;; Created 2003, Helmut Eller
;;;
;;; This code has been placed in the Public Domain.  All warranties
;;; are disclaimed.
;;;
;;;   $Id$
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

(defun without-interrupts* (body)
  (lispworks:without-interrupts (funcall body)))

(defconstant +sigint+ 2)

;;; TCP server

(defun socket-fd (socket)
  (etypecase socket
    (fixnum socket)
    (comm:socket-stream (comm:socket-stream-socket socket))))

(defmethod create-socket (port)
  (multiple-value-bind (socket where errno)
      (comm::create-tcp-socket-for-service port :address "localhost")
    (cond (socket socket)
          (t (error 'network-error 
              :format-control "~A failed: ~A (~D)"
              :format-arguments (list where 
                                      (list #+unix (lw:get-unix-error errno))
                                      errno))))))

(defmethod local-port (socket)
  (nth-value 1 (comm:get-socket-address (socket-fd socket))))

(defmethod close-socket (socket)
  (comm::close-socket (socket-fd socket)))

(defmethod accept-connection (socket)
  (let ((fd (comm::get-fd-from-socket socket)))
    (assert (/= fd -1))
    (make-instance 'comm:socket-stream :socket fd :direction :io 
                   :element-type 'base-char)))

(defmethod spawn (fn &key name)
  (mp:process-run-function name () fn))

(defmethod emacs-connected ()
  ;; Set SIGINT handler on Swank request handler thread.
  (sys:set-signal-handler +sigint+ #'sigint-handler))

(defun sigint-handler (&rest args)
  (declare (ignore args))
  (invoke-debugger "SIGINT"))

;;;

(defslimefun getpid ()
  "Return the process ID of this superior Lisp."
  (system::getpid))

(defmethod arglist-string (fname)
  "Return the lambda list for function FNAME as a string."
  (let ((*print-case* :downcase))
    (multiple-value-bind (function condition)
        (ignore-errors (values 
                        (find-symbol-designator fname *buffer-package*)))
      (when condition
        (return-from arglist-string (format nil "(-- ~A)" condition)))
      (let ((arglist (and (fboundp function)
			  (lispworks:function-lambda-list function))))
        (if arglist
            (princ-to-string arglist)
            "(-- <Unknown-Function>)")))))

(defmethod macroexpand-all (form)
  (walker:walk-form form))

(defmethod describe-symbol-for-emacs (symbol)
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

(defslimefun describe-function (symbol-name)
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
(defmethod describe-object ((sym symbol) *standard-output*)
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

(defslimefun sldb-abort ()
  (invoke-restart (find 'abort *sldb-restarts* :key #'restart-name)))

(defmethod call-with-debugging-environment (fn)
  (dbg::with-debugger-stack ()
    (let ((*sldb-restarts* (compute-restarts *swank-debugger-condition*)))
      (funcall fn))))

(defun format-restarts-for-emacs ()
  (loop for restart in *sldb-restarts*
        collect (list (princ-to-string (restart-name restart))
                      (princ-to-string restart))))

(defun interesting-frame-p (frame)
  (or (dbg::call-frame-p frame)
      (dbg::catch-frame-p frame)))

(defun nth-frame (index)
  (do ((frame (dbg::debugger-stack-current-frame dbg::*debugger-stack*)
	      (dbg::frame-next frame))
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

(defmethod backtrace (start end)
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

(defmethod debugger-info-for-emacs (start end)
  (list (debugger-condition-for-emacs)
        (format-restarts-for-emacs)
        (backtrace start end)))

(defun nth-restart (index)
  (nth index *sldb-restarts*))

(defslimefun invoke-nth-restart (index)
  (invoke-restart-interactively (nth-restart index)))

(defmethod frame-locals (n)
  (let ((frame (nth-frame n)))
    (if (dbg::call-frame-p frame)
	(destructuring-bind (vars with)
	    (dbg::frame-locals-format-list frame #'list 75 0)
	  (declare (ignore with))
	  (loop for (name value symbol location) in vars
		collect (list :name (to-string symbol) :id 0
			      :value-string (princ-to-string value)))))))

(defmethod frame-catch-tags (index)
  (declare (ignore index))
  nil)

(defmethod frame-source-location-for-emacs (frame)
  (let ((frame (nth-frame frame)))
    (if (dbg::call-frame-p frame)
	(let ((func (dbg::call-frame-function-name frame)))
	  (if func 
	      (dspec-source-location func))))))

(defun dspec-source-location (dspec)
  (destructuring-bind (first) (dspec-source-locations dspec)
    first))

(defun dspec-source-locations (dspec)
  (let ((locations (dspec:find-dspec-locations dspec)))
    (cond ((not locations) 
	   (list :error (format nil "Cannot find source for ~S" dspec)))
	  (t
           (loop for (dspec location) in locations
                 collect (make-dspec-location dspec location))))))

(defmethod find-function-locations (fname)
  (dspec-source-locations (from-string fname)))

;;; callers

(defun stringify-function-name-list (list)
  (let ((*print-pretty* nil)) (mapcar #'to-string list)))

(defslimefun list-callers (symbol-name)
  (stringify-function-name-list (hcl:who-calls (from-string symbol-name))))

;;; Compilation

(defmethod compile-file-for-emacs (filename load-p)
  (let ((compiler::*error-database* '()))
    (with-compilation-unit ()
      (compile-file filename :load load-p)
      (signal-error-data-base compiler::*error-database*)
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

(defun make-dspec-location (dspec location &optional tmpfile buffer position)
  (flet ((from-buffer-p () 
           (and (pathnamep location) tmpfile 
                (pathname-match-p location tmpfile)))
         (filename (pathname)
           (multiple-value-bind (truename condition)
               (ignore-errors (truename pathname))
             (cond (condition 
                    (return-from make-dspec-location
                      (list :error (format nil "~A" condition))))
                   (t (namestring truename)))))
         (function-name (dspec)
           (etypecase dspec
             (symbol (symbol-name dspec))
             (cons (symbol-name (dspec:dspec-primary-name dspec))))))
    (cond ((from-buffer-p)
           (make-location `(:buffer ,buffer) `(:position ,position)))
          (t
           (etypecase location
             (pathname 
              (make-location `(:file ,(filename location))
                             `(:function-name ,(function-name dspec))))
             ((member :listener)
              `(:error ,(format nil "Function defined in listener: ~S" dspec)))
             ((member :unknown)
              `(:error ,(format nil "Function location unkown: ~S" dspec))))
           )))) 

(defun signal-error-data-base (database &optional tmpfile buffer position)
  (map-error-database 
   database
   (lambda (filename dspec condition)
     (signal-compiler-condition
      (format nil "~A" condition)
      (make-dspec-location dspec filename tmpfile buffer position)
      condition))))

(defun signal-undefined-functions (htab filename 
				   &optional tmpfile buffer position)
  (maphash (lambda (unfun dspecs)
	     (dolist (dspec dspecs)
	       (signal-compiler-condition 
		(format nil "Undefined function ~A" unfun)
		(make-dspec-location dspec filename tmpfile buffer position)
		nil)))
	   htab))

(defmethod compile-string-for-emacs (string &key buffer position)
  (assert buffer)
  (assert position)
  (let ((*package* *buffer-package*)
	(compiler::*error-database* '())
	(tmpname (hcl:make-temp-file nil "lisp")))
    (with-compilation-unit ()
      (compile-from-temp-file string tmpname)
      (format t "~A~%" compiler:*messages*)
      (signal-error-data-base
       compiler::*error-database* tmpname buffer position)
      (signal-undefined-functions compiler::*unknown-functions*
                                  tmpname tmpname buffer position))))

;;; xref

(defun lookup-xrefs (finder name)
  (xref-results-for-emacs (funcall finder (from-string name))))

(defslimefun who-calls (function-name)
  (lookup-xrefs #'hcl:who-calls function-name))

(defslimefun who-references (variable)
  (lookup-xrefs #'hcl:who-references variable))

(defslimefun who-binds (variable)
  (lookup-xrefs #'hcl:who-binds variable))

(defslimefun who-sets (variable)
  (lookup-xrefs #'hcl:who-sets variable))

(defun xref-results-for-emacs (dspecs)
  (let ((xrefs '()))
    (dolist (dspec dspecs)
      (loop for (dspec location) in (dspec:find-dspec-locations dspec)
            do (push (cons (to-string dspec)
                           (make-dspec-location dspec location))
                     xrefs)))
    (group-xrefs xrefs)))

(defslimefun list-callers (symbol-name)
  (lookup-xrefs #'hcl:who-calls symbol-name))

(defslimefun list-callees (symbol-name)
  (lookup-xrefs #'hcl:calls-who symbol-name))

;; (dspec:at-location 
;;  ('(:inside (:buffer "foo" 34)))
;;  (defun foofun () (foofun)))

;; (dspec:find-dspec-locations 'xref-results-for-emacs)
;; (who-binds '*package*)