;;;; SWANK support for CLISP.

;;;; Copyright (C) 2003, 2004 W. Jenkner, V. Sedach

;;;; swank-clisp.lisp is free software; you can redistribute it and/or
;;;; modify it under the terms of the GNU General Public License as
;;;; published by the Free Software Foundation; either version 2, or
;;;; (at your option) any later version.

;;; This is work in progress, but it's already usable.  Many things
;;; are adapted from other swank-*.lisp, in particular from
;;; swank-allegro (I don't use allegro at all, but it's the shortest
;;; one and I found Helmut Eller's code there enlightening).

;;; This code is developed using the current CVS version of CLISP and
;;; CLISP 2.32 on Linux. Older versions may not work (2.29 and below
;;; are confirmed non-working; please upgrade).  You need an image
;;; containing the "SOCKET", "REGEXP", and "LINUX" packages.  The
;;; portable xref from the CMU AI repository and metering.lisp from
;;; CLOCC [1] are also required (alternatively, you have to manually
;;; comment out some code below). 
;;;
;;; [1] http://cvs.sourceforge.net/viewcvs.py/clocc/clocc/src/tools/metering/

(in-package "SWANK")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (use-package "SOCKET")
  (use-package "GRAY"))

(eval-when (:compile-toplevel :execute)
  (when (find-package "LINUX")
    (pushnew :linux *features*)))

#+linux
(defmacro with-blocked-signals ((&rest signals) &body body)
  (ext:with-gensyms ("SIGPROCMASK" ret mask)
    `(multiple-value-bind (,ret ,mask)
	 (linux:sigprocmask-set-n-save
	  ,linux:SIG_BLOCK
	  ,(do ((sigset (linux:sigset-empty)
			(linux:sigset-add sigset (the fixnum (pop signals)))))
	       ((null signals) sigset)))
       (linux:check-res ,ret 'linux:sigprocmask-set-n-save)
       (unwind-protect
	    (progn ,@body)
	 (linux:sigprocmask-set ,linux:SIG_SETMASK ,mask nil)))))
	
;; #+linux
;; (defmethod call-without-interrupts (fn)
;;   (with-blocked-signals (#.linux:SIGINT) (funcall fn)))
;; 
;; #-linux
(defmethod call-without-interrupts (fn)
  (funcall fn))

#+unix (defmethod getpid () (system::program-id))
#+win32 (defmethod getpid () (or (system::getenv "PID") -1))
;; the above is likely broken; we need windows NT users!

(defimplementation lisp-implementation-type-name ()
  "clisp")


;;; TCP Server

(setq *swank-in-background* nil)

(defimplementation create-socket (host port)
  (declare (ignore host))
  (socket:socket-server port))

(defimplementation local-port (socket)
  (socket:socket-server-port socket))

(defimplementation close-socket (socket)
  (socket:socket-server-close socket))

(defimplementation accept-connection (socket)
  (socket:socket-accept socket
			:buffered nil ;; XXX should be t
			:element-type 'character
			:external-format (ext:make-encoding 
					  :charset 'charset:iso-8859-1
					  :line-terminator :unix)))

(defvar *sigio-handlers* '()
  "List of (key . fn) pairs to be called on SIGIO.")

(defun sigio-handler (signal)
  (mapc (lambda (handler) (funcall (cdr handler))) *sigio-handlers*))

;(trace sigio-handler)

(defvar *saved-sigio-handler*)

#+(or)
(progn
  (defun set-sigio-handler ()
    (setf *saved-sigio-handler*
	  (linux:set-signal-handler linux:SIGIO 
				    (lambda (signal) (sigio-handler signal))))
    (let* ((action (linux:signal-action-retrieve linux:SIGIO))
	   (flags (linux:sa-flags action)))
      (setf (linux:sa-flags action) (logior flags linux:SA_NODEFER))
      (linux:signal-action-install linux:SIGIO action)))

  (defimplementation add-input-handler (socket fn)
    (set-sigio-handler)
    (let ((fd (socket:socket-stream-handle socket)))
      (format *debug-io* "Adding input handler: ~S ~%" fd)
      ;; XXX error checking
      (linux:fcntl3l fd linux:F_SETOWN (getpid))
      (linux:fcntl3l fd linux:F_SETFL linux:O_ASYNC)
      (push (cons fd fn) *sigio-handlers*)))

  (defimplementation remove-input-handlers (socket)
    (let ((fd (socket:socket-stream-handle socket)))
      (remove-sigio-handler fd)
      (setf *sigio-handlers* (delete fd *sigio-handlers* :key #'car)))
    (close socket))
  )

;;; Swank functions

(defimplementation arglist-string (fname)
  (format-arglist fname #'ext:arglist))

(defimplementation macroexpand-all (form)
  (ext:expand-form form))

(defimplementation describe-symbol-for-emacs (symbol)
  "Return a plist describing SYMBOL.
Return NIL if the symbol is unbound."
  (let ((result ()))
    (labels ((doc (kind)
	       (or (documentation symbol kind) :not-documented))
	     (maybe-push (property value)
	       (when value
		 (setf result (list* property value result)))))
      (when (fboundp symbol)
	(if (macro-function symbol)
	    (setf (getf result :macro) (doc 'function))
	  (setf (getf result :function) (doc 'function))))
      (maybe-push :variable (when (boundp symbol) (doc 'variable)))
      (maybe-push :class (when (find-class symbol nil) 
			   (doc 'type))) ;this should be fixed
      result)))

(defun fspec-pathname (symbol &optional type)
  (declare (ignore type))
  (let ((path (getf (gethash symbol sys::*documentation*) 'sys::file)))
    (if (and path
	     (member (pathname-type path)
		     custom:*compiled-file-types* :test #'string=))
	(loop
	   for suffix in custom:*source-file-types*
	   thereis (make-pathname :defaults path :type suffix))
	path)))

(defun find-multiple-definitions (fspec)
  (list `(,fspec t)))
(fspec-pathname 'disassemble)
(defun find-definition-in-file (fspec type file)
  (declare (ignore fspec type file))
  ;; FIXME
  0)

(defun fspec-source-locations (fspec)
  (let ((defs (find-multiple-definitions fspec)))
    (let ((locations '()))
      (loop for (fspec type) in defs do
	    (let ((file (fspec-pathname fspec type)))
	      (etypecase file
		(pathname
		 (let ((start (find-definition-in-file fspec type file)))
		   (push (make-location
			  (list :file (namestring (truename file)))
			  (if start
			      (list :position (1+ start))
			      (list :function-name (string fspec))))
			 locations)))
		((member :top-level)
		 (push (list :error (format nil "Defined at toplevel: ~A"
					    fspec))
		       locations))
		(null
		 (push (list :error (format nil
					    "Unkown source location for ~A"
					    fspec))
		       locations))
		)))
      locations)))

(defimplementation find-function-locations (symbol-name)
  (multiple-value-bind (symbol foundp) (find-symbol-designator symbol-name)
    (cond ((not foundp)
	   (list (list :error (format nil "Unkown symbol: ~A" symbol-name))))
	  ((macro-function symbol)
	   (fspec-source-locations symbol))
	  ((special-operator-p symbol)
	   (list (list :error (format nil "~A is a special-operator" symbol))))
	  ((fboundp symbol)
	   (fspec-source-locations symbol))
	  (t (list (list :error
			 (format nil "Symbol not fbound: ~A" symbol-name))))
	  )))

(defvar *sldb-topframe*)
(defvar *sldb-botframe*)
(defvar *sldb-source*)
(defvar *sldb-restarts*)
(defvar *sldb-debugmode* 4)

(defimplementation call-with-debugging-environment (debugger-loop-fn)
  (let* ((sys::*break-count* (1+ sys::*break-count*))
	 (sys::*driver* debugger-loop-fn)
	 (sys::*fasoutput-stream* nil)
;;;      (sys::*frame-limit1* (sys::frame-limit1 43))
	 (sys::*frame-limit1* (sys::frame-limit1 0))
;;;      (sys::*frame-limit2* (sys::frame-limit2))
	 (sys::*debug-mode* *sldb-debugmode*)
	 (*sldb-topframe* 
	  (sys::frame-down-1
	   (sys::frame-up-1 sys::*frame-limit1* sys::*debug-mode*)
	   sys::*debug-mode*))
	 (*sldb-botframe* (sys::frame-up *sldb-topframe* sys::*debug-mode*))
	 (*sldb-restarts* (compute-restarts *swank-debugger-condition*)))
    (funcall debugger-loop-fn)))

(defun format-restarts-for-emacs ()
  (loop for restart in *sldb-restarts*
	collect (list (princ-to-string (restart-name restart))
		      (princ-to-string restart))))

(defun nth-frame (index)
  (loop for frame = *sldb-topframe* then (sys::frame-up-1 frame 
							  sys::*debug-mode*)
	repeat index
	never (eq frame *sldb-botframe*)
	finally (return frame)))

(defun compute-backtrace (start end)
  (let ((end (or end most-positive-fixnum)))
    (loop for f = (nth-frame start)
	  then (sys::frame-up-1 f sys::*debug-mode*)
	  for i from start below end
	  until (eq f *sldb-botframe*)
	  collect f)))

(defimplementation backtrace (start-frame-number end-frame-number)
  (flet ((format-frame (f i)
	   (print-with-frame-label
	    i (lambda (s)
		(princ (string-left-trim 
			'(#\Newline)
			(with-output-to-string (stream)
			  (sys::describe-frame stream f)))
		       s)))))
    (loop for i from start-frame-number
	  for f in (compute-backtrace start-frame-number end-frame-number)
	  collect (list i (format-frame f i)))))

(defimplementation eval-in-frame (form frame-number)
  (sys::eval-at (nth-frame frame-number) form))

(defimplementation frame-locals (frame-number)
  (let* ((frame (nth-frame frame-number))
	 (frame-env (sys::eval-at frame '(sys::the-environment))))
    (append
     (frame-do-venv frame (svref frame-env 0))
     (frame-do-fenv frame (svref frame-env 1))
     (frame-do-benv frame (svref frame-env 2))
     (frame-do-genv frame (svref frame-env 3))
     (frame-do-denv frame (svref frame-env 4)))))

;; Interpreter-Variablen-Environment has the shape
;; NIL or #(v1 val1 ... vn valn NEXT-ENV).

(defun frame-do-venv (frame venv)
  (loop for i from 1 below (length venv) by 2
	as symbol = (svref venv (1- i))
	and value = (svref venv i)
	collect (list :name (to-string symbol) :id 0
		      :value-string (to-string
				     (if (eq sys::specdecl value)
					 ;; special variable
					 (sys::eval-at frame symbol)
					 ;; lexical variable or symbol macro
					 value)))))

(defun frame-do-fenv (frame fenv)
  (declare (ignore frame fenv))
  nil)

(defun frame-do-benv (frame benv)
  (declare (ignore frame benv))
  nil)

(defun frame-do-genv (frame genv)
  (declare (ignore frame genv))
  nil)

(defun frame-do-denv (frame denv)
  (declare (ignore frame denv))
  nil)

(defimplementation frame-catch-tags (index)
  (declare (ignore index))
  nil)

(defimplementation return-from-frame (index form)
  (sys::return-from-eval-frame (nth-frame index) (from-string form)))

(defimplementation restart-frame (index)
  (sys::redo-eval-frame (nth-frame index)))

(defimplementation frame-source-location-for-emacs (index)
  (list :error (format nil "Cannot find source for frame: ~A"
		       (nth-frame index))))

(defimplementation debugger-info-for-emacs (start end)
  (list (debugger-condition-for-emacs)
	(format-restarts-for-emacs)
	(backtrace start end)))

(defun nth-restart (index)
  (nth index *sldb-restarts*))

(defslimefun invoke-nth-restart (index)
  (invoke-restart-interactively (nth-restart index)))

(defslimefun sldb-abort ()
  (invoke-restart (find 'abort *sldb-restarts* :key #'restart-name)))

;;; Profiling

(defimplementation profile (fname)
  (eval `(mon:monitor ,fname)))		;monitor is a macro

(defimplementation profiled-functions ()
  mon:*monitored-functions*)

(defimplementation unprofile (fname)
  (eval `(mon:unmonitor ,fname)))	;unmonitor is a macro

(defimplementation unprofile-all ()
  (mon:unmonitor))

(defimplementation profile-report ()
  (mon:report-monitoring))

(defimplementation profile-reset ()
  (mon:reset-all-monitoring))

(defimplementation profile-package (package callers-p methods)
  (declare (ignore callers-p methods))
  (mon:monitor-all package))

;;; Handle compiler conditions (find out location of error etc.)

(defmacro compile-file-frobbing-notes ((&rest args) &body body)
  "Pass ARGS to COMPILE-FILE, send the compiler notes to
*STANDARD-INPUT* and frob them in BODY."
  `(let ((*error-output* (make-string-output-stream))
	 (*compile-verbose* t))
     (multiple-value-prog1
      (compile-file ,@args)
      (handler-case  
       (with-input-from-string
	(*standard-input* (get-output-stream-string *error-output*))
	,@body)
       (sys::simple-end-of-file () nil)))))

(defimplementation call-with-compilation-hooks (function)
  (handler-bind ((compiler-condition #'handle-notification-condition))
    (funcall function)))

(defun handle-notification-condition (condition)
  "Handle a condition caused by a compiler warning."
  (declare (ignore condition)))

(defvar *buffer-name* nil)
(defvar *buffer-offset*)

(defvar *compiler-note-line-regexp*
  (regexp:regexp-compile
   "^(WARNING|ERROR) .* in lines ([0-9]+)\\.\\.[0-9]+ :$"
   :extended t))

(defun split-compiler-note-line (line)
  (multiple-value-bind (all head tail)
      (regexp:regexp-exec *compiler-note-line-regexp* line)
    (declare (ignore all))
    (if head
	(list (let ((*package* (find-package :keyword)))
		(read-from-string (regexp:match-string line head)))
	      (read-from-string (regexp:match-string line tail)))
	(list nil line))))

;;; Ugly but essentially working.
;;; TODO: Do something with the summary about undefined functions etc.

(defimplementation compile-file-for-emacs (filename load-p)
  (with-compilation-hooks ()
    (multiple-value-bind (fas-file w-p f-p)
	(compile-file-frobbing-notes (filename)
	  (read-line)                   ;""
	  (read-line)                   ;"Compiling file ..."
	  (loop
	     with condition
	     for (severity message) = (split-compiler-note-line (read-line))
	     until (and (stringp message) (string= message ""))
	     if severity
	     do (when condition
		  (signal condition))
	     (setq condition
		   (make-condition 'compiler-condition
				   :severity severity
				   :message ""
				   :location `(:location (:file ,filename)
							 (:line ,message))))
	     else do (setf (message condition)
			   (format nil "~a~&~a" (message condition) message))
	     finally (when condition
		       (signal condition))))
      ;; w-p = errors + warnings, f-p = errors + warnings - style warnings,
      ;; where a result of 0 is replaced by NIL.  It follows that w-p
      ;; is non-NIL iff there was any note whatsoever and that f-p is
      ;; non-NIL iff there was anything more severe than a style
      ;; warning.  This is completely ANSI compliant.
      (declare (ignore w-p f-p))
      (if (and fas-file load-p)
	  (load fas-file)
	  fas-file))))

(defimplementation compile-string-for-emacs (string &key buffer position)
  (with-compilation-hooks ()
    (let ((*package* *buffer-package*)
	  (*buffer-name* buffer)
	  (*buffer-offset* position))
      (eval (from-string
	     (format nil "(funcall (compile nil '(lambda () ~A)))"
		     string))))))

;;; Portable XREF from the CMU AI repository.

(setq xref::*handle-package-forms* '(cl:in-package))

(defun lookup-xrefs (finder name)
  (xref-results-for-emacs (funcall finder (from-string name))))

(defimplementation who-calls (function-name)
  (lookup-xrefs #'xref:list-callers function-name))

(defimplementation who-references (variable)
  (lookup-xrefs #'xref:list-readers variable))

(defimplementation who-binds (variable)
  (lookup-xrefs #'xref:list-setters variable))

(defimplementation who-sets (variable)
  (lookup-xrefs #'xref:list-setters variable))

(defimplementation list-callers (symbol-name)
  (lookup-xrefs #'xref:who-calls symbol-name))

(defimplementation list-callees (symbol-name)
  (lookup-xrefs #'xref:list-callees symbol-name))

(defun xref-results-for-emacs (fspecs)
  (let ((xrefs '()))
    (dolist (fspec fspecs)
      (dolist (location (fspec-source-locations fspec))
	(push (cons (to-string fspec) location) xrefs)))
    (group-xrefs xrefs)))

(when (find-package :swank-loader)
  (setf (symbol-function (intern "USER-INIT-FILE" :swank-loader))
	(lambda ()
	  (let ((home (user-homedir-pathname)))
	    (and (ext:probe-directory home)
		 (probe-file (format nil "~A/.swank.lisp"
				     (namestring (truename home)))))))))

;; Don't set *debugger-hook* to nil on break.
(ext:without-package-lock () 
 (defun break (&optional (format-string "Break") &rest args)
   (if (not sys::*use-clcs*)
       (progn
	 (terpri *error-output*)
	 (apply #'format *error-output*
		(concatenate 'string "*** - " format-string)
		args)
	 (funcall ext:*break-driver* t))
       (let ((condition
	      (make-condition 'simple-condition
			      :format-control format-string
			      :format-arguments args))
	     ;;(*debugger-hook* nil)
	     ;; Issue 91
	     )				
	 (ext:with-restarts
	     ((CONTINUE
	       :report (lambda (stream)
			 (format stream (sys::TEXT "Return from ~S loop")
				 'break))
	       ()))
	   (with-condition-restarts condition (list (find-restart 'CONTINUE))
				    (invoke-debugger condition)))))
   nil))

;;; Inspecting

(defmethod inspected-parts (o)
  (let* ((*print-array* nil) (*print-pretty* t)
	 (*print-circle* t) (*print-escape* t)
	 (*print-lines* custom:*inspect-print-lines*)
	 (*print-level* custom:*inspect-print-level*)
	 (*print-length* custom:*inspect-print-length*)
	 (sys::*inspect-all* (make-array 10 :fill-pointer 0 :adjustable t))
	 (tmp-pack (make-package (gensym "INSPECT-TMP-PACKAGE-")))
	 (*package* tmp-pack)
	 (sys::*inspect-unbound-value* (intern "#<unbound>" tmp-pack)))
    (let ((inspection (sys::inspect-backend o)))
      (values (format nil "~S~% ~A~{~%~A~}" o 
		      (sys::insp-title inspection)
		      (sys::insp-blurb inspection))
	      (let ((count (sys::insp-num-slots inspection))
		    (pairs '()))
		(dotimes (i count)
		  (multiple-value-bind (value name)
		      (funcall (sys::insp-nth-slot inspection) i)
		    (push (cons (to-string (or name i)) value)
			  pairs)))
		(nreverse pairs))))))

;;; Local Variables:
;;; eval: (put 'compile-file-frobbing-notes 'lisp-indent-function 1)
;;; End:
