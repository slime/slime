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
;;; containing the "SOCKET", "REGEXP", and (optionally) "LINUX"
;;; packages.

(in-package "SWANK")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (use-package "SOCKET")
  (use-package "GRAY"))

(setq *use-dedicated-output-stream* nil)
;(setq *redirect-output* nil)

#+linux
(defmacro without-interrupts (&body body)
  `(let ((sigact (linux:signal-action-retrieve linux:SIGINT)))
     (unwind-protect
	  (progn
	    (linux:set-sigprocmask linux:SIG_BLOCK (linux:sa-mask sigact))
	    ,@body)
       (linux:set-sigprocmask linux:SIG_UNBLOCK (linux:sa-mask sigact)))))

#-linux
(defmacro without-interrupts (body)
  body)

(defun without-interrupts* (fun)
  (without-interrupts (funcall fun)))

#+linux (defslimefun getpid () (linux::getpid))
#+unix (defslimefun getpid () (system::program-id))
#+win32 (defslimefun getpid () (or (system::getenv "PID") -1))
;; the above is likely broken; we need windows NT users!


;;; Gray streams

;; From swank-gray.lisp.

(defclass slime-input-stream (fundamental-character-input-stream)
  ((buffer :initform "") (index :initform 0)))

;; We have to define an additional method for the sake of the C
;; function listen_char (see src/stream.d), on which SYS::READ-FORM
;; depends.

;; We could make do with either of the two methods below.

(defmethod stream-read-char-no-hang ((s slime-input-stream))
  (with-slots (buffer index) s
    (when (< index (length buffer))
      (prog1 (aref buffer index) (incf index)))))

;; This CLISP extension is what listen_char actually calls.  The
;; default method would call STREAM-READ-CHAR-NO-HANG, so it is a bit
;; more efficient to define it directly.

(defmethod stream-read-char-will-hang-p ((s slime-input-stream))
  (with-slots (buffer index) s
    (= index (length buffer))))


;;; TCP Server

(defmethod accept-socket/stream (&key (port 0) announce-fn)
  (get-socket-stream port announce-fn))

(defmethod accept-socket/run (&key (port 0) announce-fn init-fn)
  (let* ((slime-stream (get-socket-stream port announce-fn))
	 (handler-fn (funcall init-fn slime-stream)))
    (loop while t do (funcall handler-fn))))

(defun get-socket-stream (port announce)
  (let ((socket (socket:socket-server port)))
    (unwind-protect
	(progn
	  (funcall announce (socket:socket-server-port socket))
	  (socket:socket-wait socket 0)
	  (socket:socket-accept socket
				:buffered nil
				:element-type 'character
				:external-format (ext:make-encoding 
						  :charset 'charset:iso-8859-1
						  :line-terminator :unix)))
      (socket:socket-server-close socket))))

(defmethod make-fn-streams (input-fn output-fn)
  (let* ((output (make-instance 'slime-output-stream
                                :output-fn output-fn))
         (input  (make-instance 'slime-input-stream
                                :input-fn input-fn
                                :output-stream output)))
    (values input output)))

;;; Swank functions

(defmethod arglist-string (fname)
  (declare (type string fname))
  (multiple-value-bind (function condition)
      (ignore-errors (values (from-string fname)))
    (when condition
      (return-from arglist-string (format nil "(-- ~A)" condition)))
    (multiple-value-bind (arglist condition)
	(ignore-errors (values (ext:arglist function)))
      (cond (condition (format  nil "(-- ~A)" condition))
	    (t (format nil "(~{~A~^ ~})" arglist))))))

(defmethod macroexpand-all (form)
  (ext:expand-form form))

(defmethod describe-symbol-for-emacs (symbol)
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

(defmethod find-function-locations (symbol-name)
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


(defmethod call-with-debugging-environment (debugger-loop-fn)
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
	 (*debugger-hook* nil)
	 (*package* *buffer-package*)
	 (*sldb-restarts*
	  (compute-restarts *swank-debugger-condition*))
	 (*print-pretty* nil)
	 (*print-readably* nil))
;;;    (*print-level* 3)
;;;    (*print-length* 10))
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
	finally (return frame)));(setq sys::*debug-frame* frame))))

(defun compute-backtrace (start end)
  (let ((end (or end most-positive-fixnum)))
    (loop for f = (nth-frame start)
	  then (sys::frame-up-1 f sys::*debug-mode*)
	  for i from start below end
	  until (eq f *sldb-botframe*)
	  collect f)))

(defmethod backtrace (start-frame-number end-frame-number)
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

(defmethod eval-in-frame (form frame-number)
  (sys::eval-at (nth-frame frame-number) form))

(defmethod frame-locals (frame-number)
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

(defmethod frame-catch-tags (index)
  (declare (ignore index))
  nil)

(defmethod frame-source-location-for-emacs (index)
  (list :error (format nil "Cannot find source for frame: ~A"
		       (nth-frame index))))

(defmethod debugger-info-for-emacs (start end)
  (list (debugger-condition-for-emacs)
	(format-restarts-for-emacs)
	(backtrace start end)))

(defun nth-restart (index)
  (nth index *sldb-restarts*))

(defslimefun invoke-nth-restart (index)
  (invoke-restart-interactively (nth-restart index)))

(defslimefun sldb-abort ()
  (invoke-restart (find 'abort *sldb-restarts* :key #'restart-name)))

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

(defmethod call-with-compilation-hooks (function)
  (handler-bind ((compiler-condition #'handle-notification-condition))
    (funcall function)))

(defun handle-notification-condition (condition)
  "Handle a condition caused by a compiler warning."
  (signal condition))

(defvar *buffer-name* nil)
(defvar *buffer-offset*)

(defvar *compiler-note-line-regexp*
  (regexp:regexp-compile
   "^\\(WARNING\\|ERROR\\) .* in lines \\([0-9]\\+\\)..[0-9]\\+ :$"))

(defun split-compiler-note-line (line)
  (multiple-value-bind (all head tail)
      (regexp:regexp-exec *compiler-note-line-regexp* line)
    (declare (ignore all))
    (if head
	(values (let ((*package* (find-package :keyword)))
		  (read-from-string (regexp:match-string line head)))
		(read-from-string (regexp:match-string line tail)))
	(values nil line))))

;;; Ugly but essentially working.
;;; FIXME:  I get all notes twice.

(defmethod compile-file-for-emacs (filename load-p)
  (with-compilation-hooks ()
    (multiple-value-bind (fasl-file w-p f-p)
	(compile-file-frobbing-notes (filename)
	  (read-line)                   ;""
	  (read-line)                   ;"Compiling file ..."
	  (do ((condition)
	       (severity)
	       (comp-message))
	      ((and (stringp comp-message) (string= comp-message "")) t)
	    (multiple-value-setq (severity comp-message)
	      (split-compiler-note-line (read-line)))
	    (when severity
	      (setq condition
		    (make-condition 'compiler-condition
				    :severity severity
				    :message ""
				    :location `(:location (:file ,filename)
							  (:line ,comp-message))))
	      (setf (message condition)
		    (format nil "~a~&~a" (message condition) comp-message))
	      (signal condition))))
      (declare (ignore w-p))
      (if (and (not (not f-p)) fasl-file load-p)
;;;!!! CLISP provides a fixnum for failure-p and warning-p for compile-file
	  (load fasl-file)
	fasl-file))))

(defmethod compile-string-for-emacs (string &key buffer position)
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

(defslimefun who-calls (function-name)
  (lookup-xrefs #'xref:list-callers function-name))

(defslimefun who-references (variable)
  (lookup-xrefs #'xref:list-readers variable))

(defslimefun who-binds (variable)
  (lookup-xrefs #'xref:list-setters variable))

(defslimefun who-sets (variable)
  (lookup-xrefs #'xref:list-setters variable))

(defslimefun list-callers (symbol-name)
  (lookup-xrefs #'xref:who-calls symbol-name))

(defslimefun list-callees (symbol-name)
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

;;; Local Variables:
;;; eval: (put 'compile-file-frobbing-notes 'lisp-indent-function 1)
;;; End:
