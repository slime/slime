
(declaim (optimize debug))

(in-package :swank)

;;; Setup and hooks.

(defun set-fd-non-blocking (fd)
  (flet ((fcntl (fd cmd arg)
	   (multiple-value-bind (flags errno) (unix:unix-fcntl fd cmd arg)
	     (or flags 
		 (error "fcntl: ~A" (unix:get-unix-error-msg errno))))))
    (let ((flags (fcntl fd unix:F-GETFL 0)))
      (fcntl fd unix:F-SETFL (logior flags unix:O_NONBLOCK)))))

(set-fd-non-blocking (sys:fd-stream-fd sys:*stdin*))
(setf c:*record-xref-info* t)

;;; TCP Server.

(defstruct (slime-output-stream
	    (:include lisp::string-output-stream
		      (lisp::misc #'slime-out-misc)))
  (last-charpos 0 :type kernel:index))

(defun slime-out-misc (stream operation &optional arg1 arg2)
  (case operation
    (:force-output
     (unless (zerop (lisp::string-output-stream-index stream))
       (setf (slime-output-stream-last-charpos stream)
	     (slime-out-misc stream :charpos))
       (send-to-emacs `(:read-output ,(get-output-stream-string stream)))))
    (:file-position nil)
    (:charpos 
     (do ((index (1- (the fixnum (lisp::string-output-stream-index stream)))
		 (1- index))
	  (count 0 (1+ count))
	  (string (lisp::string-output-stream-string stream)))
	 ((< index 0) (+ count (slime-output-stream-last-charpos stream)))
       (declare (simple-string string)
		(fixnum index count))
       (if (char= (schar string index) #\newline)
	   (return count))))
    (t (lisp::string-out-misc stream operation arg1 arg2))))

(defstruct (slime-input-stream
	     (:include sys:lisp-stream
		       (lisp::in #'slime-input-stream-read-char)
		       (lisp::misc #'slime-input-stream-misc)))
  (buffered-char nil :type (or null character)))

(defun slime-input-stream-read-char (stream &optional eoferr eofval)
  (declare (ignore eoferr eofval))
  (let ((c (slime-input-stream-buffered-char stream)))
    (cond (c (setf (slime-input-stream-buffered-char stream) nil) c)
	  (t (slime-read-char)))))

(defun slime-input-stream-misc (stream operation &optional arg1 arg2)
  (declare (ignore arg2))
  (case operation
    (:unread 
     (assert (not (slime-input-stream-buffered-char stream)))
     (setf (slime-input-stream-buffered-char stream) arg1)
     nil)
    (:listen t)))

(defun create-swank-server (port &key reuse-address (address "localhost"))
  "Create a SWANK TCP server."
  (let* ((hostent (ext:lookup-host-entry address))
         (address (car (ext:host-entry-addr-list hostent)))
         (ip (ext:htonl address)))
    (let ((fd (ext:create-inet-listener port :stream
                                        :reuse-address reuse-address
                                        :host ip)))
      (system:add-fd-handler fd :input #'accept-connection)
      (nth-value 1 (ext::get-socket-host-and-port fd)))))

(defun accept-connection (socket)
  "Accept one Swank TCP connection on SOCKET and then close it."
  (setup-request-handler (ext:accept-tcp-connection socket))
  (sys:invalidate-descriptor socket)
  (unix:unix-close socket))

(defun setup-request-handler (socket)
  "Setup request handling for SOCKET."
  (let* ((stream (sys:make-fd-stream socket
                                    :input t :output t
                                    :element-type 'base-char))
	 (input (make-slime-input-stream))
	 (output (make-slime-output-stream))
	 (io (make-two-way-stream input output)))
    (system:add-fd-handler socket
                           :input (lambda (fd)
                                    (declare (ignore fd))
                                    (serve-request stream output input io)))))

(defun serve-request (*emacs-io* *slime-output* *slime-input* *slime-io*)
  "Read and process a request from a SWANK client.
The request is read from the socket as a sexp and then evaluated."
  (catch 'slime-toplevel
    (with-simple-restart (abort "Return to Slime toplevel.")
      (handler-case (read-from-emacs)
	(slime-read-error (e)
	  (when *swank-debug-p*
	    (format *debug-io* "~&;; Connection to Emacs lost.~%;; [~A]~%" e))
	  (sys:invalidate-descriptor (sys:fd-stream-fd *emacs-io*))
	  (close *emacs-io*))))))

;;;

(defslimefun set-default-directory (directory)
  (setf (ext:default-directory) (namestring directory))
  ;; Setting *default-pathname-defaults* to an absolute directory
  ;; makes the behavior of MERGE-PATHNAMES a bit more intuitive.
  (setf *default-pathname-defaults* (pathname (ext:default-directory)))
  (namestring (ext:default-directory)))

;;;; Compilation Commands

(defvar *buffername*)
(defvar *buffer-offset*)

(defun handle-notification-condition (condition)
  "Handle a condition caused by a compiler warning.
This traps all compiler conditions at a lower-level than using
C:*COMPILER-NOTIFICATION-FUNCTION*. The advantage is that we get to
craft our own error messages, which can omit a lot of redundant
information."
  (unless (eq condition *previous-compiler-condition*)
    (let ((context (or (c::find-error-context nil) *previous-context*)))
      (setq *previous-compiler-condition* condition)
      (setq *previous-context* context)
      (let ((note (if context
		      (compiler-note-for-emacs condition context)
		      (minimal-compiler-note-for-emacs condition))))
	(push note *compiler-notes*)
	(when *compile-file-truename*
	  (push note (gethash (namestring *compile-file-truename*)
			      *notes-database*)))))))
	       
(defun compiler-note-for-emacs (condition context)
  (let* ((file-name (c::compiler-error-context-file-name context))
	 (file-position (c::compiler-error-context-file-position context))
	 (file (if (typep file-name 'pathname)
		   (unix-truename file-name)
		   file-name)))
    (list
     :position file-position
     :filename (and (stringp file) file)
     :source-path (current-compiler-error-source-path context)
     :severity (severity-for-emacs condition)
     :message (brief-compiler-message-for-emacs condition context)
     :buffername (if (boundp '*buffername*) *buffername*)
     :buffer-offset (if (boundp '*buffer-offset*) *buffer-offset*))))

(defun minimal-compiler-note-for-emacs (condition)
  "Return compiler note with only minimal context information."
  (list :position 0
	:filename (if *compile-file-truename* 
		      (namestring *compile-file-truename*))
	:source-path nil 
	:severity (severity-for-emacs condition)
	:message (princ-to-string condition)
	:buffername (if (boundp '*buffername*) *buffername*)
	:buffer-offset (if (boundp '*buffer-offset*) *buffer-offset*)))

(defun severity-for-emacs (condition)
  (etypecase condition
    (c::compiler-error :error)
    (c::style-warning :note)
    (c::warning :warning)))

(defun brief-compiler-message-for-emacs (condition error-context)
  "Briefly describe a compiler error for Emacs.
When Emacs presents the message it already has the source popped up
and the source form highlighted. This makes much of the information in
the error-context redundant."
  (declare (type c::compiler-error-context error-context))
  (let ((enclosing (c::compiler-error-context-enclosing-source error-context)))
    (if enclosing
        (format nil "--> ~{~<~%--> ~1:;~A~> ~}~%~A" enclosing condition)
        (format nil "~A" condition))))

(defun current-compiler-error-source-path (context)
  "Return the source-path for the current compiler error.
Returns NIL if this cannot be determined by examining internal
compiler state."
  (cond ((c::node-p context)
	 (reverse
	  (c::source-path-original-source (c::node-source-path context))))
	((c::compiler-error-context-p context)
	 (reverse
	  (c::compiler-error-context-original-source-path context)))))

(defun call-trapping-compilation-notes (fn)
  (handler-bind ((c::compiler-error #'handle-notification-condition)
                 (c::style-warning #'handle-notification-condition)
                 (c::warning #'handle-notification-condition))
    (funcall fn)))

(defslimefun swank-compile-file (filename load)
  (call-with-compilation-hooks
   (lambda ()
     (clear-note-database filename)
     (clear-xref-info filename)
     (let ((*buffername* nil)
	   (*buffer-offset* nil))
       (compile-file filename :load load)))))

(defslimefun swank-compile-string (string buffer start)
  (call-with-compilation-hooks
   (lambda ()
     (let ((*package* *buffer-package*)
	   (*buffername* buffer)
	   (*buffer-offset* start))
       (with-input-from-string (stream string)
	 (ext:compile-from-stream 
	  stream 
	  :source-info `(:emacs-buffer ,buffer 
			 :emacs-buffer-offset ,start)))))))

(defun clear-xref-info (namestring)
  "Clear XREF notes pertaining to FILENAME.
This is a workaround for a CMUCL bug: XREF records are cumulative."
  (let ((filename (parse-namestring namestring)))
    (when c:*record-xref-info*
      (dolist (db (list xref::*who-calls*
                        #+cmu19 xref::*who-is-called*
                        #+cmu19 xref::*who-macroexpands*
                        xref::*who-references*
                        xref::*who-binds*
                        xref::*who-sets*))
        (maphash (lambda (target contexts)
                   (setf (gethash target db)
                         (delete-if (lambda (ctx)
                                      (xref-context-derived-from-p ctx filename))
                                    contexts)))
                 db)))))

(defun xref-context-derived-from-p (context filename)
  (let ((xref-file (xref:xref-context-file context)))
    (and xref-file (pathname= filename xref-file))))
  
(defun pathname= (&rest pathnames)
  "True if PATHNAMES refer to the same file."
  (apply #'string= (mapcar #'unix-truename pathnames)))

(defun unix-truename (pathname)
  (ext:unix-namestring (truename pathname)))

(defslimefun arglist-string (fname)
  "Return a string describing the argument list for FNAME.
The result has the format \"(...)\"."
  (declare (type string fname))
  (multiple-value-bind (function condition)
      (ignore-errors (values (from-string fname)))
    (when condition
      (return-from arglist-string (format nil "(-- ~A)" condition)))
    (let ((arglist
	   (if (not (or (fboundp function)
			(functionp function)))
	       "(-- <Unknown-Function>)"
	       (let* ((fun (etypecase function
			     (symbol (or (macro-function function)
					 (symbol-function function)))
			     ;;(function function)
			     ))
		      (df (di::function-debug-function fun))
		      (arglist (kernel:%function-arglist fun)))
		 (cond ((eval:interpreted-function-p fun)
			(eval:interpreted-function-arglist fun))
		       ((pcl::generic-function-p fun)
			(pcl::gf-pretty-arglist fun))
		       (arglist arglist)
		       ;; this should work both for
		       ;; compiled-debug-function and for
		       ;; interpreted-debug-function
		       (df (di::debug-function-lambda-list df))
		       (t "(<arglist-unavailable>)"))))))
      (if (stringp arglist)
	  arglist
	  (to-string arglist)))))

(defslimefun who-calls (function-name)
  "Return the places where FUNCTION-NAME is called."
  (xref-results-for-emacs (xref:who-calls function-name)))

(defslimefun who-references (variable)
  "Return the places where the global variable VARIABLE is referenced."
  (xref-results-for-emacs (xref:who-references variable)))

(defslimefun who-binds (variable)
  "Return the places where the global variable VARIABLE is bound."
  (xref-results-for-emacs (xref:who-binds variable)))

(defslimefun who-sets (variable)
  "Return the places where the global variable VARIABLE is set."
  (xref-results-for-emacs (xref:who-sets variable)))

#+cmu19
(defslimefun who-macroexpands (macro)
  "Return the places where MACRO is expanded."
  (xref-results-for-emacs (xref:who-macroexpands macro)))

(defun xref-results-for-emacs (contexts)
  "Prepare a list of xref contexts for Emacs.
The result is a list of file-referrers:
file-referrer ::= (FILENAME ({reference}+))
reference     ::= (FUNCTION-SPECIFIER SOURCE-PATH)"
  (let ((hash (make-hash-table :test 'equal))
        (files '()))
    (dolist (context contexts)
      (let* ((file (xref:xref-context-file context))
             (unix-path (if file (unix-truename file) "<unknown>")))
        (push context (gethash unix-path hash))
        (pushnew unix-path files :test #'string=)))
    (mapcar (lambda (unix-path)
              (let ((real-path (if (string= unix-path "<unknown>")
                                   nil
                                   unix-path)))
                (file-xrefs-for-emacs real-path (gethash unix-path hash))))
            (sort files #'string<))))

(defun file-xrefs-for-emacs (unix-filename contexts)
  "Return a summary of the references from a particular file.
The result is a list of the form (FILENAME ((REFERRER SOURCE-PATH) ...))"
  (list unix-filename
        (loop for context in (sort-contexts-by-source-path contexts)
              collect (list (let ((*print-pretty* nil))
                              (to-string (xref:xref-context-name context)))
                            (xref:xref-context-source-path context)))))

(defun sort-contexts-by-source-path (contexts)
  "Sort xref contexts by lexical position of source-paths.
It is assumed that all contexts belong to the same file."
  (sort contexts #'source-path< :key #'xref:xref-context-source-path))

(defun source-path< (path1 path2)
  "Return true if PATH1 is lexically before PATH2."
  (and (every #'< path1 path2)
       (< (length path1) (length path2))))

;;; Find callers and callees by looking at the constant pool of
;;; compiled code objects.  We assume every fdefn object in the
;;; constant pool corresponds to a call to that function.  A better
;;; strategy would be to use the disassembler to find actual
;;; call-sites.

(declaim (inline map-code-constants))
(defun map-code-constants (code fn)
  "Call FN for each constant in CODE's constant pool."
  (check-type code kernel:code-component)
  (loop for i from vm:code-constants-offset below (kernel:get-header-data code)
	do (funcall fn (kernel:code-header-ref code i))))

(defun function-callees (function)
  "Return FUNCTION's callees as a list of names."
  (let ((callees '()))
    (map-code-constants 
     (vm::find-code-object function)
     (lambda (obj)
       (when (kernel:fdefn-p obj)
	 (push (kernel:fdefn-name obj) 
	       callees))))
    callees))

(declaim (inline map-allocated-code-components))
(defun map-allocated-code-components (spaces fn)
  "Call FN for each allocated code component in one of SPACES.  FN
receives the object and it's size as arguments.  SPACES should be a
list of the symbols :dynamic, :static, or :read-only."
  (dolist (space spaces)
    (vm::map-allocated-objects
     (lambda (obj header size)
       (when (= vm:code-header-type header)
	 (funcall fn obj size)))
     space)))

(declaim (inline map-caller-code-components))
(defun map-caller-code-components (function spaces fn)
  "Call FN for each code component with a fdefn for FUNCTION in its
constant pool."
  (let ((function (coerce function 'function)))
    (map-allocated-code-components
     spaces 
     (lambda (obj size)
       (declare (ignore size))
       (map-code-constants 
	obj 
	(lambda (constant)
	  (when (and (kernel:fdefn-p constant)
		     (eq (kernel:fdefn-function constant)
			 function))
	    (funcall fn obj))))))))

(defun function-callers (function &optional (spaces '(:read-only :static 
						      :dynamic)))
  "Return FUNCTION's callers as a list of names."
  (let ((referrers '()))
    (map-caller-code-components 
     function
     spaces
     (lambda (code)
       (let ((entry (kernel:%code-entry-points code)))
	 (cond ((not entry)
		(push (princ-to-string code) referrers))
	       (t 
		(loop for e = entry then (kernel::%function-next e)
		      while e
		      for name = (kernel:%function-name e)
		      do (pushnew name referrers :test #'equal)))))))
    referrers))

(defun stringify-function-name-list (list)
  (let ((*print-pretty* nil))
    (mapcar #'to-string (remove-if-not #'ext:valid-function-name-p list))))

(defslimefun list-callers (symbol-name)
  (stringify-function-name-list (function-callers (from-string symbol-name))))

(defslimefun list-callees (symbol-name)
  (stringify-function-name-list (function-callees (from-string symbol-name))))

;;;; Definitions

(defvar *debug-definition-finding* nil
  "When true don't handle errors while looking for definitions.
This is useful when debugging the definition-finding code.")

(defun function-first-code-location (function)
  (and (function-has-debug-function-p function)
       (di:debug-function-start-location
        (di:function-debug-function function))))

(defun function-has-debug-function-p (function)
  (di:function-debug-function function))

(defun function-code-object= (closure function)
  (and (eq (vm::find-code-object closure)
	   (vm::find-code-object function))
       (not (eq closure function))))

(defun struct-closure-p (function)
  (or (function-code-object= function #'kernel::structure-slot-accessor)
      (function-code-object= function #'kernel::structure-slot-setter)
      (function-code-object= function #'kernel::%defstruct)))

(defun struct-closure-dd (function)
  (assert (= (kernel:get-type function) vm:closure-header-type))
  (flet ((find-layout (function)
	   (sys:find-if-in-closure 
	    (lambda (x) 
	      (cond ((kernel::layout-p x)
		     (return-from find-layout x))
		    ((di::indirect-value-cell-p x)
		     (let ((value (c:value-cell-ref x)))
		       (when (kernel::layout-p value)
			 (return-from find-layout value))))))
	    function)))
    (kernel:layout-info (find-layout function))))
	    
(defun dd-source-location (dd)
  (let ((constructor (or (kernel:dd-default-constructor dd)
			 (car (kernel::dd-constructors dd)))))
    (cond (constructor 
	   (function-source-location 
	    (coerce (if (consp constructor) (car constructor) constructor)
		    'function)))
	  (t (error "Cannot locate struct without constructor: ~S" 
		    (kernel::dd-name dd))))))

(defun function-source-location (function)
  "Try to find the canonical source location of FUNCTION."
  ;; First test if FUNCTION is a closure created by defstruct; if so
  ;; extract the defstruct-description (dd) from the closure and find
  ;; the constructor for the struct.  Defstruct creates a defun for
  ;; the default constructor and we use that as an approximation to
  ;; the source location of the defstruct.
  ;;
  ;; For an ordinary function we return the source location of the
  ;; first code-location we find.
  (cond ((struct-closure-p function)
	 (dd-source-location (struct-closure-dd function)))
	(t
         (let ((location (function-first-code-location function)))
           (when location
             (source-location-for-emacs location))))))

(defslimefun function-source-location-for-emacs (fname)
  "Return the source-location of FNAME's definition."
  (let* ((fname (from-string fname))
         (finder
          (lambda ()
            (cond ((and (symbolp fname) (macro-function fname))
                   (function-source-location (macro-function fname)))
                  ((fboundp fname)
                   (function-source-location (coerce fname 'function)))))))
    (if *debug-definition-finding*
        (funcall finder)
        (handler-case (funcall finder)
          (error (e) (list :error (format nil "Error: ~A" e)))))))

;;;

(defun briefly-describe-symbol-for-emacs (symbol)
  "Return a plist describing SYMBOL.
Return NIL if the symbol is unbound."
  (let ((result '()))
    (labels ((first-line (string) 
               (let ((pos (position #\newline string)))
                 (if (null pos) string (subseq string 0 pos))))
	     (doc (kind)
	       (let ((string (documentation symbol kind)))
		 (if string 
		     (first-line string)
		     :not-documented)))
	     (maybe-push (property value)
	       (when value
		 (setf result (list* property value result)))))
      (maybe-push
       :variable (multiple-value-bind (kind recorded-p)
		     (ext:info variable kind symbol)
		   (declare (ignore kind))
		   (if (or (boundp symbol) recorded-p)
		       (doc 'variable))))
      (maybe-push
       :function (if (fboundp symbol) 
		     (doc 'function)))
      (maybe-push
       :setf (if (or (ext:info setf inverse symbol)
		     (ext:info setf expander symbol))
		 (doc 'setf)))
      (maybe-push
       :type (if (ext:info type kind symbol)
		 (doc 'type)))
      (maybe-push
       :class (if (find-class symbol nil) 
		  (doc 'class)))
      (if result
	  (list* :designator (to-string symbol) result)))))

(defslimefun describe-setf-function (symbol-name)
  (print-description-to-string
   (or (ext:info setf inverse (from-string symbol-name))
       (ext:info setf expander (from-string symbol-name)))))

(defslimefun describe-type (symbol-name)
  (print-description-to-string
   (kernel:values-specifier-type (from-string symbol-name))))

(defslimefun describe-class (symbol-name)
  (print-description-to-string (find-class (from-string symbol-name) nil)))

;;; Macroexpansion

(defslimefun swank-macroexpand-all (string)
  (apply-macro-expander #'walker:macroexpand-all string))


;;;

(defun tracedp (fname)
  (gethash (debug::trace-fdefinition fname)
	   debug::*traced-functions*))

(defslimefun toggle-trace-fdefinition (fname-string)
  (let ((fname (from-string fname-string)))
    (cond ((tracedp fname)
	   (debug::untrace-1 fname)
	   (format nil "~S is now untraced." fname))
	  (t
	   (debug::trace-1 fname (debug::make-trace-info))
	   (format nil "~S is now traced." fname)))))


;;; Debugging

(defvar *sldb-level* 0)
(defvar *sldb-stack-top*)
(defvar *sldb-restarts*)

(defslimefun getpid ()
  (unix:unix-getpid))

(defslimefun sldb-loop ()
  (unix:unix-sigsetmask 0)
  (ignore-errors (force-output))
  (let* ((*sldb-level* (1+ *sldb-level*))
	 (*sldb-stack-top* (or debug:*stack-top-hint* (di:top-frame)))
	 (*sldb-restarts* (compute-restarts *swank-debugger-condition*))
	 (debug:*stack-top-hint* nil)
	 (*debugger-hook* nil)
	 (level *sldb-level*)
	 (*package* *buffer-package*)
	 (*readtable* (or debug:*debug-readtable* *readtable*))
	 (*print-level* debug:*debug-print-level*)
	 (*print-length* debug:*debug-print-length*))
    (send-to-emacs (list* :debug *sldb-level* (debugger-info-for-emacs 0 1)))
    (handler-bind ((di:debug-condition 
		    (lambda (condition)
		      (send-to-emacs `(:debug-condition
				       ,(princ-to-string condition)))
		      (throw 'sldb-loop-catcher nil))))
      (unwind-protect
	   (loop
	    (catch 'sldb-loop-catcher
 	      (with-simple-restart (abort "Return to sldb level ~D." level)
		(read-from-emacs))))
	(send-to-emacs `(:debug-return ,level))))))

(defun format-restarts-for-emacs ()
  "Return a list of restarts for *swank-debugger-condition* in a
format suitable for Emacs."
  (loop for restart in *sldb-restarts*
	collect (list (princ-to-string (restart-name restart))
		      (princ-to-string restart))))

(defun format-condition-for-emacs ()
  (format nil "~A~%   [Condition of type ~S]"
	  (debug::safe-condition-message *swank-debugger-condition*)
          (type-of *swank-debugger-condition*)))

(defun nth-frame (index)
  (do ((frame *sldb-stack-top* (di:frame-down frame))
       (i index (1- i)))
      ((zerop i) frame)))

(defun nth-restart (index)
  (nth index *sldb-restarts*))

(defun format-frame-for-emacs (frame)
  (list (di:frame-number frame)
	(with-output-to-string (*standard-output*) 
          (let ((*print-pretty* nil))
            (debug::print-frame-call frame :verbosity 1 :number t)))))

(defun backtrace-length ()
  "Return the number of frames on the stack."
  (do ((frame *sldb-stack-top* (di:frame-down frame))
       (i 0 (1+ i)))
      ((not frame) i)))

(defun compute-backtrace (start end)
  "Return a list of frames starting with frame number START and
continuing to frame number END or, if END is nil, the last frame on the
stack."
  (let ((end (or end most-positive-fixnum)))
    (loop for f = (nth-frame start) then (di:frame-down f)
	  for i from start below end
	  while f
	  collect f)))

(defslimefun backtrace-for-emacs (start end)
  (mapcar #'format-frame-for-emacs (compute-backtrace start end)))

(defslimefun debugger-info-for-emacs (start end)
  (list (format-condition-for-emacs)
	(format-restarts-for-emacs)
	(backtrace-length)
	(backtrace-for-emacs start end)))

(defun code-location-source-path (code-location)
  (let* ((location (debug::maybe-block-start-location code-location))
	 (form-num (di:code-location-form-number location)))
    (let ((translations (debug::get-top-level-form location)))
      (unless (< form-num (length translations))
	(error "Source path no longer exists."))
      (reverse (cdr (svref translations form-num))))))

(defun code-location-file-position (code-location)
  (let* ((debug-source (di:code-location-debug-source code-location))
	 (filename (di:debug-source-name debug-source))
	 (path (code-location-source-path code-location)))
    (source-path-file-position path filename)))

(defun source-path-file-position (path filename)
  (let ((*read-suppress* t))
    (with-open-file (file filename)
      (dolist (n path)
	(dotimes (i n)
	  (read file))
	(read-delimited-list #\( file))
      (file-position file))))

(defun debug-source-info-from-emacs-buffer-p (debug-source)
  (let ((info (c::debug-source-info debug-source)))
    (and info 
	 (consp info)
	 (eq :emacs-buffer (car info)))))

(defun source-location-for-emacs (code-location)
  (let* ((debug-source (di:code-location-debug-source code-location))
	 (from (di:debug-source-from debug-source))
	 (name (di:debug-source-name debug-source)))
    (list
     :from from
     :filename (if (eq from :file)
		   (ext:unix-namestring (truename name)))
     :position (if (eq from :file)
		   (code-location-file-position code-location))
     :info (and (debug-source-info-from-emacs-buffer-p debug-source)
		(c::debug-source-info debug-source))
     :path (code-location-source-path code-location)
     :source-form
     (unless (or (eq from :file)
		 (debug-source-info-from-emacs-buffer-p debug-source))
	 (with-output-to-string (*standard-output*)
	   (debug::print-code-location-source-form code-location 100 t))))))

(defun safe-source-location-for-emacs (code-location)
  (handler-case (source-location-for-emacs code-location)
    (t (c) (list :error (debug::safe-condition-message c)))))

(defslimefun frame-source-location-for-emacs (index)
  (safe-source-location-for-emacs (di:frame-code-location (nth-frame index))))

(defslimefun eval-string-in-frame (string index)
  (to-string (di:eval-in-frame (nth-frame index) (from-string string))))

(defslimefun inspect-in-frame (string index)
  (reset-inspector)
  (inspect-object (di:eval-in-frame (nth-frame index) (from-string string))))

(defslimefun frame-locals (index)
  (let* ((frame (nth-frame index))
	 (location (di:frame-code-location frame))
	 (debug-function (di:frame-debug-function frame))
	 (debug-variables (di::debug-function-debug-variables debug-function)))
    (loop for v across debug-variables
	  collect (list
		   :symbol (di:debug-variable-symbol v)
		   :id (di:debug-variable-id v)
		   :value-string
		   (if (eq (di:debug-variable-validity v location)
			   :valid)
		       (to-string (di:debug-variable-value v frame))
		       "<not-available>")))))

(defslimefun frame-catch-tags (index)
  (loop for (tag . code-location) in (di:frame-catches (nth-frame index))
	collect `(,tag . ,(safe-source-location-for-emacs code-location))))

(defslimefun invoke-nth-restart (index)
  (invoke-restart (nth-restart index)))

(defslimefun sldb-abort ()
  (invoke-restart (find 'abort *sldb-restarts* :key #'restart-name)))


;;; Inspecting

(defvar *inspectee*)
(defvar *inspectee-parts*)
(defvar *inspector-stack* '())
(defvar *inspector-history* (make-array 10 :adjustable t :fill-pointer 0))
(defvar *inspect-length* 30)

(defun reset-inspector ()
  (setq *inspectee* nil)
  (setq *inspectee-parts* nil)
  (setq *inspector-stack* nil)
  (setf (fill-pointer *inspector-history*) 0))

(defslimefun init-inspector (string)
  (reset-inspector)
  (inspect-object (eval (from-string string))))

(defun print-part-to-string (value)
  (let ((*print-pretty* nil))
    (let ((string (to-string value))
	  (pos (position value *inspector-history*)))
      (if pos 
	  (format nil "#~D=~A" pos string)
	  string))))

(defun inspect-object (object)
  (push (setq *inspectee* object) *inspector-stack*)
  (unless (find object *inspector-history*)
    (vector-push-extend object *inspector-history*))
  (multiple-value-bind (text parts) (inspected-parts object)
    (setq *inspectee-parts* parts)
      (list :text text 
	    :type (to-string (type-of object))
	    :primitive-type (describe-primitive-type object)
	    :parts (loop for (label . value) in parts
			 collect (cons label 
				       (print-part-to-string value))))))
(defconstant +lowtag-symbols+ 
  '(vm:even-fixnum-type
    vm:function-pointer-type
    vm:other-immediate-0-type
    vm:list-pointer-type
    vm:odd-fixnum-type
    vm:instance-pointer-type
    vm:other-immediate-1-type
    vm:other-pointer-type))

(defconstant +header-type-symbols+
  ;; Is there a convinient place for all those constants?
  (flet ((tail-comp (string tail)
	   (and (>= (length string) (length tail))
		(string= string tail :start1 (- (length string) 
						(length tail))))))
    (remove-if-not
     (lambda (x) (and (tail-comp (symbol-name x) "-TYPE")
		      (not (member x +lowtag-symbols+))
		      (boundp x)
		      (typep (symbol-value x) 'fixnum)))
     (append (apropos-list "-TYPE" "VM" t)
	     (apropos-list "-TYPE" "BIGNUM" t)))))

(defun describe-primitive-type (object)
  (with-output-to-string (*standard-output*)
    (let* ((lowtag (kernel:get-lowtag object))
	   (lowtag-symbol (find lowtag +lowtag-symbols+ :key #'symbol-value)))
      (format t "[lowtag: ~A" lowtag-symbol)
      (cond ((member lowtag (list vm:other-pointer-type
				  vm:function-pointer-type
				  vm:other-immediate-0-type
				  vm:other-immediate-1-type
				  ))
	     (let* ((type (kernel:get-type object))
		    (type-symbol (find type +header-type-symbols+
				       :key #'symbol-value)))
	       (format t ", type: ~A]" type-symbol)))
	    (t (format t "]"))))))

(defun nth-part (index)
  (cdr (nth index *inspectee-parts*)))

(defslimefun inspect-nth-part (index)
  (inspect-object (nth-part index)))

(defslimefun inspector-pop ()
  "Drop the inspector stack and inspect the second element.  Return
nil if there's no second element."
  (cond ((cdr *inspector-stack*)
	 (pop *inspector-stack*)
	 (inspect-object (pop *inspector-stack*)))
	(t nil)))

(defslimefun inspector-next ()
  "Inspect the next element in the *inspector-history*."
  (let ((position (position *inspectee* *inspector-history*)))
    (cond ((= (1+ position) (length *inspector-history*))
	   nil)
	  (t (inspect-object (aref *inspector-history* (1+ position)))))))

(defslimefun quit-inspector ()
  (reset-inspector)
  nil)

(defslimefun describe-inspectee ()
  "Describe the currently inspected object."
  (print-description-to-string *inspectee*))

(defgeneric inspected-parts (object)
  (:documentation
   "Return a short description and a list of (label . value) pairs."))

(defmethod inspected-parts (o)
  (cond ((di::indirect-value-cell-p o)
	 (inspected-parts-of-value-cell o))
	(t
	 (destructuring-bind (text labeledp . parts)
	     (inspect::describe-parts o)
	   (let ((parts (if labeledp 
			    (loop for (label . value) in parts
				  collect (cons (string label) value))
			    (loop for value in parts
				  for i from 0
				  collect (cons (format nil "~D" i) value)))))
	     (values text parts))))))

(defun inspected-parts-of-value-cell (o)
  (values (format nil "~A~% is a value cell." o)
	  (list (cons "Value" (c:value-cell-ref o)))))

;; borrowed from sbcl
(defmethod inspected-parts ((object cons))
  (if (consp (cdr object))
      (inspected-parts-of-nontrivial-list object)
      (inspected-parts-of-simple-cons object)))

(defun inspected-parts-of-simple-cons (object)
  (values "The object is a CONS."
	  (list (cons (string 'car) (car object))
		(cons (string 'cdr) (cdr object)))))

(defun inspected-parts-of-nontrivial-list (object)
  (let ((length 0)
	(in-list object)
	(reversed-elements nil))
    (flet ((done (description-format)
	     (return-from inspected-parts-of-nontrivial-list
	       (values (format nil description-format length)
		       (nreverse reversed-elements)))))
      (loop
       (cond ((null in-list)
	      (done "The object is a proper list of length ~S.~%"))
	     ((>= length *inspect-length*)
	      (push (cons  (string 'rest) in-list) reversed-elements)
	      (done "The object is a long list (more than ~S elements).~%"))
	     ((consp in-list)
	      (push (cons (format nil "~D" length) (pop in-list)) 
		    reversed-elements)
	      (incf length))
	     (t
	      (push (cons (string 'rest) in-list) reversed-elements)
	      (done "The object is an improper list of length ~S.~%")))))))

(defmethod inspected-parts ((o function))
  (let ((header (kernel:get-type o)))
    (cond ((= header vm:function-header-type)
	   (values 
	    (format nil "~A~% is a function." o)
	    (list (cons "Self" (kernel:%function-self o))
		  (cons "Next" (kernel:%function-next o))
		  (cons "Name" (kernel:%function-name o))
		  (cons "Arglist" (kernel:%function-arglist o))
		  (cons "Type" (kernel:%function-type o))
		  (cons "Code Object" (kernel:function-code-header o)))))
	  ((= header vm:closure-header-type)
	   (values (format nil "~A~% is a closure." o)
		   (list* 
		    (cons "Function" (kernel:%closure-function o))
		    (loop for i from 0 below (- (kernel:get-closure-length o) 
						(1- vm:closure-info-offset))
			  collect (cons (format nil "~D" i)
					(kernel:%closure-index-ref o i))))))
	  (t (call-next-method o)))))

(defmethod inspected-parts ((o kernel:code-component))
  (values (format nil "~A~% is a code data-block." o)
	  `(("First entry point" . ,(kernel:%code-entry-points o))
	    ,@(loop for i from vm:code-constants-offset 
		    below (kernel:get-header-data o)
		    collect (cons (format nil "Constant#~D" i)
				  (kernel:code-header-ref o i)))
	    ("Debug info" . ,(kernel:%code-debug-info o))
	    ("Instructions"  . ,(kernel:code-instructions o)))))

(defmethod inspected-parts ((o kernel:fdefn))
  (values (format nil "~A~% is a fdefn object." o)
	  `(("Name" . ,(kernel:fdefn-name o))
	    ("Function" . ,(kernel:fdefn-function o)))))

;;; Local Variables:
;;; eval: (font-lock-add-keywords 'lisp-mode '(("(\\(defslimefun\\)\\s +\\(\\(\\w\\|\\s_\\)+\\)"  (1 font-lock-keyword-face) (2 font-lock-function-name-face))))
;;; End:
