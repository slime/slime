;;;; -*- indent-tabs-mode: nil; outline-regexp: ";;;;+" -*-

(declaim (optimize debug))

(in-package :swank)

;; Turn on xref. [should we?]
(setf c:*record-xref-info* t)

(defun without-interrupts* (body)
  (sys:without-interrupts (funcall body)))

(defun set-fd-non-blocking (fd)
  (flet ((fcntl (fd cmd arg)
	   (multiple-value-bind (flags errno) (unix:unix-fcntl fd cmd arg)
	     (or flags 
		 (error "fcntl: ~A" (unix:get-unix-error-msg errno))))))
    (let ((flags (fcntl fd unix:F-GETFL 0)))
      (fcntl fd unix:F-SETFL (logior flags unix:O_NONBLOCK)))))


;;;; TCP server.

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
	  (close *emacs-io*)))))
  (sys:scrub-control-stack))


;;;; Stream handling

(defstruct (slime-output-stream
	     (:include lisp::lisp-stream
		       (lisp::misc #'sos/misc)
		       (lisp::out #'sos/out)
		       (lisp::sout #'sos/sout))
	     (:conc-name sos.)
	     (:print-function %print-slime-output-stream))
  (buffer (make-string 512) :type string)
  (index 0 :type kernel:index)
  (column 0 :type kernel:index))

(defun %print-slime-output-stream (s stream d)
  (declare (ignore d))
  (print-unreadable-object (s stream :type t :identity t)))

(defun sos/out (stream char)
  (let ((buffer (sos.buffer stream))
	(index (sos.index stream)))
    (setf (schar buffer index) char)
    (setf (sos.index stream) (1+ index))
    (incf (sos.column stream))
    (when (char= #\newline char)
      ;;(force-output stream)
      (setf (sos.column stream) 0))
    (when (= index (1- (length buffer)))
      (force-output stream)))
  char)

(defun sos/sout (stream string start end)
  (loop for i from start below end 
	do (sos/out stream (aref string i))))
	      
(defun sos/misc (stream operation &optional arg1 arg2)
  (declare (ignore arg1 arg2))
  (case operation
    ((:force-output :finish-output)
     (let ((end (sos.index stream)))
       (unless (zerop end)
	 (send-to-emacs `(:read-output ,(subseq (sos.buffer stream) 0 end)))
	 (setf (sos.index stream) 0))))
    (:charpos (sos.column stream))
    (:line-length 75)
    (:file-position nil)
    (:element-type 'base-char)
    (:get-command nil)
    (:close nil)
    (t (format *terminal-io* "~&~Astream: ~S~%" stream operation))))

(defstruct (slime-input-stream
	     (:include string-stream
		       (lisp::in #'sis/in)
		       (lisp::misc #'sis/misc))
	     (:conc-name sis.)
	     (:print-function %print-slime-output-stream))
  (buffer "" :type string)
  (index 0 :type kernel:index))

(defun sis/in (stream eof-errorp eof-value)
  (declare (ignore eof-errorp eof-value))
  (let ((index (sis.index stream))
	(buffer (sis.buffer stream)))
    (when (= index (length buffer))
      (setf buffer (slime-read-string))
      (setf (sis.buffer stream) buffer)
      (setf index 0))
    (prog1 (aref buffer index)
      (setf (sis.index stream) (1+ index)))))

(defun sis/misc (stream operation &optional arg1 arg2)
  (declare (ignore arg2))
  (ecase operation
    (:file-position nil)
    (:file-length nil)
    (:unread (setf (aref (sis.buffer stream) 
			 (decf (sis.index stream)))
		   arg1))
    (:clear-input (setf (sis.index stream) 0
			(sis.buffer stream) ""))
    (:listen (< (sis.index stream) (length (sis.buffer stream))))
    (:charpos nil)
    (:line-length nil)
    (:get-command nil)
    (:element-type 'base-char)
    (:close nil)))


;;;; Compilation Commands

(defvar *swank-source-info* nil
  "Bound to a SOURCE-INFO object during compilation.")

(defclass source-info () ()
	  (:documentation "Some info about the current compilatoin unit."))

(defclass file-source-info (source-info) 
  ((filename :initarg :filename)))

(defclass buffer-source-info (source-info)
  ((buffer :initarg :buffer) 
   (start-offset :initarg :start-offset)
   (string :initarg :string)))

(defvar *previous-compiler-condition* nil
  "Used to detect duplicates.")

(defvar *previous-context* nil
  "Previous compiler error context.")

(defvar *compiler-notes* '()
  "List of compiler notes for the last compilation unit.")


;;;;; Trapping notes

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
      (signal-compiler-condition condition context))))

(defun signal-compiler-condition (condition context)
  (signal (make-condition
           'compiler-condition
           :original-condition condition
           :severity (severity-for-emacs condition)
           :message (brief-compiler-message-for-emacs condition context)
           :location (compiler-note-location context))))

(defun compiler-note-location (context)
  (cond (context
         (let ((cx context))
           (resolve-location
            *swank-source-info*
            (c::compiler-error-context-file-name cx)
            (c::compiler-error-context-file-position cx)
            (reverse (c::compiler-error-context-original-source-path cx))
            (c::compiler-error-context-original-source cx))))
        (t
         (resolve-location *swank-source-info* nil nil nil nil))))

(defun severity-for-emacs (condition)
  "Return the severity of CONDITION."
  (etypecase condition
    (c::compiler-error :error)
    (c::style-warning :note)
    (c::warning :warning)))

(defun brief-compiler-message-for-emacs (condition error-context)
  "Briefly describe a compiler error for Emacs.
When Emacs presents the message it already has the source popped up
and the source form highlighted. This makes much of the information in
the error-context redundant."
  (declare (type (or c::compiler-error-context null) error-context))
  (let ((enclosing (and error-context
			(c::compiler-error-context-enclosing-source 
			 error-context))))
    (if enclosing
        (format nil "--> ~{~<~%--> ~1:;~A~> ~}~%~A" enclosing condition)
        (format nil "~A" condition))))

(defgeneric resolve-location (source-info 
			      file-name file-position
			      source-path source))

(defmethod resolve-location (i (f pathname) position path source)
  `(:file ,(unix-truename f) ,(1+ (source-path-file-position path f))))

(defmethod resolve-location ((i buffer-source-info) (f (eql :stream))
			     position path source)
  (with-slots (buffer start-offset string) i
    `(:emacs-buffer 
      ,buffer
      ,(+ start-offset (source-path-string-position path string)))))

(defmethod resolve-location (i (f (eql :lisp)) position path source)
  '(:null))

(defmethod resolve-location (i (f (eql nil)) 
			       (pos (eql nil)) 
			       (path (eql nil))
			       (source (eql nil)))
  '(:null))

(defmacro with-compilation-hooks (() &body body)
  "Execute BODY and record the set of compiler notes."
  `(let ((*previous-compiler-condition* nil)
         (*previous-context* nil))
    (handler-bind ((c::compiler-error #'handle-notification-condition)
                   (c::style-warning  #'handle-notification-condition)
                   (c::warning        #'handle-notification-condition))
      ,@body)))

(defmethod compile-file-for-emacs (filename load-p)
  (clear-xref-info filename)
  (with-compilation-hooks ()
    (let ((*swank-source-info* (make-instance 'file-source-info
                                              :filename filename)))
      (compile-file filename :load load-p))))

(defmethod compile-string-for-emacs (string &key buffer position)
  (with-compilation-hooks ()
    (let ((*package* *buffer-package*)
          (*swank-source-info* (make-instance 'buffer-source-info
                                              :buffer buffer
                                              :start-offset position
                                              :string string)))
      (with-input-from-string (stream string)
        (ext:compile-from-stream 
         stream 
         :source-info `(:emacs-buffer ,buffer 
                        :emacs-buffer-offset ,position
                        :emacs-buffer-string ,string))))))


;;;; XREF

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
                         (delete-if 
			  (lambda (ctx)
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


;;;; Find callers and callees

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

(declaim (ext:maybe-inline map-allocated-code-components))
(defun map-allocated-code-components (spaces fn)
  "Call FN for each allocated code component in one of SPACES.  FN
receives the object as argument.  SPACES should be a list of the
symbols :dynamic, :static, or :read-only."
  (dolist (space spaces)
    (declare (inline vm::map-allocated-objects))
    (vm::map-allocated-objects
     (lambda (obj header size)
       (declare (type fixnum size) (ignore size))
       (when (= vm:code-header-type header)
	 (funcall fn obj)))
     space)))

(declaim (ext:maybe-inline map-caller-code-components))
(defun map-caller-code-components (function spaces fn)
  "Call FN for each code component with a fdefn for FUNCTION in its
constant pool."
  (let ((function (coerce function 'function)))
    (declare (inline map-allocated-code-components))
    (map-allocated-code-components
     spaces 
     (lambda (obj)
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
    (declare (inline map-caller-code-components))
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
	      (let ((value (if (di::indirect-value-cell-p x)
			       (c:value-cell-ref x) 
			       x)))
		(when (kernel::layout-p value)
		  (return-from find-layout value))))
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

(defun genericp (fn)
  (typep fn 'generic-function))

(defun gf-definition-location (gf)
  (flet ((guess-source-file (faslfile)
           (unix-truename
            (merge-pathnames (make-pathname :type "lisp")
                             faslfile))))
    (let ((def-source (pcl::definition-source gf))
          (name (string (pcl:generic-function-name gf))))
      (etypecase def-source
        (pathname `(:dspec (:file ,(guess-source-file def-source)) ,name))
        (cons
         (destructuring-bind ((dg name) pathname) def-source
           (declare (ignore dg))
           (if pathname
               `(:dspec (:file ,(guess-source-file pathname)) 
                 ,(string name)))))))))

(defun method-source-location (method)
  (function-source-location (or (pcl::method-fast-function method)
                                (pcl:method-function method))))

(defun gf-method-locations (gf)
  (let ((ms (pcl::generic-function-methods gf)))
    (mapcar #'method-source-location ms)))

(defun gf-source-locations (gf)
  (list* (gf-definition-location gf)
         (gf-method-locations gf)))

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
        ((genericp function)
         (car (gf-source-locations function)))
        (t
         (let ((location (function-first-code-location function)))
           (when location
             (source-location-for-emacs location))))))

(defmethod function-source-location-for-emacs (fname)
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


;;;; Documentation.

(defmethod describe-symbol-for-emacs (symbol)
  (let ((result '()))
    (flet ((doc (kind)
             (or (documentation symbol kind) :not-documented))
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
       :generic-function 
       (if (and (fboundp symbol)
                (typep (fdefinition symbol) 'generic-function))
           (doc 'function)))
      (maybe-push
       :function (if (and (fboundp symbol)
                          (not (typep (fdefinition symbol) 'generic-function)))
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
      (maybe-push
       :alien-type (if (not (eq (ext:info alien-type kind symbol) :unknown))
		       (doc 'alien-type)))
      (maybe-push
       :alien-struct (if (ext:info alien-type struct symbol)
			 (doc nil)))
      (maybe-push
       :alien-union (if (ext:info alien-type union symbol)
			 (doc nil)))
      (maybe-push
       :alien-enum (if (ext:info alien-type enum symbol)
		       (doc nil)))
      result)))

(defslimefun describe-setf-function (symbol-name)
  (print-description-to-string
   (or (ext:info setf inverse (from-string symbol-name))
       (ext:info setf expander (from-string symbol-name)))))

(defslimefun describe-type (symbol-name)
  (print-description-to-string
   (kernel:values-specifier-type (from-string symbol-name))))

(defslimefun describe-class (symbol-name)
  (print-description-to-string (find-class (from-string symbol-name) nil)))

(defslimefun describe-alien-type (symbol-name)
  (let ((name (from-string symbol-name)))
    (ecase (ext:info :alien-type :kind name)
      (:primitive
       (print-description-to-string
	(let ((alien::*values-type-okay* t))
	  (funcall (ext:info :alien-type :translator name) (list name)))))
      ((:defined)
       (print-description-to-string (ext:info :alien-type :definition name)))
      (:unknown
       (format nil "Unkown alien type: ~A" symbol-name)))))

(defmacro %describe-alien (symbol-name namespace)
  `(print-description-to-string
    (ext:info :alien-type ,namespace (from-string ,symbol-name))))

(defslimefun describe-alien-struct (symbol-name)
  (%describe-alien symbol-name :struct))

(defslimefun describe-alien-union (symbol-name)
  (%describe-alien symbol-name :union))

(defslimefun describe-alien-enum (symbol-name)
  (%describe-alien symbol-name :enum))

(defmethod arglist-string (fname)
  "Return a string describing the argument list for FNAME.
The result has the format \"(...)\"."
  (declare (type string fname))
  (multiple-value-bind (function condition)
      (ignore-errors (values (find-symbol-designator fname *buffer-package*)))
    (when condition
      (return-from arglist-string (format nil "(-- ~A)" condition)))
    (let ((arglist
	   (if (not (or (fboundp function)
			(functionp function)))
	       "(-- <Unknown-Function>)"
	       (let* ((fun (or (macro-function function)
                               (symbol-function function)))
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


;;;; Miscellaneous.

(defmethod macroexpand-all (form)
  (walker:macroexpand-all form))

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

(defslimefun set-default-directory (directory)
  (setf (ext:default-directory) (namestring directory))
  ;; Setting *default-pathname-defaults* to an absolute directory
  ;; makes the behavior of MERGE-PATHNAMES a bit more intuitive.
  (setf *default-pathname-defaults* (pathname (ext:default-directory)))
  (namestring (ext:default-directory)))


;;;; Source-paths

;;; CMUCL uses a data structure called "source-path" to locate
;;; subforms.  The compiler assigns a source-path to each form in a
;;; compilation unit.  Compiler notes usually contain the source-path
;;; of the error location.
;;;
;;; Compiled code objects don't contain source paths, only the
;;; "toplevel-form-number" and the (sub-) "form-number".  To get from
;;; the form-number to the source-path we need the entire toplevel-form
;;; (i.e. we have to read the source code).  CMUCL has already some
;;; utilities to do this translation, but we use some extended
;;; versions, because we need more exact position info.  Apparently
;;; Hemlock is happy with the position of the toplevel-form; we also
;;; need the position of subforms.
;;;
;;; We use a special readtable to get the positions of the subforms.
;;; The readtable stores the start and end position for each subform in
;;; hashtable for later retrieval.

(defun make-source-recorder (fn source-map)
  "Return a macro character function that does the same as FN, but
additionally stores the result together with the stream positions
before and after of calling FN in the hashtable SOURCE-MAP."
  (lambda (stream char)
    (let ((start (file-position stream))
	  (values (multiple-value-list (funcall fn stream char)))
	  (end (file-position stream)))
      #+(or) (format t "~&[~D ~{~A~^, ~} ~D]~%" start values end)
      (unless (null values) 
	(push (cons start end) (gethash (car values) source-map)))
      (values-list values))))

(defun make-source-recording-readtable (readtable source-map) 
  "Return a source position recording copy of READTABLE.
The source locations are stored in SOURCE-MAP."
  (let* ((tab (copy-readtable readtable))
	 (*readtable* tab))
    (dotimes (code char-code-limit)
      (let ((char (code-char code)))
	(multiple-value-bind (fn term) (get-macro-character char tab)
	  (when fn
	    (set-macro-character char (make-source-recorder fn source-map) 
				 term tab)))))
    tab))

(defun make-source-map ()
  (make-hash-table :test #'eq))

(defvar *source-map* (make-source-map)
  "The hashtable table used for source position recording.")

(defvar *recording-readtable-cache* '()
  "An alist of (READTABLE . RECORDING-READTABLE) pairs.")

(defun lookup-recording-readtable (readtable)
  "Find a cached or create a new recording readtable for READTABLE."
  (or (cdr (assoc readtable *recording-readtable-cache*))
      (let ((table (make-source-recording-readtable readtable *source-map*)))
	(push (cons readtable table) *recording-readtable-cache*)
	table)))
			
(defun read-and-record-source-map (stream)
  "Read the next object from STREAM.
Return the object together with a hashtable that maps
subexpressions of the object to stream positions."
  (let ((*readtable* (lookup-recording-readtable *readtable*)))
    (clrhash *source-map*)
    (values (read stream) *source-map*)))
  
(defun source-path-stream-position (path stream)
  "Search the source-path PATH in STREAM and return its position."
  (destructuring-bind (tlf-number . path) path
    (let ((*read-suppress* t))
      (dotimes (i tlf-number) (read stream))
      (multiple-value-bind (form source-map)
	  (read-and-record-source-map stream)
	(source-path-source-position (cons 0 path) form source-map)))))

(defun source-path-string-position (path string)
  (with-input-from-string (s string)
    (source-path-stream-position path s)))

(defun source-path-file-position (path filename)
  (with-open-file (file filename)
    (source-path-stream-position path file)))

(defun source-path-source-position (path form source-map)
  "Return the start position of PATH form FORM and SOURCE-MAP.  All
subforms along the path are considered and the start and end position
of deepest (i.e. smallest) possible form is returned."
  ;; compute all subforms along path
  (let ((forms (loop for n in path
		     for f = form then (nth n f)
		     collect f)))
    ;; select the first subform present in source-map
    (loop for form in (reverse forms)
	  for positions = (gethash form source-map)
	  until positions 
	  finally (destructuring-bind ((start . end)) positions
		    (return (values (1- start) end))))))

(defun code-location-stream-position (code-location stream)
  "Return the byte offset of CODE-LOCATION in STREAM.  Extract the
toplevel-form-number and form-number from CODE-LOCATION and use that
to find the position of the corresponding form."
  (let* ((location (debug::maybe-block-start-location code-location))
	 (tlf-offset (di:code-location-top-level-form-offset location))
	 (form-number (di:code-location-form-number location))
	 (*read-suppress* t))
    (dotimes (i tlf-offset) (read stream))
    (multiple-value-bind (tlf position-map) (read-and-record-source-map stream)
      (let* ((path-table (di:form-number-translations tlf 0))
	     (source-path (reverse (cdr (aref path-table form-number)))))
	(source-path-source-position source-path tlf position-map)))))
  
(defun code-location-string-offset (code-location string)
  (with-input-from-string (s string)
    (code-location-stream-position code-location s)))

(defun code-location-file-position (code-location filename)
  (with-open-file (s filename :direction :input)
    (code-location-stream-position code-location s)))

(defun make-file-location (pathname code-location)
  (list :file
	(unix-truename pathname) 
	(1+ (code-location-file-position code-location pathname))))

(defun make-buffer-location (buffer start string code-location)
  (list :emacs-buffer
	buffer 
	(+ start (code-location-string-offset code-location string))))

(defun debug-source-info-from-emacs-buffer-p (debug-source)
  (let ((info (c::debug-source-info debug-source)))
    (and info 
	 (consp info)
	 (eq :emacs-buffer (car info)))))

(defun source-location-for-emacs (code-location)
  (let* ((debug-source (di:code-location-debug-source code-location))
	 (from (di:debug-source-from debug-source))
	 (name (di:debug-source-name debug-source)))
    (ecase from
      (:file (make-file-location name code-location))
      (:stream 
       (assert (debug-source-info-from-emacs-buffer-p debug-source))
       (let ((info (c::debug-source-info debug-source)))
	 (make-buffer-location (getf info :emacs-buffer)
			       (getf info :emacs-buffer-offset)
			       (getf info :emacs-buffer-string)
			       code-location)))
      (:lisp
       `(:sexp , (with-output-to-string (*standard-output*)
		   (debug::print-code-location-source-form 
		    code-location 100 t)))))))

(defun safe-source-location-for-emacs (code-location)
  (handler-case (source-location-for-emacs code-location)
    (t (c) (list :error (debug::safe-condition-message c)))))

(defslimefun getpid ()
  (unix:unix-getpid))


;;;; Debugging

(defvar *sldb-stack-top*)
(defvar *sldb-restarts*)

(defmethod call-with-debugging-environment (debugger-loop-fn)
  (unix:unix-sigsetmask 0)
  (let* ((*sldb-stack-top* (or debug:*stack-top-hint* (di:top-frame)))
	 (*sldb-restarts* (compute-restarts *swank-debugger-condition*))
	 (debug:*stack-top-hint* nil)
	 (*debugger-hook* nil)
	 (*readtable* (or debug:*debug-readtable* *readtable*))
	 (*print-level* debug:*debug-print-level*)
	 (*print-length* debug:*debug-print-length*))
    (handler-bind ((di:debug-condition 
		    (lambda (condition)
                      (signal (make-condition
                               'sldb-condition
                               :original-condition condition)))))
      (funcall debugger-loop-fn))))

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
          (let ((*print-pretty* *sldb-pprint-frames*))
            (debug::print-frame-call frame :verbosity 1 :number t)))))

(defun compute-backtrace (start end)
  "Return a list of frames starting with frame number START and
continuing to frame number END or, if END is nil, the last frame on the
stack."
  (let ((end (or end most-positive-fixnum)))
    (loop for f = (nth-frame start) then (di:frame-down f)
	  for i from start below end
	  while f
	  collect f)))

(defmethod backtrace (start end)
  (mapcar #'format-frame-for-emacs (compute-backtrace start end)))

(defmethod debugger-info-for-emacs (start end)
  (list (format-condition-for-emacs)
	(format-restarts-for-emacs)
	(backtrace start end)))

(defmethod frame-source-location-for-emacs (index)
  (safe-source-location-for-emacs (di:frame-code-location (nth-frame index))))

(defmethod eval-in-frame (form index)
  (di:eval-in-frame (nth-frame index) form))

(defslimefun pprint-eval-string-in-frame (string index)
  (swank-pprint 
   (multiple-value-list
    (di:eval-in-frame (nth-frame index) (from-string string)))))

(defslimefun inspect-in-frame (string index)
  (reset-inspector)
  (inspect-object (di:eval-in-frame (nth-frame index) (from-string string))))

(defmethod frame-locals (index)
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

(defmethod frame-catch-tags (index)
  (loop for (tag . code-location) in (di:frame-catches (nth-frame index))
	collect `(,tag . ,(safe-source-location-for-emacs code-location))))

(defslimefun invoke-nth-restart (index)
  (invoke-restart (nth-restart index)))

(defslimefun sldb-abort ()
  (invoke-restart (find 'abort *sldb-restarts* :key #'restart-name)))


;;;; Inspecting

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
