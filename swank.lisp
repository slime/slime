(defpackage :swank
  (:use :common-lisp)
  (:export #:start-server #:evaluate #:lookup-notes #:clear-notes
           #:swank-compile-file #:swank-compile-string 
	   #:arglist-string #:completions
           #:find-fdefinition
           #:eval-string-async
           #:sldb-loop))

(in-package :swank)

(defconstant server-port 4005
  "Default port for the swank TCP server.")

(defvar *notes-database* (make-hash-table :test #'equal)
  "Database of recorded compiler notes/warnings/erros (keyed by filename).
Each value is a list of (LOCATION SEVERITY MESSAGE CONTEXT) lists.
  LOCATION is a position in the source code (integer or source path).
  SEVERITY is one of :ERROR, :WARNING, and :NOTE.
  MESSAGE is a string describing the note.
  CONTEXT is a string giving further details of where the error occured.")

(defvar *swank-debug-p* nil
  "When true extra debug printouts are enabled.")

;;; Setup and hooks.

(defun start-server (&optional (port server-port))
  (create-swank-server port :reuse-address t)
  (setf c:*record-xref-info* t)
  (ext:without-package-locks
   (setf c:*compiler-notification-function* #'handle-notification))
  (when *swank-debug-p*
    (format *debug-io* "~&Swank ready.~%")))

(defun debugger-hook (condition old-hook)
  "Hook function to be invoked instead of the debugger.
See CL:*DEBUGGER-HOOK*."
  ;; FIXME: Debug from Emacs!
  (declare (ignore old-hook))
  (handler-case
      (progn (format *error-output*
                     "~@<SWANK: unhandled condition ~2I~_~A~:>~%"
                     condition)
             (debug:backtrace 20 *error-output*)
             (finish-output *error-output*))
    (condition ()
      nil)))

(defun handle-notification (severity message context where-from position)
  "Hook function called by the compiler.
See C:*COMPILER-NOTIFICATION-FUNCTION*"
  (let ((location (or (current-compiler-error-source-path) position))
        (namestring (cond ((stringp where-from) where-from)
                          ;; we can be passed a stream from READER-ERROR
                          ((lisp::fd-stream-p where-from)
                           (lisp::fd-stream-file where-from))
                          (t where-from))))
    (when namestring
      (push (list location severity message context)
            (gethash namestring *notes-database*)))))

(defun current-compiler-error-source-path ()
  "Return the source-path for the current compiler error.
Returns NIL if this cannot be determined by examining internal
compiler state."
  (let ((context c::*compiler-error-context*))
    (cond ((c::node-p context)
           (reverse
            (c::source-path-original-source (c::node-source-path context))))
          ((c::compiler-error-context-p context)
           (reverse
            (c::compiler-error-context-original-source-path context))))))

;;; TCP Server.

(defvar *emacs-io* nil
  "Bound to a TCP stream to Emacs during request processing.")

(defun create-swank-server (port &key reuse-address)
  "Create a SWANK TCP server."
  (system:add-fd-handler
   (ext:create-inet-listener port :stream :reuse-address reuse-address)
   :input #'accept-connection))

(defun accept-connection (socket)
  "Accept a SWANK TCP connection on SOCKET."
  (setup-request-handler (ext:accept-tcp-connection socket)))

(defun setup-request-handler (socket)
  "Setup request handling for SOCKET."
  (let ((stream (sys:make-fd-stream socket
                                    :input t :output t
                                    :element-type 'unsigned-byte)))
    (system:add-fd-handler socket
                           :input (lambda (fd)
                                    (declare (ignore fd))
                                    (serve-request stream)))))

(defun serve-request (*emacs-io*)
  "Read and process a request from a SWANK client.
The request is read from the socket as a sexp and then evaluated."
  (handler-case
      (let* ((length (logior (ash (read-byte *emacs-io*) 16)
                             (ash (read-byte *emacs-io*) 8)
                             (read-byte *emacs-io*)))
             (string (make-string length)))
        (sys:read-n-bytes *emacs-io* string 0 length)
        (eval (read-from-string string)))
    (stream-error (condition)
      (when *swank-debug-p*
        (format *debug-io* "~&;; Connection to Emacs lost.~%"))
      (sys:invalidate-descriptor (sys:fd-stream-fd *emacs-io*)))))

(defun send-to-emacs (object)
  "Send OBJECT to Emacs."
  (let* ((string (prin1-to-string-for-emacs object))
         (length (length string)))
    (loop for position from 16 downto 0 by 8
          do (write-byte (ldb (byte 8 position) length) *emacs-io*))
    (write-string string *emacs-io*)
    (force-output *emacs-io*)))

(defun read-from-emacs ()
  "Read and process a request from Emacs."
  (serve-request *emacs-io*))

(defun prin1-to-string-for-emacs (object)
  (let ((*print-case* :downcase))
    (format nil "~S~%" object)))

;;; Functions for Emacs to call.

(defun swank-compile-string (string buffer start)
  (with-input-from-string (stream string)
    (multiple-value-list
     (ext:compile-from-stream stream :source-info (cons buffer start)))))

;;;; LOOKUP-NOTES -- interface

(defun canonicalize-filename (filename)
  (namestring (unix:unix-resolve-links filename)))

(defun lookup-notes (filename)
  "Return the compiler notes recorded for FILENAME.
\(See *NOTES-DATABASE* for a description of the return type.)"
  (gethash (canonicalize-filename filename) *notes-database*))

(defun clear-notes (filename)
  (remhash (canonicalize-filename filename) *notes-database*))

;;;; ARGLIST-STRING -- interface
(defun arglist-string (function)
  "Return a string describing the argument list for FUNCTION.
The result has the format \"(...)\"."
  (declare (type (or symbol function) function))
  (let ((arglist
         (if (not (or (fboundp function)
                      (functionp function)))
             "(-- <Unknown-Function>)"
             (let* ((fun (etypecase function
                           (symbol (or (macro-function function)
                                       (symbol-function function)))
                           (function function)))
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
        (prin1-to-string-for-emacs arglist))))

;;;; COMPLETIONS -- interface

(defun completions (prefix package-name &optional only-external-p)
  "Return a list of completions for a symbol's PREFIX and PACKAGE-NAME.
The result is a list of symbol-name strings. All symbols accessible in
the package are considered."
  (let ((completions nil)
        (package (find-package package-name)))
    (when package
      (do-symbols (symbol package)
        (when (and (or (not only-external-p) (symbol-external-p symbol))
                   (string-prefix-p prefix (symbol-name symbol)))
          (push (symbol-name symbol) completions))))
    completions))

(defun symbol-external-p (s)
  (multiple-value-bind (_ status)
      (find-symbol (symbol-name s) (symbol-package s))
    (declare (ignore _))
    (eq status :external)))

(defun string-prefix-p (s1 s2)
  "Return true iff the string S1 is a prefix of S2.
\(This includes the case where S1 is equal to S2.)"
  (and (<= (length s1) (length s2))
       (string= s1 s2 :end2 (length s1))))

;;;; Definitions

;;; FIND-FDEFINITION -- interface
;;;
(defun find-fdefinition (symbol-name package-name)
  "Return the name of the file in which the function was defined, or NIL."
  (fdefinition-file (read-symbol/package symbol-name package-name)))

;;; Clone of HEMLOCK-INTERNALS::FUN-DEFINED-FROM-PATHNAME
(defun fdefinition-file (function)
  "Return the name of the file in which FUNCTION was defined."
  (declare (type (or symbol function) function))
  (typecase function
    (symbol
     (let ((def (or (macro-function function)
                                  (and (fboundp function)
                                       (fdefinition function)))))
       (when def (fdefinition-file def))))
    (kernel:byte-closure
     (fdefinition-file (kernel:byte-closure-function function)))
    (kernel:byte-function
     (code-definition-file (c::byte-function-component function)))
    (function
     (code-definition-file (kernel:function-code-header
                              (kernel:%function-self function))))
    (t nil)))

(defun code-definition-file (code)
  "Return the name of the file in which CODE was defined."
  (declare (type kernel:code-component code))
  (flet ((to-namestring (pathname)
           (handler-case (namestring (truename pathname))
             (file-error () nil))))
    (let ((info (kernel:%code-debug-info code)))
      (when info
        (let ((source (car (c::debug-info-source info))))
          (when (and source (eq (c::debug-source-from source) :file))
            (to-namestring (c::debug-source-name source))))))))

;;;; Utilities.

(defun read-symbol/package (symbol-name package-name)
  (let ((package (find-package package-name)))
    (unless package (error "No such package: %S" package-name))
    (handler-case 
        (let ((*package* package))
          (read-from-string symbol-name))
      (reader-error () nil))))

;;; Asynchronous eval

(defun guess-package-from-string (name)
  (or (and name
	   (or (find-package name) 
	       (find-package (string-upcase name))))
      *package*))

(defun read-catch-errors (string)
  (let (form (error nil))
    (handler-case 
	(setq form (read-from-string string))
      (t (condition) (setq error (princ-to-string condition))))
    (values form error)))

(defvar *swank-debugger-condition*)
(defvar *swank-debugger-hook*)

(defun eval-string-async (string package-name id)
  (let ((*package* (guess-package-from-string package-name)))
    (multiple-value-bind (form error) (read-catch-errors string)
      (if error
	(send-to-emacs `(:CALL-CONTINUATION ,id (:READ-FAILED ,error)))
	(let ((*debugger-hook* 
	       (lambda (condition hook)
		 (send-to-emacs `(:DEBUGGER-HOOK
				  ,debug::*debug-command-level*))
		 (let ((*swank-debugger-condition* condition)
		       (*swank-debugger-hook* hook))
                   (read-from-emacs)))))
	  (let (ok result)
	    (unwind-protect
		 (progn (setq result (eval form))
			(setq ok t))
	      (send-to-emacs `(:CALL-CONTINUATION ,id 
			       ,(if ok 
				    `(:OK ,result)
				    `(:ABORTED)))))))))))

(defun briefely-describe-symbol-for-emacs (symbol)
  "Return a plist of describing SYMBOL.  Return NIL if the symbol is
unbound."
  (let ((result '()))
    (labels ((first-line (string) 
	       (subseq string 0 (position #\newline string)))
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
	  (list* :designator (prin1-to-string-for-emacs symbol) result)))))

(defun apropos-list-for-emacs  (name)
  (list (package-name *package*)
	(ext:collect ((pack))
	  (ext:map-apropos 
	   (lambda (symbol)
	     (unless (keywordp symbol)
	       (let ((plist (briefely-describe-symbol-for-emacs symbol)))
		 (when plist
		   (pack plist)))))
	   name)
	  (pack))))

(defun set-stdin-non-blocking ()
  (let ((fd (sys:fd-stream-fd sys:*stdin*)))
    (flet ((fcntl (fd cmd arg)
	     (multiple-value-bind (flags errno) (unix:unix-fcntl fd cmd arg)
	       (or flags 
		   (error "fcntl: ~A" (unix:get-unix-error-msg errno))))))
      (let ((flags (fcntl fd unix:F-GETFL 0)))
	(fcntl fd unix:F-SETFL (logior flags unix:O_NONBLOCK))))))

(set-stdin-non-blocking)


;;; Debugging stuff

(defvar *sldb-level* 0)
(defvar *sldb-stack-top*)
(defvar *sldb-restarts*)

(defun sldb-loop ()
  (unix:unix-sigsetmask 0)
  (let* ((*sldb-level* (1+ *sldb-level*))
	 (*sldb-stack-top* (or debug:*stack-top-hint* (di:top-frame)))
	 (*sldb-restarts* (compute-restarts *swank-debugger-condition*))
	 (debug:*stack-top-hint* nil)
	 (*debugger-hook* nil)
	 (level *sldb-level*))
    (unwind-protect
	 (loop
	  (with-simple-restart (abort "Return to sldb level ~D." level)
	    (send-to-emacs `(:sldb-prompt ,level))
	    (read-from-emacs)))
      (send-to-emacs `(:sldb-abort ,level)))))

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

(defun compute-backtrace ()
  (loop for frame = *sldb-stack-top* then (di:frame-down frame)
	while frame collect frame))

(defun format-frame-for-emacs (frame)
  (list (di:frame-number frame)
	(with-output-to-string (*standard-output*) 
	  (debug::print-frame-call frame :verbosity 1))))

(defun backtrace-for-emacs (start end)
  (let ((frames (compute-backtrace)))
    (list (length frames)
	  (mapcar #'format-frame-for-emacs (subseq frames start end)))))

(defun debugger-info-for-emacs (start end)
  (list (format-condition-for-emacs)
	(format-restarts-for-emacs)
	(backtrace-for-emacs start end)))

(defun nth-frame (index)
  (nth index (compute-backtrace)))

(defun nth-restart (index)
  (nth index *sldb-restarts*))

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
      (let ((start (file-position file)))
	(file-position file (1- start))
	(read file)
	(list start (file-position file))))))

(defun frame-code-location-for-emacs (index)
  (let* ((frame (nth-frame index))
	 (code-location (di:frame-code-location frame))
	 (debug-source (di:code-location-debug-source code-location))
	 (from (di:debug-source-from debug-source))
	 (name (di:debug-source-name debug-source)))
    (list
     :from from
     :filename (if (eq from :file)
		   (ext:unix-namestring (truename name)))
     :position (if (eq from :file)
		   (code-location-file-position code-location))
     :source-form
     (with-output-to-string (*standard-output*)
       (debug::print-code-location-source-form code-location 100 t)))))

(defun safe-frame-code-location-for-emacs (index)
  (handler-case (frame-code-location-for-emacs index)
    (t (c) (list :error (debug::safe-condition-message c)))))
      
(defun eval-string-in-frame (string index)
  (prin1-to-string-for-emacs
   (di:eval-in-frame (nth-frame index) (read-from-string string))))

(defun list-locals (index)
  (let* ((frame (nth-frame index))
	 (location (di:frame-code-location frame))
	 (debug-function (di:frame-debug-function frame))
	 (debug-variables (di:ambiguous-debug-variables debug-function "")))
    (with-output-to-string (*standard-output*)
      (dolist (v debug-variables)
	(format t "~S~:[#~D~;~*~] ~A~&   ~S~&"
		(di:debug-variable-symbol v)
		(zerop (di:debug-variable-id v))
		(di:debug-variable-id v)
		(di:debug-variable-validity v location)
		(di:debug-variable-value v frame))))))

(defun invoke-nth-restart (index)
  (invoke-restart (nth-restart index)))

(defun quit-from-debugger ()
  (throw 'lisp::top-level-catcher nil))

;; (+ 1 1)
;; (/ 1 0)