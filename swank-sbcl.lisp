;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; swank-sbcl.lisp --- SLIME backend for SBCL.
;;;
;;; Created 2003, Daniel Barlow <dan@metacircles.com>
;;;
;;; This code has been placed in the Public Domain.  All warranties are 
;;; disclaimed.

;;; This is a Slime backend for SBCL.  Requires SBCL 0.8.5 or later
;;; for the SB-INTROSPECT contrib


;;; Administrivia

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require 'sb-bsd-sockets)
  (require 'sb-introspect)
  (require 'sb-posix)
  )

(declaim (optimize (debug 3)))
(in-package :swank-backend)

(import
 '(sb-gray:fundamental-character-output-stream
   sb-gray:stream-write-char
   sb-gray:stream-line-length
   sb-gray:stream-force-output
   sb-gray:fundamental-character-input-stream
   sb-gray:stream-read-char
   sb-gray:stream-listen
   sb-gray:stream-unread-char
   sb-gray:stream-clear-input
   sb-gray:stream-line-column
   sb-gray:stream-line-length))

;;; TCP Server

(defimplementation preferred-communication-style ()
  (cond ((and (sb-int:featurep :sb-thread)
              (sb-int:featurep :sb-futex))
         :spawn)
        ((fboundp 'sb-posix::fcntl)
         :sigio)
        (t
         :fd-handler)))
        
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
  (sb-sys:invalidate-descriptor (socket-fd socket))
  (sb-bsd-sockets:socket-close socket))

(defimplementation accept-connection (socket)
  (make-socket-io-stream (accept socket)))

(defvar *sigio-handlers* '()
  "List of (key . fn) pairs to be called on SIGIO.")

(defun sigio-handler (signal code scp)
  (declare (ignore signal code scp))
  (mapc (lambda (handler)
          (funcall (the function (cdr handler))))
        *sigio-handlers*))

(defun set-sigio-handler ()
  (sb-sys:enable-interrupt sb-unix:sigio (lambda (signal code scp)
                                           (sigio-handler signal code scp))))

(defun enable-sigio-on-fd (fd)
  (sb-posix::fcntl fd sb-posix::f-setfl sb-posix::o-async)
  (sb-posix::fcntl fd sb-posix::f-setown (getpid)))

(defimplementation add-sigio-handler (socket fn)
  (set-sigio-handler)
  (let ((fd (socket-fd socket)))
    (format *debug-io* "Adding sigio handler: ~S ~%" fd)
    (enable-sigio-on-fd fd)
    (push (cons fd fn) *sigio-handlers*)))

(defimplementation remove-sigio-handlers (socket)
  (let ((fd (socket-fd socket)))
    (setf *sigio-handlers* (delete fd *sigio-handlers* :key #'car))
    (sb-sys:invalidate-descriptor fd)) 
  (close socket))

(defimplementation add-fd-handler (socket fn)
  (declare (type function fn))
  (let ((fd (socket-fd socket)))
    (format *debug-io* "; Adding fd handler: ~S ~%" fd)
    (sb-sys:add-fd-handler fd :input (lambda (_) 
                                       _
                                       (funcall fn)))))

(defimplementation remove-fd-handlers (socket)
  (sb-sys:invalidate-descriptor (socket-fd socket)))

(defun socket-fd (socket)
  (etypecase socket
    (fixnum socket)
    (sb-bsd-sockets:socket (sb-bsd-sockets:socket-file-descriptor socket))
    (file-stream (sb-sys:fd-stream-fd socket))))

(defun make-socket-io-stream (socket)
  (sb-bsd-sockets:socket-make-stream socket
                                     :output t
                                     :input t
                                     :element-type 'base-char))

(defun accept (socket)
  "Like socket-accept, but retry on EAGAIN."
  (loop (handler-case
            (return (sb-bsd-sockets:socket-accept socket))
          (sb-bsd-sockets:interrupted-error ()))))

(defimplementation emacs-connected (stream)
  (declare (ignore stream))
  (setq sb-ext:*invoke-debugger-hook* 
        (find-symbol (string :swank-debugger-hook) (find-package :swank))))

(defmethod call-without-interrupts (fn)
  (declare (type function fn))
  (sb-sys:without-interrupts (funcall fn)))

(defimplementation getpid ()
  (sb-posix:getpid))

(defimplementation lisp-implementation-type-name ()
  "sbcl")

(defimplementation quit-lisp ()
  (sb-ext:quit))

;;; Utilities

(defvar *swank-debugger-stack-frame*)

(defimplementation arglist (fname)
  (sb-introspect:function-arglist fname))

(defvar *buffer-name* nil)
(defvar *buffer-offset*)
(defvar *buffer-substring* nil)

(defvar *previous-compiler-condition* nil
  "Used to detect duplicates.")

(defun handle-notification-condition (condition)
  "Handle a condition caused by a compiler warning.
This traps all compiler conditions at a lower-level than using
C:*COMPILER-NOTIFICATION-FUNCTION*. The advantage is that we get to
craft our own error messages, which can omit a lot of redundant
information."
  (let ((context (sb-c::find-error-context nil)))
    (unless (eq condition *previous-compiler-condition*)
      (setq *previous-compiler-condition* condition)
      (signal-compiler-condition condition context))))

(defun signal-compiler-condition (condition context)
  (signal (make-condition
           'compiler-condition
           :original-condition condition
           :severity (etypecase condition
                       (sb-c:compiler-error  :error)
                       (sb-ext:compiler-note :note)
                       (style-warning        :style-warning)
                       (warning              :warning))
           :short-message (brief-compiler-message-for-emacs condition)
           :message (long-compiler-message-for-emacs condition context)
           :location (compiler-note-location context))))



(defun compiler-note-location (context)
  (cond (context
         (resolve-note-location
          *buffer-name*
          (sb-c::compiler-error-context-file-name context)
          (sb-c::compiler-error-context-file-position context)
          (current-compiler-error-source-path context)
          (sb-c::compiler-error-context-original-source  context)))
        (t
         (resolve-note-location *buffer-name* nil nil nil nil))))

(defgeneric resolve-note-location (buffer file-name file-position 
                                          source-path source))

(defmethod resolve-note-location ((b (eql nil)) (f pathname) pos path source)
  (make-location
   `(:file ,(namestring (truename f)))
   `(:position ,(1+ (source-path-file-position path f)))))

#+(or)
(defmethod resolve-note-location ((b string) (f (eql :stream)) pos path source)
  (make-location
   `(:buffer ,b)
   `(:position ,(+ *buffer-offset*
                   (source-path-string-position path *buffer-substring*)))))

;; SBCL doesn't have compile-from-stream, so C-c C-c ends up here
(defmethod resolve-note-location ((b string) (f (eql :lisp)) pos path source)
  ;; Remove the surrounding lambda from the path (was added by
  ;; swank-compile-string)
  (destructuring-bind (_ form &rest rest) path
    (declare (ignore _))
    (make-location
     `(:buffer ,b)
     `(:position ,(+ *buffer-offset*
                     (source-path-string-position (list* (- form 2) rest)
                                                  *buffer-substring*))))))

(defmethod resolve-note-location (b (f (eql :lisp)) pos path (source string))
  (make-location
   `(:source-form ,source)
   `(:position 1)))

(defmethod resolve-note-location (buffer
                                  (file (eql nil)) 
                                  (pos (eql nil)) 
                                  (path (eql nil))
                                  (source (eql nil)))
  (list :error "No error location available"))

(defun brief-compiler-message-for-emacs (condition)
  "Briefly describe a compiler error for Emacs.
When Emacs presents the message it already has the source popped up
and the source form highlighted. This makes much of the information in
the error-context redundant."
  (princ-to-string condition))

(defun long-compiler-message-for-emacs (condition error-context)
  "Describe a compiler error for Emacs including context information."
  (declare (type (or sb-c::compiler-error-context null) error-context))
  (multiple-value-bind (enclosing source)
      (if error-context
          (values (sb-c::compiler-error-context-enclosing-source error-context)
                  (sb-c::compiler-error-context-source error-context)))
    (format nil "~@[--> ~{~<~%--> ~1:;~A~> ~}~%~]~@[~{==>~%~A~%~}~]~A"
            enclosing source condition)))

(defun current-compiler-error-source-path (context)
  "Return the source-path for the current compiler error.
Returns NIL if this cannot be determined by examining internal
compiler state."
  (cond ((sb-c::node-p context)
         (reverse
          (sb-c::source-path-original-source
           (sb-c::node-source-path context))))
        ((sb-c::compiler-error-context-p context)
         (reverse
          (sb-c::compiler-error-context-original-source-path context)))))

(defimplementation call-with-compilation-hooks (function)
  (declare (type function function))
  (handler-bind ((sb-c:compiler-error  #'handle-notification-condition)
                 (sb-ext:compiler-note #'handle-notification-condition)
                 (style-warning        #'handle-notification-condition)
                 (warning              #'handle-notification-condition))
    (funcall function)))

(defvar *trap-load-time-warnings* nil)

(defimplementation swank-compile-file (filename load-p)
  (flet ((loadit (fasl-file) (when (and load-p fasl-file) (load fasl-file))))
    (cond (*trap-load-time-warnings*
           (with-compilation-hooks ()
             (loadit (compile-file filename))))
          (t
           (loadit (with-compilation-hooks () 
                     (compile-file filename)))))))

(defimplementation swank-compile-string (string &key buffer position)
  (let ((form (read-from-string (format nil "(~S () ~A)" 'lambda string))))
    (flet ((compileit (cont)
             (with-compilation-hooks ()
               (let ((*buffer-name* buffer)
                     (*buffer-offset* position)
                     (*buffer-substring* string))
                 (funcall cont (compile nil form))))))
      (cond (*trap-load-time-warnings*
             (compileit #'funcall))
            (t 
             (funcall (compileit #'identity)))))))

;;;; Definitions

(defvar *debug-definition-finding* nil
  "When true don't handle errors while looking for definitions.
This is useful when debugging the definition-finding code.")

;;; FIXME we don't handle the compiled-interactively case yet.  That
;;; should have NIL :filename & :position, and non-NIL :source-form
(defun function-source-location (function &optional name)
  "Try to find the canonical source location of FUNCTION."
  (let* ((def (sb-introspect:find-definition-source function))
         (pathname (sb-introspect:definition-source-pathname def))
         (path (sb-introspect:definition-source-form-path def))
         (position (sb-introspect:definition-source-character-offset def)))
    (unless pathname
      (return-from function-source-location
        (list :error (format nil "No filename for: ~S" function))))
    (multiple-value-bind (truename condition) 
        (ignore-errors (truename pathname))
      (when condition 
        (return-from function-source-location
          (list :error (format nil "~A" condition))))
      (make-location
       (list :file (namestring truename))
       ;; source-paths depend on the file having been compiled with
       ;; lotsa debugging.  If not present, return the function name 
       ;; for emacs to attempt to find with a regex
       (cond (path (list :source-path path position))
             (t (list :function-name 
                      (or (and name (string name))
                          (string (sb-kernel:%fun-name function))))))))))

(defun safe-function-source-location (fun name)
  (if *debug-definition-finding*
      (function-source-location fun name)
      (handler-case (function-source-location fun name)
        (error (e) 
          (list (list :error (format nil "Error: ~A" e)))))))

(defun method-definitions (gf)
  (let ((methods (sb-mop:generic-function-methods gf))
        (name (sb-mop:generic-function-name gf)))
    (loop for method in methods 
          collect (list `(method ,name ,(sb-pcl::unparse-specializers method)) 
                        (safe-function-source-location method name)))))

(defun function-definitions (name)
  (flet ((loc (fn name) (safe-function-source-location fn name)))
    (cond ((and (symbolp name) (macro-function name))
           (list (list `(defmacro ,name) 
                       (loc (macro-function name) name))))
          ((fboundp name)
           (let ((fn (fdefinition name)))
             (typecase fn
               (generic-function
                (cons (list `(defgeneric ,name) (loc fn name))
                      (method-definitions fn)))
               (t
                (list (list `(function ,name) (loc fn name))))))))))

(defimplementation find-definitions (name)
  (function-definitions name))

(defimplementation describe-symbol-for-emacs (symbol)
  "Return a plist describing SYMBOL.
Return NIL if the symbol is unbound."
  (let ((result '()))
    (labels ((doc (kind)
	       (or (documentation symbol kind) :not-documented))
	     (maybe-push (property value)
	       (when value
		 (setf result (list* property value result)))))
      (maybe-push
       :variable (multiple-value-bind (kind recorded-p)
		     (sb-int:info :variable :kind symbol)
		   (declare (ignore kind))
		   (if (or (boundp symbol) recorded-p)
		       (doc 'variable))))
      (maybe-push
       :function (if (fboundp symbol) 
		     (doc 'function)))
      (maybe-push
       :setf (if (or (sb-int:info :setf :inverse symbol)
		     (sb-int:info :setf :expander symbol))
		 (doc 'setf)))
      (maybe-push
       :type (if (sb-int:info :type :kind symbol)
		 (doc 'type)))
      result)))

(defimplementation describe-definition (symbol type)
  (case type
    (:variable
     (describe symbol))
    (:function
     (describe (symbol-function symbol)))
    (:setf
     (describe (or (sb-int:info :setf :inverse symbol)
                   (sb-int:info :setf :expander symbol))))
    (:class
     (describe (find-class symbol)))
    (:type
     (describe (sb-kernel:values-specifier-type symbol)))))

;;; macroexpansion

(defimplementation macroexpand-all (form)
  (let ((sb-walker:*walk-form-expand-macros-p* t))
    (sb-walker:walk-form form)))


;;; Debugging

(defvar *sldb-stack-top*)

(defimplementation call-with-debugging-environment (debugger-loop-fn)
  (declare (type function debugger-loop-fn))
  (let* ((*sldb-stack-top* (or sb-debug:*stack-top-hint* (sb-di:top-frame)))
	 (sb-debug:*stack-top-hint* nil))
    (handler-bind ((sb-di:debug-condition 
		    (lambda (condition)
                      (signal (make-condition
                               'sldb-condition
                               :original-condition condition)))))
      (funcall debugger-loop-fn))))

(defun nth-frame (index)
  (do ((frame *sldb-stack-top* (sb-di:frame-down frame))
       (i index (1- i)))
      ((zerop i) frame)))

(defimplementation compute-backtrace (start end)
  "Return a list of frames starting with frame number START and
continuing to frame number END or, if END is nil, the last frame on the
stack."
  (let ((end (or end most-positive-fixnum)))
    (loop for f = (nth-frame start) then (sb-di:frame-down f)
	  for i from start below end
	  while f
	  collect f)))

(defimplementation print-frame (frame stream)
  (let ((*standard-output* stream))
    (sb-debug::print-frame-call frame :verbosity 1 :number nil)))

(defun code-location-source-path (code-location)
  (let* ((location (sb-debug::maybe-block-start-location code-location))
	 (form-num (sb-di:code-location-form-number location)))
    (let ((translations (sb-debug::get-toplevel-form location)))
      (unless (< form-num (length translations))
	(error "Source path no longer exists."))
      (reverse (cdr (svref translations form-num))))))

(defun code-location-file-position (code-location)
  (let* ((debug-source (sb-di:code-location-debug-source code-location))
	 (filename (sb-di:debug-source-name debug-source))
	 (path (code-location-source-path code-location)))
    (source-path-file-position path filename)))

;;; source-path-file-position and friends are in swank-source-path-parser

(defun debug-source-info-from-emacs-buffer-p (debug-source)
  (let ((info (sb-c::debug-source-info debug-source)))
    (and info 
	 (consp info)
	 (eq :emacs-buffer (car info)))))

(defun source-location-for-emacs (code-location)
  (let* ((debug-source (sb-di:code-location-debug-source code-location))
	 (from (sb-di:debug-source-from debug-source))
	 (name (sb-di:debug-source-name debug-source)))
    (ecase from
      (:file 
       (let ((source-path (ignore-errors
                            (code-location-source-path code-location))))
         (cond (source-path
                ;; XXX: code-location-source-path reads the source !!
                (let ((position (code-location-file-position code-location)))
                  (make-location 
                   (list :file (namestring (truename name)))
                   (list :source-path source-path position))))
               (t
                (let* ((dfn (sb-di:code-location-debug-fun code-location))
                       (fn (sb-di:debug-fun-fun dfn)))
                  (unless fn
                    (error "Cannot find source location for: ~A "
                           code-location))
                  (function-source-location 
                   fn (sb-di:debug-fun-name dfn)))))))
               
      (:lisp
       (make-location
        (list :source-form (with-output-to-string (*standard-output*)
                             (sb-debug::print-code-location-source-form
                              code-location 100)))
        (list :position 0))))))

(defun safe-source-location-for-emacs (code-location)
  (handler-case (source-location-for-emacs code-location)
    (error (c) (list :error (format nil "~A" c)))))
                                               
(defimplementation frame-source-location-for-emacs (index)
  (safe-source-location-for-emacs 
   (sb-di:frame-code-location (nth-frame index))))

(defimplementation frame-locals (index)
  (let* ((frame (nth-frame index))
	 (location (sb-di:frame-code-location frame))
	 (debug-function (sb-di:frame-debug-fun frame))
	 (debug-variables (sb-di::debug-fun-debug-vars debug-function)))
    (declare (type (or null simple-vector) debug-variables))
    (loop for v across debug-variables
          collect (list
                   :name (sb-di:debug-var-symbol v)
                   :id (sb-di:debug-var-id v)
                   :value (if (eq (sb-di:debug-var-validity v location)
                                  :valid)
                              (sb-di:debug-var-value v frame)
                              '#:<not-available>)))))

(defimplementation frame-catch-tags (index)
  (mapcar #'car (sb-di:frame-catches (nth-frame index))))

(defimplementation eval-in-frame (form index)
  (let ((frame (nth-frame index)))
    (funcall (the function
               (sb-di:preprocess-for-eval form 
                                          (sb-di:frame-code-location frame)))
             frame)))

(defun sb-debug-catch-tag-p (tag)
  (and (symbolp tag)
       (not (symbol-package tag))
       (string= tag :sb-debug-catch-tag)))

(defimplementation return-from-frame (index form)
  (let* ((frame (nth-frame index))
         (probe (assoc-if #'sb-debug-catch-tag-p
                          (sb-di::frame-catches frame))))
    (cond (probe (throw (car probe) (eval-in-frame form index)))
          (t (format nil "Cannot return from frame: ~S" frame)))))
    
;;;;; reference-conditions

(defimplementation format-sldb-condition (condition)
  (let ((sb-int:*print-condition-references* nil))
    (princ-to-string condition)))

(defimplementation condition-references (condition)
  (if (typep condition 'sb-int:reference-condition)
      (sb-int:reference-condition-references condition)
      '()))


;;;; Profiling

(defimplementation profile (fname)
  (when fname (eval `(sb-profile:profile ,fname))))

(defimplementation unprofile (fname)
  (when fname (eval `(sb-profile:unprofile ,fname))))

(defimplementation unprofile-all ()
  (sb-profile:unprofile)
  "All functions unprofiled.")

(defimplementation profile-report ()
  (sb-profile:report))

(defimplementation profile-reset ()
  (sb-profile:reset)
  "Reset profiling counters.")

(defimplementation profiled-functions ()
  (sb-profile:profile))


;;;; Inspector

(defmethod inspected-parts (o)
  (cond ((sb-di::indirect-value-cell-p o)
	 (inspected-parts-of-value-cell o))
	(t
	 (multiple-value-bind (text labeledp parts)
             (sb-impl::inspected-parts o)
	   (let ((parts (if labeledp 
			    (loop for (label . value) in parts
				  collect (cons (string label) value))
			    (loop for value in parts
				  for i from 0
				  collect (cons (format nil "~D" i) value)))))
	     (values text parts))))))

(defun inspected-parts-of-value-cell (o)
  (values (format nil "~A~% is a value cell." o)
	  (list (cons "Value" (sb-kernel:value-cell-ref o)))))

(defmethod inspected-parts ((o function))
  (let ((header (sb-kernel:widetag-of o)))
    (cond ((= header sb-vm:simple-fun-header-widetag)
	   (values 
	    (format nil "~A~% is a simple-fun." o)
	    (list (cons "Self" (sb-kernel:%simple-fun-self o))
		  (cons "Next" (sb-kernel:%simple-fun-next o))
		  (cons "Name" (sb-kernel:%simple-fun-name o))
		  (cons "Arglist" (sb-kernel:%simple-fun-arglist o))
		  (cons "Type" (sb-kernel:%simple-fun-type o))
		  (cons "Code Object" (sb-kernel:fun-code-header o)))))
	  ((= header sb-vm:closure-header-widetag)
	   (values (format nil "~A~% is a closure." o)
		   (list* 
		    (cons "Function" (sb-kernel:%closure-fun o))
		    (loop for i from 0 
                          below (- (sb-kernel:get-closure-length o) 
                                   (1- sb-vm:closure-info-offset))
			  collect (cons (format nil "~D" i)
					(sb-kernel:%closure-index-ref o i))))))
	  (t (call-next-method o)))))

(defmethod inspected-parts ((o sb-kernel:code-component))
  (values (format nil "~A~% is a code data-block." o)
	  `(("First entry point" . ,(sb-kernel:%code-entry-points o))
	    ,@(loop for i from sb-vm:code-constants-offset 
		    below (sb-kernel:get-header-data o)
		    collect (cons (format nil "Constant#~D" i)
				  (sb-kernel:code-header-ref o i)))
	    ("Debug info" . ,(sb-kernel:%code-debug-info o))
	    ("Instructions"  . ,(sb-kernel:code-instructions o)))))

(defmethod inspected-parts ((o sb-kernel:fdefn))
  (values (format nil "~A~% is a fdefn object." o)
	  `(("Name" . ,(sb-kernel:fdefn-name o))
	    ("Function" . ,(sb-kernel:fdefn-fun o)))))


(defmethod inspected-parts ((o generic-function))
  (values (format nil "~A~% is a generic function." o)
          (list
           (cons "Method-Class" (sb-pcl:generic-function-method-class o))
           (cons "Methods" (sb-pcl:generic-function-methods o))
           (cons "Name" (sb-pcl:generic-function-name o))
           (cons "Declarations" (sb-pcl:generic-function-declarations o))
           (cons "Method-Combination" 
                 (sb-pcl:generic-function-method-combination o))
           (cons "Lambda-List" (sb-pcl:generic-function-lambda-list o))
           (cons "Precedence-Order" 
                 (sb-pcl:generic-function-argument-precedence-order o))
           (cons "Pretty-Arglist"
                 (sb-pcl::generic-function-pretty-arglist o))
           (cons "Initial-Methods" 
                 (sb-pcl::generic-function-initial-methods  o)))))


;;;; Support for SBCL syntax

(defun feature-in-list-p (feature list)
  (etypecase feature
    (symbol (member feature list :test #'eq))
    (cons (flet ((subfeature-in-list-p (subfeature)
		   (feature-in-list-p subfeature list)))
	    (ecase (first feature)
	      (:or  (some  #'subfeature-in-list-p (rest feature)))
	      (:and (every #'subfeature-in-list-p (rest feature)))
	      (:not (let ((rest (cdr feature)))
		      (if (or (null (car rest)) (cdr rest))
			(error "wrong number of terms in compound feature ~S"
			       feature)
			(not (subfeature-in-list-p (second feature)))))))))))

(defun shebang-reader (stream sub-character infix-parameter)
  (declare (ignore sub-character))
  (when infix-parameter
    (error "illegal read syntax: #~D!" infix-parameter))
  (let ((next-char (read-char stream)))
    (unless (find next-char "+-")
      (error "illegal read syntax: #!~C" next-char))
    ;; When test is not satisfied
    ;; FIXME: clearer if order of NOT-P and (NOT NOT-P) were reversed? then
    ;; would become "unless test is satisfied"..
    (when (let* ((*package* (find-package "KEYWORD"))
		 (*read-suppress* nil)
		 (not-p (char= next-char #\-))
		 (feature (read stream)))
	    (if (feature-in-list-p feature *features*)
		not-p
		(not not-p)))
      ;; Read (and discard) a form from input.
      (let ((*read-suppress* t))
	(read stream t nil t))))
 (values))

(defvar *shebang-readtable* 
  (let ((*readtable* (copy-readtable nil)))
    (set-dispatch-macro-character #\# #\! 
                                  (lambda (s c n) (shebang-reader s c n))
                                  *readtable*)
    *readtable*))

(defun shebang-readtable ()
  *shebang-readtable*)

(defun sbcl-package-p (package)
  (let ((name (package-name package)))
    (eql (mismatch "SB-" name) 3)))

(defvar *debootstrap-packages* t)

(defimplementation call-with-syntax-hooks (fn)
  (cond ((and *debootstrap-packages* 
              (sbcl-package-p *package*))
         (handler-bind ((sb-int:bootstrap-package-not-found 
                         #'sb-int:debootstrap-package))
           (funcall fn)))
        (t
         (funcall fn))))

(defimplementation default-readtable-alist ()
  (let ((readtable (shebang-readtable)))
    (loop for p in (remove-if-not #'sbcl-package-p (list-all-packages))
          collect (cons (package-name p) readtable))))


;;;; Multiprocessing

#+sb-thread
(progn
  (defimplementation spawn (fn &key name)
    (declare (ignore name))
    (sb-thread:make-thread fn))

  (defimplementation startup-multiprocessing ())

  (defimplementation thread-name (thread)
    (format nil "Thread ~D" thread))

  (defimplementation thread-status (thread)
    (declare (ignore thread))
    "???")

  (defimplementation make-lock (&key name)
    (sb-thread:make-mutex :name name))

  (defimplementation call-with-lock-held (lock function)
    (declare (type function function))
    (sb-thread:with-mutex (lock) (funcall function)))

  (defimplementation current-thread ()
    (sb-thread:current-thread-id))

  (defimplementation all-threads ()
    (sb-thread::mapcar-threads
     (lambda (sap)
       (sb-sys:sap-ref-32 sap (* sb-vm:n-word-bytes
                                 sb-vm::thread-pid-slot)))))
 
  (defimplementation interrupt-thread (thread fn)
    (sb-thread:interrupt-thread thread fn))

  (defimplementation kill-thread (thread)
    (sb-thread:terminate-thread thread))

  ;; XXX there is some deadlock / race condition here (with old 2.4 kernels)

  (defvar *mailbox-lock* (sb-thread:make-mutex :name "mailbox lock"))
  (defvar *mailboxes* (list))
  (declaim (type list *mailboxes*))

  (defstruct (mailbox (:conc-name mailbox.)) 
    thread
    (mutex (sb-thread:make-mutex))
    (waitqueue  (sb-thread:make-waitqueue))
    (queue '() :type list))

  (defun mailbox (thread)
    "Return THREAD's mailbox."
    (sb-thread:with-mutex (*mailbox-lock*)
      (or (find thread *mailboxes* :key #'mailbox.thread)
          (let ((mb (make-mailbox :thread thread)))
            (push mb *mailboxes*)
            mb))))

  (defimplementation send (thread message)
    (let* ((mbox (mailbox thread))
           (mutex (mailbox.mutex mbox)))
      (sb-thread:with-mutex (mutex)
        (setf (mailbox.queue mbox)
              (nconc (mailbox.queue mbox) (list message)))
        (sb-thread:condition-broadcast (mailbox.waitqueue mbox)))))

  (defimplementation receive ()
    (let* ((mbox (mailbox (sb-thread:current-thread-id)))
           (mutex (mailbox.mutex mbox)))
      (sb-thread:with-mutex (mutex)
        (loop
         (let ((q (mailbox.queue mbox)))
           (cond (q (return (pop (mailbox.queue mbox))))
                 (t (sb-thread:condition-wait (mailbox.waitqueue mbox)
                                              mutex))))))))

  )
