;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; swank-sbcl.lisp --- SLIME backend for SBCL.
;;;
;;; Created 2003, Daniel Barlow <dan@metacircles.com>
;;;
;;; This code has been placed in the Public Domain.  All warranties are 
;;; disclaimed.

;;; Requires the SB-INTROSPECT contrib.

;;; Administrivia

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require 'sb-bsd-sockets)
  (require 'sb-introspect)
  (require 'sb-posix)
  )


(in-package :swank-backend)
(declaim (optimize (debug 2)))

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

;;; swank-mop

(import-swank-mop-symbols :sb-mop '(:slot-definition-documentation))

(defun swank-mop:slot-definition-documentation (slot)
  (sb-pcl::documentation slot t))  

;;; TCP Server

(defimplementation preferred-communication-style ()
  (if (and (sb-int:featurep :sb-thread)
           (sb-int:featurep :sb-futex))
      :spawn
      :fd-handler))
        
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

(defimplementation accept-connection (socket &key external-format)
  (make-socket-io-stream (accept socket) external-format))

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

(defun make-socket-io-stream (socket external-format)
  (let ((encoding (ecase external-format
                    (:iso-latin-1-unix :iso-8859-1)
                    (:utf-8-unix :utf-8))))
    (sb-bsd-sockets:socket-make-stream socket
                                       :output t
                                       :input t
                                       :element-type 'character
                                       :external-format encoding)))

(defun accept (socket)
  "Like socket-accept, but retry on EAGAIN."
  (loop (handler-case
            (return (sb-bsd-sockets:socket-accept socket))
          (sb-bsd-sockets:interrupted-error ()))))

(defimplementation emacs-connected ()
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

(defimplementation arglist ((fname t))
  (sb-introspect:function-arglist fname))

(defimplementation function-name ((f function))
  (sb-impl::%fun-name f))

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
                       (warning              :warning)
                       (error                :error))
           :short-message (brief-compiler-message-for-emacs condition)
           :references (condition-references (real-condition condition))
           :message (long-compiler-message-for-emacs condition context)
           :location (compiler-note-location context))))

(defun real-condition (condition)
  "Return the encapsulated condition or CONDITION itself."
  (typecase condition
    (sb-int:encapsulated-condition (sb-int:encapsulated-condition condition))
    (t condition)))

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
  (let ((sb-int:*print-condition-references* nil))
    (princ-to-string condition)))

(defun long-compiler-message-for-emacs (condition error-context)
  "Describe a compiler error for Emacs including context information."
  (declare (type (or sb-c::compiler-error-context null) error-context))
  (multiple-value-bind (enclosing source)
      (if error-context
          (values (sb-c::compiler-error-context-enclosing-source error-context)
                  (sb-c::compiler-error-context-source error-context)))
    (let ((sb-int:*print-condition-references* nil))
      (format nil "~@[--> ~{~<~%--> ~1:;~A~> ~}~%~]~@[~{==>~%~A~%~}~]~A"
              enclosing source condition))))

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
  (handler-bind ((sb-c:fatal-compiler-error #'handle-file-compiler-termination)
                 (sb-c:compiler-error  #'handle-notification-condition)
                 (sb-ext:compiler-note #'handle-notification-condition)
                 (style-warning        #'handle-notification-condition)
                 (warning              #'handle-notification-condition))
    (funcall function)))

(defun handle-file-compiler-termination (condition)
  "Handle a condition that caused the file compiler to terminate."
  (handle-notification-condition
   (sb-int:encapsulated-condition condition)))

(defvar *trap-load-time-warnings* nil)

(defimplementation swank-compile-file (filename load-p)
  (handler-case
      (let ((output-file (with-compilation-hooks ()
                           (compile-file filename))))
        (when (and load-p output-file)
          (load output-file)))
    (sb-c:fatal-compiler-error () nil)))

(defimplementation swank-compile-string (string &key buffer position directory)
  (declare (ignore directory))
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
    (append
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
                 (list (list `(function ,name) (loc fn name))))))))
     (when (compiler-macro-function name)
       (list (list `(define-compiler-macro ,name)
                   (loc (compiler-macro-function name) name)))))))

(defun transform-definitions (fun-info name)
  (loop for xform in (sb-c::fun-info-transforms fun-info)
        for loc = (safe-function-source-location
                   (sb-c::transform-function xform) name)
        for typespec = (sb-kernel:type-specifier (sb-c::transform-type xform))
        for note = (sb-c::transform-note xform)
        for spec = (if (consp typespec)
                       `(sb-c:deftransform ,(second typespec) ,note)
                       `(sb-c:deftransform ,note))
        collect `(,spec ,loc)))

(defun optimizer-definitions (fun-info fun-name)
  (let ((otypes '((sb-c::fun-info-derive-type . sb-c:derive-type)
                  (sb-c::fun-info-ltn-annotate . sb-c:ltn-annotate)
                  (sb-c::fun-info-ltn-annotate . sb-c:ltn-annotate)
                  (sb-c::fun-info-optimizer . sb-c:optimizer))))
    (loop for (reader . name) in otypes
          for fn = (funcall reader fun-info)
          when fn collect `((sb-c:defoptimizer ,name)
                            ,(safe-function-source-location fn fun-name)))))

(defun compiler-definitions (name)
  (let ((fun-info (sb-int:info :function :info name)))
    (when fun-info
      (append (transform-definitions fun-info name)
              (optimizer-definitions fun-info name)))))

(defimplementation find-definitions (name)
  (append (function-definitions name)
          (compiler-definitions name)))

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

(defun function-dspec (fn)
  "Describe where the function FN was defined.
Return a list of the form (NAME LOCATION)."
  (let ((name (sb-kernel:%fun-name fn)))
    (list name (safe-function-source-location fn name))))

(defimplementation list-callers (symbol)
  (let ((fn (fdefinition symbol)))
    (mapcar #'function-dspec (sb-introspect:find-function-callers fn))))

(defimplementation list-callees (symbol)
  (let ((fn (fdefinition symbol)))
    (mapcar #'function-dspec (sb-introspect:find-function-callees fn))))

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

(defun frame-debug-vars (frame)
  "Return a vector of debug-variables in frame."
  (sb-di::debug-fun-debug-vars (sb-di:frame-debug-fun frame)))

(defun debug-var-value (var frame location)
  (ecase (sb-di:debug-var-validity var location)
    (:valid (sb-di:debug-var-value var frame))
    ((:invalid :unknown) ':<not-available>)))

(defimplementation frame-locals (index)
  (let* ((frame (nth-frame index))
	 (loc (sb-di:frame-code-location frame))
	 (vars (frame-debug-vars frame)))
    (loop for v across vars collect
          (list :name (sb-di:debug-var-symbol v)
                :id (sb-di:debug-var-id v)
                :value (debug-var-value v frame loc)))))

(defimplementation frame-var-value (frame var)
  (let* ((frame (nth-frame frame))
         (dvar (aref (frame-debug-vars frame) var)))
    (debug-var-value dvar frame (sb-di:frame-code-location frame))))

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

(defclass sbcl-inspector (inspector)
  ())

(defimplementation make-default-inspector ()
  (make-instance 'sbcl-inspector))

(defmethod inspect-for-emacs ((o t) (inspector sbcl-inspector))
  (declare (ignore inspector))
  (cond ((sb-di::indirect-value-cell-p o)
         (values "A value cell."
                 `("Value: " (:value ,(sb-kernel:value-cell-ref o)))))
	(t
	 (multiple-value-bind (text labeledp parts)
             (sb-impl::inspected-parts o)
           (if labeledp
               (values text
                       (loop for (label . value) in parts
                             collect `(:value ,label)
                             collect " = "
                             collect `(:value ,value)
                             collect '(:newline)))
               (values text
                       (loop for value in parts
                             for i from 0
                             collect (princ-to-string i)
                             collect " = "
                             collect `(:value ,value)
                             collect '(:newline))))))))

(defmethod inspect-for-emacs ((o function) (inspector sbcl-inspector))
  (declare (ignore inspector))
  (let ((header (sb-kernel:widetag-of o)))
    (cond ((= header sb-vm:simple-fun-header-widetag)
	   (values "A simple-fun." 
                   `("Name: " (:value ,(sb-kernel:%simple-fun-name o))
                     (:newline)
                     "Arglist: " (:value ,(sb-kernel:%simple-fun-arglist o))
                     (:newline)
                     ,@(when (documentation o t)
                         `("Documentation: " (:newline) ,(documentation o t) (:newline)))
                     "Self: " (:value ,(sb-kernel:%simple-fun-self o))
                     (:newline)
                     "Next: " (:value ,(sb-kernel:%simple-fun-next o))
                     (:newline)
                     "Type: " (:value ,(sb-kernel:%simple-fun-type o))
                     (:newline)
                     "Code Object: " (:value ,(sb-kernel:fun-code-header o)))))
	  ((= header sb-vm:closure-header-widetag)
	   (values "A closure."
		   `("Function: " (:value ,(sb-kernel:%closure-fun o))
                     (:newline)
                     ,@(when (documentation o t)
                         `("Documentation: " (:newline) ,(documentation o t) (:newline)))
                     "Closed over values:"
                     (:newline)
                     ,@(loop for i from 0 
                          below (- (sb-kernel:get-closure-length o) 
                                   (1- sb-vm:closure-info-offset))
			  collect (princ-to-string i)
                          collect " = "
                          collect `(:value ,(sb-kernel:%closure-index-ref o i))
                          collect '(:newline)))))
	  (t (call-next-method o)))))

(defmethod inspect-for-emacs ((o sb-kernel:code-component) (inspector sbcl-inspector))
  (declare (ignore inspector))
  (values "A code data-block."
	  `("First entry point: " (:value ,(sb-kernel:%code-entry-points o))
            (:newline)
            "Constants: " (:newline)
	    ,@(loop
                 for i from sb-vm:code-constants-offset 
                 below (sb-kernel:get-header-data o)
                 collect (princ-to-string i)
                 collect " = "
                 collect `(:value ,(sb-kernel:code-header-ref o i))
                 collect '(:newline))
	    "Debug info: " (:value ,(sb-kernel:%code-debug-info o))
	    "Instructions: "  (:value ,(sb-kernel:code-instructions o)))))

(defmethod inspect-for-emacs ((o sb-kernel:fdefn) (inspector sbcl-inspector))
  (declare (ignore inspector))
  (values "A fdefn object."
	  `("Name: "  (:value ,(sb-kernel:fdefn-name o))
            (:newline)
            "Function" (:value,(sb-kernel:fdefn-fun o))
            (:newline)
            ,@(when (documentation o t)
                `("Documentation: " (:newline) ,(documentation o t) (:newline))))))

(defmethod inspect-for-emacs :around ((o generic-function) (inspector sbcl-inspector))
  (declare (ignore inspector))
  (multiple-value-bind (title contents)
      (call-next-method)
    (values title
            (append contents
                    `("Pretty arglist: " (:value ,(sb-pcl::generic-function-pretty-arglist o))
                      (:newline)
                      "Initial methods: " (:value ,(sb-pcl::generic-function-initial-methods  o)))))))


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

(defmacro with-debootstrapping (&body body)
  (let ((not-found (find-symbol "BOOTSTRAP-PACKAGE-NOT-FOUND" "SB-INT"))
        (debootstrap (find-symbol "DEBOOTSTRAP-PACKAGE" "SB-INT")))
    (if (and not-found debootstrap)
        `(handler-bind ((,not-found #',debootstrap)) ,@body)
        `(progn ,@body))))

(defimplementation call-with-syntax-hooks (fn)
  (cond ((and *debootstrap-packages* 
              (sbcl-package-p *package*))
         (with-debootstrapping (funcall fn)))
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

  (defimplementation thread-id (thread)
    thread)

  (defimplementation find-thread (id)
    (if (member id (all-threads))
        id))
  
  (defimplementation thread-name (thread)
    (format nil "Thread ~D" thread))

  (defimplementation thread-status (thread)
    (sb-sys:without-gcing
     (let ((thread (sb-thread::thread-sap-from-id thread)))
       (cond (thread
              (let* ((sap (sb-sys:sap-ref-sap thread 
                                              (* sb-vm::thread-state-slot
                                                 sb-vm::n-word-bytes)))
                     (state (ash (sb-sys:sap-int sap)
                                 (- sb-vm::n-fixnum-tag-bits))))
                (case state
                  (0 "running")
                  (1 "stopping")
                  (2 "stopped")
                  (3 "dead")
                  (t (format nil "??? ~A" state)))))
             (t "??? ???")))))

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
