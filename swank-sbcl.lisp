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

;;; Cursory testing has found that the following appear to work
;;;
;;; * Symbol completion.
;;; * Evaluation of forms with C-M-x
;;; * Apropos
;;; * Compilation of defuns with C-c C-c
;;; * File compilation with C-c C-k, apparently including error parsing
;;; * Disassembling the symbol at point with C-c M-d
;;; * Describing symbol at point with C-c C-d
;;; * Macroexpanding with C-c RET
;;; * find-definition, using sb-introspect
;;; * Basic debugger stuff: restarts, backtrace, toggle details
;;; * Can now interrupt a busy sbcl with C-c C-g
;;; * Most of the tests
;;;
;;; Things that aren't done/don't work yet:
;;;
;;; * Cross-referencing (nor is it likely, absent XREF port to SBCL)
;;; * testsuite can't find LOOP, reports bogus failure on some arglist lookups
;;; * eval-in-frame
;;; * A slime command to load an asdf system.  Note that this might involve
;;;    compiling/loading files that Emacs has no buffers for
;;; * Dealing with multiple threads

;;; Administrivia

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require 'sb-bsd-sockets)
  (require 'sb-introspect))

(declaim (optimize (debug 3)))
(in-package :swank)

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

(defun without-interrupts* (body)
  (sb-sys:without-interrupts (funcall body)))

;;; TCP Server

(setq *swank-in-background* :fd-handler)

(defmethod create-socket (port)
  (let ((socket (make-instance 'sb-bsd-sockets:inet-socket
			       :type :stream
			       :protocol :tcp)))
    (setf (sb-bsd-sockets:sockopt-reuse-address socket) t)
    (sb-bsd-sockets:socket-bind socket #(127 0 0 1) port)
    (sb-bsd-sockets:socket-listen socket 5)
    socket))

(defmethod local-port (socket)
  (nth-value 1 (sb-bsd-sockets:socket-name socket)))

(defmethod close-socket (socket)
  (sb-bsd-sockets:socket-close socket))

(defmethod accept-connection (socket)
  (make-socket-io-stream (accept socket)))

(defmethod add-input-handler (socket fn)
  (sb-sys:add-fd-handler (socket-fd  socket)
                         :input (lambda (fd)
                                  (declare (ignore fd))
                                  (funcall fn))))

(defmethod remove-input-handlers (socket)
  (sb-sys:invalidate-descriptor (socket-fd socket))
  (close socket))

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

(defmethod make-fn-streams (input-fn output-fn)
  (let* ((output (make-instance 'slime-output-stream
                                :output-fn output-fn))
         (input  (make-instance 'slime-input-stream
                                :input-fn input-fn
                                :output-stream output)))
    (values input output)))

;;; Utilities

(defvar *swank-debugger-stack-frame*)

;;; adapted from cmucl
(defslimefun set-default-directory (directory)
  (setf *default-pathname-defaults* (merge-pathnames directory))
  (namestring *default-pathname-defaults*))

(defmethod arglist-string (fname)
  (let ((*print-case* :downcase))
    (multiple-value-bind (function condition)
        (ignore-errors (values 
                        (find-symbol-designator fname *buffer-package*)))
      (when condition
        (return-from arglist-string (format nil "(-- ~A)" condition)))
      (let ((arglist
             (ignore-errors (sb-introspect:function-arglist function))))
        (if arglist
            (princ-to-string arglist)
            "(-- <Unknown-Function>)")))))

(defvar *buffer-name* nil)
(defvar *buffer-offset*)

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
           :message (brief-compiler-message-for-emacs condition context)
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

(defmethod resolve-note-location ((b string) (f (eql :stream)) pos path source)
  (make-location
   `(:buffer ,b)
   `(:position ,(+ *buffer-offset*
                   (source-path-string-position path *buffer-substring*)))))

(defmethod resolve-note-location (b (f (eql :lisp)) pos path (source string))
  (make-location
   `(:source-form ,source)
   `(:position 1)))

(defmethod resolve-note-location (buffer
                                  (file (eql nil)) 
                                  (pos (eql nil)) 
                                  (path (eql nil))
                                  (source (eql nil)))
  (cond (buffer
         (make-location (list :buffer buffer) 
                        (list :position *buffer-offset*)))
        (*compile-file-truename*
         (make-location (list :file (namestring *compile-file-truename*))
                        (list :position 0)))
        (t 
         (list :error "No error location available"))))


(defun brief-compiler-message-for-emacs (condition error-context)
  "Briefly describe a compiler error for Emacs.
When Emacs presents the message it already has the source popped up
and the source form highlighted. This makes much of the information in
the error-context redundant."
  (declare (type (or sb-c::compiler-error-context null) error-context))
  (let ((enclosing 
         (and error-context
              (sb-c::compiler-error-context-enclosing-source error-context))))
    (if enclosing
        (format nil "--> ~{~<~%--> ~1:;~A~> ~}~%~A" enclosing condition)
        (format nil "~A" condition))))

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

(defmethod call-with-compilation-hooks (function)
  (handler-bind ((sb-c:compiler-error  #'handle-notification-condition)
                 (sb-ext:compiler-note #'handle-notification-condition)
                 (style-warning        #'handle-notification-condition)
                 (warning              #'handle-notification-condition))
    (funcall function)))

(defmethod compile-file-for-emacs (filename load-p)
  (with-compilation-hooks ()
    (multiple-value-bind (fasl-file w-p f-p) (compile-file filename)
      (cond ((and fasl-file (not f-p) load-p)
             (load fasl-file))
            (t fasl-file)))))

(defmethod compile-system-for-emacs (system-name)
  (with-compilation-hooks ()
    (asdf:operate 'asdf:load-op system-name)))

(defmethod compile-string-for-emacs (string &key buffer position)
  (with-compilation-hooks ()
    (let ((*package* *buffer-package*)
          (*buffer-name* buffer)
          (*buffer-offset* position))
      (eval (from-string
             (format nil "(funcall (compile nil '(lambda () ~A)))"
                     string))))))

;;;; xref stuff doesn't exist for sbcl yet

(defslimefun-unimplemented who-calls (function-name))

(defslimefun-unimplemented who-references (variable))

(defslimefun-unimplemented who-binds (variable))

(defslimefun-unimplemented who-sets (variable))

(defslimefun-unimplemented who-macroexpands (macro))

(defun source-path< (path1 path2)
  "Return true if PATH1 is lexically before PATH2."
  (and (every #'< path1 path2)
       (< (length path1) (length path2))))

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
                          (sb-kernel:%fun-name function)))))))))
                                
(defmethod find-function-locations (fname-string)
  (let* ((symbol (from-string fname-string)))
    (labels ((finder (fun)
               (cond ((and (symbolp fun) (macro-function fun))
                      (list 
                       (function-source-location (macro-function fun)
                                                 symbol)))
                     ((typep fun 'sb-mop:generic-function)
                      (list*
                       (function-source-location fun symbol)
                       (mapcar 
                        (lambda (x) (function-source-location x symbol))
                        (sb-mop:generic-function-methods fun))))
                     ((functionp fun) 
                      (list 
                       (function-source-location fun symbol)))
                     ((sb-introspect:valid-function-name-p fun)
                      (finder (fdefinition fun)))
                     (t (list 
                         (list :error "Not a function: ~A" fun))))))
      (if *debug-definition-finding*
          (finder symbol)
          (handler-case (finder symbol)
            (error (e) 
              (list (list :error (format nil "Error: ~A" e)))))))))

(defmethod describe-symbol-for-emacs (symbol)
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

(defslimefun describe-setf-function (symbol-name)
  (print-description-to-string `(setf ,(from-string symbol-name))))

(defslimefun describe-type (symbol-name)
  (print-description-to-string
   (sb-kernel:values-specifier-type (from-string symbol-name))))

(defslimefun describe-class (symbol-name)
  (print-description-to-string (find-class (from-string symbol-name) nil)))

;;; macroexpansion

(defmethod macroexpand-all (form)
  (let ((sb-walker:*walk-form-expand-macros-p* t))
    (sb-walker:walk-form form)))


;;;

(defslimefun getpid ()
  (sb-unix:unix-getpid))


;;; Debugging

(defvar *sldb-stack-top*)
(defvar *sldb-restarts*)

(defmethod call-with-debugging-environment (debugger-loop-fn)
  (let* ((*sldb-stack-top* (or sb-debug:*stack-top-hint* (sb-di:top-frame)))
	 (*sldb-restarts* (compute-restarts *swank-debugger-condition*))
	 (sb-debug:*stack-top-hint* nil)
	 (*debugger-hook* nil)
	 (*readtable* (or sb-debug:*debug-readtable* *readtable*))
	 (*print-level* 4 #+nil sb-debug:*debug-print-level*)
	 (*print-length* 10 #+nil sb-debug:*debug-print-length*)
         (*print-readably* nil))
    (handler-bind ((sb-di:debug-condition 
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

(defun nth-frame (index)
  (do ((frame *sldb-stack-top* (sb-di:frame-down frame))
       (i index (1- i)))
      ((zerop i) frame)))

(defun nth-restart (index)
  (nth index *sldb-restarts*))

(defun format-frame-for-emacs (number frame)
  (print-with-frame-label 
   number (lambda (*standard-output*)
            (sb-debug::print-frame-call frame :verbosity 1 :number nil))))

(defun compute-backtrace (start end)
  "Return a list of frames starting with frame number START and
continuing to frame number END or, if END is nil, the last frame on the
stack."
  (let ((end (or end most-positive-fixnum)))
    (loop for f = (nth-frame start) then (sb-di:frame-down f)
	  for i from start below end
	  while f
	  collect (cons i f))))

(defmethod backtrace (start end)
  (loop for (n . frame) in (compute-backtrace start end)
        collect (list n (format-frame-for-emacs n frame))))

(defmethod debugger-info-for-emacs (start end)
  (list (debugger-condition-for-emacs)
	(format-restarts-for-emacs)
	(backtrace start end)))

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
                                               
(defmethod frame-source-location-for-emacs (index)
  (safe-source-location-for-emacs 
   (sb-di:frame-code-location (nth-frame index))))

#+nil
(defmethod eval-in-frame (form index)
  (sb-di:eval-in-frame (nth-frame index) string))

(defmethod frame-locals (index)
  (let* ((frame (nth-frame index))
	 (location (sb-di:frame-code-location frame))
	 (debug-function (sb-di:frame-debug-fun frame))
	 (debug-variables (sb-di::debug-fun-debug-vars debug-function)))
    (loop for v across debug-variables
          collect (list
                   :name (to-string (sb-di:debug-var-symbol v))
                   :id (sb-di:debug-var-id v)
                   :value-string
                   (if (eq (sb-di:debug-var-validity v location)
                           :valid)
                       (to-string (sb-di:debug-var-value v frame))
                       "<not-available>")))))

(defmethod frame-catch-tags (index)
  (loop for (tag . code-location) in (sb-di:frame-catches (nth-frame index))
	collect `(,tag . ,(safe-source-location-for-emacs code-location))))

(defslimefun invoke-nth-restart (index)
  (invoke-restart-interactively (nth-restart index)))

(defslimefun sldb-abort ()
  (invoke-restart (find 'abort *sldb-restarts* :key #'restart-name)))

;;;; Multiprocessing

#+SB-THREAD
(progn
  (defmethod spawn (fn &key name)
    (declare (ignore name))
    (sb-thread:make-thread fn))

  (defmethod startup-multiprocessing ()
    (setq *swank-in-background* :spawn))

  (defmethod thread-id ()
    (sb-thread:current-thread-id))

  (defmethod thread-name (thread-id)
    (format nil "Thread ~S" thread-id))

  (defmethod make-lock (&key name)
    (sb-thread:make-mutex :name name))

  (defmethod call-with-lock-held (lock function)
    (sb-thread:with-mutex (lock) (funcall function)))
)

;;; Local Variables:
;;; eval: (font-lock-add-keywords 'lisp-mode '(("(\\(defslimefun\\)\\s +\\(\\(\\w\\|\\s_\\)+\\)"  (1 font-lock-keyword-face) (2 font-lock-function-name-face))))
;;; End:
