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

(defun open-listener (port reuse-address)
  (let ((socket (make-instance 'sb-bsd-sockets:inet-socket
			       :type :stream
			       :protocol :tcp)))
    (when reuse-address
      (setf (sb-bsd-sockets:sockopt-reuse-address socket) t))
    (setf (sb-bsd-sockets:non-blocking-mode socket) t)
    (sb-bsd-sockets:socket-bind socket #(127 0 0 1) port)
    (sb-bsd-sockets:socket-listen socket 5)
    socket))

(defun accept (socket)
  "Like socket-accept, but retry on EAGAIN."
  (loop (handler-case
            (return (sb-bsd-sockets:socket-accept socket))
          (sb-bsd-sockets:interrupted-error ()))))

(defun create-swank-server (port &key reuse-address)
  "Create a SWANK TCP server."
  (let ((socket (open-listener port reuse-address)))
    (sb-sys:add-fd-handler 
     (sb-bsd-sockets:socket-file-descriptor socket)
     :input (lambda (fd) 
	      (declare (ignore fd))
	      (accept-connection socket)))
    (nth-value 1 (sb-bsd-sockets:socket-name socket))))

(defun open-stream-to-emacs ()
  (let* ((server-socket (open-listener 0 t))
         (port (nth-value 1 (sb-bsd-sockets:socket-name server-socket))))
    (unwind-protect
         (progn
           (eval-in-emacs `(slime-open-stream-to-lisp ,port))
           (let ((socket (accept server-socket)))
             (sb-bsd-sockets:socket-make-stream 
              socket :output t :element-type 'base-char)))
      (sb-bsd-sockets:socket-close server-socket))))

(defvar *use-dedicated-output-stream* t)

(defun accept-connection (server-socket)
  "Accept one Swank TCP connection on SOCKET and then close it."
  (let* ((socket (accept server-socket))
	 (stream (sb-bsd-sockets:socket-make-stream 
		  socket :input t :output t :element-type 'base-char))
         (out (if *use-dedicated-output-stream*
                  (let ((*emacs-io* stream)) (open-stream-to-emacs))
                  (make-instance 'slime-output-stream)))
         (in (make-instance 'slime-input-stream))
         (io (make-two-way-stream in out)))
    ;; we're being called from a serve-event handler: remove it now
    ;; because socket-close doesn't (in 0.8.6 anyway) do it for us
    (sb-sys:invalidate-descriptor (sb-bsd-sockets:socket-file-descriptor
                                   server-socket))
    (sb-bsd-sockets:socket-close server-socket)
    (sb-sys:add-fd-handler 
     (sb-bsd-sockets:socket-file-descriptor socket)
     :input (lambda (fd) 
	      (declare (ignore fd))
	      (serve-request stream out in io)))))


(defun serve-request (*emacs-io* *slime-output* *slime-input* *slime-io*)
  "Read and process a request from a SWANK client.
The request is read from the socket as a sexp and then evaluated."
  (catch 'slime-toplevel
    (handler-case (read-from-emacs)
      (slime-read-error (e)
        (when *swank-debug-p*
          (format *debug-io* "~&;; Connection to Emacs lost.~%;; [~A]~%" e))
        (close *emacs-io* :abort t)))))

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

(defvar *buffername*)
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
    (when (and context (not (eq condition *previous-compiler-condition*)))
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
  "Determine from CONTEXT the current compiler source location."
  (let* ((file-name (sb-c::compiler-error-context-file-name context))
         (file-pos (sb-c::compiler-error-context-file-position context))
         (source-path (current-compiler-error-source-path context)))
    (cond ((and (boundp '*buffername*) *buffername*)
           ;; account for the added lambda, replace leading
           ;; position with 0
           (make-location 
            (list :buffer *buffername*)
            (list :source-path (cons 0 (cddr source-path)) *buffer-offset*)))
          (t
           (etypecase file-name
             (pathname
              (make-location 
               (list :file (namestring (truename file-name)))
               (list :source-path source-path file-pos))))))))

(defun brief-compiler-message-for-emacs (condition error-context)
  "Briefly describe a compiler error for Emacs.
When Emacs presents the message it already has the source popped up
and the source form highlighted. This makes much of the information in
the error-context redundant."
  (declare (type sb-c::compiler-error-context error-context))
  (let ((enclosing (sb-c::compiler-error-context-enclosing-source error-context)))
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

(defmacro with-compilation-hooks (() &body body)
  `(handler-bind ((sb-c:compiler-error  #'handle-notification-condition)
                  (sb-ext:compiler-note #'handle-notification-condition)
                  (style-warning        #'handle-notification-condition)
                  (warning              #'handle-notification-condition))
    ,@body))

(defmethod compile-file-for-emacs (filename load-p)
  (with-compilation-hooks ()
    (let* ((*buffername* nil)
           (*buffer-offset* nil)
           (ret (compile-file filename)))
      (if load-p (load ret) ret))))

(defmethod compile-string-for-emacs (string &key buffer position)
  (with-compilation-hooks ()
    (let ((*package* *buffer-package*)
          (*buffername* buffer)
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
(defun function-source-location (function fname)
  "Try to find the canonical source location of FUNCTION."
  (let* ((def (sb-introspect:find-definition-source function))
         (pathname (sb-introspect:definition-source-pathname def))
         (path (sb-introspect:definition-source-form-path def))
         (position (sb-introspect:definition-source-character-offset def)))
    (unless pathname
      (return-from function-source-location
        (list :error (format nil "No filename for: ~S" fname))))
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
             (t (list :function-name fname)))))))
                                
(defmethod function-source-location-for-emacs (fname-string)
  "Return the source-location(s) of FNAME's definition(s)."
  (let* ((fname (from-string fname-string)))
    (labels ((finder (fname)
               (cond ((and (symbolp fname) (macro-function fname))
                      (function-source-location (macro-function fname) 
                                                fname-string))
                     ((typep fname 'sb-mop:generic-function)
                      (list*
                       (function-source-location fname fname-string)
                       (mapcar 
                        (lambda (x) (function-source-location x fname-string))
                        (sb-mop:generic-function-methods fname))))
                     ((functionp fname) 
                      (function-source-location fname fname-string))
                     ((sb-introspect:valid-function-name-p fname)
                      (finder (fdefinition fname))) )))
      (if *debug-definition-finding*
          (finder fname)
          (handler-case (finder fname)
            (error (e) 
              (list :error (format nil "Error: ~A" e))))))))

(defslimefun find-function-locations (name)
  (let ((loc (function-source-location-for-emacs name)))
    (if (listp loc)
        loc
        (list loc))))

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
(defun tracedp (fname)
  (gethash (sb-debug::trace-fdefinition fname)
	   sb-debug::*traced-funs*))

(defslimefun toggle-trace-fdefinition (fname-string)
  (let ((fname (from-string fname-string)))
    (cond ((tracedp fname)
	   (sb-debug::untrace-1 fname)
	   (format nil "~S is now untraced." fname))
	  (t
	   (sb-debug::trace-1 fname (sb-debug::make-trace-info))
	   (format nil "~S is now traced." fname)))))

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
	 (*print-level* nil #+nil sb-debug:*debug-print-level*)
	 (*print-length* nil #+nil sb-debug:*debug-print-length*)
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

(defun format-condition-for-emacs ()
  (format nil "~A~%   [Condition of type ~S]"
	  (ignore-errors *swank-debugger-condition*)
          (type-of *swank-debugger-condition*)))

(defun nth-frame (index)
  (do ((frame *sldb-stack-top* (sb-di:frame-down frame))
       (i index (1- i)))
      ((zerop i) frame)))

(defun nth-restart (index)
  (nth index *sldb-restarts*))

(defun format-frame-for-emacs (frame)
  (list (sb-di:frame-number frame)
	(with-output-to-string (*standard-output*) 
          (let ((*print-pretty* *sldb-pprint-frames*))
            (sb-debug::print-frame-call frame :verbosity 1 :number t)))))

(defun compute-backtrace (start end)
  "Return a list of frames starting with frame number START and
continuing to frame number END or, if END is nil, the last frame on the
stack."
  (let ((end (or end most-positive-fixnum)))
    (do ((frame *sldb-stack-top* (sb-di:frame-down frame))
	 (i 0 (1+ i)))
	((= i start)
	 (loop for f = frame then (sb-di:frame-down f)
	       for i from start below end
	       while f
	       collect f)))))

(defmethod backtrace (start end)
  (mapcar #'format-frame-for-emacs (compute-backtrace start end)))

(defmethod debugger-info-for-emacs (start end)
  (list (format-condition-for-emacs)
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

(defun source-path-file-position (path filename)
  (let ((*read-suppress* t))
    (with-open-file (file filename)
      (dolist (n path)
	(dotimes (i n)
	  (read file))
	(read-delimited-list #\( file))
      (file-position file))))

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
       ;; XXX: code-location-source-path reads the source !!
       (let ((source-path (code-location-source-path code-location))
             (position (code-location-file-position code-location)))
         (make-location 
          (list :file (namestring (truename name)))
          (list :source-path source-path position))))
      (:lisp
       (make-location
        (list :source-form (with-output-to-string (*standard-output*)
                             (sb-debug::print-code-location-source-form
                              code-location 100)))
        (list :position 0))))))

(defun safe-source-location-for-emacs (code-location)
  (handler-case (source-location-for-emacs code-location)
    (t (c) (list :error (format nil "~A" c)))))

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
                   :symbol (sb-di:debug-var-symbol v)
                   :id (sb-di:debug-var-id v)
                   :validity (sb-di:debug-var-validity v location)
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

;;; Local Variables:
;;; eval: (font-lock-add-keywords 'lisp-mode '(("(\\(defslimefun\\)\\s +\\(\\(\\w\\|\\s_\\)+\\)"  (1 font-lock-keyword-face) (2 font-lock-function-name-face))))
;;; End:
