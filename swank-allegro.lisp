;;;; -*- Mode: lisp; indent-tabs-mode: nil; outline-regexp: ";;;;;*"; -*-
;;;
;;; swank-allegro.lisp --- Allegro CL specific code for SLIME. 
;;;
;;; Created 2003
;;;
;;; This code has been placed in the Public Domain.  All warranties
;;; are disclaimed. This code was written for "Allegro CL Trial
;;; Edition "5.0 [Linux/X86] (8/29/98 10:57)".
;;;  

(in-package :swank-backend)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sock)
  (require :process)

  (import
   '(excl:fundamental-character-output-stream
     excl:stream-write-char
     excl:stream-force-output
     excl:fundamental-character-input-stream
     excl:stream-read-char
     excl:stream-listen
     excl:stream-unread-char
     excl:stream-clear-input
     excl:stream-line-column
     excl:stream-read-char-no-hang)))

;;; swank-mop

;; maybe better change MOP to ACLMOP ?  
;; CLOS also works in ACL5. --he
(import-swank-mop-symbols :clos '(:slot-definition-documentation))

(defun swank-mop:slot-definition-documentation (slot)
  (documentation slot t))

;;;; TCP Server

(defimplementation preferred-communication-style ()
  :spawn)

(defimplementation create-socket (host port)
  (socket:make-socket :connect :passive :local-port port 
                      :local-host host :reuse-address t))

(defimplementation local-port (socket)
  (socket:local-port socket))

(defimplementation close-socket (socket)
  (close socket))

(defimplementation accept-connection (socket &key external-format)
  (let ((ef (or external-format :iso-latin-1-unix))
        (s (socket:accept-connection socket :wait t)))
    (set-external-format s ef)
    s))

(defun set-external-format (stream external-format)
  #-allegro-v5.0
  (let* ((name (ecase external-format
                 (:iso-latin-1-unix :latin1)
                 (:utf-8-unix :utf-8-unix)
                 (:emacs-mule-unix :emacs-mule)))
         (ef (excl:crlf-base-ef
              (excl:find-external-format name :try-variant t))))
    (setf (stream-external-format stream) ef)))

(defimplementation format-sldb-condition (c)
  (princ-to-string c))

(defimplementation condition-references (c)
  (declare (ignore c))
  '())

(defimplementation call-with-syntax-hooks (fn)
  (funcall fn))

;;;; Unix signals

(defimplementation call-without-interrupts (fn)
  (excl:without-interrupts (funcall fn)))

(defimplementation getpid ()
  (excl::getpid))

(defimplementation lisp-implementation-type-name ()
  "allegro")

(defimplementation set-default-directory (directory)
  (let ((dir (namestring (setf *default-pathname-defaults* 
                               (truename (merge-pathnames directory))))))
    (excl:chdir dir)
    dir))

(defimplementation default-directory ()
  (namestring (excl:current-directory)))

;;;; Misc

(defimplementation arglist (symbol)
  (handler-case (excl:arglist symbol)
    (simple-error () :not-available)))

(defimplementation macroexpand-all (form)
  (excl::walk form))

(defimplementation describe-symbol-for-emacs (symbol)
  (let ((result '()))
    (flet ((doc (kind &optional (sym symbol))
             (or (documentation sym kind) :not-documented))
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
      result)))

(defimplementation describe-definition (symbol namespace)
  (ecase namespace
    (:variable 
     (describe symbol))
    ((:function :generic-function)
     (describe (symbol-function symbol)))
    (:class
     (describe (find-class symbol)))))

(defimplementation make-stream-interactive (stream)
  (setf (interactive-stream-p stream) t))

;;;; Debugger

(defvar *sldb-topframe*)

(defimplementation call-with-debugging-environment (debugger-loop-fn)
  (let ((*sldb-topframe* (find-topframe))
        (excl::*break-hook* nil))
    (funcall debugger-loop-fn)))

(defun find-topframe ()
  (let ((skip-frames 3))
    (do ((f (excl::int-newest-frame) (next-frame f))
         (i 0 (1+ i)))
        ((= i skip-frames) f))))

(defun next-frame (frame)
  (let ((next (excl::int-next-older-frame frame)))
    (cond ((not next) nil)
          ((debugger:frame-visible-p next) next)
          (t (next-frame next)))))

(defun nth-frame (index)
  (do ((frame *sldb-topframe* (next-frame frame))
       (i index (1- i)))
      ((zerop i) frame)))

(defimplementation compute-backtrace (start end)
  (let ((end (or end most-positive-fixnum)))
    (loop for f = (nth-frame start) then (next-frame f)
	  for i from start below end
	  while f
	  collect f)))

(defimplementation print-frame (frame stream)
  (debugger:output-frame stream frame :moderate))

(defimplementation frame-locals (index)
  (let ((frame (nth-frame index)))
    (loop for i from 0 below (debugger:frame-number-vars frame)
	  collect (list :name (debugger:frame-var-name frame i)
			:id 0
			:value (debugger:frame-var-value frame i)))))

(defimplementation frame-var-value (frame var)
  (let ((frame (nth-frame frame)))
    (debugger:frame-var-value frame var)))
        
(defimplementation frame-catch-tags (index)
  (declare (ignore index))
  nil)

(defimplementation disassemble-frame (index)
  (disassemble (debugger:frame-function (nth-frame index))))

(defimplementation frame-source-location-for-emacs (index)
  (let* ((frame (nth-frame index))
         (expr (debugger:frame-expression frame))
         (fspec (first expr)))
    (second (first (fspec-definition-locations fspec)))))

(defimplementation eval-in-frame (form frame-number)
  (let ((frame (nth-frame frame-number)))
    ;; let-bind lexical variables
    (let ((vars (loop for i below (debugger:frame-number-vars frame)
                      for name = (debugger:frame-var-name frame i)
                      if (symbolp name)
                      collect `(,name ',(debugger:frame-var-value frame i)))))
      (debugger:eval-form-in-context 
       `(let* ,vars ,form)
       (debugger:environment-of-frame frame)))))

(defimplementation return-from-frame (frame-number form)
  (let ((frame (nth-frame frame-number)))
    (multiple-value-call #'debugger:frame-return 
      frame (debugger:eval-form-in-context 
             form 
             (debugger:environment-of-frame frame)))))

(defimplementation restart-frame (frame-number)
  (let ((frame (nth-frame frame-number)))
    (cond ((debugger:frame-retryable-p frame)
           (apply #'debugger:frame-retry frame (debugger:frame-function frame)
                  (cdr (debugger:frame-expression frame))))
          (t "Frame is not retryable"))))

;;;; Compiler hooks

(defvar *buffer-name* nil)
(defvar *buffer-start-position*)
(defvar *buffer-string*)
(defvar *compile-filename* nil)

(defun compiler-note-p (object)
  (member (type-of object) '(excl::compiler-note compiler::compiler-note)))

(defun compiler-undefined-functions-called-warning-p (object)
  #-allegro-v5.0
  (typep object 'excl:compiler-undefined-functions-called-warning))

(deftype compiler-note ()
  `(satisfies compiler-note-p))

(defun signal-compiler-condition (&rest args)
  (signal (apply #'make-condition 'compiler-condition args)))

(defun handle-compiler-warning (condition)
  (declare (optimize (debug 3) (speed 0) (space 0)))
  (cond ((and (not *buffer-name*) 
              (compiler-undefined-functions-called-warning-p condition))
         (handle-undefined-functions-warning condition))
        (t
         (signal-compiler-condition
          :original-condition condition
          :severity (etypecase condition
                      (warning :warning)
                      (compiler-note :note))
          :message (format nil "~A" condition)
          :location (location-for-warning condition)))))

(defun location-for-warning (condition)
  (let ((loc (getf (slot-value condition 'excl::plist) :loc)))
    (cond (*buffer-name*
           (make-location 
            (list :buffer *buffer-name*)
            (list :position *buffer-start-position*)))
          (loc
           (destructuring-bind (file . pos) loc
             (make-location
              (list :file (namestring (truename file)))
              (list :position (1+ pos)))))
          (t
           (list :error "No error location available.")))))

(defun handle-undefined-functions-warning (condition)
  (let ((fargs (slot-value condition 'excl::format-arguments)))
    (loop for (fname . pos-file) in (car fargs) do
          (loop for (pos file) in pos-file do
                (signal-compiler-condition
                 :original-condition condition
                 :severity :warning
                 :message (format nil "Undefined function referenced: ~S" 
                                  fname)
                 :location (make-location (list :file file)
                                          (list :position (1+ pos))))))))

(defimplementation call-with-compilation-hooks (function)
  (handler-bind ((warning #'handle-compiler-warning)
                 ;;(compiler-note #'handle-compiler-warning)
                 )
    (funcall function)))

(defimplementation swank-compile-file (*compile-filename* load-p)
  (with-compilation-hooks ()
    (let ((*buffer-name* nil))
      (compile-file *compile-filename* :load-after-compile load-p))))

(defun call-with-temp-file (fn)
  (let ((tmpname (system:make-temp-file-name)))
    (unwind-protect
         (with-open-file (file tmpname :direction :output :if-exists :error)
           (funcall fn file tmpname))
      (delete-file tmpname))))

(defun compile-from-temp-file (string)
  (call-with-temp-file 
   (lambda (stream filename)
       (write-string string stream)
       (finish-output stream)
       (let ((binary-filename (compile-file filename :load-after-compile t)))
         (when binary-filename
           (delete-file binary-filename))))))

(defimplementation swank-compile-string (string &key buffer position directory)
  ;; We store the source buffer in excl::*source-pathname* as a string
  ;; of the form <buffername>;<start-offset>.  Quite ugly encoding, but
  ;; the fasl file is corrupted if we use some other datatype.
  (with-compilation-hooks ()
    (let ((*buffer-name* buffer)
          (*buffer-start-position* position)
          (*buffer-string* string)
          (*default-pathname-defaults*
           (if directory (merge-pathnames (pathname directory))
               *default-pathname-defaults*)))
      (compile-from-temp-file
       (format nil "~S ~S~%~A" 
               `(in-package ,(package-name *package*))
               `(eval-when (:compile-toplevel :load-toplevel)
                 (setq excl::*source-pathname*
                  ',(format nil "~A;~D" buffer position)))
               string)))))

;;;; Definition Finding

(defun fspec-primary-name (fspec)
  (etypecase fspec
    (symbol fspec)
    (list (fspec-primary-name (second fspec)))))

;; If Emacs uses DOS-style eol conventions, \n\r are considered as a
;; single character, but file-position counts them as two.  Here we do
;; our own conversion.
(defun count-cr (file pos)
  (let* ((bufsize 256)
         (type '(unsigned-byte 8))
         (buf (make-array bufsize :element-type type))
         (cr-count 0))
  (with-open-file (stream file :direction :input :element-type type)
    (loop for bytes-read = (read-sequence buf stream) do
          (incf cr-count (count (char-code #\return) buf 
                                :end (min pos bytes-read)))
          (decf pos bytes-read)
          (when (<= pos 0)
            (return cr-count))))))
              
(defun find-definition-in-file (fspec type file)
  (let* ((start (or (scm:find-definition-in-file fspec type file)
                    (scm:find-definition-in-file (fspec-primary-name fspec)
                                                 type file)))
         (pos (if start
                  (list :position (1+ (- start (count-cr file start))))
                  (list :function-name (string (fspec-primary-name fspec))))))
    (make-location (list :file (namestring (truename file)))
                   pos)))
  
(defun find-definition-in-buffer (filename)
  (let ((pos (position #\; filename :from-end t)))
    (make-location
     (list :buffer (subseq filename 0 pos))
     (list :position (parse-integer (subseq filename (1+ pos)))))))

(defun find-fspec-location (fspec type)
  (multiple-value-bind (file err) (ignore-errors (excl:source-file fspec type))
    (etypecase file
      (pathname
       (find-definition-in-file fspec type file))
      ((member :top-level)
       (list :error (format nil "Defined at toplevel: ~A"
                            (fspec->string fspec))))
      (string
       (find-definition-in-buffer file))
      (null 
       (list :error (if err
                        (princ-to-string err)
                        (format nil "Unknown source location for ~A" 
                                (fspec->string fspec)))))
      (cons 
       (destructuring-bind ((type . filename)) file
         (assert (member type '(:operator)))
         (etypecase filename
           (pathname
            (find-definition-in-file fspec type filename))
           (string 
            (find-definition-in-buffer filename))))))))

(defun fspec->string (fspec)
  (etypecase fspec
    (symbol (let ((*package* (find-package :keyword)))
              (prin1-to-string fspec)))
    (list (format nil "(~A ~A)"
                  (prin1-to-string (first fspec))
                  (let ((*package* (find-package :keyword)))
                    (prin1-to-string (second fspec)))))))

(defun fspec-definition-locations (fspec)
  (let ((defs (excl::find-multiple-definitions fspec)))
    (loop for (fspec type) in defs 
          collect (list (list type fspec)
                        (find-fspec-location fspec type)))))

(defimplementation find-definitions (symbol)
  (fspec-definition-locations symbol))

;;;; XREF

(defmacro defxref (name relation name1 name2)
  `(defimplementation ,name (x)
    (xref-result (xref:get-relation ,relation ,name1 ,name2))))

(defxref who-calls        :calls       :wild x)
(defxref calls-who        :calls       x :wild)
(defxref who-references   :uses        :wild x)
(defxref who-binds        :binds       :wild x)
(defxref who-macroexpands :macro-calls :wild x)
(defxref who-sets         :sets        :wild x)

(defun xref-result (fspecs)
  (loop for fspec in fspecs
        append (fspec-definition-locations fspec)))

;; list-callers implemented by groveling through all fbound symbols.
;; Only symbols are considered.  Functions in the constant pool are
;; searched recursively.  Closure environments are ignored at the
;; moment (constants in methods are therefore not found).

(defun map-function-constants (function fn depth)
  "Call FN with the elements of FUNCTION's constant pool."
  (do ((i 0 (1+ i))
       (max (excl::function-constant-count function)))
      ((= i max))
    (let ((c (excl::function-constant function i)))
      (cond ((and (functionp c) 
                  (not (eq c function))
                  (plusp depth))
             (map-function-constants c fn (1- depth)))
            (t
             (funcall fn c))))))

(defun in-constants-p (fun symbol)
  (map-function-constants fun 
                          (lambda (c) 
                            (when (eq c symbol) 
                              (return-from in-constants-p t)))
                          3))
 
(defun function-callers (name)
  (let ((callers '()))
    (do-all-symbols (sym)
      (when (fboundp sym)
        (let ((fn (fdefinition sym)))
          (when (in-constants-p fn name)
            (push sym callers)))))
    callers))

(defimplementation list-callers (name)
  (xref-result (function-callers name)))

(defimplementation list-callees (name)
  (let ((result '()))
    (map-function-constants (fdefinition name)
                            (lambda (c)
                              (when (fboundp c)
                                (push c result)))
                            2)
    (xref-result result)))

;;;; Inspecting

(defclass acl-inspector (inspector)
  ())

(defimplementation make-default-inspector ()
  (make-instance 'acl-inspector))

#-allegro-v5.0
(defmethod inspect-for-emacs ((f function) inspector)
  inspector
  (values "A function."
          (append
           (label-value-line "Name" (function-name f))
           `("Formals" ,(princ-to-string (arglist f)) (:newline))
           (let ((doc (documentation (excl::external-fn_symdef f) 'function)))
             (when doc
               `("Documentation:" (:newline) ,doc))))))

(defmethod inspect-for-emacs ((o t) (inspector acl-inspector))
  inspector
  (values "A value." (allegro-inspect o)))

(defmethod inspect-for-emacs ((o function) (inspector acl-inspector))
  inspector
  (values "A function." (allegro-inspect o)))

(defun allegro-inspect (o)
  (loop for (d dd) on (inspect::inspect-ctl o)
        append (frob-allegro-field-def o d)
        until (eq d dd)))

(defun frob-allegro-field-def (object def)
  (with-struct (inspect::field-def- name type access) def
    (ecase type
      ((:unsigned-word :unsigned-byte :unsigned-natural 
                       :unsigned-half-long :unsigned-3byte)
       (label-value-line name (inspect::component-ref-v object access type)))
      ((:lisp :value)
       (label-value-line name (inspect::component-ref object access)))
      (:indirect 
       (destructuring-bind (prefix count ref set) access
         (declare (ignore set prefix))
         (loop for i below (funcall count object)
               append (label-value-line (format nil "~A-~D" name i)
                                        (funcall ref object i))))))))

;;;; Multithreading

(defimplementation startup-multiprocessing ()
  (mp:start-scheduler))

(defimplementation spawn (fn &key name)
  (mp:process-run-function name fn))

(defvar *id-lock* (mp:make-process-lock :name "id lock"))
(defvar *thread-id-counter* 0)

(defimplementation thread-id (thread)
  (mp:with-process-lock (*id-lock*)
    (or (getf (mp:process-property-list thread) 'id)
        (setf (getf (mp:process-property-list thread) 'id)
              (incf *thread-id-counter*)))))

(defimplementation find-thread (id)
  (find id mp:*all-processes*
        :key (lambda (p) (getf (mp:process-property-list p) 'id))))

(defimplementation thread-name (thread)
  (mp:process-name thread))

(defimplementation thread-status (thread)
  (format nil "~A ~D" (mp:process-whostate thread)
          (mp:process-priority thread)))

(defimplementation make-lock (&key name)
  (mp:make-process-lock :name name))

(defimplementation call-with-lock-held (lock function)
  (mp:with-process-lock (lock) (funcall function)))

(defimplementation current-thread ()
  mp:*current-process*)

(defimplementation all-threads ()
  (copy-list mp:*all-processes*))

(defimplementation interrupt-thread (thread fn)
  (mp:process-interrupt thread fn))

(defimplementation kill-thread (thread)
  (mp:process-kill thread))

(defvar *mailbox-lock* (mp:make-process-lock :name "mailbox lock"))

(defstruct (mailbox (:conc-name mailbox.)) 
  (mutex (mp:make-process-lock :name "process mailbox"))
  (queue '() :type list))

(defun mailbox (thread)
  "Return THREAD's mailbox."
  (mp:with-process-lock (*mailbox-lock*)
    (or (getf (mp:process-property-list thread) 'mailbox)
        (setf (getf (mp:process-property-list thread) 'mailbox)
              (make-mailbox)))))

(defimplementation send (thread message)
  (let* ((mbox (mailbox thread))
         (mutex (mailbox.mutex mbox)))
    (mp:process-wait-with-timeout 
     "yielding before sending" 0.1
     (lambda ()
       (mp:with-process-lock (mutex)
         (< (length (mailbox.queue mbox)) 10))))
    (mp:with-process-lock (mutex)
      (setf (mailbox.queue mbox)
            (nconc (mailbox.queue mbox) (list message))))))

(defimplementation receive ()
  (let* ((mbox (mailbox mp:*current-process*))
         (mutex (mailbox.mutex mbox)))
    (mp:process-wait "receive" #'mailbox.queue mbox)
    (mp:with-process-lock (mutex)
      (pop (mailbox.queue mbox)))))

(defimplementation quit-lisp ()
  (excl:exit 0 :quiet t))


;;Trace implementations
;;In Allegro 7.0, we have:
;; (trace <name>)
;; (trace ((method <name> <qualifier>? (<specializer>+))))
;; (trace ((labels <name> <label-name>)))
;; (trace ((labels (method <name> (<specializer>+)) <label-name>)))
;; <name> can be a normal name or a (setf name)

(defimplementation toggle-trace (spec)
  (ecase (car spec)
    ((setf) 
     (toggle-trace-aux spec))
    (:defgeneric (toggle-trace-generic-function-methods (second spec)))
    ((setf :defmethod :labels :flet) 
     (toggle-trace-aux (process-fspec-for-allegro spec)))
    (:call
     (destructuring-bind (caller callee) (cdr spec)
       (toggle-trace-aux callee 
                         :inside (list (process-fspec-for-allegro caller)))))))

(defun tracedp (fspec)
  (member fspec (eval '(trace)) :test #'equal))

(defun toggle-trace-aux (fspec &rest args)
  (cond ((tracedp fspec)
         (eval `(untrace ,fspec))
         (format nil "~S is now untraced." fspec))
        (t
         (eval `(trace (,fspec ,@args)))
         (format nil "~S is now traced." fspec))))

#-allegro-v5.0
(defun toggle-trace-generic-function-methods (name)
  (let ((methods (mop:generic-function-methods (fdefinition name))))
    (cond ((tracedp name)
           (eval `(untrace ,name))
           (dolist (method methods (format nil "~S is now untraced." name))
             (excl:funtrace (mop:method-function method))))
          (t
           (eval `(trace (,name)))
           (dolist (method methods (format nil "~S is now traced." name))
             (excl:ftrace (mop:method-function method)))))))

(defun process-fspec-for-allegro (fspec)
  (cond ((consp fspec)
         (ecase (first fspec)
           ((setf) fspec)
           ((:defun :defgeneric) (second fspec))
           ((:defmethod) `(method ,@(rest fspec)))
           ((:labels) `(labels ,(process-fspec-for-allegro (second fspec))
                         ,(third fspec)))
           ((:flet) `(flet ,(process-fspec-for-allegro (second fspec)) 
                       ,(third fspec)))))
        (t
         fspec)))
