;;;; -*- Mode: lisp; indent-tabs-mode: nil; outline-regexp: ";;;;;*"; -*-
;;;
;;; swank-allegro.lisp --- Allegro CL specific code for SLIME. 
;;;
;;; Created 2003, Helmut Eller
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
(import-to-swank-mop
 '( ;; classes
   cl:standard-generic-function
   mop::standard-slot-definition
   cl:method
   cl:standard-class
   ;; standard-class readers
   mop:class-default-initargs
   mop:class-direct-default-initargs
   mop:class-direct-slots
   mop:class-direct-subclasses
   mop:class-direct-superclasses
   mop:class-finalized-p
   cl:class-name
   mop:class-precedence-list
   mop:class-prototype
   mop:class-slots
   ;; generic function readers
   mop:generic-function-argument-precedence-order
   mop:generic-function-declarations
   mop:generic-function-lambda-list
   mop:generic-function-methods
   mop:generic-function-method-class
   mop:generic-function-method-combination
   mop:generic-function-name
   ;; method readers
   mop:method-generic-function
   mop:method-function
   mop:method-lambda-list
   mop:method-specializers
   excl::method-qualifiers
   ;; slot readers
   mop:slot-definition-allocation
   mop:slot-definition-initargs
   mop:slot-definition-initform
   mop:slot-definition-initfunction
   mop:slot-definition-name
   mop:slot-definition-type
   mop:slot-definition-readers
   mop:slot-definition-writers))

(defun swank-mop:slot-definition-documentation (slot)
  (documentation slot))

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

(defimplementation accept-connection (socket)
  (socket:accept-connection socket :wait t))

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
  (excl:chdir directory)
  (namestring (setf *default-pathname-defaults* 
                    (truename (merge-pathnames directory)))))

(defimplementation default-directory ()
  (excl:chdir))

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
  (let ((*sldb-topframe* (excl::int-newest-frame))
        (excl::*break-hook* nil))
    (funcall debugger-loop-fn)))

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
  (debugger:eval-form-in-context 
   form
   (debugger:environment-of-frame (nth-frame frame-number))))

(defimplementation return-from-frame (frame-number form)
  (let ((frame (nth-frame frame-number)))
    (multiple-value-call #'debugger:frame-return 
      frame (debugger:eval-form-in-context 
             form 
             (debugger:environment-of-frame frame)))))
                         
;;; XXX doesn't work for frames with arguments 
(defimplementation restart-frame (frame-number)
  (let ((frame (nth-frame frame-number)))
    (debugger:frame-retry frame (debugger:frame-function frame))))
                          
;;;; Compiler hooks

(defvar *buffer-name* nil)
(defvar *buffer-start-position*)
(defvar *buffer-string*)
(defvar *compile-filename* nil)

(defun handle-compiler-warning (condition)
  (let ((loc (getf (slot-value condition 'excl::plist) :loc)))
    (signal (make-condition
             'compiler-condition
             :original-condition condition
             :severity :warning
             :message (format nil "~A" condition)
             :location (cond (*buffer-name*
                              (make-location 
                               (list :buffer *buffer-name*)
                               (list :position *buffer-start-position*)))
                             (loc
                              (destructuring-bind (file . pos) loc
                                (make-location
                                 (list :file (namestring (truename file)))
                                 (list :position (1+ pos)))))
                             (*compile-filename*
                              (make-location
                               (list :file *compile-filename*)
                               (list :position 1)))
                             (t
                              (list :error "No error location available.")))))))

(defimplementation call-with-compilation-hooks (function)
  (handler-bind ((warning #'handle-compiler-warning))
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
    (symbol (string fspec))
    (list (string (second fspec)))))

(defun find-fspec-location (fspec type)
  (multiple-value-bind (file err) (ignore-errors (excl:source-file fspec type))
    (etypecase file
      (pathname
       (let* ((start (scm:find-definition-in-file fspec type file))
              (pos (if start
                       (list :position (1+ start))
                       (list :function-name (fspec-primary-name fspec)))))
         (make-location (list :file (namestring (truename file)))
                        pos)))
      ((member :top-level)
       (list :error (format nil "Defined at toplevel: ~A"
                            (fspec->string fspec))))
      (string
       (let ((pos (position #\; file :from-end t)))
         (make-location
          (list :buffer (subseq file 0 pos))
          (list :position (parse-integer (subseq file (1+ pos)))))))
      (null 
       (list :error (if err
                        (princ-to-string err)
                        (format nil "Unknown source location for ~A" 
                                (fspec->string fspec))))))))

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
(defxref who-references   :uses        :wild x)
(defxref who-binds        :binds       :wild x)
(defxref who-macroexpands :macro-calls :wild x)
(defxref who-sets         :sets        :wild x)
(defxref list-callees     :calls       x :wild)

(defun xref-result (fspecs)
  (loop for fspec in fspecs
        append (fspec-definition-locations fspec)))

;; list-callers implemented by groveling through all fbound symbols.
;; Only symbols are considered.  Functions in the constant pool are
;; searched recursevly.  Closure environments are ignored at the
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

(defun in-constants-p (fn symbol)
  (map-function-constants 
   fn 
   (lambda (c) (if (eq c symbol) (return-from in-constants-p t)))
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

;;;; Inspecting

(defclass acl-inspector (inspector)
  ())

(defimplementation make-default-inspector ()
  (make-instance 'acl-inspector))

(defimplementation inspect-for-emacs ((o t) (inspector acl-inspector))
  (declare (ignore inspector))
  (values "A value."
          `("Type: " (:value ,(class-of o))
            (:newline)
            "Slots:" (:newline)
            ,@(loop
                 for slot in (clos:class-slots class)
                 for name = (clos:slot-definition-name slot)
                 collect `(:value ,name)
                 collect " = "
                 collect (if (slot-boundp o name)
                             `(:value ,(slot-value o name))
                             "#<unbound>")))))

;; duplicated from swank.lisp in order to avoid package dependencies
(defun common-seperated-spec (list &optional (callback (lambda (v) `(:value ,v))))
  (butlast
   (loop
      for i in list
      collect (funcall callback i)
      collect ", ")))

(defmethod inspect-for-emacs ((f function) (inspector acl-inspector))
  (values "A function."
          `("Name: " (:value ,(function-name f)) (:newline)
            "Its argument list is: " ,(princ-to-string (arglist f)) (:newline)
            "Documentation:" (:newline)
            ;; AllegroCL doesn't support (documentation <function-obj> t)
            ;; so we get the symbol and then its doc
            ,(documentation (excl::external-fn_symdef f) 'function))))

(defmethod inspect-for-emacs ((class structure-class) (inspector acl-inspector))
  (values "A structure class."
          `("Name: " (:value ,(class-name class))
            (:newline)
            "Super classes: " ,@(common-seperated-spec (swank-mop:class-direct-superclasses class))
            (:newline)
            "Direct Slots: " ,@(common-seperated-spec (swank-mop:class-direct-slots class)
                                                      (lambda (slot)
                                                        `(:value ,slot ,(princ-to-string
                                                                         (swank-mop:slot-definition-name slot)))))
            (:newline)
            "Effective Slots: " ,@(if (swank-mop:class-finalized-p class)
                                      (common-seperated-spec (swank-mop:class-slots class)
                                                             (lambda (slot)
                                                               `(:value ,slot ,(princ-to-string
                                                                                (swank-mop:slot-definition-name slot)))))
                                      '("N/A (class not finalized)"))
            (:newline)
            "Documentation:" (:newline)
            ,@(when (documentation class t)
                `(,(documentation class t) (:newline)))
            "Sub classes: " ,@(common-seperated-spec (swank-mop:class-direct-subclasses class)
                                                     (lambda (sub)
                                                       `(:value ,sub ,(princ-to-string (class-name sub)))))
            (:newline)
            "Precedence List: " ,@(if (swank-mop:class-finalized-p class)
                                      (common-seperated-spec (swank-mop:class-precedence-list class)
                                                             (lambda (class)
                                                               `(:value ,class ,(princ-to-string (class-name class)))))
                                      '("N/A (class not finalized)"))
            (:newline)
            "Prototype: " ,(if (swank-mop:class-finalized-p class)
                               `(:value ,(swank-mop:class-prototype class))
                               '"N/A (class not finalized)"))))

(defmethod inspect-for-emacs ((slot excl::structure-slot-definition) (inspector acl-inspector))
  (values "A structure slot." 
          `("Name: " (:value ,(mop:slot-definition-name slot))
            (:newline)
            "Documentation:" (:newline)
            ,@(when (documentation slot)
                `((:value ,(documentation slot)) (:newline)))
            "Initform: " ,(if (swank-mop:slot-definition-initform slot)
                             `(:value ,(swank-mop:slot-definition-initform slot))
                             "#<unspecified>") (:newline)
            "Type: " ,(if (swank-mop:slot-definition-type slot)
                          `(:value ,(swank-mop:slot-definition-type slot))
                          "#<unspecified>") (:newline)
            "Allocation: " (:value ,(excl::slotd-allocation slot)) (:newline)
            "Read-only: " (:value ,(excl::slotd-read-only slot)) (:newline))))

(defmethod inspect-for-emacs ((o structure-object) (inspector acl-inspector))
  (values "An structure object."
          `("Structure class: " (:value ,(class-of o))
            (:newline)
            "Slots:" (:newline)
            ,@(loop
                 with direct-slots = (swank-mop:class-direct-slots (class-of o))
                 for slot in (swank-mop:class-slots (class-of o))
                 for slot-def = (or (find-if (lambda (a)
                                               ;; find the direct slot with the same as
                                               ;; SLOT (an effective slot).
                                               (eql (swank-mop:slot-definition-name a)
                                                    (swank-mop:slot-definition-name slot)))
                                             direct-slots)
                                    slot)
                 collect `(:value ,slot-def ,(princ-to-string (swank-mop:slot-definition-name slot-def)))
                 collect " = "
                 if (slot-boundp o (swank-mop:slot-definition-name slot-def))
                   collect `(:value ,(slot-value o (swank-mop:slot-definition-name slot-def)))
                 else
                   collect "#<unbound>"
                 collect '(:newline)))))

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
