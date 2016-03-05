;;;;; -*- indent-tabs-mode: nil -*-
;;;
;;; swank-mezzano.lisp --- SLIME backend for Mezzano
;;;
;;; This code has been placed in the Public Domain.  All warranties are
;;; disclaimed.
;;;

;;; Administrivia

(defpackage swank/mezzano
  (:use cl swank/backend))

(in-package swank/mezzano)

(defclass logical-pathname () ())

;;; swank-mop

(import-swank-mop-symbols :sys.clos '(:class-default-initargs
                                      :class-direct-default-initargs
                                      :class-finalized-p
                                      :class-prototype
                                      :specializer-direct-methods
                                      :generic-function-argument-precedence-order
                                      :generic-function-declarations
                                      :generic-function-method-combination
                                      :slot-definition-documentation
                                      :slot-definition-type))

(defimplementation gray-package-name ()
  "SYS.GRAY")

;;;; TCP server

(defclass listen-socket ()
  ((%host :initarg :host)
   (%port :initarg :port)
   (%connection-fifo :initarg :connections)
   (%callback :initarg :callback)))

(defimplementation create-socket (host port &key backlog)
  (let* ((connections (mezzano.supervisor:make-fifo (or backlog 10)))
         (sock (make-instance 'listen-socket
                              :host host
                              :port port
                              :connections connections
                              :callback (lambda (conn)
                                          (when (not (mezzano.supervisor:fifo-push
                                                      (make-instance 'mezzano.network.tcp::tcp-stream :connection conn)
                                                      connections
                                                      nil))
                                            ;; Drop connections when they can't be handled.
                                            (close conn)))))
         (listen-fn (slot-value sock '%callback)))
    (when (find port mezzano.network.tcp::*server-alist*
                :key #'first)
      (error "Server already listening on port ~D" port))
    (push (list port listen-fn) mezzano.network.tcp::*server-alist*)
    sock))

(defimplementation local-port (socket)
  (slot-value socket '%port))

(defimplementation close-socket (socket)
  (setf mezzano.network.tcp::*server-alist*
        (remove (slot-value socket '%callback)
                mezzano.network.tcp::*server-alist*
                :key #'second))
  (let ((fifo (slot-value socket '%connection-fifo)))
    (loop
       (let ((conn (mezzano.supervisor:fifo-pop fifo nil)))
         (when (not conn)
           (return))
         (close conn))))
  (setf (slot-value socket '%connection-fifo) nil))

(defimplementation accept-connection (socket &key external-format
                                             buffering timeout)
  (declare (ignore external-format buffering timeout))
  (mezzano.supervisor:fifo-pop
               (slot-value socket '%connection-fifo)))

(defimplementation preferred-communication-style ()
  nil)

;;;; Unix signals
;;;; ????

(defimplementation getpid ()
  0)

;;;; Compilation

(defimplementation swank-compile-string (string &key buffer position filename
                                                policy)
  (declare (ignore buffer position filename policy))
  (eval (read-from-string (concatenate 'string "(progn " string " )")))
  t)

(defimplementation find-external-format (coding-system)
  (if (or (equal coding-system "utf-8")
          (equal coding-system "utf-8-unix"))
      :default
      nil))

;;;; Debugging

;; Definitely don't allow this.
(defimplementation install-debugger-globally (function)
  (declare (ignore function))
  nil)

(defvar *current-backtrace*)

(defimplementation call-with-debugging-environment (debugger-loop-fn)
  (let ((*current-backtrace* '()))
    (let ((prev-fp nil))
      (sys.int::map-backtrace
       (lambda (i fp)
         (push (list (1- i) fp prev-fp) *current-backtrace*)
         (setf prev-fp fp))))
    (setf *current-backtrace* (reverse *current-backtrace*))
    (funcall debugger-loop-fn)))

(defimplementation compute-backtrace (start end)
  (subseq *current-backtrace* start end))

(defimplementation print-frame (frame stream)
  (format stream "~S" (sys.int::function-from-frame frame)))

(defimplementation frame-source-location (frame-number)
  (let* ((frame (nth frame-number *current-backtrace*))
         (fn (sys.int::function-from-frame frame)))
    (function-location fn)))

(defimplementation frame-locals (frame-number)
  (let* ((frame (nth frame-number *current-backtrace*))
         (fn (sys.int::function-from-frame frame))
         (info (sys.int::function-debug-info fn))
         (result '())
         (var-id 0))
    (loop
       for (name stack-slot) in (sys.int::debug-info-local-variable-locations info)
       do
         (push (list :name name
                     :id var-id
                     :value (sys.int::read-frame-slot frame stack-slot))
               result)
         (incf var-id))
    (multiple-value-bind (env-slot env-layout)
        (sys.int::debug-info-closure-layout info)
      (when env-slot
        (let ((env-object (sys.int::read-frame-slot frame env-slot)))
          (dolist (level env-layout)
            (loop
               for i from 1
               for name in level
               when name do
                 (push (list :name name
                             :id var-id
                             :value (svref env-object i))
                       result)
                 (incf var-id))
            (setf env-object (svref env-object 0))))))
    (reverse result)))

(defimplementation frame-var-value (frame-number var-id)
  (let* ((frame (nth frame-number *current-backtrace*))
         (fn (sys.int::function-from-frame frame))
         (info (sys.int::function-debug-info fn))
         (current-var-id 0))
    (loop
       for (name stack-slot) in (sys.int::debug-info-local-variable-locations info)
       do
         (when (eql current-var-id var-id)
           (return-from frame-var-value
             (sys.int::read-frame-slot frame stack-slot)))
         (incf current-var-id))
    (multiple-value-bind (env-slot env-layout)
        (sys.int::debug-info-closure-layout info)
      (when env-slot
        (let ((env-object (sys.int::read-frame-slot frame env-slot)))
          (dolist (level env-layout)
            (loop
               for i from 1
               for name in level
               when name do
                 (when (eql current-var-id var-id)
                   (return-from frame-var-value
                     (svref env-object i)))
                 (incf current-var-id))
            (setf env-object (svref env-object 0))))))
    (error "Invalid variable id ~D for frame number ~D." var-id frame-number)))

;;;; Definition finding

(defun top-level-form-position (pathname tlf)
  (ignore-errors
    (with-open-file (s pathname)
      (loop
         repeat tlf
         do (with-standard-io-syntax
              (let ((*read-suppress* t)
                    (*read-eval* nil))
                (read s nil))))
      (make-location `(:file ,(namestring s))
                     `(:position ,(1+ (file-position s)))))))

(defun function-location (function)
  "Return a location object for FUNCTION."
  (let* ((info (sys.int::function-debug-info function))
         (pathname (sys.int::debug-info-source-pathname info))
         (tlf (sys.int::debug-info-source-top-level-form-number info)))
    (top-level-form-position pathname tlf)))

(defimplementation find-definitions (name)
  (let ((result '()))
    (labels ((frob-fn (dspec fn)
               (let ((loc (function-location fn)))
                 (when loc
                   (push (list dspec loc) result))))
             (try-fn (name)
               (when (valid-function-name-p name)
                 (when (and (fboundp name)
                            (not (and (symbolp name)
                                      (or (special-operator-p name)
                                          (macro-function name)))))
                   (let ((fn (fdefinition name)))
                     (cond ((typep fn 'sys.clos:standard-generic-function)
                            (dolist (m (sys.clos:generic-function-methods fn))
                              (frob-fn `(defmethod ,name
                                            ,@(sys.clos:method-qualifiers m)
                                          ,(mapcar #'sys.clos:class-name
                                                   (sys.clos:method-specializers m)))
                                       (sys.clos:method-function m))))
                           (t
                            (frob-fn `(defun ,name) fn)))))
                 (when (compiler-macro-function name)
                   (frob-fn `(define-compiler-macro ,name)
                            (compiler-macro-function name))))))
      (try-fn name)
      (try-fn `(setf name))
      (try-fn `(sys.int::cas name))
      (when (and (symbolp name)
                 (get name 'sys.int::setf-expander))
        (frob-fn `(define-setf-expander ,name)
                 (get name 'sys.int::setf-expander)))
      (when (and (symbolp name)
                 (macro-function name))
        (frob-fn `(defmacro ,name)
                 (macro-function name))))
    result))

;;;; XREF
;;; Simpler variants.

(defun find-all-frefs ()
  (let ((frefs (make-array 500 :adjustable t :fill-pointer 0))
        (keep-going t))
    (loop
       (when (not keep-going)
         (return))
       (adjust-array frefs (* (array-dimension frefs 0) 2))
       (setf keep-going nil
             (fill-pointer frefs) 0)
       ;; Walk the wired area looking for FREFs.
       (sys.int::walk-area
        :wired
        (lambda (object address size)
          (when (sys.int::function-reference-p object)
            (when (not (vector-push object frefs))
              (setf keep-going t))))))
    (remove-duplicates (coerce frefs 'list))))

(defimplementation list-callers (function-name)
  (let ((frefs (find-all-frefs))
        (fref-for-fn (sys.int::function-reference function-name))
        (callers '()))
    (loop
       for fref in frefs
       for fn = (sys.int::function-reference-function fref)
       when fn
       do (when (member fref-for-fn
                        (get-all-frefs-in-function fn))
            (pushnew fref callers)))
    ;; CALLERS contains all FREFs that call FUNCTION-NAME.
    ;; Convert to nice result.
    (loop
       for fref in callers
       for name = (sys.int::function-reference-name fref)
       for fn = (sys.int::function-reference-function fref)
       when fn
       collect `((defun ,name) ,(function-location fn)))))

(defun get-all-frefs-in-function (function)
  (loop
     for i below (sys.int::function-pool-size function)
     for entry = (sys.int::function-pool-object function i)
     when (sys.int::function-reference-p entry)
     collect entry
     when (compiled-function-p entry) ; closures
     append (get-all-frefs-in-function entry)))

(defimplementation list-callees (function-name)
  (let* ((fn (fdefinition function-name))
         ;; Grovel around in the function's constant pool looking for function-references.
         ;; These may be for #', but they're probably going to be for normal calls.
         ;; TODO: This doesn't work well on interpreted functions or funcallable instances.
         (callees (remove-duplicates (get-all-frefs-in-function fn))))
    (loop
       for fref in callees
       for name = (sys.int::function-reference-name fref)
       for fn = (sys.int::function-reference-function fref)
       when fn
       collect `((defun ,name) ,(function-location fn)))))

;;;; Documentation

(defimplementation arglist (name)
  (let ((macro (when (symbolp name)
                 (macro-function name)))
        (fn (if (functionp name)
                name
                (ignore-errors (fdefinition name)))))
    (cond
      (macro
       (get name 'sys.int::macro-lambda-list))
      (fn
       (cond
         ((typep fn 'sys.clos:standard-generic-function)
          (sys.clos:generic-function-lambda-list fn))
         (t
          (sys.int::debug-info-lambda-list (sys.int::function-debug-info fn)))))
      (t :not-available))))

(defimplementation type-specifier-p (symbol)
  (cond
    ((or (get symbol 'sys.int::type-expander)
         (get symbol 'sys.int::compound-type)
         (get symbol 'sys.int::type-symbol))
     t)
    (t :not-available)))

(defimplementation function-name (function)
  (sys.int::function-name function))

(defimplementation valid-function-name-p (form)
  "Is FORM syntactically valid to name a function?
   If true, FBOUNDP should not signal a type-error for FORM."
  (flet ((length=2 (list)
           (and (not (null (cdr list))) (null (cddr list)))))
    (or (symbolp form)
        (and (consp form) (length=2 form)
             (or (eq (first form) 'setf)
                 (eq (first form) 'sys.int::cas))
             (symbolp (second form))))))

(defimplementation describe-symbol-for-emacs (symbol)
  (let ((result '()))
    (when (boundp symbol)
      (setf (getf result :variable) nil))
    (when (and (fboundp symbol)
               (not (macro-function symbol)))
      (setf (getf result :function) (sys.int::debug-info-docstring (sys.int::function-debug-info (fdefinition symbol)))))
    (when (fboundp `(setf ,symbol))
      (setf (getf result :setf) (sys.int::debug-info-docstring (sys.int::function-debug-info (fdefinition `(setf ,symbol))))))
    (when (get symbol 'sys.int::setf-expander)
      (setf (getf result :setf) nil))
    (when (special-operator-p symbol)
      (setf (getf result :special-operator) nil))
    (when (macro-function symbol)
      (setf (getf result :macro) nil))
    (when (compiler-macro-function symbol)
      (setf (getf result :compiler-macro) nil))
    (when (type-specifier-p symbol)
      (setf (getf result :type) nil))
    (when (find-class symbol nil)
      (setf (getf result :class) nil))
    result))

;;;; Multithreading

(defimplementation wait-for-input (streams &optional timeout)
  (loop
       (let ((ready '()))
         (dolist (s streams)
           (when (listen s)
             (push s ready)))
         (when ready
           (return ready))
         (when (check-slime-interrupts)
           (return :interrupt))
         (when timeout
           (return '()))
         (sleep 1)
         (when (numberp timeout)
           (decf timeout 1)
           (when (not (plusp timeout))
             (return '()))))))
