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

(defimplementation frame-locals (frame-number)
  (let* ((frame (nth frame-number *current-backtrace*))
         (fn (sys.int::function-from-frame frame))
         (info (sys.int::function-debug-info fn))
         (result '())
         (var-id 0))
    (when (and (listp info) (eql (first info) :debug-info))
      (loop
         for (name stack-slot) in (third info)
         do
           (push (list :name name
                       :id var-id
                       :value (sys.int::read-frame-slot frame stack-slot))
                 result)
           (incf var-id))
      (when (fourth info)
        (let ((env-object (sys.int::read-frame-slot frame (fourth info))))
          (dolist (level (fifth info))
            (do ((i 1 (1+ i))
                 (var level (cdr var)))
                ((null var))
              (when (car var)
                (push (list :name (car var)
                            :id var-id
                            :value (svref env-object i))
                      result)
                (incf var-id)))
            (setf env-object (svref env-object 0))))))
    (format *terminal-io* "Frame locals: ~S~%" result)
    (reverse result)))

(defimplementation frame-var-value (frame-number var-id)
  (let* ((frame (nth frame-number *current-backtrace*))
         (fn (sys.int::function-from-frame frame))
         (info (sys.int::function-debug-info fn))
         (current-var-id 0))
    (when (and (listp info) (eql (first info) :debug-info))
      (loop
         for (name stack-slot) in (third info)
         do
           (when (eql current-var-id var-id)
             (return-from frame-var-value
               (sys.int::read-frame-slot frame stack-slot)))
           (incf current-var-id))
      (when (fourth info)
        (let ((env-object (sys.int::read-frame-slot frame (fourth info))))
          (dolist (level (fifth info))
            (do ((i 1 (1+ i))
                 (var level (cdr var)))
                ((null var))
              (when (car var)
                (when (eql current-var-id var-id)
                  (return-from frame-var-value
                    (svref env-object i)))
                (incf current-var-id)))
            (setf env-object (svref env-object 0))))))
    (error "Invalid variable id ~D for frame number ~D." var-id frame-number)))

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
       (let ((info (sys.int::function-debug-info fn)))
         (eighth info)))
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

(defimplementation describe-symbol-for-emacs (symbol)
  (let ((result '()))
    (when (boundp symbol)
      (setf (getf result :variable) nil))
    (when (and (fboundp symbol)
               (not (macro-function symbol)))
      (setf (getf result :function) (ninth (sys.int::function-debug-info (fdefinition symbol)))))
    (when (fboundp `(setf ,symbol))
      (setf (getf result :setf) (ninth (sys.int::function-debug-info (fdefinition `(setf ,symbol))))))
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
