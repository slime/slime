;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; swank-allegro.lisp --- Allegro CL specific code for SLIME. 
;;;
;;; Created 2003, Helmut Eller
;;;
;;; This code has been placed in the Public Domain.  All warranties
;;; are disclaimed.
;;;
;;;   $Id$
;;;
;;; This code was written for 
;;;   Allegro CL Trial Edition "5.0 [Linux/X86] (8/29/98 10:57)"
;;;  

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sock)
  (require :process))

(in-package :swank)

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
   ))

(defun without-interrupts* (body)
  (excl:without-interrupts (funcall body)))

;;; TCP Server

(defun create-swank-server (port &key (reuse-address t))
  "Create a Swank TCP server on `port'.
Return the port number that the socket is actually listening on."
  (let ((server-socket (socket:make-socket :connect :passive :local-port port
                                           :reuse-address reuse-address)))
    (mp:process-run-function "Swank Request Processor"
                              #'swank-accept-connection
                              server-socket)
    (socket:local-port server-socket)))

(defun swank-accept-connection (server-socket)
  "Accept one Swank TCP connection on SOCKET.
Run the connection handler in a new thread."
  (loop
   (let ((socket (socket:accept-connection server-socket :wait t)))
     (mp:process-run-function
      (list :name (format nil "Swank Client ~D" (socket:socket-os-fd socket))
            :initial-bindings `((*emacs-io* . ',socket)))
      #'request-loop))))

(defun request-loop ()
  "Thread function for a single Swank connection.  Processes requests
until the remote Emacs goes away."
  (unwind-protect
       (let* ((*slime-output* (make-instance 'slime-output-stream))
              (*slime-input* (make-instance 'slime-input-stream))
              (*slime-io* (make-two-way-stream *slime-input* *slime-output*)))
         (loop
            (catch 'slime-toplevel
              (with-simple-restart (abort "Return to Slime event loop.")
                (handler-case (read-from-emacs)
                  (slime-read-error (e)
                    (when *swank-debug-p*
                      (format *debug-io*
                              "~&;; Connection to Emacs lost.~%;; [~A]~%" e))
                    (return)))))))
    (format *terminal-io* "~&;; Swank: Closed connection: ~A~%" *emacs-io*)
    (close *emacs-io*)))

(defmethod arglist-string (fname)
  (declare (type string fname))
  (multiple-value-bind (function condition)
      (ignore-errors (values (from-string fname)))
    (when condition
      (return-from arglist-string (format nil "(-- ~A)" condition)))
    (multiple-value-bind (arglist condition) 
        (ignore-errors (values (excl:arglist function)))
      (cond (condition (format  nil "(-- ~A)" condition))
            (t (format nil "(~{~A~^ ~})" arglist))))))

(defslimefun getpid ()
  (excl::getpid))

(defun apropos-symbols (string &optional external-only package)
  (remove-if (lambda (sym)
               (or (keywordp sym) 
                   (and external-only
                        (not (equal (symbol-package sym) *buffer-package*))
                        (not (symbol-external-p sym)))))
             (apropos-list string package external-only t)))

(defmethod describe-symbol-for-emacs (symbol)
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

(defvar *sldb-topframe*)
(defvar *sldb-source*)
(defvar *sldb-restarts*)
	
(defmethod call-with-debugging-environment (debugger-loop-fn)
  (flet ((break-hook (frame source continue-format-string args condition)
	   (let ((*sldb-topframe* frame))
	     (funcall debugger-loop-fn))))
    (let* ((*debugger-hook* nil)
	   (*package* *buffer-package*)
	   (*sldb-restarts* (compute-restarts *swank-debugger-condition*))
           (*print-pretty* nil)
           (*print-readably* nil)
           (*print-level* 3)
           (*print-length* 10)
	   (excl::*break-hook* #'break-hook))
      (break))))

(defun format-condition-for-emacs ()
  (format nil "~A~%   [Condition of type ~S]"
          *swank-debugger-condition* (type-of *swank-debugger-condition*)))

(defun format-restarts-for-emacs ()
  (loop for restart in *sldb-restarts*
        collect (list (princ-to-string (restart-name restart))
                      (princ-to-string restart))))

(defun nth-frame (index)
  (do ((frame *sldb-topframe* (excl::int-next-older-frame frame))
       (i index (1- i)))
      ((zerop i) frame)))

(defun compute-backtrace (start end)
  (let ((end (or end most-positive-fixnum)))
    (loop for f = (nth-frame start) then (excl::int-next-older-frame f)
	  for i from start below end
	  while f
	  collect f)))

(defmethod backtrace (start-frame-number end-frame-number)
  (flet ((format-frame (f i)
	   (with-output-to-string (stream)
	     (let ((*print-pretty* *sldb-pprint-frames*))
	       (format stream "~D: " i)
	       (debugger:output-frame stream f :moderate)))))
    (loop for i from start-frame-number
	  for f in (compute-backtrace start-frame-number end-frame-number)
	  collect (list i (format-frame f i)))))

(defmethod debugger-info-for-emacs (start end)
  (list (format-condition-for-emacs)
        (format-restarts-for-emacs)
        (backtrace start end)))

(defun nth-restart (index)
  (nth index *sldb-restarts*))

(defslimefun invoke-nth-restart (index)
  (let ((restart (nth-restart index)))
    (invoke-restart restart)))

(defmethod frame-locals (index)
  (let ((frame (nth-frame index)))
    (loop for i from 0 below (debugger:frame-number-vars frame)
	  collect (list :symbol (debugger:frame-var-name frame i)
			:id 0
			:value-string 
			(to-string (debugger:frame-var-value frame i))))))

(defmethod frame-catch-tags (index)
  (declare (ignore index))
  nil)

(defvar *buffer-name* nil)
(defvar *buffer-start-position*)
(defvar *buffer-string*)
(defvar *compile-filename*)

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
                             (t  
                              (make-location
                               (list :file *compile-filename*)
                               (list :position 1))))))))

(defmethod compile-file-for-emacs (*compile-filename* load-p)
  (handler-bind ((warning #'handle-compiler-warning))
    (let ((*buffer-name* nil))
      (compile-file *compile-filename* :load load-p))))

(defmethod compile-string-for-emacs (string &key buffer position)
  (handler-bind ((warning #'handle-compiler-warning))
    (let ((*package* *buffer-package*)
          (*buffer-name* buffer)
          (*buffer-start-position* position)
          (*buffer-string* string))
      (eval (from-string
	     (format nil "(funcall (compile nil '(lambda () ~A)))" string))))))

(defun fspec-source-locations (fspec)
  (let ((defs (excl::find-multiple-definitions fspec)))
    (let ((locations '()))
      (loop for (fspec type) in defs do
            (let ((file (excl::fspec-pathname fspec type)))
              (etypecase file
                (pathname
                 (let ((start (scm:find-definition-in-file fspec type file)))
                   (push (make-location 
                          (list :file (namestring (truename file)))
                          (if start
                              (list :position (1+ start))
                              (list :function-name (string fspec))))
                         locations)))
                ((member :top-level)
                 (push (list :error (format nil "Defined at toplevel: ~A" 
                                            fspec))
                       locations))
                (null 
                 (push (list :error (format nil 
                                            "Unkown source location for ~A" 
                                            fspec))
                       locations))
                )))
      locations)))

(defslimefun find-function-locations (symbol-name)
  (multiple-value-bind (symbol foundp) (find-symbol-designator symbol-name)
    (cond ((not foundp)
           (list (list :error (format nil "Unkown symbol: ~A" symbol-name))))
          ((macro-function symbol)
           (fspec-source-locations symbol))
          ((special-operator-p symbol)
           (list (list :error (format nil "~A is a special-operator" symbol))))
          ((fboundp symbol)
           (fspec-source-locations symbol))
          (t (list (list :error
                         (format nil "Symbol not fbound: ~A" symbol-name))))
          )))

(defun lookup-xrefs (finder name)
  (xref-results-for-emacs (funcall finder (from-string name))))

(defslimefun who-calls (function-name)
  (lookup-xrefs (lambda (x) (xref:get-relation :calls :wild x))
                function-name))

(defslimefun who-references (variable)
  (lookup-xrefs (lambda (x) (xref:get-relation :uses :wild x))
                variable))

(defslimefun who-binds (variable)
  (lookup-xrefs (lambda (x) (xref:get-relation :binds :wild x))
                variable))

(defslimefun who-sets (variable)
  (lookup-xrefs (lambda (x) (xref:get-relation :sets :wild x))
                variable))

(defslimefun list-callers (name)
  (lookup-xrefs (lambda (x) (xref:get-relation :calls :wild x))
                name))

(defslimefun list-callees (name)
  (lookup-xrefs (lambda (x) (xref:get-relation :calls x :wild))
                name))

(defun xref-results-for-emacs (fspecs)
  (let ((xrefs '()))
    (dolist (fspec fspecs)
      (loop for location in (fspec-source-locations fspec) 
            do (push (cons (to-string fspec) location)
                     xrefs)))
    (group-xrefs xrefs)))
