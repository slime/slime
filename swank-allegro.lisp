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
   excl:stream-read-char-no-hang
   ))

;;;; TCP Server

(setq *swank-in-background* :spawn)

(defimplementation create-socket (host port)
  (socket:make-socket :connect :passive :local-port port 
                      :local-host host :reuse-address t))

(defimplementation local-port (socket)
  (socket:local-port socket))

(defimplementation close-socket (socket)
  (close socket))

(defimplementation accept-connection (socket)
  (socket:accept-connection socket :wait t))

(defimplementation emacs-connected ())

;;;; Unix signals

(defimplementation call-without-interrupts (fn)
  (excl:without-interrupts (funcall fn)))

(defimplementation getpid ()
  (excl::getpid))

(defimplementation lisp-implementation-type-name ()
  "allegro")

;;;; Misc

(defimplementation arglist-string (fname)
  (format-arglist fname #'excl:arglist))

(defun apropos-symbols (string &optional external-only package)
  (remove-if (lambda (sym)
               (or (keywordp sym) 
                   (and external-only
                        (not (equal (symbol-package sym) *buffer-package*))
                        (not (symbol-external-p sym)))))
             (apropos-list string package external-only t)))

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

(defimplementation macroexpand-all (form)
  (excl::walk form))

(defimplementation describe-definition (symbol-name type)
  (let ((symbol (from-string symbol-name)))
    (ecase type
      (:variable (print-description-to-string symbol))
      ((:function :generic-function)
       (print-description-to-string (symbol-function symbol)))
      (:class
       (print-description-to-string (find-class symbol))))))

;;;; Debugger

(defvar *sldb-topframe*)
(defvar *sldb-source*)
(defvar *sldb-restarts*)

(defimplementation call-with-debugging-environment (debugger-loop-fn)
  (let ((*sldb-topframe* (excl::int-newest-frame))
        (excl::*break-hook* nil)
        (*sldb-restarts*
         (compute-restarts *swank-debugger-condition*)))
    (funcall debugger-loop-fn)))

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

(defimplementation backtrace (start-frame-number end-frame-number)
  (flet ((format-frame (f i)
           (print-with-frame-label 
            i (lambda (s) (debugger:output-frame s f :moderate)))))
    (loop for i from start-frame-number
	  for f in (compute-backtrace start-frame-number end-frame-number)
	  collect (list i (format-frame f i)))))

(defimplementation debugger-info-for-emacs (start end)
  (list (debugger-condition-for-emacs)
        (format-restarts-for-emacs)
        (backtrace start end)))

(defun nth-restart (index)
  (nth index *sldb-restarts*))

(defslimefun invoke-nth-restart (index)
  (invoke-restart-interactively (nth-restart index)))

(defslimefun sldb-abort ()
  (invoke-restart (find 'abort *sldb-restarts* :key #'restart-name)))

(defimplementation frame-locals (index)
  (let ((frame (nth-frame index)))
    (loop for i from 0 below (debugger:frame-number-vars frame)
	  collect (list :name (to-string (debugger:frame-var-name frame i))
			:id 0
			:value-string 
			(to-string (debugger:frame-var-value frame i))))))

(defimplementation frame-catch-tags (index)
  (declare (ignore index))
  nil)

(defimplementation frame-source-location-for-emacs (index)
  (list :error (format nil "Cannot find source for frame: ~A"
                       (nth-frame index))))

(defimplementation eval-in-frame (form frame-number)
  (debugger:eval-form-in-context 
   form
   (debugger:environment-of-frame (nth-frame frame-number))))

(defimplementation return-from-frame (frame-number form)
  (let ((frame (nth-frame frame-number)))
    (multiple-value-call #'debugger:frame-return 
      frame (debugger:eval-form-in-context 
             (from-string form) (debugger:environment-of-frame frame)))))
                         
;;; XXX doens't work for frames with arguments 
(defimplementation restart-frame (frame-number)
  (let ((frame (nth-frame frame-number)))
    (debugger:frame-retry frame (debugger:frame-function frame))))
                          
;;;; Compiler hooks

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

(defimplementation compile-file-for-emacs (*compile-filename* load-p)
  (handler-bind ((warning #'handle-compiler-warning))
    (let ((*buffer-name* nil))
      (compile-file *compile-filename* :load-after-compile load-p))))

(defimplementation compile-string-for-emacs (string &key buffer position)
  (handler-bind ((warning #'handle-compiler-warning))
    (let ((*package* *buffer-package*)
          (*buffer-name* buffer)
          (*buffer-start-position* position)
          (*buffer-string* string))
      (eval (from-string
	     (format nil "(funcall (compile nil '(lambda () ~A)))" string))))))

;;;; Definition Finding

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

(defimplementation find-function-locations (symbol-name)
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

;;;; XREF

(defun lookup-xrefs (finder name)
  (xref-results-for-emacs (funcall finder (from-string name))))

(defimplementation who-calls (function-name)
  (lookup-xrefs (lambda (x) (xref:get-relation :calls :wild x))
                function-name))

(defimplementation who-references (variable)
  (lookup-xrefs (lambda (x) (xref:get-relation :uses :wild x))
                variable))

(defimplementation who-binds (variable)
  (lookup-xrefs (lambda (x) (xref:get-relation :binds :wild x))
                variable))

(defimplementation who-macroexpands (variable)
  (lookup-xrefs (lambda (x) (xref:get-relation :macro-calls :wild x))
                variable))

(defimplementation who-sets (variable)
  (lookup-xrefs (lambda (x) (xref:get-relation :sets :wild x))
                variable))

(defimplementation list-callers (name)
  (lookup-xrefs (lambda (x) (xref:get-relation :calls :wild x))
                name))

(defimplementation list-callees (name)
  (lookup-xrefs (lambda (x) (xref:get-relation :calls x :wild))
                name))

(defun xref-results-for-emacs (fspecs)
  (let ((xrefs '()))
    (dolist (fspec fspecs)
      (dolist (location (fspec-source-locations fspec))
        (push (cons (to-string fspec) location) xrefs)))
    (group-xrefs xrefs)))

;;;; Multiprocessing 

(defimplementation startup-multiprocessing ()
  (mp:start-scheduler))

(defimplementation spawn (fn &key name)
  (mp:process-run-function name fn))

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
    (mp:with-process-lock (mutex)
      (setf (mailbox.queue mbox)
            (nconc (mailbox.queue mbox) (list message))))))

(defimplementation receive ()
  (let* ((mbox (mailbox mp:*current-process*))
         (mutex (mailbox.mutex mbox)))
    (mp:process-wait "receive" #'mailbox.queue mbox)
    (mp:with-process-lock (mutex)
      (pop (mailbox.queue mbox)))))
