;;;; -*- Mode: lisp; indent-tabs-mode: nil; outline-regexp: ";;;;;*"; -*-
;;;
;;; swank-abcl.lisp --- Armedbear CL specific code for SLIME. 
;;;
;;; Created 2004, Andras Simon
;;;
;;; This code has been placed in the Public Domain.  All warranties
;;; are disclaimed. This code was written for "Allegro CL Trial
;;; Edition "5.0 [Linux/X86] (8/29/98 10:57)".
;;;  

(in-package :swank-backend)
(use-package :java)



(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :collect) ;just so that it doesn't spoil the flying letters
  (require :gray-streams)
  (require :pprint)
  )

(import
 '(gs:fundamental-character-output-stream
   gs:stream-write-char
   gs:stream-force-output
   gs:fundamental-character-input-stream
   gs:stream-read-char
   gs:stream-listen
   gs:stream-unread-char
   gs:stream-clear-input
   gs:stream-line-column
   gs:stream-read-char-no-hang
   ))

;;;; TCP Server


(defimplementation preferred-communication-style ()
  :spawn)



(defimplementation create-socket (host port)
  (ext:make-server-socket port))


(defimplementation local-port (socket)
  (java:jcall (java:jmethod "java.net.ServerSocket" "getLocalPort") socket))


(defimplementation close-socket (socket)
  (ext:server-socket-close socket))


(defimplementation accept-connection (socket)
  (ext:get-socket-stream (ext:socket-accept socket)))

(defimplementation emacs-connected (stream)
  (declare (ignore stream)))

;;;; Unix signals

(defimplementation call-without-interrupts (fn)
  (funcall fn))

;;there are too many to count
(defimplementation getpid ()
  0)

(defimplementation lisp-implementation-type-name ()
  "armedbear")

(defimplementation set-default-directory (directory)
  (let ((dir (sys::probe-directory directory)))
    (when dir (setf *default-pathname-defaults* dir))
    (namestring dir)))


;;;; Misc

(defimplementation arglist (symbol)
  (handler-case (sys::arglist symbol)
    (simple-error () :not-available)))

(defimplementation macroexpand-all (form)
  (macroexpand form))

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

(defimplementation describe-definition (symbol namespace)
  (ecase namespace
    (:variable 
     (describe symbol))
    ((:function :generic-function)
     (describe (symbol-function symbol)))
    (:class
     (describe (find-class symbol)))))


;;;; Debugger

(defvar *sldb-topframe*)

(defimplementation call-with-debugging-environment (debugger-loop-fn)
  (let ((*sldb-topframe* (car (ext:backtrace-as-list)) #+nil (excl::int-newest-frame)))
    (funcall debugger-loop-fn)))

(defun nth-frame (index)
  (nth index (ext:backtrace-as-list)))

(defimplementation compute-backtrace (start end)
  (let ((end (or end most-positive-fixnum)))
    (subseq (ext:backtrace-as-list) start end)))

(defimplementation print-frame (frame stream)
  (print frame stream))

#+nil
(defimplementation frame-locals (index)
  (let ((frame (nth-frame index)))
    (loop for i from 0 below (debugger:frame-number-vars frame)
	  collect (list :name (debugger:frame-var-name frame i)
			:id 0
			:value (debugger:frame-var-value frame i)))))

(defimplementation frame-catch-tags (index)
  (declare (ignore index))
  nil)

#+nil
(defimplementation disassemble-frame (index)
  (disassemble (debugger:frame-function (nth-frame index))))

(defimplementation frame-source-location-for-emacs (index)
  (list :error (format nil "Cannot find source for frame: ~A"
                       (nth-frame index))))

#+nil
(defimplementation eval-in-frame (form frame-number)
  (debugger:eval-form-in-context 
   form
   (debugger:environment-of-frame (nth-frame frame-number))))

#+nil
(defimplementation return-from-frame (frame-number form)
  (let ((frame (nth-frame frame-number)))
    (multiple-value-call #'debugger:frame-return 
      frame (debugger:eval-form-in-context 
             form 
             (debugger:environment-of-frame frame)))))
                         
;;; XXX doesn't work for frames with arguments 
#+nil
(defimplementation restart-frame (frame-number)
  (let ((frame (nth-frame frame-number)))
    (debugger:frame-retry frame (debugger:frame-function frame))))
                          
;;;; Compiler hooks

(defvar *buffer-name* nil)
(defvar *buffer-start-position*)
(defvar *buffer-string*)
(defvar *compile-filename*)

(defun handle-compiler-warning (condition)
  #+nil
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

(defimplementation swank-compile-file (*compile-filename* load-p)
  (handler-bind ((warning #'handle-compiler-warning))
                (let ((*buffer-name* nil))
                  (multiple-value-bind (fn warn fail) 
                      (compile-file *compile-filename*)
                    (when load-p (unless fail (load fn)))))))

(defimplementation swank-compile-string (string &key buffer position)
  (handler-bind ((warning #'handle-compiler-warning))
    (let ((*buffer-name* buffer)
          (*buffer-start-position* position)
          (*buffer-string* string))
      (funcall (compile nil (read-from-string
                             (format nil "(~S () ~A)" 'lambda string)))))))

#|
;;;; Definition Finding

(defun find-fspec-location (fspec type)
  (let ((file (excl::fspec-pathname fspec type)))
    (etypecase file
      (pathname
       (let ((start (scm:find-definition-in-file fspec type file)))
         (make-location (list :file (namestring (truename file)))
                        (if start
                            (list :position (1+ start))
                            (list :function-name (string fspec))))))
      ((member :top-level)
       (list :error (format nil "Defined at toplevel: ~A" fspec)))
      (null 
       (list :error (format nil "Unkown source location for ~A" fspec))))))

(defun fspec-definition-locations (fspec)
  (let ((defs (excl::find-multiple-definitions fspec)))
    (loop for (fspec type) in defs 
          collect (list fspec (find-fspec-location fspec type)))))

(defimplementation find-definitions (symbol)
  (fspec-definition-locations symbol))

|#

(defun source-location (symbol)
  (when (ext:source symbol)
    `(((,symbol)
       (:location 
        (:file ,(namestring (ext:source-pathname symbol)))
        (:position ,(ext:source-file-position symbol) t)
        (:snippet nil))))))
  

(defimplementation find-definitions (symbol)
  (source-location symbol))


#|
Should work (with a patched xref.lisp) but is it any use without find-definitions?
;;;; XREF
(setq pxref::*handle-package-forms* '(cl:in-package))

(defmacro defxref (name function)
  `(defimplementation ,name (name)
    (xref-results (,function name))))

(defxref who-calls      pxref:list-callers)
(defxref who-references pxref:list-readers)
(defxref who-binds      pxref:list-setters)
(defxref who-sets       pxref:list-setters)
(defxref list-callers   pxref:list-callers)
(defxref list-callees   pxref:list-callees)

(defun xref-results (symbols)
  (let ((xrefs '()))
    (dolist (symbol symbols)
      (push (list symbol (fspec-location symbol)) xrefs))
    xrefs))

|#

#|

;;;; Inspecting

(defmethod inspected-parts (o)
  (let* ((class (class-of o))
         (slots (clos:class-slots class)))
    (values (format nil "~A~%   is a ~A" o class)
            (mapcar (lambda (slot)
                      (let ((name (clos:slot-definition-name slot)))
                        (cons (princ-to-string name)
                              (slot-value o name))))
                    slots))))
|#
;;;; Multithreading

(defimplementation startup-multiprocessing ()
  #+nil(mp:start-scheduler))


(defimplementation spawn (fn &key name)
  (ext:make-thread (lambda () (funcall fn))))

(defimplementation thread-name (thread)
  "thread-name not implemented")

(defimplementation thread-status (thread)
  (format nil "Thread is ~[dead~;alive~]" (thread-alive-p thread)))

(defimplementation make-lock (&key name)
  (ext:make-thread-lock))

(defimplementation call-with-lock-held (lock function)
  (ext:with-thread-lock (lock) (funcall function)))

(defimplementation current-thread ()
  (ext:current-thread))

(defimplementation all-threads ()
  (copy-list (ext:mapcar-threads #'identity)))

(defimplementation interrupt-thread (thread fn)
  (ext:interrupt-thread thread fn))

(defimplementation kill-thread (thread)
  (ext:destroy-thread thread))

(defvar *mailbox-lock* (ext:make-thread-lock))

(defstruct (mailbox (:conc-name mailbox.)) 
  (mutex (ext:make-thread-lock))
  (queue '() :type list))

(defvar *thread-mailbox* (make-hash-table))


(defun mailbox (thread)
  "Return THREAD's mailbox."
  (ext:with-thread-lock (*mailbox-lock*)
    (or (gethash thread *thread-mailbox*)
        (setf (gethash thread *thread-mailbox*)
              (make-mailbox)))))

(defimplementation send (thread message)
  (let* ((mbox (mailbox thread))
         (mutex (mailbox.mutex mbox)))
    #+nil
    (mp:process-wait-with-timeout 
     "yielding before sending" 0.1
     (lambda ()
       (mp:with-process-lock (mutex)
         (< (length (mailbox.queue mbox)) 10))))
    ;(sleep 0.1)
    (ext:with-thread-lock (mutex)
      (setf (mailbox.queue mbox)
            (nconc (mailbox.queue mbox) (list message))))))

(defimplementation receive ()
  (let* ((mbox (mailbox (ext:current-thread)))
         (mutex (mailbox.mutex mbox)))
    #+nil(mp:process-wait "receive" #'mailbox.queue mbox)
    (loop until (mailbox.queue mbox) do (sleep 0.1)) ;;FIXME
    (ext:with-thread-lock (mutex)
      (pop (mailbox.queue mbox)))))



(defimplementation quit-lisp ()
  (ext:exit))
