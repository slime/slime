;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; openmcl-swank.lisp --- SLIME backend for OpenMCL.
;;;
;;; Copyright (C) 2003, James Bielman  <jamesjb@jamesjb.com>
;;;
;;; This program is licensed under the terms of the Lisp Lesser GNU
;;; Public License, known as the LLGPL, and distributed with OpenMCL
;;; as the file "LICENSE".  The LLGPL consists of a preamble and the
;;; LGPL, which is distributed with OpenMCL as the file "LGPL".  Where
;;; these conflict, the preamble takes precedence.
;;;
;;; The LLGPL is also available online at
;;; http://opensource.franz.com/preamble.html

;;;
;;; This is the beginning of a Slime backend for OpenMCL.  It has been
;;; tested only with OpenMCL version 0.14-030901 on Darwin --- I would
;;; be interested in hearing the results with other versions.
;;;
;;; Additionally, reporting the positions of warnings accurately requires
;;; a small patch to the OpenMCL file compiler, which may be found at:
;;;
;;;   http://www.jamesjb.com/slime/openmcl-warning-position.diff
;;;
;;; Things that work:
;;;
;;; * Evaluation of forms with C-M-x.
;;; * Compilation of defuns with C-c C-c.
;;; * File compilation with C-c C-k.
;;; * Most of the debugger functionality, except EVAL-IN-FRAME,
;;;   FRAME-SOURCE-LOCATION, and FRAME-CATCH-TAGS.
;;; * Macroexpanding with C-c RET.
;;; * Disassembling the symbol at point with C-c M-d.
;;; * Describing symbol at point with C-c C-d.
;;; * Compiler warnings are trapped and sent to Emacs using the buffer
;;;   position of the offending top level form.
;;; * Symbol completion and apropos.
;;;
;;; Things that sort of work:
;;;
;;; * WHO-CALLS is implemented but is only able to return the file a
;;;   caller is defined in---source location information is not
;;;   available.
;;;
;;; Things that aren't done yet:
;;;
;;; * Cross-referencing.
;;; * Due to unimplementation functionality the test suite does not
;;;   run correctly (it hangs upon entering the debugger).
;;;

(in-package :swank)

(import
 '(ccl:fundamental-character-output-stream
   ccl:stream-write-char
   ccl:stream-line-length
   ccl:stream-force-output
   ccl:fundamental-character-input-stream
   ccl:stream-read-char
   ccl:stream-listen
   ccl:stream-unread-char
   ccl:stream-clear-input
   ccl:stream-line-column
   ccl:stream-line-length))

(defun without-interrupts* (body)
  (ccl:without-interrupts (funcall body)))

(defvar *swank-debugger-stack-frame* nil)

;;; TCP Server

(setq *swank-in-background* :spawn)

(defmethod create-socket (port)
  (ccl:make-socket :connect :passive :local-port port :reuse-address t))

(defmethod local-port (socket)
  (ccl:local-port socket))

(defmethod close-socket (socket)
  (close socket))

(defmethod accept-connection (socket)
  (ccl:accept-connection socket :wait t))

(defmethod spawn (fn &key name)
  (ccl:process-run-function name fn))

(defmethod emacs-connected ()
  (setq ccl::*interactive-abort-process* ccl::*current-process*))

;;;

(let ((ccl::*warn-if-redefine-kernel* nil))
  (defun ccl::force-break-in-listener (p)
    (ccl::process-interrupt
     p
     #'(lambda ()
         (ccl::ignoring-without-interrupts 
          (let ((*swank-debugger-stack-frame* nil)
                (previous-f nil))
            (block find-frame
              (map-backtrace  
               #'(lambda(frame-number p tcr lfun pc)
                   (declare (ignore frame-number tcr pc))
                   (when (eq  previous-f 'ccl::%pascal-functions%) 
                     (setq *swank-debugger-stack-frame* p)
                     (return-from find-frame))
                   (setq previous-f (ccl::lfun-name lfun)))))
      (restart-case (invoke-debugger)
        (continue () :report (lambda (stream) (write-string "Resume interrupted evaluation" stream)) t))
      ))))))

(defvar *break-in-sldb* t)

(let ((ccl::*warn-if-redefine-kernel* nil))
  (ccl::advise 
   cl::break 
   (if (and *break-in-sldb* 
            (eq ccl::*current-process* ccl::*interactive-abort-process*))
       (apply 'break-in-sldb ccl::arglist)
       (:do-it)) :when :around :name sldb-break))

(defun break-in-sldb (&optional string &rest args)
  (let ((c (make-condition 'simple-condition
                           :format-control (or string "Break")
                           :format-arguments args)))
    (let ((*swank-debugger-stack-frame* nil)
          (previous-f nil)
          (previous-f2 nil))
      (block find-frame
        (map-backtrace  
         #'(lambda(frame-number p tcr lfun pc)
             (declare (ignore frame-number tcr pc))
             (when (eq  previous-f2 'break-in-sldb) 
               (setq *swank-debugger-stack-frame* p)
               (return-from find-frame))
             (setq previous-f2 previous-f)
             (setq previous-f (ccl::lfun-name lfun)))))
      (restart-case (invoke-debugger c)
        (continue () :report (lambda (stream) (write-string "Resume interrupted evaluation" stream)) t))
      )))

;;; Evaluation

(defmethod ccl::application-error :before (application condition error-pointer)
  (declare (ignore application condition))
  (setq *swank-debugger-stack-frame* error-pointer))

(defmethod arglist-string (fname)
  (let ((*print-case* :downcase))
    (multiple-value-bind (function condition)
        (ignore-errors (values 
                        (find-symbol-designator fname *buffer-package*)))
      (when condition
        (return-from arglist-string (format nil "(-- ~A)" condition)))
      (let ((arglist (ccl:arglist function)))
        (if arglist
            (princ-to-string arglist)
            "(-- <Unknown-Function>)")))))

;;; Compilation

(defvar *buffer-offset*)
(defvar *buffer-name*)

(defun condition-source-position (condition)
  "Return the position in the source file of a compiler condition."
  (+ 1
     (or *buffer-offset* 0)
     (ccl::compiler-warning-stream-position condition)))

(defun handle-compiler-warning (condition)
  "Construct a compiler note for Emacs from a compiler warning
condition."
  (signal (make-condition
           'compiler-condition
           :original-condition condition
           :message (format nil "~A" condition)
           :severity :warning
           :location
           (let ((position (condition-source-position condition)))
             (if *buffer-name*
                 (make-location
                  (list :buffer *buffer-name*)
                  (list :position position t))
                 (make-location
                  (list :file (ccl::compiler-warning-file-name condition))
                  (list :position position t)))))))

(defun temp-file-name ()
  "Return a temporary file name to compile strings into."
  (ccl:%get-cstring (#_tmpnam (ccl:%null-ptr))))

(defmethod compile-file-for-emacs (filename load-p)
  (handler-bind ((ccl::compiler-warning #'handle-compiler-warning))
    (let ((*buffer-name* nil)
          (*buffer-offset* nil))
      (compile-file filename :load load-p))))

(defmethod compile-string-for-emacs (string &key buffer position)
  (handler-bind ((ccl::compiler-warning #'handle-compiler-warning))
    (let ((*buffer-name* buffer)
          (*buffer-offset* position)
          (*package* *buffer-package*)
          (filename (temp-file-name)))
      (unwind-protect
           (with-open-file (s filename :direction :output :if-exists :error)
             (write-string string s))
        (let ((binary-filename (compile-file filename :load t)))
          (delete-file binary-filename)))
      (delete-file filename))))

(defslimefun getpid ()
  "Return the process ID of this superior Lisp."
  (ccl::getpid))

;;; Debugging

(defvar *sldb-stack-top*)
(defvar *sldb-restarts*)

(defmethod call-with-debugging-environment (debugger-loop-fn)
  (let* ((*sldb-stack-top* nil)
         ;; This is a complete hack --- since we're not running at top level we
         ;; don't want to publish the last restart to Emacs which would allow
         ;; the user to break outside of the request loop.  What's the right
         ;; way to do this?
         (*sldb-restarts* (butlast
                           (compute-restarts *swank-debugger-condition*)))
         (*debugger-hook* nil)
         (*package* *buffer-package*))
    (funcall debugger-loop-fn)))

(defun format-restarts-for-emacs ()
  (loop for restart in *sldb-restarts*
        collect (list (princ-to-string (restart-name restart))
                      (princ-to-string restart))))

(defun map-backtrace (function &optional
                      (start-frame-number 0)
                      (end-frame-number most-positive-fixnum))
  "Call FUNCTION passing information about each stack frame
 from frames START-FRAME-NUMBER to END-FRAME-NUMBER."
  (let ((tcr (ccl::%current-tcr))
        (frame-number 0)
        (top-stack-frame (or *swank-debugger-stack-frame* 
                             (ccl::%get-frame-ptr))))
    (do* ((p top-stack-frame (ccl::parent-frame p tcr))
          (q (ccl::last-frame-ptr tcr)))
         ((or (null p) (eq p q) (ccl::%stack< q p tcr))
          (values))
      (multiple-value-bind (lfun pc) (ccl::cfp-lfun p)
        (when lfun
          (if (and (>= frame-number start-frame-number)
                   (< frame-number end-frame-number))
              (funcall function frame-number p tcr lfun pc))
          (incf frame-number))))))

(defun frame-arguments (p tcr lfun pc)
  "Returns a string representing the arguments of a frame."
  (multiple-value-bind (count vsp parent-vsp)
      (ccl::count-values-in-frame p tcr)
    (let (result)
        (dotimes (i count)
          (multiple-value-bind (var type name)
              (ccl::nth-value-in-frame p i tcr lfun pc vsp parent-vsp)
            (when name
              (cond ((equal type "required")
                     (push (to-string var) result))
                    ((equal type "optional")
                     (push (to-string var) result))
                    ((equal type "keyword")
                     (push (format nil "~S ~A" 
                                   (intern (symbol-name name) "KEYWORD")
                                   (to-string var))
                           result))))))
        (format nil "~{ ~A~}" (nreverse result)))))

(defmethod backtrace (start-frame-number end-frame-number)
  "Return a list containing a stack backtrace of the condition
currently being debugged.  The return value of this function is
unspecified unless called in the dynamic contour of a function
defined by DEFINE-DEBUGGER-HOOK.

START-FRAME-NUMBER and END-FRAME-NUMBER are zero-based indices
constraining the number of frames returned.  Frame zero is
defined as the frame which invoked the debugger.

The backtrace is returned as a list of tuples of the form
\(FRAME-NUMBER FRAME-DESCRIPTION\), where FRAME-NUMBER is the
index of the frame, defined like START-FRAME-NUMBER, and
FRAME-DESCRIPTION is a string containing a textual description
of the call at this stack frame.

An example return value:

   ((0 \"(HELLO \"world\"))
    (1 \"(RUN-EXCITING-LISP-DEMO)\")
    (2 \"(SYS::%TOPLEVEL #<SYS::ENVIRONMENT #x2930843>)\"))

If the backtrace cannot be calculated, this function returns NIL."
  (let (result)
    (map-backtrace (lambda (frame-number p tcr lfun pc)
                     (push (list frame-number
                                 (print-with-frame-label
                                  frame-number
                                  (lambda (s)
                                    (format s "(~A~A)"
                                            (ccl::%lfun-name-string lfun)
                                            (frame-arguments p tcr lfun pc)))))
                           result))
                   start-frame-number end-frame-number)
    (nreverse result)))

(defmethod debugger-info-for-emacs (start end)
  (list (debugger-condition-for-emacs)
        (format-restarts-for-emacs)
        (backtrace start end)))

(defmethod frame-locals (index)
  (map-backtrace 
   (lambda (frame-number p tcr lfun pc)
     (when (= frame-number index)
       (multiple-value-bind (count vsp parent-vsp)
           (ccl::count-values-in-frame p tcr)
         (let (result)
           (dotimes (i count)
             (multiple-value-bind (var type name)
                 (ccl::nth-value-in-frame p i tcr lfun pc vsp parent-vsp)
               (declare (ignore type))
               (when name
                 (push (list 
                        :name (to-string name)
                        :id 0
                        :value-string (to-string var))
                       result))))
           (return-from frame-locals (nreverse result))))))))

(defmethod frame-catch-tags (index)
  (declare (ignore index))
  nil)
                       
(defslimefun sldb-disassemble (the-frame-number)
  "Return a string with the disassembly of frames code."
  (let ((function-to-disassemble nil))
    (block find-frame
      (map-backtrace
       (lambda(frame-number p tcr lfun pc)
         (declare (ignore p tcr pc))
         (when (= frame-number the-frame-number)
           (setq function-to-disassemble lfun)
           (return-from find-frame)))))
    (with-output-to-string (s)
      (ccl::print-ppc-instructions 
       s (ccl::function-to-dll-header function-to-disassemble) nil))))

;;;

(defun find-source-locations (symbol name)
  (let* ((info (ccl::source-file-or-files symbol nil nil nil))
         (locations '()))
    (labels ((frob (pathname position)
               (multiple-value-bind (truename c) 
                   (ignore-errors (truename pathname))
                 (cond (c 
                        (push (list :error (princ-to-string c)) locations))
                       (t 
                        (push (make-location (list :file (namestring truename))
                                             position)
                              locations)))))
             (frob* (list position)
               (etypecase list
                 (cons (dolist (file list) (frob file position)))
                 ((or string pathname) (frob list position)))))
      (etypecase info
        (null (return-from find-source-locations
                (list
                 (list :error 
                       (format nil "No source info available for ~A" 
                               symbol)))))
        ((or string pathname) (frob info (list :function-name name)))
        (cons
         (dolist (i info)
           (typecase (car i)
             ((member method)
              (loop for (m . files) in (cdr i) 
                    do (frob* files (list :function-name name))))
             ((member function variable method-combination)
              (frob* (cdr i) (list :function-name name)))
             (t (list :error "Cannot resolve source info: ~A" info)))))))
      locations))

(defmethod find-function-locations (fname)
  (let ((symbol (from-string fname)))
    (find-source-locations symbol (symbol-name symbol))))

(defun function-source-location (symbol)
  "Return a plist containing a source location for the function
named SYMBOL."
  (car (find-source-locations symbol (string symbol))))

(defmethod frame-source-location-for-emacs (index)
  "Return to Emacs the location of the source code for the
function in a debugger frame.  In OpenMCL, we are not able to
find the precise position of the frame, but we do attempt to give
at least the filename containing it."
  (map-backtrace
   (lambda (frame-number p tcr lfun pc)
     (declare (ignore p tcr pc))
     (when (and (= frame-number index) lfun)
       (return-from frame-source-location-for-emacs
         (if (typep lfun 'ccl::method-function)
             (method-source-location lfun)
             (function-source-location (ccl:function-name lfun))))))))

;; FIXME this is still wrong since it doesn't pass back which method in the file is the one you are looking for.
(defun method-source-location (method)
  (multiple-value-bind (files name type specializers qualifiers)
      (ccl::edit-definition-p method)
    (declare (ignore type specializers qualifiers))
    (let ((file (cdr (car files))))
      `(:location
        (:file
         ,(namestring (translate-logical-pathname file)))
        (:function-name ,(string name))))))

(defun nth-restart (index)
  (nth index *sldb-restarts*))

(defslimefun invoke-nth-restart (index)
  (invoke-restart-interactively (nth-restart index)))

(defslimefun sldb-abort ()
  (invoke-restart (find 'abort *sldb-restarts* :key #'restart-name)))

(defslimefun eval-in-frame (form index)
  (map-backtrace
   (lambda (frame-number p tcr lfun pc)
     (when (= frame-number index)
       (multiple-value-bind (count vsp parent-vsp)
           (ccl::count-values-in-frame p tcr)
         (let ((bindings nil))
           (dotimes (i count)
             (multiple-value-bind (var type name)
                 (ccl::nth-value-in-frame p i tcr lfun pc vsp parent-vsp)
               (declare (ignore type))
               (when name
                 (push (list name `',var) bindings))
               ))
           (return-from eval-in-frame
             (eval `(let ,bindings
                     (Declare (ccl::ignore-if-unused 
                               ,@(mapcar 'car bindings)))
                     ,form)))
           ))))))

;;; Utilities

(defslimefun-unimplemented describe-setf-function (symbol-name))
(defslimefun-unimplemented describe-type (symbol-name))

(defslimefun describe-class (symbol-name)
  (print-description-to-string (find-class (from-string symbol-name) nil)))

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
       :setf (let ((setf-function-name (ccl::setf-function-spec-name 
                                        `(setf ,symbol))))
               (when (fboundp setf-function-name)
                 (doc 'function setf-function-name))))
      result)))

;;; XREF

(defslimefun list-callers (symbol-name)
  (let ((callers (ccl::callers (from-string symbol-name))))
    (group-xrefs 
     (mapcan (lambda (caller)
               (mapcar (lambda (loc) (cons (to-string caller) loc))
                       (typecase caller
                         (symbol
                          (find-source-locations caller (symbol-name caller)))
                         (method 
                          (let ((n (ccl::method-name caller)))
                            (find-source-locations n (symbol-name n))))
                         (t 
                          (find-source-locations caller (to-string caller))))))
             callers))))

(defslimefun-unimplemented list-callees (symbol-name))

(defslimefun-unimplemented who-calls (symbol-name))
(defslimefun-unimplemented who-references (symbol-name package-name))
(defslimefun-unimplemented who-binds (symbol-name package-name))
(defslimefun-unimplemented who-sets (symbol-name package-name))
(defslimefun-unimplemented who-macroexpands (symbol-name package-name))

;;; Macroexpansion
(defslimefun-unimplemented swank-macroexpand-all (string))

(defvar *value2tag* (make-hash-table))

(do-symbols (s (find-package 'arch))
  (if (and (> (length (symbol-name s)) 7)
	   (string= (symbol-name s) "SUBTAG-" :end1 7)
	   (boundp s)
	   (numberp (symbol-value s))
	   (< (symbol-value s) 255))
      (setf (gethash (symbol-value s) *value2tag*) s)))

(defmethod describe-primitive-type (thing)
  (let ((typecode (ccl::typecode thing)))
    (if (gethash typecode *value2tag*)
	(string (gethash typecode *value2tag*))
	(string (nth typecode '(tag-fixnum tag-list tag-misc tag-imm))))))

(defmethod inspected-parts (o)
  (let* ((i (inspector::make-inspector o))
	 (count (inspector::compute-line-count i))
	 (lines 
          (loop for l below count
                for (value label) = (multiple-value-list 
                                     (inspector::line-n i l))
                collect (cons (string-right-trim 
                               " :" (string-capitalize 
                                     (format nil "~a" label)))
                              value))))
    (values (string-left-trim
	     (string #\newline)
	     (with-output-to-string (s)
	       (let ((*print-lines* 1)
		     (*print-right-margin* 80))
		 (pprint o s))))
	    (cddr lines))))

(defslimefun inspect-in-frame (string index)
  (reset-inspector)
  (inspect-object (eval-in-frame (from-string string) index)))

;;; Multiprocessing

(defvar *known-processes* '()         ; FIXME: leakage. -luke
  "Alist (ID . PROCESS) list of processes that we have handed out IDs for.")

(defmethod spawn (fn &key name)
  (ccl:process-run-function (or name "Anonymous (Swank)") fn))

(defmethod startup-multiprocessing ()
  (setq *swank-in-background* :spawn))

(defmethod thread-id ()
  (let ((id (ccl::process-serial-number ccl:*current-process*)))
    ;; Possibly not thread-safe.
    (pushnew (cons id ccl:*current-process*) *known-processes*)
    id))

(defmethod thread-name (thread-id)
  (ccl::process-name (cdr (assq thread-id *known-processes*))))

(defmethod make-lock (&key name)
  (ccl:make-lock name))

(defmethod call-with-lock-held (lock function)
  (ccl:with-lock-grabbed (lock)
    (funcall function)))

