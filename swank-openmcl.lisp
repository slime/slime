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
;;;   $Id$
;;;

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

;;; TCP Server

;; In OpenMCL, the Swank backend runs in a separate thread and simply
;; blocks on its TCP port while waiting for forms to evaluate.

(defun create-swank-server (port &key reuse-address)
  "Create a Swank TCP server on `port'.
Return the port number that the socket is actually listening on."
  (let ((server-socket (ccl:make-socket :connect :passive :local-port port
                                        :reuse-address reuse-address)))
    (ccl:process-run-function "Swank Request Processor"
                              #'swank-accept-connection
                              server-socket)
    (ccl:local-port server-socket)))

(defun swank-accept-connection (server-socket)
  "Accept one Swank TCP connection on SOCKET and then close it.
Run the connection handler in a new thread."
  (let ((socket (ccl:accept-connection server-socket :wait t)))
    (request-loop socket)))

(defun request-loop (*emacs-io*)
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

;;; Evaluation

(defvar *swank-debugger-stack-frame*)

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

(defun format-condition-for-emacs ()
  (format nil "~A~%   [Condition of type ~S]"
          *swank-debugger-condition* (type-of *swank-debugger-condition*)))

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
    (map-backtrace #'(lambda (frame-number p tcr lfun pc)
                       (push (list frame-number
                                   (format nil "~D: (~A~A)" frame-number
                                           (ccl::%lfun-name-string lfun)
                                           (frame-arguments p tcr lfun pc)))
                            result))
                   start-frame-number end-frame-number)
    (nreverse result)))

(defmethod debugger-info-for-emacs (start end)
  (list (format-condition-for-emacs)
        (format-restarts-for-emacs)
        (backtrace start end)))

(defmethod frame-locals (index)
  (map-backtrace 
   #'(lambda (frame-number p tcr lfun pc)
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
                          :symbol (to-string name)
                          :id 0
                          :validity :valid
                          :value-string (to-string var))
                         result))))
             (return-from frame-locals (nreverse result))))))))

(defmethod frame-catch-tags (index)
  (declare (ignore index))
  nil)

(defun source-info-first-file-name (info)
  (etypecase info
    ((or pathname string) (namestring (truename info)))
    (cons 
     (etypecase (car info)
       (cons (source-info-first-file-name (car info)))
       (standard-method (source-info-first-file-name (cdr info)))
       ((member function) (source-info-first-file-name (cdr info)))
       ((member method) (source-info-first-file-name (cdr info)))
       ((or pathname string) (namestring (truename (car info))))))))

(defun function-source-location (symbol)
  "Return a plist containing a source location for the function
named SYMBOL."
  (let ((source-info (ccl::source-file-or-files symbol nil nil nil)))
    (if source-info
        (make-location
         (list :file (source-info-first-file-name source-info))
         (list :function-name (symbol-name symbol)))
        (list :error (format nil "No source infor for ~S"  symbol)))))

(defmethod frame-source-location-for-emacs (index)
  "Return to Emacs the location of the source code for the
function in a debugger frame.  In OpenMCL, we are not able to
find the precise position of the frame, but we do attempt to give
at least the filename containing it."
  (map-backtrace
   #'(lambda (frame-number p tcr lfun pc)
       (declare (ignore p tcr pc))
       (when (and (= frame-number index) lfun)
         (return-from frame-source-location-for-emacs
           (function-source-location (ccl:function-name lfun)))))))

(defun nth-restart (index)
  (nth index *sldb-restarts*))

(defslimefun invoke-nth-restart (index)
  (let ((restart (nth-restart index)))
    (invoke-restart restart)))

(defslimefun sldb-abort ()
  (invoke-restart (find 'abort *sldb-restarts* :key #'restart-name)))

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

;;; Tracing and Disassembly

(defslimefun who-calls (symbol-name)
  (let ((callers (ccl::callers symbol-name))
        (result (make-hash-table :test 'equalp))
        (list nil))
    (dolist (caller callers)
      (let ((source-info (ccl::%source-files caller)))
        (when (and source-info (atom source-info))
          (let ((filename (namestring (truename source-info)))
                ;; This is clearly not the real source path but it will
                ;; get us into the file at least...
                (source-path '(0)))
            (push (list (string caller) source-path)
                        (gethash filename result))))))
    (maphash #'(lambda (k v)
                 (push (cons k (list v)) list))
             result)
    list))

(defslimefun-unimplemented who-references (symbol-name package-name))
(defslimefun-unimplemented who-binds (symbol-name package-name))
(defslimefun-unimplemented who-sets (symbol-name package-name))
(defslimefun-unimplemented who-macroexpands (symbol-name package-name))

(defslimefun-unimplemented find-fdefinition (symbol-name package-name))

(defslimefun function-source-location-for-emacs (fname)
  "Return a source position of the definition of FNAME.  The
precise location of the definition is not available, but we are
able to return the file name in which the definition occurs."
  (function-source-location (from-string fname)))

(defslimefun find-function-locations (fname)
  (let* ((symbol (from-string fname))
         (symbol-name (string symbol))
         (info (ccl::source-file-or-files symbol nil nil nil))
         (locations '()))
    (labels ((frob (pathname position)
               (multiple-value-bind (truename c) (truename pathname)
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
        (null (return-from find-function-locations
                (list
                 (list :error 
                       (format nil "No source info available for ~A" fname)))))
        ((or string pathname) (frob info (list :function-name fname)))
        (cons
         (dolist (i info)
           (etypecase (car i)
             ((member method)
              (loop for (m . files) in (cdr i) 
                    do (frob* files 
                              (list :function-name symbol-name))))
             ((member function) 
              (frob* (cdr i) 
                     (list :function-name fname))))))))
      locations))

;;; Macroexpansion
(defslimefun-unimplemented swank-macroexpand-all (string))
