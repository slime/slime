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
    (close server-socket)
    (ccl:process-run-function
     (list :name (format nil "Swank Client ~D" (ccl:socket-os-fd socket))
           :initial-bindings `((*emacs-io* . ',socket)))
     #'request-loop)))

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

;;; Redirecting Output to Emacs

;; This buffering is done via a Gray stream instead of the CMU-specific
;; stream method business...

(defclass slime-output-stream (ccl:fundamental-character-output-stream)
  ((buffer :initform (make-string 512))
   (fill-pointer :initform 0)
   (column :initform 0)))

(defmethod ccl:stream-write-char ((stream slime-output-stream) char)
  (with-slots (buffer fill-pointer column) stream
    (setf (schar buffer fill-pointer) char)
    (incf fill-pointer)
    (incf column)
    (cond ((char= #\newline char)
	   (force-output stream)
	   (setf column 0))
	  ((= fill-pointer (length buffer))
	   (force-output stream))))
  char)

(defmethod ccl:stream-line-column ((stream slime-output-stream))
  (slot-value stream 'column))

(defmethod ccl:stream-force-output ((stream slime-output-stream))
  (with-slots (buffer fill-pointer last-charpos) stream
    (let ((end fill-pointer))
      (unless (zerop end)
        (send-to-emacs `(:read-output ,(subseq buffer 0 end)))
        (setf fill-pointer 0))))
  nil)

(defun make-slime-output-stream ()
  (make-instance 'slime-output-stream))

(defclass slime-input-stream (ccl:fundamental-character-input-stream)
  ((buffer :initform "") (index :initform 0)))

(defmethod ccl:stream-read-char ((s slime-input-stream))
  (with-slots (buffer index) s
    (when (= index (length buffer))
      (setf buffer (slime-read-string))
      (setf index 0))
    (assert (plusp (length buffer)))
    (prog1 (aref buffer index) (incf index))))

(defmethod ccl:stream-listen ((s slime-input-stream))
  (with-slots (buffer index) s
    (< index (length buffer))))

(defmethod ccl:stream-unread-char ((s slime-input-stream) char)
  (with-slots (buffer index) s
    (setf (aref buffer (decf index)) char))
  nil)

(defmethod ccl:stream-clear-input ((s slime-input-stream))
  (with-slots (buffer index) s 
    (setf buffer ""  
	  index 0))
  nil)

(defmethod ccl:stream-line-column ((s slime-input-stream))
  nil)

;;; Evaluation

(defvar *swank-debugger-stack-frame*)

(defmethod ccl::application-error :before (application condition error-pointer)
  (declare (ignore application condition))
  (setq *swank-debugger-stack-frame* error-pointer))

(defslimefun arglist-string (fname)
  "Return the lambda list for function FNAME as a string."
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

(defun condition-source-position (condition)
  "Return the position in the source file of a compiler condition."
  (+ 1 *buffer-offset* (ccl::compiler-warning-stream-position condition)))

(defun handle-compiler-warning (condition)
  "Construct a compiler note for Emacs from a compiler warning
condition."
  (push (list :message (format nil "~A" condition)
              :severity :warning
              :location
              (list :file 
                    (ccl::compiler-warning-file-name condition)
                    (condition-source-position condition)))
        *compiler-notes*)
  (muffle-warning condition))

(defun call-trapping-compilation-notes (fn)
  "Call FN trapping compiler notes and storing them in the notes database."
  (handler-bind ((ccl::compiler-warning #'handle-compiler-warning))
    (funcall fn)))

(defun temp-file-name ()
  "Return a temporary file name to compile strings into."
  (ccl:%get-cstring (#_tmpnam (ccl:%null-ptr))))

(defslimefun swank-compile-string (string buffer start)
  "Compile STRING, using BUFFER and START as information for
reporting back the location of compiler notes.  In OpenMCL we
have to use the file compiler to get compiler warning positions,
so we write the string to a temporary file and compile it."
  (declare (ignore buffer))
  (let ((*buffer-offset* start)
        (*package* *buffer-package*)
        (filename (temp-file-name)))
    (call-with-compilation-hooks
     (lambda ()
       (unwind-protect
            (progn
              (with-open-file (s filename :direction :output :if-exists :error)
                (write-string string s))
              (let ((binary-filename (compile-file filename :load t)))
                (delete-file binary-filename)))
         (delete-file filename))))))

(defslimefun swank-compile-file (filename load)
  "Compile and optionally load FILENAME, trapping compiler notes for Emacs."
  (let ((*buffer-offset* 0))
    (call-with-compilation-hooks
     (lambda ()
       (compile-file filename :load load)))))

;;; Debugging

(defvar *sldb-level* 0)
(defvar *sldb-stack-top*)
(defvar *sldb-restarts*)

(defslimefun getpid ()
  "Return the process ID of this superior Lisp."
  (ccl::getpid))

(defslimefun sldb-loop ()
  (let* ((*sldb-level* (1+ *sldb-level*))
         (*sldb-stack-top* nil)
         ;; This is a complete hack --- since we're not running at top level we
         ;; don't want to publish the last restart to Emacs which would allow
         ;; the user to break outside of the request loop.  What's the right
         ;; way to do this?
         (*sldb-restarts* (butlast
                           (compute-restarts *swank-debugger-condition*)))
         (*debugger-hook* nil)
         (level *sldb-level*)
         (*package* *buffer-package*))
    (send-to-emacs (list* :debug *sldb-level* (debugger-info-for-emacs 0 1)))
    (unwind-protect
         (loop
          (catch 'sldb-loop-catcher
            (with-simple-restart (abort "Return to sldb level ~D." level)
              (read-from-emacs))))
      (send-to-emacs `(:debug-return ,level)))))

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

(defslimefun backtrace-for-emacs (&optional
                  (start-frame-number 0)
                  (end-frame-number most-positive-fixnum))
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

(defslimefun debugger-info-for-emacs (start end)
  (list (format-condition-for-emacs)
        (format-restarts-for-emacs)
        (backtrace-for-emacs start end)))

(defslimefun frame-locals (index)
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

(defslimefun frame-catch-tags (index)
  (declare (ignore index))
  nil)

(defun function-source-location (symbol)
  "Return a plist containing a source location for the function
named SYMBOL."
  (let ((source-info (ccl::%source-files symbol)))
    ;; This is not entirely correct---%SOURCE-FILES can apparently
    ;; return a list under some circumstances...
    (when (and source-info (atom source-info))
      (let ((filename (namestring (truename source-info))))
        (list :openmcl filename (symbol-name symbol))))))

(defslimefun frame-source-location-for-emacs (index)
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

(defun briefly-describe-symbol-for-emacs (symbol)
  "Return a plist describing SYMBOL.
Return NIL if the symbol is unbound."
  (let ((result '()))
    (labels ((first-line (string) 
               (let ((pos (position #\newline string)))
                 (if (null pos) string (subseq string 0 pos))))
             (doc (kind &optional (sym symbol))
               (let ((string (documentation sym kind)))
                 (if string 
                     (first-line string)
                     :not-documented)))
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
;;       (maybe-push
;;        :type (if (ext:info type kind symbol)
;;                  (doc 'type)))
      (maybe-push
       :class (if (find-class symbol nil) 
                  (doc 'class)))
      (if result
          (list* :designator (to-string symbol) result)))))

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

;;; Macroexpansion

(defslimefun-unimplemented swank-macroexpand-all (string))
