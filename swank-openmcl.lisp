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
;;; Things that work:
;;;
;;; * Evaluation of forms with C-M-x.
;;; * Compilation of defuns with C-c C-c.
;;; * File compilation with C-c C-k.
;;; * Basic debugger functionality, jumping to frames is not implemented yet.
;;; * Macroexpanding with C-c RET.
;;; * Disassembling the symbol at point with C-c M-d.
;;; * Describing symbol at point with C-c C-d.
;;;
;;; Things that sort of work:
;;;
;;; * Compiler warnings are trapped and sent to Emacs.  The elisp code
;;;   attempts to place the warnings on the correct defun by doing
;;;   regular expression searches.  Does not work in all cases.
;;;
;;; Things that aren't done yet:
;;;
;;; * Cross-referencing.
;;; * Symbol completion.
;;; * Apropos.
;;; * Due to unimplementation functionality the test suite does not
;;;   run correctly (it hangs upon entering the debugger).
;;;

(in-package :swank)

;;; TCP Server

;; In OpenMCL, the Swank backend runs in a separate thread and simply
;; blocks on its TCP port while waiting for forms to evaluate.

(defun create-swank-server (port &key reuse-address)
  "Create a Swank TCP server on `port'."
  (ccl:process-run-function "Swank Request Processor" #'swank-main-loop
                            port reuse-address))

(defun swank-main-loop (port reuse-address)
  "Create the TCP server and accept connections in a new thread."
  (let ((server-socket (ccl:make-socket :connect :passive :local-port port
                                        :reuse-address reuse-address)))
    (format *terminal-io* "~&;; Swank: Accepting connections on port ~D.~%"
            port)
    (loop
     (let ((socket (ccl:accept-connection server-socket :wait t)))
       (ccl:process-run-function
        (list :name (format nil "Swank Client ~D" (ccl:socket-os-fd socket))
              :initial-bindings `((*emacs-io* . ',socket)))
        #'request-loop)))))

(defun request-loop ()
  "Thread function for a single Swank connection.  Processes requests
until the remote Emacs goes away."
  (unwind-protect
       (loop
        (catch 'slime-toplevel
          (with-simple-restart (abort "Return to Slime event loop.")
            (let ((completed nil))
              (let ((*slime-output* (make-instance 'slime-output-stream)))
                (let ((condition (catch 'serve-request-catcher
                                   (read-from-emacs)
                                   (setq completed t))))
                  (unless completed
                    (when *swank-debug-p*
                      (format *debug-io*
                              "~&;; Connection to Emacs lost.~%;; [~A]~%"
                              condition))
                    (return))))))))
    (format *terminal-io* "~&;; Swank: Closed connection: ~A~%" *emacs-io*)
    (close *emacs-io*)))

;;; Redirecting Output to Emacs

;; This buffering is done via a Gray stream instead of the CMU-specific
;; stream method business...
(defclass slime-output-stream (ccl::fundamental-character-output-stream)
  ((buffer :initform (make-string-output-stream :element-type 'character)
           :accessor slime-output-stream-buffer)))

(defmethod ccl:stream-write-char ((stream slime-output-stream) char)
  (write-char char (slime-output-stream-buffer stream)))

(defmethod ccl:stream-line-column ((stream slime-output-stream))
  0)

(defmethod ccl:stream-force-output ((stream slime-output-stream))
  (send-to-emacs `(:read-output ,(get-output-stream-string
                                  (slime-output-stream-buffer stream))))
  (setf (slime-output-stream-buffer stream) (make-string-output-stream)))

;;; Evaluation

(defvar *swank-debugger-condition*)
(defvar *swank-debugger-hook*)
(defvar *swank-debugger-stack-frame*)

;;; XXX i hope this is correct for threads
(defmethod ccl::application-error :before (application condition error-pointer)
  (declare (ignore application condition))
  (setq *swank-debugger-stack-frame* error-pointer))

(defun swank-debugger-hook (condition hook)
  (let ((*swank-debugger-condition* condition)
        (*swank-debugger-hook* hook))
    (sldb-loop)))

(defslimefun-unimplemented interactive-eval-region (string))
(defslimefun-unimplemented re-evaluate-defvar (form))

(defslimefun arglist-string (fname)
  (let ((*print-case* :downcase))
    (multiple-value-bind (function condition)
        (ignore-errors (values (from-string fname)))
      (when condition
        (return-from arglist-string (format nil "(-- ~A)" condition)))
      (let ((arglist (ccl:arglist function)))
        (if arglist
            (princ-to-string arglist)
            "(-- <Unknown-Function>)")))))

;;; Compilation

(defun condition-function-name (condition)
  "Return the function name as a symbol from a compiler condition."
  (symbol-name (car (ccl::compiler-warning-function-name
                     condition))))

(defun handle-compiler-warning (condition)
  "Construct a compiler note for Emacs from a compiler warning
condition."
  (describe (car (ccl::compiler-warning-function-name condition)))
  (push (list :position nil
              :function-name (condition-function-name condition)
              :source-path nil
              :filename (ccl::compiler-warning-file-name condition)
              :severity :warning
              :message (format nil "~A" condition)
              :context nil
              :buffername nil
              :buffer-offset 0)
        *compiler-notes*))

(defmacro with-trapping-compilation-notes (() &body body)
  `(handler-bind ((ccl::compiler-warning #'handle-compiler-warning))
    ,@body))

(defslimefun swank-compile-string (string buffer start)
  (declare (ignore buffer start))
  (call-with-compilation-hooks
   (lambda ()
     (let ((*package* *buffer-package*))
       (eval (from-string
              (format nil "(funcall (compile nil '(lambda () ~A)))"
                      string)))))))

(defslimefun swank-compile-file (filename load)
  (call-with-compilation-hooks
   (lambda ()
     (compile-file filename :load load))))

;;; Debugging

(defvar *sldb-level* 0)
(defvar *sldb-stack-top*)
(defvar *sldb-restarts*)

(defslimefun ping (level)
  (cond ((= level *sldb-level*)
         *sldb-level*)
        (t
         (throw-to-toplevel))))

(defslimefun getpid ()
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

;; This is deep voodoo copied from ccl:lib/backtrace.lisp --- ideally
;; OpenMCL would provide a function for copying backtrace info into a
;; vector or something.
(defun frame-parameters (p tcr lfun pc)
  (with-output-to-string (s)
    (multiple-value-bind (count vsp parent-vsp)
        (ccl::count-values-in-frame p tcr)
      (declare (fixnum count))
      (dotimes (i count)
        (multiple-value-bind (var type name)
            (ccl::nth-value-in-frame p i tcr lfun pc vsp parent-vsp)
          (declare (ignore name type))
          (format s " ~S" var))))))

;; Also copied almost verbatim from the OpenMCL sources.
(defun compute-backtrace (start end &key (start-frame (ccl::%get-frame-ptr)))
  (let ((tcr (ccl::%current-tcr))
        (result)
        (total 0))
    (do* ((p start-frame (ccl::parent-frame p tcr))
          (frame-number 0 (1+ frame-number))
          (q (ccl::last-frame-ptr tcr)))
         ((or (null p) (eq p q) (ccl::%stack< q p tcr))
          (values))
      (declare (fixnum frame-number))
      (progn
        (multiple-value-bind (lfun pc) (ccl::cfp-lfun p)
          (incf total)
          (if (and (>= frame-number start) (< frame-number end))
              (push (list frame-number
                          (format nil "~D: (~A~A)"
                                  frame-number
                                  (if lfun
                                      (ccl::%lfun-name-string lfun)
                                      "#<Unknown Frame>")
                                  (if lfun
                                      (frame-parameters p tcr lfun pc)
                                      "")))
                    result)))))
    (values (nreverse result) total)))

(defslimefun backtrace-for-emacs (start end)
  (compute-backtrace start end :start-frame *swank-debugger-stack-frame*))

(defslimefun debugger-info-for-emacs (start end)
  (multiple-value-bind (backtrace length)
      (backtrace-for-emacs start end)
    (list (format-condition-for-emacs)
          (format-restarts-for-emacs)
          length backtrace)))

(defun nth-restart (index)
  (nth index *sldb-restarts*))

(defslimefun invoke-nth-restart (index)
  (let ((restart (nth-restart index)))
    (invoke-restart restart)))

(defslimefun sldb-continue ()
  (continue *swank-debugger-condition*))

(defslimefun sldb-abort ()
  (invoke-restart (find 'abort *sldb-restarts* :key #'restart-name)))

(defslimefun throw-to-toplevel ()
  (throw 'slime-toplevel nil))

;;; Utilities

(defslimefun-unimplemented describe-setf-function (symbol-name))
(defslimefun-unimplemented describe-type (symbol-name))

(defslimefun describe-class (symbol-name)
  (print-description-to-string (find-class (from-string symbol-name) nil)))

(defslimefun-unimplemented apropos-list-for-emacs (name &optional
                                                        external-only
                                                        package))

;;; Tracing and Disassembly

(defslimefun-unimplemented who-references (symbol-name package-name))
(defslimefun-unimplemented who-binds (symbol-name package-name))
(defslimefun-unimplemented who-sets (symbol-name package-name))
(defslimefun-unimplemented who-macroexpands (symbol-name package-name))

(defslimefun-unimplemented find-fdefinition (symbol-name package-name))
(defslimefun-unimplemented function-source-location-for-emacs (fname))

;;; Completion

(defslimefun-unimplemented completions (string default-package-name))

;;; Macroexpansion

(defslimefun-unimplemented swank-macroexpand-all (string))
