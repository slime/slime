;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; swank.lisp --- the portable bits
;;;
;;; Created 2003, Daniel Barlow <dan@metacircles.com>
;;;
;;; This code has been placed in the Public Domain.  All warranties are 
;;; disclaimed.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defpackage :swank
    (:use :common-lisp)
    (:nicknames "SWANK-IMPL")
    (:export #:start-server)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar swank::*sysdep-pathname*
    (merge-pathnames (or #+cmu "swank-cmucl" 
			 #+(and sbcl sb-thread) "swank-sbcl" 
			 #+openmcl "swank-openmcl")
		     (or *compile-file-pathname* *load-pathname* 
			 *default-pathname-defaults*))))

(in-package :swank)
(defvar *swank-io-package*
  (let ((package (make-package "SWANK-IO-PACKAGE")))
    (import '(nil t quote) package)
    package))

(declaim (optimize (debug 3)))

(defconstant server-port 4005
  "Default port for the Swank TCP server.")

(defvar *swank-debug-p* t
  "When true, print extra debugging information.")

;;; Setup and Hooks

(defun start-server (&optional (port server-port))
  "Start the Slime backend on TCP port `port'."
  (swank-impl:create-swank-server port :reuse-address t)
  #+xref (setf c:*record-xref-info* t)
  (when *swank-debug-p*
    (format *debug-io* "~&;; Swank ready.~%")))

;;; IO to emacs

(defvar *emacs-io* nil
  "Bound to a TCP stream to Emacs during request processing.")

(defvar *slime-output* nil
  "Bound to a slime-output-stream during request processing.")

(defparameter *redirect-output* t)

(defun read-from-emacs ()
  "Read and process a request from Emacs."
  (let ((form (read-next-form)))
    (if *redirect-output*
	(let ((*standard-output* *slime-output*)
	      (*error-output* *slime-output*)
	      (*trace-output* *slime-output*)
	      (*debug-io*  *slime-output*)
	      (*query-io* *slime-output*))
	  (apply #'funcall form))
	(apply #'funcall form))))

(defun read-next-form ()
  "Read the next Slime request from *EMACS-IO* and return an
S-expression to be evaulated to handle the request.  If an error
occurs during parsing, it will be noted and control will be tranferred
back to the main request handling loop."
  (handler-case
      (let* ((length (logior (ash (read-byte *emacs-io*) 16)
                             (ash (read-byte *emacs-io*) 8)
                             (read-byte *emacs-io*)))
             (string (make-string length)))
        (read-sequence string *emacs-io*)
        (read-form string))
    (condition (c)
      (throw 'serve-request-catcher c))))

(defun read-form (string)
  (with-standard-io-syntax
    (let ((*package* *swank-io-package*))
      (read-from-string string))))

(defun send-to-emacs (object)
  "Send `object' to Emacs."
  (let* ((string (prin1-to-string-for-emacs object))
         (length (1+ (length string))))
    (loop for position from 16 downto 0 by 8
          do (write-byte (ldb (byte 8 position) length) *emacs-io*))
    (write-string string *emacs-io*)
    (terpri *emacs-io*)
    (force-output *emacs-io*)))

(defun prin1-to-string-for-emacs (object)
  (let ((*print-case* :downcase)
        (*print-readably* nil)
        (*print-pretty* nil)
        (*package* *swank-io-package*))
    (prin1-to-string object)))

;;; The Reader

(defvar *buffer-package*)
(setf (documentation '*buffer-package* 'symbol)
      "Package corresponding to slime-buffer-package.  

EVAL-STRING binds *buffer-package*.  Strings originating from a slime
buffer are best read in this package.  See also FROM-STRING and TO-STRING.")


(defun from-string (string)
  "Read string in the *BUFFER-PACKAGE*"
  (let ((*package* *buffer-package*))
    (read-from-string string)))

(defun to-string (string)
  "Write string in the *BUFFER-PACKAGE*."
  (let ((*package* *buffer-package*))
    (prin1-to-string string)))

(defun guess-package-from-string (name)
  (or (and name
           (or (find-package name)
               (find-package (string-upcase name))))
      *package*))


;;; public interface.  slimefuns are the things that emacs is allowed
;;; to call

(defmacro defslimefun (fun &rest rest)
  `(progn
    (defun ,fun ,@rest)
    (export ',fun :swank)))

(defmacro defslimefun-unimplemented (fun args)
  `(progn
    (defun ,fun ,args
      (declare (ignore ,@args))
      (error "Backend function ~A not implemented." ',fun))
    (export ',fun :swank)))

(defslimefun eval-string (string buffer-package)
  (let ((*debugger-hook* #'swank-debugger-hook))
    (let (ok result)
      (unwind-protect
           (let ((*buffer-package* (guess-package-from-string buffer-package)))
             (assert (packagep *buffer-package*))
             (setq result (eval (read-form string)))
             (force-output)
             (setq ok t))
        (send-to-emacs (if ok `(:ok ,result) '(:aborted)))))))

(defslimefun interactive-eval (string)
  (let ((values (multiple-value-list (eval (from-string string)))))
    (force-output)
    (format nil "~{~S~^, ~}" values)))




(eval-when (:compile-toplevel) (compile-file swank::*sysdep-pathname*))
(eval-when (:load-toplevel :execute) (load swank::*sysdep-pathname*))


;;; Local Variables:
;;; eval: (font-lock-add-keywords 'lisp-mode '(("(\\(defslimefun\\)\\s +\\(\\(\\w\\|\\s_\\)+\\)"  (1 font-lock-keyword-face) (2 font-lock-function-name-face))))
;;; End:
