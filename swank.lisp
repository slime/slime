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
  (defparameter swank::*sysdep-pathname*
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
  (create-swank-server port :reuse-address t)
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
S-expression to be evaluated to handle the request.  If an error
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
  (with-standard-io-syntax
    (let ((*print-case* :downcase)
          (*print-readably* nil)
          (*print-pretty* nil)
          (*package* *swank-io-package*))
      (prin1-to-string object))))

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

;;; this was unimplemented in -openmcl, anyone know why?
;;; ditto interactive-eval-region
(defslimefun pprint-eval (string)
  (let ((*package* *buffer-package*))
    (let ((value (eval (read-from-string string))))
      (let ((*print-pretty* t)
	    (*print-circle* t)
	    (*print-level* nil)
	    (*print-length* nil))
	(with-output-to-string (stream)
	  (pprint value stream))))))

(defslimefun set-package (package)
  (setq *package* (guess-package-from-string package))
  (package-name *package*))

;;;; Compilation Commands.

(defvar *compiler-notes* '()
  "List of compiler notes for the last compilation unit.")

(defun clear-compiler-notes ()  
  (setf *compiler-notes* '())
  (setf *previous-compiler-condition* nil)
  (setf *previous-context* nil))

(defvar *notes-database* (make-hash-table :test #'equal)
  "Database of recorded compiler notes/warnings/erros (keyed by filename).
Each value is a list of (LOCATION SEVERITY MESSAGE CONTEXT) lists.
  LOCATION is a position in the source code (integer or source path).
  SEVERITY is one of :ERROR, :WARNING, and :NOTE.
  MESSAGE is a string describing the note.
  CONTEXT is a string giving further details of where the error occured.")

(defun clear-note-database (filename)
  (remhash (canonicalize-filename filename) *notes-database*))

(defslimefun features ()
  (mapcar #'symbol-name *features*))

(defun canonicalize-filename (filename)
  (namestring (truename filename)))

(defslimefun compiler-notes-for-file (filename)
  "Return the compiler notes recorded for FILENAME.
\(See *NOTES-DATABASE* for a description of the return type.)"
  (gethash (canonicalize-filename filename) *notes-database*))

(defslimefun compiler-notes-for-emacs ()
  "Return the list of compiler notes for the last compilation unit."
  (reverse *compiler-notes*))

(defun measure-time-interval (fn)
  "Call FN and return the first return value and the elapsed time.
The time is measured in microseconds."
  (let ((before (get-internal-real-time)))
    (values
     (funcall fn)
     (* (- (get-internal-real-time) before)
        (/ 1000000 internal-time-units-per-second)))))

(defmacro with-trapping-compilation-notes (() &body body)
  `(call-trapping-compilation-notes (lambda () ,@body)))

(defun call-with-compilation-hooks (fn)
  (multiple-value-bind (result usecs)
      (with-trapping-compilation-notes ()
	 (clear-compiler-notes)
	 (measure-time-interval fn))
    (list (to-string result)
	  (format nil "~,2F" (/ usecs 1000000.0)))))

(defslimefun list-all-package-names ()
  (mapcar #'package-name (list-all-packages)))

(defun apropos-symbols (string &optional external-only package)
  "Return the symbols matching an apropos search."
  ;; CMUCL used ext:map-apropos here, not sure why
  (remove-if #'keywordp (apropos-list string package external-only)))

(defun print-output-to-string (fn)
  (with-output-to-string (*standard-output*)
    (let ((*debug-io* *standard-output*))
      (funcall fn))))

(defun print-description-to-string (object)
  (print-output-to-string (lambda () (describe object))))

(defslimefun describe-symbol (symbol-name)
  (print-description-to-string (from-string symbol-name)))

(defslimefun describe-function (symbol-name)
  (print-description-to-string (symbol-function (from-string symbol-name))))

;;; Macroexpansion

(defun apply-macro-expander (expander string)
  (let ((*print-pretty* t)
	(*print-length* 20)
	(*print-level* 20))
    (to-string (funcall expander (from-string string)))))

(defslimefun swank-macroexpand-1 (string)
  (apply-macro-expander #'macroexpand-1 string))

(defslimefun swank-macroexpand (string)
  (apply-macro-expander #'macroexpand string))

(defslimefun disassemble-symbol (symbol-name)
  (print-output-to-string (lambda () (disassemble (from-string symbol-name)))))


;;;; now pull the per-backend stuff in
(eval-when (:compile-toplevel) (compile-file swank::*sysdep-pathname*))
(eval-when (:load-toplevel :execute) (load swank::*sysdep-pathname*))


;;; Local Variables:
;;; eval: (font-lock-add-keywords 'lisp-mode '(("(\\(defslimefun\\)\\s +\\(\\(\\w\\|\\s_\\)+\\)"  (1 font-lock-keyword-face) (2 font-lock-function-name-face))))
;;; End:
