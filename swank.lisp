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

;;; Setup and Hooks

(defun start-server (port-file-namestring)
  "Create a SWANK server and write its port number to the file
PORT-FILE-NAMESTRING in ascii text."
  (let ((port (create-swank-server 0 :reuse-address t)))
    (with-open-file (s port-file-namestring
                       :direction :output
                       :if-exists :overwrite
                       :if-does-not-exist :create)
      (format s "~S~%" port)))
  (when *swank-debug-p*
    (format *debug-io* "~&;; Swank ready.~%")))

;;; IO to emacs

(defvar *emacs-io* nil
  "Bound to a TCP stream to Emacs during request processing.")

(defvar *slime-output* nil
  "Bound to a slime-output-stream during request processing.")

(defvar *slime-input* nil
  "Bound to a slime-input-stream during request processing.")

(defvar *slime-io* nil
  "Bound to a two-way-stream built from *slime-input* and *slime-output*.")

(defparameter *redirect-output* t)

(defun read-from-emacs ()
  "Read and process a request from Emacs."
  (let ((form (read-next-form)))
    (if *redirect-output*
	(let ((*standard-output* *slime-output*)
	      (*error-output* *slime-output*)
	      (*trace-output* *slime-output*)
	      (*debug-io* *slime-io*)
	      (*query-io* *slime-io*)
              (*standard-input* *slime-input*))
	  (apply #'funcall form))
	(apply #'funcall form))))

(define-condition slime-read-error (error) 
  ((condition :initarg :condition :reader slime-read-error.condition))
  (:report (lambda (condition stream)
             (format stream "~A" (slime-read-error.condition condition)))))

(defun read-next-form ()
  "Read the next Slime request from *EMACS-IO* and return an
S-expression to be evaluated to handle the request.  If an error
occurs during parsing, it will be noted and control will be tranferred
back to the main request handling loop."
  (flet ((next-byte () (char-code (read-char *emacs-io*))))
    (handler-case
        (let* ((length (logior (ash (next-byte) 16)
                               (ash (next-byte) 8)
                               (next-byte)))
               (string (make-string length))
               (pos (read-sequence string *emacs-io*)))
          (assert (= pos length) nil
                  "Short read: length=~D  pos=~D" length pos)
          (read-form string))
      (serious-condition (c) 
        (error (make-condition 'slime-read-error :condition c))))))

(defun read-form (string)
  (with-standard-io-syntax
    (let ((*package* *swank-io-package*))
      (read-from-string string))))

(defun send-to-emacs (object)
  "Send `object' to Emacs."
  (let* ((string (prin1-to-string-for-emacs object))
         (length (1+ (length string))))
    (loop for position from 16 downto 0 by 8
          do (write-char (code-char (ldb (byte 8 position) length))
                         *emacs-io*))
    (write-string string *emacs-io*)
    (terpri *emacs-io*)
    (force-output *emacs-io*)))

(defun prin1-to-string-for-emacs (object)
  (with-standard-io-syntax
    (let ((*print-case* :downcase)
          (*print-readably* t)
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

;;; Input from Emacs

(defvar *read-input-catch-tag* 0)

(defun slime-read-char ()
  (let ((*read-input-catch-tag* (1+ *read-input-catch-tag*)))
    (send-to-emacs `(:read-char ,*read-input-catch-tag*))
    (code-char (catch *read-input-catch-tag* 
                 (loop (read-from-emacs))))))

(defslimefun take-input (tag input)
  (throw tag input))

;;; Evaluation

(defvar *swank-debugger-condition*)
(defvar *swank-debugger-hook*)

(defun swank-debugger-hook (condition hook)
  (let ((*swank-debugger-condition* condition)
	(*swank-debugger-hook* hook))
    (sldb-loop)))

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
  (let ((values (multiple-value-list
                 (let ((*package* *buffer-package*))
                   (eval (from-string string))))))
    (force-output)
    (format nil "~{~S~^, ~}" values)))

(defslimefun interactive-eval-region (string)
  (let ((*package* *buffer-package*))
    (with-input-from-string (stream string)
      (loop for form = (read stream nil stream)
	    until (eq form stream)
	    for result = (multiple-value-list (eval form))
	    do (force-output)
	    finally (return (format nil "~{~S~^, ~}" result))))))

(defslimefun re-evaluate-defvar (form)
  (let ((*package* *buffer-package*))
    (let ((form (read-from-string form)))
      (destructuring-bind (dv name &optional value doc) form
	(declare (ignore value doc))
	(assert (eq dv 'defvar))
	(makunbound name)
	(prin1-to-string (eval form))))))

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

(defvar *previous-compiler-condition* nil
  "Used to detect duplicates.")

(defvar *previous-context* nil
  "Used for compiler warnings without context.")

(defvar *compiler-notes* '()
  "List of compiler notes for the last compilation unit.")

(defun clear-compiler-notes ()  
  (setf *compiler-notes* '())
  (setf *previous-compiler-condition* nil)
  (setf *previous-context* nil))

(defvar *notes-database* (make-hash-table :test #'equal)
  "Database of recorded compiler notes/warnings/errors (keyed by filename).
Each value is a list of (LOCATION SEVERITY MESSAGE CONTEXT) lists.
  LOCATION is a position in the source code (integer or source path).
  SEVERITY is one of :ERROR, :WARNING, :STYLE-WARNING and :NOTE.
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
  (remove-if (lambda (sym)
               (or (keywordp sym) 
                   (and external-only
                        (not (equal (symbol-package sym) *buffer-package*))
                        (not (symbol-external-p sym)))))
             (apropos-list string package)))

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

;;; Completion

(defslimefun completions (string default-package-name)
  "Return a list of completions for a symbol designator STRING.  

The result is a list of strings.  If STRING is package qualified the
result list will also be qualified.  If string is non-qualified the
result strings are also not qualified and are considered relative to
DEFAULT-PACKAGE-NAME.

The way symbols are matched depends on the symbol designator's
format. The cases are as follows:
  FOO      - Symbols with matching prefix and accessible in the buffer package.
  PKG:FOO  - Symbols with matching prefix and external in package PKG.
  PKG::FOO - Symbols with matching prefix and accessible in package PKG."
  (multiple-value-bind (name package-name internal-p)
      (parse-symbol-designator string)
    (let ((completions nil)
          (package (find-package 
                    (string-upcase (cond ((equal package-name "") "KEYWORD")
                                         (package-name)
                                         (default-package-name))))))
      (flet ((symbol-matches-p (symbol)
               (and (string-prefix-p name (symbol-name symbol))
                    (or (or internal-p (null package-name))
                        (symbol-external-p symbol package)))))
        (when package
          (do-symbols (symbol package)
            (when (symbol-matches-p symbol)
              (push symbol completions)))))
      (let ((*print-case* (if (find-if #'upper-case-p string)
                              :upcase :downcase))
            (*package* package))
        (mapcar (lambda (s)
                  (cond (internal-p (format nil "~A::~A" package-name s))
                        (package-name (format nil "~A:~A" package-name s))
                        (t (format nil "~A" s))))
                ;; DO-SYMBOLS can consider the same symbol more than
                ;; once, so remove duplicates.
                (remove-duplicates (sort completions #'string<
                                         :key #'symbol-name)))))))

(defun parse-symbol-designator (string)
  "Parse STRING as a symbol designator.
Return three values:
 SYMBOL-NAME
 PACKAGE-NAME, or nil if the designator does not include an explicit package.
 INTERNAL-P, if the symbol is qualified with `::'."
  (values (let ((pos (position #\: string :from-end t)))
            (if pos (subseq string (1+ pos)) string))
          (let ((pos (position #\: string)))
            (if pos (subseq string 0 pos) nil))
          (search "::" string)))

(defun symbol-external-p (symbol &optional (package (symbol-package symbol)))
  "True if SYMBOL is external in PACKAGE.
If PACKAGE is not specified, the home package of SYMBOL is used."
  (multiple-value-bind (_ status)
      (find-symbol (symbol-name symbol) (or package (symbol-package symbol)))
    (declare (ignore _))
    (eq status :external)))
 
(defun string-prefix-p (s1 s2)
  "Return true iff the string S1 is a prefix of S2.
\(This includes the case where S1 is equal to S2.)"
  (and (<= (length s1) (length s2))
       (string-equal s1 s2 :end2 (length s1))))

;;; Apropos

(defslimefun apropos-list-for-emacs  (name &optional external-only package)
  "Make an apropos search for Emacs.
The result is a list of property lists."
  (mapcan (listify #'briefly-describe-symbol-for-emacs)
          (sort (apropos-symbols name external-only package)
                #'present-symbol-before-p)))

(defun listify (f)
  "Return a function like F, but which returns any non-null value
wrapped in a list."
  (lambda (x)
    (let ((y (funcall f x)))
      (and y (list y)))))

(defun present-symbol-before-p (a b)
  "Return true if A belongs before B in a printed summary of symbols.
Sorted alphabetically by package name and then symbol name, except
that symbols accessible in the current package go first."
  (flet ((accessible (s)
           (find-symbol (symbol-name s) *buffer-package*)))
    (cond ((and (accessible a) (accessible b))
           (string< (symbol-name a) (symbol-name b)))
          ((accessible a) t)
          ((accessible b) nil)
          (t
           (string< (package-name (symbol-package a))
                    (package-name (symbol-package b)))))))

;;;

(defslimefun untrace-all ()
  (untrace))

(defslimefun load-file (filename)
  (load filename))

;;;

(defslimefun sldb-continue ()
  (continue *swank-debugger-condition*))

(defslimefun throw-to-toplevel ()
  (throw 'slime-toplevel nil))


;;; Local Variables:
;;; eval: (font-lock-add-keywords 'lisp-mode '(("(\\(defslimefun\\)\\s +\\(\\(\\w\\|\\s_\\)+\\)"  (1 font-lock-keyword-face) (2 font-lock-function-name-face))))
;;; End:
