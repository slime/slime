(defpackage :swank
  (:use :common-lisp :wire)
  (:export #:start-server #:evaluate #:lookup-notes
           #:swank-compile-file #:arglist-string #:completions))

(in-package :swank)

(defconstant server-port 4004
  "Default port for the swank TCP server.")

(defconstant +internal-error+ 56)
(defconstant +condition+ 57)
(defconstant +ok+ 42)

(define-condition swank-error (simple-error) ())

(defvar *notes-database* (make-hash-table :test #'equal)
  "Database of recorded compiler notes/warnings/erros (keyed by filename).
Each value is a list of (LOCATION SEVERITY MESSAGE CONTEXT) lists.
  LOCATION is a position in the source code (integer or source path).
  SEVERITY is one of :ERROR, :WARNING, and :NOTE.
  MESSAGE is a string describing the note.
  CONTEXT is a string giving further details of where the error occured.")

(defvar *swank-debug-p* nil
  "When true extra debug printouts are enabled.")

;;; Setup and hooks.

(defun start-server (&optional (port server-port))
  (wire:create-request-server port nil :reuse-address t)
  (setf c:*record-xref-info* t)
  (ext:without-package-locks
   (setq *debugger-hook* #'debugger-hook)
   (setf c:*compiler-notification-function* #'handle-notification))
  (when *swank-debug-p*
    (format *debug-io* "~&Swank ready.~%")))

(defun debugger-hook (condition old-hook)
  "Hook function to be invoked instead of the debugger.
See CL:*DEBUGGER-HOOK*."
  ;; FIXME: Debug from Emacs!
  (declare (ignore old-hook))
  (handler-case
      (progn (format *error-output*
                     "~@<SWANK: unhandled condition ~2I~_~A~:>~%"
                     condition)
             (debug:backtrace 20 *error-output*)
             (finish-output *error-output*))
    (condition ()
      nil)))

(defun handle-notification (severity message context where-from position)
  "Hook function called by the compiler.
See C:*COMPILER-NOTIFICATION-FUNCTION*"
  (let ((location (or (current-compiler-error-source-path) position))
        (namestring (cond ((stringp where-from) where-from)
                          ;; we can be passed a stream from READER-ERROR
                          ((lisp::fd-stream-p where-from)
                           (lisp::fd-stream-file where-from))
                          (t where-from))))
    (when namestring
      (push (list location severity message context)
            (gethash namestring *notes-database*)))))

(defun current-compiler-error-source-path ()
  "Return the source-path for the current compiler error.
Returns NIL if this cannot be determined by examining internal
compiler state."
  (let ((context c::*compiler-error-context*))
    (cond ((c::node-p context)
           (reverse
            (c::source-path-original-source (c::node-source-path context))))
          ((c::compiler-error-context-p context)
           (reverse
            (c::compiler-error-context-original-source-path context))))))

;;; Functions for Emacs to call.

;;;; EVALUATE -- interface

(defun evaluate (string package)
  "Evaluate an expression for Emacs."
  (declare (type simple-string string))
  (when *swank-debug-p*
    (format *debug-io* "~&;; SWANK:EVALUATE (~S) |~S|~%" package string))
  (handler-case
      (send-value (eval (let ((*package* (find-package package)))
                          (read-from-string string))))
    (swank-error (condition)
      (send-reply +condition+
                  (format nil
                          (simple-condition-format-control condition)
                          (simple-condition-format-arguments condition))
                  ""))
    (error (condition)
      (send-and-log-internal-error condition))))

;;;; SWANK-COMPILE-FILE -- interface

(defun swank-compile-file (filename load-p)
  (remhash filename *notes-database*)
  (if (not (probe-file filename))
      (send-reply +condition+ "File does not exist" "")
      (handler-case
          (multiple-value-bind (output warnings failure)
              (compile-file filename :load (read-from-string load-p))
            (send-value (list (and output (namestring output))
                              warnings
                              failure)))
        (reader-error (condition)
          (send-condition condition))
        (end-of-file (condition)
          (send-condition condition))
        (package-error (condition)
          (send-condition condition))
        (c::compiler-error (condition)
          (send-condition condition (current-compiler-error-source-path)))
        (error (condition)
          (format *debug-io* "~&Condition: ~S / ~S~%" (type-of condition) condition)
          ;; Oops.
          (send-and-log-internal-error condition)))))

(defun send-reply (status message result)
  "Send a result triple over the wire to Emacs."
  (declare (type integer status))
  (when *swank-debug-p*
    (format *debug-io* "~&;; SWANK Reply: ~S, ~S, ~S~%" status message result))
  (wire-output-object *current-wire* status)
  (wire-output-object *current-wire* message)
  (wire-output-object *current-wire* result)
  (wire-force-output *current-wire*))

(defun send-value (value)
  (send-reply +ok+ "ok" (prin1-to-string value)))

(defun send-condition (condition &optional result)
  (send-reply +condition+ (princ-to-string condition) (prin1-to-string result)))

(defun send-and-log-internal-error (condition)
  (format *debug-io* "~&Internal Swank Error: ~A~%" condition)
  (send-reply +internal-error+
              (format nil "~&Internal Swank Error: ~A~%" condition)
              ""))

;;;; LOOKUP-NOTES -- interface

(defun lookup-notes (filename)
  "Return the compiler notes recorded for FILENAME.
\(See *NOTES-DATABASE* for a description of the return type.)"
  (gethash filename *notes-database*))

;;;; ARGLIST-STRING -- interface

(defun arglist-string (function)
  "Return a string describing the argument list for FUNCTION.
The result has the format \"(...)\"."
  (declare (type (or symbol function) function))
  (let ((arglist
         (if (not (or (fboundp function)
                      (functionp function)))
             "(-- <Unknown-Function>)"
             (let* ((fun (etypecase function
                           (symbol (or (macro-function function)
                                       (symbol-function function)))
                           (function function)))
                    (df (di::function-debug-function fun))
                    (arglist (kernel:%function-arglist fun)))
               (cond ((eval:interpreted-function-p fun)
                      (eval:interpreted-function-arglist fun))
                     ((pcl::generic-function-p fun)
                      (pcl::gf-pretty-arglist fun))
                     (arglist arglist)
                     ;; this should work both for
                     ;; compiled-debug-function and for
                     ;; interpreted-debug-function
                     (df (di::debug-function-lambda-list df))
                     (t "(<arglist-unavailable>)"))))))
    (if (stringp arglist)
        arglist
        (prin1-to-string arglist))))

;;;; COMPLETIONS -- interface

(defun completions (prefix package-name)
  "Return a list of completions for a symbol's PREFIX and PACKAGE-NAME.
The result is a list of symbol-name strings. All symbols accessible in
the package are considered."
  (let ((completions nil))
    (do-symbols (symbol (find-package package-name))
      (when (string-prefix-p prefix (symbol-name symbol))
        (push (symbol-name symbol) completions)))
    completions))

(defun string-prefix-p (s1 s2)
  "Return true iff the string S1 is a prefix of S2.
\(This includes the case where S1 is equal to S2.)"
  (and (<= (length s1) (length s2))
       (string= s1 s2 :end2 (length s1))))

