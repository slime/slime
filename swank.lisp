;;;; -*- Mode: lisp; outline-regexp: ";;;;+"; indent-tabs-mode: nil -*-
;;;
;;; swank.lisp --- the portable bits
;;;
;;; Created 2003, Daniel Barlow <dan@metacircles.com>
;;;
;;; This code has been placed in the Public Domain.  All warranties are 
;;; disclaimed.

;;; Currently the package is declared in swank-backend.lisp
#+nil
(defpackage :swank
  (:use :common-lisp)
  (:export #:start-server #:create-swank-server
           #:*sldb-pprint-frames*))

(in-package :swank)

;; Directly exported backend functions.
(export '(arglist-string backtrace function-source-location-for-emacs
          frame-locals frame-catch-tags frame-source-position
          frame-source-location-for-emacs
          eval-in-frame eval-string-in-frame))

(defvar *swank-io-package*
  (let ((package (make-package "SWANK-IO-PACKAGE")))
    (import '(nil t quote) package)
    package))

(declaim (optimize (debug 3)))

(defconstant server-port 4005
  "Default port for the Swank TCP server.")

(defvar *swank-debug-p* t
  "When true, print extra debugging information.")

(defvar *sldb-pprint-frames* nil
  "*pretty-print* is bound to this value when sldb prints a frame.")

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


;;;; Setup and Hooks

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


;;;; IO to Emacs
;;;
;;; We have two layers of I/O:
;;;
;;; The lower layer is a socket connection. Emacs sends us forms to
;;; evaluate, and we accept these by calling READ-FROM-EMACS. These
;;; evaluations can send messages back to Emacs as a side-effect by
;;; calling SEND-TO-EMACS.
;;;
;;; The upper layer is streams for redirecting I/O through Emacs, by
;;; mapping I/O requests onto messages.

;;; These stream variables are all dynamically-bound during request
;;; processing.

(defvar *emacs-io* nil
  "The raw TCP stream connected to Emacs.")

(defvar *slime-output* nil
  "Output stream for writing Lisp output text to Emacs.")

(defvar *slime-input* nil
  "Input stream to read user input from Emacs.")

(defvar *slime-io* nil
  "Two-way-stream built from *slime-input* and *slime-output*.")

(defparameter *redirect-output* t
  "When non-nil redirect Lisp standard I/O to Emacs.
Redirection is done while Lisp is processing a request for Emacs.")

(defun read-from-emacs ()
  "Read and process a request from Emacs."
  (let ((form (read-next-form)))
    (if *redirect-output*
	(let ((*standard-output* *slime-output*)
	      (*error-output* *slime-output*)
	      (*trace-output* *slime-output*)
	      (*debug-io* *slime-io*)
	      (*query-io* *slime-io*)
              (*standard-input* *slime-input*)
              (*terminal-io* *slime-io*))
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

(defvar *slime-features* nil
  "The feature list that has been sent to Emacs.")

(defun sync-state-to-emacs ()
  "Update Emacs if any relevant Lisp state has changed."
  (unless (eq *slime-features* *features*)
    (setq *slime-features* *features*)
    (send-to-emacs (list :new-features (mapcar #'symbol-name *features*)))))

(defun send-to-emacs (object)
  "Send `object' to Emacs."
   (let* ((string (prin1-to-string-for-emacs object))
          (length (1+ (length string))))
     (without-interrupts*
      (lambda ()
        (loop for position from 16 downto 0 by 8
              do (write-char (code-char (ldb (byte 8 position) length))
                             *emacs-io*))
        (write-string string *emacs-io*)
        (terpri *emacs-io*)
        (force-output *emacs-io*)))))

(defun prin1-to-string-for-emacs (object)
  (with-standard-io-syntax
    (let ((*print-case* :downcase)
          (*print-readably* t)
          (*print-pretty* nil)
          (*package* *swank-io-package*))
      (prin1-to-string object))))


;;;;; Input from Emacs

(defvar *read-input-catch-tag* 0)

(defun slime-read-string ()
  (force-output)
  (let ((*read-input-catch-tag* (1+ *read-input-catch-tag*)))
    (send-to-emacs `(:read-string ,*read-input-catch-tag*))
    (let (ok)
      (unwind-protect
           (prog1 (catch *read-input-catch-tag* 
                    (loop (read-from-emacs)))
             (setq ok t))
        (unless ok 
          (send-to-emacs `(:read-aborted)))))))
      
(defslimefun take-input (tag input)
  (throw tag input))


;;;; Reading and printing

(defvar *buffer-package*)
(setf (documentation '*buffer-package* 'symbol)
      "Package corresponding to slime-buffer-package.  

EVAL-STRING binds *buffer-package*.  Strings originating from a slime
buffer are best read in this package.  See also FROM-STRING and TO-STRING.")

(defun from-string (string)
  "Read string in the *BUFFER-PACKAGE*"
  (let ((*package* *buffer-package*))
    (read-from-string string)))

(defun symbol-from-string (string)
  "Read string in the *BUFFER-PACKAGE*"
  (let ((*package* *buffer-package*))
    (find-symbol (string-upcase string))))

(defun to-string (string)
  "Write string in the *BUFFER-PACKAGE*."
  (let ((*package* *buffer-package*))
    (prin1-to-string string)))

(defun guess-package-from-string (name &optional (default-package *package*))
  (or (and name
           (or (find-package name)
               (find-package (string-upcase name))))
      default-package))


;;;; Debugger

;;; These variables are dynamically bound during debugging.

;; The condition being debugged.
(defvar *swank-debugger-condition* nil)

(defvar *sldb-level* 0
  "The current level of recursive debugging.")

(defun swank-debugger-hook (condition hook)
  "Debugger entry point, called from *DEBUGGER-HOOK*.
Sends a message to Emacs declaring that the debugger has been entered,
then waits to handle further requests from Emacs. Eventually returns
after Emacs causes a restart to be invoked."
  (let ((*swank-debugger-condition* condition)
        (*package* *buffer-package*))
    (let ((*sldb-level* (1+ *sldb-level*)))
      (call-with-debugging-environment
       (lambda () (sldb-loop *sldb-level*))))))

(defun sldb-loop (level)
  (send-to-emacs (list* :debug *sldb-level* (debugger-info-for-emacs 0 1)))
  (unwind-protect
       (loop (catch 'sldb-loop-catcher
               (with-simple-restart
                   (abort "Return to sldb level ~D." level)
                 (handler-bind ((sldb-condition #'handle-sldb-condition))
                   (read-from-emacs)))))
    (send-to-emacs `(:debug-return ,level))))

(defun handle-sldb-condition (condition)
  "Handle an internal debugger condition.
Rather than recursively debug the debugger (a dangerous idea!), these
conditions are simply reported."
  (let ((real-condition (original-condition condition)))
    (send-to-emacs `(:debug-condition ,(princ-to-string real-condition))))
  (throw 'sldb-loop-catcher nil))

(defslimefun sldb-continue ()
  (continue *swank-debugger-condition*))

(defslimefun eval-string-in-frame (string index)
  (to-string (swank-backend:eval-in-frame (from-string string) index)))


;;;; Evaluation

(defslimefun eval-string (string buffer-package)
  (let ((*debugger-hook* #'swank-debugger-hook))
    (let (ok result)
      (unwind-protect
           (let ((*buffer-package* (guess-package-from-string buffer-package)))
             (assert (packagep *buffer-package*))
             (setq result (eval (read-form string)))
             (force-output)
             (setq ok t))
        (sync-state-to-emacs)
        (force-output *slime-output*)
        (send-to-emacs (if ok `(:ok ,result) '(:aborted)))))))

(defun format-values-for-echo-area (values)
  (cond (values (format nil "~{~S~^, ~}" values))
        (t "; No value")))

(defslimefun interactive-eval (string)
  (let ((values (multiple-value-list
                 (let ((*package* *buffer-package*))
                   (eval (from-string string))))))
    (force-output)
    (format-values-for-echo-area values)))

(defun eval-region (string &optional package-update-p)
  "Evaluate STRING and return the result.
If PACKAGE-UPDATE-P is non-nil, and evaluation causes a package
change, then send Emacs an update."
  (let ((*package* *buffer-package*))
    (unwind-protect
         (with-input-from-string (stream string)
           (loop for form = (read stream nil stream)
                 until (eq form stream)
                 for - = form
                 for values = (multiple-value-list (eval form))
                 do (force-output)
                 finally (return (values values -))))
      (when (and package-update-p (not (eq *package* *buffer-package*)))
        (send-to-emacs (list :new-package (package-name *package*)))))))

(defslimefun interactive-eval-region (string)
  (let ((*package* *buffer-package*))
    (format-values-for-echo-area (eval-region string))))

(defslimefun re-evaluate-defvar (form)
  (let ((*package* *buffer-package*))
    (let ((form (read-from-string form)))
      (destructuring-bind (dv name &optional value doc) form
	(declare (ignore value doc))
	(assert (eq dv 'defvar))
	(makunbound name)
	(prin1-to-string (eval form))))))

(defun swank-pprint (list)
  "Bind some printer variables and pretty print each object in LIST."
  (let ((*print-pretty* t)
        (*print-circle* t)
        (*print-escape* t)
        (*print-level* nil)
        (*print-length* nil))
    (cond ((null list) "; No value")
          (t (with-output-to-string (*standard-output*)
               (dolist (o list)
                 (pprint o)
                 (terpri)))))))

(defslimefun pprint-eval (string)
  (let ((*package* *buffer-package*))
    (swank-pprint (multiple-value-list (eval (read-from-string string))))))

(defslimefun set-package (package)
  (setq *package* (guess-package-from-string package))
  (package-name *package*))

(defslimefun listener-eval (string)
  (clear-input *slime-input*)
  (multiple-value-bind (values last-form) (eval-region string t)
    (setq +++ ++  ++ +  + last-form
	  *** **  ** *  * (car values)
	  /// //  // /  / values)
    (cond ((null values) "; No value")
          (t
           (let ((*package* *buffer-package*))
             (format nil "~{~S~^~%~}" values))))))


;;;; Compilation Commands.

(defvar *compiler-notes* '()
  "List of compiler notes for the last compilation unit.")

(defun clear-compiler-notes ()  
  (setf *compiler-notes* '()))

(defun canonicalize-filename (filename)
  (namestring (truename filename)))

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

(defun record-note-for-condition (condition)
  "Record a note for a compiler-condition."
  (push (make-compiler-note condition) *compiler-notes*))

(defun make-compiler-note (condition)
  "Make a compiler note data structure from a compiler-condition."
  (declare (type compiler-condition condition))
  (list :message (message condition)
        :severity (severity condition)
        :location (location condition)))

(defslimefun swank-compile-file (filename load-p)
  "Compile FILENAME and, when LOAD-P, load the result.
Record compiler notes signalled as `compiler-condition's."
  (clear-compiler-notes)
  (multiple-value-bind (result usecs)
      (handler-bind ((compiler-condition #'record-note-for-condition))
        (measure-time-interval (lambda ()
                                 (compile-file-for-emacs filename load-p))))
    (list (to-string result)
	  (format nil "~,2F" (/ usecs 1000000.0)))))

(defslimefun swank-compile-string (string buffer position)
  "Compile STRING (exerpted from BUFFER at POSITION).
Record compiler notes signalled as `compiler-condition's."
  (clear-compiler-notes)
  (multiple-value-bind (result usecs)
      (handler-bind ((compiler-condition #'record-note-for-condition))
        (measure-time-interval
         (lambda ()
           (compile-string-for-emacs string :buffer buffer :position position))))
    (list (to-string result)
	  (format nil "~,2F" (/ usecs 1000000.0)))))


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

(defslimefun swank-macroexpand-all (string)
  (apply-macro-expander #'macroexpand-all string))


;;; Completion

(defun case-convert (string)
  "Convert STRING according to the current readtable-case."
  (check-type string string)
  (ecase (readtable-case *readtable*)
    (:upcase (string-upcase string))
    (:downcase (string-downcase string))
    (:preserve string)
    (:invert (cond ((every #'lower-case-p string) (string-upcase string))
                   ((every #'upper-case-p string) (string-downcase string))
                   (t string)))))

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
          (package (let ((n (cond ((equal package-name "") "KEYWORD")
                                  (t (or package-name default-package-name)))))
                     (if n 
                         (find-package (case-convert n))
                         *buffer-package* ))))
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

(defun find-symbol-designator (string &optional (default-package *buffer-package*))
  "Return the symbol corresponding to the symbol designator STRING.
If string is not package qualified use DEFAULT-PACKAGE for the
resolution.  Return nil if no such symbol exists."
  (multiple-value-bind (name package-name internal-p)
      (parse-symbol-designator (case-convert string))
    (cond ((and package-name (not (find-package package-name)))
           nil)
          (t
           (let ((package (or (find-package package-name) default-package)))
             (multiple-value-bind (symbol access) (find-symbol name package)
               (cond ((and symbol package-name (not internal-p)
                           (not (eq access :external)))
                      nil)
                     (symbol (values symbol access)))))))))

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


;;;; Documentation

(defslimefun apropos-list-for-emacs  (name &optional external-only package)
  "Make an apropos search for Emacs.
The result is a list of property lists."
  (mapcan (listify #'briefly-describe-symbol-for-emacs)
          (sort (apropos-symbols name external-only package)
                #'present-symbol-before-p)))

(defun briefly-describe-symbol-for-emacs (symbol)
  "Return a property list describing SYMBOL.
Like `describe-symbol-for-emacs' but with at most one line per item."
  (flet ((first-line (string) 
           (let ((pos (position #\newline string)))
             (if (null pos) string (subseq string 0 pos)))))
    (list* :designator (to-string symbol)
           (map-if #'stringp #'first-line (describe-symbol-for-emacs symbol)))))

(defun map-if (test fn &rest lists)
  "Like (mapcar FN . LISTS) but only call FN on objects satisfying TEST.
Example:
\(map-if #'oddp #'- '(1 2 3 4 5)) => (-1 2 -3 4 -5)"
  (apply #'mapcar
         (lambda (x) (if (funcall test x) (funcall fn x) x))
         lists))

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
  (print-description-to-string (symbol-from-string symbol-name)))

(defslimefun describe-function (symbol-name)
  (print-description-to-string
   (symbol-function (symbol-from-string symbol-name))))

(defslimefun documentation-symbol (symbol-name)
  (let ((*package* *buffer-package*))
    (let ((vdoc (documentation (symbol-from-string symbol-name) 'variable))
          (fdoc (documentation (symbol-from-string symbol-name) 'function)))
      (and (or vdoc fdoc)
           (concatenate 'string
                        fdoc
                        (and vdoc fdoc '(#\Newline #\Newline))
                        vdoc)))))


;;;;

(defslimefun list-all-package-names ()
  (mapcar #'package-name (list-all-packages)))

(defslimefun untrace-all ()
  (untrace))

(defslimefun load-file (filename)
  (load filename))

(defslimefun throw-to-toplevel ()
  (throw 'slime-toplevel nil))

;;; Local Variables:
;;; eval: (font-lock-add-keywords 'lisp-mode '(("(\\(defslimefun\\)\\s +\\(\\(\\w\\|\\s_\\)+\\)"  (1 font-lock-keyword-face) (2 font-lock-function-name-face))))
;;; End:
