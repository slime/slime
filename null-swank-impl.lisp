;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; null-swank-impl.lisp --- Empty SWANK-IMPL reference implementation.
;;;
;;; Copyright (C) 2003, James Bielman  <jamesjb@jamesjb.com>
;;; Released into the public domain; all warranties are disclaimed.
;;;
;;;   $Id$
;;;

;; The "SWANK-IMPL" package contains functions that access the naughty
;; bits of the Lisp implementation, such as debugger or compiler
;; internals.  In an ideal world, each function in this package would
;; be a thin wrapper around a public interface defined by the vendor
;; that massages the input/output into the format we need.
;;
;; Additionally, there is nothing Slime-specific about the functions
;; in this module---ilisp could use this as a portability layer, for
;; example.
;;
;; This is the simplest possible SWANK-IMPL implementation; it signals
;; unimplemented errors for all exported entry points.  When porting
;; Swank to new Lisp implementations, start by copying this interface.
;;
;; Open Issues:
;;
;; We need to work out exactly what "source position designators" are;
;; for now I'm assuming the plists that we use currently---is it worth
;; formalizing that into a class that we flatten to/from a plist
;; during transmission to Emacs?  What about the plists used for frame
;; locals?
;;
;; Currently there is some duplicate functionality in the compiler
;; note and xref source position plist stuff.  It might be useful to
;; merge both these things into the aforementioned position
;; designators.
;;
;; There is some inconsistency about when the functions in this module
;; return nil or signal a not-implemented-error---I think what's in
;; here is the right thing but it may be worth specifying precisely
;; for each exported function whether it must return a meaningful
;; value or not.
;;
;; Many of these functions have the same name as the slimefuns defined
;; in swank.lisp---this is okay but the intent is not to use the
;; functions defined here as the slimefuns; instead the entry points
;; should package-qualify calls into SLIME-IMPL (or we could rename
;; everything).
;;
;; I haven't attempt to refactor the inspector in the CMU backend.

;;; Administrivia

(defpackage :null-swank-impl
  ;; Commented out because this conflicts with the existing
  ;; "SWANK-IMPL" package, we'll want to uncomment this eventually.
  ;; (:nicknames :swank-impl)
  (:use :common-lisp)
  (:export 
   #:backtrace
   #:backtrace-length
   #:compile-file-trapping-conditions
   #:compile-stream-trapping-conditions
   #:compiler-condition
   #:compiler-condition-message
   #:compiler-condition-original-condition
   #:compiler-condition-source-pathname
   #:compiler-condition-source-position
   #:compiler-error
   #:compiler-note
   #:compiler-style-warning
   #:compiler-warning
   #:debug-condition
   #:define-debugger-hook
   #:eval-in-frame
   #:frame-catch-tags
   #:frame-locals
   #:frame-source-position
   #:function-arglist
   #:function-source-location
   #:not-implemented-error
   #:not-implemented-function-name
   #:setf-function-name
   #:type-information
   #:walk-form
   #:who-binds
   #:who-calls
   #:who-macroexpands
   #:who-sets))

(in-package :null-swank-impl)

;;; Conditions and Handlers

;; XXX We may want to move the conditions out of here to avoid
;; duplicating them.  The intent was to specify a general interface
;; but allow each backend to add extra slots or derive from
;; implementation-specific conditions.  I don't know if that will be
;; useful.

;; Condition signalled when functionality is not implemented for this
;; Lisp.  It is desirable to handle this and notify the user
;; accordingly.
(define-condition not-implemented-error (error)
  ((function-name
    ;; The name of the unimplemented function.
    :reader not-implemented-function-name
    :initarg :function-name))
  (:report (lambda (condition stream)
             (format stream "The Slime function ~S is not implemented."
                     (not-implemented-function-name condition)))))

;; Base condition type for compiler errors, warnings, and notes.
(define-condition compiler-condition (condition)
  ((original-condition
    ;; The original condition signalled by the compiler, if appropriate.
    ;; May be nil if the compiler does not report using conditions.
    :initarg :original-condition
    :reader compiler-condition-original-condition)

   (message
    ;; The error or warning message, must be a non-nil string.
    ;; [Would it be better to make this a method?]
    :initarg :message
    :reader compiler-condition-message)

   (source-pathname
    ;; A pathname designator of the source file in which the condition
    ;; originated, or nil if no pathname information is available.
    :initarg :source-pathname
    :reader compiler-condition-source-pathname)

   (source-position
    ;; A "source position designator" indicating the precise location
    ;; within SOURCE-PATHNAME of the condition.
    :initarg :source-position
    :reader compiler-condition-source-position)))

;; Portable interface for errors during complication.
(define-condition compiler-error (compiler-condition)
  ())

;; Portable interface to warnings during compilation.
(define-condition compiler-warning (compiler-condition)
  ())

;; Portable interface to compiler style warnings.
(define-condition compiler-style-warning (compiler-condition)
  ())

;; Portable interface to informational compiler notes.
(define-condition compiler-note (compiler-condition)
  ())

;; XXX what is this used for?
(define-condition debug-condition (condition)
  ())

;;; Compilation

(defun compile-file-trapping-conditions (filename &key (load t))
  "Compile FILENAME like COMPILE-FILE but resignal compilation
conditions as Swank compiler conditions."
  (declare (ignore filename load))
  (error 'not-implemented-error 
         :function-name 'compile-file-trapping-conditions))

(defun compile-stream-trapping-conditions (stream &key 
                                           filename position (load t))
  "Compile source from STREAM.  During compilation, compiler
conditions must be trapped and resignalled as
COMPILER-CONDITIONs.

If supplied, FILENAME and POSITION are used in any cross
referencing or source file recording systems supported by the
compiler.

Additionally, if POSITION is supplied, it must be added to
source positions reported in compiler conditions."
  (declare (ignore stream filename position load))
  (error 'not-implemented-error 
         :function-name 'compile-stream-trapping-conditions))

;;; Symbol and Function Introspection

(defun function-arglist (symbol)
  "Return the function argument list of SYMBOL if it is fbound,
either as a list of symbols, a string or nil if no such function
exists or no argument list is available."
  (declare (ignore symbol))
  nil)

(defun type-information (type-name)
  "Return information XXX in what format about TYPE-NAME."
  (declare (ignore type-name))
  nil)

(defun setf-function-name (symbol)
  "Given a function name, return the compiler's notion of the
function name of (SETF SYMBOL), or nil if no such symbol exists
or cannot be calculated.  This is used by SWANK:APROPOS to show
documentation for (setf foo)."
  (declare (ignore symbol))
  nil)

(defun walk-form (form)
  "Recursively expand all macros in FORM and return the result
expression."
  (declare (ignore form))
  (error 'not-implemented-error :function-name 'walk-form))

;;; Cross-Referencing

;; XXX do these return "source position designators" the same as the
;; compiler note conditions?  in the current slime these seem to be
;; handled differently---we should probably merge them

(defun function-source-location (function-name)
  "Return the canonical source location of the definition of
FUNCTION-NAME."
  (declare (ignore function-name))
  (error 'not-implemented-error :function-name 'function-source-location))

(defun who-calls (symbol)
  "Return the places where FUNCTION-NAME is called."
  (declare (ignore symbol))
  (error 'not-implemented-error :function-name 'who-calls))

(defun who-references (variable-name)
  "Return the places where VARIABLE-NAME is referenced."
  (declare (ignore variable-name))
  (error 'not-implemented-error :function-name 'who-references))

(defun who-binds (variable-name)
  "Return the places where VARIABLE-NAME is bound."
  (declare (ignore variable-name))
  (error 'not-implemented-error :function-name 'who-binds))

(defun who-sets (variable-name)
  "Return the places where VARIABLE-NAME is set."
  (declare (ignore variable-name))
  (error 'not-implemented-error :function-name 'who-sets))

(defun who-macroexpands (macro)
  "Return the places where MACRO is expanded."
  (declare (ignore macro))
  (error 'not-implemented-error :function-name 'who-macroexpands))

;;; Debugging

(defmacro define-debugger-hook (name (condition hook) &body body)
  "Define a function NAME taking arguments CONDITION and HOOK.

Before executing BODY, the defined function executes whatever
implementation-specific setup is necessary to enable other
SWANK-IMPL debugging functions to work.

This usually consists of marking the current frame pointer
somehow as to avoid seeing the Slime debugger itself in the
backtrace.  For some backends there is no setup and this macro
simply expands into DEFUN."
  `(defun ,name (,condition ,hook)
    ,@body))

(defun backtrace-length ()
  "Return the total number of stack frames known to the debugger."
  0)

(defun backtrace (&optional (start 0) (end most-positive-fixnum))
  "Return a list containing a backtrace of the condition current
being debugged.  The results are unspecified if this function is
called outside the dynamic contour of a debugger hook defined by
DEFINE-DEBUGGER-HOOK.

START and END are zero-based indices constraining the number of
frames returned.  Frame zero is defined as the frame which invoked
the debugger.

The backtrace is returned as a list of tuples of the form
\(FRAME-NUMBER FRAME-DESCRIPTION), where FRAME-NUMBER is the
index of the frame, defined like START/END, and FRAME-DESCRIPTION
is a string containing text to display in the debugger for this
frame.

An example return value:

   ((0 \"(HELLO \"world\")\")
    (1 \"(RUN-EXCITING-LISP-DEMO)\")
    (2 \"(SYS::%TOPLEVEL #<SYS::ENVIRONMENT #x394834>)\"))"
  (declare (ignore start end))
  (error 'not-implemented-error :function-name 'backtrace))

(defun frame-source-position (frame-number)
  "Return a source position designator for a debugger stack frame
index.  The results are undefined unless this is called within
the dynamic contour of a function defined by
DEFINE-DEBUGGER-HOOK.

FRAME-NUMBER must be a positive integer with 0 indicating the
frame which invoked the debugger."
  (declare (ignore frame-number))
  nil)

(defun frame-catch-tags (frame-number)
  "Return a list of XXX list of what? catch tags for a debugger
stack frame.  The results are undefined unless this is called
within the dynamic contour of a function defined by
DEFINE-DEBUGGER-HOOK."
  (declare (ignore frame-number))
  nil)

(defun frame-locals (frame-number)
  "Return a list of XXX local variable designators define me
for a debugger stack frame.  The results are undefined unless
this is called within the dynamic contour of a function defined
by DEFINE-DEBUGGER-HOOK."
  (declare (ignore frame-number))
  nil)

(defun eval-in-frame (form frame-number)
  "Evaluate a Lisp form in the lexical context of a stack frame
in the debugger.  The results are undefined unless called in the
dynamic contour of a function defined by DEFINE-DEBUGGER-HOOK.

FRAME-NUMBER must be a positive integer with 0 indicating the
frame which invoked the debugger.

The return value is the result of evaulating FORM in the
appropriate context."
  (declare (ignore form frame-number))
  (error 'not-implemented-error :function-name 'eval-in-frame))
