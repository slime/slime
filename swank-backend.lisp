;;;; -*- Mode: lisp; indent-tabs-mode: nil; outline-regexp: ";;;;;*" -*-
;;;
;;; slime-backend.lisp --- SLIME backend interface.
;;;
;;; Created by James Bielman in 2003. Released into the public domain.
;;;
;;; This file defines the functions that must be implemented
;;; separately for each Lisp. Each is declared as a generic function
;;; for which swank-<implementation>.lisp provides methods.

(defpackage :swank-backend
  (:use :common-lisp)
  (:export #:sldb-condition
           #:original-condition
           #:compiler-condition
           #:message
           #:short-message
           #:condition
           #:severity
           #:location
           #:location-p
           #:location-buffer
           #:location-position
           #:position-p
           #:position-pos
           #:print-output-to-string
           #:quit-lisp
           ))

(in-package :swank-backend)


;;;; Metacode

(defparameter *interface-functions* '()
  "The names of all interface functions.")

(defparameter *unimplemented-interfaces* '()
  "List of interface functions that are not implemented.
DEFINTERFACE adds to this list and DEFIMPLEMENTATION removes.")

(defmacro definterface (name args documentation &rest default-body)
  "Define an interface function for the backend to implement.
A generic function is defined with NAME, ARGS, and DOCUMENTATION.

If a DEFAULT-BODY is supplied then NO-APPLICABLE-METHOD is specialized
to execute the body if the backend doesn't provide a specific
implementation.

Backends implement these functions using DEFIMPLEMENTATION."
  (check-type documentation string "a documentation string")
  (flet ((gen-default-impl ()
           (let ((received-args (gensym "ARGS-")))
             `(defmethod no-applicable-method ((#:method
                                                (eql (function ,name)))
                                               &rest ,received-args)
               (destructuring-bind ,args ,received-args
                 ,@default-body)))))
     `(progn (defgeneric ,name ,args (:documentation ,documentation))
             (pushnew ',name *interface-functions*)
             ,(if (null default-body)
                `(pushnew ',name *unimplemented-interfaces*)
                (gen-default-impl))
             ;; see <http://www.franz.com/support/documentation/6.2/doc/pages/variables/compiler/s_cltl1-compile-file-toplevel-compatibility-p_s.htm>
             (eval-when (:compile-toplevel :load-toplevel :execute)
               (export ',name :swank-backend))
             ',name)))

(defmacro defimplementation (name args &body body)
  `(progn (defmethod ,name ,args ,@body)
          (if (member ',name *interface-functions*)
              (setq *unimplemented-interfaces*
                    (remove ',name *unimplemented-interfaces*))
              (warn "DEFIMPLEMENTATION of undefined interface (~S)" ',name))
          ',name))

(defun warn-unimplemented-interfaces ()
  "Warn the user about unimplemented backend features.
The portable code calls this function at startup."
  (warn "These Swank interfaces are unimplemented:~% ~A"
        (sort (copy-list *unimplemented-interfaces*) #'string<)))


;;;; Utilities

(defmacro with-struct ((conc-name &rest names) obj &body body)
  "Like with-slots but works only for structs."
  (flet ((reader (slot) (intern (concatenate 'string
					     (symbol-name conc-name)
					     (symbol-name slot))
				(symbol-package conc-name))))
    (let ((tmp (gensym "OO-")))
    ` (let ((,tmp ,obj))
        (symbol-macrolet
            ,(loop for name in names collect 
                   (typecase name
                     (symbol `(,name (,(reader name) ,tmp)))
                     (cons `(,(first name) (,(reader (second name)) ,tmp)))
                     (t (error "Malformed syntax in WITH-STRUCT: ~A" name))))
          ,@body)))))


;;;; TCP server

(definterface create-socket (host port)
  "Create a listening TCP socket on interface HOST and port PORT .")

(definterface local-port (socket)
  "Return the local port number of SOCKET.")

(definterface close-socket (socket)
  "Close the socket SOCKET.")

(definterface accept-connection (socket)
   "Accept a client connection on the listening socket SOCKET.  Return
a stream for the new connection.")

(definterface add-sigio-handler (socket fn)
  "Call FN whenever SOCKET is readable.")

(definterface remove-sigio-handlers (socket)
  "Remove all sigio handlers for SOCKET.")

(definterface add-fd-handler (socket fn)
  "Call FN when Lisp is waiting for input and SOCKET is readable.")

(definterface remove-fd-handlers (socket)
  "Remove all fd-handlers for SOCKET.")

(definterface preferred-communication-style ()
  "Return one of the symbols :spawn, :sigio, :fd-handler, or NIL."
  nil)

;;; Base condition for networking errors.
(define-condition network-error (simple-error) ())

(definterface emacs-connected (stream)
   "Hook called when the first connection from Emacs is established.
Called from the INIT-FN of the socket server that accepts the
connection.

This is intended for setting up extra context, e.g. to discover
that the calling thread is the one that interacts with Emacs.

STREAM is the redirected user output stream to Emacs.  This is passed
so that the backend can apply buffer flushing magic."  
  nil)


;;;; Unix signals

(defconstant +sigint+ 2)

(definterface call-without-interrupts (fn)
  "Call FN in a context where interrupts are disabled."
  (funcall fn))

(definterface getpid ()
  "Return the (Unix) process ID of this superior Lisp.")

(definterface lisp-implementation-type-name ()
  "Return a short name for the Lisp implementation."
  (lisp-implementation-type))

(definterface default-directory ()
  "Return the default directory."
  (directory-namestring (truename *default-pathname-defaults*)))

(definterface set-default-directory (directory)
  "Set the default directory.
This is used to resolve filenames without directory component."
  (setf *default-pathname-defaults* (truename (merge-pathnames directory)))
  (default-directory))

(definterface call-with-syntax-hooks (fn)
  "Call FN with hooks to handle special syntax."
  (funcall fn))

(definterface default-readtable-alist ()
    "Return a suitable initial value for SWANK:*READTABLE-ALIST*."
  '())


;;;; Compilation

(definterface call-with-compilation-hooks (func)
  "Call FUNC with hooks to record compiler conditions.")

(defmacro with-compilation-hooks ((&rest ignore) &body body)
  "Execute BODY as in CALL-WITH-COMPILATION-HOOKS."
  (declare (ignore ignore))
  `(call-with-compilation-hooks (lambda () (progn ,@body))))

(definterface swank-compile-string (string &key buffer position)
  "Compile source from STRING.  During compilation, compiler
conditions must be trapped and resignalled as COMPILER-CONDITIONs.

If supplied, BUFFER and POSITION specify the source location in Emacs.

Additionally, if POSITION is supplied, it must be added to source
positions reported in compiler conditions.")

(definterface operate-on-system (system-name operation-name &rest keyword-args)
  "Perform OPERATION-NAME on SYSTEM-NAME using ASDF.
The KEYWORD-ARGS are passed on to the operation.
Example:
\(operate-on-system \"SWANK\" \"COMPILE-OP\" :force t)"
  (unless (member :asdf *features*)
    (error "ASDF is not loaded."))
  (with-compilation-hooks ()
    (let ((operate (find-symbol "OPERATE" :asdf))
          (operation (find-symbol operation-name :asdf)))
      (when (null operation)
        (error "Couldn't find ASDF operation ~S" operation-name))
      (apply operate operation system-name keyword-args))))

(definterface swank-compile-file (filename load-p)
   "Compile FILENAME signalling COMPILE-CONDITIONs.
If LOAD-P is true, load the file after compilation.")

(deftype severity () '(member :error :warning :style-warning :note))

;; Base condition type for compiler errors, warnings and notes.
(define-condition compiler-condition (condition)
  ((original-condition
    ;; The original condition thrown by the compiler if appropriate.
    ;; May be NIL if a compiler does not report using conditions.
    :type (or null condition)
    :initarg :original-condition
    :accessor original-condition)

   (severity :type severity
             :initarg :severity
             :accessor severity)

   (message :initarg :message
            :accessor message)

   (short-message :initarg :short-message
                  :initform nil
                  :accessor short-message)

   (location :initarg :location
             :accessor location)))


;;;; Streams

(definterface make-fn-streams (input-fn output-fn)
   "Return character input and output streams backended by functions.
When input is needed, INPUT-FN is called with no arguments to
return a string.
When output is ready, OUTPUT-FN is called with the output as its
argument.

Output should be forced to OUTPUT-FN before calling INPUT-FN.

The streams are returned as two values.")


;;;; Documentation

(definterface arglist (name)
   "Return the lambda list for the symbol NAME.

The result can be a list or the :not-available if the arglist cannot
be determined.")

(definterface macroexpand-all (form)
   "Recursively expand all macros in FORM.
Return the resulting form.")

(definterface describe-symbol-for-emacs (symbol)
   "Return a property list describing SYMBOL.

The property list has an entry for each interesting aspect of the
symbol. The recognised keys are:

  :VARIABLE :FUNCTION :SETF :TYPE :CLASS :MACRO :COMPILER-MACRO
  :ALIEN-TYPE :ALIEN-STRUCT :ALIEN-UNION :ALIEN-ENUM

The value of each property is the corresponding documentation string,
or :NOT-DOCUMENTED. It is legal to include keys not listed here.

Properties should be included if and only if they are applicable to
the symbol. For example, only (and all) fbound symbols should include
the :FUNCTION property.

Example:
\(describe-symbol-for-emacs 'vector)
  => (:CLASS :NOT-DOCUMENTED
      :TYPE :NOT-DOCUMENTED
      :FUNCTION \"Constructs a simple-vector from the given objects.\")")

(definterface describe-definition (name type)
  "Describe the definition NAME of TYPE.
TYPE can be any value returned by DESCRIBE-SYMBOL-FOR-EMACS.

Return a documentation string, or NIL if none is available.")


;;;; Debugging

(definterface call-with-debugging-environment (debugger-loop-fn)
   "Call DEBUGGER-LOOP-FN in a suitable debugging environment.

This function is called recursively at each debug level to invoke the
debugger loop. The purpose is to setup any necessary environment for
other debugger callbacks that will be called within the debugger loop.

For example, this is a reasonable place to compute a backtrace, switch
to safe reader/printer settings, and so on.")

(define-condition sldb-condition (condition)
  ((original-condition
    :initarg :original-condition
    :accessor original-condition))
  (:documentation
   "Wrapper for conditions that should not be debugged.

When a condition arises from the internals of the debugger, it is not
desirable to debug it -- we'd risk entering an endless loop trying to
debug the debugger! Instead, such conditions can be reported to the
user without (re)entering the debugger by wrapping them as
`sldb-condition's."))

(definterface compute-backtrace (start end)
   "Return a list containing a backtrace of the condition current
being debugged.  The results are unspecified if this function is
called outside the dynamic contour CALL-WITH-DEBUGGING-ENVIRONMENT.

START and END are zero-based indices constraining the number of frames
returned.  Frame zero is defined as the frame which invoked the
debugger.  If END is nil, return the frames from START to the end of
the stack.")

(definterface print-frame (frame stream)
  "Print frame to stream.")

(definterface frame-source-location-for-emacs (frame-number)
  "Return the source location for FRAME-NUMBER.")

(definterface frame-catch-tags (frame-number)
  "Return a list of XXX list of what? catch tags for a debugger
stack frame.  The results are undefined unless this is called
within the dynamic contour of a function defined by
DEFINE-DEBUGGER-HOOK.")

(definterface frame-locals (frame-number)
   "Return a list of XXX local variable designators define me
for a debugger stack frame.  The results are undefined unless
this is called within the dynamic contour of a function defined
by DEFINE-DEBUGGER-HOOK.")

(definterface disassemble-frame (frame-number)
  "Disassemble the code for the FRAME-NUMBER.
The output should be written to standard output.
FRAME-NUMBER is a non-negative interger.")

(definterface eval-in-frame (form frame-number)
   "Evaluate a Lisp form in the lexical context of a stack frame
in the debugger.  The results are undefined unless called in the
dynamic contour of a function defined by DEFINE-DEBUGGER-HOOK.

FRAME-NUMBER must be a positive integer with 0 indicating the
frame which invoked the debugger.

The return value is the result of evaulating FORM in the
appropriate context.")

(definterface return-from-frame (frame-number form)
  "Unwind the stack to the frame FRAME-NUMBER and return the value(s)
produced by evaluating FORM in the frame context to its caller.

Execute any clean-up code from unwind-protect forms above the frame
during unwinding.

Return a string describing the error if it's not possible to return
from the frame.")

(definterface restart-frame (frame-number)
  "Restart execution of the frame FRAME-NUMBER with the same arguments
as it was called originally.")

(definterface format-sldb-condition (condition)
  "Format a condition for display in SLDB."
  (princ-to-string condition))

(definterface condition-references (condition)
  "Return a list of documentation references for a condition.
Each reference is one of:
  (:ANSI-CL
   {:FUNCTION | :SPECIAL-OPERATOR | :MACRO | :SECTION | :GLOSSARY }
   symbol-or-name)
  (:SBCL :NODE node-name)"
  '())

(definterface sldb-step (frame-number)
    "Step to the next code location in the frame FRAME-NUMBER.")
  


;;;; Definition finding

(defstruct (:location (:type list) :named
                      (:constructor make-location
                                    (buffer position &optional hints)))
  buffer position
  ;; Hints is a property list optionally containing:
  ;;   :snippet SOURCE-TEXT
  ;;     This is a snippet of the actual source text at the start of
  ;;     the definition, which could be used in a text search.
  hints)

(defstruct (:error (:type list) :named (:constructor)) message)
(defstruct (:file (:type list) :named (:constructor)) name)
(defstruct (:buffer (:type list) :named (:constructor)) name)
(defstruct (:position (:type list) :named (:constructor)) pos)

(definterface find-definitions (name)
   "Return a list ((DSPEC LOCATION) ...) for NAME's definitions.

NAME is a \"definition specifier\".

DSPEC is a \"definition specifier\" describing the
definition, e.g., FOO or (METHOD FOO (STRING NUMBER)) or
\(DEFVAR FOO).

LOCATION is the source location for the definition.")


;;;; XREF

(definterface who-calls (function-name)
  "Return the call sites of FUNCTION-NAME (a symbol).
The results is a list ((DSPEC LOCATION) ...).")

(definterface who-references (variable-name)
  "Return the locations where VARIABLE-NAME (a symbol) is referenced.
See WHO-CALLS for a description of the return value.")

(definterface who-binds (variable-name)
  "Return the locations where VARIABLE-NAME (a symbol) is bound.
See WHO-CALLS for a description of the return value.")

(definterface who-sets (variable-name)
  "Return the locations where VARIABLE-NAME (a symbol) is set.
See WHO-CALLS for a description of the return value.")

(definterface who-macroexpands (macro-name)
  "Return the locations where MACRO-NAME (a symbol) is expanded.
See WHO-CALLS for a description of the return value.")

(definterface who-specializes (class-name)
  "Return the locations where CLASS-NAME (a symbol) is specialized.
See WHO-CALLS for a description of the return value.")

;;; Simpler variants.

(definterface list-callers (function-name)
  "List the callers of FUNCTION-NAME.
This function is like WHO-CALLS except that it is expected to use
lower-level means. Whereas WHO-CALLS is usually implemented with
special compiler support, LIST-CALLERS is usually implemented by
groveling for constants in function objects throughout the heap.

The return value is as for WHO-CALLS.")

(definterface list-callees (function-name)
  "List the functions called by FUNCTION-NAME.
See LIST-CALLERS for a description of the return value.")


;;;; Profiling

;;; The following functions define a minimal profiling interface.

(definterface profile (fname)
  "Marks symbol FNAME for profiling.")

(definterface profiled-functions ()
  "Returns a list of profiled functions.")

(definterface unprofile (fname)
  "Marks symbol FNAME as not profiled.")

(definterface unprofile-all ()
  "Marks all currently profiled functions as not profiled."
  (dolist (f (profiled-functions))
    (unprofile f)))

(definterface profile-report ()
  "Prints profile report.")

(definterface profile-reset ()
  "Resets profile counters.")

(definterface profile-package (package callers-p methods)
  "Wrap profiling code around all functions in PACKAGE.  If a function
is already profiled, then unprofile and reprofile (useful to notice
function redefinition.)

If CALLERS-P is T names have counts of the most common calling
functions recorded.

When called with arguments :METHODS T, profile all methods of all
generic functions having names in the given package.  Generic functions
themselves, that is, their dispatch functions, are left alone.")


;;;; Inspector

(definterface inspected-parts (object)
  "Return a short description and a list of (LABEL . VALUE) pairs."
  (values (format nil "~S is an atom." object) '()))

(definterface describe-primitive-type (object)
  "Return a string describing the primitive type of object."
  (declare (ignore object))
  "N/A")


;;;; Multithreading
;;;
;;; The default implementations are sufficient for non-multiprocessing
;;; implementations.

(definterface startup-multiprocessing ()
   "Initialize multiprocessing, if necessary.

This function is called directly through the listener, not in an RPC
from Emacs. This is to support interfaces such as CMUCL's
MP::STARTUP-IDLE-AND-TOP-LEVEL-LOOPS which does not return like a
normal function."
   nil)

(definterface spawn (fn &key name)
  "Create a new thread to call FN.")

(definterface thread-name (thread)
   "Return the name of THREAD.

Thread names are be single-line strings and are meaningful to the
user. They do not have to be unique."
   (declare (ignore thread))
   "The One True Thread")

(definterface thread-status (thread)
   "Return a string describing THREAD's state."
   (declare (ignore thread))
   "")

(definterface make-lock (&key name)
   "Make a lock for thread synchronization.
Only one thread may hold the lock (via CALL-WITH-LOCK-HELD) at a time."
   (declare (ignore name))
   :null-lock)

(definterface call-with-lock-held (lock function)
   "Call FUNCTION with LOCK held, queueing if necessary."
   (declare (ignore lock)
            (type function function))
   (funcall function))

(definterface current-thread ()
  "Return the currently executing thread."
  0)

(definterface all-threads ()
  "Return a list of all threads.")

(definterface thread-alive-p (thread)
  "Test if THREAD is termintated."
  (member thread (all-threads)))

(definterface interrupt-thread (thread fn)
  "Cause THREAD to execute FN.")

(definterface kill-thread (thread)
  "Kill THREAD."
  (declare (ignore thread))
  nil)

(definterface send (thread object)
  "Send OBJECT to thread THREAD.")

(definterface receive ()
  "Return the next message from current thread's mailbox.")

(definterface quit-lisp ()
  "Exit the current lisp image.")
