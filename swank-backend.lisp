;;;; -*- Mode: lisp; indent-tabs-mode: nil; outline-regexp: ";;;;;*" -*-
;;;
;;; slime-backend.lisp --- SLIME backend interface.
;;;
;;; Created by James Bielman in 2003. Released into the public domain.
;;;
;;; This file defines the functions that must be implemented
;;; separately for each Lisp. Each is declared as a generic function
;;; for which swank-<implementation>.lisp provides methods.

(defpackage :swank
  (:use :common-lisp)
  (:nicknames #:swank-backend)
  (:export #:*sldb-pprint-frames*
           #:apropos-list-for-emacs
           #:arglist-string
           #:backtrace
           #:call-with-I/O-lock
           #:call-with-conversation-lock
           #:compiler-notes-for-emacs
           #:completions
           #:create-server
           #:create-swank-server
           #:describe-definition
           #:describe-symbol
           #:describe-symbol-for-emacs
           #:disassemble-symbol
           #:documentation-symbol
           #:eval-in-frame
           #:return-from-frame
           #:restart-frame
           #:eval-string
           #:eval-string-in-frame
           #:find-function-locations
           #:frame-catch-tags
           #:frame-locals
           #:frame-source-location-for-emacs
           #:frame-source-position
           #:getpid
           #:give-goahead
           #:give-gohead
           #:init-inspector
           #:inspect-in-frame
           #:inspect-nth-part
           #:inspector-next
           #:inspector-pop
           #:interactive-eval
           #:interactive-eval-region
           #:invoke-nth-restart
           #:invoke-nth-restart-for-emacs
           #:list-all-package-names
           #:list-callees
           #:list-callers
           #:listener-eval
           #:load-file
           #:pprint-eval
           #:pprint-eval-string-in-frame
           #:quit-inspector
           #:re-evaluate-defvar
           #:set-default-directory
           #:set-package
           #:sldb-abort
           #:sldb-break-with-default-debugger
           #:sldb-continue
           #:slime-debugger-function
           #:start-server
           #:startup-multiprocessing
           #:startup-multiprocessing-for-emacs
           #:swank-compile-file
           #:swank-compile-string
           #:swank-macroexpand
           #:swank-macroexpand-1
           #:swank-macroexpand-all
           #:take-input
           #:thread-id
           #:thread-name
           #:throw-to-toplevel
           #:toggle-trace-fdefinition
           #:untrace-all
           #:profile
           #:unprofile
           #:unprofile-all
           #:profiled-functions
           #:profile-report
           #:profile-reset
           #:profile-package
           #:wait-goahead
           #:warn-unimplemented-interfaces
           #:who-binds
           #:who-calls
           #:who-macroexpands
           #:who-references
           #:who-sets
           ))

(in-package :swank)


;;;; Metacode

(defparameter *interface-functions* '()
  "The names of all interface functions.")

(defparameter *unimplemented-interfaces* '()
  "List of interface functions that are not implemented.
DEFINTERFACE adds to this list and DEFIMPLEMENTATION removes.")

(defmacro definterface (name args documentation &body default-body)
  "Define an interface function for the backend to implement.
A generic function is defined with NAME, ARGS, and DOCUMENTATION.

If a DEFAULT-BODY is supplied then NO-APPLICABLE-METHOD is specialized
to execute the body if the backend doesn't provide a specific
implementation.

Backends implement these functions using DEFIMPLEMENTATION."
  (flet ((gen-default-impl ()
           (let ((received-args (gensym "ARGS-")))
             `(defmethod no-applicable-method ((#:method
                                                (eql (function ,name)))
                                               &rest ,received-args)
               (destructuring-bind ,args ,received-args
                 ,@default-body)))))
    `(prog1 (defgeneric ,name ,args (:documentation ,documentation))
            (pushnew ',name *interface-functions*)
            ,(if (null default-body)
                 `(pushnew ',name *unimplemented-interfaces*)
                 (gen-default-impl)))))

(defmacro defimplementation (name args &body body)
  ;; Is this a macro no-no -- should it be pushed out of macroexpansion?
  `(prog1 (defmethod ,name ,args ,@body)
    (if (member ',name *interface-functions*)
        (setq *unimplemented-interfaces*
              (remove ',name *unimplemented-interfaces*))
        (warn "DEFIMPLEMENTATION of undefined interface (~S)" ',name))))

(defun warn-unimplemented-interfaces ()
  "Warn the user about unimplemented backend features.
The portable code calls this function at startup."
  (warn "These Swank interfaces are unimplemented:~% ~A"
        (sort (copy-list *unimplemented-interfaces*) #'string<)))


;;;; TCP server

(definterface create-socket (port)
  "Create a listening TCP socket on port PORT.")

(definterface local-port (socket)
  "Return the local port number of SOCKET.")

(definterface close-socket (socket)
  "Close the socket SOCKET.")

(definterface accept-connection (socket)
   "Accept a client connection on the listening socket SOCKET.  Return
a stream for the new connection.")

(definterface add-input-handler (socket fn)
  "Call FN whenever SOCKET is readable.")

(definterface remove-input-handlers (socket)
  "Remove all input handlers for SOCKET.")

;;; Base condition for networking errors.
(define-condition network-error (error) ())

(definterface emacs-connected ()
   "Hook called when the first connection from Emacs is established.
Called from the INIT-FN of the socket server that accepts the
connection.

This is intended for setting up extra context, e.g. to discover
that the calling thread is the one that interacts with Emacs."
   nil)


;;;; Unix signals

(defconstant +sigint+ 2)

(defgeneric call-without-interrupts (fn)
  (:documentation "Call FN in a context where interrupts are disabled."))

(defgeneric getpid ()
  (:documentation "Return the (Unix) process ID of this superior Lisp."))


;;;; Compilation

(definterface call-with-compilation-hooks (func)
   "Call FUNC with hooks to trigger SLDB on compiler errors.")

(defmacro with-compilation-hooks ((&rest ignore) &body body)
  (declare (ignore ignore))
  `(call-with-compilation-hooks (lambda () (progn ,@body))))

(definterface compile-string-for-emacs (string &key buffer position)
   "Compile source from STRING.  During compilation, compiler
conditions must be trapped and resignalled as COMPILER-CONDITIONs.

If supplied, BUFFER and POSITION specify the source location in Emacs.

Additionally, if POSITION is supplied, it must be added to source
positions reported in compiler conditions.")

(definterface compile-file-for-emacs (filename load-p)
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

(definterface arglist-string (function-name)
   "Return the argument for FUNCTION-NAME as a string.
The result should begin and end with parenthesis.")

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

(definterface debugger-info-for-emacs (start end)
   "Return debugger state, with stack frames from START to END.
The result is a list:
  (condition ({restart}*) ({stack-frame}*)
where
  condition   ::= (description type)
  restart     ::= (name description)
  stack-frame ::= (number description)

condition---a pair of strings: message, and type.

restart---a pair of strings: restart name, and description.

stack-frame---a number from zero (the top), and a printed
representation of the frame's call.

Below is an example return value. In this case the condition was a
division by zero (multi-line description), and only one frame is being
fetched (start=0, end=1).

 ((\"Arithmetic error DIVISION-BY-ZERO signalled.
Operation was KERNEL::DIVISION, operands (1 0).\"
   \"[Condition of type DIVISION-BY-ZERO]\")
  ((\"ABORT\" \"Return to Slime toplevel.\")
   (\"ABORT\" \"Return to Top-Level.\"))
  ((0 \"(KERNEL::INTEGER-/-INTEGER 1 0)\")))")

(definterface backtrace (start end)
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
    (2 \"(SYS::%TOPLEVEL #<SYS::ENVIRONMENT #x394834>)\"))")

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


;;;; Queries

#+(or)
;;; This is probably a better interface than find-function-locations.
(definterface find-definitions (name)
   "Return a list of (LABEL . LOCATION) pairs for NAME's definitions.

NAME is string denoting a symbol or \"definition specifier\".

LABEL is a string describing the definition, e.g., \"foo\" or
\"(method foo (string number))\" or \"(variable bar)\".

LOCATION is a source location of the form:

<location> ::= (:location <buffer> <position>)
             | (:error <message>) 

<buffer>   ::= (:file <filename>)
             | (:buffer <buffername>)
             | (:source-form <string>)

<position> ::= (:position <fixnum> [<align>]) ; 1 based
             | (:function-name <string>)
")

(definterface find-function-locations (name)
   "Return a list (LOCATION LOCATION ...) for NAME's definitions.

LOCATION is a source location of the form:

<location> ::= (:location <buffer> <position>)
             | (:error <message>) 

<buffer>   ::= (:file <filename>)
             | (:buffer <buffername>)
             | (:source-form <string>)

<position> ::= (:position <fixnum> [<align>]) ; 1 based
             | (:line <fixnum> [<fixnum>])
             | (:function-name <string>)
             | (:source-path <list> <start-position>)
")


;;;; Inspector

(defgeneric inspected-parts (object)
  (:documentation
   "Return a short description and a list of (LABEL . VALUE) pairs."))

(defgeneric describe-primitive-type (object)
  (:documentation
   "Return a string describing the primitive type of object."))


;;;; Multiprocessing
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

(definterface thread-id ()
   "Return a value that uniquely identifies the current thread.
Thread-IDs allow Emacs to refer to individual threads.

When called several times by the same thread, all return values are
EQUAL. The value has a READable printed representation that preserves
equality. The printed representation must be identical in Emacs Lisp
and Common Lisp, and short enough to include in the REPL prompt.

For example, a THREAD-ID could be an integer or a short ASCII string.

Systems that do not support multiprocessing return NIL."
   nil)

(definterface thread-name (thread-id)
   "Return the name of the thread identified by THREAD-ID.

Thread names are be single-line strings and are meaningful to the
user. They do not have to be unique."
   (declare (ignore thread-id))
   "The One True Thread")

(definterface make-lock (&key name)
   "Make a lock for thread synchronization.
Only one thread may hold the lock (via CALL-WITH-LOCK-HELD) at a time."
   (declare (ignore name))
   :null-lock)

(definterface call-with-lock-held (lock function)
   "Call FUNCTION with LOCK held, queueing if necessary."
   (declare (ignore lock))
   (funcall function))


;;;; XREF

(definterface who-calls (function-name)
  "Return the call sites of FUNCTION-NAME (a string).
The results are grouped together by filename:
  <result>    ::= (<file>*)
  <file>      ::= (<filename> . (<reference>*))
  <reference> ::= (<label> . <location>)
  <label>     ::= string
  <location>  ::= source-location")

(definterface who-references (variable-name)
  "Return the locations where VARIABLE-NAME (a string) is referenced.
See WHO-CALLS for a description of the return value.")

(definterface who-binds (variable-name)
  "Return the locations where VARIABLE-NAME (a string) is bound.
See WHO-CALLS for a description of the return value.")

(definterface who-sets (variable-name)
  "Return the locations where VARIABLE-NAME (a string) is set.
See WHO-CALLS for a description of the return value.")

(definterface who-macroexpands (macro-name)
  "Return the locations where MACRO-NAME (a string) is expanded.
See WHO-CALLS for a description of the return value.")

(definterface who-specializes (class-name)
  "Return the locations where CLASS-NAME (a string) is specialized.
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

