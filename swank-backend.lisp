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
           #:describe-alien-enum
           #:describe-alien-struct
           #:describe-alien-type
           #:describe-alien-union
           #:describe-class
           #:describe-function
           #:describe-inspectee
           #:describe-setf-function
           #:describe-symbol
           #:describe-type
           #:disassemble-symbol
           #:documentation-symbol
           #:eval-in-frame
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
           #:wait-goahead
           #:who-binds
           #:who-calls
           #:who-macroexpands
           #:who-references
           #:who-sets
           ))

(in-package :swank)


;;;; TCP server

(defgeneric create-socket (port)
  (:documentation "Create a listening TCP socket on port PORT."))

(defgeneric local-port (socket)
  (:documentation "Return the local port number of SOCKET."))

(defgeneric close-socket (socket)
  (:documentation "Close the socket SOCKET."))

(defgeneric accept-connection (socket)
  (:documentation 
   "Accept a client connection on the listening socket SOCKET.  Return
a stream for the new connection."))

(defgeneric add-input-handler (socket fn)
  (:documentation "Call FN whenever SOCKET is readable."))

(defgeneric remove-input-handlers (socket)
  (:documentation "Remove all input handlers for SOCKET."))

;;; Base condition for networking errors.
(define-condition network-error (error) ())

(defgeneric emacs-connected ()
  (:documentation
   "Hook called when the first connection from Emacs is established.
Called from the INIT-FN of the socket server that accepts the
connection.

This is intended for setting up extra context, e.g. to discover
that the calling thread is the one that interacts with Emacs."))

(defmethod no-applicable-method ((m (eql #'emacs-connected)) &rest _)
  (declare (ignore _))
  nil)


;;;; Compilation

(defgeneric call-with-compilation-hooks (func)
  (:documentation
   "Call FUNC with hooks to trigger SLDB on compiler errors."))

(defmacro with-compilation-hooks ((&rest ignore) &body body)
  (declare (ignore ignore))
  `(call-with-compilation-hooks (lambda () (progn ,@body))))

(defgeneric compile-string-for-emacs (string &key buffer position)
  (:documentation
   "Compile source from STRING.  During compilation, compiler
conditions must be trapped and resignalled as COMPILER-CONDITIONs.

If supplied, BUFFER and POSITION specify the source location in Emacs.

Additionally, if POSITION is supplied, it must be added to source
positions reported in compiler conditions."))

(defgeneric compile-file-for-emacs (filename load-p)
  (:documentation
   "Compile FILENAME signalling COMPILE-CONDITIONs.
If LOAD-P is true, load the file after compilation."))

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

(defgeneric make-fn-streams (input-fn output-fn)
  (:documentation
   "Return character input and output streams backended by functions.
When input is needed, INPUT-FN is called with no arguments to
return a string.
When output is ready, OUTPUT-FN is called with the output as its
argument.

Output should be forced to OUTPUT-FN before calling INPUT-FN.

The streams are returned as two values."))


;;;; Documentation

(defgeneric arglist-string (function-name)
  (:documentation
   "Return the argument for FUNCTION-NAME as a string.
The result should begin and end with parenthesis."))

(defgeneric macroexpand-all (form)
  (:documentation
   "Recursively expand all macros in FORM.
Return the resulting form."))

(defgeneric describe-symbol-for-emacs (symbol)
  (:documentation
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
      :FUNCTION \"Constructs a simple-vector from the given objects.\")"))


;;;; Debugging

(defgeneric call-with-debugging-environment (debugger-loop-fn)
  (:documentation
   "Call DEBUGGER-LOOP-FN in a suitable debugging environment.

This function is called recursively at each debug level to invoke the
debugger loop. The purpose is to setup any necessary environment for
other debugger callbacks that will be called within the debugger loop.

For example, this is a reasonable place to compute a backtrace, switch
to safe reader/printer settings, and so on."))

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

(defgeneric debugger-info-for-emacs (start end)
  (:documentation
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
  ((0 \"(KERNEL::INTEGER-/-INTEGER 1 0)\")))"))

(defgeneric backtrace (start end)
  (:documentation
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
    (2 \"(SYS::%TOPLEVEL #<SYS::ENVIRONMENT #x394834>)\"))"))

(defgeneric frame-source-location-for-emacs (frame-number)
  (:documentation
  "Return the source location for FRAME-NUMBER."))

(defgeneric frame-catch-tags (frame-number)
  (:documentation
  "Return a list of XXX list of what? catch tags for a debugger
stack frame.  The results are undefined unless this is called
within the dynamic contour of a function defined by
DEFINE-DEBUGGER-HOOK."))

(defgeneric frame-locals (frame-number)
  (:documentation
   "Return a list of XXX local variable designators define me
for a debugger stack frame.  The results are undefined unless
this is called within the dynamic contour of a function defined
by DEFINE-DEBUGGER-HOOK."))
   
(defgeneric eval-in-frame (form frame-number)
  (:documentation
   "Evaluate a Lisp form in the lexical context of a stack frame
in the debugger.  The results are undefined unless called in the
dynamic contour of a function defined by DEFINE-DEBUGGER-HOOK.

FRAME-NUMBER must be a positive integer with 0 indicating the
frame which invoked the debugger.

The return value is the result of evaulating FORM in the
appropriate context."))


;;;; Queries

#+(or)
;;; This is probably a better interface than find-function-locations.
(defgeneric find-definitions (name)
  (:documentation
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
"))

(defgeneric find-function-locations (name)
  (:documentation
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
"))


;;;; Inspector

(defgeneric inspected-parts (object)
  (:documentation
   "Return a short description and a list of (LABEL . VALUE) pairs."))

(defgeneric describe-primitive-type (object)
  (:documentation
   "Return a string describing the primitive type of object."))


;;;; Multiprocessing

(defgeneric startup-multiprocessing ()
  (:documentation
   "Initialize multiprocessing, if necessary.

This function is called directly through the listener, not in an RPC
from Emacs. This is to support interfaces such as CMUCL's
MP::STARTUP-IDLE-AND-TOP-LEVEL-LOOPS which does not return like a
normal function."))

(defgeneric spawn (fn &key name)
  (:documentation "Create a new thread to call FN."))

(defgeneric thread-id ()
  (:documentation
   "Return a value that uniquely identifies the current thread.
Thread-IDs allow Emacs to refer to individual threads.

When called several times by the same thread, all return values are
EQUAL. The value has a READable printed representation that preserves
equality. The printed representation must be identical in Emacs Lisp
and Common Lisp, and short enough to include in the REPL prompt.

For example, a THREAD-ID could be an integer or a short ASCII string.

Systems that do not support multiprocessing return NIL."))

(defgeneric thread-name (thread-id)
  (:documentation
   "Return the name of the thread identified by THREAD-ID.

Thread names are be single-line strings and are meaningful to the
user. They do not have to be unique."))

(defgeneric make-lock (&key name)
  (:documentation
   "Make a lock for thread synchronization.
Only one thread may hold the lock (via CALL-WITH-LOCK-HELD) at a time."))

(defgeneric call-with-lock-held (lock function)
  (:documentation
   "Call FUNCTION with LOCK held, queueing if necessary."))


;;;;; Default implementation for non-MP systems

;;; Using NO-APPLICABLE-METHOD to supply a default implementation that
;;; works in systems that don't have multiprocessing.
;;; (Good or bad idea? -luke)

(defmethod no-applicable-method ((m (eql #'startup-multiprocessing)) &rest _)
  (declare (ignore _))
  nil)
(defmethod no-applicable-method ((m (eql #'thread-id)) &rest _)
  (declare (ignore _))
  nil)
(defmethod no-applicable-method ((m (eql #'thread-name)) &rest _)
  (declare (ignore _))
  "The One True Thread")
(defmethod no-applicable-method ((m (eql #'make-lock)) &rest _)
  (declare (ignore _))
  :null-lock)
(defmethod no-applicable-method ((m (eql #'call-with-lock-held)) &rest args)
  (funcall (second args)))

