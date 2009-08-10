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
;;; This is the beginning of a Slime backend for OpenMCL.  It has been
;;; tested only with OpenMCL version 0.14-030901 on Darwin --- I would
;;; be interested in hearing the results with other versions.
;;;
;;; Additionally, reporting the positions of warnings accurately requires
;;; a small patch to the OpenMCL file compiler, which may be found at:
;;;
;;;   http://www.jamesjb.com/slime/openmcl-warning-position.diff
;;;
;;; Things that work:
;;;
;;; * Evaluation of forms with C-M-x.
;;; * Compilation of defuns with C-c C-c.
;;; * File compilation with C-c C-k.
;;; * Most of the debugger functionality, except EVAL-IN-FRAME,
;;;   FRAME-SOURCE-LOCATION, and FRAME-CATCH-TAGS.
;;; * Macroexpanding with C-c RET.
;;; * Disassembling the symbol at point with C-c M-d.
;;; * Describing symbol at point with C-c C-d.
;;; * Compiler warnings are trapped and sent to Emacs using the buffer
;;;   position of the offending top level form.
;;; * Symbol completion and apropos.
;;;
;;; Things that sort of work:
;;;
;;; * WHO-CALLS is implemented but is only able to return the file a
;;;   caller is defined in---source location information is not
;;;   available.
;;;
;;; Things that aren't done yet:
;;;
;;; * Cross-referencing.
;;; * Due to unimplementation functionality the test suite does not
;;;   run correctly (it hangs upon entering the debugger).
;;;

(in-package :swank-backend)

(eval-when (:compile-toplevel :execute :load-toplevel)
  (assert (and (= ccl::*openmcl-major-version* 1)
               (>= ccl::*openmcl-minor-version* 3))
          () "This file needs CCL version 1.3 or newer"))

(import-from :ccl *gray-stream-symbols* :swank-backend)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require 'xref))

;;; swank-mop

(import-to-swank-mop
 '( ;; classes
   cl:standard-generic-function
   ccl::standard-slot-definition
   cl:method
   cl:standard-class
   ccl::eql-specializer
   openmcl-mop:finalize-inheritance
   ;; standard-class readers
   openmcl-mop:class-default-initargs
   openmcl-mop:class-direct-default-initargs
   openmcl-mop:class-direct-slots
   openmcl-mop:class-direct-subclasses
   openmcl-mop:class-direct-superclasses
   openmcl-mop:class-finalized-p
   cl:class-name
   openmcl-mop:class-precedence-list
   openmcl-mop:class-prototype
   openmcl-mop:class-slots
   openmcl-mop:specializer-direct-methods
   ;; eql-specializer accessors
   openmcl-mop:eql-specializer-object
   ;; generic function readers
   openmcl-mop:generic-function-argument-precedence-order
   openmcl-mop:generic-function-declarations
   openmcl-mop:generic-function-lambda-list
   openmcl-mop:generic-function-methods
   openmcl-mop:generic-function-method-class
   openmcl-mop:generic-function-method-combination
   openmcl-mop:generic-function-name
   ;; method readers
   openmcl-mop:method-generic-function
   openmcl-mop:method-function
   openmcl-mop:method-lambda-list
   openmcl-mop:method-specializers
   openmcl-mop:method-qualifiers
   ;; slot readers
   openmcl-mop:slot-definition-allocation
   ccl::slot-definition-documentation
   openmcl-mop:slot-value-using-class
   openmcl-mop:slot-definition-initargs
   openmcl-mop:slot-definition-initform
   openmcl-mop:slot-definition-initfunction
   openmcl-mop:slot-definition-name
   openmcl-mop:slot-definition-type
   openmcl-mop:slot-definition-readers
   openmcl-mop:slot-definition-writers
   openmcl-mop:slot-boundp-using-class
   openmcl-mop:slot-makunbound-using-class))

(defun specializer-name (spec)
  (etypecase spec
    (cons spec)
    (class (class-name spec))
    (ccl::eql-specializer `(eql ,(ccl::eql-specializer-object spec)))))

(defun swank-mop:compute-applicable-methods-using-classes (gf args)
  (let* ((methods (ccl::%gf-methods gf))
         (args-length (length args))
         (bits (ccl::inner-lfun-bits gf))
         arg-count res)
    (when methods
      (setq arg-count (length (ccl::%method-specializers (car methods))))
      (unless (<= arg-count args-length)
        (error "Too few args to ~s" gf))
      (unless (or (logbitp ccl::$lfbits-rest-bit bits)
                  (logbitp ccl::$lfbits-restv-bit bits)
                  (logbitp ccl::$lfbits-keys-bit bits)
                  (<= args-length 
                      (+ (ldb ccl::$lfbits-numreq bits) (ldb ccl::$lfbits-numopt bits))))
        (error "Too many args to ~s" gf))
      (let ((cpls (make-list arg-count)))
        (declare (dynamic-extent cpls))
        (do* ((args-tail args (cdr args-tail))
              (cpls-tail cpls (cdr cpls-tail)))
             ((null cpls-tail))
          (setf (car cpls-tail)
                (ccl::%class-precedence-list (car args-tail))))
        (flet ((%method-applicable-p (method args cpls)
                 (do* ((specs (ccl::%method-specializers method) (ccl::%cdr specs))
                       (args args (ccl::%cdr args))
                       (cpls cpls (ccl::%cdr cpls)))
                      ((null specs) t)
                   (let ((spec (ccl::%car specs)))
                     (if (typep spec 'ccl::eql-specializer)
                         (unless (subtypep (ccl::%car args) (class-of (ccl::eql-specializer-object spec)))
                           (return nil))
                         (unless (ccl:memq spec (ccl::%car cpls))
                           (return nil)))))))
          (dolist (m methods)
            (if (%method-applicable-p m args cpls)
                (push m res))))
        (ccl::sort-methods res cpls (ccl::%gf-precedence-list gf))))))

;;; TCP Server

(defimplementation preferred-communication-style ()
  :spawn)

(defimplementation create-socket (host port)
  (ccl:make-socket :connect :passive :local-port port 
                   :local-host host :reuse-address t))

(defimplementation local-port (socket)
  (ccl:local-port socket))

(defimplementation close-socket (socket)
  (close socket))

(defimplementation accept-connection (socket &key external-format
                                             buffering timeout)
  (declare (ignore buffering timeout))
  (when external-format
    (let ((keys (ccl::socket-keys socket)))
      (setf (getf keys :external-format) external-format
            (slot-value socket 'ccl::keys) keys)))
  (ccl:accept-connection socket :wait t))

(defvar *external-format-to-coding-system*
  '((:iso-8859-1 
     "latin-1" "latin-1-unix" "iso-latin-1-unix" 
     "iso-8859-1" "iso-8859-1-unix")
    (:utf-8 "utf-8" "utf-8-unix")))

(defimplementation find-external-format (coding-system)
  (car (rassoc-if (lambda (x) (member coding-system x :test #'equal))
                  *external-format-to-coding-system*)))

;;; Unix signals

(defimplementation call-without-interrupts (fn)
  (ccl:without-interrupts (funcall fn)))

(defimplementation getpid ()
  (ccl::getpid))

(defimplementation lisp-implementation-type-name ()
  "ccl")

;;; Arglist

(defimplementation arglist (fname)
  (multiple-value-bind (arglist binding) (arglist% fname)
    (if binding
        arglist
        :not-available)))

(defmethod arglist% ((f symbol))
  (ccl:arglist f))

(defmethod arglist% ((f function))
  (ccl:arglist (ccl:function-name f)))

(defimplementation function-name (function)
  (ccl:function-name function))

(defmethod declaration-arglist ((decl-identifier (eql 'optimize)))
  (let ((flags (ccl:declaration-information decl-identifier)))
    (if flags
        `(&any ,flags)
        (call-next-method))))

;;; Compilation

(defun handle-compiler-warning (condition)
  "Resignal a ccl:compiler-warning as swank-backend:compiler-warning."
  (signal (make-condition
           'compiler-condition
           :original-condition condition
           :message (compiler-warning-short-message condition)
           :source-context nil
           :severity (compiler-warning-severity condition)
           :location (source-note-to-source-location 
                      (ccl::compiler-warning-source-note condition)
                      (lambda () "Unknown source")))))

(defgeneric compiler-warning-severity (condition))
(defmethod compiler-warning-severity ((c ccl::compiler-warning)) :warning)
(defmethod compiler-warning-severity ((c ccl::style-warning)) :style-warning)

(defgeneric compiler-warning-short-message (condition))

;; Pretty much the same as ccl::report-compiler-warning but
;; without the source position and function name stuff.
(defmethod compiler-warning-short-message ((c ccl::compiler-warning))
  (with-accessors ((type ccl::compiler-warning-warning-type) 
                   (args ccl::compiler-warning-args) 
                   (nrefs ccl::compiler-warning-nrefs)) c
    (with-output-to-string (stream)
      (let ((format-string (cdr (assoc type ccl::*compiler-warning-formats*))))
        (typecase format-string
          (string (apply #'format stream format-string 
                         (ccl::adjust-compiler-warning-args type args)))
          (null (format stream "~A: ~S" type args))
          (t (funcall format-string c stream)))
        (let ((nrefs (cond ((numberp nrefs) nrefs)
                           ((consp nrefs) (length nrefs)))))
          (when (and nrefs (/= nrefs 1))
            (format stream " (~D references)" nrefs)))))))

(defimplementation call-with-compilation-hooks (function)
  (handler-bind ((ccl::compiler-warning 'handle-compiler-warning))
    (funcall function)))

(defimplementation swank-compile-file (input-file output-file
                                       load-p external-format)
  (with-compilation-hooks ()
    (compile-file input-file 
                  :output-file output-file 
                  :load load-p
                  :external-format external-format)))

(defun temp-file-name ()
  "Return a temporary file name to compile strings into."
  (ccl:%get-cstring (#_tmpnam (ccl:%null-ptr))))

(defimplementation swank-compile-string (string &key buffer position filename
                                         policy)
  (declare (ignore policy))
  (with-compilation-hooks ()
    (let ((temp-file-name (temp-file-name))
          (ccl:*save-source-locations* t))
      (unwind-protect
           (progn
             (with-open-file (s temp-file-name :direction :output 
                                :if-exists :error)
               (write-string string s))
             (let ((binary-filename (compile-temp-file
                                     temp-file-name filename buffer position)))
               (delete-file binary-filename)))
        (delete-file temp-file-name)))))

(defvar *temp-file-map* (make-hash-table :test #'equal)
  "A mapping from tempfile names to Emacs buffer names.")

(defun compile-temp-file (temp-file-name buffer-file-name buffer-name offset)
  (compile-file temp-file-name
                :load t
                :compile-file-original-truename 
                (or buffer-file-name
                    (progn 
                      (setf (gethash temp-file-name *temp-file-map*)
                            buffer-name)
                      temp-file-name))
                :compile-file-original-buffer-offset (1- offset)))

;;; Cross-referencing

(defun xref-locations (relation name &optional inverse)
  (mapcan #'find-definitions
          (if inverse 
              (ccl::get-relation relation name :wild :exhaustive t)
              (ccl::get-relation relation :wild name :exhaustive t))))

(defimplementation who-binds (name)
  (xref-locations :binds name))

(defimplementation who-macroexpands (name)
  (xref-locations :macro-calls name t))
  
(defimplementation who-references (name)
  (remove-duplicates
   (append (xref-locations :references name)
           (xref-locations :sets name)
           (xref-locations :binds name))
   :test 'equal))

(defimplementation who-sets (name)
  (xref-locations :sets name))

(defimplementation who-calls (name)
  (remove-duplicates
   (append
    (xref-locations :direct-calls name)
    (xref-locations :indirect-calls name)
    (xref-locations :macro-calls name t))
   :test 'equal))

(defimplementation who-specializes (class)
  (mapcar (lambda (m) 
            (car (find-definitions m)))
          (ccl::%class.direct-methods (find-class class))))

(defimplementation list-callees (name)
  (remove-duplicates
   (append
   (xref-locations :direct-calls name t)
   (xref-locations :macro-calls name nil))
   :test 'equal))

(defimplementation list-callers (symbol)
  (mapcan #'find-definitions (ccl::caller-functions symbol)))

;;; Profiling (alanr: lifted from swank-clisp)

(defimplementation profile (fname)
  (eval `(mon:monitor ,fname)))		;monitor is a macro

(defimplementation profiled-functions ()
  mon:*monitored-functions*)

(defimplementation unprofile (fname)
  (eval `(mon:unmonitor ,fname)))	;unmonitor is a macro

(defimplementation unprofile-all ()
  (mon:unmonitor))

(defimplementation profile-report ()
  (mon:report-monitoring))

(defimplementation profile-reset ()
  (mon:reset-all-monitoring))

(defimplementation profile-package (package callers-p methods)
  (declare (ignore callers-p methods))
  (mon:monitor-all package))

;;; Debugging

(defun openmcl-set-debug-switches ()
  (setq ccl::*fasl-save-definitions* nil)
  (setq ccl::*fasl-save-doc-strings* t)
  (setq ccl::*fasl-save-local-symbols* t)
  #+ppc (setq ccl::*ppc2-compiler-register-save-label* t)
  #+x86-64 (setq ccl::*x862-compiler-register-save-label* t)
  (setq ccl::*save-arglist-info* t)
  (setq ccl::*save-definitions* nil)
  (setq ccl::*save-doc-strings* t)
  (setq ccl::*save-local-symbols* t)
  (ccl::start-xref))

(defvar *sldb-stack-top* nil)
(defvar *sldb-stack-top-hint* nil)
(defvar *break-in-sldb* nil)

(defimplementation call-with-debugging-environment (debugger-loop-fn)
  (let* (;;(*debugger-hook* nil)
         (*sldb-stack-top* (or *sldb-stack-top-hint*
                               (guess-stack-top 2)))
         (*sldb-stack-top-hint* nil)
         ;; don't let error while printing error take us down
         (ccl::*signal-printing-errors* nil))
    (funcall debugger-loop-fn)))

(defimplementation call-with-debugger-hook (hook fun)
  (let ((*debugger-hook* hook)
        (*break-in-sldb* t))
    (funcall fun)))

(defimplementation install-debugger-globally (function)
  (setq *debugger-hook* function)
  (setq *break-in-sldb* t)
  ;;(setq ccl::*interactive-abort-process* ccl::*current-process*)
  )

(defun backtrace-context ()
  nil)

(labels ((error-entry? (frame)
           (let ((fun (ccl::cfp-lfun frame)))
             (or (eq fun #'ccl::%error)
                 (eq fun #'ccl::%pascal-functions%)))))

  (defun guess-stack-top (offset)
    ;; search the beginning of the stack for some well known functions
    (do ((ctx (backtrace-context))
         (result (ccl::%get-frame-ptr))
         (i 0 (1+ i))
         (frame (ccl::%get-frame-ptr) (ccl::parent-frame frame ctx))
         (last nil frame))
        (nil)
      (cond ((or (not frame) (or (> i (+ offset 7))))
             (return result))
            ((or (= i offset) (and last (error-entry? last)))
             (setq result frame))))))

(defun map-backtrace (function &optional
                      (start-frame-number 0)
                      (end-frame-number most-positive-fixnum))
  "Call FUNCTION passing information about each stack frame
 from frames START-FRAME-NUMBER to END-FRAME-NUMBER."
  (let ((context (backtrace-context))
        (frame-number 0)
        (top-stack-frame (or *sldb-stack-top*
                             (ccl::%get-frame-ptr))))
    (do ((p top-stack-frame (ccl::parent-frame p context)))
        ((null p))
      (multiple-value-bind (lfun pc) (ccl::cfp-lfun p)
        (when lfun
          (if (and (>= frame-number start-frame-number)
                   (< frame-number end-frame-number))
              (funcall function frame-number p context lfun pc))
          (incf frame-number))))))

(defun frame-arguments (p context lfun pc)
  "Returns a list representing the arguments of a frame."
  (multiple-value-bind (args types names)
      (ccl::frame-supplied-args p lfun pc nil context)
    (loop for value in args
          for type in types
          for name in names
          append (cond ((equal type "keyword")
                        (list (intern (symbol-name name) "KEYWORD") value))
                       (t (list value))))))

(defimplementation compute-backtrace (start-frame-number end-frame-number)
  (let (result)
    (map-backtrace (lambda (frame-number p context lfun pc)
                     (declare (ignore frame-number))
                     (push (list :frame p context lfun pc)
                           result))
                   start-frame-number end-frame-number)
    (nreverse result)))

(defimplementation print-frame (frame stream)
  (assert (eq (first frame) :frame))
  (destructuring-bind (p context lfun pc) (rest frame)
    (format stream "(~S~{ ~S~})"
            (or (ccl::function-name lfun) lfun)
            (frame-arguments p context lfun pc))))

(defun call/frame (frame-number if-found)
  (map-backtrace  
   (lambda (fnumber p context lfun pc)
     (when (= fnumber frame-number)
       (return-from call/frame 
         (funcall if-found p context lfun pc))))))

(defmacro with-frame ((p context lfun pc) frame-number &body body)
  `(call/frame ,frame-number (lambda (,p ,context ,lfun ,pc) . ,body)))

(defimplementation frame-var-value (frame var)
  (with-frame (p context lfun pc) frame
    (cadr (nth var (frame-visible-variables p context lfun pc)))))

(defimplementation frame-locals (index)
  (with-frame (p context lfun pc) index
    (loop for (name value) in (frame-visible-variables p context lfun pc)
          collect (list :name name :value value :id 0))))

(defun frame-visible-variables (p context lfun pc)
  "Return a list ((NAME VALUE) ...) of the named variables for this frame."
  (multiple-value-bind (count vsp parent-vsp) 
      (ccl::count-values-in-frame p context)
    (let (result)
      (dotimes (i count)
        (multiple-value-bind (var type name)
            (ccl::nth-value-in-frame p i context lfun pc vsp parent-vsp)
          (declare (ignore type))
          (when name
            (let ((value (typecase var
                           (ccl::value-cell (ccl::uvref var 0))
                           (t var))))
              (push (list name value) result)))))
      (reverse result))))

(defimplementation frame-source-location (index)
  (with-frame (p context lfun pc) index
    (declare (ignore p context))
    (if pc
        (pc-source-location lfun pc)
        (function-source-location lfun))))

(defimplementation eval-in-frame (form index)
  (with-frame (p context lfun pc) index
    (let ((vars (frame-visible-variables p context lfun pc)))
      (eval `(let ,(loop for (var val) in vars collect `(,var ',val))
               (declare (ignorable ,@(mapcar #'car vars)))
               ,form)))))

(defimplementation return-from-frame (index form)
  (let ((values (multiple-value-list (eval-in-frame form index))))
    (with-frame (p context lfun pc) index
       (declare (ignore context lfun pc))
       (ccl::apply-in-frame p #'values values))))

(defimplementation restart-frame (index)
  (with-frame (p context lfun pc) index
    (ccl::apply-in-frame p lfun 
                         (ccl::frame-supplied-args p lfun pc nil context))))

(defimplementation disassemble-frame (the-frame-number)
  (with-frame (p context lfun pc) the-frame-number
    (format t "LFUN: ~a~%PC: ~a  FP: #x~x  CONTEXT: ~a~%" lfun pc p context)
    (disassemble lfun)))

;; BREAK 

(ccl::advise ccl::cbreak-loop
             (if *break-in-sldb* 
                 (apply #'break-in-sldb ccl::arglist)
                 (:do-it))
             :when :around
             :name sldb-break)

(defun break-in-sldb (msg cont-string condition error-pointer)
  (let ((*sldb-stack-top-hint* error-pointer))
    (with-simple-restart (continue "~a" cont-string)
      (funcall (read-from-string "SWANK:INVOKE-SLIME-DEBUGGER")
               (condition-for-break condition msg)))))

(defun condition-for-break (condition msg)
  (cond ((and (eq (type-of condition) 'simple-condition)
              (equal (simple-condition-format-control condition) ""))
         (make-condition 'simple-condition :format-control "~a" 
                         :format-arguments (list msg)))
        (t condition)))


;; CCL commit r11373 | gz | 2008-11-16 16:35:28 +0100 (Sun, 16 Nov 2008)
;; contains some interesting details:
;; 
;; Source location are recorded in CCL:SOURCE-NOTE's, which are objects
;; with accessors CCL:SOURCE-NOTE-FILENAME, CCL:SOURCE-NOTE-START-POS,
;; CCL:SOURCE-NOTE-END-POS and CCL:SOURCE-NOTE-TEXT.  The start and end
;; positions are file positions (not character positions).  The text will
;; be NIL unless text recording was on at read-time.  If the original
;; file is still available, you can force missing source text to be read
;; from the file at runtime via CCL:ENSURE-SOURCE-NOTE-TEXT.
;; 
;; Source-note's are associated with definitions (via record-source-file)
;; and also stored in function objects (including anonymous and nested
;; functions).  The former can be retrieved via
;; CCL:FIND-DEFINITION-SOURCES, the latter via CCL:FUNCTION-SOURCE-NOTE.
;; 
;; The recording behavior is controlled by the new variable
;; CCL:*SAVE-SOURCE-LOCATIONS*:
;; 
;;   If NIL, don't store source-notes in function objects, and store only
;;   the filename for definitions (the latter only if
;;   *record-source-file* is true).
;; 
;;   If T, store source-notes, including a copy of the original source
;;   text, for function objects and definitions (the latter only if
;;   *record-source-file* is true).
;; 
;;   If :NO-TEXT, store source-notes, but without saved text, for
;;   function objects and defintions (the latter only if
;;   *record-source-file* is true).  This is the default.
;; 
;; PC to source mapping is controlled by the new variable
;; CCL:*RECORD-PC-MAPPING*.  If true (the default), functions store a
;; compressed table mapping pc offsets to corresponding source locations.
;; This can be retrieved by (CCL:FIND-SOURCE-NOTE-AT-PC function pc)
;; which returns a source-note for the source at offset pc in the
;; function.
;; 
;; Currently the only thing that makes use of any of this is the
;; disassembler.  ILISP and current version of Slime still use
;; backward-compatible functions that deal with filenames only.  The plan
;; is to make Slime, and our IDE, use this eventually.

(defun function-source-location (function)
  (source-note-to-source-location
   (ccl:function-source-note function)
   (lambda ()
     (format nil "Function has no source note: ~A" function))))

(defun pc-source-location (function pc)
  (source-note-to-source-location
   (or (ccl:find-source-note-at-pc function pc)
       (ccl:function-source-note function))
   (lambda ()
     (format nil "No source note at PC: ~a[~d]" function pc))))

(defun source-note-to-source-location (note if-nil-thunk)
  (labels ((filename-to-buffer (filename)
             (cond ((gethash filename *temp-file-map*)
                    (list :buffer (gethash filename *temp-file-map*)))
                   ((probe-file filename)
                    (list :file (namestring (truename filename))))
                   (t (error "File ~s doesn't exist" filename)))))
    (cond (note
           (handler-case
               (make-location 
                (filename-to-buffer (ccl:source-note-filename note))
                (list :position (1+ (ccl:source-note-start-pos note))))
             (error (c) 
               ;;(break "~a" c)
               `(:error ,(princ-to-string c)))))
          (t `(:error ,(funcall if-nil-thunk))))))

(defimplementation find-definitions (obj)
  (loop for (loc . name) in (source-locations obj)
        collect (list name loc)))

;; Return a list ((LOC . NAME) ...) of possible src-locs.
(defgeneric source-locations (thing))

(defmethod source-locations ((f function))
  (list (cons (function-source-location f)
              (list 'function (ccl:function-name f)))))

(defmethod source-locations ((s symbol))
  (append
   #+(or)
   (if (and (fboundp s) 
            (not (macro-function s))
            (not (special-operator-p s))
            (functionp (symbol-function s)))
       (source-locations (symbol-function s)))
   (loop for ((type . name) source) in (ccl:find-definition-sources s)
         collect (cons (source-note-to-source-location 
                        source (lambda () "No source info available"))
                       (definition-name type name)))))

(defmethod source-locations ((m method))
  (list (cons (function-source-location (ccl::method-function m))
              (definition-name ccl::*method-definition-type* m))))

(defmethod source-locations ((xe ccl::xref-entry))
  (with-slots (ccl::name type method-qualifiers ccl::method-specializers) xe
    (let ((name (case type
                  (method 
                   `(,ccl::name ,@method-qualifiers ,ccl::method-specializers))
                  (t ccl::name))))
      (loop for ((type . name) src) in (ccl:find-definition-sources name type)
            collect (cons (source-note-to-source-location 
                           src (lambda () "No source-note available"))
                          (definition-name type name))))))
    
(defgeneric definition-name (type object)
  (:method ((type ccl::definition-type) object)
    (list (ccl::definition-type-name type) object)))

(defmethod definition-name ((type ccl::method-definition-type)
                            (met method))
  `(,(ccl::definition-type-name type)
     ,(ccl::method-name met)
     ,@(ccl::method-qualifiers met)
     ,(mapcar #'specializer-name (ccl::method-specializers met))))

;;; Utilities

(defimplementation describe-symbol-for-emacs (symbol)
  (let ((result '()))
    (flet ((doc (kind &optional (sym symbol))
             (or (documentation sym kind) :not-documented))
           (maybe-push (property value)
             (when value
               (setf result (list* property value result)))))
      (maybe-push
       :variable (when (boundp symbol)
                   (doc 'variable)))
      (maybe-push
       :function (if (fboundp symbol)
                     (doc 'function)))
      (maybe-push
       :setf (let ((setf-function-name (ccl::setf-function-spec-name 
                                        `(setf ,symbol))))
               (when (fboundp setf-function-name)
                 (doc 'function setf-function-name))))
      (maybe-push
       :type (when (ccl:type-specifier-p symbol)
               (doc 'type)))
      result)))

(defimplementation describe-definition (symbol namespace)
  (ecase namespace
    (:variable 
     (describe symbol))
    ((:function :generic-function)
     (describe (symbol-function symbol)))
    (:setf
     (describe (ccl::setf-function-spec-name `(setf ,symbol))))
    (:class
     (describe (find-class symbol)))
    (:type
     (describe (or (find-class symbol nil) symbol)))))

(defimplementation toggle-trace (spec)
  "We currently ignore just about everything."
  (ecase (car spec)
    (setf 
     (ccl:trace-function spec))
    ((:defgeneric)
     (ccl:trace-function (second spec)))
    ((:defmethod)
     (destructuring-bind (name qualifiers specializers) (cdr spec)
       (ccl:trace-function 
        (find-method (fdefinition name) qualifiers specializers)))))
  t)

;;; Macroexpansion

(defvar *value2tag* (make-hash-table))

(do-symbols (s (find-package 'arch))
  (if (and (> (length (symbol-name s)) 7)
	   (string= (symbol-name s) "SUBTAG-" :end1 7)
	   (boundp s)
	   (numberp (symbol-value s))
	   (< (symbol-value s) 255))
      (setf (gethash (symbol-value s) *value2tag*) s)))

(defimplementation macroexpand-all (form)
  (ccl:macroexpand-all form))

;;;; Inspection

(defimplementation describe-primitive-type (thing)
  (let ((typecode (ccl::typecode thing)))
    (if (gethash typecode *value2tag*)
	(string (gethash typecode *value2tag*))
	(string (nth typecode '(tag-fixnum tag-list tag-misc tag-imm))))))

(defun comment-type-p (type)
  (or (eq type :comment)
      (and (consp type) (eq (car type) :comment))))

(defmethod emacs-inspect ((o t))
  (let* ((inspector::*inspector-disassembly* t)
         (i (inspector::make-inspector o))
	 (count (inspector::compute-line-count i)))
    (loop for l from 0 below count append 
          (multiple-value-bind (value label type) (inspector::line-n i l)
            (etypecase type
              ((member nil :normal) 
               `(,(or label "") (:value ,value) (:newline)))
              ((member :colon) 
               (label-value-line label value))
              ((member :static) 
               (list (princ-to-string label) " " `(:value ,value) '(:newline)))
              ((satisfies comment-type-p)
               (list (princ-to-string label) '(:newline))))))))

(defmethod emacs-inspect :around ((o t))
  (if (or (uvector-inspector-p o)
          (not (ccl:uvectorp o)))
      (call-next-method)
      (let ((value (call-next-method)))
        (cond ((listp value)
               (append value
                       `((:newline)
                         (:value ,(make-instance 'uvector-inspector :object o)
                                 "Underlying UVECTOR"))))
              (t value)))))

(defclass uvector-inspector ()
  ((object :initarg :object)))

(defgeneric uvector-inspector-p (object)
  (:method ((object t)) nil)
  (:method ((object uvector-inspector)) t))

(defmethod emacs-inspect ((uv uvector-inspector))
  (with-slots (object) uv
    (loop for i below (ccl::uvsize object) append 
          (label-value-line (princ-to-string i) (ccl::uvref object i)))))

;;; Multiprocessing

(defvar *known-processes* 
  (make-hash-table :size 20 :weak :key :test #'eq)
  "A map from threads to mailboxes.")

(defvar *known-processes-lock* (ccl:make-lock "*known-processes-lock*"))

(defstruct (mailbox (:conc-name mailbox.)) 
  (mutex (ccl:make-lock "thread mailbox"))
  (semaphore (ccl:make-semaphore))
  (queue '() :type list))

(defimplementation spawn (fun &key name)
  (ccl:process-run-function 
   (or name "Anonymous (Swank)")
   (lambda ()
     (handler-bind ((ccl:process-reset (lambda (c) c nil)))
       (funcall fun)))))

(defimplementation thread-id (thread)
  (ccl::process-serial-number thread))

(defimplementation find-thread (id)
  (find id (ccl:all-processes) :key #'ccl::process-serial-number))

(defimplementation thread-name (thread)
  (ccl::process-name thread))

(defimplementation thread-status (thread)
  (format nil "~A" (ccl:process-whostate thread)))

(defimplementation thread-attributes (thread)
   (list :priority (ccl::process-priority thread)))

(defimplementation make-lock (&key name)
  (ccl:make-lock name))

(defimplementation call-with-lock-held (lock function)
  (ccl:with-lock-grabbed (lock)
    (funcall function)))

(defimplementation current-thread ()
  ccl:*current-process*)

(defimplementation all-threads ()
  (ccl:all-processes))

(defimplementation kill-thread (thread)
  (and (ccl:process-interrupt thread
                              (lambda () 
                                (ccl::maybe-finish-process-kill 
                                 ccl:*current-process* :kill)))
       (setf (ccl::process-kill-issued thread) t)))

(defimplementation thread-alive-p (thread)
  (not (ccl::process-exhausted-p thread)))

(defimplementation interrupt-thread (thread function)
  (ccl:process-interrupt 
   thread 
   (lambda ()
     (let ((*sldb-stack-top-hint* (or *sldb-stack-top-hint*
                                      (ccl::%get-frame-ptr))))
       (funcall function)))))
  
(defun mailbox (thread)
  (ccl:with-lock-grabbed (*known-processes-lock*)
    (or (gethash thread *known-processes*)
        (setf (gethash thread *known-processes*) (make-mailbox)))))

(defimplementation send (thread message)
  (assert message)
  (let* ((mbox (mailbox thread))
         (mutex (mailbox.mutex mbox)))
    (ccl:with-lock-grabbed (mutex)
      (setf (mailbox.queue mbox)
            (nconc (mailbox.queue mbox) (list message)))
      (ccl:signal-semaphore (mailbox.semaphore mbox)))))

(defimplementation receive-if (test &optional timeout)
  (let* ((mbox (mailbox ccl:*current-process*))
         (mutex (mailbox.mutex mbox)))
    (assert (or (not timeout) (eq timeout t)))
    (loop
     (check-slime-interrupts)
     (ccl:with-lock-grabbed (mutex)
       (let* ((q (mailbox.queue mbox))
              (tail (member-if test q)))
         (when tail 
           (setf (mailbox.queue mbox) 
                 (nconc (ldiff q tail) (cdr tail)))
           (return (car tail)))))
     (when (eq timeout t) (return (values nil t)))
     (ccl:timed-wait-on-semaphore (mailbox.semaphore mbox) 1))))

(defimplementation set-default-initial-binding (var form)
  (eval `(ccl::def-standard-initial-binding ,var ,form)))

(defimplementation quit-lisp ()
  (ccl::quit))

;;; Weak datastructures

(defimplementation make-weak-key-hash-table (&rest args)
  (apply #'make-hash-table :weak :key args))

(defimplementation make-weak-value-hash-table (&rest args)
  (apply #'make-hash-table :weak :value args))

(defimplementation hash-table-weakness (hashtable)
  (ccl::hash-table-weak-p hashtable))
