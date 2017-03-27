;;;; -*- indent-tabs-mode: nil; outline-regexp: ";;;;;*"; -*-
;;;
;;; swank-abcl.lisp --- Armedbear CL specific code for SLIME.
;;;
;;; Adapted from swank-acl.lisp, Andras Simon, 2004
;;; New work by Alan Ruttenberg, 2016-7
;;;
;;; This code has been placed in the Public Domain.  All warranties
;;; are disclaimed.
;;;

(defpackage swank/abcl
  (:use cl swank/backend)
  (:import-from :java
                #:jcall #:jstatic
                #:jmethod
                #:jfield #:jfield-name
                #:jconstructor
                #:jnew-array #:jarray-length #:jarray-ref #:jnew-array-from-array
                #:jclass #:jnew #:jinstance-of-p #:jclass-superclass #:java-object #:jclass-interfaces
                #:java-exception))
(in-package swank/abcl)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :collect) ;just so that it doesn't spoil the flying letters
  (require :pprint)
  (require :gray-streams)
  (require :abcl-contrib)

  ;;; Probe for existence of a functioning abcl-introspect, loading
  ;;; it necessary conditions are met.
  (when (ignore-errors (and
                        (fboundp '(setf sys::function-plist))
                        (progn
                          (require :abcl-introspect)
                          (find "ABCL-INTROSPECT" *modules* :test 'equal))))
    ;; NOT WORKING
    ;; Record source information for DEFIMPLEMENTATION
    #+nil
    (defmacro defimplementation/abcl (name args &body body)
      `(sys::record-source-information-for-type ',name '(:swank-implementation ,name))
      `(swank-backend:defimplementation ,name ,args &body ,body))
    #+nil
    (setf (symbol-function 'swank-backend:defimplementation)
          (symbol-function 'swank/abcl::defimplementation/recording-source-information))))

(defimplementation gray-package-name ()
  "GRAY-STREAMS")

;; FIXME: switch to shared Gray stream implementation when bugs are
;; fixed in ABCL.  See: http://abcl.org/trac/ticket/373.
(progn
  (defimplementation make-output-stream (write-string)
    (ext:make-slime-output-stream write-string))

  (defimplementation make-input-stream (read-string)
    (ext:make-slime-input-stream read-string
                                 (make-synonym-stream '*standard-output*))))

(defimplementation call-with-compilation-hooks (function)
  (funcall function))

;;; swank-mop

;;dummies and definition

(defclass standard-slot-definition ()())

;(defun class-finalized-p (class) t)

(defun slot-definition-documentation (slot)
  (declare (ignore slot))
  #+nil (documentation slot 't))

(defun slot-definition-type (slot)
  (declare (ignore slot))
  t)

(defun class-prototype (class)
  (declare (ignore class))
  nil)

(defun generic-function-declarations (gf)
  (declare (ignore gf))
  nil)

(defun specializer-direct-methods (spec)
  (mop:class-direct-methods spec))

(defun slot-definition-name (slot)
  (mop:slot-definition-name slot))

(defun class-slots (class)
  (mop:class-slots class))

(defun method-generic-function (method)
  (mop:method-generic-function method))

(defun method-function (method)
  (mop:method-function method))

(defun slot-boundp-using-class (class object slotdef)
  (declare (ignore class))
  (system::slot-boundp object (slot-definition-name slotdef)))

(defun slot-value-using-class (class object slotdef)
  (declare (ignore class))
  (system::slot-value object (slot-definition-name slotdef)))

(defun (setf slot-value-using-class) (new class object slotdef )
  (declare (ignore class))
  (mop::%set-slot-value object (slot-definition-name slotdef) new))

(import-to-swank-mop
 '( ;; classes
   cl:standard-generic-function
   standard-slot-definition ;;dummy
   cl:method
   cl:standard-class
   #+#.(swank/backend:with-symbol 'compute-applicable-methods-using-classes
         'mop)
   mop:compute-applicable-methods-using-classes
   ;; standard-class readers
   mop:class-default-initargs
   mop:class-direct-default-initargs
   mop:class-direct-slots
   mop:class-direct-subclasses
   mop:class-direct-superclasses
   mop:eql-specializer
   mop:class-finalized-p
   mop:finalize-inheritance
   cl:class-name
   mop:class-precedence-list
   class-prototype ;;dummy
   class-slots
   specializer-direct-methods
   ;; eql-specializer accessors
   mop::eql-specializer-object
   ;; generic function readers
   mop:generic-function-argument-precedence-order
   generic-function-declarations ;;dummy
   mop:generic-function-lambda-list
   mop:generic-function-methods
   mop:generic-function-method-class
   mop:generic-function-method-combination
   mop:generic-function-name
   ;; method readers
   method-generic-function
   method-function
   mop:method-lambda-list
   mop:method-specializers
   mop:method-qualifiers
   ;; slot readers
   mop:slot-definition-allocation
   slot-definition-documentation ;;dummy
   mop:slot-definition-initargs
   mop:slot-definition-initform
   mop:slot-definition-initfunction
   slot-definition-name
   slot-definition-type ;;dummy
   mop:slot-definition-readers
   mop:slot-definition-writers
   slot-boundp-using-class
   slot-value-using-class
   set-slot-value-using-class
   mop:slot-makunbound-using-class))

;;;; TCP Server


(defimplementation preferred-communication-style ()
  :spawn)

(defimplementation create-socket (host port &key backlog)
  (ext:make-server-socket port))

(defimplementation local-port (socket)
  (jcall (jmethod "java.net.ServerSocket" "getLocalPort") socket))

(defimplementation close-socket (socket)
  (ext:server-socket-close socket))

(defimplementation accept-connection (socket
                                      &key external-format buffering timeout)
  (declare (ignore buffering timeout))
  (ext:get-socket-stream (ext:socket-accept socket)
                         :element-type (if external-format
                                           'character
                                           '(unsigned-byte 8))
                         :external-format (or external-format :default)))

;;;; UTF8

;; faster please!
(defimplementation string-to-utf8 (s)
  (jbytes-to-octets
   (jcall
    (jmethod "java.lang.String" "getBytes" "java.lang.String")
    s
    "UTF8")))

(defimplementation utf8-to-string (u)
  (jnew
   (jconstructor "org.armedbear.lisp.SimpleString"
                 "java.lang.String")
   (jnew (jconstructor "java.lang.String" "[B" "java.lang.String")
         (octets-to-jbytes u)
         "UTF8")))

(defun octets-to-jbytes (octets)
  (declare (type octets (simple-array (unsigned-byte 8) (*))))
  (let* ((len (length octets))
         (bytes (jnew-array "byte" len)))
    (loop for byte across octets
          for i from 0
          do (jstatic (jmethod "java.lang.reflect.Array"  "setByte"
                               "java.lang.Object" "int" "byte")
                      "java.lang.relect.Array"
                      bytes i byte))
    bytes))

(defun jbytes-to-octets (jbytes)
  (let* ((len (jarray-length jbytes))
         (octets (make-array len :element-type '(unsigned-byte 8))))
    (loop for i from 0 below len
          for jbyte = (jarray-ref jbytes i)
          do (setf (aref octets i) jbyte))
    octets))

;;;; External formats

(defvar *external-format-to-coding-system*
  '((:iso-8859-1 "latin-1" "iso-latin-1" "iso-8859-1")
    ((:iso-8859-1 :eol-style :lf)
     "latin-1-unix" "iso-latin-1-unix" "iso-8859-1-unix")
    (:utf-8 "utf-8")
    ((:utf-8 :eol-style :lf) "utf-8-unix")
    (:euc-jp "euc-jp")
    ((:euc-jp :eol-style :lf) "euc-jp-unix")
    (:us-ascii "us-ascii")
    ((:us-ascii :eol-style :lf) "us-ascii-unix")))

(defimplementation find-external-format (coding-system)
  (car (rassoc-if (lambda (x)
                    (member coding-system x :test #'equal))
                  *external-format-to-coding-system*)))

;;;; Unix signals

(defimplementation getpid ()
  (if (fboundp 'ext::get-pid)
      (ext::get-pid)       ;;; Introduced with abcl-1.5.0
      (handler-case
          (let* ((runtime
                  (java:jstatic "getRuntime" "java.lang.Runtime"))
                 (command
                  (java:jnew-array-from-array
                   "java.lang.String" #("sh" "-c" "echo $PPID")))
                 (runtime-exec-jmethod
                  ;; Complicated because java.lang.Runtime.exec() is
                  ;; overloaded on a non-primitive type (array of
                  ;; java.lang.String), so we have to use the actual
                  ;; parameter instance to get java.lang.Class
                  (java:jmethod "java.lang.Runtime" "exec"
                                (java:jcall
                                 (java:jmethod "java.lang.Object" "getClass")
                                 command)))
                 (process
                  (java:jcall runtime-exec-jmethod runtime command))
                 (output
                  (java:jcall (java:jmethod "java.lang.Process" "getInputStream")
                              process)))
            (java:jcall (java:jmethod "java.lang.Process" "waitFor")
                        process)
            (loop :with b :do
               (setq b
                     (java:jcall (java:jmethod "java.io.InputStream" "read")
                                 output))
               :until (member b '(-1 #x0a))	; Either EOF or LF
               :collecting (code-char b) :into result
               :finally (return
                          (parse-integer (coerce result 'string)))))
        (t () 0))))

(defimplementation lisp-implementation-type-name ()
  "armedbear")

(defimplementation set-default-directory (directory)
  (let ((dir (sys::probe-directory directory)))
    (when dir (setf *default-pathname-defaults* dir))
    (namestring dir)))


;;;; Misc

(defimplementation arglist (fun)
  (cond ((symbolp fun)
          (multiple-value-bind (arglist present)
              (sys::arglist fun)
            (when (and (not present)
                       (fboundp fun)
                       (typep (symbol-function fun)
                              'standard-generic-function))
              (setq arglist
                    (mop::generic-function-lambda-list (symbol-function fun))
                    present
                    t))
            (if present arglist :not-available)))
        (t :not-available)))

(defimplementation function-name (function)
  (if (fboundp 'sys::any-function-name) ;; abcl-1.5.0
      (sys::any-function-name function)
      (nth-value 2 (function-lambda-expression function))))

(defimplementation macroexpand-all (form &optional env)
  (ext:macroexpand-all form env))

(defimplementation collect-macro-forms (form &optional env)
  ;; Currently detects only normal macros, not compiler macros.
  (declare (ignore env))
  (with-collected-macro-forms (macro-forms)
      (handler-bind ((warning #'muffle-warning))
        (ignore-errors
          (compile nil `(lambda () ,(macroexpand-all form env)))))
    (values macro-forms nil)))

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
      (when (fboundp symbol)
        (maybe-push
         (cond ((macro-function symbol)     :macro)
	       ((special-operator-p symbol) :special-operator)
	       ((typep (fdefinition symbol) 'generic-function)
                :generic-function)
	       (t :function))
         (doc 'function)))
      (maybe-push
       :class (if (find-class symbol nil)
                  (doc 'class)))
      result)))

(defimplementation describe-definition (symbol namespace)
  (ecase namespace
    ((:variable :macro)
     (describe symbol))
    ((:function :generic-function)
     (describe (symbol-function symbol)))
    (:class
     (describe (find-class symbol)))))

(defimplementation describe-definition (symbol namespace)
  (ecase namespace
    (:variable
     (describe symbol))
    ((:function :generic-function)
     (describe (symbol-function symbol)))
    (:class
     (describe (find-class symbol)))))


;;;; Debugger

;; Copied from swank-sbcl.lisp.
;;
;; Notice that *INVOKE-DEBUGGER-HOOK* is tried before *DEBUGGER-HOOK*,
;; so we have to make sure that the latter gets run when it was
;; established locally by a user (i.e. changed meanwhile.)
(defun make-invoke-debugger-hook (hook)
  (lambda (condition old-hook)
    (if *debugger-hook*
        (funcall *debugger-hook* condition old-hook)
        (funcall hook condition old-hook))))

(defimplementation call-with-debugger-hook (hook fun)
  (let ((*debugger-hook* hook)
        (sys::*invoke-debugger-hook* (make-invoke-debugger-hook hook)))
    (funcall fun)))

(defimplementation install-debugger-globally (function)
  (setq *debugger-hook* function)
  (setq sys::*invoke-debugger-hook* (make-invoke-debugger-hook function)))

(defvar *sldb-topframe*)

(defimplementation call-with-debugging-environment (debugger-loop-fn)
  (let* ((magic-token (intern "SWANK-DEBUGGER-HOOK" 'swank))
         (*sldb-topframe*
          (second (member magic-token (sys:backtrace)
                          :key (lambda (frame)
                                 (first (sys:frame-to-list frame)))))))
    (funcall debugger-loop-fn)))

(defun backtrace (start end)
  "A backtrace without initial SWANK frames."
  (let ((backtrace (sys:backtrace)))
    (subseq (or (member *sldb-topframe* backtrace) backtrace)
            start end)))
(defun nth-frame (index)
  (nth index (backtrace 0 nil)))

(defimplementation compute-backtrace (start end)
  (let ((end (or end most-positive-fixnum)))
    (backtrace start end)))

(defun jss-p ()
  (and (member "JSS" *modules* :test 'string=) (intern "INVOKE-RESTARGS" "JSS")))
          
(defun matches-jss-call (form)
  (flet ((gensymp (s) (and (symbolp s) (null (symbol-package s))))
         (invokep (s)  (and (symbolp s) (eq s (jss-p)))))
    (let ((method
            (swank/match::select-match 
             form
             (((LAMBDA ((#'gensymp a) &REST (#'gensymp b)) 
                 ((#'invokep fun) (#'stringp c) (#'gensymp d) (#'gensymp e) . args)) . args) '=> c)
             (other nil))))
      method)))

;; Use princ cs write-string for lisp frames as it respects (print-object (function t))
;; Rewrite jss expansions to their unexpanded state
(defimplementation print-frame (frame stream)
  (if (typep frame 'sys::lisp-stack-frame)
      (if (not (jss-p))
          (princ (system:frame-to-list frame) stream)
          ;; rewrite jss forms as they would be written
          (let ((form (system:frame-to-list frame)))
            (if (eq (car form) (jss-p))
                (format stream "(#~s ~{~s~^~})" (second form) (list* (third  form) (fourth form)))
                (loop initially  (write-char #\( stream)
                      for (el . rest) on form
                      for method =  (swank/abcl::matches-jss-call el)
                      do
                         (cond (method 
                                (format stream "(#~s ~{~s~^~})" method (cdr el)))
                               (t
                                (prin1 el stream)))
                         (unless (null rest) (write-char #\space stream))
                      finally (write-char #\) stream)))))
      (write-string (sys:frame-to-string frame) stream)))

(defimplementation frame-locals (index)
  (when (typep (nth-frame index) 'sys::lisp-stack-frame) ;; java stack frames have no locals available
    (loop
       :for id :upfrom 0
       :with frame = (java:jcall "toLispList" (nth-frame index))
       :with operator = (first frame)
       :with values = (rest frame)
       :with arglist = (if (and operator (consp values))
                           (jvm::match-lambda-list
                            (multiple-value-list
                             (jvm::parse-lambda-list
                              (arglist operator)))
                            values)
                           :not-available)
       :for value in values
       :collecting (list
                    :name (if (consp arglist)
                              (nth id arglist)
                              (format nil "arg~A" id))
                    :id id
                    :value value))))

(defimplementation frame-var-value (index id)
 (elt (rest (java:jcall "toLispList" (nth-frame index))) id))

(defimplementation disassemble-frame (index)
  (sys::disassemble (frame-function (nth-frame index))))

(defun frame-function (frame)
  (let ((list (sys::frame-to-list frame)))
    (cond 
      ((keywordp (car list))
       (find (getf list :method) 
             (jcall "getDeclaredMethods" (jclass (getf list :class)))
             :key (lambda(e)(jcall "getName" e)) :test 'equal))
      (t (car list) ))))
       
(defimplementation frame-source-location (index)
  (let ((frame (nth-frame index)))
    (or (source-location (nth-frame index))
        `(:error ,(format nil "No source for frame: ~a" frame)))))

#+nil
(defimplementation eval-in-frame (form frame-number)
  (debugger:eval-form-in-context
   form
   (debugger:environment-of-frame (nth-frame frame-number))))

#+nil
(defimplementation return-from-frame (frame-number form)
  (let ((frame (nth-frame frame-number)))
    (multiple-value-call #'debugger:frame-return
      frame (debugger:eval-form-in-context
             form
             (debugger:environment-of-frame frame)))))

;;; XXX doesn't work for frames with arguments
#+nil
(defimplementation restart-frame (frame-number)
  (let ((frame (nth-frame frame-number)))
    (debugger:frame-retry frame (debugger:frame-function frame))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Compiler hooks

(defvar *buffer-name* nil)
(defvar *buffer-start-position*)
(defvar *buffer-string*)
(defvar *compile-filename*)

(defvar *abcl-signaled-conditions*)

(defun handle-compiler-warning (condition)
  (let ((loc (when (and jvm::*compile-file-pathname*
                        system::*source-position*)
               (cons jvm::*compile-file-pathname* system::*source-position*))))
    ;; filter condition signaled more than once.
    (unless (member condition *abcl-signaled-conditions*)
      (push condition *abcl-signaled-conditions*)
      (signal 'compiler-condition
              :original-condition condition
              :severity :warning
              :message (format nil "~A" condition)
              :location (cond (*buffer-name*
                               (make-location
                                (list :buffer *buffer-name*)
                                (list :offset *buffer-start-position* 0)))
                              (loc
                               (destructuring-bind (file . pos) loc
                                 (make-location
                                  (list :file (namestring (truename file)))
                                  (list :position (1+ pos)))))
                              (t
                               (make-location
                                (list :file (namestring *compile-filename*))
                                (list :position 1))))))))

(defimplementation swank-compile-file (input-file output-file
                                       load-p external-format
                                       &key policy)
  (declare (ignore external-format policy))
  (let ((jvm::*resignal-compiler-warnings* t)
        (*abcl-signaled-conditions* nil))
    (handler-bind ((warning #'handle-compiler-warning))
      (let ((*buffer-name* nil)
            (*compile-filename* input-file))
        (multiple-value-bind (fn warn fail)
            (compile-file input-file :output-file output-file)
          (values fn warn
                  (and fn load-p
                       (not (load fn)))))))))

(defimplementation swank-compile-string (string &key buffer position filename
                                                policy)
  (declare (ignore filename policy))
  (let ((jvm::*resignal-compiler-warnings* t)
        (*abcl-signaled-conditions* nil))
    (handler-bind ((warning #'handle-compiler-warning))
      (let ((*buffer-name* buffer)
            (*buffer-start-position* position)
            (*buffer-string* string)
            (sys::*source* (make-pathname :device "emacs-buffer" :name buffer))
            (sys::*source-position* position))
        (funcall (compile nil (read-from-string
                               (format nil "(~S () ~A)" 'lambda string))))
        t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; source location and users of it

(defgeneric source-location (object))

;; try to find some kind of source for internals
(defun implementation-source-location (arg)
  (let ((function (cond ((functionp arg)
                         arg)
                        ((and (symbolp arg) (fboundp arg)) 
                         (or (symbol-function arg) (macro-function arg))))))
    (when (typep function 'generic-function)
      (setf function (mop::funcallable-instance-function function)))
    ;; functions are execute methods of class
    (when (or (functionp function) (special-operator-p arg))
      (let ((fclass (jcall "getClass" function)))
        (let ((classname (jcall "getName" fclass)))
          (destructuring-bind (class local) (if (find #\$ classname)
                                                (split-string classname "\\$")
                                                (list classname (jcall "replaceFirst" classname "([^.]*\\.)*" "")))
            (unless (member local '("MacroObject" "CompiledClosure" "Closure") :test 'equal)
            ;; look for java source
            (let* ((partial-path   (substitute #\/ #\. class))
                   (java-path (concatenate 'string partial-path ".java"))
                   (found-in-source-path (find-file-in-path java-path *source-path*))) 
              ;; snippet for finding the internal class within the file
              (if found-in-source-path 
                  `((:primitive , local)
                    (:location ,found-in-source-path
                               (:line 0)
                               (:snippet ,(format nil "class ~a" local))))
                  ;; if not, look for the class file, and hope that
                  ;; emacs is configured to disassemble class entries in jars.
                  ;; I use jdc.el <https://github.com/m0smith/dotfiles/blob/master/.emacs.d/site-lisp/jdc.el>
                  ;; with jad <https://github.com/moparisthebest/jad>
                  ;; Also (setq sys::*disassembler* "jad -a -p")
                  (let ((class-in-source-path 
                          (find-file-in-path (concatenate 'string partial-path ".class") *source-path*)))
                    ;; no snippet, since internal class is in its own file
                    (if class-in-source-path `(:primitive (:location ,class-in-source-path (:line 0) nil)))))))))))))

(defun get-declared-field (class fieldname)
  (find fieldname (jcall "getDeclaredFields" class) :key 'jfield-name :test 'equal))

(defun symbol-defined-in-java (symbol)
  (loop  with internal-name1 = (jcall "replaceAll" (jcall "replaceAll" (string symbol) "\\*" "") "-" "_")
         with internal-name2 = (jcall "replaceAll" (jcall "replaceAll" (string symbol) "\\*" "_") "-" "_")
         for class in 
                   (load-time-value (mapcar
                                     'jclass
                                     '("org.armedbear.lisp.Package"
                                       "org.armedbear.lisp.Symbol"
                                       "org.armedbear.lisp.Debug"
                                       "org.armedbear.lisp.Extensions"
                                       "org.armedbear.lisp.JavaObject"
                                       "org.armedbear.lisp.Lisp"
                                       "org.armedbear.lisp.Pathname"
                                       "org.armedbear.lisp.Site")))
           thereis 
           (or (get-declared-field class internal-name1)
               (get-declared-field class internal-name2))))

(defun maybe-implementation-variable (s)
  (let ((field (symbol-defined-in-java s)))
    (and field
         (let ((class (jcall "getName" (jcall "getDeclaringClass" field))))
           (let* ((partial-path (substitute #\/ #\. class))
                  (java-path (concatenate 'string partial-path ".java"))
                  (found-in-source-path (find-file-in-path java-path *source-path*)))
             (if found-in-source-path
                 `(symbol (:location ,found-in-source-path (:line 0) (:snippet ,(format nil  "~s" (string s)))))))))))

(defun if-we-have-to-choose-one-choose-the-function (sources)
  (or (loop for spec in  sources
            for (dspec) = spec
            when (and (consp dspec) (eq (car dspec) :function))
            when (and (consp dspec) (member (car dspec) '(:swank-implementation :function)))
                 do (return-from if-we-have-to-choose-one-choose-the-function spec))
      (car sources)))
    
(defmethod source-location ((symbol symbol))
  (or (let ((maybe (if-we-have-to-choose-one-choose-the-function (get symbol 'sys::source))))
        (and maybe (second (slime-location-from-source-annotation symbol maybe))))
      ;; This below should be obsolete - it uses the old sys:%source
      ;; leave it here for now just in case
      (and (pathnamep (ext:source-pathname symbol))
           (let ((pos (ext:source-file-position symbol))
                 (path (namestring (ext:source-pathname symbol))))
             ;; boot.lisp gets recorded wrong
             (if (equal path "boot.lisp")
                 (setq path (second (find-file-in-path "org/armedbear/lisp/boot.lisp" *source-path*))))
             (cond ((ext:pathname-jar-p path)
                    `(:location
                      ;; strip off "jar:file:" = 9 characters
                      (:zip ,@(split-string (subseq path 9) "!/"))
                      ;; pos never seems right. Use function name.
                      (:function-name ,(string symbol))
                      (:align t)))
                   ((equal (pathname-device (ext:source-pathname symbol)) "emacs-buffer")
                    ;; conspire with swank-compile-string to keep the buffer
                    ;; name in a pathname whose device is "emacs-buffer".
                    `(:location
                      (:buffer ,(pathname-name (ext:source-pathname symbol)))
                      (:function-name ,(string symbol))
                      (:align t)))
                   (t
                    `(:location
                      (:file ,path)
                      ,(if pos
                           (list :position (1+ pos))
                           (list :function-name (string symbol)))
                      (:align t))))))
      (second (implementation-source-location symbol))))

(defmethod source-location ((frame sys::java-stack-frame))
  (destructuring-bind (&key class method file line) (sys:frame-to-list frame)
    (declare (ignore method))
    (let ((file (or (find-file-in-path file *source-path*)
                    (let ((f (format nil "~{~a/~}~a"
                                     (butlast (split-string class "\\."))
                                     file)))
                      (find-file-in-path f *source-path*)))))
      (and file
           `(:location ,file (:line ,line) ())))))

(defmethod source-location ((frame sys::lisp-stack-frame))
  (destructuring-bind (operator &rest args) (sys:frame-to-list frame)
    (declare (ignore args))
    (etypecase operator
      (function (source-location operator))
      (list nil)
      (symbol (source-location operator)))))

(defmethod source-location ((fun function))
  (if (sys::local-function-p fun)
      (source-location (sys::local-function-owner fun))
      (let ((name (function-name fun)))
        (and name (source-location name)))))

(defun system-property (name)
  (jstatic "getProperty" "java.lang.System" name))

(defun pathname-parent (pathname)
  (make-pathname :directory (butlast (pathname-directory pathname))))

(defun pathname-absolute-p (pathname)
  (eq (car (pathname-directory pathname)) ':absolute))

(defun split-string (string regexp)
  (coerce
   (jcall (jmethod "java.lang.String" "split" "java.lang.String")
               string regexp)
   'list))

(defun path-separator ()
  (jfield "java.io.File" "pathSeparator"))

(defun search-path-property (prop-name)
  (let ((string (system-property prop-name)))
    (and string
         (remove nil
                 (mapcar #'truename
                         (split-string string (path-separator)))))))

(defun jdk-source-path ()
  (let* ((jre-home (truename (system-property "java.home")))
         (src-zip (merge-pathnames "src.zip" (pathname-parent jre-home)))
         (truename (probe-file src-zip)))
    (and truename (list truename))))

(defun class-path ()
  (append (search-path-property "java.class.path")
          (search-path-property "sun.boot.class.path")))

(defvar *source-path*
  (remove nil 
          (append (search-path-property "user.dir")
                  (jdk-source-path)
                  ;; include lib jar files. contrib has lisp
                  ;; code. Would be good to build abcl.jar with source
                  ;; code as well
                  (list (sys::find-system-jar)
                        (sys::find-contrib-jar))))
                  ;; you should tell slime where the abcl sources are. In .swank.lisp I have:
                  ;; (push (probe-file "/Users/alanr/repos/abcl/src/") *SOURCE-PATH*)

"List of directories to search for source files.")

(defun zipfile-contains-p (zipfile-name entry-name)
  (let ((zipfile (jnew (jconstructor "java.util.zip.ZipFile"
                                               "java.lang.String")
                            zipfile-name)))
    (jcall
     (jmethod "java.util.zip.ZipFile" "getEntry" "java.lang.String")
     zipfile entry-name)))

;; Try to find FILENAME in PATH.  If found, return a file spec as
;; needed by Emacs.  We also look in zip files.
(defun find-file-in-path (filename path)
  (labels ((try (dir)
             (cond ((not (pathname-type dir))
                    (let ((f (probe-file (merge-pathnames filename dir))))
                      (and f `(:file ,(namestring f)))))
                   ((member (pathname-type dir) '("zip" "jar") :test 'equal)
                    (try-zip dir))
                   (t (error "strange path element: ~s" path))))
           (try-zip (zip)
             (let* ((zipfile-name (namestring (truename zip))))
               (and (zipfile-contains-p zipfile-name filename)
                    `(:zip ,zipfile-name  ,filename)))))
    (cond ((pathname-absolute-p filename) (probe-file filename))
          (t
           (loop for dir in path
                 if (try dir) return it)))))

(defparameter *definition-types*
  '(:variable defvar
    :constant defconstant
    :type deftype
    :symbol-macro define-symbol-macro
    :macro defmacro
    :compiler-macro define-compiler-macro
    :function defun
    :generic-function defgeneric
    :method defmethod
    :setf-expander define-setf-expander
    :structure defstruct
    :condition define-condition
    :class defclass
    :method-combination define-method-combination
    :package defpackage
    :transform :deftransform
    :optimizer :defoptimizer
    :vop :define-vop
    :source-transform :define-source-transform
    :ir1-convert :def-ir1-translator
    :declaration declaim
    :alien-type :define-alien-type)
  "Map SB-INTROSPECT definition type names to Slime-friendly forms")

(defun definition-specifier (type)
  "Return a pretty specifier for NAME representing a definition of type TYPE."
  (or (if (and (consp type) (getf *definition-types* (car type)))
       `(,(getf *definition-types* (car type)) ,(second type) ,@(third type) ,@(cdddr type))
       (getf *definition-types* type))
      type))

(defun stringify-method-specs (type)
  "return a (:method ..) location for slime"
  (let ((*print-case* :downcase))
    (flet ((p (a) (princ-to-string a)))
      (destructuring-bind (name qualifiers specializers) (cdr type)
        `(,(car type) ,(p name) ,(mapcar #'p specializers) ,@(mapcar #'p qualifiers))))))

;; for abcl source, check if it is still there, and if not, look in abcl jar instead
(defun maybe-redirect-to-jar (path)
  (setq path (namestring path))
  (if (probe-file path)
      path
      (if (search "/org/armedbear/lisp" path :test 'string=)
          (let ((jarpath (format nil "jar:file:~a!~a" (namestring (sys::find-system-jar)) 
                                 (subseq path (search "/org/armedbear/lisp" path)))))
            (if (probe-file jarpath) 
                jarpath
                path))
          path)))

(defimplementation find-definitions (symbol)
  (if (stringp symbol) 
      ;; allow a string to be passed. If it is package prefixed, remove the prefix 
      (setq symbol (intern (string-upcase 
                            (subseq symbol (1+ (or (position #\: symbol :from-end t) -1))))
                           'keyword)))
  (let ((sources nil)
        (implementation-variables nil)
        (implementation-functions nil))
    (loop for package in (list-all-packages)
          for sym = (find-symbol (string symbol) package)
          when (and sym (equal (symbol-package sym) package))
            do
               (when (sys::autoloadp symbol)
                 (sys::resolve symbol))
               (let ((source (or (get sym 'ext::source) (get sym 'sys::source)))
                     (i-var  (maybe-implementation-variable sym))
                     (i-fun  (implementation-source-location sym)))
                 (when source  (setq sources (append sources (or (get sym 'ext::source) (get sym 'sys::source)))))
                 (when i-var (push i-var implementation-variables))
                 (when i-fun (push i-fun implementation-functions))))
    (setq sources (remove-duplicates sources :test 'equalp))
    (append (remove-duplicates implementation-functions :test 'equalp)
            (mapcar (lambda(s) (slime-location-from-source-annotation symbol s)) sources)
            (remove-duplicates implementation-variables :test 'equalp))))

(defun slime-location-from-source-annotation (sym it)
  (destructuring-bind (what path pos) it
    ;; all of these are (defxxx forms, which is what :function locations look for in slime
    (let* ((isfunction  (and (consp what)
                             (member (car what)
                                     '(:function :generic-function :macro :class
                                       :compiler-macro :type :constant :variable
                                       :package :structure :condition))))
           (ismethod (and (consp what) (eq (car what) :method)))
           (<position> (cond (isfunction
                              (list :function-name (princ-to-string (second what))))
                             (ismethod
                              (stringify-method-specs what))
                             (t
                              (list :position (1+ (or pos 0))))))
          (path2 (if (eq path :top-level)
                     "emacs-buffer:*slime-repl*"
                     (maybe-redirect-to-jar path))))
      (when (atom what)
        (setq what (list what sym)))
      (list (definition-specifier what)
            (if (ext:pathname-jar-p path2)
                `(:location
                  ;; jar-pathname stores JAR path as first of DEVICE
                  (:zip ,@(pathname-device path2))
                  ;; pos never seems right. Use function name.
                  ,<position>
                  (:align t))
                ;; conspire with swank-compile-string to keep the
                ;; buffer name in a pathname whose device is
                ;; "emacs-buffer".
                  (if (eql 0 (search "emacs-buffer:" path2))
                      `(:location
                        (:buffer ,(subseq path2  (load-time-value (length "emacs-buffer:"))))
                        ,<position>
                        (:align t))
                      `(:location
                        (:file ,path2)
                        ,<position>
                        (:align t))))))))

(defimplementation list-callers (thing)
  (loop for caller in (sys::callers thing)
        when (typep caller 'method)
          append (let ((name (mop:generic-function-name
                              (mop:method-generic-function caller))))
                   (mapcar (lambda(s) (slime-location-from-source-annotation thing s))
                           (remove `(:method ,@(sys::method-spec-list caller))
                                   (get 
                                    (if (consp name) (second name) name)
                                    'sys::source)
                                   :key 'car :test-not 'equalp)))
        when (symbolp caller)
          append   (mapcar (lambda(s) (slime-location-from-source-annotation caller s))
                           (get caller 'sys::source))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Inspecting

(defvar *slime-inspector-hyperspec-in-browser* t
  "If t then invoking hyperspec within the inspector browses the hyperspec in an emacs buffer, otherwise respecting the value of browse-url-browser-function")

(defun hyperspec-do (name)
  (let ((form `(let ((browse-url-browser-function 
                       ,(if *slime-inspector-hyperspec-in-browser* 
                            '(lambda(a v) (eww a))
                            'browse-url-browser-function)))
                        (slime-hyperdoc-lookup ,name))))
    (swank::eval-in-emacs form t)))

;;; Although by convention toString() is supposed to be a
;;; non-computationally expensive operation this isn't always the
;;; case, so make its computation a user interaction.
(defparameter *to-string-hashtable* (make-hash-table))


(defmethod emacs-inspect ((o t))
  (let* ((type (type-of o))
         (class (ignore-errors (find-class type)))
         (jclass (and (typep  class 'sys::built-in-class)
                      (jcall "getClass" o))))
    (let ((parts (sys:inspected-parts o)))
      `((:label "Type: ") (:value ,(or class type)) (:Newline)
        ,@(if jclass 
              `((:label "Java type: ") (:value ,jclass) (:newline)))
        ,@(if parts
              (loop :for (label . value) :in parts
                 :appending (list
                             (list :label (string-capitalize label))
                             ": "
                             (list :value value (princ-to-string value)) '(:newline)))
              (list '(:label "No inspectable parts, dumping output of CL:DESCRIBE:")
                    '(:newline)
                    (with-output-to-string (desc) (describe o desc))))))))

(defmethod emacs-inspect ((string string))
  (swank::lcons* 
   '(:label "Value: ")  `(:value ,string ,(concatenate 'string "\"" string "\""))  '(:newline)
   `(:action "[Edit in emacs buffer]" ,(lambda() (swank::ed-in-emacs `(:string ,string))))
   '(:newline)
   (if (ignore-errors (jclass string))
       `(:line "Names java class" ,(jclass string))
       "")
   (if (and (jss-p) 
            (stringp (funcall (intern "LOOKUP-CLASS-NAME" :jss) string :return-ambiguous t :muffle-warning t)))
       `(:multiple
         (:label "Abbreviates java class: ")
         ,(let ((it (funcall (intern "LOOKUP-CLASS-NAME" :jss) string :return-ambiguous t :muffle-warning t)))
           `(:value ,(jclass it)))
         (:newline))
       "")
   (if (ignore-errors (find-package (string-upcase string)))
       `(:line "Names package" ,(find-package (string-upcase string)))
       "")
   (let ((symbols (loop for p in (list-all-packages)
                        for found = (find-symbol (string-upcase string))
                        when (and found (eq (symbol-package found) p)
                                  (or (fboundp found)
                                      (boundp found)
                                      (symbol-plist found)
                                      (ignore-errors (find-class found))))
                          collect found)))
     (if symbols
         `(:multiple (:label "Names symbols: ") 
                     ,@(loop for s in symbols
                             collect
                             (Let ((*package* (find-package :keyword))) 
                               `(:value ,s ,(prin1-to-string s))) collect " ") (:newline))
         ""))
   (call-next-method)))

(defmethod emacs-inspect ((o java:java-exception))
  (append (call-next-method)
          (list '(:newline) '(:label "Stack trace")
                      '(:newline)
                      (let ((w (jnew "java.io.StringWriter"))) 
                        (jcall "printStackTrace" (java:java-exception-cause o) (jnew "java.io.PrintWriter" w))
                        (jcall "toString" w)))))

(defmethod emacs-inspect ((slot mop::slot-definition))
  `("Name: "
    (:value ,(mop:slot-definition-name slot))
    (:newline)
    "Documentation:" (:newline)
    ,@(when (slot-definition-documentation slot)
            `((:value ,(slot-definition-documentation slot)) (:newline)))
    "Initialization:" (:newline)
    (:label "  Args: ") (:value ,(mop:slot-definition-initargs slot)) (:newline)
    (:label "  Form: ")  ,(if (mop:slot-definition-initfunction slot)
                     `(:value ,(mop:slot-definition-initform slot))
                     "#<unspecified>") (:newline)
                     (:label "  Function: ")
                     (:value ,(mop:slot-definition-initfunction slot))
                     (:newline)))

(defmethod emacs-inspect ((f function))
  `(,@(when (function-name f)
        `((:label "Name: ")
          ,(princ-to-string (sys::any-function-name f)) (:newline)))
      ,@(multiple-value-bind (args present) (sys::arglist f)
          (when present
            `((:label "Argument list: ")
              ,(princ-to-string args)
              (:newline))))
      ,@(when (documentation f t)
          `("Documentation:" (:newline)
                             ,(documentation f t) (:newline)))
      ,@(when (function-lambda-expression f)
          `((:label "Lambda expression:")
            (:newline) ,(princ-to-string
                         (function-lambda-expression f)) (:newline)))
      (:label "Function java class: ") (:value ,(jcall "getClass" f)) (:newline)
      ,@(when (jcall "isInstance"  (java::jclass "org.armedbear.lisp.CompiledClosure") f)
          `((:label "Closed over: ")
            ,@(loop
                 for el in (sys::compiled-closure-context f)
                 collect `(:value ,el)
                 collect " ")
            (:newline)))
      ,@(when (sys::get-loaded-from f)
          (list `(:label "Defined in: ")
                `(:value ,(sys::get-loaded-from f) ,(namestring (sys::get-loaded-from f)))
                '(:newline)))
      ,@(let ((fields (jcall "getDeclaredFields" (jcall "getClass" f))))
          (when (plusp (length fields))
            (list* '(:label "Internal fields: ") '(:newline)
                   (loop for field across fields
                      do (jcall "setAccessible" field t) ;;; not a great idea esp. wrt. Java9
                      append
                        (let ((value (jcall "get" field f)))
                          (list "  "
                                `(:label ,(jcall "getName" field))
                                ": "
                                `(:value ,value ,(princ-to-string value))
                                '(:newline)))))))
      ,@(when (and (function-name f) (symbolp (function-name f))
                   (eq (symbol-package (function-name f)) (find-package :cl)))
          (list '(:newline) (list :action "Lookup in hyperspec"
                                  (lambda () (hyperspec-do (symbol-name (function-name f))))
                                  :refreshp nil)
                '(:newline)))))

(defmethod emacs-inspect ((o java:java-object))
  (if (jinstance-of-p o (jclass "java.lang.Class"))
      (emacs-inspect-java-class o)
      (let ((to-string (lambda ()
                         (handler-case
                             (setf (gethash o *to-string-hashtable*)
                                   (jcall "toString" o))
                           (t (e)
                             (setf (gethash o *to-string-hashtable*)
                                   (format nil
                                           "Could not invoke toString(): ~A"
                                           e)))))))
        (append
         (if (gethash o *to-string-hashtable*)
             (label-value-line "toString()" (gethash o *to-string-hashtable*))
             `((:action "[compute toString()]" ,to-string) (:newline)))
         (loop :for (label . value) :in (sys:inspected-parts o)
               :appending (label-value-line label value))))))

(defmethod emacs-inspect ((slot mop::slot-definition))
  `("Name: "
    (:value ,(mop:slot-definition-name slot))
    (:newline)
    "Documentation:" (:newline)
    ,@(when (slot-definition-documentation slot)
            `((:value ,(slot-definition-documentation slot)) (:newline)))
    "Initialization:" (:newline)
    "  Args: " (:value ,(mop:slot-definition-initargs slot)) (:newline)
    "  Form: "  ,(if (mop:slot-definition-initfunction slot)
                     `(:value ,(mop:slot-definition-initform slot))
                     "#<unspecified>") (:newline)
                     "  Function: "
                     (:value ,(mop:slot-definition-initfunction slot))
                     (:newline)))

(defun inspector-java-fields (class)
  (loop for super = class then (jclass-superclass super)
        while super
        for fields = (jcall "getDeclaredFields" super)
        for fromline = nil then (list `(:label "From: ") `(:value ,super  ,(jcall "getName" super)) '(:newline))
        when (and (plusp (length fields)) fromline)
          append fromline
        append
        (loop for this across fields
              for pre = (subseq (jcall "toString" this)
                                0 
                                (1+ (position #\. (jcall "toString" this)  :from-end t)))
              collect "  "
              collect (list :value this pre)
              collect (list :strong-value this (jcall "getName" this) )
              collect '(:newline))))

(defun inspector-java-methods (class)
   (loop for super = class then (jclass-superclass super)
        while super
        for methods = (jcall "getDeclaredMethods" super)
        for fromline = nil then (list `(:label "From: ") `(:value ,super  ,(jcall "getName" super)) '(:newline))
        when (and (plusp (length methods)) fromline) append fromline
        append
        (loop for this across methods
          for desc = (jcall "toString" this)
          for paren =  (position #\( desc)
          for dot = (position #\. (subseq desc 0 paren) :from-end t)
          for pre = (subseq desc 0 dot)
          for name = (subseq desc dot paren)
          for after = (subseq desc paren)
          collect "  "
          collect (list :value this pre)
          collect (list :strong-value this name)
          collect (list :value this after)
          collect '(:newline))))

(defun emacs-inspect-java-class (class)
  (let ((has-superclasses (jclass-superclass class))
        (has-interfaces (plusp (length (jclass-interfaces class))))
        (fields (inspector-java-fields class))
        (path (jcall "getResource" 
                class
                (concatenate 'string "/" (substitute #\/ #\. (jcall "getName" class)) ".class"))))
    `((:label ,(format nil "Java Class: ~a" (jcall "getName" class) ))
      (:newline)
      ,@(when path (list `(:label ,"Path: ") `(:value ,path) '(:newline)))
      ,@(if has-superclasses 
            (list* '(:label "Superclasses: ") (butlast (loop for super = (jclass-superclass class) then (jclass-superclass super)
                            while super collect (list :value super (jcall "getName" super)) collect ", "))))
      ,@(if has-interfaces
            (list* '(:newline) '(:label "Implements Interfaces: ")
                   (butlast (loop for i across (jclass-interfaces class) collect (list :value i (jcall "getName" i)) collect ", "))))
      (:newline) (:label "Methods:") (:newline)
      ,@(inspector-java-methods class)
      ,@(if fields
            (list*
             '(:newline) '(:label "Fields:") '(:newline)
             fields)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Multithreading

(defimplementation spawn (fn &key name)
  (threads:make-thread (lambda () (funcall fn)) :name name))

(defvar *thread-plists* (make-hash-table) ; should be a weak table
  "A hashtable mapping threads to a plist.")

(defvar *thread-id-counter* 0)

(defimplementation thread-id (thread)
  (threads:synchronized-on *thread-plists*
    (or (getf (gethash thread *thread-plists*) 'id)
        (setf (getf (gethash thread *thread-plists*) 'id)
              (incf *thread-id-counter*)))))

(defimplementation find-thread (id)
  (find id (all-threads)
        :key (lambda (thread)
               (getf (gethash thread *thread-plists*) 'id))))

(defimplementation thread-name (thread)
  (threads:thread-name thread))

(defimplementation thread-status (thread)
  (format nil "Thread is ~:[dead~;alive~]" (threads:thread-alive-p thread)))

(defimplementation make-lock (&key name)
  (declare (ignore name))
  (threads:make-thread-lock))

(defimplementation call-with-lock-held (lock function)
  (threads:with-thread-lock (lock) (funcall function)))

(defimplementation current-thread ()
  (threads:current-thread))

(defimplementation all-threads ()
  (copy-list (threads:mapcar-threads #'identity)))

(defimplementation thread-alive-p (thread)
  (member thread (all-threads)))

(defimplementation interrupt-thread (thread fn)
  (threads:interrupt-thread thread fn))

(defimplementation kill-thread (thread)
  (threads:destroy-thread thread))

(defstruct mailbox
  (queue '()))

(defun mailbox (thread)
  "Return THREAD's mailbox."
  (threads:synchronized-on *thread-plists*
    (or (getf (gethash thread *thread-plists*) 'mailbox)
        (setf (getf (gethash thread *thread-plists*) 'mailbox)
              (make-mailbox)))))

(defimplementation send (thread message)
  (let ((mbox (mailbox thread)))
    (threads:synchronized-on mbox
      (setf (mailbox-queue mbox)
            (nconc (mailbox-queue mbox) (list message)))
      (threads:object-notify-all mbox))))

(defimplementation receive-if (test &optional timeout)
  (let* ((mbox (mailbox (current-thread))))
    (assert (or (not timeout) (eq timeout t)))
    (loop
     (check-slime-interrupts)
     (threads:synchronized-on mbox
       (let* ((q (mailbox-queue mbox))
              (tail (member-if test q)))
         (when tail
           (setf (mailbox-queue mbox) (nconc (ldiff q tail) (cdr tail)))
           (return (car tail)))
         (when (eq timeout t) (return (values nil t)))
         (threads:object-wait mbox 0.3))))))

(defimplementation quit-lisp ()
  (ext:exit))

(defimplementation call-with-syntax-hooks (fn)
  (let ((*print-case* :downcase))
    (funcall fn)))
;;;
#+#.(swank/backend:with-symbol 'package-local-nicknames 'ext)
(defimplementation package-local-nicknames (package)
  (ext:package-local-nicknames package))

;; all the defimplentations aren't compiled. Compile them. Set their
;; function name to be the same as the implementation name so
;; meta-. works.

(eval-when (:load-toplevel :execute)
  (loop for s in swank-backend::*interface-functions*
        for impl = (get s 'swank-backend::implementation)
        do (when (and impl (not (compiled-function-p impl)))
             (let ((name (gensym)))
               (compile name  impl)
               (let ((compiled (symbol-function name)))
                 (system::%set-lambda-name compiled (second (sys::lambda-name impl)))
                 (setf (get s 'swank-backend::implementation) compiled))))))

