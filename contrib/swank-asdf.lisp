;;; swank-asdf.lisp -- ASDF support
;;
;; Authors: Daniel Barlow <dan@telent.net>
;;          Marco Baringer <mb@bese.it>
;;          Edi Weitz <edi@agharta.de>
;;          Francois-Rene Rideau <tunes@google.com>
;;          and others
;; License: Public Domain
;;

(in-package :swank)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *asdf-directory*
    (merge-pathnames #p"cl/asdf/" (user-homedir-pathname))
    "Directory in which your favorite and/or latest version
     of the ASDF source code is located"))

;;; Doing our best to load ASDF
;; First, try loading asdf from your implementation
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (member :asdf *features*)
    (ignore-errors (require "asdf"))))

;; If not found, load asdf from wherever the user specified it
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (member :asdf *features*)
    (handler-bind ((warning #'muffle-warning))
      (ignore-errors (load (make-pathname :name "asdf" :type "lisp"
                                          :defaults *asdf-directory*))))))
;; If still not found, error out.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (member :asdf *features*)
    (error "Could not load ASDF.
Please install ASDF2 and in your ~~/.swank.lisp specify:
 (defparameter swank::*asdf-directory* #p\"/path/containing/asdf/\")")))

;;; If ASDF is found, try to upgrade it to the latest installed version.
;; (eval-when (:compile-toplevel :load-toplevel :execute)
;;   (handler-bind ((warning #'muffle-warning))
;;     (pushnew *asdf-directory* asdf:*central-registry*)
;;     (asdf:oos 'asdf:load-op :asdf)))

;;; If ASDF is too old, punt.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (or #+asdf2 (asdf:version-satisfies (asdf:asdf-version) "2.000"))
    (error "Your ASDF version is too old.
Please upgrade to ASDF2 and in your ~~/.swank.lisp specify:
 (defparameter swank::*asdf-directory* #p\"/path/containing/asdf/\")")))

;;; Import functionality from ASDF that isn't available in all ASDF versions.
;;; Please do NOT depend on any of the below as reference:
;;; they are sometimes stripped down versions, for compatibility only.
;;;
;;; The way I got these is usually by looking at the current definition,
;;; using git blame in one screen to locate which commit last modified it,
;;; and git log in another to determine which release that made it in.
;;; It is OK for some of the below definitions to be or become obsolete,
;;; as long as it will make do with versions older than the tagged version:
;;; if ASDF is more recent, its more recent version will win.
;;;
;;; If your software is hacking ASDF, use its internals.
;;; If you want ASDF utilities in user software, please use ASDF-UTILS.

(defun asdf-at-least (version)
  (or #+asdf2 (asdf:version-satisfies
               (asdf:asdf-version) version)))

(defmacro asdefs (version &rest defs)
  (flet ((defun* (version name aname rest)
           `(progn
              (defun ,name ,@rest)
              (declaim (notinline ,name))
              (when (asdf-at-least ,version)
                (setf (fdefinition ',name) (fdefinition ',aname)))))
         (defvar* (name aname rest)
           `(progn
              (define-symbol-macro ,name ,aname)
              (defvar ,aname ,@rest))))
    `(progn
       ,@(loop :for (def name . args) :in defs
               :for aname = (intern (string name) :asdf)
               :collect
               (ecase def
                 ((defun) (defun* version name aname args))
                 ((defvar) (defvar* name aname args)))))))

(asdefs "2.015"
 (defvar *wild* #-cormanlisp :wild #+cormanlisp "*"))

(asdefs "2.010"
 (defun collect-sub*directories (directory collectp recursep collector)
   (when (funcall collectp directory)
     (funcall collector directory))
   (dolist (subdir (subdirectories directory))
     (when (funcall recursep subdir)
       (collect-sub*directories subdir collectp recursep collector)))))

(asdefs "2.011"
 (defun find-symbol* (s p)
   (find-symbol (string s) p)))

(asdefs "2.012"
 (defvar *wild-file*
   (make-pathname :name *wild* :type *wild*
                  :version (or #-(or abcl xcl) *wild*) :directory nil))
 (defvar *wild-directory*
   (make-pathname :directory `(:relative ,*wild*)
                  :name nil :type nil :version nil)))

(asdefs "2.014"
 (defun ensure-directory-pathname (pathspec)
   (cond
     ((stringp pathspec)
      (ensure-directory-pathname (pathname pathspec)))
     ((not (pathnamep pathspec))
      (error "Invalid pathname designator ~S" pathspec))
     ((wild-pathname-p pathspec)
      (error "Can't reliably convert wild pathname ~S" pathspec))
     ((asdf::directory-pathname-p pathspec)
      pathspec)
     (t
      (make-pathname :directory (append (or (pathname-directory pathspec)
                                            (list :relative))
                                        (list (file-namestring pathspec)))
                     :name nil :type nil :version nil
                     :defaults pathspec)))))

(asdefs "2.015"
 (defun collect-asds-in-directory (directory collect)
   (map () collect (directory-asd-files directory)))

 (defun register-asd-directory (directory &key recurse exclude collect)
   (if (not recurse)
       (collect-asds-in-directory directory collect)
       (collect-sub*directories-asd-files
        directory :exclude exclude :collect collect))))

(asdefs "2.016"
 (defun load-sysdef (name pathname)
   (declare (ignore name))
   (let ((package (asdf::make-temporary-package)))
     (unwind-protect
          (let ((*package* package)
                (*default-pathname-defaults*
                  (asdf::pathname-directory-pathname
                   (translate-logical-pathname pathname))))
            (asdf::asdf-message
             "~&; Loading system definition from ~A into ~A~%"
             pathname package)
            (load pathname))
     (delete-package package))))

 (defun directory* (pathname-spec &rest keys &key &allow-other-keys)
   (apply 'directory pathname-spec
          (append keys
                  '#.(or #+allegro
                         '(:directories-are-files nil
                           :follow-symbolic-links nil)
                         #+clozure
                         '(:follow-links nil)
                         #+clisp
                         '(:circle t :if-does-not-exist :ignore)
                         #+(or cmu scl)
                         '(:follow-links nil :truenamep nil)
                         #+sbcl
                         (when (find-symbol "RESOLVE-SYMLINKS" '#:sb-impl)
                           '(:resolve-symlinks nil)))))))
(asdefs "2.017"
 (defun collect-sub*directories-asd-files
     (directory &key
                (exclude asdf::*default-source-registry-exclusions*)
                collect)
   (collect-sub*directories
    directory
    (constantly t)
    (lambda (x) (not (member (car (last (pathname-directory x)))
                             exclude :test #'equal)))
    (lambda (dir) (collect-asds-in-directory dir collect))))

 (defun system-source-directory (system-designator)
   (pathname-directory-pathname (asdf::system-source-file system-designator)))

 (defun filter-logical-directory-results (directory entries merger)
   (if (typep directory 'logical-pathname)
       (loop for f in entries
             when 
             (if (typep f 'logical-pathname)
                 f
                 (let ((u (ignore-errors (funcall merger f))))
                   (and u
                        (equal (ignore-errors (truename u))
                               (truename f))
                        u)))
             collect it)
       entries))

 (defun directory-asd-files (directory)
   (directory-files directory asdf::*wild-asd*)))

(asdefs "2.019"
    (defun subdirectories (directory)
      (let* ((directory (ensure-directory-pathname directory))
             #-(or abcl cormanlisp genera xcl)
             (wild (asdf::merge-pathnames*
                    #-(or abcl allegro cmu lispworks sbcl scl xcl)
                    *wild-directory*
                #+(or abcl allegro cmu lispworks sbcl scl xcl) "*.*"
                directory))
             (dirs
               #-(or abcl cormanlisp genera xcl)
               (ignore-errors
                (directory* wild . #.(or #+clozure '(:directories t :files nil)
                                         #+mcl '(:directories t))))
               #+(or abcl xcl) (system:list-directory directory)
               #+cormanlisp (cl::directory-subdirs directory)
               #+genera (fs:directory-list directory))
             #+(or abcl allegro cmu genera lispworks sbcl scl xcl)
             (dirs (loop for x in dirs
                         for d = #+(or abcl xcl) (extensions:probe-directory x)
                         #+allegro (excl:probe-directory x)
                         #+(or cmu sbcl scl) (asdf::directory-pathname-p x)
                         #+genera (getf (cdr x) :directory)
                         #+lispworks (lw:file-directory-p x)
                         when d collect #+(or abcl allegro xcl) d
                           #+genera (ensure-directory-pathname (first x))
                         #+(or cmu lispworks sbcl scl) x)))
        (filter-logical-directory-results
         directory dirs
         (let ((prefix (or (normalize-pathname-directory-component
                            (pathname-directory directory))
                           ;; because allegro returns NIL for #p"FOO:"
                           '(:absolute)))) 
           (lambda (d)
             (let ((dir (normalize-pathname-directory-component
                         (pathname-directory d))))
               (and (consp dir) (consp (cdr dir))
                    (make-pathname
                     :defaults directory :name nil :type nil :version nil
                     :directory
                     (append prefix
                             (make-pathname-component-logical
                              (last dir))))))))))))

(asdefs "2.21"
 (defun normalize-pathname-directory-component (directory)
   (cond
     #-(or cmu sbcl scl)
     ((stringp directory) `(:absolute ,directory) directory)
     #+gcl
     ((and (consp directory) (stringp (first directory)))
      `(:absolute ,@directory))
     ((or (null directory)
          (and (consp directory)
               (member (first directory) '(:absolute :relative))))
      directory)
     (t
      (error "Unrecognized pathname directory component ~S" directory))))

 (defun make-pathname-component-logical (x)
   (typecase x
     ((eql :unspecific) nil)
     #+clisp (string (string-upcase x))
     #+clisp (cons (mapcar 'make-pathname-component-logical x))
     (t x)))

 (defun make-pathname-logical (pathname host)
   (make-pathname
    :host host
    :directory (make-pathname-component-logical (pathname-directory pathname))
    :name (make-pathname-component-logical (pathname-name pathname))
    :type (make-pathname-component-logical (pathname-type pathname))
    :version (make-pathname-component-logical (pathname-version pathname)))))

(asdefs "2.022"
 (defun directory-files (directory &optional (pattern *wild-file*))
   (let ((dir (pathname directory)))
     (when (typep dir 'logical-pathname)
       (when (wild-pathname-p dir)
         (error "Invalid wild pattern in logical directory ~S" directory))
       (unless (member (pathname-directory pattern)
                       '(() (:relative)) :test 'equal)
         (error "Invalid file pattern ~S for logical directory ~S"
                pattern directory))
       (setf pattern (make-pathname-logical pattern (pathname-host dir))))
     (let ((entries (ignore-errors
                     (directory* (asdf::merge-pathnames* pattern dir)))))
       (filter-logical-directory-results
        directory entries
        (lambda (f)
          (make-pathname :defaults dir
                         :name (make-pathname-component-logical
                                (pathname-name f))
                         :type (make-pathname-component-logical
                                (pathname-type f))
                         :version (make-pathname-component-logical
                                   (pathname-version f)))))))))

;;; Taken from ASDF 1.628
(defmacro while-collecting ((&rest collectors) &body body)
  (let ((vars (mapcar (lambda (x) (gensym (symbol-name x))) collectors))
        (initial-values (mapcar (constantly nil) collectors)))
    `(let ,(mapcar #'list vars initial-values)
       (flet ,(mapcar (lambda (c v) `(,c (x) (push x ,v) (values)))
               collectors vars)
         ,@body
         (values ,@(mapcar (lambda (v) `(reverse ,v)) vars))))))


;;; Now for SLIME-specific stuff

(defun find-operation (operation)
  (or (find-symbol* operation :asdf)
      (error "Couldn't find ASDF operation ~S" operation)))

(defun map-system-components (fn system)
  (map-component-subcomponents fn (asdf:find-system system)))

(defun map-component-subcomponents (fn component)
  (when component
    (funcall fn component)
    (when (typep component 'asdf:module)
      (dolist (c (asdf:module-components component))
        (map-component-subcomponents fn c)))))

;;; Maintaining a pathname to component table

(defvar *pathname-component* (make-hash-table :test 'equal))

(defun clear-pathname-component-table ()
  (clrhash *pathname-component*))

(defun register-system-pathnames (system)
  (map-system-components 'register-component-pathname system))

(defun recompute-pathname-component-table ()
  (clear-pathname-component-table)
  (asdf::map-systems 'register-system-pathnames))

(defun pathname-component (x)
  (gethash (pathname x) *pathname-component*))

(defmethod asdf:component-pathname :around ((component asdf:component))
  (let ((p (call-next-method)))
    (setf (gethash p *pathname-component*) component)
    p))

(defun register-component-pathname (component)
  (asdf:component-pathname component))

(recompute-pathname-component-table)

;;; This is a crude hack, see ASDF's LP #481187.
(defslimefun who-depends-on (system)
  (flet ((system-dependencies (op system)
           (mapcar (lambda (dep)
                     (asdf::coerce-name (if (consp dep) (second dep) dep)))
                   (cdr (assoc op (asdf:component-depends-on op system))))))
    (let ((system-name (asdf::coerce-name system))
          (result))
      (asdf::map-systems
       (lambda (system)
         (when (member system-name
                       (system-dependencies 'asdf:load-op system)
                       :test #'string=)
           (push (asdf:component-name system) result))))
      result)))

(defmethod xref-doit ((type (eql :depends-on)) thing)
  (when (typep thing '(or string symbol))
    (loop for dependency in (who-depends-on thing)
          for asd-file = (asdf:system-definition-pathname dependency)
          when asd-file
          collect (list dependency
                        (swank-backend::make-location
                         `(:file ,(namestring asd-file))
                         `(:position 1)
                         `(:snippet ,(format nil "(defsystem :~A" dependency)
                           :align t))))))

(defslimefun operate-on-system-for-emacs (system-name operation &rest keywords)
  "Compile and load SYSTEM using ASDF.
Record compiler notes signalled as `compiler-condition's."
  (collect-notes
   (lambda ()
     (apply #'operate-on-system system-name operation keywords))))

(defun operate-on-system (system-name operation-name &rest keyword-args)
  "Perform OPERATION-NAME on SYSTEM-NAME using ASDF.
The KEYWORD-ARGS are passed on to the operation.
Example:
\(operate-on-system \"swank\" 'compile-op :force t)"
  (handler-case
      (with-compilation-hooks ()
	(apply #'asdf:operate (find-operation operation-name)
               system-name keyword-args)
        t)
    (asdf:compile-error () nil)))

(defun unique-string-list (&rest lists)
  (sort (delete-duplicates (apply #'append lists) :test #'string=) #'string<))

(defslimefun list-all-systems-in-central-registry ()
  "Returns a list of all systems in ASDF's central registry
AND in its source-registry. (legacy name)"
  (unique-string-list
   (mapcar
    #'pathname-name
    (while-collecting (c)
      (loop for dir in asdf:*central-registry*
            for defaults = (eval dir)
            when defaults
            do (collect-asds-in-directory defaults #'c))
      #+asdf2
      (progn
        (asdf:ensure-source-registry)
        (if (asdf:version-satisfies (asdf:asdf-version) "2.015")
            (loop :for k :being :the :hash-keys :of asdf::*source-registry*
                  :do (c k))
            (dolist (entry (asdf::flatten-source-registry))
              (destructuring-bind (directory &key recurse exclude) entry
                (register-asd-directory
                 directory
                 :recurse recurse :exclude exclude :collect #'c)))))))))

(defslimefun list-all-systems-known-to-asdf ()
  "Returns a list of all systems ASDF knows already."
  (while-collecting (c)
    (asdf::map-systems (lambda (system) (c (asdf:component-name system))))))

(defslimefun list-asdf-systems ()
  "Returns the systems in ASDF's central registry and those which ASDF
already knows."
  (unique-string-list
   (list-all-systems-known-to-asdf)
   (list-all-systems-in-central-registry)))

(defun asdf-component-source-files (component)
  (while-collecting (c)
    (labels ((f (x)
               (typecase x
                 (asdf:source-file (c (asdf:component-pathname x)))
                 (asdf:module (map () #'f (asdf:module-components x))))))
      (f component))))

(defun asdf-component-output-files (component)
  (while-collecting (c)
    (labels ((f (x)
               (typecase component
                 (asdf:source-file
                  (map () #'c
                       (asdf:output-files (make-instance 'asdf:compile-op) x)))
                 (asdf:module (map () #'f (asdf:module-components x))))))
      (f component))))

(defslimefun asdf-system-files (name)
  (let* ((system (asdf:find-system name))
         (files (mapcar #'namestring
                        (cons
                         (asdf:system-definition-pathname system)
                         (asdf-component-source-files system))))
         (main-file (find name files
                          :test #'equalp :key #'pathname-name :start 1)))
    (if main-file
        (cons main-file (remove main-file files
                                :test #'equal :count 1))
        files)))

(defslimefun asdf-system-loaded-p (name)
  (and (gethash 'asdf:load-op
                (asdf::component-operation-times (asdf:find-system name)))
       t))

(defslimefun asdf-system-directory (name)
  (namestring (asdf:system-source-directory name)))

(defun pathname-system (pathname)
  (let ((component (pathname-component pathname)))
    (when component
      (asdf:component-system component))))

(defslimefun asdf-determine-system (file buffer-package-name)
  (or
   (pathname-system file)
   (progn ; If not found, let's rebuild the table first
     (recompute-pathname-component-table)
     (pathname-system file))
   ;; If we couldn't find an already defined system,
   ;; try finding a system that's named like BUFFER-PACKAGE-NAME.
   (loop :with package = (guess-buffer-package buffer-package-name)
         :for name :in (package-names package)
         :for system = (asdf:find-system (asdf::coerce-name name) nil)
         :when system :do (register-system-pathnames system)
         :thereis (pathname-system file))))

(defslimefun delete-system-fasls (name)
  (let ((removed-count
         (loop for file in (asdf-component-output-files
                            (asdf:find-system name))
               when (probe-file file)
               count it
               and
               do (delete-file file))))
    (format nil "~d file~:p ~:*~[were~;was~:;were~] removed" removed-count)))

(defvar *recompile-system* nil)

(defmethod asdf:operation-done-p :around
    ((operation asdf:compile-op)
     component)
    (unless (eql *recompile-system*
                 (asdf:component-system component))
      (call-next-method)))

(defslimefun reload-system (name)
  (let ((*recompile-system* (asdf:find-system name)))
    (operate-on-system-for-emacs name 'asdf:load-op)))

;; Doing list-all-systems-in-central-registry might be quite slow
;; since it accesses a file-system, so run it once at the background
;; to initialize caches.
(eval-when (:load-toplevel :execute)
  (when (eql *communication-style* :spawn)
    (spawn (lambda ()
             (ignore-errors (list-all-systems-in-central-registry)))
           :name "init-asdf-fs-caches")))

;;; Hook for compile-file-for-emacs

(defun try-compile-file-with-asdf (pathname load-p &rest options)
  (declare (ignore options))
  (let ((component (pathname-component pathname)))
    (when component
      ;;(format t "~&Compiling ASDF component ~S~%" component)
      (let ((op (make-instance 'asdf:compile-op)))
        (asdf:perform op component)
        (when load-p
          (asdf:perform (make-instance 'asdf:load-op) component))
        (values t t nil (first (asdf:output-files op component)))))))

(defun try-compile-asd-file (pathname load-p &rest options)
  (declare (ignore load-p options))
  (when (equalp (pathname-type pathname) "asd")
    (load-sysdef (string-downcase (pathname-name pathname)) pathname)
    (values t t nil pathname)))

(pushnew 'try-compile-asd-file *compile-file-for-emacs-hook*)

;;; (pushnew 'try-compile-file-with-asdf *compile-file-for-emacs-hook*)

(provide :swank-asdf)
