;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; swank-loader.lisp --- Compile and load the Slime backend.
;;;
;;; Created 2003, James Bielman <jamesjb@jamesjb.com>
;;;
;;; This code has been placed in the Public Domain.  All warranties
;;; are disclaimed.
;;;

(cl:defpackage :swank-loader
  (:use :common-lisp))

(in-package :swank-loader)

(defun make-swank-pathname (name &optional (type "lisp"))
  "Return a pathname with name component NAME in the Slime directory."
  (merge-pathnames (make-pathname :name name :type type)
                   (or *compile-file-pathname*
                       *load-pathname*
                       *default-pathname-defaults*)))

(defparameter *sysdep-pathnames*
  (mapcar #'make-swank-pathname 
          (append 
           '("nregex")
           #+cmu '("swank-source-path-parser"  "swank-source-file-cache" "swank-cmucl")
           #+sbcl '("swank-sbcl" "swank-source-path-parser" "swank-source-file-cache" "swank-gray")
           #+openmcl '("metering" "swank-openmcl" "swank-gray")
           #+lispworks '("swank-lispworks" "swank-gray")
           #+allegro '("swank-allegro" "swank-gray")
           #+clisp '("xref" "metering" "swank-clisp" "swank-gray")
           #+armedbear '("swank-abcl")
           )))

(defparameter *implementation-features*
  '(:allegro :sbcl :openmcl :cmu :clisp :ccl :corman :armedbear :gcl))

(defparameter *os-features*
  '(:macosx :linux :windows :solaris :darwin :sunos :unix))

(defparameter *architecture-features*
  '(:powerpc :ppc :x86 :x86-64 :i686 :pc386 :sparc))

(defun unique-directory-name ()
  "Return a name that can be used as a directory name that is
unique to a Lisp implementation, Lisp implementation version,
operating system, and hardware architecture."
  (flet ((first-of (features)
           (loop for f in features
              when (find f *features*) return it)))
    (let ((lisp         (first-of *implementation-features*))
          (os           (first-of *os-features*))
          (architecture (first-of *architecture-features*))
          (version
           (block nil
             #+cmu
             (return (substitute #\- #\/ (lisp-implementation-version)))
             #+sbcl
             (return (lisp-implementation-version))
             #+gcl       
             (let ((s (lisp-implementation-version))) (subseq s 4))
             #+openmcl
             (return (format nil "~d.~d"
                             ccl::*openmcl-major-version*
                             ccl::*openmcl-minor-version*))
             #+lispworks
             (return (lisp-implementation-version))
             #+allegro
             (return excl::*common-lisp-version-number*)
             #+clisp
             (return (let ((s (lisp-implementation-version)))
                       (subseq s 0 (position #\space s))))
             #+armedbear
             (return "unknown")
             
             (warn "Don't know how to get Lisp implementation version.")
             (return "unknown"))))

      (unless lisp
        (warn "No implementation feature found in ~a."
              *implementation-features*)
        (setf lisp "unknown"))
      (unless os
        (warn "No os feature found in ~a." *os-features*)
        (setf os "unknown"))
      (unless architecture
        (warn "No architecture feature found in ~a."
              *architecture-features*)
        (setf architecture "unknown"))

      (format nil "~(~@{~a~^-~}~)" lisp version os architecture))))

(defparameter *swank-pathname* (make-swank-pathname "swank"))

(defun file-newer-p (new-file old-file)
  "Returns true if NEW-FILE is newer than OLD-FILE."
  (> (file-write-date new-file) (file-write-date old-file)))

(defun binary-pathname (source-pathname)
  "Return the pathname where SOURCE-PATHNAME's binary should be compiled."
  (let ((cfp (compile-file-pathname source-pathname)))
    (merge-pathnames (make-pathname
                      :directory
                      `(:relative ".slime" "fasl" ,(unique-directory-name))
                      :name (pathname-name cfp)
                      :type (pathname-type cfp))
                     (user-homedir-pathname))))

(defun compile-files-if-needed-serially (files)
  "Compile each file in FILES if the source is newer than
its corresponding binary, or the file preceding it was 
recompiled."
  (with-compilation-unit ()
    (let ((needs-recompile nil))
      (dolist (source-pathname files)
        (let ((binary-pathname (binary-pathname source-pathname)))
          (handler-case
              (progn
                (when (or needs-recompile
                          (not (probe-file binary-pathname))
                          (file-newer-p source-pathname binary-pathname))
                  (ensure-directories-exist binary-pathname)
                  (compile-file source-pathname :output-file binary-pathname
                                :print nil :verbose t)
                  (setq needs-recompile t))
                (load binary-pathname :verbose t))
            #+(or)
            (error ()
              ;; If an error occurs compiling, load the source instead
              ;; so we can try to debug it.
              (load source-pathname))
            ))))))

(compile-files-if-needed-serially
  (append (list (make-swank-pathname "swank-backend"))
          *sysdep-pathnames* 
          (list *swank-pathname*)))

(funcall (intern (string :warn-unimplemented-interfaces) :swank-backend))

(defun load-user-init-file ()
  "Load the user init file, return NIL if it does not exist."
  (load (merge-pathnames (user-homedir-pathname)
                         (make-pathname :name ".swank" :type "lisp"))
        :if-does-not-exist nil))
(export 'load-user-init-file)

(defun load-site-init-file ()
  (load (make-pathname :name "site-init" :type "lisp"
                       :defaults *load-truename*)
        :if-does-not-exist nil))

(or (load-site-init-file)
    (load-user-init-file))

