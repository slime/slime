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
  (merge-pathnames name 
                   (make-pathname 
                    :type type
                    :device 
                    (pathname-device 
                     (or *compile-file-pathname* *load-pathname*
                         *default-pathname-defaults*))
                    :directory 
                    (pathname-directory
                     (or *compile-file-pathname* *load-pathname*
                         *default-pathname-defaults*)))))

(defparameter *sysdep-pathnames*
  (mapcar #'make-swank-pathname 
          (append 
           '("nregex")
           #+cmu '("swank-source-path-parser" "swank-cmucl")
           #+sbcl '("swank-sbcl" "swank-source-path-parser" "swank-gray")
           #+openmcl '("swank-openmcl" "swank-gray")
           #+lispworks '("swank-lispworks" "swank-gray")
           #+allegro '("swank-allegro" "swank-gray")
           #+clisp '("xref" "metering" "swank-clisp" "swank-gray")
           #+armedbear '("swank-abcl" "swank-gray")
           )))

(defparameter *lisp-name*
  #+cmu       (format nil "cmu-~A" (lisp-implementation-version))
  #+sbcl      (format nil "sbcl-~A" (lisp-implementation-version))
  #+openmcl   "openmcl"
  #+lispworks (format nil "lispworks-~A" (lisp-implementation-version))
  #+allegro   "allegro"
  #+clisp     (format nil "clisp-~A" (let ((s (lisp-implementation-version)))
                                       (subseq s 0 (position #\space s))))
  #+armedbear "abcl"
  )

(defparameter *swank-pathname* (make-swank-pathname "swank"))

(defun file-newer-p (new-file old-file)
  "Returns true if NEW-FILE is newer than OLD-FILE."
  (> (file-write-date new-file) (file-write-date old-file)))

(defun binary-pathname (source-pathname)
  "Return the pathname where SOURCE-PATHNAME's binary should be compiled."
  (let ((cfp (compile-file-pathname source-pathname)))
    (merge-pathnames (make-pathname
                      :directory `(:relative
                                   ".slime" "fasl" ,*lisp-name*)
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
                  (format t "~&;; Compiling ~A...~%" source-pathname)
                  (ensure-directories-exist binary-pathname)
                  (compile-file source-pathname :output-file binary-pathname)
                  (setq needs-recompile t))
                (load binary-pathname))
            #+(or)
            (error ()
              ;; If an error occurs compiling, load the source instead
              ;; so we can try to debug it.
              (load source-pathname))
            ))))))

(defun user-init-file ()
  "Return the name of the user init file or nil."
  (probe-file
   (merge-pathnames (user-homedir-pathname)
                    (make-pathname :name ".swank" :type "lisp"))))

       
(compile-files-if-needed-serially
  (append (list (make-swank-pathname "swank-backend"))
          *sysdep-pathnames* 
          (list *swank-pathname*)))

(funcall (intern (string :warn-unimplemented-interfaces) :swank-backend))

(when (user-init-file)
  (load (user-init-file)))

