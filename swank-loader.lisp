;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; swank-loader.lisp --- Compile and load the Slime backend.
;;;
;;; Created 2003, James Bielman <jamesjb@jamesjb.com>
;;;
;;; This code has been placed in the Public Domain.  All warranties
;;; are disclaimed.
;;;
;;;   $Id$
;;;

(cl:defpackage :swank-loader
  (:use :common-lisp))

(in-package :swank-loader)

(defun make-swank-pathname (name &optional (type "lisp"))
  "Return a pathname with name component NAME in the Slime directory."
  (merge-pathnames name 
                   (make-pathname 
                    :type type
                    :directory 
                    (pathname-directory
                     (or *compile-file-pathname* *load-pathname*
                         *default-pathname-defaults*)))))

(defparameter *sysdep-pathnames*
  (mapcar #'make-swank-pathname 
          #+cmu '("swank-source-path-parser" "swank-cmucl")
          #+sbcl '("swank-sbcl" "swank-source-path-parser" "swank-gray")
          #+openmcl '("swank-openmcl" "swank-gray")
          #+lispworks '("swank-lispworks" "swank-gray")
          #+allegro '("swank-allegro" "swank-gray")
          #+clisp '("xref" "swank-clisp" "swank-gray")
          ))

(defparameter *swank-pathname* (make-swank-pathname "swank"))

(defun file-newer-p (new-file old-file)
  "Returns true if NEW-FILE is newer than OLD-FILE."
  (> (file-write-date new-file) (file-write-date old-file)))

(defun compile-files-if-needed-serially (files)
  "Compile each file in FILES if the source is newer than
its corresponding binary, or the file preceding it was 
recompiled."
  (with-compilation-unit ()
    (let ((needs-recompile nil))
      (dolist (source-pathname files)
        (let ((binary-pathname (compile-file-pathname source-pathname)))
          (handler-case
              (progn
                (when (or needs-recompile
                          (not (probe-file binary-pathname))
                          (file-newer-p source-pathname binary-pathname))
                  (format t "~&;; Compiling ~A...~%" source-pathname)
                  (compile-file source-pathname)
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
                    #-mswindows (make-pathname :name ".swank" :type "lisp")
                    #+mswindows (make-pathname :name "_swank" :type "lsp"))))

(compile-files-if-needed-serially
  (list* (make-swank-pathname "swank-backend") *swank-pathname*
         *sysdep-pathnames*))

(swank:warn-unimplemented-interfaces)

(when (user-init-file)
  (load (user-init-file)))

