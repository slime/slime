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

(defpackage :swank-loader
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
           #+cmu '("swank-cmucl")
           #+sbcl '("swank-sbcl" "swank-gray")
           #+openmcl '("swank-openmcl" "swank-gray")))

(defparameter *swank-pathname* (make-swank-pathname "swank"))

(defun file-newer-p (new-file old-file)
  "Returns true if NEW-FILE is newer than OLD-FILE."
  (> (file-write-date new-file) (file-write-date old-file)))

(defun compile-files-if-needed-serially (files)
  "Compile each file in FILES if the source is newer than
its corresponding binary, or the file preceding it was 
recompiled."
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
          (error ()
            ;; If an error occurs compiling, load the source instead
            ;; so we can try to debug it.
            (load source-pathname)))))))

(defun user-init-file ()
  "Return a the name of the user init file or nil."
  (let ((filename (format nil "~A/.swank.lisp"
                          (namestring (user-homedir-pathname)))))
    (cond ((probe-file filename) filename)
          (t nil))))

(compile-files-if-needed-serially
 (list* (make-swank-pathname "swank-backend") *swank-pathname*
        *sysdep-pathnames*))

(when (user-init-file)
  (load (user-init-file)))

