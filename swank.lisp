;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; swank.lisp --- the portable bits
;;;
;;; Created 2003, Daniel Barlow <dan@metacircles.com>
;;;
;;; This code has been placed in the Public Domain.  All warranties are 
;;; disclaimed.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defpackage :swank
    (:use :common-lisp)
    (:export #:start-server)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar swank::*sysdep-pathname*
    (merge-pathnames (or #+cmu "swank-cmucl" 
			 #+(and sbcl sb-thread) "swank-sbcl" 
			 #+openmcl "swank-openmcl")
		     (or *compile-file-pathname* *load-pathname* 
			 *default-pathname-defaults*))))

(in-package :swank)
(defvar *swank-io-package*
  (let ((package (make-package "SWANK-IO-PACKAGE")))
    (import '(nil t quote) package)
    package))

(declaim (optimize (debug 3)))

(eval-when (:compile-toplevel) (compile-file swank::*sysdep-pathname*))
(eval-when (:load-toplevel :execute) (load swank::*sysdep-pathname*))


;;; Local Variables:
;;; eval: (font-lock-add-keywords 'lisp-mode '(("(\\(defslimefun\\)\\s +\\(\\(\\w\\|\\s_\\)+\\)"  (1 font-lock-keyword-face) (2 font-lock-function-name-face))))
;;; End:
