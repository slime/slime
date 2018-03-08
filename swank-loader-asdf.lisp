;;;; -*- lisp -*-
;;;
;;; swank-loader.lisp --- Compat to allow the Elisp code to load SWANK
;;; via ASDF transparently.
;;;

(defpackage #:swank-loader
  (:use #:common-lisp)
  (:export #:init #:*source-directory*))

(in-package #:swank-loader)

(defun init (&key reload)
  (asdf:load-system :swank :force reload))

;; Gets initialized at the end of swank.asd
(defvar *source-directory*)
