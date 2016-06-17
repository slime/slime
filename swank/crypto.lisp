;;; -*- indent-tabs-mode: nil; outline-regexp: ";;;;;*" -*-
;;;
;;; swank/crypto.lisp --- SLIME cryptography interface
;;;
;;; Created by Demetri Obenour in 2015.  Released under CC0.

(defpackage swank/crypto
  (:use :cl)
  (:export :make-secret
           :check-secret))

(in-package :swank/crypto)

(locally
    ;; In this crypto code, we do not want any useful debugging information.
    ;; Therefore, compile this code with (speed 3) and (debug 0)
    (declare (optimize (speed 3) (safety 3) (debug 0)))
  (defun make-secret-unix (secret-pathname)
    "Creates a secret"
    (let ((secret (make-array 16
                              :element-type '(unsigned-byte 8)
                              :initial-element 255)))
      (with-open-file (s "/dev/urandom"
                         :if-does-not-exist :error
                         :external-format nil)
        (unless (= (read-sequence secret s) (len secret))
          (error "Failed to read bytes from /dev/urandom"))
        s)))
  (defun write-secret (secret pathname)
    "Write `SECRET' to `PATHNAME'"
    (with-open-file (s pathname
                       :if-exists :error
                       :external-format nil)
      (write-sequence secret s)))
  (
        (lambda (password)
          "Check a password"
