;;; -*- indent-tabs-mode:nil coding:latin-1-unix -*-
;;;
;;; swank-rpc.lisp  -- Pass remote calls and responses between lisp systems.
;;;
;;; Created 2010, Terje Norderhaug <terje@in-progress.com>
;;;
;;; This code has been placed in the Public Domain.  All warranties
;;; are disclaimed.
;;;

(defpackage #:swank-rpc
  (:use :cl)
  (:export 
   #:read-message
   #:swank-reader-error
   #:swank-reader-error.packet
   #:swank-reader-error.cause
   #:write-message))

(in-package :swank-rpc)


;;;;; Input

(define-condition swank-reader-error (reader-error)
  ((packet :type string :initarg :packet :reader swank-reader-error.packet)
   (cause :type reader-error :initarg :cause :reader swank-reader-error.cause)))

(defun read-message (stream package)
  (let ((packet (read-packet stream)))
    (handler-case (values (read-form packet package))
      (reader-error (c)
        (error (make-condition 'swank-reader-error :packet packet :cause c))))))

;; use peek-char to detect EOF, read-sequence may return 0 instead of
;; signaling a condition.
(defun read-packet (stream)
  (peek-char nil stream) 
  (let* ((header (read-chunk stream 6))
         (length (parse-integer header :radix #x10))
         (payload (read-chunk stream length)))
    payload))

(defun read-chunk (stream length)
  (let* ((buffer (make-string length))
         (count (read-sequence buffer stream)))
    (assert (= count length) () "Short read: length=~D  count=~D" length count)
    buffer))

;; FIXME: no one ever tested this and will probably not work.
(defparameter *validate-input* nil
  "Set to true to require input that strictly conforms to the protocol")

(defun read-form (string package)
  (with-standard-io-syntax
    (let ((*package* package))
      (if *validate-input*
          (validating-read string)
          (read-from-string string)))))

(defun validating-read (string)
  (with-input-from-string (*standard-input* string)
    (simple-read)))

(defun simple-read ()
   "Read a form that conforms to the protocol, otherwise signal an error."
   (let ((c (read-char)))
     (case c
       (#\" (with-output-to-string (*standard-output*)
              (loop for c = (read-char) do
                    (case c
                      (#\" (return))
                      (#\\ (write-char (read-char)))
                      (t (write-char c))))))
       (#\( (loop collect (simple-read)
                  while (ecase (read-char)
                          (#\) nil)
                          (#\space t))))
       (#\' `(quote ,(simple-read)))
       (t (let ((string (with-output-to-string (*standard-output*)
                          (loop for ch = c then (read-char nil nil) do
                                (case ch
                                  ((nil) (return))
                                  (#\\ (write-char (read-char)))
                                  ((#\space #\)) (unread-char ch)(return))
                                  (t (write-char ch)))))))
            (cond ((digit-char-p c) (parse-integer string))
                  ((intern string))))))))


;;;;; Output

(defun write-message (message package stream)
  (let* ((string (prin1-to-string-for-emacs message package))
         (length (length string)))
    (let ((*print-pretty* nil))
      (format stream "~6,'0x" length))
    (write-string string stream)
    (finish-output stream)))

(defun prin1-to-string-for-emacs (object package)
  (with-standard-io-syntax
    (let ((*print-case* :downcase)
          (*print-readably* nil)
          (*print-pretty* nil)
          (*package* package))
      (prin1-to-string object))))


#| TEST/DEMO:

(defparameter *transport*
  (with-output-to-string (out)
    (write-message '(:message (hello "world")) *package* out)
    (write-message '(:return 5) *package* out)
    (write-message '(:emacs-rex NIL) *package* out)))

*transport*
                 
(with-input-from-string (in *transport*)
  (loop while (peek-char T in NIL)
        collect (read-message in *package*)))

|#
