;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; swank-gray.lisp --- Gray stream based IO redirection.
;;;
;;; Created 2003, Helmut Eller
;;;
;;; This code has been placed in the Public Domain.  All warranties
;;; are disclaimed.
;;;
;;;   $Id$
;;;

(in-package :swank)

(defclass slime-output-stream (fundamental-character-output-stream)
  ((buffer :initform (make-string 512))
   (fill-pointer :initform 0)
   (column :initform 0)))

(defmethod stream-write-char ((stream slime-output-stream) char)
  (with-slots (buffer fill-pointer column) stream
    (setf (schar buffer fill-pointer) char)
    (incf fill-pointer)
    (incf column)
    (when (char= #\newline char)
      (setf column 0))
    (when (= fill-pointer (length buffer))
      (force-output stream)))
  char)

(defmethod stream-line-column ((stream slime-output-stream))
  (slot-value stream 'column))

(defmethod stream-line-length ((stream slime-output-stream))
  75)

(defmethod stream-force-output ((stream slime-output-stream))
  (with-slots (buffer fill-pointer) stream
    (let ((end fill-pointer))
      (unless (zerop end)
        (send-to-emacs `(:read-output ,(subseq buffer 0 end)))
        (setf fill-pointer 0))))
  nil)

(defclass slime-input-stream (fundamental-character-input-stream)
  ((buffer :initform "") (index :initform 0)))

(defmethod stream-read-char ((s slime-input-stream))
  (with-slots (buffer index) s
    (when (= index (length buffer))
      (setf buffer (slime-read-string))
      (setf index 0))
    (assert (plusp (length buffer)))
    (prog1 (aref buffer index) (incf index))))

(defmethod stream-listen ((s slime-input-stream))
  (with-slots (buffer index) s
    (< index (length buffer))))

(defmethod stream-unread-char ((s slime-input-stream) char)
  (with-slots (buffer index) s
    (setf (aref buffer (decf index)) char))
  nil)

(defmethod stream-clear-input ((s slime-input-stream))
  (with-slots (buffer index) s 
    (setf buffer ""  
	  index 0))
  nil)

(defmethod stream-line-column ((s slime-input-stream))
  nil)

(defmethod stream-line-length ((s slime-input-stream))
  75)

