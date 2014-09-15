(defpackage :slynk-stickers
  (:use :cl)
  (:import-from :slynk-backend :slynk-compile-string)
  (:import-from :slynk :defslyfun :with-buffer-syntax)
  (:export #:record))
(in-package :slynk-stickers)

(defclass sticker ()
  ((reported-value-lists :initform nil :accessor reported-value-lists-of)
   (new-value-lists :initform nil :accessor new-value-lists-of)))

(defvar *stickers* (make-hash-table))

(defslyfun compile-for-stickers (new-stickers dead-stickers string buffer position filename policy)
  (with-buffer-syntax ()
    (let ((*compile-print* nil) (*compile-verbose* nil)
          (offset (cadr (assoc :position position))))
      (loop for id in dead-stickers
            do (remhash id *stickers*))
      (when (handler-case
                (slynk-compile-string string
                                      :buffer buffer
                                      :position offset
                                      :filename filename
                                      :policy policy)
              (error () nil))
        (loop for id in new-stickers
              do (setf (gethash id *stickers*) (make-instance 'sticker)))
        new-stickers))))

(defmacro record (id &rest body)
  (let ((id-sym (gensym "ID-")))
    `(let* ((values (multiple-value-list (progn ,@body)))
            (,id-sym ,id)
            (sticker (gethash ,id-sym *stickers*)))
       (when sticker
         (push values (new-value-lists-of sticker)))
       (values-list values))))

(defslyfun check-stickers ()
  (loop for k being the hash-keys of *stickers*
        for sticker being the hash-values of *stickers*
        for new-value-lists = (new-value-lists-of sticker)
        collect (list k
                      (length new-value-lists)
                      (mapcar #'slynk::to-line
                              (car (last new-value-lists))))
        do
           (setf (reported-value-lists-of sticker)
                 (append new-value-lists
                         (reported-value-lists-of sticker)))
           (setf (new-value-lists-of sticker) nil)))

(defun find-sticker-or-lose (id)
  (let ((probe (gethash id *stickers* :unknown)))
    (if (eq probe :unknown)
        (error "Cannot find sticker ~a" id)
        probe)))

(defslyfun inspect-sticker-values (id)
  (let ((sticker (find-sticker-or-lose id)))
    (slynk::inspect-object sticker)))

(provide 'slynk-stickers)
