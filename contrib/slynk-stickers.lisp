(defpackage :slynk-stickers
  (:use :cl)
  (:import-from :slynk-backend :slynk-compile-string)
  (:import-from :slynk :defslyfun :with-buffer-syntax :compile-string-for-emacs)
  (:export #:record))
(in-package :slynk-stickers)

(defclass sticker ()
  ((reported-value-lists :initform nil :accessor reported-value-lists-of)
   (new-value-lists :initform nil :accessor new-value-lists-of)))

(defvar *stickers* (make-hash-table))

(defslyfun compile-for-stickers (new-stickers
                                 dead-stickers
                                 instrumented-string
                                 string
                                 buffer
                                 position
                                 filename
                                 policy)
  ;; Dead stickers are unconditionally removed from *stickers*
  ;; 
  (kill-stickers dead-stickers)
  (let ((probe
          (handler-case
              (compile-string-for-emacs instrumented-string
                                        buffer
                                        position
                                        filename
                                        policy)
            (error () nil))))
    (cond (probe
           (loop for id in new-stickers
                 do (setf (gethash id *stickers*) (make-instance 'sticker)))
           (list probe t))
          (t
           (list (compile-string-for-emacs string buffer position filename policy)
                 nil)))))

(defslyfun kill-stickers (ids)
  (loop for id in ids
        do (remhash id *stickers*)))

(defmacro record (id &rest body)
  (let ((id-sym (gensym "ID-"))
        (sticker-sym (gensym "STICKER-"))
        (values-sym (gensym "VALUES-")))
    `(let* ((,id-sym ,id)
            (,sticker-sym (gethash ,id-sym *stickers*))
            (,values-sym :exited-non-locally))
       (unwind-protect
            (progn
              (setq ,values-sym
                    (multiple-value-list (progn ,@body)))
              (values-list ,values-sym))
         (if ,sticker-sym
             (push ,values-sym (new-value-lists-of ,sticker-sym)))))))

(defslyfun check-stickers ()
  (loop for k being the hash-keys of *stickers*
        for sticker being the hash-values of *stickers*
        for new-value-lists = (new-value-lists-of sticker)
        for last-new-value-list = (car (last new-value-lists))
        for last-value-desc = (if (listp last-new-value-list)
                                  (mapcar #'slynk::to-line
                                          last-new-value-list)
                                  last-new-value-list)
        collect (list k
                      (length new-value-lists)
                      last-value-desc)
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
