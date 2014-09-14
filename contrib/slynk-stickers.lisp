(defpackage :slynk-stickers
  (:use :cl)
  (:import-from :slynk-backend :slynk-compile-string)
  (:import-from :slynk :defslyfun :with-buffer-syntax)
  (:export #:record))
(in-package :slynk-stickers)

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
              do (setf (gethash id *stickers*) nil))
        new-stickers))))

(defmacro record (id &rest body)
  (let ((values-sym (gensym))
        (id-sym (gensym)))
    `(let ((,values-sym (multiple-value-list (progn ,@body)))
           (,id-sym ,id))
       (unless (eq (gethash ,id-sym *stickers* :unknown) :unknown)
         (push  ,values-sym (gethash ,id-sym *stickers*)))
       (values-list ,values-sym))))

(defslyfun check-stickers ()
  (loop for k being the hash-keys of *stickers*
        for values-lists being the hash-values of *stickers*
        collect (cons k
                      (loop for values-list in values-lists
                            collect
                            (mapcar #'slynk::to-line values-list)))))

(defun find-sticker-or-lose (id)
  (let ((probe (gethash id *stickers* :unknown)))
    (if (eq probe :unknown)
        (error "Cannot find sticker ~a" id)
        probe)))

(defslyfun inspect-sticker-values (id)
  (let ((values-lists (find-sticker-or-lose id)))
    (slynk::inspect-object values-lists)))

(provide 'slynk-stickers)
