(defpackage :slynk-stickers
  (:use :cl)
  (:import-from :slynk-backend :slynk-compile-string)
  (:import-from :slynk :defslyfun :with-buffer-syntax :compile-string-for-emacs)
  (:export #:record
           #:compile-for-stickers
           #:kill-stickers
           #:inspect-sticker-values
           #:fetch-and-forget))
(in-package :slynk-stickers)

(defclass sticker ()
  ((reported-value-lists :initform nil :accessor reported-value-lists-of)
   (new-value-lists :initform nil :accessor new-value-lists-of)))

(defvar *stickers* (make-hash-table))

(defslyfun compile-for-stickers (new-stickers
                                 dead-stickers
                                 instrumented-string
                                 original-string
                                 buffer
                                 position
                                 filename
                                 policy)
  "Considering NEW-STICKERS, compile INSTRUMENTED-STRING.
INSTRUMENTED-STRING is exerpted from BUFFER at POSITION. BUFFER may be
associated with FILENAME. DEAD-STICKERS if any, are killed. If
compilation succeeds, return a list (NOTES T).

If ORIGINAL-STRING, if non-nil, is compiled as a fallback if the
previous compilation. In this case a list (NOTES NIL) is returned or
an error is signalled.

If ORIGINAL-STRING is not supplied and compilation of
INSTRUMENTED-STRING fails, return NIL."
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
    (cond (;; a non-nil and successful compilation result
           (and probe
                (third probe))
           (loop for id in new-stickers
                 do (setf (gethash id *stickers*) (make-instance 'sticker)))
           (list probe t))
          (original-string
           (list (compile-string-for-emacs original-string buffer position filename policy)
                 nil)))))

(defslyfun kill-stickers (ids)
  (loop for id in ids
        do (remhash id *stickers*)))

(defun call-with-sticker-recording (id fn)
  (let* ((sticker (gethash id *stickers*))
         (values :exited-non-locally))
    (unwind-protect
         (progn
           (setq values (multiple-value-list (funcall fn)))
           (values-list values))
      (if sticker
          (push values (new-value-lists-of sticker))))))

(defmacro record (id &rest body)
  `(call-with-sticker-recording ,id (lambda () ,@body)))

(defslyfun fetch-and-forget ()
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
