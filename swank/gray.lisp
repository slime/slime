;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; swank-gray.lisp --- Gray stream based IO redirection.
;;;
;;; Created 2003
;;;
;;; This code has been placed in the Public Domain.  All warranties
;;; are disclaimed.
;;;

(in-package swank/backend)

#.(progn
    (defvar *gray-stream-symbols*
    '(fundamental-character-output-stream
      stream-write-char
      stream-write-string
      stream-fresh-line
      stream-force-output
      stream-finish-output

      fundamental-character-input-stream
      stream-read-char
      stream-peek-char
      stream-read-line
      stream-listen
      stream-unread-char
      stream-clear-input
      stream-line-column
      stream-read-char-no-hang

      #+sbcl stream-file-position))
    nil)

(defpackage swank/gray
  (:use cl swank/backend)
  (:import-from #.(gray-package-name) . #.*gray-stream-symbols*)
  (:export . #.*gray-stream-symbols*))

(in-package swank/gray)

;;; Avoid using CLOS in the auto-flush thread due to possible
;;; deadlocks between CLOS and streams.
(defstruct stream-data
  (output-fn)
  (buffer (make-string 64000))
  (fill-pointer 0)
  (column 0)
  (lock (make-lock :name "buffer write lock"))
  (flush-thread)
  (flush-scheduled))

(defclass slime-output-stream (fundamental-character-output-stream)
  ((data :initform (make-stream-data)
         :initarg :data
         :accessor data)))

(defmacro with-stream-data (data &body body)
  `(with-accessors ((lock stream-data-lock)
                    (output-fn stream-data-output-fn)
                    (buffer stream-data-buffer)
                    (fill-pointer stream-data-fill-pointer)
                    (column stream-data-column)
                    (flush-thread stream-data-flush-thread)
                    (flush-scheduled stream-data-flush-scheduled))
       ,data
     (call-with-lock-held lock (lambda () ,@body))))

(defmacro with-slime-output-stream (stream &body body)
  `(let ((data (data ,stream)))
     (with-stream-data data ,@body)))

(defun maybe-schedule-flush (data)
  (with-stream-data data
    (when flush-thread
      (or flush-scheduled
          (progn
            (setf flush-scheduled t)
            (send flush-thread t)
            t)))))

(defmethod stream-write-char ((stream slime-output-stream) char)
  (with-slime-output-stream stream
    (setf (schar buffer fill-pointer) char)
    (incf fill-pointer)
    (incf column)
    (when (char= #\newline char)
      (setf column 0))
    (if (= fill-pointer (length buffer))
        (%stream-finish-output data)
        (maybe-schedule-flush data)))
  char)

(defmethod stream-write-string ((stream slime-output-stream) string
                                &optional start end)
  (with-slime-output-stream stream
    (let* ((start (or start 0))
           (end (or end (length string)))
           (len (length buffer))
           (count (- end start))
           (free (- len fill-pointer)))
      (when (>= count free)
        (%stream-finish-output data))
      (cond ((< count len)
             (replace buffer string :start1 fill-pointer
                      :start2 start :end2 end)
             (incf fill-pointer count)
             (maybe-schedule-flush data))
            (t
             (funcall output-fn (subseq string start end))))
      (let ((last-newline (position #\newline string :from-end t
                                    :start start :end end)))
        (setf column (if last-newline
                         (- end last-newline 1)
                         (+ column count))))))
  string)

(defmethod stream-line-column ((stream slime-output-stream))
  (with-slime-output-stream stream column))

(defun reset-stream-line-column (stream)
  (with-slime-output-stream stream (setf column 0)))

(defun %stream-finish-output (data)
  (with-stream-data data
    (unless (zerop fill-pointer)
      (funcall output-fn (subseq buffer 0 fill-pointer))
      (setf fill-pointer 0))
    (setf flush-scheduled nil))
  nil)

(defmethod stream-force-output ((stream slime-output-stream))
  (stream-finish-output stream))

(defmethod stream-finish-output ((stream slime-output-stream))
  (with-slime-output-stream stream
    (unless (maybe-schedule-flush data)
      (%stream-finish-output data))))

(defmethod stream-fresh-line ((stream slime-output-stream))
  (with-slime-output-stream stream
    (cond ((zerop column) nil)
          (t (terpri stream) t))))

#+sbcl
(defmethod stream-file-position ((stream slime-output-stream) &optional position)
  (declare (ignore position))
  nil)

(defclass slime-input-stream (fundamental-character-input-stream)
  ((input-fn :initarg :input-fn)
   (buffer :initform "") (index :initform 0)
   (lock :initform (make-lock :name "buffer read lock"))))

(defmethod stream-read-char ((s slime-input-stream))
  (call-with-lock-held
   (slot-value s 'lock)
   (lambda ()
     (with-slots (buffer index input-fn) s
       (when (= index (length buffer))
         (let ((string (funcall input-fn)))
           (cond ((zerop (length string))
                  (return-from stream-read-char :eof))
                 (t
                  (setf buffer string)
                  (setf index 0)))))
       (assert (plusp (length buffer)))
       (prog1 (aref buffer index) (incf index))))))

(defmethod stream-listen ((s slime-input-stream))
  (call-with-lock-held
   (slot-value s 'lock)
   (lambda ()
     (with-slots (buffer index) s
       (< index (length buffer))))))

(defmethod stream-unread-char ((s slime-input-stream) char)
  (call-with-lock-held
   (slot-value s 'lock)
   (lambda ()
     (with-slots (buffer index) s
       (decf index)
       (cond ((eql (aref buffer index) char)
              (setf (aref buffer index) char))
             (t
              (warn "stream-unread-char: ignoring ~S (expected ~S)"
                    char (aref buffer index)))))))
  nil)

(defmethod stream-clear-input ((s slime-input-stream))
  (call-with-lock-held
   (slot-value s 'lock)
   (lambda ()
     (with-slots (buffer index) s
       (setf buffer ""
             index 0))))
  nil)

(defmethod stream-line-column ((s slime-input-stream))
  nil)

(defmethod stream-read-char-no-hang ((s slime-input-stream))
  (call-with-lock-held
   (slot-value s 'lock)
   (lambda ()
     (with-slots (buffer index) s
       (when (< index (length buffer))
         (prog1 (aref buffer index) (incf index)))))))

#+sbcl
(defmethod stream-file-position ((stream slime-input-stream) &optional position)
  (declare (ignore position))
  nil)


;;;

(defimplementation make-auto-flush-thread (stream)
  (if (typep stream 'slime-output-stream)
      (setf (stream-data-flush-thread (data stream))
            (spawn (lambda () (auto-flush-loop stream #-allegro 0.005
                                                      #+allegro 0.08
                                                      t (lambda (stream)
                                                          (%stream-finish-output (data stream)))))
                   :name "auto-flush-thread"))
      (spawn (lambda () (auto-flush-loop stream *auto-flush-interval*))
             :name "auto-flush-thread")))

(defimplementation really-finish-output (stream)
  (let ((stream (swank::real-output-stream stream)))
    (if (typep stream 'slime-output-stream)
        (%stream-finish-output (data stream))
        (finish-output stream))))

(defimplementation make-output-stream (write-string)
  (make-instance 'slime-output-stream :data (make-stream-data :output-fn write-string)))

(defimplementation make-input-stream (read-string)
  (make-instance 'slime-input-stream :input-fn read-string))
