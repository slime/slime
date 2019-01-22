;;; swank-buffer-streams.lisp --- Streams that output to a buffer
;;;
;;; Authors: Ed Langley  <el-github@elangley.org>
;;;
;;; License: This code has been placed in the Public Domain.  All warranties
;;;          are disclaimed.

(in-package :swank)

(defpackage :swank-buffer-streams
  (:use :cl)
  (:import-from :swank
                defslimefun
                add-hook
                encode-message
                send-event
                find-thread
                dcase
                current-socket-io
                send-to-emacs
                current-thread-id
                wait-for-event

                *emacs-connection*
                *event-hook*)
  (:export initialize-output-stream-hooks
           make-buffer-output-stream))

(in-package :swank-buffer-streams)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (swank:swank-require :swank-repl))

(defslimefun initialize-output-stream-hooks ()
  (add-hook *event-hook* 'handle-events)
  nil)

(defun handle-events (_ event)
  (declare (ignore _))
  (dcase event
    ((:make-target &rest _) (declare (ignore _))
     (encode-message event (current-socket-io))
     t)
    (((:target-created) thread-id &rest _)
     (declare (ignore _))
     (send-event (find-thread thread-id) event)
     t)
    (t nil)))

(defun get-temporary-identifier ()
  (intern (symbol-name (gensym "BUFFER"))
          :keyword))

(defun make-buffer-output-stream (&optional (target-identifier (get-temporary-identifier)))
  (send-to-emacs `(:make-target ,(current-thread-id)
                                ,target-identifier))
  (wait-for-event `(:target-created ,(current-thread-id) ,target-identifier))
  (values (swank-repl::make-output-stream-for-target *emacs-connection* target-identifier)
          target-identifier))

(provide :swank-buffer-streams)
