;;; -*- indent-tabs-mode:nil coding:latin-1-unix -*-
;;;
;;; swank-rpc.lisp  -- Pass remote calls and responses between lisp systems.
;;;
;;; Created 2010, Terje Norderhaug <terje@in-progress.com>
;;;
;;; This code has been placed in the Public Domain.  All warranties
;;; are disclaimed.
;;;

(defpackage :swank-rpc
  (:use :cl)
  (:export 
    ; export everything for compatibility, need to be trimmed down!
    #:decode-message
    #:read-packet
    #:read-chunk
    #:*swank-io-package*
    #:read-form
    #:encode-message
    #:prin1-to-string-for-emacs
    #:destructure-case
    #:swank-protocol-error
    #:swank-protocol-error.condition
    #:make-swank-protocol-error
    #:*log-events*
    #:*log-output*
    #:init-log-output
    #:real-input-stream
    #:real-output-stream
    #:*event-history*
    #:*event-history-index*
    #:*enable-event-history*
    #:log-event
    #:event-history-to-list
    #:clear-event-history
    #:dump-event-history
    #:dump-event
    #:escape-non-ascii
    #:ascii-string-p
    #:ascii-char-p))

(in-package :swank-rpc)

;;;;; Input

(defun simple-read ()
   "Reads a form that conforms to the protocol, otherwise signalling an error."
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

(defun decode-message (stream)
  "Read an S-expression from STREAM using the SLIME protocol."
  ;;(log-event "decode-message~%")
  (handler-bind ((error (lambda (c) (error (make-swank-protocol-error c)))))
    (let ((packet (read-packet stream)))
      (handler-case (values (read-form packet) nil)
        (reader-error (c) 
          `(:reader-error ,packet ,c))))))

;; use peek-char to detect EOF, read-sequence may return 0 instead of
;; signaling a condition.
(defun read-packet (stream)
  (peek-char nil stream) 
  (let* ((header (read-chunk stream 6))
         (length (parse-integer header :radix #x10))
         (payload (read-chunk stream length)))
    (log-event "READ: ~S~%" payload)
    payload))

(defun read-chunk (stream length)
  (let* ((buffer (make-string length))
         (count (read-sequence buffer stream)))
    (assert (= count length) () "Short read: length=~D  count=~D" length count)
    buffer))

(defvar *swank-io-package*
  (let ((package (make-package :swank-io-package :use '())))
    (import '(nil t quote) package)
    package))

(defparameter *validate-input* nil
  "Set to true to require input that strictly conforms to the protocol")

(defun read-form (string)
  (with-standard-io-syntax
    (let ((*package* *swank-io-package*))
      (if *validate-input*
        (with-input-from-string (*standard-input* string)
          (simple-read))
        (read-from-string string)))))

;;;;; Output

(defun encode-message (message stream)
  (handler-bind ((error (lambda (c) (error (make-swank-protocol-error c)))))
    (let* ((string (prin1-to-string-for-emacs message))
           (length (length string))) 
      (log-event "WRITE: ~A~%" string)
      (let ((*print-pretty* nil))
        (format stream "~6,'0x" length))
      (write-string string stream)
      (finish-output stream))))

(defun prin1-to-string-for-emacs (object)
  (with-standard-io-syntax
    (let ((*print-case* :downcase)
          (*print-readably* nil)
          (*print-pretty* nil)
          (*package* *swank-io-package*))
      (prin1-to-string object))))

;;;;; message decomposition

(defmacro destructure-case (value &rest patterns)
  "Dispatch VALUE to one of PATTERNS.
A cross between `case' and `destructuring-bind'.
The pattern syntax is:
  ((HEAD . ARGS) . BODY)
The list of patterns is searched for a HEAD `eq' to the car of
VALUE. If one is found, the BODY is executed with ARGS bound to the
corresponding values in the CDR of VALUE."
  (let ((operator (gensym "op-"))
	(operands (gensym "rand-"))
	(tmp (gensym "tmp-")))
    `(let* ((,tmp ,value)
	    (,operator (car ,tmp))
	    (,operands (cdr ,tmp)))
       (case ,operator
         ,@(loop for (pattern . body) in patterns collect 
                 (if (eq pattern t)
                     `(t ,@body)
                     (destructuring-bind (op &rest rands) pattern
                       `(,op (destructuring-bind ,rands ,operands 
                               ,@body)))))
         ,@(if (eq (caar (last patterns)) t)
               '()
               `((t (error "destructure-case failed: ~S" ,tmp))))))))

;;;;; Error handling

(define-condition swank-protocol-error (error) 
  ((condition :initarg :condition :reader swank-protocol-error.condition))
  (:report (lambda (condition stream)
             (princ (swank-protocol-error.condition condition) stream))))

(defun make-swank-protocol-error (condition)
  (make-condition 'swank-protocol-error :condition condition))

;;;;; Logging

(defvar *log-events* nil)
(defvar *log-output* nil) ; should be nil for image dumpers

(defun init-log-output ()
  (unless *log-output*
    (setq *log-output* (real-output-stream *error-output*))))

(defun real-input-stream (stream)
  (typecase stream
    (synonym-stream 
     (real-input-stream (symbol-value (synonym-stream-symbol stream))))
    (two-way-stream
     (real-input-stream (two-way-stream-input-stream stream)))
    (t stream)))

(defun real-output-stream (stream)
  (typecase stream
    (synonym-stream 
     (real-output-stream (symbol-value (synonym-stream-symbol stream))))
    (two-way-stream
     (real-output-stream (two-way-stream-output-stream stream)))
    (t stream)))

(defvar *event-history* (make-array 40 :initial-element nil)
  "A ring buffer to record events for better error messages.")
(defvar *event-history-index* 0)
(defvar *enable-event-history* t)

(defun log-event (format-string &rest args)
  "Write a message to *terminal-io* when *log-events* is non-nil.
Useful for low level debugging."
  (with-standard-io-syntax
    (let ((*print-readably* nil)
          (*print-pretty* nil)
          (*package* *swank-io-package*))
      (when *enable-event-history*
        (setf (aref *event-history* *event-history-index*) 
              (format nil "~?" format-string args))
        (setf *event-history-index* 
              (mod (1+ *event-history-index*) (length *event-history*))))
      (when *log-events*
        (write-string (escape-non-ascii (format nil "~?" format-string args))
                      *log-output*)
        (force-output *log-output*)))))

(defun event-history-to-list ()
  "Return the list of events (older events first)."
  (let ((arr *event-history*)
        (idx *event-history-index*))
    (concatenate 'list (subseq arr idx) (subseq arr 0 idx))))

(defun clear-event-history ()
  (fill *event-history* nil)
  (setq *event-history-index* 0))

(defun dump-event-history (stream)
  (dolist (e (event-history-to-list))
    (dump-event e stream)))

(defun dump-event (event stream)
  (cond ((stringp event)
         (write-string (escape-non-ascii event) stream))
        ((null event))
        (t 
         (write-string
          (escape-non-ascii (format nil "Unexpected event: ~A~%" event))
          stream))))

(defun escape-non-ascii (string)
  "Return a string like STRING but with non-ascii chars escaped."
  (cond ((ascii-string-p string) string)
        (t (with-output-to-string (out)
             (loop for c across string do
               (cond ((ascii-char-p c) (write-char c out))
                     (t (format out "\\x~4,'0X" (char-code c)))))))))

(defun ascii-string-p (o)
  (and (stringp o)
       (every #'ascii-char-p o)))

(defun ascii-char-p (c) 
  (<= (char-code c) 127))


#| TEST/DEMO:

(setf *log-events* T)

(defparameter *transport*
  (with-output-to-string (out)
    (encode-message '(:message (hello "world")) out)
    (encode-message '(:return 5) out)
    (encode-message '(:emacs-rex NIL) out)))

*transport*
                 
(with-input-from-string (in *transport*)
  (loop while (peek-char T in NIL)
        collect (decode-message in)))

|#
