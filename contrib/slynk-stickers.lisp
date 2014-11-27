(defpackage :slynk-stickers
  (:use :cl)
  (:import-from :slynk-backend :slynk-compile-string)
  (:import-from :slynk :defslyfun :with-buffer-syntax :compile-string-for-emacs)
  (:export #:record
           #:compile-for-stickers
           #:kill-stickers
           #:inspect-sticker
           #:inspect-sticker-recording
           #:fetch
           #:forget
           #:find-recording-or-lose
           #:search-for-recording))
(in-package :slynk-stickers)

(defclass recording ()
  ((index :reader index-of)
   (ctime :initform (common-lisp:get-universal-time) :accessor ctime-of)
   (sticker :initform (error "required") :initarg :sticker :accessor sticker-of)
   (values :initform (error "required") :initarg :values :accessor values-of)
   (condition :initarg :condition :accessor condition-of)))

(defmethod initialize-instance :after ((x recording) &key sticker)
  (push x (recordings-of sticker))
  (setf (slot-value x 'index) (fill-pointer *recordings*))
  (vector-push-extend x *recordings*))

(defun recording-description-string (recording &optional stream print-first-value)
  (let ((values (values-of recording))
        (condition (condition-of recording)))
    (cond (condition
           (format stream "exited non-locally with: ~a" (slynk::to-line condition)))
          ((eq values 'exited-non-locally)
           (format stream "exited non-locally"))
          ((listp values)
           (if (and print-first-value
                    values)
               (format stream "~a" (slynk::to-line (car values)))
               (format stream "~a values" (length values))))
          (t
           (format stream "corrupt recording")))))

(defmethod print-object ((r recording) s)
  (print-unreadable-object (r s :type t)
    (recording-description-string r s)))

(defclass sticker ()
  ((id :initform (error "required")  :initarg :id :accessor id-of)
   (recordings :initform nil :accessor recordings-of)))

(defmethod print-object ((sticker sticker) s)
  (print-unreadable-object (sticker s :type t)
    (format s "~a new recordings" (length (recordings-of sticker)))))

(defun exited-non-locally-p (recording)
  (when (or (condition-of recording)
            (eq (values-of recording) 'exited-non-locally))
    t))


;; FIXME: This won't work for multiple-connections. A channel, or some
;; connection specific structure, is needed for that.
;;
(defvar *stickers* (make-hash-table))
(defvar *recordings* (make-array 40 :fill-pointer 0 :adjustable t))
(defvar *visitor* nil)

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
                 do (setf (gethash id *stickers*)
                          (make-instance 'sticker :id id)))
           (list probe t))
          (original-string
           (list (compile-string-for-emacs original-string buffer position filename policy)
                 nil)))))

(defslyfun kill-stickers (ids)
  (loop for id in ids
        do (remhash id *stickers*)))

(defun call-with-sticker-recording (id fn)
  (let* ((sticker (gethash id *stickers*))
         (values 'exited-non-locally)
         (last-condition))
    (unwind-protect
         (handler-bind ((condition (lambda (condition)
                                     (setq last-condition condition))))
           (setq values (multiple-value-list (funcall fn)))
           (values-list values))
      (when sticker
        (make-instance 'recording
          :sticker sticker
          :values values
          :condition (and (eq values 'exited-non-locally) last-condition))))))

(defmacro record (id &rest body)
  `(call-with-sticker-recording ,id (lambda () ,@body)))

(define-setf-expander record (x &environment env)
  (declare (ignore x env))
  (error "Sorry, not allowing ~a for ~a" 'setf 'record))

(define-condition abort-search (simple-error) ())

(defun abort-search (format-control &rest format-args)
  (error 'abort-search :format-control format-control :format-arguments format-args))

(defun search-for-recording-1 (from ignore-list &key direction)
  ;; if directly requesting a recording, ignore IGNORE-LIST for this
  ;; invocation.
  ;; 
  (when (numberp direction)
    (setq ignore-list nil))
  (loop for candidate-id = (cond ((eq direction :next)
                                  (incf from))
                                 ((eq direction :prev)
                                  (decf from))
                                 ((numberp direction)
                                  direction)
                                 (t
                                  (error "unknown direction spec ~a" direction)))
        while (< -1 candidate-id (length *recordings*))
        for recording = (aref *recordings* candidate-id)
        for sticker-id = (id-of (sticker-of recording))
        unless (member sticker-id ignore-list)
          ;; JT@14/11/17: Could also check if STICKER-ID is in
          ;; *STICKERS* (meaning it has been killed by KILL-STICKERS),
          ;; but for now it's better to warn the user on the Emacs
          ;; side.
          return recording
        finally (abort-search "No such recording in direction: ~a" direction)))

(defun describe-recording-for-emacs (recording)
  "Describe RECORDING as (ID VALUE-DESCRIPTIONS EXITED-NON-LOCALLY-P)
ID is a number. VALUE-DESCRIPTIONS is a list of
strings. EXITED-NON-LOCALLY-P is an integer."
  (list (index-of recording)
        (and (listp (values-of recording))
             (mapcar #'slynk::to-line (values-of recording)))
        (exited-non-locally-p recording)))

(defun describe-sticker-for-emacs (sticker &optional recording)
  "Describe STICKER and either its latest recording or RECORDING.
Returns a list (ID NRECORDINGS . RECORDING-DESCRIPTION).
RECORDING-DESCRIPTION is as given by DESCRIBE-RECORDING-FOR-EMACS."
  (let* ((recordings (recordings-of sticker))
         (recording (or recording
                        (car (last recordings)))))
    (list* (id-of sticker)
           (length recordings)
           (and recording
                (describe-recording-for-emacs recording)))))

(defslyfun search-for-recording (key ignore-list direction)
  "Visit the next recording for the visitor KEY.
Ignore stickers whose ID is in IGNORE-LIST. DIRECTION can be the
keyword :UP, :DOWN or a recording index.

If a recording can be found return a list (TOTAL-RECORDINGS
. STICKER-DESCRIPTION).  STICKER-DESCRIPTION is as given by
DESCRIBE-STICKER-FOR-EMACS.

Otherwise returns a list (NIL ERROR-DESCRIPTION)"
  (unless (and *visitor*
               (eq key (car *visitor*)))
    (setf *visitor* (cons key -1)))
  (handler-case 
      (let ((recording (search-for-recording-1 (cdr *visitor*) ignore-list
                                               :direction direction)))
        (setf (cdr *visitor*)  (index-of recording))
        (list* (length *recordings*)
               (describe-sticker-for-emacs (sticker-of recording) recording)))
    (abort-search (error)
      (list nil (format nil "~a" error)))))

(defslyfun fetch ()
  "Describe each known sticker to Emacs."
  (loop for sticker being the hash-values of *stickers*
        collect (describe-sticker-for-emacs sticker)))

(defslyfun forget ()
  "Forget all sticker recordings."
  (maphash (lambda (id sticker)
             (declare (ignore id))
             (setf (recordings-of sticker) nil))
           *stickers*)
  (setf (fill-pointer *recordings*) 0))

(defslyfun find-recording-or-lose (recording-id)
  (let ((recording (aref *recordings* recording-id)))
    (values-list (values-of recording))))

(defun find-sticker-or-lose (id)
  (let ((probe (gethash id *stickers* :unknown)))
    (if (eq probe :unknown)
        (error "Cannot find sticker ~a" id)
        probe)))

(defslyfun inspect-sticker (sticker-id)
  (let ((sticker (find-sticker-or-lose sticker-id)))
    (slynk::inspect-object sticker)))

(defslyfun inspect-sticker-recording (recording-id)
  (let ((recording (find-recording-or-lose recording-id)))
    (slynk::inspect-object recording)))

(provide 'slynk-stickers)
