;;; cmucl-wire.el ---  trivial implementation of the CMUCL wire protocol

;; Author: Eric Marsden <emarsden@laas.fr>
;; Time-stamp: <2003-08-07 emarsden>
;; Version: 0.1
;; Keywords: comm
;;
;;     Copyright (C) 2003  Eric Marsden
;;   
;;     This program is free software; you can redistribute it and/or
;;     modify it under the terms of the GNU General Public License as
;;     published by the Free Software Foundation; either version 2 of
;;     the License, or (at your option) any later version.
;;     
;;     This program is distributed in the hope that it will be useful,
;;     but WITHOUT ANY WARRANTY; without even the implied warranty of
;;     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;     GNU General Public License for more details.
;;     
;;     You should have received a copy of the GNU General Public
;;     License along with this program; if not, write to the Free
;;     Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;;     MA 02111-1307, USA.
;;
;; Please send suggestions and bug reports to <emarsden@laas.fr>.
;; The latest version of this package should be available from
;;
;;     <URL:http://purl.org/net/emarsden/home/downloads/>
;;
;;
;;; Commentary:
;;
;; Communication with a slave CMUCL using the WIRE protocol. We don't
;; implement the remote-object aspects of the protocol, so the
;; marshaling business is pretty simple.
;;
;; A wire is represented by a buffer, which has an associated network
;; stream with the slave CMUCL. The slave CMUCL has to say
;;
;;  (wire:create-request-server port)



;;; Code:

(require 'cl)

(defconst +wire-op/funcall+  6)
(defconst +wire-op/number+   7)
(defconst +wire-op/string+   8)
(defconst +wire-op/symbol+   9)
(defconst +wire-op/save+     10)
(defconst +wire-op/lookup+   11)
(defconst +wire-op/cons+     13)

(defvar wire-default-package "CL-USER")

(make-variable-buffer-local
 (defvar wire-input-marker nil
   "Marker for the start of unread input."))

(put 'wire-error
     'error-conditions
     '(error wire-error))

(put 'wire-error 'error-message "Wire error")

;; the buffer will contain all wired output from the slave CMUCL
(defun wire-connect-to-remote-server (host port success fail)
  (condition-case nil
      (let ((process (open-network-stream "CMUCL" nil host port)))
	(wire-init-process process success fail)
	process)
    (file-error
     (signal 'wire-error (list "Can't connect to wire server")))))

;;; building blocks to write "asynchronous" stuff.

;; The state of the reader is stored in buffer local variables:
;;
;;  - *rstack* ("return stack") contains the functions to be executed.
;;  - *stack*  stack of argument lists for the functions.
;;  - *cont*  the next function (to support tail calls)
;;  - *args*  arguments for *cont*
;;  - *success*  this function is called when a complete object is read
;;  - *fail*  called when an errors occurs during reading.
;;  - *end-of-file*  non-nil when eof was reached (contains condition)
;;  - *object-cache*
;;
;; The separation of rstack and stack avoids the creation of closures
;; resp. continuations.  The stacks could be implemented as vectors,
;; but are lists for simplicity.

(defvar *rstack*)
(defvar *stack*)
(defvar *cont*)
(defvar *args*)
(defvar *success*)
(defvar *fail*)
(defvar *end-of-file*)
(defvar *object-cache*)

(defun wire-make-buffer (name)
  (let ((buffer (generate-new-buffer name)))
    (with-current-buffer buffer
      (when (fboundp 'set-buffer-multibyte)
	(set-buffer-multibyte nil))
      (buffer-disable-undo))
    buffer))

(defun wire-init-process (process success fail)
  (let ((buffer (wire-make-buffer "*cmucl-wire*")))
    (set-process-coding-system process 'no-conversion 'no-conversion)
    (set-process-buffer process buffer)
    (set-process-filter process 'wire-filter)
    (set-process-sentinel process 'wire-sentinel)
    (with-current-buffer buffer
      (dolist (var '(*rstack* 
		     *stack* 
		     *cont* 
		     *args* 
		     *success* 
		     *fail*
		     *end-of-file*
		     *object-cache*))
	(make-local-variable var))
      (setq *success* success)
      (setq *fail* fail)
      (setq *object-cache* (make-hash-table :size 16 :test #'eq))
      (wire-initialize-stacks))))

(defun wire-close (wire)
  (and wire
       (eq 'open (process-status wire))
       (kill-buffer (process-buffer wire))))

(defun wire-filter (wire string)
  "Insert new data into the wire's buffer *without* moving point."
  (with-current-buffer (process-buffer wire)
    (save-excursion
      (goto-char (point-max))
      (insert string))
    (wire-continue)))

(defun wire-sentinel (process message)
  (message "wire sentinel: %s" message)
  (with-current-buffer (process-buffer process)
    (setq *end-of-file* (list 'wire-error process message))
    (wire-continue)))

(defun wire-initialize-stacks ()
  (setq *rstack* (list 'wire-read-object 'wire-finish))
  (setq *stack*  (list '()               '())))

(defun wire-continue ()
  (let ((result (wire-read-loop)))
    (ecase (car result)
      ((wait quit))
      (finish
       (wire-initialize-stacks)
       (save-current-buffer (funcall *success* (second result)))
       (wire-continue)))))

(defun wire-read-loop ()
  (let ((*cont* (pop *rstack*))
	(*args* (pop *stack*))
	(buffer (current-buffer))
	(error t))
    (unwind-protect
	(prog1 (catch 'unloop
		 (while t
		   (apply *cont* *args*)))
	  (setq error nil))
      (when error
	(cond ((buffer-live-p buffer)
	       (message "buffer still alive!!"))
	      (t
	       (message "some strange bug")))
	(debug)))))

(defun wire-tailcall (fn args)
  (setq *cont* fn)
  (setq *args* args))

(defun wire-bind (producer consumer freevars)
  (push consumer *rstack*)
  (push freevars *stack*)
  (wire-tailcall producer freevars))

(defun wire-return (arg)
  (wire-tailcall (pop *rstack*) (cons arg (pop *stack*))))
      
(defun wire-finish (value)
  (assert (null *rstack*))
  (assert (null *stack*))
  (throw 'unloop `(finish ,value)))

(defun wire-cleanup ()
  (when (get-buffer-process (current-buffer))
    (delete-process (get-buffer-process (current-buffer))))
  (kill-buffer (current-buffer))
  (throw 'unloop '(quit)))

(defun wire-error (&rest args)
  (unwind-protect
      (save-current-buffer
	(funcall *fail* (etypecase (car args)
			  (string (list 'wire-error (apply #'format args)))
			  (symbol args))))
    (wire-cleanup)))

(defun wire-read-byte ()
  (cond ((eobp)
	 (wire-eat-input)
	 (cond ((and (boundp '*end-of-file*) *end-of-file*)
		(apply #'wire-error *end-of-file*)
		(throw 'unloop '(wait)))
	       (t
		(push #'wire-read-byte *rstack*)
		(push '()          *stack*)
		(throw 'unloop '(wait)))))
	(t
	 (forward-char)
	 (wire-return (char-before)))))

(defun wire-unread-byte (c)
  (backward-char))

(defun wire-eat-input ()
  (unless (bobp)
    (delete-region (point-min) (1- (point)))))

(defun wire-expand-bind (v exp body freevars)
  `(progn
     (push (lambda (,v ,@freevars)
	     (macrolet ((bind ((v exp) body)
			      (wire-expand-bind 
			       v exp body ',(cons v freevars))))
	       ,body))
	   *rstack*)
     (push (list ,@freevars) *stack*)
     ,exp))

;;; The reader must be carefully written, so that all nececessary
;;; state is saved when the reader "blocks".  The macro(let)s below
;;; are intented to simplify this task.

(defmacro deffn (name args &rest body)
  `(defun ,name ,args
     (macrolet ((bind ((v exp) body)
		      (wire-expand-bind v exp body ',args))
		(call (fn &rest args)
		      `(wire-tailcall ',fn (list ,@args)))
		(ret (arg)
		     `(wire-return ,arg)))
       (,@ body))))

(deffn wire-read-object ()
  (bind (type (wire-read-byte))
	(cond ((= type +wire-op/number+)
	       (call wire-read-number))
	      ((= type +wire-op/string+)
	       (call wire-read-string))
	      ((= type +wire-op/symbol+)
	       (call wire-read-symbol))
	      ((= type +wire-op/cons+)
	       (call wire-read-cons))
	      ((= type +wire-op/save+)
	       (call wire-read-save))
	      ((= type +wire-op/lookup+)
	       (call wire-read-lookup))
	      (t
	       (wire-error "Unsupported wire datatype: %S" type)))))

(deffn wire-read-number () 
  (bind (b1 (wire-read-byte))
	(bind (b2-b3-b4 (wire-read-number-aux 2 0))
	      (progn (wire-validate-high-byte b1)
		     (ret (logior (lsh b1 24) b2-b3-b4))))))

(defun wire-validate-high-byte (byte)
  (unless (if (zerop (logand byte 128)) ; posivite?
	      (<= byte (eval-when-compile (lsh -1 -25)))
	    (>= byte (eval-when-compile
		       (logand (ash most-negative-fixnum -24)
			       255))))
    (wire-error "fixnum overlow: %s" byte)))

(deffn wire-read-number-aux (i accum)
  (bind (byte (wire-read-byte))
	(let ((accum (+ (* 256 accum) byte)))
	  (if (zerop i)
	      (ret accum)
	    (call wire-read-number-aux (1- i) accum)))))

(deffn wire-read-string ()
  (bind (count (wire-read-number))
	(call wire-read-string-aux count (make-string count ??))))

(deffn wire-read-string-aux (remaining string)
  (if (zerop remaining)
      (ret string)
    (bind (char (wire-read-byte))
	  (progn 
	    (aset string (- (length string) remaining) char)
	    (call wire-read-string-aux (1- remaining) string)))))

(deffn wire-read-symbol ()
  (bind (name (wire-read-string))
	(bind (package (wire-read-string))
	      (cond ((and (string= name "NIL")
			  (string= package "COMMON-LISP"))
		     (ret 'nil))
		    ((string= package "KEYWORD")
		     (ret (intern (concat ":" name))))
		    (t
		     (ret (intern (format "%s::%s" package name))))))))

(deffn wire-read-cons ()
  (bind (car (wire-read-object))
	(bind (cdr (wire-read-object))
	      (ret (cons car cdr)))))

(deffn wire-read-save ()
  (bind (index (wire-read-number))
	(bind (object (wire-read-object))
	      (progn 
		(setf (gethash index *object-cache*) object)
		(ret object)))))

(deffn wire-read-lookup ()
  (bind (index (wire-read-number))
	(let ((object (gethash index *object-cache*)))
	  (ret object))))

;; === low-level encoding issues === 

(defun wire-output-byte (wire byte)
  (process-send-string wire (char-to-string byte)))

;; use a separate variable for pos, in case input arrives during
;; the execution of this function
(defsubst wire-get-byte (wire)
  (let ((inhibit-eol-conversion t))
    (with-current-buffer (process-buffer wire)
      (goto-char wire-input-marker)
      (while (eobp)
	(accept-process-output wire))
      (forward-char 1)
      (set-marker wire-input-marker (point))
      (char-before))))

(defun wire-output-number (wire number &optional length)
  (let* ((length (or length 4))
         (str (make-string length 0))
         (i (- length 1)))
    (while (>= i 0)
      (aset str i (% number 256))
      (setq number (floor number 256))
      (decf i))
    (process-send-string wire str)))

(defun wire-get-number (wire)
  (do ((i 4 (- i 1))
       (accum 0))
      ((zerop i) accum)
    (setf accum (+ (* 256 accum) (wire-get-byte wire)))))

(defun wire-get-symbol (wire)
  (let ((name (wire-get-string wire))
	(package (wire-get-string wire)))
    (intern (format "%s:%s" package name))))

(defun wire-get-object (wire)
  (let ((type (wire-get-byte wire)))
    (cond ((= type +wire-op/number+)
	   (wire-get-number wire))
	  ((= type +wire-op/string+)
	   (wire-get-string wire))
	  ((= type +wire-op/symbol+)
	   (wire-get-symbol wire))
	  (t
	   (error "Unknown wire dataype: %S" type)))))

;; Strings are represented by the length as a number, followed by the
;; bytes of the string. Assume that we're dealing with a "simple"
;; string.
(defun wire-output-string (wire string)
  (let ((length (length string)))
    (wire-output-number wire length)
    (process-send-string wire string)))

;; the first four octets read are the size of the string.
(defun wire-get-string (wire)
  (let ((count (wire-get-number wire)))
    (when (minusp count)
      (error "Number overflow in wire-get-string"))
    (do ((i 0 (+ i 1))
         (chars (make-string count ?.)))
        ((= i count) chars)
      (aset chars i (wire-get-byte wire)))))

(defun wire-output-object (wire object)
  (typecase object
    (integer
     (wire-output-byte wire +wire-op/number+)
     (wire-output-number wire object))
    (string
     (wire-output-byte wire +wire-op/string+)
     (wire-output-string wire object))
    (symbol
     (wire-output-byte wire +wire-op/symbol+)
     (wire-output-string wire (wire-symbol-name object))
     (wire-output-string wire
			 (wire-symbol-package object wire-default-package)))
    (cons
     (wire-output-byte wire +wire-op/cons+) ;
     (wire-output-object wire (car object))
     (wire-output-object wire (cdr object)))
    (t
     (error "Cannot output objects of type %s across a wire."
            (type-of object))))
  nil)

(defun wire-symbol-name (symbol)
  (let ((n (if (stringp symbol) symbol (symbol-name symbol))))
    (if (string-match ":\\([^:]*\\)$" n)
	(match-string 1 n)
	n)))

(defun wire-symbol-package (symbol &optional default)
  (let ((n (if (stringp symbol) symbol (symbol-name symbol))))
    (if (string-match "^\\([^:]*\\):" n)
	(match-string 1 n)
	default)))

(defun wire-symbol-external-ref-p (symbol)
  "Does SYMBOL refer to an external symbol?
FOO:BAR is an external reference.
FOO::BAR is not, and nor is BAR."
  (let ((name (if (stringp symbol) symbol (symbol-name symbol))))
    (and (string-match ":" name)
         (not (string-match "::" name)))))

;; send the function and its arguments down the wire as a funcall. 
(defun wire-output-funcall (wire function &rest args)
  (let ((num-args (length args)))
    (wire-output-byte wire +wire-op/funcall+)
    (wire-output-byte wire num-args)
    (wire-output-object wire function)
    (dolist (arg args)
      (wire-output-object wire arg))
    nil))

;; returns a single value. Note that we send the function name in
;; uppercase, because it doesn't go through the CL reader. 
(defun wire-remote-eval (wire string package)
  (wire-output-funcall wire 'SWANK:EVALUATE string package)
  (let ((status (wire-get-object wire))
	(condition (wire-get-object wire))
	(result (wire-get-object wire)))
    ;; for efficiency, we empty the wire buffer when it gets very large
    (with-current-buffer (process-buffer wire)
      (when (> wire-input-marker 100000)
	(delete-region (point-min) wire-input-marker)))
    (values status condition result)))

(provide 'cmucl-wire)

;;; cmucl-wire.el ends here
