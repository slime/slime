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
(defconst +wire-op/cons+     13)

(defvar wire-default-package "CL-USER")

(make-variable-buffer-local
 (defvar wire-input-marker nil
   "Marker for the start of unread input."))

(put 'wire-error
     'error-conditions
     '(error wire-error))


;; the buffer will contain all wired output from the slave CMUCL
(defun wire-connect-to-remote-server (host port)
  (let ((buf (get-buffer-create " *cmucl-wire*"))
        (process nil))
    (condition-case nil
        (progn
          (setq process (open-network-stream "CMUCL" buf host port))
	  (set-process-coding-system process 'no-conversion 'no-conversion)
	  (set-process-filter process 'wire-filter)
	  (wire-init-process-buffer process)
	  process)
      (file-error
       (kill-buffer buf)
       (signal 'wire-error (list "Can't connect to wire server"))))))

(defun wire-init-process-buffer (process)
  (with-current-buffer (process-buffer process)
    (unless (featurep 'xemacs)
      (set-buffer-multibyte nil))
    (setq wire-input-marker (make-marker))
    (set-marker-insertion-type wire-input-marker nil)
    (move-marker wire-input-marker (point-min))))  

(defun wire-filter (wire string)
  "Insert new data into the wire's buffer *without* moving the point."
  (with-current-buffer (process-buffer wire)
  (save-excursion
    (goto-char (point-max))
    (insert string))))

(defun wire-close (wire)
  (and wire
       (eq 'open (process-status wire))
       (kill-buffer (process-buffer wire))))


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
  (let ((n (symbol-name symbol)))
    (if (string-match ":\\(.*\\)$" n)
	(match-string 1 n)
	n)))

(defun wire-symbol-package (symbol &optional default)
  (let ((n (symbol-name symbol)))
    (if (string-match "^\\(.*\\):" n)
	(match-string 1 n)
	default)))

;; send the function and its arguments down the wire as a funcall. 
(defun wire-output-funcall (wire function &rest args)
  (let ((num-args (length args)))
    (wire-output-byte wire +wire-op/funcall+)
    (wire-output-byte wire num-args)
    (wire-output-object wire function)
    (dolist (arg args)
      (wire-output-object wire arg))
    nil))



(provide 'cmucl-wire)

;;; cmucl-wire.el ends here
