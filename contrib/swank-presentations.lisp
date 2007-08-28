;;; swank-presentation-streams.lisp --- imitate LispM's presentations
;;;
;;; Authors: FIXME -- find all guilty parties
;;;
;;; License: This code has been placed in the Public Domain.  All warranties
;;;          are disclaimed.

(in-package :swank)

;;; More presentation-related code from swank.lisp can go here. --mkoeppe


(defun send-repl-results-to-emacs (values)
  ;; Override a function in swank.lisp, so that 
  ;; presentations are associated with every REPL result.
  (flet ((send (value)
           (let ((id (and *record-repl-results*
                          (save-presented-object value))))
	     (send-to-emacs `(:presentation-start ,id :repl-result))
	     (send-to-emacs `(:write-string ,(prin1-to-string value)
					    :repl-result))
	     (send-to-emacs `(:presentation-end ,id :repl-result))
	     (send-to-emacs `(:write-string ,(string #\Newline) 
					    :repl-result)))))
    (if (null values)
        (send-to-emacs `(:write-string "; No value" nil :repl-result))
        (mapc #'send values))))

(provide :swank-presentations)
