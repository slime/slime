;;; slime-mdot-fu.el --- Making M-. work on local functions.
;;
;; Author:  Tobias C. Rittweiler <tcr@freebits.de>
;;
;; License: GNU GPL (same license as Emacs)
;;

(require 'slime-enclosing-context)

(defun slime-edit-local-definition (name &optional where)
  "Like `slime-edit-definition', but tries to find the definition
in a local function binding near point."
  (interactive (list (slime-read-symbol-name "Name: ")))
  (multiple-value-bind (binding-name point)
      (multiple-value-call #'some #'(lambda (binding-name point)
				      (when (equalp binding-name name)
					(values binding-name point)))
			   (slime-enclosing-bound-names))
    (when (and binding-name point)
      (slime-edit-definition-cont 
       `((,binding-name
	  ,(make-slime-buffer-location (buffer-name (current-buffer)) point)))
       name
       where))))

(defun slime-mdot-fu-init ()
  (add-hook 'slime-edit-definition-hooks 
	    'slime-edit-local-definition))

(defun slime-mdot-fu-unload ()
  (remove-hook 'slime-edit-definition-hooks 
	       'slime-edit-local-definition))



(def-slime-test find-local-definitions.1
    (buffer-sexpr definition target-regexp)
    "Check that finding local definitions work."
    '(((defun foo (x)
	  (let ((y (+ x 1)))
	    (- x y *HERE*)))
       y
       "(y (+ x 1))")

      ((defun bar (x)
	 (flet ((foo (z) (+ x z)))
	   (* x (foo *HERE*))))
       foo
       "(foo (z) (+ x z))")

      ((defun quux (x)
	 (flet ((foo (z) (+ x z)))
	   (let ((foo (- 1 x)))
	     (+ x foo *HERE*))))
       foo
       "(foo (- 1 x)")
      
      ((defun zurp (x)
	 (macrolet ((frob (x y) `(quux ,x ,y)))
	   (frob x *HERE*)))
       frob
       "(frob (x y)"))
  (slime-check-top-level)
  (with-temp-buffer
    (let ((tmpbuf (current-buffer)))
      (insert (prin1-to-string buffer-sexpr))
      (search-backward "*HERE*")
      (slime-edit-local-definition (prin1-to-string definition))
      (slime-sync)
      (slime-check "Check that we didnt leave the temp buffer." 
	(eq (current-buffer) tmpbuf))
      (slime-check "Check that we are at the local definition."
	(looking-at (regexp-quote target-regexp))))))


(provide 'slime-mdot-fu)


