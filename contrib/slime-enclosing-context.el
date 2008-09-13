;;; slime-enclosing-context.el --- Utilities on top of slime-parse.
;;
;; Author:  Tobias C. Rittweiler <tcr@freebits.de>
;;
;; License: GNU GPL (same license as Emacs)
;;

(require 'slime-parse)

(defvar slime-variable-binding-ops-alist
  '((let &bindings &body)))

(defvar slime-function-binding-ops-alist
  '((flet &bindings &body) 
    (labels &bindings &body)
    (macrolet &bindings &body)))

(defun slime-lookup-binding-op (op &optional binding-type)
  (flet ((lookup-in (list) (assoc* op list :test 'equalp :key 'symbol-name)))
    (cond ((eq binding-type :variable) (lookup-in slime-variable-binding-ops-alist))
	  ((eq binding-type :function) (lookup-in slime-function-binding-ops-alist))
	  (t (or (lookup-in slime-variable-binding-ops-alist)
		 (lookup-in slime-function-binding-ops-alist))))))

(defun slime-binding-op-p (op &optional binding-type)
  (and (slime-lookup-binding-op op binding-type) t))

(defun slime-binding-op-body-pos (op)
  (when-let (special-lambda-list (slime-lookup-binding-op op))
    (position '&body special-lambda-list)))

(defun slime-binding-op-bindings-pos (op)
  (when-let (special-lambda-list (slime-lookup-binding-op op))
    (position '&bindings special-lambda-list)))


(defun slime-enclosing-bound-names ()
  "Returns all bound function names as first value, and the
points where their bindings are established as second value."
  (multiple-value-call #'slime-find-bound-names (slime-enclosing-form-specs)))

(defun slime-find-bound-names (ops indices points)
  (let ((binding-names) (binding-start-points))
    (save-excursion
      (loop for (op . nil) in ops
	    for index in indices
	    for point in points
	    do (when (and (slime-binding-op-p op) 
			  ;; Are the bindings of OP in scope?
			  (= index (slime-binding-op-body-pos op)))
		 (goto-char point) 
		 (forward-sexp (slime-binding-op-bindings-pos op))
		 (down-list)
		 (ignore-errors
		   (loop 
		    (down-list) 
		    (push (slime-parse-symbol-name-at-point 1) binding-names)
		    (push (save-excursion (backward-up-list) (point)) 
			  binding-start-points)
		    (up-list)))))
      (values (nreverse binding-names) (nreverse binding-start-points)))))


(defun slime-enclosing-bound-functions ()
  (multiple-value-call #'slime-find-bound-functions (slime-enclosing-form-specs)))

(defun slime-find-bound-functions (ops indices points)
  (let ((names) (arglists) (start-points))
    (save-excursion
      (loop for (op . nil) in ops
	    for index in indices
	    for point in points
	    do (when (and (slime-binding-op-p op :function) 
			  ;; Are the bindings of OP in scope?
			  (= index (slime-binding-op-body-pos op)))
		 (goto-char point)
		 (forward-sexp (slime-binding-op-bindings-pos op))
		 (down-list)
		 (ignore-errors
		   (loop 
		    (down-list) 
		    (destructuring-bind (name arglist)
			(slime-ensure-list (slime-parse-sexp-at-point 2))
		      (assert (slime-has-symbol-syntax-p name)) (assert arglist)
		      (push name names)
		      (push arglist arglists)
		      (push (save-excursion (backward-up-list) (point)) 
			    start-points))
		    (up-list)))))
      (values (nreverse names)
	      (nreverse arglists) 
	      (nreverse start-points)))))


(def-slime-test enclosing-context.1
    (buffer-sexpr wished-bound-names wished-bound-functions)
    "Check that finding local definitions work."
    '(("(flet ((,nil ()))
	 (let ((bar 13)
	       (,foo 42))
	   *HERE*))" 
       (",nil" "bar" ",foo")
       ((",nil" "()"))))
  (slime-check-top-level)
  (with-temp-buffer
    (let ((tmpbuf (current-buffer)))
      (lisp-mode)
      (insert buffer-sexpr)
      (search-backward "*HERE*")
      (multiple-value-bind (bound-names points) 
	  (slime-enclosing-bound-names)
	(slime-check "Check enclosing bound names"
	  (loop for name in wished-bound-names
		always (member name bound-names))))
      (multiple-value-bind (fn-names fn-arglists points) 
	  (slime-enclosing-bound-functions)
	(slime-check "Check enclosing bound functions"
	  (loop for (name arglist) in wished-bound-functions
		always (and (member name fn-names)
			    (member arglist fn-arglists)))))
      )))



(provide 'slime-enclosing-context)