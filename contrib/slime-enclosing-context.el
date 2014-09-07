(require 'slime)
(require 'slime-parse)

(define-slime-contrib slime-enclosing-context
  "Utilities on top of slime-parse."
  (:authors "Tobias C. Rittweiler <tcr@freebits.de>")
  (:license "GPL")
  (:on-load (error "This contrib does not work at the moment.")))

(defun slime-enclosing-form-specs ()
  (error "This is the reason this contrib does not work at the moment."))

(defun slime-parse-sexp-at-point (_arg)
  (error "This is the reason this contrib does not work at the moment."))

(defun slime-has-symbol-syntax-p (_name)
  (error "This is the reason this contrib does not work at the moment."))

(defvar slime-variable-binding-ops-alist
  '((let &bindings &body)))

(defvar slime-function-binding-ops-alist
  '((flet &bindings &body)
    (labels &bindings &body)
    (macrolet &bindings &body)))

(defun slime-lookup-binding-op (op &optional binding-type)
  (cl-labels ((lookup-in (list) (cl-assoc op list :test 'equalp :key 'symbol-name)))
    (cond ((eq binding-type :variable) (lookup-in slime-variable-binding-ops-alist))
	  ((eq binding-type :function) (lookup-in slime-function-binding-ops-alist))
	  (t (or (lookup-in slime-variable-binding-ops-alist)
		 (lookup-in slime-function-binding-ops-alist))))))

(defun slime-binding-op-p (op &optional binding-type)
  (and (slime-lookup-binding-op op binding-type) t))

(defun slime-binding-op-body-pos (op)
  (let ((special-lambda-list (slime-lookup-binding-op op)))
    (if special-lambda-list (cl-position '&body special-lambda-list))))

(defun slime-binding-op-bindings-pos (op)
  (let ((special-lambda-list (slime-lookup-binding-op op)))
    (if special-lambda-list (cl-position '&bindings special-lambda-list))))

(defun slime-enclosing-bound-names ()
  "Returns all bound function names as first value, and the
points where their bindings are established as second value."
  (cl-multiple-value-call #'slime-find-bound-names
                          (slime-enclosing-form-specs)))

(defun slime-find-bound-names (ops indices points)
  (let ((binding-names) (binding-start-points))
    (save-excursion
      (cl-loop for (op . nil) in ops
               for index in indices
               for point in points
               do (when (and (slime-binding-op-p op)
                             ;; Are the bindings of OP in scope?
                             (>= index (slime-binding-op-body-pos op)))
                    (goto-char point)
                    (forward-sexp (slime-binding-op-bindings-pos op))
                    (down-list)
                    (ignore-errors
                      (cl-loop
                       (down-list)
                       (push (slime-symbol-at-point) binding-names)
                       (push (save-excursion (backward-up-list) (point))
                             binding-start-points)
                       (up-list)))))
      (cl-values (nreverse binding-names) (nreverse binding-start-points)))))


(defun slime-enclosing-bound-functions ()
  (cl-multiple-value-call #'slime-find-bound-functions
                          (slime-enclosing-form-specs)))

(defun slime-find-bound-functions (ops indices points)
  (let ((names) (arglists) (start-points))
    (save-excursion
      (cl-loop for (op . nil) in ops
               for index in indices
               for point in points
               do (when (and (slime-binding-op-p op :function)
                             ;; Are the bindings of OP in scope?
                             (>= index (slime-binding-op-body-pos op)))
                    (goto-char point)
                    (forward-sexp (slime-binding-op-bindings-pos op))
                    (down-list)
                    ;; If we're at the end of the bindings, an error will
                    ;; be signalled by the `down-list' below.
                    (ignore-errors
                      (cl-loop
                       (down-list)
                       (cl-destructuring-bind (name arglist)
                           (slime-parse-sexp-at-point 2)
                         (cl-assert (slime-has-symbol-syntax-p name))
                         (cl-assert arglist)
                         (push name names)
                         (push arglist arglists)
                         (push (save-excursion (backward-up-list) (point))
                               start-points))
                       (up-list)))))
      (cl-values (nreverse names)
                 (nreverse arglists)
                 (nreverse start-points)))))


(defun slime-enclosing-bound-macros ()
  (cl-multiple-value-call #'slime-find-bound-macros
                          (slime-enclosing-form-specs)))

(defun slime-find-bound-macros (ops indices points)
  ;; Kludgy!
  (let ((slime-function-binding-ops-alist '((macrolet &bindings &body))))
    (slime-find-bound-functions ops indices points)))

(provide 'slime-enclosing-context)
