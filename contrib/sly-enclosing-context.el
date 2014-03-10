(require 'sly)
(require 'sly-parse)

(define-sly-contrib sly-enclosing-context
  "Utilities on top of sly-parse."
  (:authors "Tobias C. Rittweiler <tcr@freebits.de>")
  (:license "GPL")
  (:on-load (error "This contrib does not work at the moment.")))

(defun sly-enclosing-form-specs ()
  (error "This is the reason this contrib does not work at the moment."))

(defun sly-parse-sexp-at-point (_arg)
  (error "This is the reason this contrib does not work at the moment."))

(defun sly-has-symbol-syntax-p (_name)
  (error "This is the reason this contrib does not work at the moment."))

(defvar sly-variable-binding-ops-alist
  '((let &bindings &body)))

(defvar sly-function-binding-ops-alist
  '((flet &bindings &body)
    (labels &bindings &body)
    (macrolet &bindings &body)))

(defun sly-lookup-binding-op (op &optional binding-type)
  (cl-labels ((lookup-in (list) (cl-assoc op list :test 'equalp :key 'symbol-name)))
    (cond ((eq binding-type :variable) (lookup-in sly-variable-binding-ops-alist))
	  ((eq binding-type :function) (lookup-in sly-function-binding-ops-alist))
	  (t (or (lookup-in sly-variable-binding-ops-alist)
		 (lookup-in sly-function-binding-ops-alist))))))

(defun sly-binding-op-p (op &optional binding-type)
  (and (sly-lookup-binding-op op binding-type) t))

(defun sly-binding-op-body-pos (op)
  (when-let (special-lambda-list (sly-lookup-binding-op op))
    (cl-position '&body special-lambda-list)))

(defun sly-binding-op-bindings-pos (op)
  (when-let (special-lambda-list (sly-lookup-binding-op op))
    (cl-position '&bindings special-lambda-list)))


(defun sly-enclosing-bound-names ()
  "Returns all bound function names as first value, and the
points where their bindings are established as second value."
  (cl-multiple-value-call #'sly-find-bound-names
                          (sly-enclosing-form-specs)))

(defun sly-find-bound-names (ops indices points)
  (let ((binding-names) (binding-start-points))
    (save-excursion
      (cl-loop for (op . nil) in ops
               for index in indices
               for point in points
               do (when (and (sly-binding-op-p op)
                             ;; Are the bindings of OP in scope?
                             (>= index (sly-binding-op-body-pos op)))
                    (goto-char point)
                    (forward-sexp (sly-binding-op-bindings-pos op))
                    (down-list)
                    (ignore-errors
                      (cl-loop
                       (down-list)
                       (push (sly-symbol-at-point) binding-names)
                       (push (save-excursion (backward-up-list) (point))
                             binding-start-points)
                       (up-list)))))
      (cl-values (nreverse binding-names) (nreverse binding-start-points)))))


(defun sly-enclosing-bound-functions ()
  (cl-multiple-value-call #'sly-find-bound-functions
                          (sly-enclosing-form-specs)))

(defun sly-find-bound-functions (ops indices points)
  (let ((names) (arglists) (start-points))
    (save-excursion
      (cl-loop for (op . nil) in ops
               for index in indices
               for point in points
               do (when (and (sly-binding-op-p op :function)
                             ;; Are the bindings of OP in scope?
                             (>= index (sly-binding-op-body-pos op)))
                    (goto-char point)
                    (forward-sexp (sly-binding-op-bindings-pos op))
                    (down-list)
                    ;; If we're at the end of the bindings, an error will
                    ;; be signalled by the `down-list' below.
                    (ignore-errors
                      (cl-loop
                       (down-list)
                       (cl-destructuring-bind (name arglist)
                           (sly-parse-sexp-at-point 2)
                         (cl-assert (sly-has-symbol-syntax-p name))
                         (cl-assert arglist)
                         (push name names)
                         (push arglist arglists)
                         (push (save-excursion (backward-up-list) (point))
                               start-points))
                       (up-list)))))
      (cl-values (nreverse names)
                 (nreverse arglists)
                 (nreverse start-points)))))


(defun sly-enclosing-bound-macros ()
  (cl-multiple-value-call #'sly-find-bound-macros
                          (sly-enclosing-form-specs)))

(defun sly-find-bound-macros (ops indices points)
  ;; Kludgy!
  (let ((sly-function-binding-ops-alist '((macrolet &bindings &body))))
    (sly-find-bound-functions ops indices points)))

(provide 'sly-enclosing-context)
