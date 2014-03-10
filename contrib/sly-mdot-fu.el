(require 'sly)
(require 'cl-lib)

(define-sly-contrib sly-mdot-fu
  "Making M-. work on local functions."
  (:authors "Tobias C. Rittweiler <tcr@freebits.de>")
  (:license "GPL")
  (:sly-dependencies sly-enclosing-context)
  (:on-load
   (add-hook 'sly-edit-definition-hooks 'sly-edit-local-definition))
  (:on-unload
   (remove-hook 'sly-edit-definition-hooks 'sly-edit-local-definition)))


(defun sly-edit-local-definition (name &optional where)
  "Like `sly-edit-definition', but tries to find the definition
in a local function binding near point."
  (interactive (list (sly-read-symbol-name "Name: ")))
  (cl-multiple-value-bind (binding-name point)
      (cl-multiple-value-call #'cl-some #'(lambda (binding-name point)
                                            (when (cl-equalp binding-name name)
                                              (cl-values binding-name point)))
                              (sly-enclosing-bound-names))
    (when (and binding-name point)
      (sly-edit-definition-cont
       `((,binding-name
	  ,(make-sly-buffer-location (buffer-name (current-buffer)) point)))
       name
       where))))

(provide 'sly-mdot-fu)
