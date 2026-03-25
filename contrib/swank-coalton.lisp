;;; swank-coalton.lisp --- Coalton autodoc support
;;
;; License: Public Domain
;;

(in-package :swank)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (swank-require :swank-arglists))

(defun package-uses-package-p (package used-package)
  (member used-package (package-use-list package) :test #'eq))

(defun coalton-autodoc-candidate-p (operator)
  (let* ((coalton-package (find-package "COALTON"))
         (operator-package (and (symbolp operator) (symbol-package operator))))
    (and coalton-package
         operator-package
         (or (eq operator-package coalton-package)
             (package-uses-package-p operator-package coalton-package)))))

(defun coalton-autodoc-info (operator)
  (handler-case
      (when (coalton-autodoc-candidate-p operator)
        (let* ((tc-package (find-package "COALTON-IMPL/TYPECHECKER"))
               (entry-package (find-package "COALTON-IMPL/ENTRY"))
               (lookup-value-type (and tc-package
                                       (find-symbol "LOOKUP-VALUE-TYPE"
                                                    tc-package)))
               (type-to-string (and tc-package
                                    (find-symbol "TYPE-TO-STRING" tc-package)))
               (global-environment (and entry-package
                                        (find-symbol "*GLOBAL-ENVIRONMENT*"
                                                     entry-package))))
          (when (and lookup-value-type
                     type-to-string
                     global-environment
                     (fboundp lookup-value-type)
                     (fboundp type-to-string)
                     (boundp global-environment))
            (let* ((env (symbol-value global-environment))
                   (type (funcall lookup-value-type env operator :no-error t)))
              (when type
                (values type env type-to-string))))))
    (serious-condition ()
      nil)))

(defun coalton-autodoc-operator-p (operator)
  (nth-value 0 (coalton-autodoc-info operator)))

(defun coalton-autodoc-operator-string (operator)
  (let ((*print-case* :downcase)
        (*print-escape* nil)
        (*print-readably* nil)
        (*print-pretty* nil)
        (*print-circle* nil)
        (*print-level* 10)
        (*print-length* 20))
    (princ-to-string operator)))

(defun coalton-autodoc (form _arglist _obj-at-cursor _form-path
                        _print-right-margin)
  (declare (ignore _arglist _obj-at-cursor _form-path _print-right-margin))
  (when (and (consp form)
             (symbolp (car form)))
    (multiple-value-bind (type env type-to-string)
        (coalton-autodoc-info (car form))
      (when type
        (values (format nil "~A :: ~A"
                        (coalton-autodoc-operator-string (car form))
                        (funcall type-to-string type env))
                t)))))

(pushnew #'coalton-autodoc-operator-p *autodoc-operator-functions*)
(pushnew #'coalton-autodoc *autodoc-functions*)

(provide :swank-coalton)
