;;; slime-indentation-fu.el --- Correct indentation of local macros.
;;
;; Author:  Tobias C. Rittweiler <tcr@freebits.de>
;;
;; License: GNU GPL (same license as Emacs)
;;

(require 'slime-autodoc)

(slime-require :swank-indentation-fu)

(defun slime-indentation-spec (arglist-string)
  (slime-eval `(swank:arglist-indentation ,arglist-string)))

(defun slime-enclosing-macro-arglist (name)
  (multiple-value-bind (macro-names arglists arglist-pts)
      (slime-enclosing-bound-macros)
    (when-let (pos (position name macro-names :test 'equal))
      (nth pos arglists))))

;;; This was copied straight from the aweful cruft that is
;;; cl-indent.el (Emacs 23.0.91.2)
(defun slime-compute-indentation-column
    (method path containing-form-start sexp-column normal-indent)
  (cond ((cdr path)
         normal-indent)
        ((<= (car path) method)
         ;; `distinguished' form
         (list (+ sexp-column 4)
               containing-form-start))
        ((= (car path) (1+ method))
         ;; first body form.
         (+ sexp-column lisp-body-indent))
        (t
         ;; other body form
         normal-indent)))

(defun slime-indent-fu (path state indent-point sexp-column normal-indent)
  (let* ((containing-form-start (nth 1 state))
         (form-operator 
          (save-excursion (goto-char (1+ containing-form-start))
                          (slime-symbol-at-point))))
    (assert form-operator)
    (let* ((local-arglist (slime-enclosing-macro-arglist form-operator))
           (indent-spec   (if local-arglist
                              (slime-indentation-spec local-arglist)
                              (get (intern-soft form-operator) 'slime-indent))))
      (slime-compute-indentation-column
       indent-spec path containing-form-start sexp-column normal-indent))))

(defun slime-update-local-indentation (ops arg-indices points)
  (loop for name in (car (slime-find-bound-macros ops arg-indices points)) do 
        (put (intern name) 'slime-local-indent t) 
        (put (intern name) 'common-lisp-indent-function 'slime-indent-fu)))

(defun slime-indentation-fu-init ()
  (add-hook 'slime-autodoc-hook 'slime-update-local-indentation))

(defun slime-indentation-fu-unload ()
  (remove-hook 'slime-autodoc-hook 'slime-update-local-indentation))


;;; Tests.

(def-slime-test local-indentation.1 (buffer-content)
        "Check that indentation of MACROLET bound macros work."
    '(("
\(in-package :swank)

\(defmacro zurp (x &body body)
  `(progn ,x ,@body))

\(defun quux (foo)
  (zurp foo
    12
    *HERE1*
    14))

\(defun foo (x y)
  (let ((bar 42))
    (macrolet ((zurp (a b &body body)
                 `(progn ,a ,b ,@body)))
      (zurp x
          y
        bar
        *HERE2*
        14))))"))
  (with-temp-buffer
    (lisp-mode)
    (slime-mode 1)
    (slime-autodoc-mode 1)
    (insert buffer-content)
    (slime-compile-region (point-min) (point-max))
    (search-backward "*HERE2*")
    (slime-compute-autodoc)             ; updates indentation implicitly
    (slime-sync-to-top-level 3)
    (beginning-of-defun)
    (indent-sexp)
    (search-backward "*HERE1*")
    (beginning-of-defun)
    (indent-sexp)
    (slime-test-expect "Correct buffer content"
                       buffer-content
                       (substring-no-properties (buffer-string))))  
  )

(provide 'slime-indentation-fu)