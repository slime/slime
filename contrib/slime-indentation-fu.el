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
                              (get (intern-soft form-operator)
                                   'slime-global-indent))))
      ;; If no &BODY appeared in the arglist, indent like a casual
      ;; function invocation.
      (unless indent-spec
        (setq indent-spec 0))
      (slime-compute-indentation-column
       indent-spec path containing-form-start sexp-column normal-indent))))

(defun slime-update-local-indentation (ops arg-indices points)
  (loop for name in (car (slime-find-bound-macros ops arg-indices points)) do
        (let ((s (intern name)))
          ;; N.B. cases to take into considerations: local macro is
          ;; named like an already existing global macro; such a
          ;; global macro is redefined with a different lambda-list;
          ;; initially there's no global macro, but it's added later.
          ;;
          (put s 'slime-local-indent t)          ; unused at the moment, for debugging.
          (unless (eq (get s 'common-lisp-indent-function) 'slime-indent-fu) 
            (put s 'slime-global-indent (get s 'common-lisp-indent-function)))
          (put s 'common-lisp-indent-function 'slime-indent-fu)
          (put s 'slime-indent 'slime-indent-fu) ; for redefinition to be taken up
          )))

(defun slime-indentation-fu-init ()
  (add-hook 'slime-autodoc-hook 'slime-update-local-indentation))

(defun slime-indentation-fu-unload ()
  (remove-hook 'slime-autodoc-hook 'slime-update-local-indentation))


;;; Tests.

(def-slime-test local-indentation.1 (buffer-content point-markers)
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
        14))))

\(defun zabing (x y)
  (let ((bar 42))
    (macrolet ((barf (a b) `(progn ,a ,b)))
      (barf x
            *HERE3*))))"
       ("*HERE3*" "*HERE2*" "*HERE1*")))
  (with-temp-buffer
    (lisp-mode)
    (slime-mode 1)
    (slime-autodoc-mode 1)
    (insert buffer-content)
    (slime-compile-region (point-min) (point-max))
    (dolist (marker point-markers)
      (search-backward marker)
      (slime-compute-autodoc)         ; updates indentation implicitly
      (slime-sync-to-top-level 3)
      (beginning-of-defun)
      (indent-sexp))
    (slime-test-expect "Correct buffer content"
                       buffer-content
                       (substring-no-properties (buffer-string))))  
  )

(provide 'slime-indentation-fu)