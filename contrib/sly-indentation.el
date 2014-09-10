(require 'sly)
(require 'sly-cl-indent)
(require 'cl-lib)

(define-sly-contrib sly-indentation
  "Contrib interfacing `sly-cl-indent' and SLY."
  (:slynk-dependencies slynk-indentation)
  (:on-load
   (setq common-lisp-current-package-function 'sly-current-package)))

(defun sly-update-system-indentation (symbol indent packages)
  (let ((list (gethash symbol common-lisp-system-indentation))
        (ok nil))
    (if (not list)
        (puthash symbol (list (cons indent packages))
                 common-lisp-system-indentation)
      (dolist (spec list)
        (cond ((equal (car spec) indent)
               (dolist (p packages)
                 (unless (member p (cdr spec))
                   (push p (cdr spec))))
               (setf ok t))
              (t
               (setf (cdr spec)
                     (cl-set-difference (cdr spec) packages :test 'equal)))))
      (unless ok
        (puthash symbol (cons (cons indent packages)
                              list)
                 common-lisp-system-indentation)))))

(provide 'sly-indentation)
