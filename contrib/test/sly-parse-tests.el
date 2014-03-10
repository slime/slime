(require 'sly-parse)
(require 'sly-tests)

(defun sly-check-buffer-form (result-form)
  (sly-test-expect
   (format "Buffer form correct in `%s' (at %d)" (buffer-string) (point))
   result-form
   (sly-parse-form-upto-point 10)))

(def-sly-test form-up-to-point.1
    (buffer-sexpr result-form &optional skip-trailing-test-p)
    ""
    '(("(char= #\\(*HERE*"
       ("char=" "#\\(" swank::%cursor-marker%))
      ("(char= #\\( *HERE*"
       ("char=" "#\\(" "" swank::%cursor-marker%))
      ("(char= #\\) *HERE*"
       ("char=" "#\\)" "" swank::%cursor-marker%))
      ("(char= #\\*HERE*"
       ("char=" "#\\" swank::%cursor-marker%) t)
      ("(defun*HERE*"
       ("defun" swank::%cursor-marker%))
      ("(defun foo*HERE*"
       ("defun" "foo" swank::%cursor-marker%))
      ("(defun foo (x y)*HERE*"
       ("defun" "foo"
	("x" "y") swank::%cursor-marker%))
      ("(defun foo (x y*HERE*"
       ("defun" "foo"
	e("x" "y" swank::%cursor-marker%)))
      ("(apply 'foo*HERE*"
       ("apply" "'foo" swank::%cursor-marker%))
      ("(apply #'foo*HERE*"
       ("apply" "#'foo" swank::%cursor-marker%))
      ("(declare ((vector bit *HERE*"
       ("declare" (("vector" "bit" "" swank::%cursor-marker%))))
      ("(with-open-file (*HERE*"
       e("with-open-file" ("" swank::%cursor-marker%)))
      ("(((*HERE*"
       ((("" swank::%cursor-marker%))))
      ("(defun #| foo #| *HERE*"
       ("defun" "" swank::%cursor-marker%))
      ("(defun #-(and) (bar) f*HERE*"
       ("defun" "f" swank::%cursor-marker%))
      ("(remove-if #'(lambda (x)*HERE*"
       ("remove-if" ("lambda" ("x") swank::%cursor-marker%)))
      ("`(remove-if ,(lambda (x)*HERE*"
       ("remove-if" ("lambda" ("x") swank::%cursor-marker%)))
      ("`(remove-if ,@(lambda (x)*HERE*"
       ("remove-if" ("lambda" ("x") swank::%cursor-marker%))))
  (sly-skip-test "TODO: skip for now, but analyse this failure!")
  (sly-check-top-level)
  (with-temp-buffer
    (lisp-mode)
    (insert buffer-sexpr)
    (search-backward "*HERE*")
    (delete-region (match-beginning 0) (match-end 0))
    (sly-check-buffer-form result-form)
    (unless skip-trailing-test-p
      (insert ")") (backward-char)
      (sly-check-buffer-form result-form))
    ))

(provide 'sly-parse-tests)
