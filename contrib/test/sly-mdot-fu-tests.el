(require 'sly-mdot-fu)
(require 'sly-tests)

(def-sly-test find-local-definitions.1
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
  (sly-check-top-level)
  (with-temp-buffer
    (let ((tmpbuf (current-buffer)))
      (insert (prin1-to-string buffer-sexpr))
      (search-backward "*HERE*")
      (sly-edit-local-definition (prin1-to-string definition))
      (sly-sync)
      (sly-check "Check that we didnt leave the temp buffer."
	(eq (current-buffer) tmpbuf))
      (sly-check "Check that we are at the local definition."
	(looking-at (regexp-quote target-regexp))))))

(provide 'sly-mdot-fu-tests)
