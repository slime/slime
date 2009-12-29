;;; slime-parse.el --- parsing of Common Lisp source code
;;
;; Authors: Matthias Koeppe  <mkoeppe@mail.math.uni-magdeburg.de>
;;          Tobias C. Rittweiler <tcr@freebits.de>
;;          and others
;; 
;; License: GNU GPL (same license as Emacs)
;;

(defun slime-parse-form-until (limit form-suffix)
  "Parses form from point to `limit'."
  ;; For performance reasons, this function does not use recursion.
  (let ((todo (list (point))) ; stack of positions
        (sexps)               ; stack of expressions
        (cursexp)
        (curpos)
        (depth 1))            ; This function must be called from the
                              ; start of the sexp to be parsed.
    (while (and (setq curpos (pop todo))
                (progn
                  (goto-char curpos)
                  ;; (Here we also move over suppressed
                  ;; reader-conditionalized code! Important so CL-side
                  ;; of autodoc won't see that garbage.)
                  (ignore-errors (slime-forward-cruft))
                  (< (point) limit)))
      (setq cursexp (pop sexps))
      (cond
        ;; End of an sexp?
        ((or (looking-at "\\s)") (eolp))
         (decf depth)
         (push (nreverse cursexp) (car sexps)))
        ;; Start of a new sexp?
        ((looking-at "\\s'*\\s(")
         (let ((subpt (match-end 0)))
           (ignore-errors
             (forward-sexp)
             ;; (In case of error, we're at an incomplete sexp, and
             ;; nothing's left todo after it.)
             (push (point) todo))
           (push cursexp sexps)
           (push subpt todo)            ; to descend into new sexp
           (push nil sexps)
           (incf depth)))
        ;; In mid of an sexp..
        (t
         (let ((pt1 (point))
               (pt2 (condition-case e
                        (progn (forward-sexp) (point))
                      (scan-error
                       (fourth e)))))   ; end of sexp
           (push (buffer-substring-no-properties pt1 pt2) cursexp)
           (push pt2 todo)
           (push cursexp sexps)))))
    (when sexps
      (setf (car sexps) (nreconc form-suffix (car sexps)))
      (while (> depth 1)
        (push (nreverse (pop sexps)) (car sexps))
        (decf depth))
      (nreverse (car sexps)))))

(defun slime-compare-char-syntax (get-char-fn syntax &optional unescaped)
  "Returns t if the character that `get-char-fn' yields has
characer syntax of `syntax'. If `unescaped' is true, it's ensured
that the character is not escaped."
  (let ((char        (funcall get-char-fn (point)))
	(char-before (funcall get-char-fn (1- (point)))))
    (if (and char (eq (char-syntax char) (aref syntax 0)))
	(if unescaped
	    (or (null char-before)
		(not (eq (char-syntax char-before) ?\\)))
	    t)
        nil)))

(defconst slime-cursor-marker 'swank::%cursor-marker%)

(defun slime-parse-form-upto-point (&optional max-levels)
  (save-restriction
    ;; Don't parse more than 500 lines before point, so we don't spend
    ;; too much time. NB. Make sure to go to beginning of line, and
    ;; not possibly anywhere inside comments or strings.
    (narrow-to-region (line-beginning-position -500) (point-max))
    (save-excursion
      (let ((suffix (list slime-cursor-marker)))
        (cond ((slime-compare-char-syntax #'char-after "(" t)
               ;; We're at the start of some expression, so make sure
               ;; that SWANK::%CURSOR-MARKER% will come after that
               ;; expression.
               (ignore-errors (forward-sexp)))
              ((or (bolp) (slime-compare-char-syntax #'char-before " " t))
               ;; We're after some expression, so we have to make sure
               ;; that %CURSOR-MARKER% does not come directly after that
               ;; expression.
               (push "" suffix))
              ((slime-compare-char-syntax #'char-before "(" t)
               ;; We're directly after an opening parenthesis, so we
               ;; have to make sure that something comes before
               ;; %CURSOR-MARKER%.
               (push "" suffix))
              (t
               ;; We're at a symbol, so make sure we get the whole symbol.
               (slime-end-of-symbol)))
        (let ((pt (point)))
          (ignore-errors (up-list (if max-levels (- max-levels) -5)))
          (ignore-errors (down-list))
          (slime-parse-form-until pt suffix))))))


;;;; Test cases

(defun slime-check-buffer-form (result-form)
  (slime-test-expect 
   (format "Buffer form correct in `%s' (at %d)" (buffer-string) (point))
   result-form
   (slime-parse-form-upto-point 10)))

(def-slime-test form-up-to-point.1
    (buffer-sexpr result-form &optional skip-trailing-test-p)
    ""
    '(("(char= #\\(*HERE*"               ("char=" "#\\(" swank::%cursor-marker%))
      ("(char= #\\( *HERE*"              ("char=" "#\\(" "" swank::%cursor-marker%))
      ("(char= #\\) *HERE*"              ("char=" "#\\)" "" swank::%cursor-marker%))
      ("(char= #\\*HERE*"                ("char=" "#\\" swank::%cursor-marker%) t)
      ("(defun*HERE*"                    ("defun" swank::%cursor-marker%))
      ("(defun foo*HERE*"                ("defun" "foo" swank::%cursor-marker%))
      ("(defun foo (x y)*HERE*"          ("defun" "foo" ("x" "y") swank::%cursor-marker%))
      ("(defun foo (x y*HERE*"           ("defun" "foo" ("x" "y" swank::%cursor-marker%)))
      ("(apply 'foo*HERE*"               ("apply" "'foo" swank::%cursor-marker%))
      ("(apply #'foo*HERE*"              ("apply" "#'foo" swank::%cursor-marker%))
      ("(declare ((vector bit *HERE*"    ("declare" (("vector" "bit" "" swank::%cursor-marker%))))
      ("(with-open-file (*HERE*"         ("with-open-file" ("" swank::%cursor-marker%)))
      ("(((*HERE*"                       ((("" swank::%cursor-marker%))))
      ("(defun #| foo #| *HERE*"         ("defun" "" swank::%cursor-marker%))
      ("(defun #-(and) (bar) f*HERE*"    ("defun" "f" swank::%cursor-marker%))
      ("(remove-if #'(lambda (x)*HERE*"  ("remove-if" ("lambda" ("x") swank::%cursor-marker%)))
      ("`(remove-if ,(lambda (x)*HERE*"  ("remove-if" ("lambda" ("x") swank::%cursor-marker%)))
      ("`(remove-if ,@(lambda (x)*HERE*" ("remove-if" ("lambda" ("x") swank::%cursor-marker%))))
  (slime-check-top-level)
  (with-temp-buffer
    (lisp-mode)
    (insert buffer-sexpr)
    (search-backward "*HERE*")
    (delete-region (match-beginning 0) (match-end 0))
    (slime-check-buffer-form result-form)
    (unless skip-trailing-test-p
      (insert ")") (backward-char)
      (slime-check-buffer-form result-form))
    ))

(provide 'slime-parse)

(let ((byte-compile-warnings '()))
  (mapc #'byte-compile
        '(slime-parse-form-upto-point
          slime-parse-form-until
          slime-compare-char-syntax
          )))