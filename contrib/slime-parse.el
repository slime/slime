;;; slime-parse.el --- parsing of Common Lisp source code
;;
;; Authors: Matthias Koeppe  <mkoeppe@mail.math.uni-magdeburg.de>
;;          Tobias C. Rittweiler <tcr@freebits.de>
;;          and others
;; 
;; License: GNU GPL (same license as Emacs)
;;

(defun slime-incomplete-form-at-point ()
  "Looks for a ``raw form spec'' around point to be processed by
SWANK::PARSE-FORM-SPEC. It is similiar to
SLIME-INCOMPLETE-SEXP-AT-POINT but looks further back than just
one sexp to find out the context."
  (multiple-value-bind (operators arg-indices points)
      (slime-enclosing-form-specs)
    (if (null operators)
        ""
        (let ((op        (first operators))
	      (op-start  (first points))
	      (arg-index (first arg-indices)))
          (destructure-case (slime-ensure-list op)
            ((:declaration declspec) op)
            ((:type-specifier typespec) op)
            (t 
	     (slime-make-form-spec-from-string 
	      (concat (slime-incomplete-sexp-at-point) ")"))))))))

(defun slime-parse-sexp-at-point (&optional n skip-blanks-p)
  "Returns the sexps at point as a list of strings, otherwise nil.
\(If there are not as many sexps as N, a list with < N sexps is
returned.\) 
If SKIP-BLANKS-P is true, leading whitespaces &c are skipped.
"
  (interactive "p") (or n (setq n 1))
  (flet ((sexp-at-point (first-choice)
           (let ((string (if (eq first-choice :symbol-first)
                             (or (slime-symbol-at-point)
                                 (thing-at-point 'sexp))
                             (or (thing-at-point 'sexp)
                                 (slime-symbol-at-point)))))
             (if string (substring-no-properties string) nil))))
    (save-excursion
      (when skip-blanks-p ; e.g. `( foo bat)' where point is after ?\(.
        (slime-forward-blanks))
      (let ((result nil))
        (dotimes (i n)
          (push (slime-sexp-at-point) result)
          ;; Skip current sexp
          (ignore-errors (forward-sexp) (slime-forward-blanks))
          ;; Is there an additional sexp in front of us?
          (save-excursion
            (unless (slime-point-moves-p (ignore-errors (forward-sexp)))
              (return))))
        (nreverse result)))))

(defun slime-has-symbol-syntax-p (string)
  (if (and string (not (zerop (length string))))
      (member (char-syntax (aref string 0)) 
	      '(?w ?_ ?\' ?\\))))

(defun slime-incomplete-sexp-at-point (&optional n)
  (interactive "p") (or n (setq n 1))
  (buffer-substring-no-properties (save-excursion (backward-up-list n) (point))
                                  (point)))


(defun slime-parse-extended-operator-name (user-point forms indices points)
  "Assume that point is directly at the operator that should be parsed.
USER-POINT is the value of `point' where the user was looking at.
OPS, INDICES and POINTS are updated to reflect the new values after
parsing, and are then returned back as multiple values."
  ;; OPS, INDICES and POINTS are like the finally returned values of
  ;; SLIME-ENCLOSING-FORM-SPECS except that they're in reversed order,
  ;; i.e. the leftmost operator comes first.
  (save-excursion
    (ignore-errors
      (let* ((current-op (first (first forms)))
             (op-name (upcase (slime-cl-symbol-name current-op)))
             (assoc (assoc op-name slime-extended-operator-name-parser-alist))
             (entry (cdr assoc))
             (parser (if (and entry (listp entry)) 
                         (apply (first entry) (rest entry))
                         entry)))
        (ignore-errors
          (forward-char (1+ (length current-op)))
          (slime-forward-blanks))
        (when parser
          (multiple-value-setq (forms indices points)
            ;; We pass the fully qualified name (`current-op'), so it's the
            ;; fully qualified name that will be sent to SWANK.
            (funcall parser current-op user-point forms indices points))))))
  (values forms indices points))


(defvar slime-extended-operator-name-parser-alist
  '(("MAKE-INSTANCE"  . (slime-make-extended-operator-parser/look-ahead 1))
    ("MAKE-CONDITION" . (slime-make-extended-operator-parser/look-ahead 1))
    ("ERROR"          . (slime-make-extended-operator-parser/look-ahead 1))
    ("SIGNAL"         . (slime-make-extended-operator-parser/look-ahead 1))
    ("WARN"           . (slime-make-extended-operator-parser/look-ahead 1))
    ("CERROR"         . (slime-make-extended-operator-parser/look-ahead 2))
    ("CHANGE-CLASS"   . (slime-make-extended-operator-parser/look-ahead 2))
    ("DEFMETHOD"      . (slime-make-extended-operator-parser/look-ahead 1))
    ("DEFINE-COMPILER-MACRO" . (slime-make-extended-operator-parser/look-ahead 1))
    ("APPLY"          . (slime-make-extended-operator-parser/look-ahead 1))
    ("DECLARE"        . slime-parse-extended-operator/declare)
    ("DECLAIM"        . slime-parse-extended-operator/declare)
    ("PROCLAIM"       . slime-parse-extended-operator/proclaim)
    ("CHECK-TYPE"     . slime-parse-extended-operator/check-type)
    ("TYPEP"          . slime-parse-extended-operator/check-type)
    ("THE"            . slime-parse-extended-operator/the)))


(defun slime-make-extended-operator-parser/look-ahead (steps)
  "Returns a parser that parses the current operator at point
plus (at most) STEPS-many additional sexps on the right side of
the operator."
  (lexical-let ((n steps))
    #'(lambda (name user-point current-forms current-indices current-points)
        (let ((old-forms (rest current-forms))
              (arg-idx   (first current-indices)))
          (when (and (not (zerop arg-idx)) ; point is at CAR of form?
                     (not (= (point)       ; point is at end of form?
                             (save-excursion
                               (ignore-errors (slime-end-of-list))
                               (point)))))
            (let* ((args (slime-parse-sexp-at-point n))
                   (arg-specs (mapcar #'slime-make-form-spec-from-string args)))
              (setq current-forms (cons `(,name ,@arg-specs) old-forms))))
          (values current-forms current-indices current-points)
          ))))

;;; FIXME: We display "(proclaim (optimize ...))" instead of the
;;; correct "(proclaim '(optimize ...))".
(defun slime-parse-extended-operator/proclaim (&rest args)
  (when (looking-at "['`]")
    (forward-char)
    (apply #'slime-parse-extended-operator/declare args)))

(defun slime-parse-extended-operator/declare
    (name user-point current-forms current-indices current-points)
  (when (looking-at "(")
    (goto-char user-point)
    (slime-end-of-symbol)
    ;; Head of CURRENT-FORMS is "declare" (or similiar) at this
    ;; point, but we're interested in what comes next.
    (let* ((decl-indices (rest current-indices))
           (decl-points  (rest current-points))
           (decl-pos     (1- (first decl-points)))
           (nesting      (slime-nesting-until-point decl-pos))
           (declspec-str (concat (slime-incomplete-sexp-at-point nesting)
                                 (make-string nesting ?\)))))
      (save-match-data ; `(declare ((foo ...))' or `(declare (type (foo ...)))' ?
        (if (or (eql 0 (string-match "\\s-*(\\((\\(\\sw\\|\\s_\\|\\s-\\)*)\\))$"
                                     declspec-str))
                (eql 0 (string-match "\\s-*(type\\s-*\\((\\(\\sw\\|\\s_\\|\\s-\\)*)\\))$"
                                     declspec-str)))
            (let* ((typespec-str (match-string 1 declspec-str))
                   (typespec (slime-make-form-spec-from-string typespec-str)))
              (setq current-forms   (list `(:type-specifier ,typespec)))
              (setq current-indices (list (second decl-indices)))
              (setq current-points  (list (second decl-points))))
            (let ((declspec (slime-make-form-spec-from-string declspec-str)))
              (setq current-forms   (list `(,name) `(:declaration ,declspec)))
              (setq current-indices (list (first current-indices)
                                          (first decl-indices)))
              (setq current-points  (list (first current-points)
                                          (first decl-points))))))))
  (values current-forms current-indices current-points))

(defun slime-parse-extended-operator/check-type
    (name user-point current-forms current-indices current-points)
  (let ((arg-idx        (first current-indices))
        (typespec       (second current-forms))
        (typespec-start (second current-points)))
    (when (and (eql 2 arg-index)
               typespec                   ; `(check-type ... (foo |' ?
               (if (equalp name "typep")  ; `(typep ... '(foo |' ?
                   (progn (goto-char (- typespec-start 2))
                          (looking-at "['`]"))
                   t))
        ;; compound types VALUES and FUNCTION are not allowed in TYPEP
        ;; (and consequently CHECK-TYPE.)
        (unless (member (first typespec) '("values" "function"))
          (setq current-forms   `((:type-specifier ,typespec)))
          (setq current-indices (rest current-indices))
          (setq current-points  (rest current-points))))
    (values current-forms current-indices current-points)))

(defun slime-parse-extended-operator/the
    (name user-point current-forms current-indices current-points)
  (let ((arg-idx  (first current-indices))
        (typespec (second current-forms)))
    (if (and (eql 1 arg-idx) typespec)  ; `(the (foo |' ?
        (values `((:type-specifier ,typespec))
                (rest current-indices)
                (rest current-points))
        (values current-forms current-indices current-points))))



(defun slime-nesting-until-point (target-point)
  "Returns the nesting level between current point and TARGET-POINT.
If TARGET-POINT could not be reached, 0 is returned. (As a result
TARGET-POINT should always be placed just before a `?\('.)"
  (save-excursion
    (let ((nesting 0))
      (while (> (point) target-point)
        (backward-up-list)
        (incf nesting))
      (if (= (point) target-point)
          nesting
          0))))

(defun slime-make-form-spec-from-string (string &optional strip-operator-p)
  "If STRIP-OPERATOR-P is T and STRING is the string
representation of a form, the string representation of this form
is stripped from the form. This can be important to avoid mutual
recursion between this function, `slime-enclosing-form-specs' and
`slime-parse-extended-operator-name'.

Examples:

  \"(foo (bar 1 (baz :quux)) 'toto)\" 

      => (\"foo\" (\"bar\" \"1\" (\"baz\" \":quux\")) \"'toto\")
"
  (cond ((slime-length= string 0) "")                    ; ""
	((equal string "()") '())                        ; "()"
	((eql (char-syntax (aref string 0)) ?\') string) ; "'(foo)", "#(foo)" &c
	((not (eql (aref string 0) ?\()) string)         ; "foo"
	(t                                               ; "(op arg1 arg2 ...)"
	 (with-temp-buffer
	   ;; Do NEVER ever try to activate `lisp-mode' here with
	   ;; `slime-use-autodoc-mode' enabled, as this function is used
	   ;; to compute the current autodoc itself.
	   (erase-buffer)
	   (insert string)
	   (when strip-operator-p ; `(OP arg1 arg2 ...)' ==> `(arg1 arg2 ...)'
	     (goto-char (point-min))
	     (when (string= (thing-at-point 'char) "(")
	       (ignore-errors (forward-char 1)
			      (forward-sexp)
			      (slime-forward-blanks))
	       (delete-region (point-min) (point))
	       (insert "(")))
	   (goto-char (1- (point-max))) ; `(OP arg1 ... argN|)'
	   (assert (eql (char-after) ?\)))
	   (multiple-value-bind (forms indices points)
	       (slime-enclosing-form-specs 1)
	     (if (null forms)
		 string
                (let ((n (first (last indices))))
		  (goto-char (1+ (point-min))) ; `(|OP arg1 ... argN)'
		  (mapcar #'(lambda (s)
			      (assert (not (equal s string))) ; trap against
			      (slime-make-form-spec-from-string s)) ;  endless recursion.
			  (slime-parse-sexp-at-point (1+ n) t)))))))))


(defun slime-enclosing-form-specs (&optional max-levels)
  "Return the list of ``raw form specs'' of all the forms 
containing point from right to left.

As a secondary value, return a list of indices: Each index tells
for each corresponding form spec in what argument position the
user's point is.

As tertiary value, return the positions of the operators that are
contained in the returned form specs. 

When MAX-LEVELS is non-nil, go up at most this many levels of
parens.

\(See SWANK::PARSE-FORM-SPEC for more information about what
exactly constitutes a ``raw form specs'')

Examples:

  A return value like the following

    (values  ((\"quux\") (\"bar\") (\"foo\")) (3 2 1) (p1 p2 p3))

  can be interpreted as follows:

    The user point is located in the 3rd argument position of a
    form with the operator name \"quux\" (which starts at P1.)
   
    This form is located in the 2nd argument position of a form
    with the operator name \"bar\" (which starts at P2.)

    This form again is in the 1st argument position of a form
    with the operator name \"foo\" (which itself begins at P3.)

  For instance, the corresponding buffer content could have looked
  like `(foo (bar arg1 (quux 1 2 |' where `|' denotes point.
"
  (let ((level 1)
        (parse-sexp-lookup-properties nil)
        (initial-point (point))
        (result '()) (arg-indices '()) (points '())) 
    ;; The expensive lookup of syntax-class text properties is only
    ;; used for interactive balancing of #<...> in presentations; we
    ;; do not need them in navigating through the nested lists.
    ;; This speeds up this function significantly.
    (ignore-errors
      (save-excursion
        ;; Make sure we get the whole thing at point.
        (if (not (slime-inside-string-p))
            (slime-end-of-symbol)
          (slime-beginning-of-string)
          (forward-sexp))
        (save-restriction
          ;; Don't parse more than 20000 characters before point, so we don't spend
          ;; too much time.
          (narrow-to-region (max (point-min) (- (point) 20000)) (point-max))
          (narrow-to-region (save-excursion (beginning-of-defun) (point))
                            (min (1+ (point)) (point-max)))
          (while (or (not max-levels)
                     (<= level max-levels))
            (let ((arg-index 0))
              ;; Move to the beginning of the current sexp if not already there.
              (if (or (and (char-after)
                           (member (char-syntax (char-after)) '(?\( ?')))
                      (member (char-syntax (char-before)) '(?\  ?>)))
                  (incf arg-index))
              (ignore-errors (backward-sexp 1))
              (while (and (< arg-index 64)
                          (ignore-errors (backward-sexp 1) 
                                         (> (point) (point-min))))
                (incf arg-index))
              (backward-up-list 1)
              (when (member (char-syntax (char-after)) '(?\( ?')) 
                (incf level)
                (forward-char 1)
                (let ((name (slime-symbol-at-point)))
                  (cond
                    (name
                     (save-restriction
                       (widen) ; to allow looking-ahead/back in extended parsing.
                       (multiple-value-bind (new-result new-indices new-points)
                           (slime-parse-extended-operator-name 
                            initial-point
                            (cons `(,name) result) ; minimal form spec
                            (cons arg-index arg-indices)
                            (cons (point) points))
                         (setq result new-result)
                         (setq arg-indices new-indices)
                         (setq points new-points))))
                    (t
                     (push nil result)
                     (push arg-index arg-indices)
                     (push (point) points))))
                (backward-up-list 1)))))))
    (values 
     (nreverse result)
     (nreverse arg-indices)
     (nreverse points))))


(defun slime-ensure-list (thing)
  (if (listp thing) thing (list thing)))

(defun slime-inside-string-p ()
  (nth 3 (slime-current-parser-state)))

(defun slime-beginning-of-string ()
  (let* ((parser-state (slime-current-parser-state))
	 (inside-string-p  (nth 3 parser-state))
	 (string-start-pos (nth 8 parser-state)))
    (if inside-string-p
        (goto-char string-start-pos)
        (error "We're not within a string"))))


;;;; Test cases

(defun slime-check-enclosing-form-specs (wished-form-specs)
  (slime-test-expect 
   (format "Enclosing form specs correct in `%s' (at %d)" (buffer-string) (point))
   wished-form-specs
   (first (slime-enclosing-form-specs))))

(def-slime-test enclosing-form-specs.1
    (buffer-sexpr wished-form-specs)
    "Check that we correctly determine enclosing forms."
    '(("(defun *HERE*"                  (("defun")))
      ("(defun foo *HERE*"              (("defun")))
      ("(defun foo (x y) *HERE*"        (("defun")))
      ("(defmethod *HERE*"              (("defmethod")))
      ("(defmethod foo *HERE*"          (("defmethod" "foo")))
      ("(cerror foo *HERE*"             (("cerror" "foo")))
      ("(cerror foo bar *HERE*"         (("cerror" "foo" "bar")))
      ("(make-instance foo *HERE*"      (("make-instance" "foo")))
      ("(apply 'foo *HERE*"             (("apply" "'foo")))
      ("(apply #'foo *HERE*"            (("apply" "#'foo")))
      ("(declare *HERE*"                (("declare")))
      ("(declare (optimize *HERE*"      ((:declaration ("optimize")) ("declare")))
      ("(declare (string *HERE*"        ((:declaration ("string")) ("declare")))
      ("(declare ((vector *HERE*"       ((:type-specifier ("vector"))))
      ("(declare ((vector bit *HERE*"   ((:type-specifier ("vector" "bit"))))
      ("(proclaim '(optimize *HERE*"    ((:declaration ("optimize")) ("proclaim")))
      ("(the (string *HERE*"            ((:type-specifier ("string"))))
      ("(check-type foo (string *HERE*" ((:type-specifier ("string"))))
      ("(typep foo '(string *HERE*"     ((:type-specifier ("string")))))
  (slime-check-top-level)
  (with-temp-buffer
    (lisp-mode)
    (insert buffer-sexpr)
    (search-backward "*HERE*")
    (delete-region (match-beginning 0) (match-end 0))
    (slime-check-enclosing-form-specs wished-form-specs)
    (insert ")") (backward-char)
    (slime-check-enclosing-form-specs wished-form-specs)      
    ))



(provide 'slime-parse)

