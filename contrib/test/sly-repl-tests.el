(require 'sly-repl)
(require 'sly-tests)
(require 'cl-lib)

(defmacro sly-repl-test-markers (expected-string-spec &rest marker-specs)
  "For (MARKER SIG FORM) in MARKER-SPECS, produce suitable `should' assertions.
The assertions compare values in symbols `expected-MARKER' and
`observed-MARKER'. The former is obtained by searching EXPECTED-STRING-SPEC
for the string sig SIG, the latter by evaling FORM in the test buffer."
  (declare (indent 1))
  (cl-loop
   for (marker signature observer-form) in marker-specs
   for expected-sym = (make-symbol (format "expected-%s" marker))
   for observed-sym = (make-symbol (format "observed-%s" marker))

   collect `(,expected-sym
             (progn (goto-char (point-min))
                    (when (search-forward ,signature nil t)
                      (replace-match "")
                      (point-marker))))
   into expected-bindings
   collect `(,observed-sym ,observer-form)
   into observed-bindings
   collect `(when (and ,observed-sym (not ,expected-sym))
              (ert-fail
               (format "Didn't expect to observe %s, but did and its %s"
                       ',marker ,observed-sym)))
   into assertions
   collect `(when (and (not ,observed-sym) ,expected-sym)
              (ert-fail
               (format "Expected %s to be %s, bit didn't observe anything"
                       ',marker ,expected-sym)))
   into assertions
   collect `(when (and ,observed-sym ,expected-sym)
              (should (= ,observed-sym ,expected-sym)))
   into assertions
   finally
   (return
    `(progn
       (let (,@observed-bindings
             (observed-string (buffer-substring-no-properties (point-min)
                                                              (point-max))))
         (with-current-buffer (get-buffer-create "*sly-repl test buffer*")
           (erase-buffer)
           (insert ,expected-string-spec)
           (let (,@expected-bindings)
             (should
              (equal observed-string (buffer-string)))
             ,@assertions)))))))

(defun sly-check-buffer-contents (_msg expected-string-spec)
  (sly-repl-test-markers expected-string-spec
    (point             "*" (point))
    (output-start      "{" (next-single-property-change
                            (point-min) 'sly-repl-output))
    (output-end        "}" (previous-single-property-change
                            (point-max) 'sly-repl-output))
    (input-start       "[" sly-repl-input-start-mark)
    (point-max         "]" (point-max))
    (next-input-start  "^" nil)))

(def-sly-test package-updating
    (package-name nicknames)
    "Test if sly-lisp-package is updated."
    '(("COMMON-LISP" ("CL"))
      ("KEYWORD" ("" "KEYWORD" "||"))
      ("COMMON-LISP-USER" ("CL-USER")))
  (with-current-buffer (sly-output-buffer)
    (let ((p (sly-eval
              `(swank:listener-eval
                ,(format
                  "(cl:setq cl:*print-case* :upcase)
                   (cl:setq cl:*package* (cl:find-package %S))
                   (cl:package-name cl:*package*)" package-name))
              (sly-lisp-package))))
      (sly-check ("sly-lisp-package is %S." package-name)
        (equal (sly-lisp-package) package-name))
      (sly-check ("sly-lisp-package-prompt-string is in %S." nicknames)
        (member (sly-lisp-package-prompt-string) nicknames)))))

(defmacro with-canonicalized-sly-repl-buffer (&rest body)
  "Evaluate BODY within a fresh REPL buffer. The REPL prompt is
canonicalized to \"SWANK\"---we do actually switch to that
package, though."
  (declare (debug (&rest form)) (indent 0))
  `(let ((%old-prompt% (sly-lisp-package-prompt-string)))
     (unwind-protect
          (progn (with-current-buffer (sly-output-buffer)
                   (setf (sly-lisp-package-prompt-string) "SWANK"))
                 (kill-buffer (sly-output-buffer))
                 (with-current-buffer (sly-output-buffer)
                   ,@body))
       (setf (sly-lisp-package-prompt-string) %old-prompt%))))

(def-sly-test repl-test
    (input result-contents)
    "Test simple commands in the minibuffer."
    '(("(+ 1 2)" "SWANK> (+ 1 2)
3
SWANK> *[]")
      ("(princ 10)" "SWANK> (princ 10)
{10
}10
SWANK> *[]")
      ("(princ 10)(princ 20)" "SWANK> (princ 10)(princ 20)
{1020
}20
SWANK> *[]")
      ("(dotimes (i 10 77) (princ i) (terpri))"
       "SWANK> (dotimes (i 10 77) (princ i) (terpri))
{0
1
2
3
4
5
6
7
8
9
}77
SWANK> *[]")
      ("(abort)" "SWANK> (abort)
; Evaluation aborted on NIL.
SWANK> *[]")
      ("(progn (princ 10) (force-output) (abort))"
       "SWANK> (progn (princ 10) (force-output) (abort))
{10}; Evaluation aborted on NIL.
SWANK> *[]")
      ("(progn (princ 10) (abort))"
       ;; output can be flushed after aborting
       "SWANK> (progn (princ 10) (abort))
{10}; Evaluation aborted on NIL.
SWANK> *[]")
      ("(if (fresh-line) 1 0)"
       "SWANK> (if (fresh-line) 1 0)
{
}1
SWANK> *[]")
      ("(values 1 2 3)" "SWANK> (values 1 2 3)
1
2
3
SWANK> *[]"))
  (with-canonicalized-sly-repl-buffer
    (insert input)
    (sly-check-buffer-contents "Buffer contains input"
                                 (concat "SWANK> [" input "*]"))
    (call-interactively 'sly-repl-return)
    (sly-sync-to-top-level 5)
    (sly-check-buffer-contents "Buffer contains result" result-contents)))

(def-sly-test repl-test-2
    (input result-contents)
    "Test some more simple situations dealing with print-width and stuff"
    '(("(with-standard-io-syntax
         (write (make-list 15 :initial-element '(1 . 2)) :pretty t) 0)"
       "SWANK> (with-standard-io-syntax
         (write (make-list 15 :initial-element '(1 . 2)) :pretty t) 0)
{((1 . 2) (1 . 2) (1 . 2) (1 . 2) (1 . 2) (1 . 2) (1 . 2) (1 . 2) (1 . 2)
 (1 . 2) (1 . 2) (1 . 2) (1 . 2) (1 . 2) (1 . 2))
}0
SWANK> *[]")
      ;; Two times to test the effect of FRESH-LINE.
      ("(with-standard-io-syntax
         (write (make-list 15 :initial-element '(1 . 2)) :pretty t) 0)"
       "SWANK> (with-standard-io-syntax
         (write (make-list 15 :initial-element '(1 . 2)) :pretty t) 0)
{((1 . 2) (1 . 2) (1 . 2) (1 . 2) (1 . 2) (1 . 2) (1 . 2) (1 . 2) (1 . 2)
 (1 . 2) (1 . 2) (1 . 2) (1 . 2) (1 . 2) (1 . 2))
}0
SWANK> *[]"))
  (sly-skip-test "Repl test is unstable without the sly-presentations contrib.")
  (sly-test-repl-test input result-contents))

(def-sly-test repl-return
    (before after result-contents)
    "Test if sly-repl-return sends the correct protion to Lisp even
if point is not at the end of the line."
    '(("(+ 1 2)" "" "SWANK> (+ 1 2)
3
SWANK> ")
("(+ 1 " "2)" "SWANK> (+ 1 2)
3
SWANK> ")

("(+ 1\n" "2)" "SWANK> (+ 1
2)
3
SWANK> "))
  (with-canonicalized-sly-repl-buffer
    (insert before)
    (save-excursion (insert after))
    (sly-test-expect "Buffer contains input"
                       (concat "SWANK> " before after)
                       (buffer-string))
    (call-interactively 'sly-repl-return)
    (sly-sync-to-top-level 5)
    (sly-test-expect "Buffer contains result"
                       result-contents (buffer-string))))

(def-sly-test repl-read
    (prompt input result-contents)
    "Test simple commands in the minibuffer."
    '(("(read-line)" "foo" "SWANK> (values (read-line))
foo
\"foo\"
SWANK> ")
      ("(read-char)" "1" "SWANK> (values (read-char))
1
#\\1
SWANK> ")
      ("(read)" "(+ 2 3
4)" "SWANK> (values (read))
\(+ 2 3
4)
\(+ 2 3 4)
SWANK> "))
  (with-canonicalized-sly-repl-buffer
    (insert (format "(values %s)" prompt))
    (call-interactively 'sly-repl-return)
    (sly-wait-condition "reading" #'sly-reading-p 5)
    (insert input)
    (call-interactively 'sly-repl-return)
    (sly-sync-to-top-level 5)
    (sly-test-expect "Buffer contains result"
                       result-contents (buffer-string))))

(def-sly-test repl-read-lines
    (command inputs final-contents)
    "Test reading multiple lines from the repl."
    '(("(list (read-line) (read-line) (read-line))"
       ("a" "b" "c")
       "SWANK> (list (read-line) (read-line) (read-line))
a
b
c
\(\"a\" \"b\" \"c\")
SWANK> "))
  (with-canonicalized-sly-repl-buffer
    (insert command)
    (call-interactively 'sly-repl-return)
    (dolist (input inputs)
      (sly-wait-condition "reading" #'sly-reading-p 5)
      (insert input)
      (call-interactively 'sly-repl-return))
    (sly-sync-to-top-level 5)
    (sly-test-expect "Buffer contains result"
                       final-contents
                       (buffer-string)
                       #'equal)))

(def-sly-test repl-type-ahead
    (command input final-contents)
    "Ensure that user input is preserved correctly.
In particular, input inserted while waiting for a result."
    '(("(sleep 0.1)" "foo*" "SWANK> (sleep 0.1)
NIL
SWANK> [foo*]")
      ("(sleep 0.1)" "*foo" "SWANK> (sleep 0.1)
NIL
SWANK> [*foo]")
      ("(progn (sleep 0.1) (abort))" "*foo" "SWANK> (progn (sleep 0.1) (abort))
; Evaluation aborted on NIL.
SWANK> [*foo]"))
  (with-canonicalized-sly-repl-buffer
    (insert command)
    (call-interactively 'sly-repl-return)
    (save-excursion (insert (cl-delete ?* input)))
    (forward-char (cl-position ?* input))
    (sly-sync-to-top-level 5)
    (sly-check-buffer-contents "Buffer contains result" final-contents)))


(def-sly-test interrupt-in-blocking-read
    ()
    "Let's see what happens if we interrupt a blocking read operation."
    '(())
  (sly-skip-test "TODO: skip for now, but analyse this failure!")
  (sly-check-top-level)
  (with-canonicalized-sly-repl-buffer
    (insert "(read-char)")
    (call-interactively 'sly-repl-return)
    (sly-wait-condition "reading" #'sly-reading-p 5)
    (sly-interrupt)
    (sly-wait-condition "Debugger visible"
                          (lambda ()
                            (and (sly-sldb-level= 1)
                                 (get-buffer-window
                                  (sldb-get-default-buffer))))
                          5)
    (with-current-buffer (sldb-get-default-buffer)
      (sldb-continue))
    (sly-wait-condition "reading" #'sly-reading-p 5)
    (with-current-buffer (sly-output-buffer)
      (insert "X")
      (call-interactively 'sly-repl-return)
      (sly-sync-to-top-level 5)
      (sly-test-expect "Buffer contains result"
                         "SWANK> (read-char)
X
#\\X
SWANK> " (buffer-string)))))

(def-sly-test move-around-and-be-nasty
    ()
    "Test moving around in repl, and watching attempts to destroy prompt fail"
    '(())
  (sly-skip-test "TODO: Test causes instability for other tests.")
  (sly-check-top-level)
  (with-canonicalized-sly-repl-buffer
    (let ((start (point)))
      (insert "foo")
      (beginning-of-line)
      (should (equal (buffer-substring-no-properties
                      (point-min)
                      (point-max)) "SWANK> foo"))
      (should (equal (point) start))
      (unwind-protect
          (progn
            (let ((inhibit-field-text-motion t))
              (goto-char (line-beginning-position)))
            (should-error (delete-char 1)))
        (goto-char (line-end-position))))))

(def-sly-test mixed-output-and-results
    (prompt eval-input result-contents)
    "Test that output goes to the correct places."
    '(("(princ 123)" (cl:loop repeat 2 do (cl:princ 456)) "SWANK> (princ 123)
123
123
456456
SWANK> "))
  (with-canonicalized-sly-repl-buffer
    (insert prompt)
    (call-interactively 'sly-repl-return)
    (sly-sync-to-top-level 5)
    (sly-eval eval-input)
    (sly-sync-to-top-level 5)
    (sly-test-expect "Buffer contains result"
                       result-contents (buffer-string))))

(provide 'sly-repl-tests)
