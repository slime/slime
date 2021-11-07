(define-slime-contrib slime-named-readtable
  "Support for named readtables. Adapted from slime package code"
  (:authors "Alan Ruttenberg/search-replace")
  (:license "GPL")
  (:on-load)
  (:on-unload )
  (:swank-dependencies))

(defvar slime-buffer-readtable nil)

(defun slime-current-readtable ()
  "Return the Common Lisp readtable in the current context.
If `slime-buffer-readtable' has a value then return that, otherwise
search for and read an `in-readtable' form."
  (or slime-buffer-readtable
      (save-restriction
        (widen)
        (slime-find-buffer-readtable))))

(defvar slime-find-buffer-readtable-function 'slime-search-buffer-readtable
  "*Function to use for `slime-find-buffer-readtable'.
The result should be the readtable-name (a string)
or nil if nothing suitable can be found.")

(defun slime-find-buffer-readtable ()
  "Figure out which Lisp readtable the current buffer is associated with."
  (funcall slime-find-buffer-readtable-function))

(defun slime-search-buffer-readtable ()
  (let ((case-fold-search t)
        (regexp (concat "^(\\(cl:\\|common-lisp:\\)?in-readtable\\>[ \t']*"
                        "\\([^)]+\\)[ \t]*)")))
    (save-excursion
      (when (or (re-search-backward regexp nil t)
                (re-search-forward regexp nil t))
        (match-string-no-properties 2)))))


(provide 'slime-named-readtable)
