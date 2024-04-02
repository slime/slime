(require 'slime)
(require 'slime-c-p-c)
(require 'slime-parse)
(eval-when-compile (require 'subr-x))

(defvar slime-package-fu-init-undo-stack nil)

(define-slime-contrib slime-package-fu
  "Exporting/Unexporting symbols at point."
  (:authors "Tobias C. Rittweiler <tcr@freebits.de>")
  (:license "GPL")
  (:swank-dependencies swank-package-fu)
  (:on-load
   (push `(progn (define-key slime-mode-map "\C-cx"
                   ',(lookup-key slime-mode-map "\C-cx")))
         slime-package-fu-init-undo-stack)
   (define-key slime-mode-map "\C-cx" 'slime-export-symbol-at-point))
  (:on-unload
   (while slime-package-fu-init-undo-stack
     (eval (pop slime-package-fu-init-undo-stack)))))

(defvar slime-package-file-candidates
  (mapcar #'file-name-nondirectory
          '("package.lisp" "packages.lisp" "pkgdcl.lisp"
            "defpackage.lisp")))

(defvar slime-export-symbol-representation-function
  #'(lambda (n) (format "#:%s" n)))

(defvar slime-export-symbol-representation-auto t
  "Determine automatically which style is used for symbols, #: or :
If it's mixed or no symbols are exported so far,
use `slime-export-symbol-representation-function'.")

(defvar slime-export-save-file nil
  "Save the package file after each automatic modification")

(defvar slime-defpackage-regexp
  "^(\\(cl:\\|common-lisp:\\|uiop:\\|uiop/package:\\)?\\(defpackage\\|define-package\\)\\>[ \t']*")

(defun slime-find-package-definition-rpc (package)
  (slime-eval `(swank:find-definition-for-thing
                (swank::guess-package ,package))))

(defun slime-find-package-definition-regexp (package)
  (save-excursion
    (save-match-data
      (goto-char (point-min))
      (cl-block nil
        (while (re-search-forward slime-defpackage-regexp nil t)
          (when (slime-package-equal package (slime-sexp-at-point))
            (backward-sexp)
            (cl-return (make-slime-file-location (buffer-file-name)
                                                 (1- (point))))))))))

(defun slime-package-equal (designator1 designator2)
  ;; First try to be lucky and compare the strings themselves (for the
  ;; case when one of the designated packages isn't loaded in the
  ;; image.) Then try to do it properly using the inferior Lisp which
  ;; will also resolve nicknames for us &c.
  (or (cl-equalp (slime-cl-symbol-name designator1)
                 (slime-cl-symbol-name designator2))
      (slime-eval `(swank:package= ,designator1 ,designator2))))

(defun slime-export-symbol-in-lisp (symbol package)
  "Export `symbol' from `package' in the Lisp image."
  (slime-eval-async `(swank:export-symbol-for-emacs ,symbol ,package)))

(defun slime-unexport-symbol-in-lisp (symbol package)
  "Unexport `symbol' from `package' in the Lisp image."
  (slime-eval-async `(swank:unexport-symbol-for-emacs ,symbol ,package)))

(defun slime-import-symbol-in-lisp (symbol package)
  "Import `symbol' into `package' in the Lisp image."
  (slime-eval-async `(swank:import-symbol-for-emacs ,symbol ,package)))

(defun slime-unintern-symbol-in-lisp (symbol package)
  "Unintern `symbol' from `package' in the Lisp image."
  ;; There’s already the function `slime-unintern-symbol’, but it
  ;; produces messages which I do not want and can’t supress.
  (slime-eval-async `(swank:unintern-symbol ,symbol ,package)))

(defun slime-find-possible-package-file (buffer-file-name)
  (cl-labels ((file-name-subdirectory (dirname)
                                      (expand-file-name
                                       (concat (file-name-as-directory (slime-to-lisp-filename dirname))
                                               (file-name-as-directory ".."))))
              (try (dirname)
                   (cl-dolist (package-file-name slime-package-file-candidates)
                     (let ((f (slime-to-lisp-filename
                               (concat dirname package-file-name))))
                       (when (file-readable-p f)
                         (cl-return f))))))
    (when buffer-file-name
      (let ((buffer-cwd (file-name-directory buffer-file-name)))
        (or (try buffer-cwd)
            (try (file-name-subdirectory buffer-cwd))
            (try (file-name-subdirectory
                  (file-name-subdirectory buffer-cwd))))))))

(defun slime-goto-package-definition (package)
  "Try to find the DEFPACKAGE form of ‘package’ and go to it.

Place the cursor at the start of the DEFPACKAGE form."
  (cl-labels ((try (location)
                   (when (slime-location-p location)
                     (slime-goto-source-location location)
                     t)))
    (or (try (slime-find-package-definition-rpc package))
        (try (slime-find-package-definition-regexp package))
        (try (let ((package-file (slime-find-possible-package-file
                                  (buffer-file-name))))
               (when package-file
                 (with-current-buffer (find-file-noselect package-file t)
                   (slime-find-package-definition-regexp package)))))
        (error "Couldn't find source definition of package: %s" package))))

;;;; functions for manipulating symbol representation

(defun slime-determine-symbol-style (symbols)
  (cl-flet ((most (pred)
                  (cl-plusp (cl-reduce (lambda (acc x)
                                         (+ acc (if (funcall pred x) 1 -1)))
                                       symbols
                                       :initial-value 0))))
    (cond ((null symbols)
           slime-export-symbol-representation-function)
          ((most (lambda (x)
                   (string-match "^:" x)))
           (lambda (n) (format ":%s" n)))
          ((most (lambda (x)
                   (string-match "^#:" x)))
           (lambda (n) (format "#:%s" n)))
          ((most (lambda (x)
                   (string-match "^[-%[:alnum:]]" x)))
           (lambda (n) (format "%s" n)))
          ((most (lambda (x)
                   (string-prefix-p "\"" x)))
           (lambda (n) (prin1-to-string (upcase (substring-no-properties n)))))
          (t
           slime-export-symbol-representation-function))))

(defun slime-normalize-symbol-name (symbol-name)
  "Strip all flair from ‘symbol-name’ and downcase it."
  (if (string-prefix-p "\"" symbol-name)
      (read symbol-name)
    (downcase (slime-cl-symbol-name symbol-name))))


;;;; functions for traversing defpackage forms

(defmacro slime-find-location-to-go-to (goto-next &rest body)
  "Move around using ‘goto-next’ until ‘body’ calls ‘go-here’."
  (declare (indent 1))
  (let ((loc (gensym)))
    `(if-let (,loc (save-excursion
                     (cl-block nil
                       (cl-macrolet ((go-here () `(cl-return (point))))
                         (while ,goto-next
                           ,@body))
                       nil)))
         (goto-char ,loc))))

(defun slime-at-expression-p (pattern)
  (when (ignore-errors
          ;; at a list?
          (= (point) (progn (down-list 1)
                            (backward-up-list 1)
                            (point))))
    (save-excursion
      (down-list 1)
      (slime-in-expression-p pattern))))

(defun slime-goto-next-defpackage-clause (&optional clause-type)
  "Go to the next defpackage clause.

If ‘clause-type’ is non-nil, go to the next clause of that type
instead."
  ;; Assumes we're at a clause.
  (slime-find-location-to-go-to
      (ignore-errors (slime-forward-sexp 2)
                     (backward-sexp) t)
    (when (or (not clause-type)
              (slime-at-expression-p (list clause-type '*)))
      (go-here))))

(defun slime-goto-next-import-clause (package)
  "Go to the next import clause for ‘package’."
  ;; Assume we're at a clause.
  (slime-find-location-to-go-to
      (slime-goto-next-defpackage-clause :import-from)
    (let ((pack (save-excursion
                  (down-list)
                  (slime-forward-sexp 2)
                  (slime-sexp-at-point))))
      (when (slime-package-equal package pack)
        (go-here)))))

(defun slime-goto-next-export-clause ()
  (slime-goto-next-defpackage-clause :export))

(defun slime-goto-clause-symbol-list ()
  ;; Assume we're at a clause with a list of symbols.
  (down-list)
  (slime-forward-sexp (if (slime-in-expression-p '(:import-from *))
                          2
                        1)))

(defun slime-point-at-symbol-name-p (symbol-name)
  (cl-equalp symbol-name
             (slime-normalize-symbol-name (slime-sexp-at-point))))

(defun slime-goto-symbol-in-clause (symbol)
  "Go to the end of ‘symbol’ in a defpackage clause."
  ;; Assume we're at a clause with a list of symbols.
  (if-let (loc (save-excursion
                 (slime-goto-clause-symbol-list)
                 (cl-block nil
                   (while (ignore-errors (slime-forward-sexp) t)
                     (when (slime-point-at-symbol-name-p symbol)
                       (cl-return (point))))
                   nil)))
      (goto-char loc)))

(defun slime-symbols-in-clause ()
  "Collect all symbols in a defpackage clause."
  ;; Assumes we're at a clause.
  (save-excursion
    (slime-goto-clause-symbol-list)
    (cl-loop for sexp = (ignore-errors
                          (slime-forward-sexp)
                          (slime-sexp-at-point))
             while sexp
             collect sexp)))

(defun slime-packages-imported-from ()
  "Collect the packages which are imported from."
  ;; Assumes we're inside the beginning of a defpackage form.
  (save-excursion
    (let (packages)
      (while (slime-goto-next-defpackage-clause :import-from)
        (if-let (p (save-excursion
                     (ignore-errors
                       (down-list)
                       (slime-forward-sexp 2)
                       (slime-sexp-at-point))))
            (push p packages)))
      packages)))

(defun slime-symbols-in-clauses (goto-next-clause)
  "Collect symbols inside clauses given by ‘goto-next-clause’.

‘goto-next-clause’ can be a function or a clause type."
  ;; Assumes we're inside the beginning of a defpackage form.
  (save-excursion
    (cl-loop while (if (functionp goto-next-clause)
                       (funcall goto-next-clause)
                     (slime-goto-next-defpackage-clause goto-next-clause))
             append (slime-symbols-in-clause))))

(defun slime-symbol-member-p (symbol symbols)
  "Is ‘symbol’ one of ‘symbols’?"
  (let ((normalized-name (slime-normalize-symbol-name symbol)))
    (cl-some (lambda (s)
               (cl-equalp normalized-name
                          (slime-normalize-symbol-name s)))
             symbols)))


;;;; functions for removing from defpackage forms

(defun slime-delete-sexp-at-point ()
  (let ((bounds (slime-bounds-of-sexp-at-point)))
    (if bounds
        (delete-region (car bounds) (cdr bounds))
      (error "No sexp at point."))))

(defun slime-remove-symbol-from-clauses (symbol goto-next-clause)
  "Remove ‘symbol’ from all clauses given by ‘goto-next-clause’."
  ;; Assumes we're inside the beginning of a defpackage form.
  (save-excursion
    (while (if (functionp goto-next-clause)
               (funcall goto-next-clause)
             (slime-goto-next-defpackage-clause goto-next-clause))
      (save-excursion
        (slime-goto-clause-symbol-list)
        (while (ignore-errors (slime-forward-sexp) t)
          (when (slime-point-at-symbol-name-p symbol)
            (slime-delete-sexp-at-point)
            (slime-join-line-if-empty)
            (backward-sexp)))))))

;;;; functions for cleaning up defpackage forms

(defun slime-join-line-if-empty ()
  (save-excursion
    (beginning-of-line)
    (when (looking-at "^\\(\\s-\\|)\\)*$")
      (join-line)
      (delete-trailing-whitespace (point) (line-end-position)))))

(defun slime-empty-defpackage-clause-p ()
  ;; Assume we're at a clause.
  (save-excursion
    (not (ignore-errors
           (slime-goto-clause-symbol-list)
           (slime-forward-sexp)
           t))))

(defun slime-remove-empty-defpackage-clauses (&optional clause-type)
  ;; Assume we're inside the beginning of a defpackage form.
  (save-excursion
    (while (slime-goto-next-defpackage-clause clause-type)
      (when (slime-empty-defpackage-clause-p)
        (slime-delete-sexp-at-point)
        (slime-join-line-if-empty)
        (backward-sexp)))))


;;;; functions for adding to defpackage forms

(defun slime-format-symbol-to-match (symbol symbols)
  "Make ‘symbol’ match ‘symbols’."
  (funcall (if slime-export-symbol-representation-auto
               (slime-determine-symbol-style symbols)
             slime-export-symbol-representation-function)
           symbol))

(defun slime-ensure-newline-and-indent ()
  (unless (looking-back "^\\s-*")
    (newline-and-indent)))

(defun slime-add-symbol-to-clause (symbol &optional symbols-to-match)
  "Insert ‘symbol’ into the symbol list of a clause."
  ;; Assume we're at the beginning of a clause.
  (forward-list)
  (down-list -1)
  (slime-ensure-newline-and-indent)
  (insert (slime-format-symbol-to-match symbol
                                        symbols-to-match)))

(defun slime-add-defpackage-clause (clause-string)
  "Add a clause to a defpackage form and go to it."
  ;; Assume we're inside the beginning of a defpackage form.
  (slime-end-of-list)
  (slime-ensure-newline-and-indent)
  (save-excursion (insert clause-string)))

(defun slime-add-import-clause (from-package)
  "Add an import clause to a defpackage form and go to it."
  ;; Assume we're inside the beginning of a defpackage form.
  (let ((other-packages (slime-packages-imported-from)))
    (slime-add-defpackage-clause
     (format "(:import-from %s)"
             (slime-format-symbol-to-match from-package
                                           other-packages)))))

(defun slime-ensure-import (from-package symbol)
  "Make a defpackage form import ‘symbol’ from ‘from-package’."
  ;; Assume we're inside the beginning of a defpackage form.
  (save-excursion
    (let ((all-imports (slime-symbols-in-clauses :import-from))
          (imports-from-package (slime-symbols-in-clauses
                                 (lambda ()
                                   (slime-goto-next-import-clause from-package)))))
      (unless (slime-symbol-member-p symbol imports-from-package)
        (when (slime-symbol-member-p symbol all-imports)
          (slime-remove-symbol-from-clauses symbol :import-from))
        (unless (slime-goto-next-import-clause from-package)
          (slime-add-import-clause from-package))
        (slime-add-symbol-to-clause symbol all-imports)))))

(defun slime-ensure-export (symbol)
  "Make a defpackage form export ‘symbol’."
  ;; Assume we're inside the beginning of a defpackage form
  (save-excursion
    (let ((exported-syms (slime-symbols-in-clauses :export)))
      (unless (slime-symbol-member-p symbol exported-syms)
        (unless (slime-goto-next-export-clause)
          (slime-add-defpackage-clause "(:export)"))
        (slime-add-symbol-to-clause symbol exported-syms)))))


;;;; functions for importing/exporting

(defmacro slime-edit-defpackage-form (package &rest body)
  ;; Puts the point inside the beginnig of the defpackage form.
  (declare (indent 1))
  `(save-excursion
     (slime-goto-package-definition package)
     (down-list)
     (forward-sexp)
     (save-excursion ,@body)
     (slime-remove-empty-defpackage-clauses)
     (when slime-export-save-file
       (save-buffer))))

(defmacro slime-with-normalized-symbols (symbols &rest body)
  (declare (indent 1))
  `(let ,(mapcar (lambda (s)
                   `(,s (slime-normalize-symbol-name ,s)))
                 symbols)
     ,@body))

(defun slime-export-symbol (symbol &optional package)
  "Edit the definition of ‘package’ to export ‘symbol’.
Also attempt to reflect the change in the superior lisp."
  (setq package (or package (slime-current-package)))
  (slime-with-normalized-symbols (symbol package)
    (slime-edit-defpackage-form package
      (slime-ensure-export symbol)
      (ignore-errors (slime-export-symbol-in-lisp symbol package))
      (message "Package ‘%s’ now exports ‘%s’." package symbol))))

(defun slime-unexport-symbol (symbol &optional package)
  "Edit the definition of ‘package’ to not export ‘symbol’.
Also attempt to reflect the change in the superior lisp."
  (setq package (or package (slime-current-package)))
  (slime-with-normalized-symbols (symbol package)
    (slime-edit-defpackage-form package
      (slime-remove-symbol-from-clauses symbol :export)
      (ignore-errors (slime-unexport-symbol-in-lisp symbol package))
      (message "Package ‘%s’ now does not export ‘%s’." package symbol))))

(defun slime-import-symbol (symbol from-package &optional package)
  "Edit the definition of ‘package’ to import ‘symbol’.
Also attempt to reflect the change in the superior lisp. The symbol is
imported from ‘from-package’."
  (setq package (or package (slime-current-package)))
  (slime-with-normalized-symbols (symbol from-package package)
    (slime-edit-defpackage-form package
      (slime-ensure-import from-package symbol)
      (ignore-errors
        (slime-unintern-symbol-in-lisp symbol package)
        (slime-import-symbol-in-lisp (format "%s:%s" from-package symbol)
                                     package))
      (message "Package ‘%s’ now imports ‘%s’ from ‘%s’."
               package symbol from-package))))

(defun slime-unimport-symbol (symbol &optional package)
  "Edit the definition of ‘package’ to not import ‘symbol’.
Also attempt to reflect the change in the superior lisp."
  (setq package (or package (slime-current-package)))
  (slime-with-normalized-symbols (symbol package)
    (slime-edit-defpackage-form package
      (slime-remove-symbol-from-clauses symbol :import-from)
      (ignore-errors (slime-unintern-symbol-in-lisp symbol package))
      (message "Package ‘%s’ now does not import ‘%s’." package symbol))))


;;;; interactive functions

(defun slime-export-symbol-at-point ()
  "Edit the current package’s definition so it exports the symbol at point.
With a prefix arg, edit it so that it doesn’t export the symbol.
Additionally, attempt to perform an export/unexport in the
superior lisp."
  (interactive)
  (let ((symbol (slime-symbol-at-point)))
    (unless symbol (error "No symbol at point"))
    (if current-prefix-arg
        (slime-unexport-symbol symbol)
      (slime-export-symbol symbol))))

(defun slime-import-symbol-at-point ()
  "Edit the current package’s definition so it imports the symbol at point.
With a prefix arg, edit it so that it doesn’t import the symbol.
Unless the symbol at point has a package qualifier (e.g. ‘foo:bar’),
prompt for the name of the package to import from.

Additionally, attempt to perform an import/unintern in the superior
lisp."
  (interactive)
  (let ((symbol (slime-symbol-at-point)))
    (unless symbol (error "No symbol at point"))
    (if current-prefix-arg
        (slime-unimport-symbol symbol)
      (let ((from-package
             (or (slime-cl-symbol-package symbol)
                 (slime-read-from-minibuffer
                  (format "Import ‘%s’ from package named: " symbol)))))
        (slime-import-symbol symbol from-package)))))

(defun slime-export-class (name)
  "Export acessors, constructors, etc. associated with a structure or a class"
  (interactive (list (slime-read-from-minibuffer "Export structure named: "
                                                 (slime-symbol-at-point))))
  (let* ((package (slime-normalize-symbol-name (slime-current-package)))
         (symbols (mapcar #'symbol-name
                          (slime-eval `(swank:export-structure ,name ,package)))))
    (slime-edit-defpackage-form package
      (dolist (symbol symbols)
        (slime-with-normalized-symbols (symbol)
          (slime-ensure-export symbol)
          (ignore-errors (slime-export-symbol-in-lisp symbol package)))))
    (message "Package ‘%s’ now exports symbols related to ‘%s’." package name)))

(defalias 'slime-export-structure 'slime-export-class)

(provide 'slime-package-fu)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
