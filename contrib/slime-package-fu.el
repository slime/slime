;;; slime-package-fu.el --- Exporting/Unexporting symbols at point.
;;
;; Author:  Tobias C. Rittweiler <tcr@freebits.de>
;;
;; License: GNU GPL (same license as Emacs)
;;

(defvar slime-package-file-candidates
  (mapcar #'file-name-nondirectory
	  '("package.lisp" "packages.lisp" "pkgdcl.lisp" "defpackage.lisp")))

(defvar slime-export-symbol-representation-function
  #'(lambda (n) (format "#:%s" n)))

(defvar slime-defpackage-regexp
  "^(\\(cl:\\|common-lisp:\\)?defpackage\\>[ \t']*")


(defun slime-find-package-definition-rpc (package)
  (slime-eval `(swank:find-definition-for-thing (swank::guess-package ,package))))

(defun slime-find-package-definition-regexp (package)
  (save-excursion
    (save-match-data
      (goto-char (point-min))
      (block nil
	(while (re-search-forward slime-defpackage-regexp nil t)
	  (when (slime-package-equal package (slime-sexp-at-point))
            (backward-sexp)
	    (return (make-slime-file-location (buffer-file-name)
                                              (1- (point))))))))))

(defun slime-package-equal (designator1 designator2)
  ;; First try to be lucky and compare the strings themselves (for the
  ;; case when one of the designated packages isn't loaded in the
  ;; image.) Then try to do it properly using the inferior Lisp which
  ;; will also resolve nicknames for us &c.
  (or (equalp (slime-cl-symbol-name designator1)
	      (slime-cl-symbol-name designator2))
      (slime-eval `(swank:package= ,designator1 ,designator2))))

(defun slime-export-symbol (symbol package)
  "Unexport `symbol' from `package' in the Lisp image."
  (slime-eval `(swank:export-symbol-for-emacs ,symbol ,package)))

(defun slime-unexport-symbol (symbol package)
  "Export `symbol' from `package' in the Lisp image."
  (slime-eval `(swank:unexport-symbol-for-emacs ,symbol ,package)))


(defun slime-find-possible-package-file (buffer-file-name)
  (flet ((file-name-subdirectory (dirname)
	   (expand-file-name
	    (concat (file-name-as-directory (slime-to-lisp-filename dirname))
		    (file-name-as-directory ".."))))
	 (try (dirname)
	   (dolist (package-file-name slime-package-file-candidates)
	     (let ((f (slime-to-lisp-filename (concat dirname package-file-name))))
	       (when (file-readable-p f)
		 (return f))))))
    (when buffer-file-name
      (let ((buffer-cwd (file-name-directory buffer-file-name)))
	(or (try buffer-cwd)
	    (try (file-name-subdirectory buffer-cwd))
	    (try (file-name-subdirectory (file-name-subdirectory buffer-cwd))))))))

(defun slime-goto-package-source-definition (package)
  "Tries to find the DEFPACKAGE form of `package'. If found,
places the cursor at the start of the DEFPACKAGE form."
  (flet ((try (location)
	   (when (slime-location-p location)
	     (slime-goto-source-location location)
	     t)))
    (or (try (slime-find-package-definition-rpc package))
	(try (slime-find-package-definition-regexp package))
	(try (when-let (package-file (slime-find-possible-package-file (buffer-file-name)))
	       (with-current-buffer (find-file-noselect package-file t)
		 (slime-find-package-definition-regexp package))))
	(error "Couldn't find source definition of package: %s" package))))

(defun slime-at-expression-p (pattern)
  (when (ignore-errors
          ;; at a list?
          (= (point) (progn (down-list 1)
                            (backward-up-list 1)
                            (point))))
    (save-excursion
      (down-list 1)
      (slime-in-expression-p pattern))))

(defun slime-goto-next-export-clause ()
  ;; Assumes we're inside the beginning of a DEFPACKAGE form.
  (let ((point))
    (save-excursion
      (block nil
	(while (ignore-errors (slime-forward-sexp) t)
          (skip-chars-forward " \n\t")
	  (when (slime-at-expression-p '(:export *))
	    (setq point (point))
	    (return)))))
    (if point
	(goto-char point)
	(error "No next (:export ...) clause found"))))

(defun slime-search-exports-in-defpackage (symbol-name)
  "Look if `symbol-name' is mentioned in one of the :EXPORT clauses."
  ;; Assumes we're inside the beginning of a DEFPACKAGE form.
  (flet ((target-symbol-p (symbol)
           (string-match-p (format "^\\(\\(#:\\)\\|:\\)?%s$"
                                   (regexp-quote symbol-name))
                           symbol)))
    (save-excursion
      (block nil
        (while (ignore-errors (slime-goto-next-export-clause) t)
          (let ((clause-end (save-excursion (forward-sexp) (point))))
            (when (and (search-forward symbol-name clause-end t)
                       (target-symbol-p (slime-symbol-at-point)))
              (return (point)))))))))

(defun slime-frob-defpackage-form (current-package do-what symbol)
  "Adds/removes `symbol' from the DEFPACKAGE form of `current-package'
depending on the value of `do-what' which can either be `:export',
or `:unexport'.

Returns t if the symbol was added/removed. Nil if the symbol was
already exported/unexported."
  (let ((symbol-name (slime-cl-symbol-name symbol)))
    (save-excursion
      (slime-goto-package-source-definition current-package)
      (down-list 1)			; enter DEFPACKAGE form
      (forward-sexp)			; skip DEFPACKAGE symbol
      (forward-sexp)			; skip package name
      (let ((already-exported-p (slime-search-exports-in-defpackage symbol-name)))
	(ecase do-what
	  (:export
	   (if already-exported-p
	       nil
	       (prog1 t (slime-insert-export symbol-name))))
	  (:unexport
	   (if already-exported-p
	       (prog1 t (slime-remove-export symbol-name))
	       nil)))))))


(defun slime-insert-export (symbol-name)
  ;; Assumes we're inside the beginning of a DEFPACKAGE form.
  (flet ((goto-last-export-clause ()
	   (let (point)
	     (save-excursion
	       (while (ignore-errors (slime-goto-next-export-clause) t)
		 (setq point (point))))
	     (when point (goto-char point))
	     point)))
    (let ((defpackage-point (point))
	  (symbol-name (funcall slime-export-symbol-representation-function
				symbol-name)))
      (cond ((goto-last-export-clause)
	     (down-list) (slime-end-of-list)
	     (unless (looking-back "^\\s-*")
	       (newline-and-indent))
	     (insert symbol-name))
	    (t
	     (slime-end-of-list)
	     (newline-and-indent)
	     (insert (format "(:export %s)" symbol-name)))))))

(defun slime-remove-export (symbol-name)
  ;; Assumes we're inside the beginning of a DEFPACKAGE form.
  (let ((point))
    (while (setq point (slime-search-exports-in-defpackage symbol-name))
      (save-excursion
	(goto-char point)
	(backward-sexp)
	(delete-region (point) point)
	(beginning-of-line)
	(when (looking-at "^\\s-*$")
	  (join-line))))))


(defun slime-export-symbol-at-point ()
  "Add the symbol at point to the defpackage source definition
belonging to the current buffer-package. With prefix-arg, remove
the symbol again. Additionally performs an EXPORT/UNEXPORT of the
symbol in the Lisp image if possible."
  (interactive)
  (let ((package (slime-current-package))
	(symbol (slime-symbol-at-point)))
    (unless symbol (error "No symbol at point."))
    (cond (current-prefix-arg
	   (if (slime-frob-defpackage-form package :unexport symbol)
	       (message "Symbol `%s' no longer exported form `%s'" symbol package)
	       (message "Symbol `%s' is not exported from `%s'" symbol package))
	   (slime-unexport-symbol symbol package))
	  (t
	   (if (slime-frob-defpackage-form package :export symbol)
	       (message "Symbol `%s' now exported from `%s'" symbol package)
	       (message "Symbol `%s' already exported from `%s'" symbol package))
	   (slime-export-symbol symbol package)))))


(defvar slime-package-fu-init-undo-stack nil)

(defun slime-package-fu-init ()
  (slime-require :swank-package-fu)
  (push `(progn (define-key slime-mode-map "\C-cx"
		  ',(lookup-key slime-mode-map "\C-cx")))
	slime-package-fu-init-undo-stack)
  (define-key slime-mode-map "\C-cx"  'slime-export-symbol-at-point))

(defun slime-package-fu-unload ()
  (while slime-c-p-c-init-undo-stack
    (eval (pop slime-c-p-c-init-undo-stack))))

(provide 'slime-package-fu)
