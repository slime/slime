(require 'sly)
(require 'sly-parse "lib/sly-parse")

(define-sly-contrib sly-package-fu
  "Exporting/Unexporting symbols at point."
  (:authors "Tobias C. Rittweiler <tcr@freebits.de>")
  (:license "GPL")
  (:slynk-dependencies slynk-package-fu)
  (:on-load 
   (define-key sly-mode-map "\C-cx"  'sly-export-symbol-at-point))
  (:on-unload
   ;; FIXME: To properly support unloading, this contrib should be
   ;; made a minor mode with it's own keymap. The minor mode
   ;; activation function should be added to the proper sly-* hooks.
   ;; 
   ))

(defvar sly-package-file-candidates
  (mapcar #'file-name-nondirectory
	  '("package.lisp" "packages.lisp" "pkgdcl.lisp"
            "defpackage.lisp")))

(defvar sly-export-symbol-representation-function
  #'(lambda (n) (format "#:%s" n)))

(defvar sly-export-symbol-representation-auto t
  "Determine automatically which style is used for symbols, #: or :
If it's mixed or no symbols are exported so far,
use `sly-export-symbol-representation-function'.")

(defvar sly-export-save-file nil
  "Save the package file after each automatic modification")

(defvar sly-defpackage-regexp
  "^(\\(cl:\\|common-lisp:\\)?defpackage\\>[ \t']*")

(defun sly-find-package-definition-rpc (package)
  (sly-eval `(slynk:find-definition-for-thing
                (slynk::guess-package ,package))))

(defun sly-find-package-definition-regexp (package)
  (save-excursion
    (save-match-data
      (goto-char (point-min))
      (cl-block nil
	(while (re-search-forward sly-defpackage-regexp nil t)
	  (when (sly-package-equal package (sly-sexp-at-point))
            (backward-sexp)
	    (cl-return (make-sly-file-location (buffer-file-name)
                                                 (1- (point))))))))))

(defun sly-package-equal (designator1 designator2)
  ;; First try to be lucky and compare the strings themselves (for the
  ;; case when one of the designated packages isn't loaded in the
  ;; image.) Then try to do it properly using the inferior Lisp which
  ;; will also resolve nicknames for us &c.
  (or (cl-equalp (sly-cl-symbol-name designator1)
                 (sly-cl-symbol-name designator2))
      (sly-eval `(slynk:package= ,designator1 ,designator2))))

(defun sly-export-symbol (symbol package)
  "Unexport `symbol' from `package' in the Lisp image."
  (sly-eval `(slynk:export-symbol-for-emacs ,symbol ,package)))

(defun sly-unexport-symbol (symbol package)
  "Export `symbol' from `package' in the Lisp image."
  (sly-eval `(slynk:unexport-symbol-for-emacs ,symbol ,package)))


(defun sly-find-possible-package-file (buffer-file-name)
  (cl-labels ((file-name-subdirectory (dirname)
                                      (expand-file-name
                                       (concat (file-name-as-directory (sly-to-lisp-filename dirname))
                                               (file-name-as-directory ".."))))
              (try (dirname)
                   (cl-dolist (package-file-name sly-package-file-candidates)
                     (let ((f (sly-to-lisp-filename
                               (concat dirname package-file-name))))
                       (when (file-readable-p f)
                         (cl-return f))))))
    (when buffer-file-name
      (let ((buffer-cwd (file-name-directory buffer-file-name)))
	(or (try buffer-cwd)
	    (try (file-name-subdirectory buffer-cwd))
	    (try (file-name-subdirectory
                  (file-name-subdirectory buffer-cwd))))))))

(defun sly-goto-package-source-definition (package)
  "Tries to find the DEFPACKAGE form of `package'. If found,
places the cursor at the start of the DEFPACKAGE form."
  (cl-labels ((try (location)
                   (when (sly-location-p location)
                     (sly-move-to-source-location location)
                     t)))
    (or (try (sly-find-package-definition-rpc package))
	(try (sly-find-package-definition-regexp package))
	(try (sly--when-let
                 (package-file (sly-find-possible-package-file
                                (buffer-file-name)))
	       (with-current-buffer (find-file-noselect package-file t)
		 (sly-find-package-definition-regexp package))))
	(sly-error "Couldn't find source definition of package: %s" package))))

(defun sly-at-expression-p (pattern)
  (when (ignore-errors
          ;; at a list?
          (= (point) (progn (down-list 1)
                            (backward-up-list 1)
                            (point))))
    (save-excursion
      (down-list 1)
      (sly-in-expression-p pattern))))

(defun sly-goto-next-export-clause ()
  ;; Assumes we're inside the beginning of a DEFPACKAGE form.
  (let ((point))
    (save-excursion
      (cl-block nil
	(while (ignore-errors (sly-forward-sexp) t)
          (skip-chars-forward " \n\t")
	  (when (sly-at-expression-p '(:export *))
	    (setq point (point))
	    (cl-return)))))
    (if point
	(goto-char point)
      (error "No next (:export ...) clause found"))))

(defun sly-search-exports-in-defpackage (symbol-name)
  "Look if `symbol-name' is mentioned in one of the :EXPORT clauses."
  ;; Assumes we're inside the beginning of a DEFPACKAGE form.
  (cl-labels ((target-symbol-p (symbol)
                               (string-match-p (format "^\\(\\(#:\\)\\|:\\)?%s$"
                                                       (regexp-quote symbol-name))
                                               symbol)))
    (save-excursion
      (cl-block nil
        (while (ignore-errors (sly-goto-next-export-clause) t)
          (let ((clause-end (save-excursion (forward-sexp) (point))))
            (save-excursion
              (while (search-forward symbol-name clause-end t)
                (when (target-symbol-p (sly-symbol-at-point))
                  (cl-return (if (sly-inside-string-p)
                                 ;; Include the following "
                                 (1+ (point))
                               (point))))))))))))

(defun sly-export-symbols ()
  "Return a list of symbols inside :export clause of a defpackage."
  ;; Assumes we're at the beginning of :export
  (cl-labels ((read-sexp ()
                         (ignore-errors
                           (forward-comment (point-max))
                           (buffer-substring-no-properties
                            (point) (progn (forward-sexp) (point))))))
    (save-excursion
      (cl-loop for sexp = (read-sexp) while sexp collect sexp))))

(defun sly-defpackage-exports ()
  "Return a list of symbols inside :export clause of a defpackage."
  ;; Assumes we're inside the beginning of a DEFPACKAGE form.
  (cl-labels ((normalize-name (name)
                              (if (string-prefix-p "\"" name)
                                  (read name)
                                (replace-regexp-in-string "^\\(\\(#:\\)\\|:\\)"
                                                          "" name))))
    (save-excursion
      (mapcar #'normalize-name
              (cl-loop while (ignore-errors (sly-goto-next-export-clause) t)
                       do (down-list) (forward-sexp)
                       append (sly-export-symbols)
                       do (up-list) (backward-sexp))))))

(defun sly-symbol-exported-p (name symbols)
  (cl-member name symbols :test 'equalp))

(defun sly-frob-defpackage-form (current-package do-what symbols)
  "Adds/removes `symbol' from the DEFPACKAGE form of `current-package'
depending on the value of `do-what' which can either be `:export',
or `:unexport'.

Returns t if the symbol was added/removed. Nil if the symbol was
already exported/unexported."
  (save-excursion
    (sly-goto-package-source-definition current-package)
    (down-list 1)			; enter DEFPACKAGE form
    (forward-sexp)			; skip DEFPACKAGE symbol
    ;; Don't or will fail if (:export ...) is immediately following
    ;; (forward-sexp)			; skip package name
    (let ((exported-symbols (sly-defpackage-exports))
          (symbols (if (consp symbols)
                       symbols
                     (list symbols)))
          (number-of-actions 0))
      (cl-ecase do-what
        (:export
         (sly-add-export)
         (dolist (symbol symbols)
           (let ((symbol-name (sly-cl-symbol-name symbol)))
             (unless (sly-symbol-exported-p symbol-name exported-symbols)
               (cl-incf number-of-actions)
               (sly-insert-export symbol-name)))))
        (:unexport
         (dolist (symbol symbols)
           (let ((symbol-name (sly-cl-symbol-name symbol)))
             (when (sly-symbol-exported-p symbol-name exported-symbols)
               (sly-remove-export symbol-name)
               (cl-incf number-of-actions))))))
      (when sly-export-save-file
        (save-buffer))
      number-of-actions)))

(defun sly-add-export ()
  (let (point)
    (save-excursion
      (while (ignore-errors (sly-goto-next-export-clause) t)
        (setq point (point))))
    (cond (point
           (goto-char point)
           (down-list)
           (sly-end-of-list))
          (t
           (sly-end-of-list)
           (unless (looking-back "^\\s-*")
             (newline-and-indent))
           (insert "(:export ")
           (save-excursion (insert ")"))))))

(defun sly-determine-symbol-style ()
  ;; Assumes we're inside :export
  (save-excursion
    (sly-beginning-of-list)
    (sly-forward-sexp)
    (let ((symbols (sly-export-symbols)))
      (cond ((null symbols)
             sly-export-symbol-representation-function)
            ((cl-every (lambda (x)
                         (string-match "^:" x))
                       symbols)
             (lambda (n) (format ":%s" n)))
            ((cl-every (lambda (x)
                         (string-match "^#:" x))
                       symbols)
             (lambda (n) (format "#:%s" n)))
            ((cl-every (lambda (x)
                         (string-prefix-p "\"" x))
                       symbols)
             (lambda (n) (prin1-to-string (upcase (substring-no-properties n)))))
            (t
             sly-export-symbol-representation-function)))))

(defun sly-format-symbol-for-defpackage (symbol-name)
  (funcall (if sly-export-symbol-representation-auto
               (sly-determine-symbol-style)
             sly-export-symbol-representation-function)
           symbol-name))

(defun sly-insert-export (symbol-name)
  ;; Assumes we're at the inside :export after the last symbol
  (let ((symbol-name (sly-format-symbol-for-defpackage symbol-name)))
    (unless (looking-back "^\\s-*")
      (newline-and-indent))
    (insert symbol-name)))

(defun sly-remove-export (symbol-name)
  ;; Assumes we're inside the beginning of a DEFPACKAGE form.
  (let ((point))
    (while (setq point (sly-search-exports-in-defpackage symbol-name))
      (save-excursion
	(goto-char point)
	(backward-sexp)
	(delete-region (point) point)
	(beginning-of-line)
	(when (looking-at "^\\s-*$")
          (join-line)
          (delete-trailing-whitespace (point) (line-end-position)))))))

(defun sly-export-symbol-at-point ()
  "Add the symbol at point to the defpackage source definition
belonging to the current buffer-package. With prefix-arg, remove
the symbol again. Additionally performs an EXPORT/UNEXPORT of the
symbol in the Lisp image if possible."
  (interactive)
  (let ((package (sly-current-package))
	(symbol (sly-symbol-at-point)))
    (unless symbol (error "No symbol at point."))
    (cond (current-prefix-arg
	   (if (cl-plusp (sly-frob-defpackage-form package :unexport symbol))
	       (sly-message "Symbol `%s' no longer exported form `%s'"
                        symbol package)
             (sly-message "Symbol `%s' is not exported from `%s'"
                      symbol package))
	   (sly-unexport-symbol symbol package))
	  (t
	   (if (cl-plusp (sly-frob-defpackage-form package :export symbol))
	       (sly-message "Symbol `%s' now exported from `%s'"
                        symbol package)
             (sly-message "Symbol `%s' already exported from `%s'"
                      symbol package))
	   (sly-export-symbol symbol package)))))

(defun sly-export-class (name)
  "Export acessors, constructors, etc. associated with a structure or a class"
  (interactive (list (sly-read-from-minibuffer "Export structure named: "
                                                 (sly-symbol-at-point))))
  (let* ((package (sly-current-package))
         (symbols (sly-eval `(slynk:export-structure ,name ,package))))
    (sly-message "%s symbols exported from `%s'"
             (sly-frob-defpackage-form package :export symbols)
             package)))

(defalias 'sly-export-structure 'sly-export-class)

(provide 'sly-package-fu)
