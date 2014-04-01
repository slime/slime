(require 'slime)
(require 'eldoc)
(require 'cl-lib)
(eval-when-compile
  (require 'cl))

(define-slime-contrib slime-autodoc
  "Show fancy arglist in echo area."
  (:license "GPL")
  (:authors "Luke Gorrie  <luke@bluetail.com>"
            "Lawrence Mitchell  <wence@gmx.li>"
            "Matthias Koeppe  <mkoeppe@mail.math.uni-magdeburg.de>"
            "Tobias C. Rittweiler  <tcr@freebits.de>")
  (:slime-dependencies slime-parse)
  (:swank-dependencies swank-arglists)
  (:on-load
   (dolist (h '(slime-mode-hook slime-repl-mode-hook sldb-mode-hook))
     (add-hook h 'slime-autodoc-maybe-enable)))
  (:on-unload
   ;; FIXME: This doesn't disable eldoc-mode in existing buffers.
   (setq slime-echo-arglist-function 'slime-show-arglist)
   (dolist (h '(slime-mode-hook slime-repl-mode-hook sldb-mode-hook))
     (remove-hook h 'slime-autodoc-maybe-enable))))

(defcustom slime-use-autodoc-mode t
  "When non-nil always enable slime-autodoc-mode in slime-mode."
  :type 'boolean
  :group 'slime-ui)

(defcustom slime-autodoc-use-multiline-p nil
  "If non-nil, allow long autodoc messages to resize echo area display."
  :type 'boolean
  :group 'slime-ui)

(defcustom slime-autodoc-delay 0.3
  "*Delay before autodoc messages are fetched and displayed, in seconds."
  :type 'number
  :group 'slime-ui)

(defcustom slime-autodoc-accuracy-depth 10
  "Number of paren levels that autodoc takes into account for
  context-sensitive arglist display (local functions. etc)"
  :type 'integer
  :group 'slime-ui)



(defun slime-arglist (name)
  "Show the argument list for NAME."
  (interactive (list (slime-read-symbol-name "Arglist of: " t)))
  (let ((arglist (slime-retrieve-arglist name)))
    (if (eq arglist :not-available)
        (error "Arglist not available")
        (message "%s" (slime-fontify-string arglist)))))

(defun slime-retrieve-arglist (name)
  (let ((name (etypecase name
                 (string name)
                 (symbol (symbol-name name)))))
    (car (slime-eval `(swank:autodoc '(,name ,slime-cursor-marker))))))


;;;; Autodocs (automatic context-sensitive help)

(defun slime-make-autodoc-rpc-form ()
  "Return a cache key and a swank form."
  (let* ((levels slime-autodoc-accuracy-depth)
         (buffer-form (slime-parse-form-upto-point levels)))
    (when buffer-form
      (values buffer-form
              `(swank:autodoc ',buffer-form
                              :print-right-margin
                              ,(window-width (minibuffer-window)))))))


;;;; Autodoc cache

(defvar slime-autodoc-last-buffer-form nil)
(defvar slime-autodoc-last-autodoc nil)

(defun slime-get-cached-autodoc (buffer-form)
  "Return the cached autodoc documentation for `buffer-form', or nil."
  (when (equal buffer-form slime-autodoc-last-buffer-form)
    slime-autodoc-last-autodoc))

(defun slime-store-into-autodoc-cache (buffer-form autodoc)
  "Update the autodoc cache for SYMBOL with DOCUMENTATION.
Return DOCUMENTATION."
  (setq slime-autodoc-last-buffer-form buffer-form)
  (setq slime-autodoc-last-autodoc autodoc))


;;;; Formatting autodoc

(defsubst slime-canonicalize-whitespace (string)
  (replace-regexp-in-string "[ \n\t]+" " "  string))

(defun slime-format-autodoc (doc multilinep)
  (let ((doc (slime-fontify-string doc)))
    (if multilinep
        doc
        (slime-oneliner (slime-canonicalize-whitespace doc)))))

(defun slime-fontify-string (string)
  "Fontify STRING as `font-lock-mode' does in Lisp mode."
  (with-current-buffer (get-buffer-create (slime-buffer-name :fontify 'hidden))
    (erase-buffer)
    (unless (eq major-mode 'lisp-mode)
      ;; Just calling (lisp-mode) will turn slime-mode on in that buffer,
      ;; which may interfere with this function
      (setq major-mode 'lisp-mode)
      (lisp-mode-variables t))
    (insert string)
    (let ((font-lock-verbose nil))
      (font-lock-fontify-buffer))
    (goto-char (point-min))
    (when (re-search-forward "===> \\(\\(.\\|\n\\)*\\) <===" nil t)
      (let ((highlight (match-string 1)))
        ;; Can't use (replace-match highlight) here -- broken in Emacs 21
        (delete-region (match-beginning 0) (match-end 0))
	(slime-insert-propertized '(face highlight) highlight)))
    (buffer-substring (point-min) (point-max))))


;;;; slime-autodoc-mode

(cl-defun slime-autodoc (&optional (multilinep slime-autodoc-use-multiline-p)
                                   cache-multiline)
  "Returns the cached arglist information as string, or nil.
If it's not in the cache, the cache will be updated asynchronously."
  (interactive)
  (save-excursion
    ;; Save match data just in case. This is automatically run in
    ;; background, so it'd be rather disastrous if it touched match
    ;; data.
    (save-match-data
      (unless (if (fboundp 'slime-repl-inside-string-or-comment-p)
                  (slime-repl-inside-string-or-comment-p)
                (slime-inside-string-or-comment-p))
        (cl-multiple-value-bind (cache-key retrieve-form)
            (slime-make-autodoc-rpc-form)
          (let* (cached
                 (multilinep (or (slime-autodoc-multiline-cached
				  (car cache-key))
                                 multilinep)))
            (slime-autodoc-cache-multiline (car cache-key) cache-multiline)
            (cond
             ((not cache-key) nil)
             ((setq cached (slime-get-cached-autodoc cache-key))
              (slime-format-autodoc cached multilinep))
             (t
              ;; If nothing is in the cache, we first decline (by
              ;; returning nil), and fetch the arglist information
              ;; asynchronously.
              (slime-eval-async retrieve-form
                (lexical-let
                 ((cache-key cache-key)
                  (multilinep multilinep))
                 (lambda (doc)
                   (cl-destructuring-bind (doc cache-p) doc
                     (unless (eq doc :not-available)
                       (when cache-p
                         (slime-store-into-autodoc-cache cache-key doc))
                       ;; Now that we've got our information,
                       ;; get it to the user ASAP.
                       (eldoc-message
                        (slime-format-autodoc doc multilinep)))))))
              nil))))))))

(defvar slime-autodoc-cache-car nil)

(defun slime-autodoc-multiline-cached (cache-key)
  (equal cache-key
         slime-autodoc-cache-car))

(defun slime-autodoc-cache-multiline (cache-key cache-new-p)
  (cond (cache-new-p
         (setq slime-autodoc-cache-car
               cache-key))
        ((not (equal cache-key
                     slime-autodoc-cache-car))
         (setq slime-autodoc-cache-car nil))))

(make-variable-buffer-local (defvar slime-autodoc-mode nil))

;; FIXME: use `define-minor-mode'.
(defun slime-autodoc-mode (&optional arg interactive)
  (interactive (list (or current-prefix-arg 'toggle) t))
  (make-local-variable 'eldoc-documentation-function)
  (make-local-variable 'eldoc-idle-delay)
  (make-local-variable 'eldoc-minor-mode-string)
  (setq eldoc-documentation-function 'slime-autodoc)
  (setq eldoc-idle-delay slime-autodoc-delay)
  (setq eldoc-minor-mode-string " Autodoc")
  (setq slime-autodoc-mode (eldoc-mode arg))
  (when interactive
    (message (format "Slime autodoc mode %s."
                     (if slime-autodoc-mode "enabled" "disabled")))))

(defun slime-autodoc-manually ()
  "Like slime-autodoc, but when called twice,
or after slime-autodoc was already automatically called,
display multiline arglist"
  (interactive)
  (eldoc-message (slime-autodoc (or slime-autodoc-use-multiline-p
                                    slime-autodoc-mode)
                                t)))

(defun slime-autodoc-maybe-enable ()
  (when slime-use-autodoc-mode
    (slime-autodoc-mode 1)
    (setq slime-echo-arglist-function
          (lambda ()
            (if slime-autodoc-mode
                (eldoc-message (slime-autodoc))
              (slime-show-arglist))))))

(defadvice eldoc-display-message-no-interference-p
    (after slime-autodoc-message-ok-p)
  (when slime-autodoc-mode
    (setq ad-return-value
          (and ad-return-value
               ;; Display arglist only when the minibuffer is
               ;; inactive, e.g. not on `C-x C-f'.
               (not (active-minibuffer-window))
               ;; Display arglist only when inferior Lisp will be able
               ;; to cope with the request.
               (slime-background-activities-enabled-p)))
    (slime-bind-keys slime-doc-map t '((?A slime-autodoc-manually))))
  ad-return-value)

(provide 'slime-autodoc)
