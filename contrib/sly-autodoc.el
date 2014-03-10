(require 'sly)
(require 'eldoc)
(require 'cl-lib)
(eval-when-compile
  (require 'cl))

(define-sly-contrib sly-autodoc
  "Show fancy arglist in echo area."
  (:license "GPL")
  (:authors "Luke Gorrie  <luke@bluetail.com>"
            "Lawrence Mitchell  <wence@gmx.li>"
            "Matthias Koeppe  <mkoeppe@mail.math.uni-magdeburg.de>"
            "Tobias C. Rittweiler  <tcr@freebits.de>")
  (:sly-dependencies sly-parse)
  (:swank-dependencies swank-arglists)
  (:on-load
   (dolist (h '(sly-mode-hook sly-repl-mode-hook sldb-mode-hook))
     (add-hook h 'sly-autodoc-maybe-enable)))
  (:on-unload
   ;; FIXME: This doesn't disable eldoc-mode in existing buffers.
   (setq sly-echo-arglist-function 'sly-show-arglist)
   (dolist (h '(sly-mode-hook sly-repl-mode-hook sldb-mode-hook))
     (remove-hook h 'sly-autodoc-maybe-enable))))

(defcustom sly-use-autodoc-mode t
  "When non-nil always enable sly-autodoc-mode in sly-mode."
  :type 'boolean
  :group 'sly-ui)

(defcustom sly-autodoc-use-multiline-p nil
  "If non-nil, allow long autodoc messages to resize echo area display."
  :type 'boolean
  :group 'sly-ui)

(defcustom sly-autodoc-delay 0.3
  "*Delay before autodoc messages are fetched and displayed, in seconds."
  :type 'number
  :group 'sly-ui)

(defcustom sly-autodoc-accuracy-depth 10
  "Number of paren levels that autodoc takes into account for
  context-sensitive arglist display (local functions. etc)"
  :type 'integer
  :group 'sly-ui)



(defun sly-arglist (name)
  "Show the argument list for NAME."
  (interactive (list (sly-read-symbol-name "Arglist of: " t)))
  (let ((arglist (sly-retrieve-arglist name)))
    (if (eq arglist :not-available)
        (error "Arglist not available")
        (message "%s" (sly-fontify-string arglist)))))

(defun sly-retrieve-arglist (name)
  (let ((name (etypecase name
                 (string name)
                 (symbol (symbol-name name)))))
    (car (sly-eval `(swank:autodoc '(,name ,sly-cursor-marker))))))


;;;; Autodocs (automatic context-sensitive help)

(defun sly-make-autodoc-rpc-form ()
  "Return a cache key and a swank form."
  (let* ((levels sly-autodoc-accuracy-depth)
         (buffer-form (sly-parse-form-upto-point levels)))
    (when buffer-form
      (values buffer-form
              `(swank:autodoc ',buffer-form
                              :print-right-margin
                              ,(window-width (minibuffer-window)))))))


;;;; Autodoc cache

(defvar sly-autodoc-last-buffer-form nil)
(defvar sly-autodoc-last-autodoc nil)

(defun sly-get-cached-autodoc (buffer-form)
  "Return the cached autodoc documentation for `buffer-form', or nil."
  (when (equal buffer-form sly-autodoc-last-buffer-form)
    sly-autodoc-last-autodoc))

(defun sly-store-into-autodoc-cache (buffer-form autodoc)
  "Update the autodoc cache for SYMBOL with DOCUMENTATION.
Return DOCUMENTATION."
  (setq sly-autodoc-last-buffer-form buffer-form)
  (setq sly-autodoc-last-autodoc autodoc))


;;;; Formatting autodoc

(defsubst sly-canonicalize-whitespace (string)
  (replace-regexp-in-string "[ \n\t]+" " "  string))

(defun sly-format-autodoc (doc multilinep)
  (let ((doc (sly-fontify-string doc)))
    (if multilinep
        doc
        (sly-oneliner (sly-canonicalize-whitespace doc)))))

(defun sly-fontify-string (string)
  "Fontify STRING as `font-lock-mode' does in Lisp mode."
  (with-current-buffer (get-buffer-create (sly-buffer-name :fontify 'hidden))
    (erase-buffer)
    (unless (eq major-mode 'lisp-mode)
      ;; Just calling (lisp-mode) will turn sly-mode on in that buffer,
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
	(sly-insert-propertized '(face highlight) highlight)))
    (buffer-substring (point-min) (point-max))))


;;;; sly-autodoc-mode

(cl-defun sly-autodoc (&optional (multilinep sly-autodoc-use-multiline-p)
                                   cache-multiline)
  "Returns the cached arglist information as string, or nil.
If it's not in the cache, the cache will be updated asynchronously."
  (interactive)
  (save-excursion
    ;; Save match data just in case. This is automatically run in
    ;; background, so it'd be rather disastrous if it touched match
    ;; data.
    (save-match-data
      (unless (if (fboundp 'sly-repl-inside-string-or-comment-p)
                  (sly-repl-inside-string-or-comment-p)
                (sly-inside-string-or-comment-p))
        (cl-multiple-value-bind (cache-key retrieve-form)
            (sly-make-autodoc-rpc-form)
          (let* (cached
                 (multilinep (or (sly-autodoc-multiline-cached
				  (car cache-key))
                                 multilinep)))
            (sly-autodoc-cache-multiline (car cache-key) cache-multiline)
            (cond
             ((not cache-key) nil)
             ((setq cached (sly-get-cached-autodoc cache-key))
              (sly-format-autodoc cached multilinep))
             (t
              ;; If nothing is in the cache, we first decline (by
              ;; returning nil), and fetch the arglist information
              ;; asynchronously.
              (sly-eval-async retrieve-form
                (lexical-let
                 ((cache-key cache-key)
                  (multilinep multilinep))
                 (lambda (doc)
                   (cl-destructuring-bind (doc cache-p) doc
                     (unless (eq doc :not-available)
                       (when cache-p
                         (sly-store-into-autodoc-cache cache-key doc))
                       ;; Now that we've got our information,
                       ;; get it to the user ASAP.
                       (eldoc-message
                        (sly-format-autodoc doc multilinep)))))))
              nil))))))))

(defvar sly-autodoc-cache-car nil)

(defun sly-autodoc-multiline-cached (cache-key)
  (equal cache-key
         sly-autodoc-cache-car))

(defun sly-autodoc-cache-multiline (cache-key cache-new-p)
  (cond (cache-new-p
         (setq sly-autodoc-cache-car
               cache-key))
        ((not (equal cache-key
                     sly-autodoc-cache-car))
         (setq sly-autodoc-cache-car nil))))

(make-variable-buffer-local (defvar sly-autodoc-mode nil))

;; FIXME: use `define-minor-mode'.
(defun sly-autodoc-mode (&optional arg interactive)
  (interactive (list (or current-prefix-arg 'toggle) t))
  (make-local-variable 'eldoc-documentation-function)
  (make-local-variable 'eldoc-idle-delay)
  (make-local-variable 'eldoc-minor-mode-string)
  (setq eldoc-documentation-function 'sly-autodoc)
  (setq eldoc-idle-delay sly-autodoc-delay)
  (setq eldoc-minor-mode-string " Autodoc")
  (setq sly-autodoc-mode (eldoc-mode arg))
  (when interactive
    (message (format "Slime autodoc mode %s."
                     (if sly-autodoc-mode "enabled" "disabled")))))

(defun sly-autodoc-manually ()
  "Like sly-autodoc, but when called twice,
or after sly-autodoc was already automatically called,
display multiline arglist"
  (interactive)
  (eldoc-message (sly-autodoc (or sly-autodoc-use-multiline-p
                                    sly-autodoc-mode)
                                t)))

(defun sly-autodoc-maybe-enable ()
  (when sly-use-autodoc-mode
    (sly-autodoc-mode 1)
    (setq sly-echo-arglist-function
          (lambda ()
            (if sly-autodoc-mode
                (eldoc-message (sly-autodoc))
              (sly-show-arglist))))))

(defadvice eldoc-display-message-no-interference-p
    (after sly-autodoc-message-ok-p)
  (when sly-autodoc-mode
    (setq ad-return-value
          (and ad-return-value
               ;; Display arglist only when the minibuffer is
               ;; inactive, e.g. not on `C-x C-f'.
               (not (active-minibuffer-window))
               ;; Display arglist only when inferior Lisp will be able
               ;; to cope with the request.
               (sly-background-activities-enabled-p)))
    (sly-bind-keys sly-doc-map t '((?A sly-autodoc-manually))))
  ad-return-value)

(provide 'sly-autodoc)
