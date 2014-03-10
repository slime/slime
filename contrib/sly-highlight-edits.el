(require 'sly)
(require 'sly-parse)

(define-sly-contrib sly-highlight-edits
  "Highlight edited, i.e. not yet compiled, code."
  (:authors "William Bland <doctorbill.news@gmail.com>")
  (:license "GPL")
  (:on-load   (add-hook 'sly-mode-hook 'sly-activate-highlight-edits))
  (:on-unload (remove-hook 'sly-mode-hook 'sly-activate-highlight-edits)))

(defun sly-activate-highlight-edits ()
 (sly-highlight-edits-mode 1))

(defface sly-highlight-edits-face
    `((((class color) (background light))
       (:background "lightgray"))
      (((class color) (background dark))
       (:background "dimgray"))
      (t (:background "yellow")))
  "Face for displaying edit but not compiled code."
  :group 'sly-mode-faces)

(define-minor-mode sly-highlight-edits-mode 
  "Minor mode to highlight not-yet-compiled code." nil)

(add-hook 'sly-highlight-edits-mode-on-hook
          'sly-highlight-edits-init-buffer)

(add-hook 'sly-highlight-edits-mode-off-hook
          'sly-highlight-edits-reset-buffer)

(defun sly-highlight-edits-init-buffer ()
  (make-local-variable 'after-change-functions)
  (add-to-list 'after-change-functions 
               'sly-highlight-edits)
  (add-to-list 'sly-before-compile-functions
               'sly-highlight-edits-compile-hook))

(defun sly-highlight-edits-reset-buffer ()
  (setq after-change-functions  
        (remove 'sly-highlight-edits after-change-functions))
  (sly-remove-edits (point-min) (point-max)))

;; FIXME: what's the LEN arg for?
(defun sly-highlight-edits (beg end &optional len) 
  (save-match-data
    (when (and (sly-connected-p)
               (not (sly-inside-comment-p))
               (not (sly-only-whitespace-p beg end)))
      (let ((overlay (make-overlay beg end)))
        (overlay-put overlay 'face 'sly-highlight-edits-face)
        (overlay-put overlay 'sly-edit t)))))

(defun sly-remove-edits (start end)
  "Delete the existing Slime edit hilights in the current buffer."
  (save-excursion
    (goto-char start)
    (while (< (point) end)
      (dolist (o (overlays-at (point)))
        (when (overlay-get o 'sly-edit)
          (delete-overlay o)))
      (goto-char (next-overlay-change (point))))))

(defun sly-highlight-edits-compile-hook (start end)
  (when sly-highlight-edits-mode
    (let ((start (save-excursion (goto-char start) 
				 (skip-chars-backward " \t\n\r")
				 (point)))
	  (end (save-excursion (goto-char end) 
			       (skip-chars-forward " \t\n\r")
			       (point))))
      (sly-remove-edits start end))))

(defun sly-only-whitespace-p (beg end)
  "Contains the region from BEG to END only whitespace?"
  (save-excursion
    (goto-char beg)
    (skip-chars-forward " \n\t\r" end)
    (<= end (point))))

(provide 'sly-highlight-edits)
