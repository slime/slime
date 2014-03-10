(require 'sly)
(require 'cl-lib)

(define-sly-contrib sly-sbcl-exts
  "Misc extensions for SBCL"
  (:authors "Tobias C. Rittweiler <tcr@freebits.de>")
  (:license "GPL")
  (:sly-dependencies sly-references)
  (:swank-dependencies swank-sbcl-exts))

(defun sly-sbcl-bug-at-point ()
  (save-excursion
    (save-match-data
      (unless (looking-at "#[0-9]\\{6\\}")
        (search-backward-regexp "#\\<" (line-beginning-position) t))
      (when (looking-at "#[0-9]\\{6\\}")
        (buffer-substring-no-properties (match-beginning 0) (match-end 0))))))

(defun sly-read-sbcl-bug (prompt &optional query)
  "Either read a sbcl bug or choose the one at point.
The user is prompted if a prefix argument is in effect, if there is no
symbol at point, or if QUERY is non-nil."
  (let ((bug (sly-sbcl-bug-at-point)))
    (cond ((or current-prefix-arg query (not bug))
           (sly-read-from-minibuffer prompt bug))
          (t bug))))

(defun sly-visit-sbcl-bug (bug)
  "Visit the Launchpad site that describes `bug' (#nnnnnn)."
  (interactive (list (sly-read-sbcl-bug "Bug number (#nnnnnn): ")))
  (browse-url (format "http://bugs.launchpad.net/sbcl/+bug/%s" 
                      (substring bug 1))))

(provide 'sly-sbcl-exts)
