(eval-and-compile
  (require 'sly))

(define-sly-contrib sly-snapshot
  "Save&restore memory images without disconnecting"
  (:authors "Helmut Eller <heller@common-lisp.net>")
  (:license "GPL v3")
  (:swank-dependencies swank-snapshot))

(defun sly-snapshot (filename &optional background)
  "Save a memory image to the file FILENAME."
  (interactive (list (read-file-name "Image file: ")
		     current-prefix-arg))
  (let ((file (expand-file-name filename)))
    (when (and (file-exists-p file)
	       (not (yes-or-no-p (format "File exists %s.  Overwrite it? " 
					 filename))))
      (signal 'quit nil))
    (sly-eval-with-transcript
     `(,(if background
	    'swank-snapshot:background-save-snapshot
	  'swank-snapshot:save-snapshot)
       ,file))))

(defun sly-restore (filename)
  "Restore a memory image stored in file FILENAME."
  (interactive (list (read-file-name "Image file: ")))
  ;; bypass event dispatcher because we don't expect a reply. FIXME.
  (sly-net-send `(:emacs-rex (swank-snapshot:restore-snapshot 
				,(expand-file-name filename))
			       nil t nil)
		  (sly-connection)))

(provide 'sly-snapshot)
