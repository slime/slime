;; slime-snapshot.el --- Save&restore memory images without disconnecting

(defun slime-snapshot (filename)
  "Save a memory image to the file FILENAME."
  (interactive (list (read-file-name "Image file: ")))
  (slime-eval-with-transcript 
   `(swank-snapshot:save-snapshot ,(expand-file-name filename))))

(defun slime-restore (filename)
  "Restore a memory image stored in file FILENAME."
  (interactive (list (read-file-name "Image file: ")))
  ;; bypass event dispatcher because we don't expect a reply. FIXME.
  (slime-net-send `(:emacs-rex (swank-snapshot:restore-snapshot 
				,(expand-file-name filename))
			       nil t nil)
		  (slime-connection)))

(defun slime-snapshot-init ()
  (slime-require :swank-snapshot))