;;; inferior-sly.el --- Minor mode with Slime keys for comint buffers
;;
;; Author: Luke Gorrie  <luke@synap.se>
;; License: GNU GPL (same license as Emacs)
;;
;;; Installation:
;;
;; Add something like this to your .emacs: 
;;
;;   (add-to-list 'load-path "<directory-of-this-file>")
;;   (add-hook 'sly-load-hook (lambda () (require 'inferior-sly)))
;;   (add-hook 'inferior-lisp-mode-hook (lambda () (inferior-sly-mode 1)))
(require 'sly)
(require 'cl-lib)

(define-minor-mode inferior-sly-mode
  "\\<sly-mode-map>\
Inferior SLY mode: The Inferior Superior Lisp Mode for Emacs.

This mode is intended for use with `inferior-lisp-mode'. It provides a
subset of the bindings from `sly-mode'.

\\{inferior-sly-mode-map}"
  nil
  nil
  ;; Fake binding to coax `define-minor-mode' to create the keymap
  '((" " 'undefined)))

(add-to-list 'minor-mode-alist
             '(inferior-sly-mode
               (" Inf-Slime" sly-state-name)))

(defun inferior-sly-return ()
  "Handle the return key in the inferior-lisp buffer.
The current input should only be sent if a whole expression has been
entered, i.e. the parenthesis are matched.

A prefix argument disables this behaviour."
  (interactive)
  (if (or current-prefix-arg (inferior-sly-input-complete-p))
      (comint-send-input)
    (insert "\n")
    (inferior-sly-indent-line)))

(defun inferior-sly-indent-line ()
  "Indent the current line, ignoring everything before the prompt."
  (interactive)
  (save-restriction
    (let ((indent-start
           (save-excursion
             (goto-char (process-mark (get-buffer-process (current-buffer))))
             (let ((inhibit-field-text-motion t))
               (beginning-of-line 1))
             (point))))
      (narrow-to-region indent-start (point-max)))
    (lisp-indent-line)))

(defun inferior-sly-input-complete-p ()
  "Return true if the input is complete in the inferior lisp buffer."
  (sly-input-complete-p (process-mark (get-buffer-process (current-buffer)))
                          (point-max)))

(defun inferior-sly-closing-return ()
  "Send the current expression to Lisp after closing any open lists."
  (interactive)
  (goto-char (point-max))
  (save-restriction
    (narrow-to-region (process-mark (get-buffer-process (current-buffer)))
                      (point-max))
    (while (ignore-errors (save-excursion (backward-up-list 1) t))
      (insert ")")))
  (comint-send-input))

(defun inferior-sly-change-directory (directory)
  "Set default-directory in the *inferior-lisp* buffer to DIRECTORY."
  (let* ((proc (sly-process))
	 (buffer (and proc (process-buffer proc))))
    (when buffer 
      (with-current-buffer buffer
	(cd-absolute directory)))))

(defun inferior-sly-init-keymap ()
  (let ((map inferior-sly-mode-map))
    (set-keymap-parent map sly-parent-map)
    (sly-define-keys map
      ([return]			'inferior-sly-return)
      ([(control return)]	'inferior-sly-closing-return)
      ([(meta control ?m)]	'inferior-sly-closing-return)
      ("\t"			'sly-indent-and-complete-symbol)
      (" "			'sly-space))))

(inferior-sly-init-keymap)

(defun inferior-sly-hook-function ()
  (inferior-sly-mode 1))

(defun inferior-sly-switch-to-repl-buffer ()
  (switch-to-buffer (process-buffer (sly-inferior-process))))

(defun inferior-sly-show-transcript (string)
  (remove-hook 'comint-output-filter-functions
	       'inferior-sly-show-transcript t)
  (with-current-buffer (process-buffer (sly-inferior-process))
    (let ((window (display-buffer (current-buffer) t)))
      (set-window-point window (point-max)))))

(defun inferior-sly-start-transcript ()
  (let ((proc (sly-inferior-process)))
    (when proc
      (with-current-buffer (process-buffer proc)
	(add-hook 'comint-output-filter-functions 
		  'inferior-sly-show-transcript
		  nil t)))))

(defun inferior-sly-stop-transcript ()
  (let ((proc (sly-inferior-process)))
    (when proc
      (with-current-buffer (process-buffer (sly-inferior-process))
	(run-with-timer 0.2 nil 
			(lambda (buffer) 
			  (with-current-buffer buffer
			    (remove-hook 'comint-output-filter-functions
					 'inferior-sly-show-transcript t)))
			(current-buffer))))))

(defun inferior-sly-init ()
  (add-hook 'sly-inferior-process-start-hook 'inferior-sly-hook-function)
  (add-hook 'sly-change-directory-hooks 'inferior-sly-change-directory)
  (add-hook 'sly-transcript-start-hook 'inferior-sly-start-transcript)
  (add-hook 'sly-transcript-stop-hook 'inferior-sly-stop-transcript)
  (def-sly-selector-method ?r
    "SLY Read-Eval-Print-Loop."
    (process-buffer (sly-inferior-process))))

(provide 'inferior-sly)
