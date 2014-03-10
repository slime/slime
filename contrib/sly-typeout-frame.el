(require 'sly)
(require 'sly-autodoc)
(require 'cl-lib)

(defvar sly-typeout-frame-unbind-stack ())

(define-sly-contrib sly-typeout-frame
  "Display messages in a dedicated frame."
  (:authors "Luke Gorrie  <luke@synap.se>")
  (:license "GPL")
  (:on-load
   (unless (sly-typeout-tty-only-p)
     (add-hook 'sly-connected-hook 'sly-ensure-typeout-frame)
     (cl-loop for (var value) in 
              '((sly-message-function sly-typeout-message)
                (sly-background-message-function sly-typeout-message)
                (sly-autodoc-message-function sly-typeout-autodoc-message)
                (sly-autodoc-dimensions-function
                 sly-typeout-autodoc-dimensions))
              do (sly-typeout-frame-init-var var value))))
  (:on-unload
   (remove-hook 'sly-connected-hook 'sly-ensure-typeout-frame)
   (cl-loop for (var value) in sly-typeout-frame-unbind-stack 
            do (cond ((eq var 'sly-unbound) (makunbound var))
                     (t (set var value))))
   (setq sly-typeout-frame-unbind-stack nil)))

(defun sly-typeout-frame-init-var (var value)
  (push (list var (if (boundp var) (symbol-value var) 'sly-unbound))
	sly-typeout-frame-unbind-stack)
  (set var value))

(defun sly-typeout-tty-only-p ()
  (cond ((featurep 'xemacs)
	 (null (remove 'tty (mapcar #'device-type (console-device-list)))))
	(t (not (window-system)))))


;;;; Typeout frame

;; When a "typeout frame" exists it is used to display certain
;; messages instead of the echo area or pop-up windows.

(defvar sly-typeout-window nil
  "The current typeout window.")

(defvar sly-typeout-frame-properties
  '((height . 10) (minibuffer . nil))
  "The typeout frame properties (passed to `make-frame').")

(defun sly-typeout-buffer ()
  (with-current-buffer (get-buffer-create (sly-buffer-name :typeout))
    (setq buffer-read-only t)
    (current-buffer)))

(defun sly-typeout-active-p ()
  (and sly-typeout-window
       (window-live-p sly-typeout-window)))

(defun sly-typeout-message-aux (format-string &rest format-args)
  (sly-ensure-typeout-frame)
  (with-current-buffer (sly-typeout-buffer)
    (let ((inhibit-read-only t)
          (msg (apply #'format format-string format-args)))
      (unless (string= msg "")
	(erase-buffer)
	(insert msg)))))

(defun sly-typeout-message (format-string &rest format-args)
  (apply #'sly-typeout-message-aux format-string format-args))

(defun sly-make-typeout-frame ()
  "Create a frame for displaying messages (e.g. arglists)."
  (interactive)
  (let ((frame (make-frame sly-typeout-frame-properties)))
    (save-selected-window
      (select-window (frame-selected-window frame))
      (switch-to-buffer (sly-typeout-buffer))
      (setq sly-typeout-window (selected-window)))))

(defun sly-ensure-typeout-frame ()
  "Create the typeout frame unless it already exists."
  (interactive)
  (if (sly-typeout-active-p)
      (save-selected-window
        (select-window sly-typeout-window)
        (switch-to-buffer (sly-typeout-buffer)))
    (sly-make-typeout-frame)))

(defun sly-typeout-autodoc-message (doc)
  ;; No need for refreshing per `sly-autodoc-pre-command-refresh-echo-area'.
  ;; FIXME: eldoc doesn't know anything about this
  (sly-typeout-message-aux "%s" doc))

(defun sly-typeout-autodoc-dimensions ()
  (cond ((sly-typeout-active-p)
	 (list (window-width sly-typeout-window) nil))
	(t
	 (list 75 nil))))

(provide 'sly-typeout-frame)
