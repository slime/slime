;;; slime-typeout-frame.el --- display some message in a dedicated frame
;;
;; Author: Luke Gorrie  <luke@synap.se>
;; License: GNU GPL (same license as Emacs)
;;
;;; Installation:
;;
;; Add something like this to your .emacs: 
;;
;;   (add-to-list 'load-path "<directory-of-this-file>")
;;   (add-hook 'slime-load-hook (lambda () (require 'slime-typeout-frame)))
;;


;;;; Typeout frame

;; When a "typeout frame" exists it is used to display certain
;; messages instead of the echo area or pop-up windows.

(defvar slime-typeout-window nil
  "The current typeout window.")

(defvar slime-typeout-frame-properties
  '((height . 10) (minibuffer . nil))
  "The typeout frame properties (passed to `make-frame').")

(defun slime-typeout-active-p ()
  (and slime-typeout-window
       (window-live-p slime-typeout-window)))

(defun slime-typeout-message (format-string &rest format-args)
  (slime-ensure-typeout-frame)
  (with-current-buffer (window-buffer slime-typeout-window)
    (erase-buffer)
    (insert (apply #'format format-string format-args))))

(defun slime-make-typeout-frame ()
  "Create a frame for displaying messages (e.g. arglists)."
  (interactive)
  (let ((frame (make-frame slime-typeout-frame-properties)))
    (save-selected-window
      (select-window (frame-selected-window frame))
      (switch-to-buffer "*SLIME-Typeout*")
      (setq slime-typeout-window (selected-window)))))

(defun slime-ensure-typeout-frame ()
  "Create the typeout frame unless it already exists."
  (interactive)
  (unless (slime-typeout-active-p)
    (slime-make-typeout-frame)))

(defun slime-typeout-autodoc-message (doc)
  (setq slime-autodoc-last-message "") ; no need for refreshing
  (slime-typeout-message doc))


;;; Initialization

(defun slime-install-typeout-frame ()
  (add-hook 'slime-connected-hook 'slime-ensure-typeout-frame)
  (setq slime-message-function #'slime-typeout-message)
  (setq slime-background-message-function #'slime-typeout-message)
  (setq slime-autodoc-message-function #'slime-typeout-autodoc-message))

(slime-install-typeout-frame)

(provide 'slime-typeout-frame)
