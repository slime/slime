;;; sly-buttons.el --- Button-related utils for SLY

(defvar sly-part-button-keymap
      (let ((map (make-sparse-keymap)))
        (set-keymap-parent map button-map)
        (define-key map (kbd "RET") 'sly-button-inspect)
        (define-key map (kbd "d") 'sly-button-describe)
        (define-key map [down-mouse-3] 'sly-button-popup-part-menu)
        (define-key map [mouse-3] 'sly-button-popup-part-menu)
        map))

(defun sly-button--popup-option-name (symbol)
  (save-match-data
    (and 
     (not (eq symbol 'sly-button-popup-part-menu))
     (string-match "sly-button-\\(.*\\)" (symbol-name symbol))
     (upcase-initials (replace-regexp-in-string
                       "-" " " (match-string 1 (symbol-name symbol)))))))

(defun sly-button--popup-part-menu-options (keymap)
  (cl-loop
   for pair in (cdr keymap)
   for binding = (and (consp pair) (cdr pair))
   for name = (and binding
                   (symbolp binding)
                   (sly-button--popup-option-name binding))
   if name
   collect `(,(vector binding) menu-item ,name ,binding)
   else if (keymapp binding)
   append (sly-button--popup-part-menu-options binding)))

(defun sly-button-popup-part-menu (event)
  "Popup a menu for a `sly-part' button"
  (interactive "e")
  (popup-menu
   `(keymap
     (heading menu-item ,(button-get (button-at (posn-point (event-end event)))
                                     'part-label))
     (separator menu-item "----")
     ,@(sly-button--popup-part-menu-options sly-part-button-keymap))))

(defun sly-button--label ()
  (let ((button (sly-button--at-point-or-lose)))
    (format "%s" button)))

(defun sly-button-at-point ()
  (button-at (if (mouse-event-p last-input-event)
                 (posn-point (event-end last-input-event))
                 (point))))

(defun sly-button-inspect (button)
  "Inspect the object under BUTTON of type `sly-part'."
  (interactive (list (sly-button-at-point)))
  (apply (button-get button 'inspect-function)
         (button-get button 'part-args)))

(defun sly-button-describe (button)
  "Describe the object under the BUTTON of type `sly-part.'"
  (interactive (list (sly-button-at-point)))
  (apply (button-get button 'describe-function)
         (button-get button 'part-args)))

(defun sly-make-action-button (label action &rest props)
  (apply #'make-text-button
         label nil :type 'sly-action
         'action action
         'mouse-action action
         props))

(defun sly-button-not-implemented (&rest _ignore)
  (error "[sly] not implemented!"))

(define-button-type 'sly-action
  'face 'sly-inspector-action-face
  'mouse-face 'highlight)

(define-button-type 'sly-part
  'face 'sly-inspectable-value-face
  'action 'sly-button-inspect
  'mouse-action 'sly-button-inspect
  'inspect-function 'sly-button-not-implemented
  'describe-function 'sly-button-not-implemented
  'keymap sly-part-button-keymap)

(provide 'sly-buttons)

;;; sly-buttons.el ends here
