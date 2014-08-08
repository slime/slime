;;; sly-buttons.el --- Button-related utils for SLY
;;;
(require 'cl-lib)

(defvar sly-part-button-keymap
      (let ((map (make-sparse-keymap)))
        (set-keymap-parent map button-map)
        (define-key map (kbd "RET") 'sly-button-inspect)
        (define-key map (kbd "d")   'sly-button-describe)
        (define-key map (kbd "p")   'sly-button-pretty-print)
        (define-key map (kbd "v")   'sly-button-show-source)
        (define-key map [down-mouse-3] 'sly-button-popup-part-menu)
        (define-key map [mouse-3] 'sly-button-popup-part-menu)
        map))

(defun sly-button--popup-option-name (symbol)
  (save-match-data
    (and 
     (not (eq symbol 'sly-button-popup-part-menu))
     (string-match "sly-button-\\(.*\\)" (symbol-name symbol))
     (match-string 1 (symbol-name symbol)))))

(defun sly-button--popup-part-menu-options (button &optional keymap)
  (cl-loop
   with keymap = (or keymap (button-get button 'keymap))
   for pair in (cdr keymap)
   for binding = (and (consp pair) (cdr pair))
   for name = (and binding
                   (symbolp binding)
                   (sly-button--popup-option-name binding))
   for function-sym = (and name
                           (intern (format "%s-function" name)))
   for readable-name = (and name
                            (upcase-initials (replace-regexp-in-string
                                              "-" " " name)))
   if (and name
           (not (eq (button-get button function-sym)
                    'sly-button-not-implemented)))
   collect `(,binding menu-item ,readable-name ,binding)
   else if (keymapp binding)
   append (sly-button--popup-part-menu-options button binding)))

(defun sly-button-popup-part-menu (event)
  "Popup a menu for a `sly-part' button"
  (interactive "e")
  (let ((button (button-at (posn-point (event-end event)))))
    (popup-menu
     `(keymap
       (heading menu-item ,(button-get button 'part-label))
       (separator menu-item "----")
       ,@(sly-button--popup-part-menu-options button)))))

(defun sly-button-at-point ()
  (button-at (if (mouse-event-p last-input-event)
                 (posn-point (event-end last-input-event))
                 (point))))

(defmacro sly-button-define-part-action (action)
  `(defun ,(intern (format "sly-button-%s" action)) (button)
     ,(format "%s the object under BUTTON of type `sly-part'."
              (upcase-initials action))
     (interactive (list (sly-button-at-point)))
     (apply (button-get button ',(intern (format "%s-function" action)))
            (button-get button 'part-args))))

(sly-button-define-part-action "inspect")

(sly-button-define-part-action "describe")

(sly-button-define-part-action "pretty-print")

(sly-button-define-part-action "show-source")

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
  'pretty-print-function 'sly-button-not-implemented
  'keymap sly-part-button-keymap)

(provide 'sly-buttons)

;;; sly-buttons.el ends here
