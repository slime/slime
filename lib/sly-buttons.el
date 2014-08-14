;;; sly-buttons.el --- Button-related utils for SLY
;;;
(require 'cl-lib)

(defvar sly-part-button-keymap
      (let ((map (make-sparse-keymap)))
        (set-keymap-parent map button-map)
        (define-key map (kbd "RET") 'sly-button-inspect)
        (define-key map [down-mouse-3] 'sly-button-popup-part-menu)
        (define-key map [mouse-3] 'sly-button-popup-part-menu)
        map))

(defvar sly-button-popup-part-menu-keymap
  (let ((map (make-sparse-keymap)))
    map))

(defun sly-button-popup-part-menu (event)
  "Popup a menu for a `sly-part' button"
  (interactive "@e")
  (let ((button (button-at (posn-point (event-end event)))))
    (popup-menu
     `(keymap
       (heading menu-item ,(button-get button 'part-label))
       (separator menu-item "----")
       ,@(cdr (button-get button 'part-menu-keymap))))))

(defun sly-button-at (&optional pos type no-error)
  (let ((button (button-at (or pos
                               (if (mouse-event-p last-input-event)
                                   (posn-point (event-end last-input-event))
                                 (point))))))
    (cond ((and button type
                (eq (button-type button) type))
           button)
          ((and button type)
           (unless no-error
             (error "[sly] Button at point is not of expected type %s" type)))
          (button
           button)
          (t
           (unless no-error
             (error "[sly] No button at point"))))))

(defmacro sly-button-define-part-action (action label key)
  `(progn
     (defun ,action (button)
       ,(format "%s the object under BUTTON."
                label)
       (interactive (list (sly-button-at)))
       (let ((fn (button-get button ',action))
             (args (button-get button 'part-args)))
         (cond ((and fn args)
                (apply fn args))
               (args
                (error (format "[sly] button of type `%s' doesn't implement `%s'"
                               (button-type button) ',action)))
               (fn
                (error (format "[sly] button %s doesn't have the `part-args' property"
                               button))))))
     (define-key sly-part-button-keymap ,key ',action)
     (define-key sly-button-popup-part-menu-keymap
       [,action] '(menu-item ,label ,action
                             :visible (let ((button (sly-button-at)))
                                        (and button
                                             (button-get button ',action)))))))

(sly-button-define-part-action sly-button-inspect      "Inspect"      (kbd "i"))
(sly-button-define-part-action sly-button-describe     "Describe"     (kbd "d"))
(sly-button-define-part-action sly-button-pretty-print "Pretty Print" (kbd "p"))
(sly-button-define-part-action sly-button-show-source  "Show Source"  (kbd "v"))

(defun sly-make-action-button (label action &rest props)
  (apply #'make-text-button
         label nil :type 'sly-action
         'action action
         'mouse-action action
         props)
  label)

(define-button-type 'sly-button)

(define-button-type 'sly-action :supertype 'sly-button
  'face 'sly-inspector-action-face
  'mouse-face 'highlight)

(define-button-type 'sly-part :supertype 'sly-button
  'face 'sly-inspectable-value-face
  'action 'sly-button-inspect
  'mouse-action 'sly-button-inspect
  'keymap  sly-part-button-keymap
  'part-menu-keymap sly-button-popup-part-menu-keymap
  'help-echo "RET, mouse-2: Inspect object; mouse-3: Context menu"
  ;; these are ajust here for clarity
  ;; 
  'sly-button-inspect nil
  'sly-button-describe nil
  'sly-button-pretty-print nil
  'sly-button-show-source nil)

(provide 'sly-buttons)

;;; sly-buttons.el ends here
