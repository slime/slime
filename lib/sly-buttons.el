;;; sly-buttons.el --- Button-related utils for SLY
;;;
(require 'cl-lib)

(defvar sly-part-button-keymap
      (let ((map (make-sparse-keymap)))
        (set-keymap-parent map button-map)
        (define-key map [down-mouse-3] 'sly-button-popup-part-menu)
        (define-key map [mouse-3] 'sly-button-popup-part-menu)
        map))

(defvar sly-button-popup-part-menu-keymap
  (let ((map (make-sparse-keymap)))
    map))

(defun sly-button-popup-part-menu (event)
  "Popup a menu for a `sly-part' button"
  (interactive "@e")
  (let* ((button (button-at (posn-point (event-end event))))
         (label (button-get button 'part-label))
         (items (cdr (button-get button 'part-menu-keymap))))
    (popup-menu
     `(keymap
       ,@(when label
           `(,(truncate-string-to-width label 30 nil nil t)))
       ,@items))))

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
     (define-key sly-part-button-keymap ,key
       '(menu-item "" ,action
                   :filter (lambda (cmd)
                             (let ((button (sly-button-at)))
                               (and button
                                    (button-get button ',action)
                                    cmd)))))
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

(defface sly-action-face
  `((t (:inherit warning)))
  "Face for SLY buttons."
  :group 'sly)

(define-button-type 'sly-button)

(define-button-type 'sly-action :supertype 'sly-button
  'face 'sly-action-face
  'mouse-face 'highlight)

(defface sly-part-button-face
  '((t (:inherit font-lock-constant-face)))
  "Face for things which be interactively inspected, etc"
  :group 'sly)

(define-button-type 'sly-part :supertype 'sly-button
  'face 'sly-part-button-face
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
