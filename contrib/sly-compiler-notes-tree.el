(require 'sly)
(require 'cl-lib)

(define-sly-contrib sly-compiler-notes-tree
  "Display compiler messages in tree layout.

M-x sly-list-compiler-notes display the compiler notes in a tree
grouped by severity.

  `sly-maybe-list-compiler-notes' can be used as
  `sly-compilation-finished-hook'.
"
  (:authors "Helmut Eller <heller@common-lisp.net>")
  (:license "GPL"))

(defun sly-maybe-list-compiler-notes (notes)
  "Show the compiler notes if appropriate."
  ;; don't pop up a buffer if all notes are already annotated in the
  ;; buffer itself
  (unless (cl-every #'sly-note-has-location-p notes)
    (sly-list-compiler-notes notes)))

(defun sly-list-compiler-notes (notes)
  "Show the compiler notes NOTES in tree view."
  (interactive (list (sly-compiler-notes)))
  (with-temp-message "Preparing compiler note tree..."
    (sly-with-popup-buffer ((sly-buffer-name :notes)
                              :mode 'sly-compiler-notes-mode)
      (when (null notes)
        (insert "[no notes]"))
      (let ((collapsed-p))
        (dolist (tree (sly-compiler-notes-to-tree notes))
          (when (sly-tree.collapsed-p tree) (setf collapsed-p t))
          (sly-tree-insert tree "")
          (insert "\n"))
        (goto-char (point-min))))))

(defvar sly-tree-printer 'sly-tree-default-printer)

(defun sly-tree-for-note (note)
  (make-sly-tree :item (sly-note.message note)
                   :plist (list 'note note)
                   :print-fn sly-tree-printer))

(defun sly-tree-for-severity (severity notes collapsed-p)
  (make-sly-tree :item (format "%s (%d)" 
                                 (sly-severity-label severity)
                                 (length notes))
                   :kids (mapcar #'sly-tree-for-note notes)
                   :collapsed-p collapsed-p))

(defun sly-compiler-notes-to-tree (notes)
  (let* ((alist (sly-alistify notes #'sly-note.severity #'eq))
         (collapsed-p (sly-length> alist 1)))
    (cl-loop for (severity . notes) in alist
             collect (sly-tree-for-severity severity notes 
                                              collapsed-p))))

(defvar sly-compiler-notes-mode-map)

(define-derived-mode sly-compiler-notes-mode fundamental-mode 
  "Compiler-Notes"
  "\\<sly-compiler-notes-mode-map>\
\\{sly-compiler-notes-mode-map}
\\{sly-popup-buffer-mode-map}
"
  (sly-set-truncate-lines))

(sly-define-keys sly-compiler-notes-mode-map
  ((kbd "RET") 'sly-compiler-notes-default-action-or-show-details)
  ([return] 'sly-compiler-notes-default-action-or-show-details)
  ([mouse-2] 'sly-compiler-notes-default-action-or-show-details/mouse))

(defun sly-compiler-notes-default-action-or-show-details/mouse (event)
  "Invoke the action pointed at by the mouse, or show details."
  (interactive "e")
  (cl-destructuring-bind (mouse-2 (w pos &rest _) &rest __) event
    (save-excursion
      (goto-char pos)
      (let ((fn (get-text-property (point) 
                                   'sly-compiler-notes-default-action)))
	(if fn (funcall fn) (sly-compiler-notes-show-details))))))

(defun sly-compiler-notes-default-action-or-show-details ()
  "Invoke the action at point, or show details."
  (interactive)
  (let ((fn (get-text-property (point) 'sly-compiler-notes-default-action)))
    (if fn (funcall fn) (sly-compiler-notes-show-details))))

(defun sly-compiler-notes-show-details ()
  (interactive)
  (let* ((tree (sly-tree-at-point))
         (note (plist-get (sly-tree.plist tree) 'note))
         (inhibit-read-only t))
    (cond ((not (sly-tree-leaf-p tree))
           (sly-tree-toggle tree))
          (t
           (sly-show-source-location (sly-note.location note) t)))))


;;;;;; Tree Widget

(cl-defstruct (sly-tree (:conc-name sly-tree.))
  item
  (print-fn #'sly-tree-default-printer :type function)
  (kids '() :type list)
  (collapsed-p t :type boolean)
  (prefix "" :type string)
  (start-mark nil)
  (end-mark nil)
  (plist '() :type list))

(defun sly-tree-leaf-p (tree)
  (not (sly-tree.kids tree)))

(defun sly-tree-default-printer (tree)
  (princ (sly-tree.item tree) (current-buffer)))

(defun sly-tree-decoration (tree)
  (cond ((sly-tree-leaf-p tree) "-- ")
	((sly-tree.collapsed-p tree) "[+] ")
	(t "-+  ")))

(defun sly-tree-insert-list (list prefix)
  "Insert a list of trees."
  (cl-loop for (elt . rest) on list 
           do (cond (rest
                     (insert prefix " |")
                     (sly-tree-insert elt (concat prefix " |"))
                     (insert "\n"))
                    (t
                     (insert prefix " `")
                     (sly-tree-insert elt (concat prefix "  "))))))

(defun sly-tree-insert-decoration (tree)
  (insert (sly-tree-decoration tree)))

(defun sly-tree-indent-item (start end prefix)
  "Insert PREFIX at the beginning of each but the first line.
This is used for labels spanning multiple lines."
  (save-excursion
    (goto-char end)
    (beginning-of-line)
    (while (< start (point))
      (insert-before-markers prefix)
      (forward-line -1))))

(defun sly-tree-insert (tree prefix)
  "Insert TREE prefixed with PREFIX at point."
  (with-struct (sly-tree. print-fn kids collapsed-p start-mark end-mark) tree
    (let ((line-start (line-beginning-position)))
      (setf start-mark (point-marker))
      (sly-tree-insert-decoration tree)
      (funcall print-fn tree)
      (sly-tree-indent-item start-mark (point) (concat prefix "   "))
      (add-text-properties line-start (point) (list 'sly-tree tree))
      (set-marker-insertion-type start-mark t)
      (when (and kids (not collapsed-p))
        (terpri (current-buffer))
        (sly-tree-insert-list kids prefix))
      (setf (sly-tree.prefix tree) prefix)
      (setf end-mark (point-marker)))))

(defun sly-tree-at-point ()
  (cond ((get-text-property (point) 'sly-tree))
        (t (error "No tree at point"))))

(defun sly-tree-delete (tree)
  "Delete the region for TREE."
  (delete-region (sly-tree.start-mark tree)
                 (sly-tree.end-mark tree)))

(defun sly-tree-toggle (tree)
  "Toggle the visibility of TREE's children."
  (with-struct (sly-tree. collapsed-p start-mark end-mark prefix) tree
    (setf collapsed-p (not collapsed-p))
    (sly-tree-delete tree)
    (insert-before-markers " ") ; move parent's end-mark
    (backward-char 1)
    (sly-tree-insert tree prefix)
    (delete-char 1)
    (goto-char start-mark)))

(provide 'sly-compiler-notes-tree)
