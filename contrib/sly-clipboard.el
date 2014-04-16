(require 'sly)
(require 'sly-old-repl)
(require 'cl-lib)
(eval-when-compile
  (require 'cl)) ; lexical-let

(define-sly-contrib sly-clipboard
  "This add a few commands to put objects into a clipboard and to
insert textual references to those objects.

The clipboard command prefix is C-c @.

 C-c @ +   adds an object to the clipboard
 C-c @ @   inserts a reference to an object in the clipboard
 C-c @ ?   displays the clipboard

This package also also binds the + key in the inspector and
debugger to add the object at point to the clipboard."
  (:authors "Helmut Eller  <heller@common-lisp.net>")
  (:license "GPL")
  (:swank-dependencies swank-clipboard))

(define-derived-mode sly-clipboard-mode fundamental-mode
  "Slime-Clipboard"
  "SLY Clipboad Mode.

\\{sly-clipboard-mode-map}")

(sly-define-keys sly-clipboard-mode-map
  ("g" 'sly-clipboard-redisplay)
  ((kbd "C-k") 'sly-clipboard-delete-entry)
  ("i" 'sly-clipboard-inspect))

(defvar sly-clipboard-map (make-sparse-keymap))

(sly-define-keys sly-clipboard-map
  ("?" 'sly-clipboard-display)
  ("+" 'sly-clipboard-add)
  ("@" 'sly-clipboard-ref))

(define-key sly-mode-map (kbd "C-c @") sly-clipboard-map)
(define-key sly-repl-mode-map (kbd "C-c @") sly-clipboard-map)

(sly-define-keys sly-inspector-mode-map
  ("+" 'sly-clipboard-add-from-inspector))

(sly-define-keys sldb-mode-map
  ("+" 'sly-clipboard-add-from-sldb))

(defun sly-clipboard-add (exp package)
  "Add an object to the clipboard."
  (interactive (list (sly-read-from-minibuffer 
                      "Add to clipboard (evaluated): "
                      (sly-sexp-at-point))
		     (sly-current-package)))
  (sly-clipboard-add-internal `(:string ,exp ,package)))

(defun sly-clipboard-add-internal (datum)
  (sly-eval-async `(swank-clipboard:add ',datum) 
		    (lambda (result) (message "%s" result))))

(defun sly-clipboard-display ()
  "Display the content of the clipboard."
  (interactive)
  (sly-eval-async `(swank-clipboard:entries) 
		    #'sly-clipboard-display-entries))

(defun sly-clipboard-display-entries (entries)
  (sly-with-popup-buffer ((sly-buffer-name :clipboard)
                            :mode 'sly-clipboard-mode)
    (sly-clipboard-insert-entries entries)))

(defun sly-clipboard-insert-entries (entries)
  (let ((fstring "%2s %3s %s\n"))
    (insert (format fstring "Nr" "Id" "Value")
            (format fstring "--" "--" "-----" ))
    (save-excursion
      (cl-loop
       for i from 0 for (ref . value) in entries do
       (sly-insert-propertized `(sly-clipboard-entry ,i
                                                     sly-clipboard-ref ,ref)
                               (format fstring i ref value))))))

(defun sly-clipboard-redisplay ()
  "Update the clipboard buffer."
  (interactive)
  (lexical-let ((saved (point)))
    (sly-eval-async 
        `(swank-clipboard:entries) 
      (lambda (entries) 
        (let ((inhibit-read-only t))
          (erase-buffer)
          (sly-clipboard-insert-entries entries)
          (when (< saved (point-max))
            (goto-char saved)))))))

(defun sly-clipboard-entry-at-point ()
  (or (get-text-property (point) 'sly-clipboard-entry)
      (error "No clipboard entry at point")))

(defun sly-clipboard-ref-at-point ()
  (or (get-text-property (point) 'sly-clipboard-ref)
      (error "No clipboard ref at point")))

(defun sly-clipboard-inspect (&optional entry)
  "Inspect the current clipboard entry."
  (interactive (list (sly-clipboard-ref-at-point)))
  (sly-inspect (prin1-to-string `(swank-clipboard::clipboard-ref ,entry))))

(defun sly-clipboard-delete-entry (&optional entry)
  "Delete the current entry from the clipboard."
  (interactive (list (sly-clipboard-entry-at-point)))
  (sly-eval-async `(swank-clipboard:delete-entry ,entry)
		    (lambda (result) 
		      (sly-clipboard-redisplay)
		      (message "%s" result))))

(defun sly-clipboard-ref ()
  "Ask for a clipboard entry number and insert a reference to it."
  (interactive)
  (sly-clipboard-read-entry-number #'sly-clipboard-insert-ref))
  
;; insert a reference to clipboard entry ENTRY at point.  The text
;; receives a special 'display property to make it look nicer.  We
;; remove this property in a modification when a user tries to modify
;; he real text.
(defun sly-clipboard-insert-ref (entry)
  (cl-destructuring-bind (ref . string) 
      (sly-eval `(swank-clipboard:entry-to-ref ,entry))
    (sly-insert-propertized
     `(display ,(format "#@%d%s" ref string)
	       modification-hooks (sly-clipboard-ref-modified)
	       rear-nonsticky t)
     (format "(swank-clipboard::clipboard-ref %d)" ref))))

(defun sly-clipboard-ref-modified (start end)
  (when (get-text-property start 'display)
    (let ((inhibit-modification-hooks t))
      (save-excursion
	(goto-char start)
	(cl-destructuring-bind (dstart dend) (sly-property-bounds 'display)
	  (unless (and (= start dstart) (= end dend))
	    (remove-list-of-text-properties 
	     dstart dend '(display modification-hooks))))))))

;; Read a entry number.
;; Written in CPS because the display the clipboard before reading.
(defun sly-clipboard-read-entry-number (k)
  (sly-eval-async 
   `(swank-clipboard:entries) 
   (sly-rcurry
    (lambda (entries window-config k)
      (sly-clipboard-display-entries entries)
      (let ((entry (unwind-protect
		       (read-from-minibuffer "Entry number: " nil nil t)
		     (set-window-configuration window-config))))
	(funcall k entry)))
    (current-window-configuration)
    k)))

(defun sly-clipboard-add-from-inspector ()
  (interactive)
  (let ((part (or (get-text-property (point) 'sly-part-number)
		  (error "No part at point"))))
    (sly-clipboard-add-internal `(:inspector ,part))))

(defun sly-clipboard-add-from-sldb ()
  (interactive)
  (sly-clipboard-add-internal 
   `(:sldb ,(sldb-frame-number-at-point) 
	   ,(sldb-var-number-at-point))))

(provide 'sly-clipboard)
