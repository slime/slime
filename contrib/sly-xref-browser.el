(eval-and-compile
  (require 'sly))

(define-sly-contrib sly-xref-browser
  "Xref browsing with tree-widget"
  (:authors "Rui Patrocínio <rui.patrocinio@netvisao.pt>")
  (:license "GPL"))


;;;; classes browser

(defun sly-expand-class-node (widget)
  (or (widget-get widget :args)
      (let ((name (widget-get widget :tag)))
	(cl-loop for kid in (sly-eval `(swank:mop :subclasses ,name))
                 collect `(tree-widget :tag ,kid
                                       :expander sly-expand-class-node
                                       :has-children t)))))

(defun sly-browse-classes (name)
  "Read the name of a class and show its subclasses."
  (interactive (list (sly-read-symbol-name "Class Name: ")))
  (sly-call-with-browser-setup 
   (sly-buffer-name :browser) (sly-current-package) "Class Browser"
   (lambda ()
     (widget-create 'tree-widget :tag name 
                    :expander 'sly-expand-class-node 
                    :has-echildren t))))

(defvar sly-browser-map nil
  "Keymap for tree widget browsers")

(require 'tree-widget)
(unless sly-browser-map
  (setq sly-browser-map (make-sparse-keymap))
  (set-keymap-parent sly-browser-map widget-keymap)
  (define-key sly-browser-map "q" 'bury-buffer))

(defun sly-call-with-browser-setup (buffer package title fn)
  (switch-to-buffer buffer)
  (kill-all-local-variables)
  (setq sly-buffer-package package)
  (let ((inhibit-read-only t)) (erase-buffer))
  (widget-insert title "\n\n")
  (save-excursion
    (funcall fn))
  (lisp-mode-variables t)
  (sly-mode t)
  (use-local-map sly-browser-map)
  (widget-setup))


;;;; Xref browser

(defun sly-fetch-browsable-xrefs (type name)
  "Return a list ((LABEL DSPEC)).
LABEL is just a string for display purposes. 
DSPEC can be used to expand the node."
  (let ((xrefs '()))
    (cl-loop for (_file . specs) in (sly-eval `(swank:xref ,type ,name)) do
             (cl-loop for (dspec . _location) in specs do
                      (let ((exp (ignore-errors (read (downcase dspec)))))
                        (cond ((and (consp exp) (eq 'flet (car exp)))
                               ;; we can't expand FLET references so they're useless
                               )
                              ((and (consp exp) (eq 'method (car exp)))
                               ;; this isn't quite right, but good enough for now
                               (push (list dspec (string (cl-second exp))) xrefs))
                              (t
                               (push (list dspec dspec) xrefs))))))
    xrefs))

(defun sly-expand-xrefs (widget)
  (or (widget-get widget :args)
      (let* ((type (widget-get widget :xref-type))
             (dspec (widget-get widget :xref-dspec))
             (xrefs (sly-fetch-browsable-xrefs type dspec)))
        (cl-loop for (label dspec) in xrefs
                 collect `(tree-widget :tag ,label
                                       :xref-type ,type
                                       :xref-dspec ,dspec
                                       :expander sly-expand-xrefs
                                       :has-children t)))))

(defun sly-browse-xrefs (name type)
  "Show the xref graph of a function in a tree widget."
  (interactive 
   (list (sly-read-from-minibuffer "Name: "
                                     (sly-symbol-at-point))
         (read (completing-read "Type: " (sly-bogus-completion-alist
                                          '(":callers" ":callees" ":calls"))
                                nil t ":"))))
  (sly-call-with-browser-setup 
   (sly-buffer-name :xref) (sly-current-package) "Xref Browser"
   (lambda ()
     (widget-create 'tree-widget :tag name :xref-type type :xref-dspec name 
                    :expander 'sly-expand-xrefs :has-echildren t))))

(provide 'sly-xref-browser)
