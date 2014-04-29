(require 'sly)
(require 'advice)
(require 'sly-compiler-notes-tree) ; FIXME: actually only uses the tree bits, so that should be a library.

(define-sly-contrib sly-references
  "Clickable references to documentation (SBCL only)."
  (:authors "Christophe Rhodes  <csr21@cantab.net>"
            "Luke Gorrie  <luke@bluetail.com>"
            "Tobias C. Rittweiler <tcr@freebits.de>")
  (:license "GPL")
  (:on-load
   (ad-enable-advice 'sly-note.message 'after 'sly-note.message+references)
   (ad-activate 'sly-note.message)
   (setq sly-tree-printer 'sly-tree-print-with-references)
   (add-hook 'sldb-extras-hooks 'sldb-maybe-insert-references))
  (:on-unload
   (ad-disable-advice 'sly-note.message 'after 'sly-note.message+references)
   (ad-deactivate 'sly-note.message)
   (setq sly-tree-printer 'sly-tree-default-printer)
   (remove-hook 'sldb-extras-hooks 'sldb-maybe-insert-references)))

(defcustom sly-sbcl-manual-root "http://www.sbcl.org/manual/"
  "*The base URL of the SBCL manual, for documentation lookup."
  :type 'string
  :group 'sly-mode)

(defface sldb-reference-face 
  (list (list t '(:underline t)))
  "Face for references."
  :group 'sly-debugger)


;;;;; SBCL-style references 

(defvar sly-references-local-keymap
  (let ((map (make-sparse-keymap "local keymap for sly references")))
    (define-key map [mouse-2] 'sly-lookup-reference-at-mouse)
    (define-key map [return] 'sly-lookup-reference-at-point)
    map))

(defun sly-reference-properties (reference)
  "Return the properties for a reference.
Only add clickability to properties we actually know how to lookup."
  (cl-destructuring-bind (where type what) reference
    (if (or (and (eq where :sbcl) (eq type :node))
            (and (eq where :ansi-cl)
                 (memq type '(:function :special-operator :macro
                                        :type :system-class
                                        :section :glossary :issue))))
        `(sly-reference ,reference
                          font-lock-face sldb-reference-face
                          follow-link t
                          mouse-face highlight
                          help-echo "mouse-2: visit documentation."
                          keymap ,sly-references-local-keymap))))

(defun sly-insert-reference (reference)
  "Insert documentation reference from a condition.
See SWANK-BACKEND:CONDITION-REFERENCES for the datatype."
  (cl-destructuring-bind (where type what) reference
    (insert "\n" (sly-format-reference-source where) ", ")
    (sly-insert-propertized (sly-reference-properties reference)
                              (sly-format-reference-node what))
    (insert (format " [%s]" type))))

(defun sly-insert-references (references)
  (when references
    (insert "\nSee also:")
    (sly-with-rigid-indentation 2
      (mapc #'sly-insert-reference references))))

(defun sly-format-reference-source (where)
  (cl-case where
    (:amop    "The Art of the Metaobject Protocol")
    (:ansi-cl "Common Lisp Hyperspec")
    (:sbcl    "SBCL Manual")
    (t        (format "%S" where))))

(defun sly-format-reference-node (what)
  (if (listp what)
      (mapconcat #'prin1-to-string what ".")
    what))

(defun sly-lookup-reference-at-point ()
  "Browse the documentation reference at point."
  (interactive)
  (let ((refs (get-text-property (point) 'sly-reference)))
    (if (null refs)
        (error "No references at point")
      (cl-destructuring-bind (where type what) refs
        (cl-case where
          (:ansi-cl
           (cl-case type
             (:section
              (browse-url (funcall common-lisp-hyperspec-section-fun what)))
             (:glossary
              (browse-url (funcall common-lisp-glossary-fun what)))
             (:issue
              (browse-url (funcall 'common-lisp-issuex what)))
             (t
              (hyperspec-lookup what))))
          (t
           (let ((url (format "%s%s.html" sly-sbcl-manual-root
                              (subst-char-in-string ?\  ?\- what))))
             (browse-url url))))))))

(defun sly-lookup-reference-at-mouse (event)
  "Invoke the action pointed at by the mouse."
  (interactive "e")
  (cl-destructuring-bind (mouse-1 (w pos . _) . _) event
    (save-excursion
      (goto-char pos)
      (sly-lookup-reference-at-point))))

;;;;; Hook into *SLY COMPILATION*

(defun sly-note.references (note)
  (plist-get note :references))

;;; FIXME: `compilation-mode' will swallow the `mouse-face'
;;; etc. properties.
(defadvice sly-note.message (after sly-note.message+references)
  (setq ad-return-value 
        (concat ad-return-value
                (with-temp-buffer
                  (sly-insert-references 
                   (sly-note.references (ad-get-arg 0)))
                  (buffer-string)))))

;;;;; Hook into sly-compiler-notes-tree

(defun sly-tree-print-with-references (tree)
  ;; for SBCL-style references
  (sly-tree-default-printer tree)
  (when-let (note (plist-get (sly-tree.plist tree) 'note))
    (when-let (references (sly-note.references note))
      (terpri (current-buffer))
      (sly-insert-references references))))

;;;;; Hook into SLDB

(defun sldb-maybe-insert-references (extra)
  (destructure-case extra
    ((:references references) (sly-insert-references references) t)
    (t nil)))

(provide 'sly-references)
