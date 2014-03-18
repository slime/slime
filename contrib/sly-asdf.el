(require 'sly)
(require 'cl-lib)
(require 'grep)

(define-sly-contrib sly-asdf
  "ASDF support."
  (:authors "Daniel Barlow       <dan@telent.net>"
            "Marco Baringer      <mb@bese.it>"
            "Edi Weitz           <edi@agharta.de>"
            "Stas Boukarev       <stassats@gmail.com>"
            "Tobias C Rittweiler <tcr@freebits.de>")
  (:license "GPL")
  (:sly-dependencies sly-old-repl)
  (:swank-dependencies swank-asdf)
  (:on-load
   (add-to-list 'sly-edit-uses-xrefs :depends-on t)
   (define-key sly-who-map [?d] 'sly-who-depends-on)))

;;; NOTE: `system-name' is a predefined variable in Emacs.  Try to
;;; avoid it as local variable name.

;;; Utilities

(defgroup sly-asdf nil
  "ASDF support for Slime."
  :prefix "sly-asdf-"
  :group 'sly)

(defvar sly-system-history nil
  "History list for ASDF system names.")

(defun sly-read-system-name (&optional prompt 
                                         default-value
                                         determine-default-accurately)
  "Read a system name from the minibuffer, prompting with PROMPT.
If no `default-value' is given, one is tried to be determined: if
`determine-default-accurately' is true, by an RPC request which
grovels through all defined systems; if it's not true, by looking
in the directory of the current buffer."
  (let* ((completion-ignore-case nil)
         (prompt (or prompt "System"))
         (system-names (sly-eval `(swank:list-asdf-systems)))
         (default-value
           (or default-value 
               (if determine-default-accurately
                   (sly-determine-asdf-system (buffer-file-name)
                                                (sly-current-package))
                   (sly-find-asd-file (or default-directory
                                            (buffer-file-name))
                                        system-names))))
         (prompt (concat prompt (if default-value
                                    (format " (default `%s'): " default-value)
                                    ": "))))
    (completing-read prompt (sly-bogus-completion-alist system-names)
                     nil nil nil
                     'sly-system-history default-value)))



(defun sly-find-asd-file (directory system-names)
  "Tries to find an ASDF system definition file in the
`directory' and returns it if it's in `system-names'."
  (let ((asd-files
         (directory-files (file-name-directory directory) nil "\.asd$")))
    (loop for system in asd-files
          for candidate = (file-name-sans-extension system)
          when (cl-find candidate system-names :test #'string-equal)
            do (return candidate))))

(defun sly-determine-asdf-system (filename buffer-package)
  "Try to determine the asdf system that `filename' belongs to."
  (sly-eval
   `(swank:asdf-determine-system ,(and filename
                                       (sly-to-lisp-filename filename))
                                 ,buffer-package)))

(defun sly-who-depends-on-rpc (system)
  (sly-eval `(swank:who-depends-on ,system)))

(defcustom sly-asdf-collect-notes t
  "Collect and display notes produced by the compiler.

See also `sly-highlight-compiler-notes' and
`sly-compilation-finished-hook'."
  :group 'sly-asdf)

(defun sly-asdf-operation-finished-function (system)
  (if sly-asdf-collect-notes
      #'sly-compilation-finished
      (lexical-let ((system system))
        (lambda (result)
          (let (sly-highlight-compiler-notes
                sly-compilation-finished-hook)
            (sly-compilation-finished result))))))

(defun sly-oos (system operation &rest keyword-args)
  "Operate On System."
  (sly-save-some-lisp-buffers)
  (sly-display-output-buffer)
  (message "Performing ASDF %S%s on system %S"
           operation (if keyword-args (format " %S" keyword-args) "")
           system)
  (sly-repl-shortcut-eval-async
   `(swank:operate-on-system-for-emacs ,system ',operation ,@keyword-args)
   (sly-asdf-operation-finished-function system)))


;;; Interactive functions

(defun sly-load-system (&optional system)
  "Compile and load an ASDF system.  

Default system name is taken from first file matching *.asd in current
buffer's working directory"
  (interactive (list (sly-read-system-name)))
  (sly-oos system 'load-op))

(defun sly-open-system (name &optional load interactive)
  "Open all files in an ASDF system."
  (interactive (list (sly-read-system-name) nil t))
  (when (or load
            (and interactive
                 (not (sly-eval `(swank:asdf-system-loaded-p ,name)))
                 (y-or-n-p "Load it? ")))
    (sly-load-system name))
  (sly-eval-async
   `(swank:asdf-system-files ,name)
   (lambda (files)
     (when files
       (let ((files (mapcar 'sly-from-lisp-filename
                            (nreverse files))))
         (find-file-other-window (car files))
         (mapc 'find-file (cdr files)))))))

(defun sly-browse-system (name)
  "Browse files in an ASDF system using Dired."
  (interactive (list (sly-read-system-name)))
  (sly-eval-async `(swank:asdf-system-directory ,name)
   (lambda (directory)
     (when directory
       (dired (sly-from-lisp-filename directory))))))

(defun sly-rgrep-system (sys-name regexp)
  "Run `rgrep' on the base directory of an ASDF system."
  (interactive (progn (grep-compute-defaults)
                      (list (sly-read-system-name nil nil t)
                            (grep-read-regexp))))
  (rgrep regexp "*.lisp"
         (sly-from-lisp-filename
          (sly-eval `(swank:asdf-system-directory ,sys-name)))))

(defun sly-isearch-system (sys-name)
  "Run `isearch-forward' on the files of an ASDF system."
  (interactive (list (sly-read-system-name nil nil t)))
  (let* ((files (mapcar 'sly-from-lisp-filename
                        (sly-eval `(swank:asdf-system-files ,sys-name))))
         (multi-isearch-next-buffer-function
          (lexical-let* 
              ((buffers-forward  (mapcar #'find-file-noselect files))
               (buffers-backward (reverse buffers-forward)))
            #'(lambda (current-buffer wrap)
                ;; Contrarily to the docstring of
                ;; `multi-isearch-next-buffer-function', the first
                ;; arg is not necessarily a buffer. Report sent
                ;; upstream. (2009-11-17)
                (setq current-buffer (or current-buffer (current-buffer)))
                (let* ((buffers (if isearch-forward
                                    buffers-forward
                                  buffers-backward)))
                  (if wrap
                      (car buffers)
                    (second (memq current-buffer buffers))))))))
    (isearch-forward)))

(defun sly-read-query-replace-args (format-string &rest format-args)
  (let* ((minibuffer-setup-hook (sly-minibuffer-setup-hook))
         (minibuffer-local-map sly-minibuffer-map)
         (common (query-replace-read-args (apply #'format format-string
                                                 format-args)
                                          t t)))
    (list (nth 0 common) (nth 1 common) (nth 2 common))))

(defun sly-query-replace-system (name from to &optional delimited)
  "Run `query-replace' on an ASDF system."
  (interactive (let ((system (sly-read-system-name nil nil t)))
                 (cons system (sly-read-query-replace-args
                               "Query replace throughout `%s'" system))))
  (condition-case c
      ;; `tags-query-replace' actually uses `query-replace-regexp'
      ;; internally.
      (tags-query-replace (regexp-quote from) to delimited
                          '(mapcar 'sly-from-lisp-filename
                            (sly-eval `(swank:asdf-system-files ,name))))
    (error
     ;; Kludge: `tags-query-replace' does not actually return but
     ;; signals an unnamed error with the below error
     ;; message. (<=23.1.2, at least.)
     (unless (string-equal (error-message-string c) "All files processed")
       (signal (car c) (cdr c)))        ; resignal
     t)))

(defun sly-query-replace-system-and-dependents
    (name from to &optional delimited)
  "Run `query-replace' on an ASDF system and all the systems
depending on it."
  (interactive (let ((system (sly-read-system-name nil nil t)))
                 (cons system (sly-read-query-replace-args
                               "Query replace throughout `%s'+dependencies"
                               system))))
  (sly-query-replace-system name from to delimited)
  (dolist (dep (sly-who-depends-on-rpc name))
    (when (y-or-n-p (format "Descend into system `%s'? " dep))
      (sly-query-replace-system dep from to delimited))))

(defun sly-delete-system-fasls (name)
  "Delete FASLs produced by compiling a system."
  (interactive (list (sly-read-system-name)))
  (sly-repl-shortcut-eval-async
   `(swank:delete-system-fasls ,name)
   'message))

(defun sly-reload-system (system)
  "Reload an ASDF system without reloading its dependencies."
  (interactive (list (sly-read-system-name)))
  (sly-save-some-lisp-buffers)
  (sly-display-output-buffer)
  (message "Performing ASDF LOAD-OP on system %S" system)
  (sly-repl-shortcut-eval-async
   `(swank:reload-system ,system)
   (sly-asdf-operation-finished-function system)))

(defun sly-who-depends-on (system-name)
  (interactive (list (sly-read-system-name)))
  (sly-xref :depends-on system-name))

(defun sly-save-system (system)
  "Save files belonging to an ASDF system."
  (interactive (list (sly-read-system-name)))
  (sly-eval-async
      `(swank:asdf-system-files ,system)
    (lambda (files)
      (dolist (file files)
        (let ((buffer (get-file-buffer (sly-from-lisp-filename file))))
          (when buffer
            (with-current-buffer buffer
              (save-buffer buffer)))))
      (message "Done."))))


;;; REPL shortcuts

(defsly-repl-shortcut sly-repl-load/force-system ("force-load-system")
  (:handler (lambda ()
              (interactive)
              (sly-oos (sly-read-system-name) 'load-op :force t)))
  (:one-liner "Recompile and load an ASDF system."))

(defsly-repl-shortcut sly-repl-load-system ("load-system")
  (:handler (lambda ()
              (interactive)
              (sly-oos (sly-read-system-name) 'load-op)))
  (:one-liner "Compile (as needed) and load an ASDF system."))

(defsly-repl-shortcut sly-repl-test/force-system ("force-test-system")
  (:handler (lambda ()
              (interactive)
              (sly-oos (sly-read-system-name) 'test-op :force t)))
  (:one-liner "Recompile and test an ASDF system."))

(defsly-repl-shortcut sly-repl-test-system ("test-system")
  (:handler (lambda ()
              (interactive)
              (sly-oos (sly-read-system-name) 'test-op)))
  (:one-liner "Compile (as needed) and test an ASDF system."))

(defsly-repl-shortcut sly-repl-compile-system ("compile-system")
  (:handler (lambda ()
              (interactive)
              (sly-oos (sly-read-system-name) 'compile-op)))
  (:one-liner "Compile (but not load) an ASDF system."))

(defsly-repl-shortcut sly-repl-compile/force-system 
  ("force-compile-system")  
  (:handler (lambda ()
              (interactive)
              (sly-oos (sly-read-system-name) 'compile-op :force t)))
  (:one-liner "Recompile (but not completely load) an ASDF system."))

(defsly-repl-shortcut sly-repl-open-system ("open-system")
  (:handler 'sly-open-system)
  (:one-liner "Open all files in an ASDF system."))

(defsly-repl-shortcut sly-repl-browse-system ("browse-system")
  (:handler 'sly-browse-system)
  (:one-liner "Browse files in an ASDF system using Dired."))

(defsly-repl-shortcut sly-repl-delete-system-fasls ("delete-system-fasls")
  (:handler 'sly-delete-system-fasls)
  (:one-liner "Delete FASLs of an ASDF system."))

(defsly-repl-shortcut sly-repl-reload-system ("reload-system")
  (:handler 'sly-reload-system)
  (:one-liner "Recompile and load an ASDF system."))

(provide 'sly-asdf)
