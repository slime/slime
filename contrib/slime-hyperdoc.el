
;;; TODO: `url-http-file-exists-p' is slow, make it optional behaviour.

(require 'url-http)
(require 'browse-url)

(defun slime-hyperdoc-lookup-rpc (symbol-name)
  (slime-eval-async `(swank:hyperdoc ,symbol-name)
    (lexical-let ((symbol-name symbol-name))
      #'(lambda (result)
          (slime-log-event result)
          (loop with foundp = nil
                for (doc-type . url) in result do
                (when (and url (stringp url)
                           (let ((url-show-status nil))
                             (url-http-file-exists-p url)))
                  (message "Visiting documentation for %s `%s'..."
                           (substring (symbol-name doc-type) 1)
                           symbol-name)
                  (browse-url url)
                  (setq foundp t))
                finally
                (unless foundp
                  (error "Could not find documentation for `%s'." 
                         symbol-name)))))))

(defun slime-hyperdoc-lookup (symbol-name)
  (interactive (list (slime-read-symbol-name "Symbol: ")))
  (if (memq :hyperdoc (slime-lisp-features))
      (slime-hyperdoc-lookup-rpc symbol-name)
      (slime-hyperspec-lookup symbol-name)))

(defvar slime-old-documentation-lookup-function 
  slime-documentation-lookup-function)

(defun slime-hyperdoc-init ()
  (slime-require :swank-hyperdoc)
  (setq slime-documentation-lookup-function 'slime-hyperdoc-lookup))

(defun slime-hyperdoc-unload ()
  (setq slime-documentation-lookup-function 
        slime-old-documentation-lookup-function))

(provide 'slime-hyperdoc)