(require 'sly)
(require 'url-http)
(require 'browse-url)
(eval-when-compile (require 'cl)) ; lexical-let

(defvar sly-old-documentation-lookup-function 
  sly-documentation-lookup-function)

(define-sly-contrib sly-hyperdoc
  "Extensible C-c C-d h."
  (:authors "Tobias C Rittweiler <tcr@freebits.de>")
  (:license "GPL")
  (:swank-dependencies swank-hyperdoc)
  (:on-load
   (setq sly-documentation-lookup-function 'sly-hyperdoc-lookup))
  (:on-unload
   (setq sly-documentation-lookup-function 
         sly-old-documentation-lookup-function)))

;;; TODO: `url-http-file-exists-p' is slow, make it optional behaviour.

(defun sly-hyperdoc-lookup-rpc (symbol-name)
  (sly-eval-async `(swank:hyperdoc ,symbol-name)
    (lexical-let ((symbol-name symbol-name))
      #'(lambda (result)
          (sly-log-event result)
          (cl-loop with foundp = nil
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

(defun sly-hyperdoc-lookup (symbol-name)
  (interactive (list (sly-read-symbol-name "Symbol: ")))
  (if (memq :hyperdoc (sly-lisp-features))
      (sly-hyperdoc-lookup-rpc symbol-name)
      (sly-hyperspec-lookup symbol-name)))

(provide 'sly-hyperdoc)
