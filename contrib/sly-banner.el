(require 'sly)
(require 'sly-repl)

(define-sly-contrib sly-banner
  "Persistent header line and startup animation."
  (:authors "Helmut Eller <heller@common-lisp.net>"
            "Luke Gorrie  <luke@synap.se>")
  (:license "GPL")
  (:on-load   (setq sly-repl-banner-function 'sly-startup-message))
  (:on-unload (setq sly-repl-banner-function 'sly-repl-insert-banner)))

(defcustom sly-startup-animation (fboundp 'animate-string)
   "Enable the startup animation."
   :type '(choice (const :tag "Enable" t) (const :tag "Disable" nil))
   :group 'sly-ui)

(defcustom sly-header-line-p (boundp 'header-line-format)
  "If non-nil, display a header line in Slime buffers."
  :type 'boolean
  :group 'sly-repl)

(defun sly-startup-message ()
  (when sly-header-line-p
    (setq header-line-format 
          (format "%s  Port: %s  Pid: %s"
                  (sly-lisp-implementation-type)
                  (sly-connection-port (sly-connection))
                  (sly-pid))))
  (when (zerop (buffer-size))
    (let ((welcome (concat "; SLY " (sly-version))))
      (if sly-startup-animation
          (animate-string welcome 0 0) 
        (insert welcome)))))

(provide 'sly-banner)
