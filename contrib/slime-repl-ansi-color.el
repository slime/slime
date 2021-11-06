(require 'ansi-color)

(define-slime-contrib slime-repl-ansi-color
  "Turn on ANSI colors in REPL output"
  (:authors "Max Mikhanosha")
  (:license "GPL")
  (:slime-dependencies slime-repl)
  (:on-load
   (add-hook 'slime-repl-mode-hook
             (lambda ()
               (slime-repl-ansi-on)))))

(defvar slime-repl-ansi-color nil
  "When Non-NIL will process ANSI colors in the lisp output")

(make-variable-buffer-local 'slime-repl-ansi-color)

(defun slime-repl-ansi-on ()
  "Set `ansi-color-for-comint-mode' to t."
  (interactive)
  (setq slime-repl-ansi-color t))

(defun slime-repl-ansi-off ()
  "Set `ansi-color-for-comint-mode' to t."
  (interactive)
  (setq slime-repl-ansi-color nil))

(defadvice slime-repl-emit (around slime-repl-ansi-colorize activate compile)
  (with-current-buffer (slime-output-buffer)
    (let ((start slime-output-start))
      (setq ad-return-value ad-do-it)
      (when slime-repl-ansi-color
        (ansi-color-apply-on-region start slime-output-end)))))


(provide 'slime-repl-ansi-color)
