(require 'sly)

(define-sly-contrib sly-retro
  "Enable SLIME to connect to a SLY-started SLYNK"
  (:slynk-dependencies slynk-retro)
  (:on-load (setq sly-net-send-translator #'sly-retro-slynk-to-swank))
  (:on-unload (setq sly-net-send-translator nil)))

(defun sly-retro-slynk-to-swank (sexp)
  (cond ((and (symbolp sexp)
              (string-match "^slynk\\(.*\\)$" (symbol-name sexp)))
         (intern (format "swank%s" (match-string 1 (symbol-name sexp)))))
        ((listp sexp)
         (mapcar #'sly-retro-slynk-to-swank sexp))
        (t
         sexp)))

(provide 'sly-retro)
