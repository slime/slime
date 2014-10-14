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
        ((and (listp sexp)
	      (car sexp))
	 (cons (sly-retro-slynk-to-swank (car sexp))
	       (sly-retro-slynk-to-swank (cdr sexp))))
        (t
         sexp)))

(provide 'sly-retro)
