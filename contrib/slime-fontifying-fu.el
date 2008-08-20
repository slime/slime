;;; slime-fontifying-fu.el --- Additional fontification tweaks.
;;
;; Author:  Tobias C. Rittweiler <tcr@freebits.de>
;;
;; License: GNU GPL (same license as Emacs)
;;


;; Fontify WITH-FOO and DO-FOO like standard macros; fontify
;; CHECK-FOO like CHECK-TYPE.
(defvar slime-additional-font-lock-keywords
 '(("(\\(\\(\\s_\\|\\w\\)*:\\(define-\\|do-\\|with-\\)\\(\\s_\\|\\w\\)*\\)" 1 font-lock-keyword-face) 
   ("(\\(\\(define-\\|do-\\|with-\\)\\(\\s_\\|\\w\\)*\\)" 1 font-lock-keyword-face)
   ("(\\(check-\\(\\s_\\|\\w\\)*\\)" 1 font-lock-warning-face)))

(defun slime-fontifying-fu-init ()
  (font-lock-add-keywords
   'lisp-mode slime-additional-font-lock-keywords))

(defun slime-fontifying-fu-unload ()
  (font-lock-remove-keywords 
   'lisp-mode slime-additional-font-lock-keywords))

(provide 'slime-fontifying-fu)