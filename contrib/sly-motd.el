;;; sly-motd.el --- 
;;
;; Authors: 
;;
;; License: GNU GPL (same license as Emacs)
;;
;;; Installation
;;
;; Add sly-motd to your sly-setup call.
(require 'sly)
(require 'sly-old-repl)

(define-sly-contrib sly-motd
  "Message Of The Day in a sly repl"
  (:authors "Marco Baringer <mb@bese.it>")
  (:license "GPL")
  (:sly-dependencies sly-banner)
  (:swank-dependencies swank-motd)
  (:on-load
   (add-hook 'sly-connected-hook 'sly-insert-motd)))

(defcustom sly-motd-pathname nil
  "The local pathname the motd is read from."
  :group 'sly-mode
  :type '(file :must-match t))

(defun sly-insert-motd ()
  (sly-eval-async `(swank::read-motd ,sly-motd-pathname)
                    (lambda (motd)
                      (when motd
                        (sly-repl-insert-result (list :values motd))))))

(provide 'sly-motd)
