;;; slime-fancy-inspector.el --- Fancy inspector for CLOS objects
;;
;; Author: Marco Baringer <mb@bese.it> and others
;; License: GNU GPL (same license as Emacs)
;;

(defun slime-fancy-inspector-init ()
  (slime-require :swank-fancy-inspector))

(provide 'slime-fancy-inspector)