;;; slime-package-fu.el --- Misc extensions for SBCL
;;
;; Author:  Tobias C. Rittweiler <tcr@freebits.de>
;;
;; License: GNU GPL (same license as Emacs)
;;

(require 'slime-autodoc)
(require 'slime-references)

(defun slime-enable-autodoc-for-sb-assem:inst ()
  (push '("INST" . (slime-make-extended-operator-parser/look-ahead 1))
        slime-extended-operator-name-parser-alist))

(defun slime-sbcl-exts-init ()
  (slime-enable-autodoc-for-sb-assem:inst))

(slime-require :swank-sbcl-exts)

(provide 'slime-sbcl-exts)