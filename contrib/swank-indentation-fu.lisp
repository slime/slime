
(in-package :swank)

(defslimefun arglist-indentation (arglist)
  (with-buffer-syntax ()
    (macro-indentation (from-string arglist))))

(provide :swank-indentation-fu)
