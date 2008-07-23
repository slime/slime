
(in-package :swank)

(defslimefun package= (string1 string2)
  (let* ((pkg1 (guess-package string1))
	 (pkg2 (guess-package string2)))
    (and pkg1 pkg2 (eq pkg1 pkg2))))

(defslimefun export-symbol-for-emacs (symbol-str package-str)
  (let ((package (guess-package package-str)))
    (when package
      (let ((*buffer-package* package))
	(export `(,(from-string symbol-str)) package)))))

(defslimefun unexport-symbol-for-emacs (symbol-str package-str)
  (let ((package (guess-package package-str)))
    (when package
      (let ((*buffer-package* package))
	(unexport `(,(from-string symbol-str)) package)))))



(provide :swank-package-fu)