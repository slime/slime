(defpackage :slynk-retro
  (:use :cl :slynk :slynk-api))

(in-package :slynk-retro)

(defun ensure-slynk-package-nicknames (&rest ignored)
  "Nickname all SLYNK-* package to SWANK-*"
  (declare (ignore ignored))
  (loop for package in (list-all-packages)
      for package-name = (package-name package)
      when (search "SLYNK" package-name :test #'char-equal)
        do (rename-package package
                           package-name
                           (remove-duplicates
                            (cons
                             (format nil "SWANK~a"
                                     (subseq package-name 5))
                             (package-nicknames package))
                            :test #'string-equal))))

(setq slynk-rpc:*translating-swank-to-slynk* nil)
(push #'ensure-slynk-package-nicknames
      slynk-api:*slynk-require-hook*)

(ensure-slynk-package-nicknames)

(provide :slynk-retro)


