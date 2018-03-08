;;; -*- lisp -*-

(setf swank-loader:*source-directory*
      (asdf:system-source-directory :swank))

(setf swank::*find-module* 'swank::find-module-asdf)
