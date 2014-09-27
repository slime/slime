(require 'sly-mrepl)
(require 'sly-tests "lib/sly-tests")
(require 'cl-lib)


(cl-defun sly-mrepl-tests--assert-prompt (&optional (prompt "CL-USER>"))
  (let ((proper-prompt-p nil))
    (cl-loop 
     repeat 5
     when (looking-back (format "%s $" prompt))
     do (setq proper-prompt-p t)
     (cl-return)
     do (sit-for 0.3))
    (or proper-prompt-p
        (ert-fail (format "Proper prompt not seen in time (saw last 20 chars as \"%s\")"
                          (buffer-substring-no-properties (max (point-min)
                                                               (- (point-max)
                                                                  20))
                                                          (point-max)))))))

(defun sly-mrepl-tests--assert-dedicated-stream ()
  (let ((dedicated-stream nil))
    (cl-loop 
     repeat 5
     when (and sly-mrepl--dedicated-stream
               (processp sly-mrepl--dedicated-stream)
               (process-live-p sly-mrepl--dedicated-stream))
     do (setq dedicated-stream t)
     (cl-return)
     do (sleep-for 0 300))
    (or dedicated-stream
        (ert-fail "Dedicated stream not setup correctly"))))

(defvar sly-mrepl-tests--debug nil)
(setq sly-mrepl-tests--debug nil)

(defmacro sly-mrepl-tests--with-basic-repl-setup (&rest body)
  (declare (debug (&rest form)))
  `(with-current-buffer (sly-mrepl-new (sly-current-connection)
                                       "test-only-repl")
     (unwind-protect
         (progn
           (sly-mrepl-tests--assert-prompt)
           (sly-mrepl-tests--assert-dedicated-stream)
           ,@body)
       (unless sly-mrepl-tests--debug
         (kill-buffer (current-buffer))))))

(define-sly-ert-test basic-repl-setup ()
  (sly-mrepl-tests--with-basic-repl-setup))

(define-sly-ert-test repl-values-and-button-navigation ()
  (sly-mrepl-tests--with-basic-repl-setup
   (insert "(values (list 1 2 3) #(1 2 3))")
   (sly-mrepl-return)
   (sly-mrepl-tests--assert-prompt)
   (call-interactively 'sly-button-backward)
   (call-interactively 'sly-button-backward)
   (should-error
    (call-interactively 'sly-button-backward))
   (call-interactively 'sly-button-forward)))


(provide 'sly-mrepl-tests)
