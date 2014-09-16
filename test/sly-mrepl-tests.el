(require 'sly-mrepl)
(require 'sly-tests "lib/sly-tests")
(require 'cl-lib)


(defun sly-mrepl-tests--assert-prompt ()
  (let ((proper-prompt-p nil))
    (cl-loop 
     repeat 5
     when (string-match "CL-USER> *$" (buffer-string))
     do (setq proper-prompt-p t)
     (cl-return)
     do (sleep-for 0 300))
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

(define-sly-ert-test basic-repl-setup ()
  (with-current-buffer (sly-mrepl-new (sly-current-connection))
    (sly-mrepl-tests--assert-prompt)
    (sly-mrepl-tests--assert-dedicated-stream)
    (kill-buffer (current-buffer))))


(provide 'sly-mrepl-tests)
