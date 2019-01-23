(eval-and-compile
  (require 'slime))

(define-slime-contrib slime-buffer-streams
  "Lisp streams that output to an emacs buffer"
  (:authors "Ed Langley <el-github@elangley.org>")
  (:license "GPL")
  (:on-load
   (slime-buffer-streams-add-hooks))
  (:on-unload
   (slime-buffer-streams-remove-hooks))
  (:swank-dependencies swank-buffer-streams))

(defun slime-buffer-streams-add-hooks ()
  (add-hook 'slime-event-hooks 'slime-buffer-streams-event-hook-function)
  (add-hook 'slime-connected-hook 'slime-buffer-streams-connected-hook-function))

(defun slime-buffer-streams-remove-hooks ()
  (remove-hook 'slime-event-hooks 'slime-buffer-streams-event-hook-function)
  (remove-hook 'slime-connected-hook 'slime-buffer-streams-connected-hook-function))

(defun slime-buffer-streams-connected-hook-function ()
  (slime-eval '(swank-buffer-streams:initialize-buffer-stream-hooks)))

(defun slime-buffer-streams-event-hook-function (event)
  (slime-dcase event
    ((:make-stream-target thread name)
     (message "making target %s" name)
     (slime-buffer-streams--get-target-marker name)
     (slime-send `(:stream-target-created ,thread ,name))
     t)
    ((:stream-target-created &rest _) t)
    (t
     nil)))

(defun slime-buffer-streams--get-target-name (target)
  (format "*slime-target %s*" target))

(make-variable-buffer-local
 (defvar slime-buffer-stream-target nil))

;; TODO: tell backend that the buffer has been closed, so it can close
;;       the stream
(defun slime-buffer-streams--cleanup-markers ()
  (when slime-buffer-stream-target
    (message "Removing target: %s" slime-buffer-stream-target)
    (remhash slime-buffer-stream-target slime-output-target-to-marker)))

(defun slime-buffer-streams--get-target-marker (target)
  (or (gethash target slime-output-target-to-marker)
      (with-current-buffer
          (generate-new-buffer (slime-buffer-streams--get-target-name target))
        (setq slime-buffer-stream-target target)
        (add-hook 'kill-buffer-hook 'slime-buffer-streams--cleanup-markers)
        (setf (gethash target slime-output-target-to-marker)
              (point-marker)))))



(provide 'slime-buffer-streams)
