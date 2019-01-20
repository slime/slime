(eval-and-compile
  (require 'slime))

(define-slime-contrib slime-output-streams
  "Lisp streams that output to an emacs buffer"
  (:authors "Ed Langley <el-github@elangley.org>")
  (:license "GPL")
  (:on-load
   (slime-output-streams-add-hooks))
  (:on-unload
   (slime-output-streams-remove-hooks))
  (:swank-dependencies swank-output-streams))

(defun slime-output-streams-add-hooks ()
  (add-hook 'slime-event-hooks 'slime-output-streams-event-hook-function)
  (add-hook 'slime-connected-hook 'slime-output-streams-connected-hook-function))

(defun slime-output-streams-remove-hooks ()
  (remove-hook 'slime-event-hooks 'slime-output-streams-event-hook-function)
  (remove-hook 'slime-connected-hook 'slime-output-streams-connected-hook-function))

(defun slime-output-streams-event-hook-function (event)
  (slime-dcase event
    ((:make-target thread name)
     (message "making target %s" name)
     (slime-output-streams--get-target-marker name)
     (slime-send `(:target-created ,thread ,name))
     t)
    ((:target-created &rest _) t)
    (t
     nil)))

(defun slime-output-streams-connected-hook-function ()
  (slime-eval '(swank-output-streams:initialize-output-stream-hooks)))
(defun slime-output-streams-disconnected-hook-function ()
  (slime-eval '(swank-output-streams:deinitialize-output-stream-hooks)))

(cl-defmacro s-o-s--return-when (condition &body else)
  (let ((cond-sym (gensym)))
    `(let ((,cond-sym ,condition))
       (if ,cond-sym ,cond-sym
         ,@else))))

(defun slime-output-streams--get-target-name (target)
  (format "*slime-target %s*" target))

(defun slime-output-streams--get-target-marker (target)
  (s-o-s--return-when (gethash target slime-output-target-to-marker)
                      (with-current-buffer
                          (generate-new-buffer (slime-output-streams--get-target-name target))
                        (setf (gethash target slime-output-target-to-marker)
                              (point-marker)))))

(comment
 (setf (gethash target slime-output-target-to-marker)
       (with-current-buffer buffer
         (point-marker))))


(provide 'slime-output-streams)
