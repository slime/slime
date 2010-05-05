;;; slime-mrepl.el --- Multiple REPLs
;;
;; An experimental implementation of multiple REPLs multiplexed over a
;; single Slime socket.  M-x slime-open-listener creates a new REPL
;; buffer.
;;
;; Some copy&pasting from slime-repl.el

(require 'slime-repl)

(slime-define-channel-type listener)

(slime-define-channel-method listener :prompt (package prompt)
  (with-current-buffer (slime-channel-get self 'buffer)
    (setf slime-buffer-package package)
    (letf (((slime-lisp-package-prompt-string) prompt))
      (slime-repl-insert-prompt))))

(slime-define-channel-method listener :write-result (result)
  (letf (((slime-connection-output-buffer) (slime-channel-get self 'buffer)))
    (slime-repl-emit-result result t)))

(slime-define-channel-method listener :evaluation-aborted (package prompt)
  (with-current-buffer (slime-channel-get self 'buffer)
    (setq slime-buffer-package package)
    (letf (((slime-connection-output-buffer) (current-buffer))
	   ((slime-lisp-package-prompt-string) prompt))
      (slime-repl-show-abort))))

(slime-define-channel-method listener :write-string (string)
  (slime-mrepl-write-string self string))

(defun slime-mrepl-write-string (self string)
  (letf (((slime-connection-output-buffer) (slime-channel-get self 'buffer)))
    (slime-repl-emit string)))

(byte-compile 'slime-mrepl-write-string)

(slime-define-channel-method listener :read-string (thread tag)
  (letf (((slime-connection-output-buffer) (slime-channel-get self 'buffer)))
    (slime-repl-read-string thread tag)))

(define-derived-mode slime-mrepl-mode slime-repl-mode "mrepl")

(slime-define-keys slime-mrepl-mode-map
  ((kbd "RET") 'slime-mrepl-return)
  ([return] 'slime-mrepl-return))

(defun slime-mrepl-return (&optional end-of-input)
  "Evaluate the current input string, or insert a newline.  
Send the current input ony if a whole expression has been entered,
i.e. the parenthesis are matched. 

With prefix argument send the input even if the parenthesis are not
balanced."
  (interactive "P")
  (slime-check-connected)
  (cond (end-of-input
         (slime-mrepl-send-input))
        (slime-repl-read-mode ; bad style?
         (slime-mrepl-send-input t))
        ((and (get-text-property (point) 'slime-repl-old-input)
              (< (point) slime-repl-input-start-mark))
         (slime-repl-grab-old-input end-of-input)
         (slime-repl-recenter-if-needed))
        ((slime-input-complete-p slime-repl-input-start-mark (point-max))
         (slime-mrepl-send-input t))
        (t 
         (slime-repl-newline-and-indent)
         (message "[input not complete]"))))

(defun slime-mrepl-send-input (&optional newline)
  "Goto to the end of the input and send the current input.
If NEWLINE is true then add a newline at the end of the input."
  (unless (slime-repl-in-input-area-p)
    (error "No input at point."))
  (goto-char (point-max))
  (let ((end (point))) ; end of input, without the newline
    (slime-repl-add-to-input-history 
     (buffer-substring slime-repl-input-start-mark end))
    (when newline 
      (insert "\n")
      (slime-repl-show-maximum-output))
    (let ((inhibit-modification-hooks t))
      (add-text-properties slime-repl-input-start-mark 
                           (point)
                           `(slime-repl-old-input
                             ,(incf slime-repl-old-input-counter))))
    (let ((overlay (make-overlay slime-repl-input-start-mark end)))
      ;; These properties are on an overlay so that they won't be taken
      ;; by kill/yank.
      (overlay-put overlay 'read-only t)
      (overlay-put overlay 'face 'slime-repl-input-face)))
  (let ((input (slime-repl-current-input)))
    (goto-char (point-max))
    (slime-mark-input-start)
    (slime-mark-output-start)
    (slime-mrepl-send-string input)))

(defun slime-mrepl-send-string (string &optional command-string)
  (cond (slime-repl-read-mode
         (slime-repl-return-string string))
        (t (slime-mrepl-send `(:eval ,string)))))

(defun slime-mrepl-send (msg)
  "Send MSG to the remote channel."
  (slime-send-to-remote-channel slime-mrepl-remote-channel msg))

(defun slime-open-listener ()
  "Create a new listener window."
  (interactive)
  (let ((channel (slime-make-channel slime-listener-channel-methods)))
    (slime-eval-async
     `(swank:create-listener ,(slime-channel.id channel))
     (slime-rcurry 
      (lambda (result channel)
	(destructuring-bind (remote thread-id package prompt) result
	  (pop-to-buffer (generate-new-buffer (slime-buffer-name :listener)))
	  (slime-mrepl-mode)
	  (setq slime-current-thread thread-id)
	  (setq slime-buffer-connection (slime-connection))
	  (set (make-local-variable 'slime-mrepl-remote-channel) remote)
	  (slime-channel-put channel 'buffer (current-buffer))
	  (slime-reset-repl-markers)
	  (slime-channel-send channel `(:prompt ,package ,prompt))
	  (slime-repl-show-maximum-output)))
      channel))))

(provide 'slime-mrepl)
