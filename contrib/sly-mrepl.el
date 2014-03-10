;; An experimental implementation of multiple REPLs multiplexed over a
;; single Slime socket.  M-x sly-open-listener creates a new REPL
;; buffer.
;;
(require 'sly)
(require 'inferior-sly) ; inferior-sly-indent-lime
(require 'cl-lib)

(define-sly-contrib sly-mrepl
  "Multiple REPLs."
  (:authors "Helmut Eller <heller@common-lisp.net>")
  (:license "GPL")
  (:swank-dependencies swank-mrepl))

(require 'comint)

(defvar sly-mrepl-remote-channel nil)
(defvar sly-mrepl-expect-sexp nil)

(define-derived-mode sly-mrepl-mode comint-mode "mrepl"
  ;; idea lifted from ielm
  (unless (get-buffer-process (current-buffer))
    (let* ((process-connection-type nil)
	   (proc (start-process "mrepl (dummy)" (current-buffer) "hexl")))
      (set-process-query-on-exit-flag proc nil)))
  (set (make-local-variable 'comint-use-prompt-regexp) nil)
  (set (make-local-variable 'comint-inhibit-carriage-motion) t)
  (set (make-local-variable 'comint-input-sender) 'sly-mrepl-input-sender)
  (set (make-local-variable 'comint-output-filter-functions) nil)
  (set (make-local-variable 'sly-mrepl-expect-sexp) t)
  ;;(set (make-local-variable 'comint-get-old-input) 'ielm-get-old-input)
  (set-syntax-table lisp-mode-syntax-table)
  )

(sly-define-keys sly-mrepl-mode-map
  ((kbd "RET") 'sly-mrepl-return)
  ([return] 'sly-mrepl-return)
  ((kbd "TAB") 'sly-indent-and-complete-symbol)
  ((kbd "C-c C-b") 'sly-interrupt)
  ((kbd "C-c C-c") 'sly-interrupt))

(defun sly-mrepl-process% () (get-buffer-process (current-buffer))) ;stupid
(defun sly-mrepl-mark () (process-mark (sly-mrepl-process%)))

(defun sly-mrepl-insert (string)
  (comint-output-filter (sly-mrepl-process%) string))

(sly-define-channel-type listener)

(sly-define-channel-method listener :prompt (package prompt)
  (with-current-buffer (sly-channel-get self 'buffer)
    (sly-mrepl-prompt package prompt)))

(defun sly-mrepl-prompt (package prompt)
  (setf sly-buffer-package package)
  (sly-mrepl-insert (format "%s%s> "
			      (cl-case (current-column)
				(0 "")
				(t "\n"))
			      prompt))
  (sly-mrepl-recenter))

(defun sly-mrepl-recenter ()
  (when (get-buffer-window)
    (recenter -1)))

(sly-define-channel-method listener :write-result (result)
  (with-current-buffer (sly-channel-get self 'buffer)
    (goto-char (point-max))
    (sly-mrepl-insert result)))

(sly-define-channel-method listener :evaluation-aborted ()
  (with-current-buffer (sly-channel-get self 'buffer)
    (goto-char (point-max))
    (sly-mrepl-insert "; Evaluation aborted\n")))

(sly-define-channel-method listener :write-string (string)
  (sly-mrepl-write-string self string))

(defun sly-mrepl-write-string (self string)
  (with-current-buffer (sly-channel-get self 'buffer)
    (goto-char (sly-mrepl-mark))
    (sly-mrepl-insert string)))

(sly-define-channel-method listener :set-read-mode (mode)
  (with-current-buffer (sly-channel-get self 'buffer)
    (cl-ecase mode
      (:read (setq sly-mrepl-expect-sexp nil)
	     (message "[Listener is waiting for input]"))
      (:eval (setq sly-mrepl-expect-sexp t)))))

(defun sly-mrepl-return (&optional end-of-input)
  (interactive "P")
  (sly-check-connected)
  (goto-char (point-max))
  (cond ((and sly-mrepl-expect-sexp
	      (or (sly-input-complete-p (sly-mrepl-mark) (point))
		  end-of-input))
	 (comint-send-input))
	((not sly-mrepl-expect-sexp)
	 (unless end-of-input
	   (insert "\n"))
	 (comint-send-input t))
        (t
	 (insert "\n")
	 (inferior-sly-indent-line)
         (message "[input not complete]")))
  (sly-mrepl-recenter))

(defun sly-mrepl-input-sender (proc string)
  (sly-mrepl-send-string (substring-no-properties string)))

(defun sly-mrepl-send-string (string &optional command-string)
  (sly-mrepl-send `(:process ,string)))

(defun sly-mrepl-send (msg)
  "Send MSG to the remote channel."
  (sly-send-to-remote-channel sly-mrepl-remote-channel msg))

(defun sly-new-mrepl ()
  "Create a new listener window."
  (interactive)
  (let ((channel (sly-make-channel sly-listener-channel-methods)))
    (sly-eval-async
        `(swank-mrepl:create-mrepl ,(sly-channel.id channel))
      (sly-rcurry 
       (lambda (result channel)
         (cl-destructuring-bind (remote thread-id package prompt) result
           (pop-to-buffer (generate-new-buffer (sly-buffer-name :mrepl)))
           (sly-mrepl-mode)
           (setq sly-current-thread thread-id)
           (setq sly-buffer-connection (sly-connection))
           (set (make-local-variable 'sly-mrepl-remote-channel) remote)
           (sly-channel-put channel 'buffer (current-buffer))
           (sly-channel-send channel `(:prompt ,package ,prompt))))
       channel))))

(defun sly-mrepl ()
  (let ((conn (sly-connection)))
    (cl-find-if (lambda (x) 
	       (with-current-buffer x 
		 (and (eq major-mode 'sly-mrepl-mode)
		      (eq (sly-current-connection) conn))))
	     (buffer-list))))

(def-sly-selector-method ?m
  "First mrepl-buffer"
  (or (sly-mrepl)
      (error "No mrepl buffer (%s)" (sly-connection-name))))

(provide 'sly-mrepl)
