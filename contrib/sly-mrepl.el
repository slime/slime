;; -*- lexical-binding: t -*-
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
  (:swank-dependencies swank-mrepl)
  (:on-load
   (define-key sly-inspector-mode-map (kbd "M-RET") 'sly-inspector-copy-down-to-repl)
   (define-key sldb-mode-map (kbd "M-RET") 'sldb-copy-down-to-repl)
   ;; FIXME: ugly
   (add-hook 'sly-trace-dialog-mode-hook
             #'(lambda ()
                 (local-set-key (kbd "M-RET") 'sly-trace-dialog-copy-down-to-repl)))))

(require 'comint)

(defvar sly-mrepl--remote-channel nil)
(defvar sly-mrepl--local-channel nil)
(defvar sly-mrepl--expect-sexp-mode t)
(defvar sly-mrepl--pending-requests nil)
(defvar sly-mrepl--result-counter -1)

(defvar sly-mrepl-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET")     'sly-mrepl-return)
    (define-key map [return]        'sly-mrepl-return)
    (define-key map (kbd "TAB")     'sly-indent-and-complete-symbol)
    (define-key map (kbd "C-c C-b") 'sly-interrupt)
    (define-key map (kbd "C-c C-c") 'sly-interrupt)
    map))

(define-derived-mode sly-mrepl-mode comint-mode "mrepl"
  (sly-mode)
  (set (make-local-variable 'comint-use-prompt-regexp) nil)
  (set (make-local-variable 'comint-inhibit-carriage-motion) t)
  (set (make-local-variable 'comint-input-sender) 'sly-mrepl--input-sender)
  (set (make-local-variable 'comint-output-filter-functions) nil)
  (set (make-local-variable 'comint-input-filter-functions) nil)
  (set (make-local-variable 'sly-mrepl--expect-sexp-mode) t)
  (set (make-local-variable 'sly-mrepl--result-counter) -1)
  
  ;;(set (make-local-variable 'comint-get-old-input) 'ielm-get-old-input)
  (set-syntax-table lisp-mode-syntax-table))

(defun sly-mrepl-new ()
  "Create a new listener window."
  (interactive)
  (let ((local (sly-make-channel sly-listener-channel-methods))
        (buffer (pop-to-buffer (generate-new-buffer (sly-buffer-name :mrepl)))))
    (with-current-buffer buffer
      (sly-mrepl-mode)
      (start-process (format "sly-mrepl-pty-ch-%s" (sly-channel.id local))
                     (current-buffer) nil)
      (set-process-query-on-exit-flag (sly-mrepl--process) nil)

      (setq header-line-format (format "Waiting for REPL creation ack for channel %d..."
                                       (sly-channel.id local)))
      (set (make-local-variable 'sly-mrepl--local-channel) local))
    (sly-eval-async
        `(swank-mrepl:create-mrepl ,(sly-channel.id local))
      (lambda (result)
        (cl-destructuring-bind (remote thread-id package prompt) result
          (with-current-buffer buffer
            (setq header-line-format (format "local=%d remote=%d thread=%d"
                                             (sly-channel.id local)
                                             remote
                                             thread-id))
            (add-hook 'kill-buffer-hook 'sly-mrepl--teardown nil 'local)
            (setq sly-current-thread thread-id)
            (setq sly-buffer-connection (sly-connection))
            (set (make-local-variable 'sly-mrepl--remote-channel) remote)
            (sly-channel-put local 'buffer (current-buffer))
            (sly-channel-send local `(:prompt ,package ,prompt))
            (sly-mrepl--send-pending)))))
    buffer))

(defun sly-mrepl--process () (get-buffer-process (current-buffer))) ;stupid

(defun sly-mrepl--mark () (process-mark (sly-mrepl--process)))

(defun sly-mrepl--teardown ()
  (sly-mrepl--send `(:teardown))
  (set (make-local-variable 'sly-mrepl--remote-channel) nil)
  (sly-close-channel sly-mrepl--local-channel)
  (delete-process (sly-mrepl--process)))

(defun sly-mrepl--insert (string)
  (comint-output-filter (sly-mrepl--process) string))

(sly-define-channel-type listener)

(sly-define-channel-method listener :prompt (package prompt)
  (with-current-buffer (sly-channel-get self 'buffer)
    (sly-mrepl--prompt package prompt)))

(defun sly-mrepl--prompt (package prompt)
  (setf sly-buffer-package package)
  (sly-mrepl--insert (format "%s%s> "
			      (cl-case (current-column)
				(0 "")
				(t "\n"))
			      prompt))
  (sly-mrepl--recenter))

(defun sly-mrepl--recenter ()
  (when (get-buffer-window)
    (recenter -1)))

(sly-define-channel-method listener :write-result (result)
  (with-current-buffer (sly-channel-get self 'buffer)
    (cl-incf sly-mrepl--result-counter)
    (let* ((button-action (lambda (_button)
                            (sly-mrepl--send `(:inspect ,sly-mrepl--result-counter))))
           (filter (lambda (output)
                     (if (string-match "^;" output)
                         output
                       (make-text-button output nil 'action button-action
                                         'font-lock-face 'sly-inspectable-value-face))))
           (comint-preoutput-filter-functions (list filter)))
      (sly-mrepl--insert result))))

(sly-define-channel-method listener :evaluation-aborted ()
  (with-current-buffer (sly-channel-get self 'buffer)
    (sly-mrepl--insert "; Evaluation aborted\n")))

(sly-define-channel-method listener :write-string (string)
  (sly-mrepl--write-string self string))

(sly-define-channel-method listener :inspect-result (parts)
  (sly-open-inspector parts))

(defun sly-mrepl--write-string (self string)
  (with-current-buffer (sly-channel-get self 'buffer)
    (sly-mrepl--insert string)))

(sly-define-channel-method listener :set-read-mode (mode)
  (with-current-buffer (sly-channel-get self 'buffer)
    (cl-ecase mode
      (:read (setq sly-mrepl--expect-sexp-mode nil)
	     (message "[Listener is waiting for input]"))
      (:eval (setq sly-mrepl--expect-sexp-mode t)))))

(defun sly-mrepl-return (&optional end-of-input)
  (interactive "P")
  (sly-check-connected)
  (goto-char (point-max))
  (cond ((and sly-mrepl--expect-sexp-mode
	      (or (sly-input-complete-p (sly-mrepl--mark) (point))
		  end-of-input))
	 (comint-send-input))
	((not sly-mrepl--expect-sexp-mode)
	 (unless end-of-input
	   (insert "\n"))
	 (comint-send-input t))
        (t
	 (insert "\n")
	 (inferior-sly-indent-line)
         (message "[input not complete]")))
  (sly-mrepl--recenter))

(defun sly-mrepl--input-sender (_proc string)
  (sly-mrepl--send-string (substring-no-properties string)))

(defun sly-mrepl--send-string (string &optional _command-string)
  (sly-mrepl--send `(:process ,string)))

(defun sly-mrepl--send (msg)
  "Send MSG to the remote channel.

If message can't be sent right now, queue it onto
`sly-mrepl--pending-requests'"
  (if sly-mrepl--remote-channel
      (sly-send-to-remote-channel sly-mrepl--remote-channel msg)
    (add-to-list (make-local-variable 'sly-mrepl--pending-requests)
                 msg 'append)))

(defun sly-mrepl--send-pending ()
  "Send pending requests from `sly-mrepl--pending-requests'."
  (mapc #'sly-mrepl--send sly-mrepl--pending-requests)
  (setq sly-mrepl--pending-requests nil))

(defun sly-mrepl (&optional interactive)
  (interactive (list t))
  (let ((buffer
         (or (cl-find-if (lambda (x)
                           (with-current-buffer x
                             (and (eq major-mode 'sly-mrepl-mode)
                                  (eq (sly-current-connection)
                                      (sly-connection)))))
                         (buffer-list))
             (sly-mrepl-new))))
    (when interactive
      (pop-to-buffer buffer))
    buffer))


;;; copy-down-to-REPL behaviour
;;; 
(defun sly-inspector-copy-down-to-repl (number)
  "Evaluate the inspector slot at point via the REPL (to set `*')."
  (interactive (list (or (get-text-property (point) 'sly-part-number)
                         (error "No part at point"))))
  (with-current-buffer (sly-mrepl)
    (sly-mrepl--insert "\n")
    (sly-mrepl--send-string
     (format "%s" `(cl:nth-value 0 (swank:inspector-nth-part ,number))))
    (pop-to-buffer (current-buffer))))

(defun sldb-copy-down-to-repl (frame-id var-id)
  "Evaluate the frame var at point via the REPL (to set `*')."
  (interactive (list (sldb-frame-number-at-point) (sldb-var-number-at-point)))
  (with-current-buffer (sly-mrepl)
    (sly-mrepl--insert "\n")
    (sly-mrepl--send-string (format "%s"
                                   `(swank-backend:frame-var-value
                                     ,frame-id ,var-id)))
    (pop-to-buffer (current-buffer))))

(defun sly-trace-dialog-copy-down-to-repl (id part-id type)
  "Eval the Trace Dialog entry under point in the REPL (to set *)"
  (interactive (cl-loop for prop in '(sly-trace-dialog--id
                                      sly-trace-dialog--part-id
                                      sly-trace-dialog--type)
                        collect (get-text-property (point) prop)))
  (unless (and id part-id type) (error "No trace part at point %s" (point)))
  (with-current-buffer (sly-mrepl)
    (sly-mrepl--insert "\n")
    (sly-mrepl--send-string
     (format "%s" `(nth-value 0
                              (swank-trace-dialog::find-trace-part
                               ,id ,part-id ,type))))
    (pop-to-buffer (current-buffer))))


(def-sly-selector-method ?m
  "First mrepl-buffer"
  (or (sly-mrepl)
      (error "No mrepl buffer (%s)" (sly-connection-name))))

(provide 'sly-mrepl)
