(require 'sly)
(require 'comint)

(setq sly-comint-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [return] 'sly-comint-return)
    (define-key map (kbd "RET") 'sly-comint-return)
    map))

(define-derived-mode sly-comint-mode comint-mode "REPL"
  "Provide a SLY REPL"
  :group 'slime
  :syntax-table lisp-mode-syntax-table
  (setq comint-prompt-regexp "^[A-Z[:punct:]]+> ")
  (setq comint-input-sender 'sly-comint--input-sender)
  (unless (comint-check-proc (current-buffer))
    (when (zerop (buffer-size))
      (insert (format "; SLY %s\n" (sly-version))))
    (start-process "sly-comint-process" (current-buffer) nil)
    (set-process-query-on-exit-flag (sly-comint--process) nil)
    (add-hook 'kill-buffer-hook 'sly-comint--delete-process nil 'local)
    (goto-char (point-max))
    (set (make-local-variable 'comint-inhibit-carriage-motion) t)
    (comint-output-filter (sly-comint--process) (sly-comint--prompt))
    (set-process-filter (sly-comint--process) 'comint-output-filter)))

(sly-def-connection-var sly-comint--repl-buffer nil
  "The buffer for the REPL for the current connection.  May be nil or a dead buffer.")

(defun sly-comint--buffer (&optional noprompt)
  "Return the output buffer, create it if necessary."
  (let ((buffer (sly-comint--repl-buffer)))
    (or (if (buffer-live-p buffer) buffer)
        (setf (sly-comint--repl-buffer)
              (let ((connection (sly-connection)))
                (with-current-buffer (get-buffer-create (format "*SLY-COMINT-REPL %s*" (sly-connection-name connection)))
                  (sly-comint-mode)
                  (setq sly-buffer-connection connection)
		  (setq sly-buffer-package (sly-current-package))
                  (current-buffer)))))))


(defun sly-comint-repl ()
  (interactive)
  (pop-to-buffer (sly-comint--buffer))
  (goto-char (point-max)))

(defun sly-comint--process () (get-buffer-process (current-buffer)))

(defun sly-comint--delete-process () (delete-process (sly-comint--process)))

(defun sly-comint--prompt () "SLY-COMINT> ")

(defun sly-comint--process-mark () (process-mark (sly-comint--process)))

(defvar sly-comint--input nil)

(defun sly-comint--input-sender (_proc input)
  ;; Just sets the variable sly-comint--input, which is in the scope of
  ;; `sly-comint--send-input's call.
  (setq sly-comint--input
        (with-temp-buffer
          (insert input)
          (buffer-substring-no-properties (point-min) (point-max)))))

(defvar sly-comint-auto-right-margin t)

(defun sly-comint--send-input ()
  "Evaluate the Emacs Lisp expression after the prompt."
  (interactive)
  (let (sly-comint--input)                     ; set by sly-comint--input-sender
    (comint-send-input)                 ; update history, markers etc.
    (goto-char (sly-comint--process-mark))
    (sly-rex ()
        (`(swank:listener-eval ,sly-comint--input
                               ,@(when sly-comint-auto-right-margin
                                   `(:window-width ,(window-width))))
         (sly-current-package))
      ((:ok result)
       (message "shit went ok!")
       ;; (comint-output-filter (sly-comint--process) result)
       )
      ((:abort condition)
       (with-current-buffer (sly-comint--buffer)
         (comint-output-filter (sly-comint--process)
                               (format "; Evaluation aborted on %s.\n"
                                       condition))
         (comint-output-filter (sly-comint--process) (sly-comint--prompt)))))))

(defcustom sly-comint-dynamic-return t
  "Control behaviour of \\<sly-comint-mode-map>\\[sly-comint-return] in \
the SLY REPL.

If non-nil, \\[sly-comint-return] evaluates input for complete
sexps, or inserts a newline and indents for incomplete sexps.  If
nil, always inserts newlines."
  :type 'boolean
  :group 'sly-comint)

(defcustom sly-comint-dynamic-multiline-inputs t
  "Force multiline inputs to start from column zero?

If non-nil, after entering the first line of an incomplete sexp, a newline
will be inserted after the prompt, moving the input to the next line.
This gives more frame width for large indented sexps."
  :type 'boolean
  :group 'sly-comint)

(defun sly-comint--newline ()
  (let ((last-input-event ?\n)) (self-insert-command 1)))

(defun sly-comint-return ()
  "Newline and indent, or evaluate the sexps before the prompt.

Complete sexps are evaluated; for incomplete sexps inserts a newline
and indents.

If however `sly-comint-dynamic-return' is nil, this always
simply inserts a newline."
  (interactive)
  (if sly-comint-dynamic-return
      (let ((state
             (save-excursion
               (end-of-line)
               (parse-partial-sexp (sly-comint--process-mark)
                                   (point)))))
        (if (and (< (car state) 1) (not (nth 3 state)))
            (sly-comint--send-input)
          (when (and sly-comint-dynamic-multiline-inputs
                     (save-excursion
                       (beginning-of-line)
                       (looking-at-p comint-prompt-regexp)))
            (save-excursion
              (goto-char (sly-comint--process-mark))
              (newline 1)))
          (sly-comint--newline)))
    (sly-comint--newline)))

(defun sly-comint--event-hook-function (event)
  (destructure-case event
    ((:write-string output &optional target)
     (with-current-buffer (sly-comint--buffer)
       (comint-output-filter (sly-comint--process) output)
       (comint-output-filter (sly-comint--process) (sly-comint--prompt)))
     t)
    ((:read-string thread tag)
     (assert thread)
     (error "unimplmemented!")
     ;; (sly-repl-read-string thread tag)
     t)
    ((:read-aborted thread tag)
     (error "unimplemented!")
     ;; (sly-repl-abort-read thread tag)
     t)
    ((:open-dedicated-output-stream port coding-system)
     (error "unimplemented!")
     ;; (sly-open-stream-to-lisp port coding-system)
     t)
    ((:new-package package prompt-string)
     (error "unimplemented!")
     ;; (setf (sly-lisp-package) package)
     ;; (setf (sly-lisp-package-prompt-string) prompt-string)
     ;; (let ((buffer (sly-comint--repl-buffer)))
     ;;   (when (buffer-live-p buffer)
     ;;     (with-current-buffer buffer
     ;;       (setq sly-buffer-package package))))
     t)
    (t nil)))

(add-hook 'sly-event-hooks 'sly-comint--event-hook-function)



