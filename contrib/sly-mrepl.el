;; -*- lexical-binding: t -*- An experimental implementation of
;; multiple REPLs multiplexed over a single Slime socket.  M-x
;; sly-mrepl or M-x sly-mrepl-new create new REPL buffers.
;;
(require 'sly)
(require 'cl-lib)

(define-sly-contrib sly-mrepl
  "Multiple REPLs."
  (:swank-dependencies swank-mrepl)
  (:on-load
   ;; Define a new "part action" for the `sly-part' buttons and change
   ;; the `sly-inspector-part', `sldb-local-variable' and
   ;; `sly-trace-dialog-part' to include it.
   ;; 
   (sly-button-define-part-action sly-mrepl-copy-to-repl "Copy to REPL" (kbd "M-RET"))
   (button-type-put 'sly-inspector-part
                    'sly-mrepl-copy-to-repl
                    'sly-inspector-copy-down-to-repl)
   (button-type-put 'sldb-local-variable
                    'sly-mrepl-copy-to-repl
                    'sldb-copy-down-to-repl)
   (eval-after-load 'sly-trace-dialog
     `(progn
        (button-type-put 'sly-trace-dialog-part
                         'sly-mrepl-copy-to-repl
                         'sly-trace-dialog-copy-down-to-repl)))
   ;; Make C-c ~ bring popup REPL
   ;;
   (define-key sly-editing-mode-map (kbd "C-c ~") 'sly-mrepl-sync-package-and-default-directory)
   ;; Insinuate ourselves in hooks
   ;;
   (add-hook 'sly-connected-hook 'sly-mrepl-pop-to-mrepl)
   (add-hook 'sly-net-process-close-hooks 'sly-mrepl--teardown-repls)
   ;; The connection list is also tweaked
   ;;
   (setq sly-connection-list-button-action
         #'(lambda (process)
             (let ((sly-default-connection process))
               (sly-mrepl 'interactive))))))

(require 'comint)

(defvar sly-mrepl-hook nil
  "Functions run after `sly-mrepl-new' sets up a REPL.")

(defvar sly-mrepl-runonce-hook nil
  "Functions run once after `sly-mrepl-new' sets up a REPL.

After running the contents of this hook its default value is
emptied. See also `sly-mrepl-hook'")

(defvar sly-mrepl--remote-channel nil)
(defvar sly-mrepl--local-channel nil)
(defvar sly-mrepl--expect-sexp-mode t)
(defvar sly-mrepl--result-counter -1)
(defvar sly-mrepl--output-mark nil)
(defvar sly-mrepl--dedicated-stream nil)
 ;; perhaps could be replaced by a afield
(defvar sly-mrepl--last-prompt-overlay nil)

(defvar sly-mrepl-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET")     'sly-mrepl-return)
    (define-key map [return]        'sly-mrepl-return)
    (define-key map (kbd "TAB")     'sly-indent-and-complete-symbol)
    (define-key map (kbd "C-c C-b") 'sly-interrupt)
    (define-key map (kbd "C-c C-c") 'sly-interrupt)
    (define-key map (kbd "M-p")     'comint-previous-input)
    (define-key map (kbd "M-n")     'comint-next-input)
    map))

(define-derived-mode sly-mrepl-mode comint-mode "mrepl"
  (sly-mode)
  (font-lock-mode -1)
  (cl-loop for (var value)
           in `((comint-use-prompt-regexp nil)
                (comint-inhibit-carriage-motion t)
                (comint-input-sender sly-mrepl--input-sender)
                (comint-output-filter-functions nil)
                (comint-input-filter-functions nil)
                (comint-history-isearch dwim)
                (comint-input-ring-file-name "~/.sly-mrepl-history")
                (comint-input-ignoredups t)
                (comint-input-ring-size  1500)
                (comint-prompt-read-only t)
                (indent-line-function lisp-indent-line)
                (sly-mrepl--expect-sexp-mode t)
                (sly-mrepl--result-counter -1)
                (sly-mrepl--output-mark ,(point-marker))
                (sly-mrepl--last-prompt-overlay ,(make-overlay 0 0))
                (sly-find-buffer-package-function sly-mrepl-guess-package)
                (sly-autodoc-inhibit-autodoc sly-mrepl-inside-string-or-comment-p))
           do (set (make-local-variable var) value))
  (set-marker-insertion-type sly-mrepl--output-mark nil)
  ;;(set (make-local-variable 'comint-get-old-input) 'ielm-get-old-input)
  (set-syntax-table lisp-mode-syntax-table)
  (set-keymap-parent sly-mrepl-mode-map nil))

(sly-define-channel-type listener)

(defun sly-mrepl--buffer-name (connection remote-id local-id )
  (format "*sly-mrepl-%s-%s %s*"
          (or remote-id "?")
          local-id
          (sly-connection-name connection)))

(defun sly-mrepl--commit-buffer-name ()
  (let* ((final-name (sly-mrepl--buffer-name
                      sly-buffer-connection
                      sly-mrepl--remote-channel
                      (sly-channel.id sly-mrepl--local-channel)))
         (existing (get-buffer final-name)))
    (when existing
      (warn "Trampling over existing sly-mrepl %s, sorry" existing)
      (kill-buffer existing))
    (rename-buffer final-name)))

(defun sly-mrepl-new ()
  "Create a new listener window."
  (interactive)
  (let* ((local (sly-make-channel sly-listener-channel-methods))
         ;; FIXME: Notice a subtle bug/feature than when invoking
         ;; sly-mrepl-new in a buffer which has a connection, but not
         ;; the default connection, the new REPL will be for that
         ;; connection.
         (connection (sly-connection))
         (buffer (pop-to-buffer
                  (generate-new-buffer
                   (sly-mrepl--buffer-name connection
                                           nil
                                           (sly-channel.id local))))))
    (with-current-buffer buffer
      (sly-mrepl-mode)
      (setq sly-buffer-connection connection)
      (start-process (format "sly-pty-%s-%s"
                             (process-get connection
                                          'sly--net-connect-counter)
                             (sly-channel.id local))
                     (current-buffer)
                     nil) 
      (set-process-query-on-exit-flag (sly-mrepl--process) nil)
      (setq header-line-format (format "Waiting for REPL creation ack for channel %d..."
                                       (sly-channel.id local)))
      (sly-channel-put local 'buffer (current-buffer))
      (add-hook 'kill-buffer-hook 'sly-mrepl--teardown nil 'local)
      (set (make-local-variable 'sly-mrepl--local-channel) local))
    (sly-eval-async
        `(swank-mrepl:create-mrepl ,(sly-channel.id local))
      (lambda (result)
        (cl-destructuring-bind (remote thread-id) result
          (with-current-buffer buffer
            (sly-mrepl-read-input-ring)
            (setq header-line-format nil)
            (setq sly-current-thread thread-id)
            (set (make-local-variable 'sly-mrepl--remote-channel) remote)
            (sly-mrepl--commit-buffer-name)
            (unwind-protect
                (run-hooks 'sly-mrepl-hook 'sly-mrepl-runonce-hook)
              (set-default 'sly-mrepl-runonce-hook nil))))))
    buffer))

(defun sly-mrepl--teardown-repls (process)
  (cl-loop for buffer in (buffer-list)
           when (buffer-live-p buffer)
           do (with-current-buffer buffer
                (when (and (eq major-mode 'sly-mrepl-mode)
                           (eq sly-buffer-connection process))
                  (sly-mrepl--teardown (process-get process 'sly-net-close-reason))))))

(defun sly-mrepl--process () (get-buffer-process (current-buffer))) ;stupid

(defun sly-mrepl--mark () (process-mark (sly-mrepl--process)))

(defmacro sly-mrepl--commiting-text (props &rest body)
  (declare (debug (sexp &rest form))
           (indent 1))
  (let ((start-sym (cl-gensym)))
    `(let ((,start-sym (marker-position (sly-mrepl--mark)))
           (inhibit-read-only t))
       ,@body
       (add-text-properties ,start-sym (sly-mrepl--mark)
                            (append '(read-only t front-sticky (read-only))
                                    ,props)))))

(defun sly-mrepl--insert (string)
  (sly-mrepl--commiting-text ()
    (comint-output-filter (sly-mrepl--process) string)))

(defvar sly-mrepl--pending-output nil
  "Output that can't be inserted right now.")
(make-variable-buffer-local 'sly-mrepl--pending-output)

(defun sly-mrepl--insert-output (string)
  (cond ((and sly-mrepl--expect-sexp-mode string)
         (let ((inhibit-read-only t)
               (start (marker-position sly-mrepl--output-mark)))
           (save-excursion
             (goto-char sly-mrepl--output-mark)
             (cond ((and (zerop (current-column))
                         (get-char-property start 'sly-mrepl--prompt))
                    ;; insert after marker then go back
                    (insert "\n")
                    (goto-char sly-mrepl--output-mark))
                   ((and (sly-mrepl--busy-p)
                         (not (zerop (current-column))))
                    (insert-before-markers "\n")))
             (insert-before-markers
              (concat sly-mrepl--pending-output string))
             (setq sly-mrepl--pending-output nil)
             (add-text-properties start sly-mrepl--output-mark
                                  '(read-only t front-sticky (read-only))))))
        (t
         (setq sly-mrepl--pending-output
               (concat sly-mrepl--pending-output string))
         (message "[sly] some output saved for later insertion"))))

(defvar sly-mrepl--stacked-errors nil)
(make-variable-buffer-local 'sly-mrepl--stacked-errors)

(sly-define-channel-method listener :freeze-prompt (key)
  (with-current-buffer (sly-channel-get self 'buffer)
    (push key sly-mrepl--stacked-errors)
    (let ((inhibit-read-only t))
      (add-text-properties (overlay-start sly-mrepl--last-prompt-overlay)
                           (overlay-end sly-mrepl--last-prompt-overlay)
                           `(face ,(sly-mrepl--prompt-face))))))

(sly-define-channel-method listener :unfreeze-prompt (key)
  (with-current-buffer (sly-channel-get self 'buffer)
    (unwind-protect
        (progn
          (cl-assert sly-mrepl--stacked-errors)
          (cl-assert (string= key (car sly-mrepl--stacked-errors)))))
    (pop sly-mrepl--stacked-errors)))

(defun sly-mrepl--send-input-sexp ()
  (goto-char (point-max))
  (skip-chars-backward "\n\t\s")
  (delete-region (max (point)
                      (sly-mrepl--mark))
                 (point-max))
  (buffer-disable-undo)
  (overlay-put sly-mrepl--last-prompt-overlay 'face 'highlight)
  (sly-mrepl--commiting-text
   `(field sly-mrepl-input
           keymap ,(let ((map (make-sparse-keymap)))
                     (define-key map (kbd "RET") 'sly-mrepl-insert-input)
                     (define-key map [mouse-2] 'sly-mrepl-insert-input)
                     map))
   (comint-send-input)))

(defun sly-mrepl--ensure-newline ()
  (unless (save-excursion
            (goto-char (sly-mrepl--mark))
            (zerop (current-column)))
    (sly-mrepl--insert "\n")))

(sly-define-channel-method listener :prompt (package prompt)
  (with-current-buffer (sly-channel-get self 'buffer)
    (when (and sly-mrepl--dedicated-stream
               (process-live-p sly-mrepl--dedicated-stream))
      ;; This non-blocking call should be enough to allow asynch calls
      ;; to `sly-mrepl--insert-output' to still see the correct value
      ;; for `sly-mrepl--output-mark' just before we set it.
      (accept-process-output))
    (overlay-put sly-mrepl--last-prompt-overlay 'face nil)
    (sly-mrepl--ensure-newline)
    (sly-mrepl--catch-up)
    (let ((beg (point)))
      (sly-mrepl--insert (propertize (format "%s> " prompt)
                                     'face (sly-mrepl--prompt-face)
                                     'sly-mrepl--prompt package))
      (move-overlay sly-mrepl--last-prompt-overlay beg (point)))
    (sly-mrepl--recenter)
    (buffer-enable-undo)))

(defface sly-mrepl-normal-prompt-face
  `((t (:inherit comint-highlight-prompt)))
  "Face for the regular MREPL prompt."
  :group 'sly-mode-faces)

(defface sly-mrepl-errored-prompt-face
  `((t (:inherit font-lock-warning-face)))
  "Face for the MREPL prompt when errors are on the stack."
  :group 'sly-mode-faces)

(defun sly-mrepl--prompt-face ()
  (if sly-mrepl--stacked-errors
      'sly-mrepl-errored-prompt-face
    'sly-mrepl-normal-prompt-face))

(defun sly-mrepl--recenter ()
  (when (get-buffer-window)
    (recenter -1)))

(define-button-type 'sly-mrepl-part :supertype 'sly-part
  'sly-button-inspect #'(lambda (object-idx value-idx)
                          (sly-mrepl--send `(:inspect-object ,object-idx
                                                             ,value-idx)))
  'sly-mrepl-copy-to-repl 'sly-mrepl--copy-to-repl)

(defun sly-mrepl--copy-to-repl (&optional object-idx value-idx)
  (sly-mrepl--send `(:copy-to-repl ,object-idx ,value-idx)))

(defun sly-mrepl--make-result-button (label object-idx value-idx)
  (make-text-button label nil
                    :type 'sly-mrepl-part
                    'part-args (list object-idx value-idx)
                    'part-label label)
  label)

(defun sly-mrepl--insert-returned-values (values)
  (let* ((comint-preoutput-filter-functions nil))
    (if (null values)
        (sly-mrepl--insert "; No values")
      (cl-incf sly-mrepl--result-counter)
      (cl-loop for value in values
               for idx from 0
               do
               (sly-mrepl--ensure-newline)
               (sly-mrepl--insert (sly-mrepl--make-result-button value sly-mrepl--result-counter idx))))))

(sly-define-channel-method listener :write-values (values)
  (accept-process-output)
  (with-current-buffer (sly-channel-get self 'buffer)
    (sly-mrepl--insert-returned-values values)))

(defun sly-mrepl--catch-up ()
  (when (> (sly-mrepl--mark) sly-mrepl--output-mark)
    (set-marker sly-mrepl--output-mark (sly-mrepl--mark))))

(sly-define-channel-method listener :copy-to-repl (objects note)
  (with-current-buffer (sly-channel-get self 'buffer)
    (goto-char (sly-mrepl--mark))
    (let ((saved-text (buffer-substring (point) (point-max))))
      (delete-region (point) (point-max))
      (sly-mrepl--catch-up)
      (sly-mrepl--insert-output (concat "; " note))
      (sly-mrepl--insert-returned-values objects)
      (pop-to-buffer (current-buffer))
      (goto-char (sly-mrepl--mark))
      (insert saved-text))))

(sly-define-channel-method listener :evaluation-aborted (&optional condition)
  (with-current-buffer (sly-channel-get self 'buffer)
    (sly-mrepl--catch-up)
    (sly-mrepl--insert-output (format "; Evaluation aborted on %s\n" condition))))

(sly-define-channel-method listener :write-string (string)
  (with-current-buffer (sly-channel-get self 'buffer)
    (sly-mrepl--insert-output string)))

(sly-define-channel-method listener :inspect-object (parts)
  (cl-assert (sly-channel-p self))
  (sly-open-inspector parts))

(sly-define-channel-method listener :set-read-mode (mode)
  (with-current-buffer (sly-channel-get self 'buffer)
    (cl-ecase mode
      (:read (setq sly-mrepl--expect-sexp-mode nil)
	     (message "[sly] Listener waiting for input to read"))
      (:eval (setq sly-mrepl--expect-sexp-mode t)
             (when sly-mrepl--pending-output
               (sly-mrepl--insert-output "\n"))
             (message "[sly] Listener waiting for sexps to eval")))))

(defun sly-mrepl--busy-p ()
  (>= sly-mrepl--output-mark (sly-mrepl--mark)))

(defun sly-mrepl-return (&optional end-of-input)
  (interactive "P")
  (cl-assert (sly-connection))
  (cl-assert (process-live-p (sly-mrepl--process)) nil
             "No local live process, cannot use this REPL")
  (cond ((and
	  sly-mrepl--expect-sexp-mode
          (sly-mrepl--busy-p))
	 (message "[sly] REPL is busy"))
        ((and sly-mrepl--expect-sexp-mode
	      (or (sly-input-complete-p (sly-mrepl--mark) (point-max))
		  end-of-input))
	 (sly-mrepl--send-input-sexp)
         (sly-mrepl--catch-up))
	((not sly-mrepl--expect-sexp-mode)
	 (unless end-of-input
	   (newline))
         (comint-send-input 'no-newline))
        (t
	 (newline-and-indent)
         (message "[sly] Input not complete")))
  (sly-mrepl--recenter))

(defun sly-mrepl-insert-input (pos)
  (interactive (list (if (mouse-event-p last-input-event)
                         (posn-point (event-end last-input-event))
                       (point))))
  (let* ((pos (if (eq (field-at-pos pos) 'sly-mrepl-input)
                  pos
                (1+ pos))) 
         (new-input (field-string-no-properties pos))
         (offset (- (point) (field-beginning pos))))
    (goto-char (sly-mrepl--mark))
    (delete-region (point) (point-max))
    (insert (sly-trim-whitespace new-input))
    (goto-char (+ (sly-mrepl--mark) offset))))

(defun sly-mrepl--input-sender (_proc string)
  (sly-mrepl--send-string (substring-no-properties string)))

(defun sly-mrepl--send-string (string &optional _command-string)
  (sly-mrepl--send `(:process ,string)))

(defun sly-mrepl--send (msg)
  "Send MSG to the remote channel."
  (sly-send-to-remote-channel sly-mrepl--remote-channel msg))

(defun sly-mrepl--find-buffer (&optional connection)
  "Find the default `sly-mrepl' buffer for CONNECTION."
  ;; CONNECTION defaults to the `sly-default-connection' passing
  ;; through `sly-connection'. Seems to work OK...
  ;; 
  (let ((connection (or connection
                        (let ((sly-buffer-connection nil)
                              (sly-dispatching-connection nil))
                          (sly-connection)))))
    (cl-find-if (lambda (x)
                  (with-current-buffer x
                    (and (eq major-mode 'sly-mrepl-mode)
                         (eq sly-buffer-connection connection))))
                (buffer-list))))

(defun sly-mrepl--find-create (&optional connection)
  (or (sly-mrepl--find-buffer connection)
      (sly-mrepl-new)))

(defun sly-mrepl (&optional interactive)
  "Find or create the first useful REPL for the default connection."
  (interactive (list t))
  (let* ((buffer
          (sly-mrepl--find-create)))
    (when interactive
      (pop-to-buffer buffer))
    buffer))

(defun sly-mrepl-pop-to-mrepl ()
  (let* ((inferior-buffer (and (sly-process) (process-buffer (sly-process))))
         (inferior-window (and inferior-buffer (get-buffer-window inferior-buffer t))))
    (pop-to-buffer (sly-mrepl))
    (when inferior-window
      (bury-buffer inferior-buffer)
      (delete-window inferior-window))
    (goto-char (point-max))))


;;; copy-down-to-repl and inspection behaviour
;;;
(defun sly-mrepl--eval-for-repl (note slyfun &rest args)
  (sly-eval-async
   `(swank-mrepl:globally-save-object ,note ',slyfun ,@args)
   #'(lambda (_ignored)
       (with-current-buffer (sly-mrepl--find-create (sly-connection))
         (sly-mrepl--copy-to-repl)))))

(defun sly-inspector-copy-down-to-repl (number)
  "Evaluate the inspector slot at point via the REPL (to set `*')."
  (sly-mrepl--eval-for-repl (format "Returning inspector slot %s" number)
                            'swank:inspector-nth-part-or-lose number))

(defun sldb-copy-down-to-repl (frame-id var-id)
  "Evaluate the frame var at point via the REPL (to set `*')."
  (sly-mrepl--eval-for-repl
   (format "Returning var %s of frame %s" var-id frame-id)
   'swank-backend:frame-var-value frame-id var-id))

(defun sly-trace-dialog-copy-down-to-repl (id part-id type)
  "Eval the Trace Dialog entry under point in the REPL (to set *)"
  (sly-mrepl--eval-for-repl
   (format "Returning part %s (%s) of trace entry %s" part-id type id)
   'swank-trace-dialog::find-trace-part id part-id type))


;;; Dedicated output stream
;;;
(defun sly-mrepl--dedicated-stream-output-filter (process string)
  (let ((channel (process-get process 'sly-mrepl--channel)))
    (when channel 
      (with-current-buffer (sly-channel-get channel 'buffer)
        (when (and (cl-plusp (length string))
                   (eq (process-status sly-buffer-connection) 'open))
          (sly-mrepl--insert-output string))))))

(defvar sly-mrepl--dedicated-stream-hooks)

(defun sly-mrepl--open-dedicated-stream (channel port coding-system)
  (let* ((name (format "sly-dds-%s-%s"
                       (process-get sly-buffer-connection
                                    'sly--net-connect-counter)
                       (sly-channel.id channel)))
         (stream (open-network-stream name
                                      (generate-new-buffer
                                       (format " *%s*" name))
                                      (car (process-contact sly-buffer-connection))
                                      port))
         (emacs-coding-system (car (cl-find coding-system
                                            sly-net-valid-coding-systems
                                            :key #'cl-third))))
    (set-process-query-on-exit-flag stream nil)
    (set-process-plist stream `(sly-mrepl--channel ,channel))
    (set-process-filter stream 'sly-mrepl--dedicated-stream-output-filter)
    (set-process-coding-system stream emacs-coding-system emacs-coding-system)
    (when-let (secret (sly-secret))
      (sly-net-send secret stream))
    (run-hook-with-args 'sly-mrepl--dedicated-stream-hooks stream)
    stream))

(sly-define-channel-method listener :open-dedicated-output-stream (port _coding-system)
  (with-current-buffer (sly-channel-get self 'buffer)
    ;; HACK: no coding system
    (set (make-local-variable 'sly-mrepl--dedicated-stream)
         (sly-mrepl--open-dedicated-stream self port nil))))


;;; Misc
;;;
(defvar sly-mrepl-history-separator "####\n")

(defun sly-mrepl-read-input-ring ()
  (let ((comint-input-ring-separator sly-mrepl-history-separator))
    (comint-read-input-ring)))
  

(defun sly-mrepl--merge-and-save-history ()
  (let* ((current-ring (copy-tree comint-input-ring 'vectors-too))
         (index (ring-length current-ring))
         (comint-input-ring-separator sly-mrepl-history-separator))

    ;; this sets comint-input-ring from the file
    ;; 
    (comint-read-input-ring)
    (cl-loop for i from 0 below index
             for item = (ring-ref current-ring i)
             unless (ring-member comint-input-ring item)
             do (ring-insert comint-input-ring item))
    (comint-write-input-ring)))

(defun sly-mrepl--teardown (&optional reason)
  (remove-hook 'kill-buffer-hook 'sly-mrepl--teardown t)
  (let ((inhibit-read-only t))
    (goto-char (point-max))
    (unless (zerop (current-column)) (insert "\n"))
    (insert (format "; %s" (or reason "REPL teardown")))
    (unless (zerop (current-column)) (insert "\n"))
    (insert "; --------------------------------------------------------")
    (setq buffer-read-only t))
  (sly-mrepl--merge-and-save-history)
  (when sly-mrepl--dedicated-stream
    (kill-buffer (process-buffer sly-mrepl--dedicated-stream)))
  ;; signal lisp that we're closingq
  (when (ignore-errors (sly-connection))
    (sly-mrepl--send `(:teardown))
    (sly-close-channel sly-mrepl--local-channel))
  (set (make-local-variable 'sly-mrepl--remote-channel) nil)
  (when (sly-mrepl--process)
    (delete-process (sly-mrepl--process))))


(defun sly-mrepl-sync-package-and-default-directory ()
  "Set Lisp's package and directory to the values in current buffer."
  (interactive)
  (let ((package (sly-current-package)))
    (with-current-buffer (sly-mrepl)
      (sly-mrepl--send `(:sync-package-and-default-directory
                         ,package
                         ,default-directory)))))


(defun sly-mrepl-guess-package (&optional point interactive)
  (interactive (list (point) t))
  (let* ((point (or point (point)))
         (probe
          (previous-single-property-change point
                                           'sly-mrepl--prompt))
         (package (and probe
                       (or (get-text-property probe 'sly-mrepl--prompt)
                           (get-text-property (previous-single-property-change
                                               probe 'sly-mrepl--prompt)
                                              'sly-mrepl--prompt)))))
    (when interactive
      (message "[sly] Guessed package \"%s\"" package))
    package))


(defun sly-mrepl-inside-string-or-comment-p ()
  (when (> (point) (sly-mrepl--mark))
    (let ((ppss (parse-partial-sexp (sly-mrepl--mark) (point))))
      (or (nth 3 ppss) (nth 4 ppss)))))


;;;; Menu
;;;;
(easy-menu-define sly-mrepl--shortcut-menu nil
  "Menu for accessing the mREPL anywhere in sly."
  (let* ((C '(sly-connected-p)))
    `("mREPL"
      ["Go to default REPL" sly-mrepl ,C]
      ["New REPL" sly-mrepl-new ,C]
      ["Sync Package & Directory" sly-mrepl-sync-package-and-default-directory
       (and sly-editing-mode ,C)])))

(easy-menu-add-item sly-menu nil sly-mrepl--shortcut-menu "Documentation")

(easy-menu-define sly-mrepl--menu sly-mrepl-mode-map
  "Menu for SLY's MREPL"
  (let* ((C '(sly-connected-p)))
    `("SLY-mREPL"
      [ " Complete symbol at point " sly-indent-and-complete-symbol ,C ]
      [ " Interrupt " sly-interrupt ,C ]
      [ " Isearch history backward " isearch-backward ,C])))


(def-sly-selector-method ?m
  "First mrepl-buffer"
  (or (sly-mrepl)
      (error "No mrepl buffer (%s)" (sly-connection-name))))


(defvar sly-mrepl--debug-overlays nil)
(defun sly-mrepl--debug (&rest ignored)
  (interactive)
  (mapc #'delete-overlay sly-mrepl--debug-overlays)
  (let ((overlay (make-overlay sly-mrepl--output-mark
                               (sly-mrepl--mark)))
        (color (if (< sly-mrepl--output-mark (sly-mrepl--mark))
                   "green"
                 "orange"))
        (marker-color (if (= sly-mrepl--output-mark (sly-mrepl--mark))
                          "red"
                        "purple")))
    (overlay-put overlay
                 'face `(:background ,color))
    (overlay-put overlay
                 'after-string (propertize "F" 'face `(:background ,marker-color)))
    (push overlay sly-mrepl--debug-overlays)))

(defun sly-mrepl--turn-on-debug ()
  (interactive)
  (add-hook 'after-change-functions 'sly-mrepl--debug nil 'local)
  (add-hook 'post-command-hook 'sly-mrepl--debug nil 'local))

(defun sly-mrepl--turn-off-debug ()
  (interactive)
  (remove-hook 'after-change-functions 'sly-mrepl--debug 'local)
  (remove-hook 'post-command-hook 'sly-mrepl--debug 'local))


(provide 'sly-mrepl)
