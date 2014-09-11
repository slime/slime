(require 'sly)
(require 'cl-lib)

(define-sly-contrib sly-fuzzy
  "Fuzzy symbol completion."
  (:authors "Brian Downing <bdowning@lavos.net>"
            "Tobias C. Rittweiler <tcr@freebits.de>"
            "Attila Lendvai <attila.lendvai@gmail.com>")
  (:license "GPL")
  (:slynk-dependencies slynk-fuzzy)
  (:on-load
   (setq sly-complete-symbol-function 'sly-fuzzy-complete-symbol)
   (define-key sly-mode-map "\C-c\M-i" 'sly-fuzzy-complete-symbol)))

(defcustom sly-fuzzy-completion-in-place t
  "When non-NIL the fuzzy symbol completion is done in place as
opposed to moving the point to the completion buffer."
  :group 'sly-mode
  :type 'boolean)

(defcustom sly-fuzzy-completion-limit 300
  "Only return and present this many symbols from slynk."
  :group 'sly-mode
  :type 'integer)

(defcustom sly-fuzzy-completion-time-limit-in-msec 1500
  "Limit the time spent (given in msec) in slynk while gathering
comletitions."
  :group 'sly-mode
  :type 'integer)

(defcustom sly-when-complete-filename-expand nil
  "Use comint-replace-by-expanded-filename instead of
comint-dynamic-complete-as-filename to complete file names"
  :group 'sly-mode
  :type 'boolean)


(defvar sly-fuzzy-target-buffer nil
  "The buffer that is the target of the completion activities.")
(defvar sly-fuzzy-saved-window-configuration nil
  "The saved window configuration before the fuzzy completion
buffer popped up.")
(defvar sly-fuzzy-start nil
  "The beginning of the completion slot in the target buffer.
This is a non-advancing marker.")
(defvar sly-fuzzy-end nil
  "The end of the completion slot in the target buffer.
This is an advancing marker.")
(defvar sly-fuzzy-original-text nil
  "The original text that was in the completion slot in the
target buffer.  This is what is put back if completion is
aborted.")
(defvar sly-fuzzy-text nil
  "The text that is currently in the completion slot in the
target buffer.  If this ever doesn't match, the target buffer has
been modified and we abort without touching it.")
(defvar sly-fuzzy-first nil
  "The position of the first completion in the completions buffer.
The descriptive text and headers are above this.")
(defvar sly-fuzzy-last nil
    "The position of the last completion in the completions buffer.
If the time limit has exhausted during generation possible completion
choices inside SLYNK, an indication is printed below this.")
(defvar sly-fuzzy-current-completion nil
  "The current completion object.  If this is the same before and
after point moves in the completions buffer, the text is not
replaced in the target for efficiency.")
(defvar sly-fuzzy-current-completion-overlay nil
  "The overlay representing the current completion in the completion
buffer. This is used to hightlight the text.")

;;;;;;; sly-target-buffer-fuzzy-completions-mode
;; NOTE: this mode has to be able to override key mappings in sly-mode

(defvar sly-target-buffer-fuzzy-completions-map
  (let ((map (make-sparse-keymap)))
    (cl-labels ((def (keys command)
                   (unless (listp keys)
                     (setq keys (list keys)))
                   (dolist (key keys)
                     (define-key map key command))))
      (def `([remap keyboard-quit]
             ,(kbd "C-g"))
           'sly-fuzzy-abort)
      (def `([remap sly-fuzzy-indent-and-complete-symbol]
             [remap sly-indent-and-complete-symbol]
             ,(kbd "<tab>"))
           'sly-fuzzy-select-or-update-completions)
      (def `([remap previous-line]
             ,(kbd "<up>"))
           'sly-fuzzy-prev)
      (def `([remap next-line]
             ,(kbd "<down>"))
           'sly-fuzzy-next)
      (def `([remap isearch-forward]
             ,(kbd "C-s"))
           'sly-fuzzy-continue-isearch-in-fuzzy-buffer)
      ;; some unconditional direct bindings
      (def (list (kbd "<return>") (kbd "RET") (kbd "<SPC>") "(" ")" "[" "]")
           'sly-fuzzy-select-and-process-event-in-target-buffer))
    map)
  "Keymap for sly-target-buffer-fuzzy-completions-mode.
This will override the key bindings in the target buffer
temporarily during completion.")

;; Make sure sly-fuzzy-target-buffer-completions-mode's map is
;; before everything else.
(setf minor-mode-map-alist
      (cl-stable-sort minor-mode-map-alist
                      (lambda (a b)
                        (eq a 'sly-fuzzy-target-buffer-completions-mode))
                      :key #'car))

(defun sly-fuzzy-continue-isearch-in-fuzzy-buffer ()
  (interactive)
  (select-window (get-buffer-window (sly-get-fuzzy-buffer)))
  (call-interactively 'isearch-forward))

(defvar sly-fuzzy-target-original-point nil)

(defun sly-fuzzy-target-post-command-hook ()
  (unless (eq (point) sly-fuzzy-target-original-point)
    (save-excursion (sly-fuzzy-abort))))

(define-minor-mode sly-fuzzy-target-buffer-completions-mode
  "This minor mode is intented to override key bindings during
fuzzy completions in the target buffer. Most of the bindings will
do an implicit select in the completion window and let the
keypress be processed in the target buffer."
  nil
  nil
  sly-target-buffer-fuzzy-completions-map
  (cond (sly-fuzzy-target-buffer-completions-mode
         (set (make-local-variable 'sly-fuzzy-target-original-point)
              (point))
         (add-hook 'post-command-hook
                   'sly-fuzzy-target-post-command-hook
                   'append
                   'local)
         (add-hook 'kill-buffer-hook
                   'sly-fuzzy-abort
                   'append
                   'local))
        (t
         (remove-hook 'post-command-hook
                      'sly-fuzzy-target-post-command-hook
                      'local)
         (remove-hook 'kill-buffer-hook
                      'sly-fuzzy-abort
                      'local))))

(add-to-list 'minor-mode-alist
             '(sly-fuzzy-target-buffer-completions-mode
               " Fuzzy Target Buffer Completions"))

(defvar sly-fuzzy-completions-map
  (let ((map (make-sparse-keymap)))
    (cl-labels ((def (keys command)
                     (unless (listp keys)
                       (setq keys (list keys)))
                     (dolist (key keys)
                       (define-key map key command))))
      (def `([remap keyboard-quit]
             "q"
             ,(kbd "C-g"))
           'sly-fuzzy-abort)
      (def `([remap previous-line]
             "p"
             "\M-p"
             ,(kbd "<up>"))
           'sly-fuzzy-prev)
      (def `([remap next-line]
             "n"
             "\M-n"
             ,(kbd "<down>"))
           'sly-fuzzy-next)
      (def "\d" 'scroll-down)
      (def `([remap sly-fuzzy-indent-and-complete-symbol]
             [remap sly-indent-and-complete-symbol]
             ,(kbd "<tab>"))
           'sly-fuzzy-select)
      (def (kbd "<mouse-2>") 'sly-fuzzy-select/mouse)
      (def `(,(kbd "RET")
             ,(kbd "<SPC>"))
           'sly-fuzzy-select))
    map)
  "Keymap for sly-fuzzy-completions-mode when in the completion buffer.")

(define-derived-mode sly-fuzzy-completions-mode
  fundamental-mode "Fuzzy Completions"
  "Major mode for presenting fuzzy completion results.

When you run `sly-fuzzy-complete-symbol', the symbol token at
point is completed using the Fuzzy Completion algorithm; this
means that the token is taken as a sequence of characters and all
the various possibilities that this sequence could meaningfully
represent are offered as selectable choices, sorted by how well
they deem to be a match for the token. (For instance, the first
choice of completing on \"mvb\" would be \"multiple-value-bind\".)

Therefore, a new buffer (*Fuzzy Completions*) will pop up that
contains the different completion choices. Simultaneously, a
special minor-mode will be temporarily enabled in the original
buffer where you initiated fuzzy completion (also called the
``target buffer'') in order to navigate through the *Fuzzy
Completions* buffer without leaving.

With focus in *Fuzzy Completions*:
  Type `n' and `p' (`UP', `DOWN') to navigate between completions.
  Type `RET' or `TAB' to select the completion near point.
  Type `q' to abort.

With focus in the target buffer:
  Type `UP' and `DOWN' to navigate between completions.
  Type a character that does not constitute a symbol name
  to insert the current choice and then that character (`(', `)',
  `SPACE', `RET'.) Use `TAB' to simply insert the current choice.
  Use C-g to abort.

Alternatively, you can click <mouse-2> on a completion to select it.


Complete listing of keybindings within the target buffer:

\\<sly-target-buffer-fuzzy-completions-map>\
\\{sly-target-buffer-fuzzy-completions-map}

Complete listing of keybindings with *Fuzzy Completions*:

\\<sly-fuzzy-completions-map>\
\\{sly-fuzzy-completions-map}"
  (use-local-map sly-fuzzy-completions-map)
  (set (make-local-variable 'sly-fuzzy-current-completion-overlay)
       (make-overlay (point) (point) nil t nil)))

(defun sly-fuzzy-completions (prefix &optional default-package)
  "Get the list of sorted completion objects from completing
`prefix' in `package' from the connected Lisp."
  (let ((prefix (cl-etypecase prefix
                  (symbol (symbol-name prefix))
                  (string prefix))))
    (sly-eval `(slynk:fuzzy-completions ,prefix
                                        ,(or default-package
                                             (sly-current-package))
                                        :limit ,sly-fuzzy-completion-limit
                                        :time-limit-in-msec
                                        ,sly-fuzzy-completion-time-limit-in-msec))))

(defun sly-fuzzy-selected (prefix completion)
  "Tell the connected Lisp that the user selected completion
`completion' as the completion for `prefix'."
  (let ((no-properties (copy-sequence prefix)))
    (set-text-properties 0 (length no-properties) nil no-properties)
    (sly-eval `(slynk:fuzzy-completion-selected ,no-properties
                                                  ',completion))))

(defun sly-fuzzy-indent-and-complete-symbol ()
  "Indent the current line and perform fuzzy symbol completion.  First
indent the line. If indenting doesn't move point, complete the
symbol. If there's no symbol at the point, show the arglist for the
most recently enclosed macro or function."
  (interactive)
  (let ((pos (point)))
    (unless (get-text-property (line-beginning-position) 'sly-repl-prompt)
      (lisp-indent-line))
    (when (= pos (point))
      (cond ((save-excursion (re-search-backward "[^() \n\t\r]+\\=" nil t))
             (sly-fuzzy-complete-symbol))
            ((memq (char-before) '(?\t ?\ ))
             (sly-show-arglist))))))

(cl-defun sly-fuzzy-complete-symbol ()
  "Fuzzily completes the abbreviation at point into a symbol."
  (interactive)
  (when (save-excursion (re-search-backward "\"[^ \t\n]+\\=" nil t))
    (cl-return-from sly-fuzzy-complete-symbol
      ;; don't add space after completion
      (let ((comint-completion-addsuffix '("/" . "")))
        (if sly-when-complete-filename-expand
            (comint-replace-by-expanded-filename)
          (comint-filename-completion)))))
  (let* ((end (move-marker (make-marker) (sly-symbol-end-pos)))
         (beg (move-marker (make-marker) (sly-symbol-start-pos)))
         (prefix (buffer-substring-no-properties beg end)))
    (cl-destructuring-bind (completion-set interrupted-p)
        (sly-fuzzy-completions prefix)
      (if (null completion-set)
          (progn (sly-minibuffer-respecting-message
                  "Can't find completion for \"%s\"" prefix)
                 (ding)
                 (sly-fuzzy-done))
        (goto-char end)
        (cond ((sly-length= completion-set 1)
               ;; insert completed string
               (insert-and-inherit (caar completion-set))
               (delete-region beg end)
               (goto-char (+ beg (length (caar completion-set))))
               (sly-minibuffer-respecting-message "Sole completion")
               (sly-fuzzy-done))
              ;; Incomplete
              (t
               (sly-fuzzy-choices-buffer completion-set interrupted-p
                                         beg end)
               (sly-minibuffer-respecting-message
                "Complete but not unique")))))))


(defun sly-get-fuzzy-buffer ()
  (get-buffer-create "*Fuzzy Completions*"))

(defvar sly-fuzzy-explanation
  "For help on how the use this buffer, see `sly-fuzzy-completions-mode'.

Flags: boundp fboundp generic-function class macro special-operator package
\n"
  "The explanation that gets inserted at the beginning of the
*Fuzzy Completions* buffer.")

(defun sly-fuzzy-insert-completion-choice (completion max-length)
  "Inserts the completion object `completion' as a formatted
completion choice into the current buffer, and mark it with the
proper text properties."
  (cl-destructuring-bind (symbol-name score chunks classification-string)
      completion
    (let ((start (point))
          (end))
      (insert symbol-name)
      (setq end (point))
      (dolist (chunk chunks)
        (put-text-property (+ start (cl-first chunk))
                           (+ start (cl-first chunk)
                              (length (cl-second chunk)))
                           'face 'bold))
      (put-text-property start (point) 'mouse-face 'highlight)
      (dotimes (i (- max-length (- end start)))
        (insert " "))
      (insert (format " %s %s\n"
                      classification-string
                      score))
      (put-text-property start (point) 'completion completion))))

(defun sly-fuzzy-insert (text)
  "Inserts `text' into the target buffer in the completion slot.
If the buffer has been modified in the meantime, abort the
completion process.  Otherwise, update all completion variables
so that the new text is present."
  (with-current-buffer sly-fuzzy-target-buffer
    (cond
     ((not (string-equal sly-fuzzy-text
                         (buffer-substring sly-fuzzy-start
                                           sly-fuzzy-end)))
      (sly-fuzzy-done)
      (sly-message "[sly fuzzy-insert] Target buffer has been modified!"))
     (t
      (goto-char sly-fuzzy-start)
      (delete-region sly-fuzzy-start sly-fuzzy-end)
      (insert-and-inherit text)
      (setq sly-fuzzy-text text)
      (goto-char sly-fuzzy-end)))))

(defun sly-minibuffer-p (buffer)
  (if (featurep 'xemacs)
      (eq buffer (window-buffer (minibuffer-window)))
      (minibufferp buffer)))

(defun sly-fuzzy-choices-buffer (completions interrupted-p start end)
  "Creates (if neccessary), populates, and pops up the *Fuzzy
Completions* buffer with the completions from `completions' and
the completion slot in the current buffer bounded by `start' and
`end'.  This saves the window configuration before popping the
buffer so that it can possibly be restored when the user is
done."
  (let ((new-completion-buffer (not sly-fuzzy-target-buffer))
        (connection (sly-connection)))
    (when new-completion-buffer
      (setq sly-fuzzy-saved-window-configuration
            (current-window-configuration)))
    (sly-fuzzy-enable-target-buffer-completions-mode)
    (setq sly-fuzzy-target-buffer (current-buffer))
    (setq sly-fuzzy-start (move-marker (make-marker) start))
    (setq sly-fuzzy-end (move-marker (make-marker) end))
    (set-marker-insertion-type sly-fuzzy-end t)
    (setq sly-fuzzy-original-text (buffer-substring start end))
    (setq sly-fuzzy-text sly-fuzzy-original-text)
    (sly-fuzzy-fill-completions-buffer completions interrupted-p)
    (pop-to-buffer (sly-get-fuzzy-buffer))
    (sly-fuzzy-next)
    (setq sly-buffer-connection connection)
    (when new-completion-buffer
      ;; Hook to nullify window-config restoration if the user changes
      ;; the window configuration himself.
      (when (boundp 'window-configuration-change-hook)
        (add-hook 'window-configuration-change-hook
                  'sly-fuzzy-window-configuration-change))
      (add-hook 'kill-buffer-hook 'sly-fuzzy-abort 'append t)
      (set (make-local-variable 'cursor-type) nil)
      (setq buffer-quit-function 'sly-fuzzy-abort)) ; M-Esc Esc
    (when sly-fuzzy-completion-in-place
      ;; switch back to the original buffer
      (if (sly-minibuffer-p sly-fuzzy-target-buffer)
          (select-window (minibuffer-window))
          (switch-to-buffer-other-window sly-fuzzy-target-buffer)))))

(defun sly-fuzzy-fill-completions-buffer (completions interrupted-p)
  "Erases and fills the completion buffer with the given completions."
  (with-current-buffer (sly-get-fuzzy-buffer)
    (setq buffer-read-only nil)
    (erase-buffer)
    (sly-fuzzy-completions-mode)
    (insert sly-fuzzy-explanation)
    (let ((max-length 12))
      (dolist (completion completions)
        (setf max-length (max max-length (length (cl-first completion)))))

      (insert "Completion:")
      (dotimes (i (- max-length 10)) (insert " "))
      ;;     Flags:   Score:
      ;; ... -------  --------
      ;;     bfgctmsp
      (let* ((example-classification-string (cl-fourth (cl-first completions)))
             (classification-length (length example-classification-string))
             (spaces (- classification-length (length "Flags:"))))
        (insert "Flags:")
        (dotimes (i spaces) (insert " "))
        (insert " Score:\n")
        (dotimes (i max-length) (insert "-"))
        (insert " ")
        (dotimes (i classification-length) (insert "-"))
        (insert " --------\n")
        (setq sly-fuzzy-first (point)))

      (dolist (completion completions)
        (setq sly-fuzzy-last (point)) ; will eventually become the last entry
        (sly-fuzzy-insert-completion-choice completion max-length))

      (when interrupted-p
        (insert "...\n")
        (insert "[Interrupted: time limit exhausted]"))

      (setq buffer-read-only t))
    (setq sly-fuzzy-current-completion
          (caar completions))
    (goto-char 0)))

(defun sly-fuzzy-enable-target-buffer-completions-mode ()
  "Store the target buffer's local map, so that we can restore it."
  (unless sly-fuzzy-target-buffer-completions-mode
;    (sly-log-event "Enabling target buffer completions mode")
    (sly-fuzzy-target-buffer-completions-mode 1)))

(defun sly-fuzzy-disable-target-buffer-completions-mode ()
  "Restores the target buffer's local map when completion is finished."
  (when sly-fuzzy-target-buffer-completions-mode
;    (sly-log-event "Disabling target buffer completions mode")
    (sly-fuzzy-target-buffer-completions-mode 0)))

(defun sly-fuzzy-insert-from-point ()
  "Inserts the completion that is under point in the completions
buffer into the target buffer.  If the completion in question had
already been inserted, it does nothing."
  (with-current-buffer (sly-get-fuzzy-buffer)
    (let ((current-completion (get-text-property (point) 'completion)))
      (when (and current-completion
                 (not (eq sly-fuzzy-current-completion
                          current-completion)))
        (sly-fuzzy-insert
         (cl-first (get-text-property (point) 'completion)))
        (setq sly-fuzzy-current-completion
              current-completion)))))

(defun sly-fuzzy-post-command-hook ()
  "The post-command-hook for the *Fuzzy Completions* buffer.
This makes sure the completion slot in the target buffer matches
the completion that point is on in the completions buffer."
  (condition-case err
      (when sly-fuzzy-target-buffer
        (sly-fuzzy-insert-from-point))
    (error
     ;; Because this is called on the post-command-hook, we mustn't let
     ;; errors propagate.
     (message "Error in sly-fuzzy-post-command-hook: %S" err))))

(defun sly-fuzzy-next ()
  "Moves point directly to the next completion in the completions
buffer."
  (interactive)
  (with-current-buffer (sly-get-fuzzy-buffer)
    (let ((point (next-single-char-property-change
                  (point) 'completion nil sly-fuzzy-last)))
      (set-window-point (get-buffer-window (current-buffer)) point)
      (goto-char point))
    (sly-fuzzy-highlight-current-completion)))

(defun sly-fuzzy-prev ()
  "Moves point directly to the previous completion in the
completions buffer."
  (interactive)
  (with-current-buffer (sly-get-fuzzy-buffer)
    (let ((point (previous-single-char-property-change
                  (point)
                  'completion nil sly-fuzzy-first)))
      (set-window-point (get-buffer-window (current-buffer)) point)
      (goto-char point))
    (sly-fuzzy-highlight-current-completion)))

(defun sly-fuzzy-highlight-current-completion ()
  "Highlights the current completion,
so that the user can see it on the screen."
  (let ((pos (point)))
    (when (overlayp sly-fuzzy-current-completion-overlay)
      (move-overlay sly-fuzzy-current-completion-overlay
                    (point) (1- (search-forward " ")))
      (overlay-put sly-fuzzy-current-completion-overlay
                   'face 'secondary-selection))
    (goto-char pos)))

(defun sly-fuzzy-abort ()
  "Aborts the completion process, setting the completions slot in
the target buffer back to its original contents."
  (interactive)
  (when (and sly-fuzzy-target-buffer
             (buffer-live-p sly-fuzzy-target-buffer))
    (sly-fuzzy-done)))

(defun sly-fuzzy-select ()
  "Selects the current completion, making sure that it is inserted
into the target buffer.  This tells the connected Lisp what completion
was selected."
  (interactive)
  (when sly-fuzzy-target-buffer
    (with-current-buffer (sly-get-fuzzy-buffer)
      (let ((completion (get-text-property (point) 'completion)))
        (when completion
          (sly-fuzzy-insert (cl-first completion))
          (sly-fuzzy-selected sly-fuzzy-original-text
                                completion)
          (sly-fuzzy-done))))))

(defun sly-fuzzy-select-or-update-completions ()
  "If there were no changes since the last time fuzzy completion was started
this function will select the current completion.
Otherwise refreshes the completion list based on the changes made."
  (interactive)
;  (sly-log-event "Selecting or updating completions")
  (if (string-equal sly-fuzzy-original-text
                    (buffer-substring sly-fuzzy-start
                                      sly-fuzzy-end))
      (sly-fuzzy-select)
      (sly-fuzzy-complete-symbol)))

(defun sly-fuzzy-process-event-in-completions-buffer ()
  "Simply processes the event in the target buffer"
  (interactive)
  (with-current-buffer (sly-get-fuzzy-buffer)
    (push last-input-event unread-command-events)))

(defun sly-fuzzy-select-and-process-event-in-target-buffer ()
 "Selects the current completion, making sure that it is inserted
into the target buffer and processes the event in the target buffer."
 (interactive)
; (sly-log-event "Selecting and processing event in target buffer")
 (when sly-fuzzy-target-buffer
   (let ((buff sly-fuzzy-target-buffer))
     (sly-fuzzy-select)
     (with-current-buffer buff
       (sly-fuzzy-disable-target-buffer-completions-mode)
       (push last-input-event unread-command-events)))))

(defun sly-fuzzy-select/mouse (event)
  "Handle a mouse-2 click on a completion choice as if point were
on the completion choice and the sly-fuzzy-select command was
run."
  (interactive "e")
  (with-current-buffer (window-buffer (posn-window (event-end event)))
    (save-excursion
      (goto-char (posn-point (event-end event)))
      (when (get-text-property (point) 'mouse-face)
        (sly-fuzzy-insert-from-point)
        (sly-fuzzy-select)))))

(defun sly-fuzzy-done ()
  "Cleans up after the completion process.  This removes all hooks,
and attempts to restore the window configuration.  If this fails,
it just burys the completions buffer and leaves the window
configuration alone."
  (when sly-fuzzy-target-buffer
    (set-buffer sly-fuzzy-target-buffer)
    (sly-fuzzy-disable-target-buffer-completions-mode)
    (if (sly-fuzzy-maybe-restore-window-configuration)
        (bury-buffer (sly-get-fuzzy-buffer))
        ;; We couldn't restore the windows, so just bury the fuzzy
        ;; completions buffer and let something else fill it in.
        (pop-to-buffer (sly-get-fuzzy-buffer))
        (bury-buffer))
    (if (sly-minibuffer-p sly-fuzzy-target-buffer)
        (select-window (minibuffer-window))
        (pop-to-buffer sly-fuzzy-target-buffer))
    (goto-char sly-fuzzy-end)
    (setq sly-fuzzy-target-buffer nil)
    (remove-hook 'window-configuration-change-hook
                 'sly-fuzzy-window-configuration-change)))

(defun sly-fuzzy-maybe-restore-window-configuration ()
  "Restores the saved window configuration if it has not been
nullified."
  (when (boundp 'window-configuration-change-hook)
    (remove-hook 'window-configuration-change-hook
                 'sly-fuzzy-window-configuration-change))
  (if (not sly-fuzzy-saved-window-configuration)
      nil
    (set-window-configuration sly-fuzzy-saved-window-configuration)
    (setq sly-fuzzy-saved-window-configuration nil)
    t))

(defun sly-fuzzy-window-configuration-change ()
  "Called on window-configuration-change-hook.  Since the window
configuration was changed, we nullify our saved configuration."
  
  (setq sly-fuzzy-saved-window-configuration nil))


;;; Emulation of the old "c-p-c" contrib
;;;

(defun sly-c-p-c-symbol-completions (prefix)
  (let ((sly-current-thread t))
    (car
     (sly-eval
      `(slynk:completions ,prefix ',(sly-current-package))))))

(defun sly-c-p-c-char-name-completions (prefix)
  (let ((sly-current-thread t))
    (car
     (sly-eval
      `(slynk:completions-for-character ,prefix)))))

(defun sly-c-p-c-complete-symbol ()
  "Complete the symbol at point.
Perform completion more similar to Emacs' complete-symbol."
  (let* ((end (point))
         (beg (sly-symbol-start-pos))
         (fn (cond ((and beg
                         (save-excursion
                           (goto-char beg)
                           (looking-at "#\\\\")))
                    (setq beg (+ 2 beg)) ;;HACK
                    'sly-c-p-c-char-name-completions)
                   (beg
                    'sly-c-p-c-symbol-completions)
                   (t
                    nil))))
    (and fn
         (list beg end
               (funcall fn (buffer-substring-no-properties beg end))))))

(provide 'sly-fuzzy)
