;;; sly-repl.el ---
;;
;; Original Author: Helmut Eller
;; Contributors: too many to mention
;; License: GNU GPL (same license as Emacs)
;;
;;; Description:
;;

;;
;;; Installation:
;;
;; Call sly-setup and include 'sly-repl as argument:
;;
;;  (sly-setup '(sly-repl [others conribs ...]))
;;
(require 'sly)
(require 'sly-parse)
(require 'cl-lib)
(eval-when-compile (require 'cl)) ; sly-def-connection-var, which
                                  ; expands to defsetf not in cl-lib

(define-sly-contrib sly-repl
  "Read-Eval-Print Loop written in Emacs Lisp.

This contrib implements a Lisp Listener along with some niceties like
a persistent history and various \"shortcut\" commands.  Nothing here
depends on comint.el; I/O is multiplexed over SLY's socket.

This used to be the default REPL for SLY, but it was hard to
maintain."
  (:authors "too many to mention")
  (:license "GPL")
  (:on-load
   (sly-repl-add-hooks)
   (setq sly-find-buffer-package-function 'sly-repl-find-buffer-package))
  (:on-unload (sly-repl-remove-hooks))
  (:swank-dependencies swank-repl))

;;;;; sly-repl

(defgroup sly-repl nil
  "The Read-Eval-Print Loop (*sly-repl* buffer)."
  :prefix "sly-repl-"
  :group 'sly)

(defcustom sly-repl-shortcut-dispatch-char ?\,
  "Character used to distinguish repl commands from lisp forms."
  :type '(character)
  :group 'sly-repl)

(defcustom sly-repl-only-save-lisp-buffers t
  "When T we only attempt to save lisp-mode file buffers. When
  NIL sly will attempt to save all buffers (as per
  save-some-buffers). This applies to all ASDF related repl
  shortcuts."
  :type '(boolean)
  :group 'sly-repl)

(defcustom sly-repl-auto-right-margin nil
  "When T we bind CL:*PRINT-RIGHT-MARGIN* to the width of the
current repl's (as per sly-output-buffer) window."
  :type '(boolean)
  :group 'sly-repl)

(defface sly-repl-prompt-face
  (if (sly-face-inheritance-possible-p)
      '((t (:inherit font-lock-keyword-face)))
    '((((class color) (background light)) (:foreground "Purple"))
      (((class color) (background dark)) (:foreground "Cyan"))
      (t (:weight bold))))
  "Face for the prompt in the SLY REPL."
  :group 'sly-repl)

(defface sly-repl-output-face
  (if (sly-face-inheritance-possible-p)
      '((t (:inherit font-lock-string-face)))
    '((((class color) (background light)) (:foreground "RosyBrown"))
      (((class color) (background dark)) (:foreground "LightSalmon"))
      (t (:slant italic))))
  "Face for Lisp output in the SLY REPL."
  :group 'sly-repl)

(defface sly-repl-input-face
  '((t (:bold t)))
  "Face for previous input in the SLY REPL."
  :group 'sly-repl)

(defface sly-repl-result-face
  '((t ()))
  "Face for the result of an evaluation in the SLY REPL."
  :group 'sly-repl)

(defcustom sly-repl-history-file "~/.sly-history.eld"
  "File to save the persistent REPL history to."
  :type 'string
  :group 'sly-repl)

(defcustom sly-repl-history-size 200
  "*Maximum number of lines for persistent REPL history."
  :type 'integer
  :group 'sly-repl)

(defcustom sly-repl-history-file-coding-system
  (cond ((sly-find-coding-system 'utf-8-unix) 'utf-8-unix)
        (t sly-net-coding-system))
  "*The coding system for the history file."
  :type 'symbol
  :group 'sly-repl)


;; dummy defvar for compiler
(defvar sly-repl-read-mode)

(defun sly-reading-p ()
  "True if Lisp is currently reading input from the REPL."
  (with-current-buffer (sly-output-buffer)
    sly-repl-read-mode))


;;;; Stream output

(sly-def-connection-var sly-connection-output-buffer nil
  "The buffer for the REPL.  May be nil or a dead buffer.")

(make-variable-buffer-local
 (defvar sly-output-start nil
   "Marker for the start of the output for the evaluation."))

(make-variable-buffer-local
 (defvar sly-output-end nil
   "Marker for end of output. New output is inserted at this mark."))

;; dummy definitions for the compiler
(defvar sly-repl-package-stack)
(defvar sly-repl-directory-stack)
(defvar sly-repl-input-start-mark)
(defvar sly-repl-prompt-start-mark)

(defun sly-output-buffer (&optional noprompt)
  "Return the output buffer, create it if necessary."
  (let ((buffer (sly-connection-output-buffer)))
    (or (if (buffer-live-p buffer) buffer)
        (setf (sly-connection-output-buffer)
              (let ((connection (sly-connection)))
                (with-current-buffer (sly-repl-buffer t connection)
                  (unless (eq major-mode 'sly-repl-mode)
                    (sly-repl-mode))
                  (setq sly-buffer-connection connection)
		  (setq sly-buffer-package (sly-lisp-package connection))
                  (sly-reset-repl-markers)
                  (unless noprompt
                    (sly-repl-insert-prompt))
                  (current-buffer)))))))

(defvar sly-repl-banner-function 'sly-repl-insert-banner)

(defun sly-repl-update-banner ()
  (funcall sly-repl-banner-function)
  (sly-move-point (point-max))
  (sly-mark-output-start)
  (sly-mark-input-start)
  (sly-repl-insert-prompt))

(defun sly-repl-insert-banner ()
  (when (zerop (buffer-size))
    (let ((welcome (concat "; SLY " (or (sly-changelog-date)
                                          "- ChangeLog file not found"))))
      (insert welcome))))

(defun sly-init-output-buffer (connection)
  (with-current-buffer (sly-output-buffer t)
    (setq sly-buffer-connection connection
          sly-repl-directory-stack '()
          sly-repl-package-stack '())
    (sly-repl-update-banner)))

(defun sly-display-output-buffer ()
  "Display the output buffer and scroll to bottom."
  (with-current-buffer (sly-output-buffer)
    (goto-char (point-max))
    (unless (get-buffer-window (current-buffer) t)
      (display-buffer (current-buffer) t))
    (sly-repl-show-maximum-output)))

(defun sly-output-filter (process string)
  (with-current-buffer (process-buffer process)
    (when (and (plusp (length string))
               (eq (process-status sly-buffer-connection) 'open))
      (sly-write-string string))))

(defvar sly-open-stream-hooks)

(defun sly-open-stream-to-lisp (port coding-system)
  (let ((stream (open-network-stream "*lisp-output-stream*"
                                     (sly-with-connection-buffer ()
                                       (current-buffer))
				     (car (process-contact (sly-connection)))
                                     port))
        (emacs-coding-system (car (cl-find coding-system
                                           sly-net-valid-coding-systems
                                           :key #'cl-third))))
    (sly-set-query-on-exit-flag stream)
    (set-process-filter stream 'sly-output-filter)
    (set-process-coding-system stream emacs-coding-system emacs-coding-system)
    (when-let (secret (sly-secret))
      (sly-net-send secret stream))
    (run-hook-with-args 'sly-open-stream-hooks stream)
    stream))

(defun sly-io-speed-test (&optional profile)
  "A simple minded benchmark for stream performance.
If a prefix argument is given, instrument the sly package for
profiling before running the benchmark."
  (interactive "P")
  (eval-and-compile
    (require 'elp))
  (elp-reset-all)
  (elp-restore-all)
  (load "sly.el")
  ;;(byte-compile-file "sly-net.el" t)
  ;;(setq sly-log-events nil)
  (setq sly-enable-evaluate-in-emacs t)
  ;;(setq sly-repl-enable-presentations nil)
  (when profile
    (elp-instrument-package "sly-"))
  (kill-buffer (sly-output-buffer))
  (switch-to-buffer (sly-output-buffer))
  (delete-other-windows)
  (sit-for 0)
  (sly-repl-send-string "(swank:io-speed-test 4000 1)")
  (let ((proc (sly-inferior-process)))
    (when proc
      (display-buffer (process-buffer proc) t)
      (goto-char (point-max)))))

(defvar sly-write-string-function 'sly-repl-write-string)

(defun sly-write-string (string &optional target)
  "Insert STRING in the REPL buffer or some other TARGET.
If TARGET is nil, insert STRING as regular process
output.  If TARGET is :repl-result, insert STRING as the result of the
evaluation.  Other values of TARGET map to an Emacs marker via the
hashtable `sly-output-target-to-marker'; output is inserted at this marker."
  (funcall sly-write-string-function string target))

(defun sly-repl-write-string (string &optional target)
  (case target
    ((nil) (sly-repl-emit string))
    (:repl-result (sly-repl-emit-result string t))
    (t (sly-emit-to-target string target))))

(defvar sly-repl-popup-on-output nil
  "Display the output buffer when some output is written.
This is set to nil after displaying the buffer.")

(defmacro sly-save-marker (marker &rest body)
  (declare (debug (sexp &rest form)))
  (let ((pos (cl-gensym "pos")))
    `(let ((,pos (marker-position ,marker)))
       (prog1 (progn . ,body)
         (set-marker ,marker ,pos)))))

(put 'sly-save-marker 'lisp-indent-function 1)

(defun sly-repl-emit (string)
  ;; insert the string STRING in the output buffer
  (with-current-buffer (sly-output-buffer)
    (save-excursion
      (goto-char sly-output-end)
      (sly-save-marker sly-output-start
        (sly-propertize-region '(face sly-repl-output-face
                                        sly-repl-output t
                                        rear-nonsticky (face))
          (let ((inhibit-read-only t))
	    (insert-before-markers string)
	    (when (and (= (point) sly-repl-prompt-start-mark)
		       (not (bolp)))
	      (insert-before-markers "\n")
	      (set-marker sly-output-end (1- (point))))))))
    (when sly-repl-popup-on-output
      (setq sly-repl-popup-on-output nil)
      (display-buffer (current-buffer)))
    (sly-repl-show-maximum-output)))

(defun sly-repl-emit-result (string &optional bol)
  ;; insert STRING and mark it as evaluation result
  (with-current-buffer (sly-output-buffer)
    (save-excursion
      (goto-char sly-repl-input-start-mark)
      (sly-save-marker sly-output-start
	(goto-char sly-repl-input-start-mark)
	(when (and bol (not (bolp))) (insert-before-markers-and-inherit "\n"))
        (sly-save-marker sly-output-end
          (sly-propertize-region `(face sly-repl-result-face
                                          rear-nonsticky (face))
            (insert-before-markers string)))
        (set-marker sly-output-end (point))))
    (sly-repl-show-maximum-output)))

(defvar sly-last-output-target-id 0
  "The last integer we used as a TARGET id.")

(defvar sly-output-target-to-marker
  (make-hash-table)
  "Map from TARGET ids to Emacs markers.
The markers indicate where output should be inserted.")

(defun sly-output-target-marker (target)
  "Return the marker where output for TARGET should be inserted."
  (case target
    ((nil)
     (with-current-buffer (sly-output-buffer)
       sly-output-end))
    (:repl-result
     (with-current-buffer (sly-output-buffer)
       sly-repl-input-start-mark))
    (t
     (gethash target sly-output-target-to-marker))))

(defun sly-emit-to-target (string target)
  "Insert STRING at target TARGET.
See `sly-output-target-to-marker'."
  (let* ((marker (sly-output-target-marker target))
         (buffer (and marker (marker-buffer marker))))
    (when buffer
      (with-current-buffer buffer
        (save-excursion
          ;; Insert STRING at MARKER, then move MARKER behind
          ;; the insertion.
          (goto-char marker)
          (insert-before-markers string)
          (set-marker marker (point)))))))

(defun sly-switch-to-output-buffer ()
  "Select the output buffer, when possible in an existing window.

Hint: You can use `display-buffer-reuse-frames' and
`special-display-buffer-names' to customize the frame in which
the buffer should appear."
  (interactive)
  (pop-to-buffer (sly-output-buffer))
  (goto-char (point-max)))


;;;; REPL
;;
;; The REPL uses some markers to separate input from output.  The
;; usual configuration is as follows:
;;
;;    ... output ...    ... result ...    prompt> ... input ...
;;    ^            ^                      ^       ^           ^
;;    output-start output-end  prompt-start       input-start point-max
;;
;; input-start is a right inserting marker, because
;; we want it to stay behind when the user inserts text.
;;
;; We maintain the following invariant:
;;
;;  output-start <= output-end <= input-start.
;;
;; This invariant is important, because we must be prepared for
;; asynchronous output and asynchronous reads.  ("Asynchronous" means,
;; triggered by Lisp and not by Emacs.)
;;
;; All output is inserted at the output-end marker.  Some care must be
;; taken when output-end and input-start are at the same position: if
;; we insert at that point, we must move the right markers.  We should
;; also not leave (window-)point in the middle of the new output.  The
;; idiom we use is a combination to sly-save-marker,
;; insert-before-markers, and manually updating window-point
;; afterwards.
;;
;; A "synchronous" evaluation request proceeds as follows: the user
;; inserts some text between input-start and point-max and then hits
;; return.  We send that region to Lisp, move the output and input
;; makers to the line after the input and wait.  When we receive the
;; result, we insert it together with a prompt between the output-end
;; and input-start mark.  See `sly-repl-insert-prompt'.
;;
;; It is possible that some output for such an evaluation request
;; arrives after the result.  This output is inserted before the
;; result (and before the prompt).
;;
;; If we are in "reading" state, e.g., during a call to Y-OR-N-P,
;; there is no prompt between output-end and input-start.
;;

;; FIXME: sly-lisp-package should be local in a REPL buffer
(sly-def-connection-var sly-lisp-package
    "COMMON-LISP-USER"
  "The current package name of the Superior lisp.
This is automatically synchronized from Lisp.")

(sly-def-connection-var sly-lisp-package-prompt-string
    "CL-USER"
  "The current package name of the Superior lisp.
This is automatically synchronized from Lisp.")

(sly-make-variables-buffer-local
 (defvar sly-repl-package-stack nil
   "The stack of packages visited in this repl.")

 (defvar sly-repl-directory-stack nil
   "The stack of default directories associated with this repl.")

 (defvar sly-repl-prompt-start-mark)
 (defvar sly-repl-input-start-mark)
 (defvar sly-repl-old-input-counter 0
   "Counter used to generate unique `sly-repl-old-input' properties.
This property value must be unique to avoid having adjacent inputs be
joined together."))

(defun sly-reset-repl-markers ()
  (dolist (markname '(sly-output-start
                      sly-output-end
                      sly-repl-prompt-start-mark
                      sly-repl-input-start-mark))
    (set markname (make-marker))
    (set-marker (symbol-value markname) (point))))

;;;;; REPL mode setup

(defvar sly-repl-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map lisp-mode-map)
    map))

(sly-define-keys sly-prefix-map
  ("\C-z" 'sly-switch-to-output-buffer)
  ("\M-p" 'sly-repl-set-package))

(sly-define-keys sly-mode-map
  ("\C-c~" 'sly-sync-package-and-default-directory)
  ("\C-c\C-y" 'sly-call-defun)
  ("\C-c\C-j" 'sly-eval-last-expression-in-repl))

(sly-define-keys sly-connection-list-mode-map
  ((kbd "RET") 'sly-goto-connection)
  ([return] 'sly-goto-connection))

(sly-define-keys sly-repl-mode-map
  ("\C-m" 'sly-repl-return)
  ([return] 'sly-repl-return)
  ("\C-j" 'sly-repl-newline-and-indent)
  ("\C-\M-m" 'sly-repl-closing-return)
  ([(control return)] 'sly-repl-closing-return)
  ("\M-p" 'sly-repl-previous-input)
  ((kbd "C-<up>") 'sly-repl-backward-input)
  ("\M-n" 'sly-repl-next-input)
  ((kbd "C-<down>") 'sly-repl-forward-input)
  ("\M-r" 'sly-repl-previous-matching-input)
  ("\M-s" 'sly-repl-next-matching-input)
  ("\C-c\C-c" 'sly-interrupt)
  ("\t" 'sly-indent-and-complete-symbol)
  ("\M-\t" 'sly-complete-symbol)
  (" " 'sly-space)
  ((string sly-repl-shortcut-dispatch-char) 'sly-handle-repl-shortcut)
  ("\C-c\C-o" 'sly-repl-clear-output)
  ("\C-c\M-o" 'sly-repl-clear-buffer)
  ("\C-c\C-u" 'sly-repl-kill-input)
  ("\C-c\C-n" 'sly-repl-next-prompt)
  ("\C-c\C-p" 'sly-repl-previous-prompt)
  ("\C-c\C-z" 'sly-nop)
  ("\C-cI" 'sly-repl-inspect))

(sly-define-keys sly-inspector-mode-map
  ((kbd "M-RET") 'sly-inspector-copy-down-to-repl))

(sly-define-keys sldb-mode-map
  ("\C-y" 'sldb-insert-frame-call-to-repl)
  ((kbd "M-RET") 'sldb-copy-down-to-repl))

(def-sly-selector-method ?r
  "SLY Read-Eval-Print-Loop."
  (sly-output-buffer))

(define-minor-mode sly-repl-map-mode
  "Minor mode which makes sly-repl-mode-map available.
\\{sly-repl-mode-map}"
  nil
  nil
  sly-repl-mode-map)

(defun sly-repl-mode ()
  "Major mode for interacting with a superior Lisp.
\\{sly-repl-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'sly-repl-mode)
  (sly-editing-mode 1)
  (sly-repl-map-mode 1)
  (lisp-mode-variables t)
  (set (make-local-variable 'lisp-indent-function)
       'common-lisp-indent-function)
  (setq font-lock-defaults nil)
  (setq mode-name "REPL")
  (setq sly-current-thread :repl-thread)
  (set (make-local-variable 'scroll-conservatively) 20)
  (set (make-local-variable 'scroll-margin) 0)
  (when sly-repl-history-file
    (sly-repl-safe-load-history)
    (add-hook 'kill-buffer-hook
              'sly-repl-safe-save-merged-history
              'append t))
  (add-hook 'kill-emacs-hook 'sly-repl-save-all-histories)
  (sly-setup-command-hooks)
  ;; At the REPL, we define beginning-of-defun and end-of-defun to be
  ;; the start of the previous prompt or next prompt respectively.
  ;; Notice the interplay with SLY-REPL-BEGINNING-OF-DEFUN.
  (set (make-local-variable 'beginning-of-defun-function)
       'sly-repl-mode-beginning-of-defun)
  (set (make-local-variable 'end-of-defun-function)
       'sly-repl-mode-end-of-defun)
  (run-mode-hooks 'sly-repl-mode-hook))

(defun sly-repl-buffer (&optional create connection)
  "Get the REPL buffer for the current connection; optionally create."
  (funcall (if create #'get-buffer-create #'get-buffer)
           (format "*sly-repl %s*" (sly-connection-name connection))))

(defun sly-repl ()
  (interactive)
  (sly-switch-to-output-buffer))

(defun sly-repl-mode-beginning-of-defun (&optional arg)
  (if (and arg (< arg 0))
      (sly-repl-mode-end-of-defun (- arg))
    (dotimes (i (or arg 1))
      (sly-repl-previous-prompt))))

(defun sly-repl-mode-end-of-defun (&optional arg)
  (if (and arg (< arg 0))
      (sly-repl-mode-beginning-of-defun (- arg))
    (dotimes (i (or arg 1))
      (sly-repl-next-prompt))))

(defun sly-repl-send-string (string &optional command-string)
  (cond (sly-repl-read-mode
         (sly-repl-return-string string))
        (t (sly-repl-eval-string string))))

(defun sly-repl-eval-string (string)
  (sly-rex ()
      ((if sly-repl-auto-right-margin
           `(swank:listener-eval ,string
                                 :window-width
                                 ,(with-current-buffer (sly-output-buffer)
                                    (window-width)))
         `(swank:listener-eval ,string))
       (sly-lisp-package))
    ((:ok result)
     (sly-repl-insert-result result))
    ((:abort condition)
     (sly-repl-show-abort condition))))

(defun sly-repl-insert-result (result)
  (with-current-buffer (sly-output-buffer)
    (save-excursion
      (when result
        (destructure-case result
          ((:values &rest strings)
           (cond ((null strings)
                  (sly-repl-emit-result "; No value\n" t))
                 (t
                  (dolist (s strings)
                    (sly-repl-emit-result s t)))))))
      (sly-repl-insert-prompt))
    (sly-repl-show-maximum-output)))

(defun sly-repl-show-abort (condition)
  (with-current-buffer (sly-output-buffer)
    (save-excursion
      (sly-save-marker sly-output-start
        (sly-save-marker sly-output-end
          (goto-char sly-output-end)
          (insert-before-markers (format "; Evaluation aborted on %s.\n"
                                         condition))
          (sly-repl-insert-prompt))))
    (sly-repl-show-maximum-output)))

(defvar sly-repl-suppress-prompt nil
  "Supresses Slime REPL prompt when bound to T.")

(defun sly-repl-insert-prompt ()
  "Insert the prompt (before markers!).
Set point after the prompt.
Return the position of the prompt beginning.

If `sly-repl-suppress-prompt' is true, does nothing and returns nil."
  (goto-char sly-repl-input-start-mark)
  (unless sly-repl-suppress-prompt
    (sly-save-marker sly-output-start
      (sly-save-marker sly-output-end
        (unless (bolp) (insert-before-markers "\n"))
        (let ((prompt-start (point))
              (prompt (format "%s> " (sly-lisp-package-prompt-string))))
          (sly-propertize-region
              '(face sly-repl-prompt-face
                     read-only t sly-repl-prompt t
                     rear-nonsticky t front-sticky (read-only)
                     inhibit-line-move-field-capture t
                     field output)
            (insert-before-markers prompt))
          (set-marker sly-repl-prompt-start-mark prompt-start)
          (setq buffer-undo-list nil)
          prompt-start)))))

(defun sly-repl-show-maximum-output ()
  "Put the end of the buffer at the bottom of the window."
  (when (eobp)
    (let ((win (if (eq (window-buffer) (current-buffer))
                   (selected-window)
                 (get-buffer-window (current-buffer) t))))
      (when win
        (with-selected-window win
          (set-window-point win (point-max))
          (recenter -1))))))

(defvar sly-repl-current-input-hooks)

(defun sly-repl-current-input (&optional until-point-p)
  "Return the current input as string.
The input is the region from after the last prompt to the end of
buffer."
  (or (run-hook-with-args-until-success 'sly-repl-current-input-hooks
                                        until-point-p)
      (buffer-substring-no-properties sly-repl-input-start-mark
                                      (if until-point-p
                                          (point)
                                        (point-max)))))

(defun sly-property-position (text-property &optional object)
  "Return the first position of TEXT-PROPERTY, or nil."
  (if (get-text-property 0 text-property object)
      0
    (next-single-property-change 0 text-property object)))

(defun sly-mark-input-start ()
  (set-marker sly-repl-input-start-mark (point) (current-buffer)))

(defun sly-mark-output-start ()
  (set-marker sly-output-start (point))
  (set-marker sly-output-end (point)))

(defun sly-mark-output-end ()
  ;; Don't put sly-repl-output-face again; it would remove the
  ;; special presentation face, for instance in the SBCL inspector.
  (add-text-properties sly-output-start sly-output-end
                       '(;;face sly-repl-output-face
                         rear-nonsticky (face))))

(defun sly-preserve-zmacs-region ()
  "In XEmacs, ensure that the zmacs-region stays active after this command."
  (when (boundp 'zmacs-region-stays)
    (set 'zmacs-region-stays t)))

(defun sly-repl-in-input-area-p ()
  (<= sly-repl-input-start-mark (point)))

(defun sly-repl-at-prompt-start-p ()
  ;; This will not work on non-current prompts.
  (= (point) sly-repl-input-start-mark))

(defun sly-repl-beginning-of-defun ()
  "Move to beginning of defun."
  (interactive)
  ;; We call BEGINNING-OF-DEFUN if we're at the start of a prompt
  ;; already, to trigger SLY-REPL-MODE-BEGINNING-OF-DEFUN by means
  ;; of the locally bound BEGINNING-OF-DEFUN-FUNCTION, in order to
  ;; jump to the start of the previous prompt.
  (if (and (not (sly-repl-at-prompt-start-p))
           (sly-repl-in-input-area-p))
      (goto-char sly-repl-input-start-mark)
    (beginning-of-defun))
  t)

;; FIXME: this looks very strange
(defun sly-repl-end-of-defun ()
  "Move to next of defun."
  (interactive)
  ;; C.f. SLY-REPL-BEGINNING-OF-DEFUN.
  (if (and (not (= (point) (point-max)))
           (sly-repl-in-input-area-p))
      (goto-char (point-max))
    (end-of-defun))
  t)

(defun sly-repl-previous-prompt ()
  "Move backward to the previous prompt."
  (interactive)
  (sly-repl-find-prompt t))

(defun sly-repl-next-prompt ()
  "Move forward to the next prompt."
  (interactive)
  (sly-repl-find-prompt))

(defun sly-repl-find-prompt (&optional backward)
  (let ((origin (point))
        (prop 'sly-repl-prompt))
    (while (progn
             (sly-search-property-change prop backward)
             (not (or (sly-end-of-proprange-p prop) (bobp) (eobp)))))
    (unless (sly-end-of-proprange-p prop)
      (goto-char origin))))

(defun sly-search-property-change (prop &optional backward)
  (cond (backward
         (goto-char (or (previous-single-char-property-change (point) prop)
			(point-min))))
        (t
         (goto-char (or (next-single-char-property-change (point) prop)
			(point-max))))))

(defun sly-end-of-proprange-p (property)
  (and (get-char-property (max 1 (1- (point))) property)
       (not (get-char-property (point) property))))

(defvar sly-repl-return-hooks)

(defun sly-repl-return (&optional end-of-input)
  "Evaluate the current input string, or insert a newline.
Send the current input only if a whole expression has been entered,
i.e. the parenthesis are matched.

With prefix argument send the input even if the parenthesis are not
balanced."
  (interactive "P")
  (sly-check-connected)
  (cond (end-of-input
         (sly-repl-send-input))
        (sly-repl-read-mode ; bad style?
         (sly-repl-send-input t))
        ((and (get-text-property (point) 'sly-repl-old-input)
              (< (point) sly-repl-input-start-mark))
         (sly-repl-grab-old-input end-of-input)
         (sly-repl-recenter-if-needed))
        ((run-hook-with-args-until-success 'sly-repl-return-hooks end-of-input))
        ((sly-input-complete-p sly-repl-input-start-mark (point-max))
         (sly-repl-send-input t))
        (t
         (sly-repl-newline-and-indent)
         (message "[input not complete]"))))

(defun sly-repl-recenter-if-needed ()
  "Make sure that (point) is visible."
  (unless (pos-visible-in-window-p (point-max))
    (save-excursion
      (goto-char (point-max))
      (recenter -1))))

(defun sly-repl-send-input (&optional newline)
  "Goto to the end of the input and send the current input.
If NEWLINE is true then add a newline at the end of the input."
  (unless (sly-repl-in-input-area-p)
    (error "No input at point."))
  (goto-char (point-max))
  (let ((end (point))) ; end of input, without the newline
    (sly-repl-add-to-input-history
     (buffer-substring sly-repl-input-start-mark end))
    (when newline
      (insert "\n")
      (sly-repl-show-maximum-output))
    (let ((inhibit-modification-hooks t))
      (add-text-properties sly-repl-input-start-mark
                           (point)
                           `(sly-repl-old-input
                             ,(incf sly-repl-old-input-counter))))
    (let ((overlay (make-overlay sly-repl-input-start-mark end)))
      ;; These properties are on an overlay so that they won't be taken
      ;; by kill/yank.
      (overlay-put overlay 'face 'sly-repl-input-face)))
  (let ((input (sly-repl-current-input)))
    (goto-char (point-max))
    (sly-mark-input-start)
    (sly-mark-output-start)
    (sly-repl-send-string input)))

(defun sly-repl-grab-old-input (replace)
  "Resend the old REPL input at point.
If replace is non-nil the current input is replaced with the old
input; otherwise the new input is appended.  The old input has the
text property `sly-repl-old-input'."
  (multiple-value-bind (beg end) (sly-property-bounds 'sly-repl-old-input)
    (let ((old-input (buffer-substring beg end)) ;;preserve
          ;;properties, they will be removed later
          (offset (- (point) beg)))
      ;; Append the old input or replace the current input
      (cond (replace (goto-char sly-repl-input-start-mark))
            (t (goto-char (point-max))
               (unless (eq (char-before) ?\ )
                 (insert " "))))
      (delete-region (point) (point-max))
      (save-excursion
        (insert old-input)
        (when (equal (char-before) ?\n)
          (delete-char -1)))
      (forward-char offset))))

(defun sly-repl-closing-return ()
  "Evaluate the current input string after closing all open lists."
  (interactive)
  (goto-char (point-max))
  (save-restriction
    (narrow-to-region sly-repl-input-start-mark (point))
    (while (ignore-errors (save-excursion (backward-up-list 1)) t)
      (insert ")")))
  (sly-repl-return))

(defun sly-repl-newline-and-indent ()
  "Insert a newline, then indent the next line.
Restrict the buffer from the prompt for indentation, to avoid being
confused by strange characters (like unmatched quotes) appearing
earlier in the buffer."
  (interactive)
  (save-restriction
    (narrow-to-region sly-repl-prompt-start-mark (point-max))
    (insert "\n")
    (lisp-indent-line)))

(defun sly-repl-delete-current-input ()
  "Delete all text from the prompt."
  (interactive)
  (delete-region sly-repl-input-start-mark (point-max)))

(defun sly-eval-last-expression-in-repl (prefix)
  "Evaluates last expression in the Slime REPL.

Switches REPL to current package of the source buffer for the duration. If
used with a prefix argument (C-u), doesn't switch back afterwards."
  (interactive "P")
  (let ((expr (sly-last-expression))
        (buffer-name (buffer-name (current-buffer)))
        (new-package (sly-current-package))
        (old-package (sly-lisp-package))
        (sly-repl-suppress-prompt t)
        (yank-back nil))
    (with-current-buffer (sly-output-buffer)
      (unless (eq (current-buffer) (window-buffer))
        (pop-to-buffer (current-buffer) t))
      (goto-char (point-max))
      ;; Kill pending input in the REPL
      (when (< (marker-position sly-repl-input-start-mark) (point))
        (kill-region sly-repl-input-start-mark (point))
        (setq yank-back t))
      (unwind-protect
          (progn
            (insert-before-markers (format "\n;;; from %s\n" buffer-name))
            (when new-package
              (sly-repl-set-package new-package))
            (let ((sly-repl-suppress-prompt nil))
              (sly-repl-insert-prompt))
            (insert expr)
            (sly-repl-return))
        (unless (or prefix (equal (sly-lisp-package) old-package))
          ;; Switch back.
          (sly-repl-set-package old-package)
          (let ((sly-repl-suppress-prompt nil))
            (sly-repl-insert-prompt))))
      ;; Put pending input back.
      (when yank-back
        (yank)))))

(defun sly-repl-kill-input ()
  "Kill all text from the prompt to point."
  (interactive)
  (cond ((< (marker-position sly-repl-input-start-mark) (point))
         (kill-region sly-repl-input-start-mark (point)))
        ((= (point) (marker-position sly-repl-input-start-mark))
         (sly-repl-delete-current-input))))

(defun sly-repl-replace-input (string)
  (sly-repl-delete-current-input)
  (insert-and-inherit string))

(defun sly-repl-input-line-beginning-position ()
  (save-excursion
    (goto-char sly-repl-input-start-mark)
    (line-beginning-position)))

(defun sly-clear-repl-variables ()
  (interactive)
  (sly-eval-async `(swank:clear-repl-variables)))

(defvar sly-repl-clear-buffer-hook)

(add-hook 'sly-repl-clear-buffer-hook 'sly-clear-repl-variables)

(defun sly-repl-clear-buffer ()
  "Delete the output generated by the Lisp process."
  (interactive)
  (let ((inhibit-read-only t))
    (delete-region (point-min) sly-repl-prompt-start-mark)
    (delete-region sly-output-start sly-output-end)
    (when (< (point) sly-repl-input-start-mark)
      (goto-char sly-repl-input-start-mark))
    (recenter t))
  (run-hooks 'sly-repl-clear-buffer-hook))

(defun sly-repl-clear-output ()
  "Delete the output inserted since the last input."
  (interactive)
  (let ((start (save-excursion
                 (when (>= (point) sly-repl-input-start-mark)
                   (goto-char sly-repl-input-start-mark))
                 (sly-repl-previous-prompt)
                 (ignore-errors (forward-sexp))
                 (forward-line)
                 (point)))
        (end (1- (sly-repl-input-line-beginning-position))))
    (when (< start end)
      (let ((inhibit-read-only t))
        (delete-region start end)
        (save-excursion
          (goto-char start)
          (insert ";;; output flushed"))))))

(defun sly-repl-set-package (package)
  "Set the package of the REPL buffer to PACKAGE."
  (interactive (list (let* ((p (sly-current-package))
                            (p (and p (sly-pretty-package-name p)))
                            (p (and (not (equal p (sly-lisp-package))) p)))
                       (sly-read-package-name "Package: " p))))
  (with-current-buffer (sly-output-buffer)
    (let ((previouse-point (- (point) sly-repl-input-start-mark))
          (previous-prompt (sly-lisp-package-prompt-string)))
      (destructuring-bind (name prompt-string)
          (sly-repl-shortcut-eval `(swank:set-package ,package))
        (setf (sly-lisp-package) name)
        (setf sly-buffer-package name)
        (unless (equal previous-prompt prompt-string)
          (setf (sly-lisp-package-prompt-string) prompt-string)
          (sly-repl-insert-prompt))
        (when (plusp previouse-point)
          (goto-char (+ previouse-point sly-repl-input-start-mark)))))))


;;;;; History

(defcustom sly-repl-wrap-history nil
  "*T to wrap history around when the end is reached."
  :type 'boolean
  :group 'sly-repl)

(defcustom sly-repl-history-remove-duplicates nil
  "*When T all duplicates are removed except the last one."
  :type 'boolean
  :group 'sly-repl)

(defcustom sly-repl-history-trim-whitespaces nil
  "*When T strip all whitespaces from the beginning and end."
  :type 'boolean
  :group 'sly-repl)

(make-variable-buffer-local
 (defvar sly-repl-input-history '()
   "History list of strings read from the REPL buffer."))

(defun sly-repl-add-to-input-history (string)
  "Add STRING to the input history.
Empty strings and duplicates are ignored."
  (when sly-repl-history-trim-whitespaces
    (setq string (replace-regexp-in-string "[\t\n\s]*\\(.*?\\)[\t\n\s]*"
                                           "\\1"
                                           string)))
  (unless (equal string "")
    (when sly-repl-history-remove-duplicates
      (setq sly-repl-input-history
            (remove string sly-repl-input-history)))
    (unless (equal string (car sly-repl-input-history))
      (push string sly-repl-input-history))))

;; These two vars contain the state of the last history search.  We
;; only use them if `last-command' was 'sly-repl-history-replace,
;; otherwise we reinitialize them.

(defvar sly-repl-input-history-position -1
  "Newer items have smaller indices.")

(defvar sly-repl-history-pattern nil
  "The regexp most recently used for finding input history.")

(defun sly-repl-history-replace (direction &optional regexp)
  "Replace the current input with the next line in DIRECTION.
DIRECTION is 'forward' or 'backward' (in the history list).
If REGEXP is non-nil, only lines matching REGEXP are considered."
  (setq sly-repl-history-pattern regexp)
  (let* ((min-pos -1)
         (max-pos (length sly-repl-input-history))
         (pos0 (cond ((sly-repl-history-search-in-progress-p)
                      sly-repl-input-history-position)
                     (t min-pos)))
         (pos (sly-repl-position-in-history pos0 direction (or regexp "")
                                              (sly-repl-current-input)))
         (msg nil))
    (cond ((and (< min-pos pos) (< pos max-pos))
           (sly-repl-replace-input (nth pos sly-repl-input-history))
           (setq msg (format "History item: %d" pos)))
          ((not sly-repl-wrap-history)
           (setq msg (cond ((= pos min-pos) "End of history")
                           ((= pos max-pos) "Beginning of history"))))
          (sly-repl-wrap-history
           (setq pos (if (= pos min-pos) max-pos min-pos))
           (setq msg "Wrapped history")))
    (when (or (<= pos min-pos) (<= max-pos pos))
      (when regexp
        (setq msg (concat msg "; no matching item"))))
    ;;(message "%s [%d %d %s]" msg start-pos pos regexp)
    (message "%s%s" msg (cond ((not regexp) "")
                              (t (format "; current regexp: %s" regexp))))
    (setq sly-repl-input-history-position pos)
    (setq this-command 'sly-repl-history-replace)))

(defun sly-repl-history-search-in-progress-p ()
  (eq last-command 'sly-repl-history-replace))

(defun sly-repl-terminate-history-search ()
  (setq last-command this-command))

(defun sly-repl-position-in-history (start-pos direction regexp
                                                 &optional exclude-string)
  "Return the position of the history item matching REGEXP.
Return -1 resp. the length of the history if no item matches.
If EXCLUDE-STRING is specified then it's excluded from the search."
  ;; Loop through the history list looking for a matching line
  (let* ((step (ecase direction
                 (forward -1)
                 (backward 1)))
         (history sly-repl-input-history)
         (len (length history)))
    (loop for pos = (+ start-pos step) then (+ pos step)
          if (< pos 0) return -1
          if (<= len pos) return len
          for history-item = (nth pos history)
          if (and (string-match regexp history-item)
                  (not (equal history-item exclude-string)))
          return pos)))

(defun sly-repl-previous-input ()
  "Cycle backwards through input history.
If the `last-command' was a history navigation command use the
same search pattern for this command.
Otherwise use the current input as search pattern."
  (interactive)
  (sly-repl-history-replace 'backward (sly-repl-history-pattern t)))

(defun sly-repl-next-input ()
  "Cycle forwards through input history.
See `sly-repl-previous-input'."
  (interactive)
  (sly-repl-history-replace 'forward (sly-repl-history-pattern t)))

(defun sly-repl-forward-input ()
  "Cycle forwards through input history."
  (interactive)
  (sly-repl-history-replace 'forward (sly-repl-history-pattern)))

(defun sly-repl-backward-input ()
  "Cycle backwards through input history."
  (interactive)
  (sly-repl-history-replace 'backward (sly-repl-history-pattern)))

(defun sly-repl-previous-matching-input (regexp)
  (interactive (list (sly-read-from-minibuffer
		      "Previous element matching (regexp): ")))
  (sly-repl-terminate-history-search)
  (sly-repl-history-replace 'backward regexp))

(defun sly-repl-next-matching-input (regexp)
  (interactive (list (sly-read-from-minibuffer
		      "Next element matching (regexp): ")))
  (sly-repl-terminate-history-search)
  (sly-repl-history-replace 'forward regexp))

(defun sly-repl-history-pattern (&optional use-current-input)
  "Return the regexp for the navigation commands."
  (cond ((sly-repl-history-search-in-progress-p)
         sly-repl-history-pattern)
        (use-current-input
         (goto-char (max sly-repl-input-start-mark (point)))
         (let ((str (sly-repl-current-input t)))
           (cond ((string-match "^[ \t\n]*$" str) nil)
                 (t (concat "^" (regexp-quote str))))))
        (t nil)))

(defun sly-repl-delete-from-input-history (string)
  "Delete STRING from the repl input history.

When string is not provided then clear the current repl input and
use it as an input.  This is useful to get rid of unwanted repl
history entries while navigating the repl history."
  (interactive (list (sly-repl-current-input)))
  (let ((merged-history
         (sly-repl-merge-histories (sly-repl-read-history nil t)
                                     sly-repl-input-history)))
    (setq sly-repl-input-history
          (cl-delete string merged-history :test #'string=))
    (sly-repl-save-history))
  (sly-repl-delete-current-input))

;;;;; Persistent History

(defun sly-repl-merge-histories (old-hist new-hist)
  "Merge entries from OLD-HIST and NEW-HIST."
  ;; Newer items in each list are at the beginning.
  (let* ((ht (make-hash-table :test #'equal))
         (test (lambda (entry)
                 (or (gethash entry ht)
                     (progn (setf (gethash entry ht) t)
                            nil)))))
    (append (cl-remove-if test new-hist)
            (cl-remove-if test old-hist))))

(defun sly-repl-load-history (&optional filename)
  "Set the current SLY REPL history.
It can be read either from FILENAME or `sly-repl-history-file' or
from a user defined filename."
  (interactive (list (sly-repl-read-history-filename)))
  (let ((file (or filename sly-repl-history-file)))
    (setq sly-repl-input-history (sly-repl-read-history file t))))

(defun sly-repl-read-history (&optional filename noerrer)
  "Read and return the history from FILENAME.
The default value for FILENAME is `sly-repl-history-file'.
If NOERROR is true return and the file doesn't exits return nil."
  (let ((file (or filename sly-repl-history-file)))
    (cond ((not (file-readable-p file)) '())
          (t (with-temp-buffer
               (insert-file-contents file)
               (read (current-buffer)))))))

(defun sly-repl-read-history-filename ()
  (read-file-name "Use SLY REPL history from file: "
                  sly-repl-history-file))

(defun sly-repl-save-merged-history (&optional filename)
  "Read the history file, merge the current REPL history and save it.
This tries to be smart in merging the history from the file and the
current history in that it tries to detect the unique entries using
`sly-repl-merge-histories'."
  (interactive (list (sly-repl-read-history-filename)))
  (let ((file (or filename sly-repl-history-file)))
    (with-temp-message "saving history..."
      (let ((hist (sly-repl-merge-histories (sly-repl-read-history file t)
                                              sly-repl-input-history)))
        (sly-repl-save-history file hist)))))

(defun sly-repl-save-history (&optional filename history)
  "Simply save the current SLY REPL history to a file.
When SLY is setup to always load the old history and one uses only
one instance of sly all the time, there is no need to merge the
files and this function is sufficient.

When the list is longer than `sly-repl-history-size' it will be
truncated.  That part is untested, though!"
  (interactive (list (sly-repl-read-history-filename)))
  (let ((file (or filename sly-repl-history-file))
        (hist (or history sly-repl-input-history)))
    (unless (file-writable-p file)
      (error (format "History file not writable: %s" file)))
    (let ((hist (cl-subseq hist 0 (min (length hist) sly-repl-history-size))))
      ;;(message "saving %s to %s\n" hist file)
      (with-temp-file file
        (let ((cs sly-repl-history-file-coding-system)
              (print-length nil) (print-level nil))
          (setq buffer-file-coding-system cs)
          (insert (format ";; -*- coding: %s -*-\n" cs))
          (insert ";; History for SLY REPL. Automatically written.\n"
                  ";; Edit only if you know what you're doing\n")
          (prin1 (mapcar #'substring-no-properties hist) (current-buffer)))))))

(defun sly-repl-save-all-histories ()
  "Save the history in each repl buffer."
  (dolist (b (buffer-list))
    (with-current-buffer b
      (when (eq major-mode 'sly-repl-mode)
        (sly-repl-safe-save-merged-history)))))

(defun sly-repl-safe-save-merged-history ()
  (sly-repl-call-with-handler
   #'sly-repl-save-merged-history
   "%S while saving the history. Continue? "))

(defun sly-repl-safe-load-history ()
  (sly-repl-call-with-handler
   #'sly-repl-load-history
   "%S while loading the history. Continue? "))

(defun sly-repl-call-with-handler (fun query)
  "Call FUN in the context of an error handler.
The handler will use qeuery to ask the use if the error should be ingored."
  (condition-case err
      (funcall fun)
    (error
     (if (y-or-n-p (format query (error-message-string err)))
         nil
       (signal (car err) (cdr err))))))


;;;;; REPL Read Mode

(defvar sly-repl-read-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-m" 'sly-repl-return)
    (define-key map [return] 'sly-repl-return)
    (define-key map "\C-c\C-b" 'sly-repl-read-break)
    (define-key map "\C-c\C-c" 'sly-repl-read-break)
    (define-key map [remap sly-indent-and-complete-symbol] 'ignore)
    (define-key map [remap sly-handle-repl-shortcut] 'self-insert-command)
    map))

(define-minor-mode sly-repl-read-mode
  "Mode to read input from Emacs
\\{sly-repl-read-mode-map}"
  nil
  "[read]")

(make-variable-buffer-local
 (defvar sly-read-string-threads nil))

(make-variable-buffer-local
 (defvar sly-read-string-tags nil))

(defun sly-repl-read-string (thread tag)
  (sly-switch-to-output-buffer)
  (push thread sly-read-string-threads)
  (push tag sly-read-string-tags)
  (goto-char (point-max))
  (sly-mark-output-end)
  (sly-mark-input-start)
  (sly-repl-read-mode 1))

(defun sly-repl-return-string (string)
  (sly-dispatch-event `(:emacs-return-string
                          ,(pop sly-read-string-threads)
                          ,(pop sly-read-string-tags)
                          ,string))
  (sly-repl-read-mode -1))

(defun sly-repl-read-break ()
  (interactive)
  (sly-dispatch-event `(:emacs-interrupt ,(car sly-read-string-threads))))

(defun sly-repl-abort-read (thread tag)
  (with-current-buffer (sly-output-buffer)
    (pop sly-read-string-threads)
    (pop sly-read-string-tags)
    (sly-repl-read-mode -1)
    (message "Read aborted")))


;;;;; REPL handlers

(cl-defstruct (sly-repl-shortcut (:conc-name sly-repl-shortcut.))
  symbol names handler one-liner)

(defvar sly-repl-shortcut-table nil
  "A list of sly-repl-shortcuts")

(defvar sly-repl-shortcut-history '()
  "History list of shortcut command names.")

(defvar sly-within-repl-shortcut-handler-p nil
  "Bound to T if we're in a REPL shortcut handler invoked from the REPL.")

(defun sly-handle-repl-shortcut ()
  (interactive)
  (if (> (point) sly-repl-input-start-mark)
      (insert (string sly-repl-shortcut-dispatch-char))
    (let ((shortcut (sly-lookup-shortcut
                     (completing-read "Command: "
                                      (sly-bogus-completion-alist
                                       (sly-list-all-repl-shortcuts))
                                      nil t nil
                                      'sly-repl-shortcut-history))))
      (with-struct (sly-repl-shortcut. handler) shortcut
        (let ((sly-within-repl-shortcut-handler-p t))
          (call-interactively handler))))))

(defun sly-list-all-repl-shortcuts ()
  (loop for shortcut in sly-repl-shortcut-table
        append (sly-repl-shortcut.names shortcut)))

(defun sly-lookup-shortcut (name)
  (cl-find-if (lambda (s) (member name (sly-repl-shortcut.names s)))
              sly-repl-shortcut-table))

(defmacro defsly-repl-shortcut (elisp-name names &rest options)
  "Define a new repl shortcut. ELISP-NAME is a symbol specifying
the name of the interactive function to create, or NIL if no
function should be created.

NAMES is a list of \(full-name . aliases\).

OPTIONS is an plist specifying the handler doing the actual work
of the shortcut \(`:handler'\), and a help text \(`:one-liner'\)."
  `(progn
     ,(when elisp-name
        `(defun ,elisp-name ()
           (interactive)
           (call-interactively ,(second (assoc :handler options)))))
     (let ((new-shortcut (make-sly-repl-shortcut
                          :symbol ',elisp-name
                          :names (list ,@names)
                          ,@(apply #'append options))))
       (setq sly-repl-shortcut-table
             (cl-remove-if (lambda (s)
                             (member ',(car names) (sly-repl-shortcut.names s)))
                           sly-repl-shortcut-table))
       (push new-shortcut sly-repl-shortcut-table)
       ',elisp-name)))

(defun sly-repl-shortcut-eval (sexp &optional package)
  "This function should be used by REPL shortcut handlers instead
of `sly-eval' to evaluate their final expansion. (This
expansion will be added to the REPL's history.)"
  (when sly-within-repl-shortcut-handler-p ; were we invoked via ,foo?
    (sly-repl-add-to-input-history (prin1-to-string sexp)))
  (sly-eval sexp package))

(defun sly-repl-shortcut-eval-async (sexp &optional cont package)
  "This function should be used by REPL shortcut handlers instead
of `sly-eval-async' to evaluate their final expansion. (This
expansion will be added to the REPL's history.)"
  (when sly-within-repl-shortcut-handler-p ; were we invoked via ,foo?
    (sly-repl-add-to-input-history (prin1-to-string sexp)))
  (sly-eval-async sexp cont package))

(defun sly-list-repl-short-cuts ()
  (interactive)
  (sly-with-popup-buffer ((sly-buffer-name :repl-help))
    (let ((table (cl-sort (cl-copy-list sly-repl-shortcut-table) #'string<
                          :key (lambda (x)
                                 (car (sly-repl-shortcut.names x))))))
      (save-excursion
        (dolist (shortcut table)
          (let ((names (sly-repl-shortcut.names shortcut)))
            (insert (pop names)) ;; first print the "full" name
            (when names
              ;; we also have aliases
              (insert " (aka ")
              (while (cdr names)
                (insert (pop names) ", "))
              (insert (car names) ")"))
            (when (sly-repl-shortcut.one-liner shortcut)
              (insert "\n     " (sly-repl-shortcut.one-liner shortcut)))
            (insert "\n")))))))

(defun sly-save-some-lisp-buffers ()
  (if sly-repl-only-save-lisp-buffers
      (save-some-buffers nil (lambda ()
                               (and (memq major-mode sly-lisp-modes)
                                    (not (null buffer-file-name)))))
    (save-some-buffers)))

(defun sly-kill-all-buffers ()
  "Kill all the SLY-related buffers."
  (dolist (buf (buffer-list))
    (when (or (string= (buffer-name buf) sly-event-buffer-name)
              (string-match "^\\*inferior-lisp*" (buffer-name buf))
              (string-match "^\\*sly-repl .*\\*$" (buffer-name buf))
              (string-match "^\\*sldb .*\\*$" (buffer-name buf))
              (string-match "^\\*SLY.*\\*$" (buffer-name buf)))
      (kill-buffer buf))))

(defsly-repl-shortcut sly-repl-shortcut-help ("help")
  (:handler 'sly-list-repl-short-cuts)
  (:one-liner "Display the help."))

(defsly-repl-shortcut nil ("change-directory" "!d" "cd")
  (:handler 'sly-set-default-directory)
  (:one-liner "Change the current directory."))

(defsly-repl-shortcut nil ("pwd")
  (:handler (lambda ()
              (interactive)
              (let ((dir (sly-eval `(swank:default-directory))))
                (message "Directory %s" dir))))
  (:one-liner "Show the current directory."))

(defsly-repl-shortcut sly-repl-push-directory
  ("push-directory" "+d" "pushd")
  (:handler (lambda (directory)
              (interactive
               (list (read-directory-name
                      "Push directory: "
                      (sly-eval '(swank:default-directory))
                      nil nil "")))
              (push (sly-eval '(swank:default-directory))
                    sly-repl-directory-stack)
              (sly-set-default-directory directory)))
  (:one-liner "Save the current directory and set it to a new one."))

(defsly-repl-shortcut sly-repl-pop-directory
  ("pop-directory" "-d" "popd")
  (:handler (lambda ()
              (interactive)
              (if (null sly-repl-directory-stack)
                  (message "Directory stack is empty.")
                (sly-set-default-directory
                 (pop sly-repl-directory-stack)))))
  (:one-liner "Restore the last saved directory."))

(defsly-repl-shortcut nil ("change-package" "!p" "in-package" "in")
  (:handler 'sly-repl-set-package)
  (:one-liner "Change the current package."))

(defsly-repl-shortcut sly-repl-push-package ("push-package" "+p")
  (:handler (lambda (package)
              (interactive (list (sly-read-package-name "Package: ")))
              (push (sly-lisp-package) sly-repl-package-stack)
              (sly-repl-set-package package)))
  (:one-liner "Save the current package and set it to a new one."))

(defsly-repl-shortcut sly-repl-pop-package ("pop-package" "-p")
  (:handler (lambda ()
              (interactive)
              (if (null sly-repl-package-stack)
                  (message "Package stack is empty.")
                (sly-repl-set-package
                 (pop sly-repl-package-stack)))))
  (:one-liner "Restore the last saved package."))

(defsly-repl-shortcut sly-repl-resend ("resend-form")
  (:handler (lambda ()
              (interactive)
              (insert (car sly-repl-input-history))
              (insert "\n")
              (sly-repl-send-input)))
  (:one-liner "Resend the last form."))

(defsly-repl-shortcut sly-repl-disconnect ("disconnect")
  (:handler 'sly-disconnect)
  (:one-liner "Disconnect the current connection."))

(defsly-repl-shortcut sly-repl-disconnect-all ("disconnect-all")
  (:handler 'sly-disconnect-all)
  (:one-liner "Disconnect all connections."))

(defsly-repl-shortcut sly-repl-sayoonara ("sayoonara")
  (:handler (lambda ()
              (interactive)
              (when (sly-connected-p)
                (sly-quit-lisp))
              (sly-kill-all-buffers)))
  (:one-liner "Quit all Lisps and close all SLY buffers."))

(defsly-repl-shortcut sly-repl-quit ("quit")
  (:handler (lambda ()
	      (interactive)
              ;; `sly-quit-lisp' determines the connection to quit
              ;; on behalf of the REPL's `sly-buffer-connection'.
              (let ((repl-buffer (sly-output-buffer)))
                (sly-quit-lisp)
                (kill-buffer repl-buffer))))
  (:one-liner "Quit the current Lisp."))

(defsly-repl-shortcut sly-repl-defparameter ("defparameter" "!")
  (:handler (lambda (name value)
              (interactive (list (sly-read-symbol-name "Name (symbol): " t)
                                 (sly-read-from-minibuffer "Value: " "*")))
              (insert "(cl:defparameter " name " " value
                      " \"REPL generated global variable.\")")
              (sly-repl-send-input t)))
  (:one-liner "Define a new global, special, variable."))

(defsly-repl-shortcut sly-repl-compile-and-load ("compile-and-load" "cl")
  (:handler (lambda (filename)
              (interactive (list (expand-file-name
                                  (read-file-name "File: " nil nil nil nil))))
              (sly-save-some-lisp-buffers)
              (sly-repl-shortcut-eval-async
               `(swank:compile-file-if-needed
                 ,(sly-to-lisp-filename filename) t)
               #'sly-compilation-finished)))
  (:one-liner "Compile (if neccessary) and load a lisp file."))

(defsly-repl-shortcut nil  ("restart-inferior-lisp")
  (:handler 'sly-restart-inferior-lisp)
  (:one-liner "Restart *inferior-lisp* and reconnect SLY."))

(defun sly-redirect-inferior-output (&optional noerror)
  "Redirect output of the inferior-process to the REPL buffer."
  (interactive)
  (let ((proc (sly-inferior-process)))
    (cond (proc
           (let ((filter (sly-rcurry #'sly-inferior-output-filter
                                       (sly-current-connection))))
             (set-process-filter proc filter)))
	  (noerror)
	  (t (error "No inferior lisp process")))))

(defun sly-inferior-output-filter (proc string conn)
  (cond ((eq (process-status conn) 'closed)
         (message "Connection closed.  Removing inferior output filter.")
         (message "Lost output: %S" string)
         (set-process-filter proc nil))
        (t
         (sly-output-filter conn string))))

(defun sly-redirect-trace-output ()
  "Redirect the trace output to a separate Emacs buffer."
  (interactive)
  (let ((buffer (get-buffer-create (sly-buffer-name :trace))))
    (with-current-buffer buffer
      (let ((marker (copy-marker (buffer-size)))
            (target (incf sly-last-output-target-id)))
        (puthash target marker sly-output-target-to-marker)
        (sly-eval `(swank:redirect-trace-output ,target))))
    ;; Note: We would like the entries in
    ;; sly-output-target-to-marker to disappear when the buffers are
    ;; killed.  We cannot just make the hash-table ":weakness 'value"
    ;; -- there is no reference from the buffers to the markers in the
    ;; buffer, so entries would disappear even though the buffers are
    ;; alive.  Best solution might be to make buffer-local variables
    ;; that keep the markers. --mkoeppe
    (pop-to-buffer buffer)))

(defun sly-call-defun ()
  "Insert a call to the toplevel form defined around point into the REPL."
  (interactive)
  (cl-labels ((insert-call
               (name &key (function t)
                     defclass)
               (let* ((setf (and function
                                 (consp name)
                                 (= (length name) 2)
                                 (eql (car name) 'setf)))
                      (symbol (if setf
                                  (cadr name)
                                name))
                      (qualified-symbol-name
                       (sly-qualify-cl-symbol-name symbol))
                      (symbol-name (sly-cl-symbol-name qualified-symbol-name))
                      (symbol-package (sly-cl-symbol-package
                                       qualified-symbol-name))
                      (call (if (cl-equalp (sly-lisp-package) symbol-package)
                                symbol-name
                              qualified-symbol-name)))
                 (sly-switch-to-output-buffer)
                 (goto-char sly-repl-input-start-mark)
                 (insert (if function
                             "("
                           " "))
                 (when setf
                   (insert "setf ("))
                 (if defclass
                     (insert "make-instance '"))
                 (insert call)
                 (cond (setf
                        (insert " ")
                        (save-excursion (insert ") )")))
                       (function
                        (insert " ")
                        (save-excursion (insert ")"))))
                 (unless function
                   (goto-char sly-repl-input-start-mark)))))
    (let ((toplevel (sly-parse-toplevel-form)))
      (if (symbolp toplevel)
          (error "Not in a function definition")
        (destructure-case toplevel
          (((:defun :defgeneric :defmacro :define-compiler-macro) symbol)
           (insert-call symbol))
          ((:defmethod symbol &rest args)
           (declare (ignore args))
           (insert-call symbol))
          (((:defparameter :defvar :defconstant) symbol)
           (insert-call symbol :function nil))
          (((:defclass) symbol)
           (insert-call symbol :defclass t))
          (t
           (error "Not in a function definition")))))))

(defun sly-inspector-copy-down-to-repl (number)
  "Evaluate the inspector slot at point via the REPL (to set `*')."
  (interactive (list (or (get-text-property (point) 'sly-part-number)
                         (error "No part at point"))))
  (sly-repl-send-string
   (format "%s" `(cl:nth-value 0 (swank:inspector-nth-part ,number))))
  (sly-repl))

(defun sldb-copy-down-to-repl (frame-id var-id)
  "Evaluate the frame var at point via the REPL (to set `*')."
  (interactive (list (sldb-frame-number-at-point) (sldb-var-number-at-point)))
  (sly-repl-send-string (format "%s"
                                  `(swank-backend:frame-var-value
                                    ,frame-id ,var-id)))
  (sly-repl))

(defun sldb-insert-frame-call-to-repl ()
  "Insert a call to a frame at point."
  (interactive)
  (let ((call (sly-eval `(swank-backend::frame-call
                            ,(sldb-frame-number-at-point)))))
    (sly-switch-to-output-buffer)
    (if (>= (point) sly-repl-prompt-start-mark)
        (insert call)
      (save-excursion
        (goto-char (point-max))
        (insert call))))
  (sly-repl))

(defun sly-set-default-directory (directory)
  "Make DIRECTORY become Lisp's current directory."
  (interactive (list (read-directory-name "Directory: " nil nil t)))
  (let ((dir (expand-file-name directory)))
    (message "default-directory: %s"
             (sly-from-lisp-filename
              (sly-repl-shortcut-eval `(swank:set-default-directory
                                          ,(sly-to-lisp-filename dir)))))
    (with-current-buffer (sly-output-buffer)
      (setq default-directory dir))))

(defun sly-sync-package-and-default-directory ()
  "Set Lisp's package and directory to the values in current buffer."
  (interactive)
  (let* ((package (sly-current-package))
         (exists-p (or (null package)
                       (sly-eval `(cl:packagep
                                     (swank::guess-package ,package)))))
         (directory default-directory))
    (when (and package exists-p)
      (sly-repl-set-package package))
    (sly-set-default-directory directory)
    ;; Sync *inferior-lisp* dir
    (let* ((proc (sly-process))
           (buffer (and proc (process-buffer proc))))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (setq default-directory directory))))
    (message "package: %s%s  directory: %s"
             (with-current-buffer (sly-output-buffer)
               (sly-lisp-package))
             (if exists-p "" (format " (package %s doesn't exist)" package))
             directory)))

(defun sly-goto-connection ()
  "Switch to the REPL buffer for the connection at point."
  (interactive)
  (let ((sly-dispatching-connection (sly-connection-at-point)))
    (switch-to-buffer (sly-output-buffer))))

(defun sly-repl-inside-string-or-comment-p ()
  (save-restriction
    (when (and (boundp 'sly-repl-input-start-mark)
               sly-repl-input-start-mark
               (>= (point) sly-repl-input-start-mark))
      (narrow-to-region sly-repl-input-start-mark (point)))
    (sly-inside-string-or-comment-p)))

(defvar sly-repl-easy-menu
  (let ((C '(sly-connected-p)))
    `("REPL"
      [ "Send Input"             sly-repl-return ,C ]
      [ "Close and Send Input "  sly-repl-closing-return ,C ]
      [ "Interrupt Lisp process" sly-interrupt ,C ]
      "--"
      [ "Previous Input"         sly-repl-previous-input t ]
      [ "Next Input"             sly-repl-next-input t ]
      [ "Goto Previous Prompt "  sly-repl-previous-prompt t ]
      [ "Goto Next Prompt "      sly-repl-next-prompt t ]
      [ "Clear Last Output"      sly-repl-clear-output t ]
      [ "Clear Buffer "          sly-repl-clear-buffer t ]
      [ "Kill Current Input"     sly-repl-kill-input t ])))

(defun sly-repl-add-easy-menu ()
  (easy-menu-define menubar-sly-repl sly-repl-mode-map
    "REPL" sly-repl-easy-menu)
  (easy-menu-define menubar-sly sly-repl-mode-map
    "SLY" sly-easy-menu)
  (easy-menu-add sly-repl-easy-menu 'sly-repl-mode-map))

(add-hook 'sly-repl-mode-hook 'sly-repl-add-easy-menu)

(defun sly-hide-inferior-lisp-buffer ()
  "Display the REPL buffer instead of the *inferior-lisp* buffer."
  (let* ((buffer (if (sly-process)
                     (process-buffer (sly-process))))
         (window (if buffer (get-buffer-window buffer t)))
         (repl-buffer (sly-output-buffer t))
         (repl-window (get-buffer-window repl-buffer)))
    (when buffer
      (bury-buffer buffer))
    (cond (repl-window
           (when window
             (delete-window window)))
          (window
           (set-window-buffer window repl-buffer))
          (t
           (pop-to-buffer repl-buffer)
           (goto-char (point-max))))))

(defun sly-repl-choose-coding-system ()
  (let ((candidates (sly-connection-coding-systems)))
    (or (cl-find (symbol-name (car default-process-coding-system))
                 candidates
                 :test (lambda (s1 s2)
                         (if (fboundp 'coding-system-equal)
                             (coding-system-equal (intern s1) (intern s2)))))
	(car candidates)
	(error "Can't find suitable coding-system"))))

(defun sly-repl-connected-hook-function ()
  (destructuring-bind (package prompt)
      (let ((sly-current-thread t)
	    (cs (sly-repl-choose-coding-system)))
	(sly-eval `(swank:create-repl nil :coding-system ,cs)))
    (setf (sly-lisp-package) package)
    (setf (sly-lisp-package-prompt-string) prompt))
  (sly-hide-inferior-lisp-buffer)
  (sly-init-output-buffer (sly-connection)))

(defun sly-repl-event-hook-function (event)
  (destructure-case event
    ((:write-string output &optional target)
     (sly-write-string output target)
     t)
    ((:read-string thread tag)
     (assert thread)
     (sly-repl-read-string thread tag)
     t)
    ((:read-aborted thread tag)
     (sly-repl-abort-read thread tag)
     t)
    ((:open-dedicated-output-stream port coding-system)
     (sly-open-stream-to-lisp port coding-system)
     t)
    ((:new-package package prompt-string)
     (setf (sly-lisp-package) package)
     (setf (sly-lisp-package-prompt-string) prompt-string)
     (let ((buffer (sly-connection-output-buffer)))
       (when (buffer-live-p buffer)
	 (with-current-buffer buffer
	   (setq sly-buffer-package package))))
     t)
    (t nil)))

(defun sly-change-repl-to-default-connection ()
  "Change current REPL to the REPL of the default connection.
If the current buffer is not a REPL, don't do anything."
  (when (equal major-mode 'sly-repl-mode)
    (let ((sly-buffer-connection sly-default-connection))
      (pop-to-buffer-same-window (sly-connection-output-buffer)))))

(defun sly-repl-find-buffer-package ()
  (or (sly-search-buffer-package)
      (sly-lisp-package)))

(defun sly-repl-add-hooks ()
  (add-hook 'sly-event-hooks 'sly-repl-event-hook-function)
  (add-hook 'sly-connected-hook 'sly-repl-connected-hook-function)
  (add-hook 'sly-cycle-connections-hook
            'sly-change-repl-to-default-connection))

(defun sly-repl-remove-hooks ()
  (remove-hook 'sly-event-hooks 'sly-repl-event-hook-function)
  (remove-hook 'sly-connected-hook 'sly-repl-connected-hook-function)
  (remove-hook 'sly-cycle-connections-hook
               'sly-change-repl-to-default-connection))

(defun sly-repl-sexp-at-point ()
  "Returns the current sexp at point (or NIL if none is found)
while ignoring the repl prompt text."
  (if (<= sly-repl-input-start-mark (point))
      (save-restriction
        (narrow-to-region sly-repl-input-start-mark (point-max))
        (sly-sexp-at-point))
    (sly-sexp-at-point)))

(defun sly-repl-inspect (string)
  (interactive
   (list (sly-read-from-minibuffer "Inspect value (evaluated): "
                                     (sly-repl-sexp-at-point))))
  (sly-inspect string))

(require 'bytecomp)

;; (mapc (lambda (sym)
;;         (cond ((fboundp sym)
;;                (unless (byte-code-function-p (symbol-function sym))
;;                  (byte-compile sym)))
;;               (t (error "%S is not fbound" sym))))
;;       '(sly-repl-event-hook-function
;;         sly-write-string
;;         sly-repl-write-string
;;         sly-repl-emit
;;         sly-repl-show-maximum-output))

(provide 'sly-repl)
