(require 'sly)
(require 'bridge)
(require 'cl-lib)
(eval-when-compile
  (require 'cl))

(define-sly-contrib sly-presentations
  "Imitate LispM presentations."
  (:authors "Alan Ruttenberg  <alanr-l@mumble.net>"
            "Matthias Koeppe  <mkoeppe@mail.math.uni-magdeburg.de>")
  (:license "GPL")
  (:sly-dependencies sly-repl)
  (:swank-dependencies swank-presentations)
  (:on-load
   (add-hook 'sly-repl-mode-hook
             (lambda ()
               ;; Respect the syntax text properties of presentation.
               (set (make-local-variable 'parse-sexp-lookup-properties) t)
               (add-hook 'after-change-functions
                         'sly-after-change-function 'append t)))
   (add-hook 'sly-event-hooks 'sly-dispatch-presentation-event)
   (setq sly-write-string-function 'sly-presentation-write)
   (add-hook 'sly-repl-return-hooks 'sly-presentation-on-return-pressed)
   (add-hook 'sly-repl-current-input-hooks 'sly-presentation-current-input)
   (add-hook 'sly-open-stream-hooks 'sly-presentation-on-stream-open)
   (add-hook 'sly-repl-clear-buffer-hook 'sly-clear-presentations)
   (add-hook 'sly-edit-definition-hooks 'sly-edit-presentation)
   (setq sldb-insert-frame-variable-value-function
         'sly-presentation-sldb-insert-frame-variable-value)
   (sly-presentation-init-keymaps)
   (sly-presentation-add-easy-menu)))

;; To get presentations in the inspector as well, add this to your
;; init file.
;;
;; (eval-after-load 'sly-presentations
;;    '(setq sly-inspector-insert-ispec-function
;;           'sly-presentation-inspector-insert-ispec))
;;
(defface sly-repl-output-mouseover-face
  (if (featurep 'xemacs)
      '((t (:bold t)))
    (if (sly-face-inheritance-possible-p)
        '((t
           (:box
            (:line-width 1 :color "black" :style released-button)
            :inherit
            sly-repl-inputed-output-face)))
      '((t (:box (:line-width 1 :color "black"))))))
  "Face for Lisp output in the SLY REPL, when the mouse hovers over it"
  :group 'sly-repl)

(defface sly-repl-inputed-output-face
  '((((class color) (background light)) (:foreground "Red"))
    (((class color) (background dark)) (:foreground "Red"))
    (t (:slant italic)))
  "Face for the result of an evaluation in the SLY REPL."
  :group 'sly-repl)

;; FIXME: This conditional is not right - just used because the code
;; here does not work in XEmacs.
(when (boundp 'text-property-default-nonsticky)
  (pushnew '(sly-repl-presentation . t) text-property-default-nonsticky
	   :test 'equal)
  (pushnew '(sly-repl-result-face . t) text-property-default-nonsticky
	   :test 'equal))

(make-variable-buffer-local
 (defvar sly-presentation-start-to-point (make-hash-table)))

(defun sly-mark-presentation-start (id &optional target)
  "Mark the beginning of a presentation with the given ID.
TARGET can be nil (regular process output) or :repl-result."
  (setf (gethash id sly-presentation-start-to-point)
        ;; We use markers because text can also be inserted before this presentation.
        ;; (Output arrives while we are writing presentations within REPL results.)
        (copy-marker (sly-output-target-marker target) nil)))

(defun sly-mark-presentation-start-handler (process string)
  (if (and string (string-match "<\\([-0-9]+\\)" string))
      (let* ((match (substring string (match-beginning 1) (match-end 1)))
             (id (car (read-from-string match))))
        (sly-mark-presentation-start id))))

(defun sly-mark-presentation-end (id &optional target)
  "Mark the end of a presentation with the given ID.
TARGET can be nil (regular process output) or :repl-result."
  (let ((start (gethash id sly-presentation-start-to-point)))
    (remhash id sly-presentation-start-to-point)
    (when start
      (let* ((marker (sly-output-target-marker target))
             (buffer (and marker (marker-buffer marker))))
        (with-current-buffer buffer
          (let ((end (marker-position marker)))
            (sly-add-presentation-properties start end
                                               id nil)))))))

(defun sly-mark-presentation-end-handler (process string)
  (if (and string (string-match ">\\([-0-9]+\\)" string))
      (let* ((match (substring string (match-beginning 1) (match-end 1)))
             (id (car (read-from-string match))))
        (sly-mark-presentation-end id))))

(cl-defstruct sly-presentation text id)

(defvar sly-presentation-syntax-table
  (let ((table (copy-syntax-table lisp-mode-syntax-table)))
    ;; We give < and > parenthesis syntax, so that #< ... > is treated
    ;; as a balanced expression.  This allows to use C-M-k, C-M-SPC,
    ;; etc. to deal with a whole presentation.  (For Lisp mode, this
    ;; is not desirable, since we do not wish to get a mismatched
    ;; paren highlighted everytime we type < or >.)
    (modify-syntax-entry ?< "(>" table)
    (modify-syntax-entry ?> ")<" table)
    table)
  "Syntax table for presentations.")

(defun sly-add-presentation-properties (start end id result-p)
  "Make the text between START and END a presentation with ID.
RESULT-P decides whether a face for a return value or output text is used."
  (let* ((text (buffer-substring-no-properties start end))
         (presentation (make-sly-presentation :text text :id id)))
    (let ((inhibit-modification-hooks t))
      (add-text-properties start end
                           `(modification-hooks (sly-after-change-function)
                             insert-in-front-hooks (sly-after-change-function)
                             insert-behind-hooks (sly-after-change-function)
                             syntax-table ,sly-presentation-syntax-table
                             rear-nonsticky t))
      ;; Use the presentation as the key of a text property
      (case (- end start)
        (0)
        (1
         (add-text-properties start end
                              `(sly-repl-presentation ,presentation
                                ,presentation :start-and-end)))
        (t
         (add-text-properties start (1+ start)
                              `(sly-repl-presentation ,presentation
                                ,presentation :start))
         (when (> (- end start) 2)
           (add-text-properties (1+ start) (1- end)
                                `(,presentation :interior)))
         (add-text-properties (1- end) end
                              `(sly-repl-presentation ,presentation
                                ,presentation :end))))
      ;; Also put an overlay for the face and the mouse-face.  This enables
      ;; highlighting of nested presentations.  However, overlays get lost
      ;; when we copy a presentation; their removal is also not undoable.
      ;; In these cases the mouse-face text properties need to take over ---
      ;; but they do not give nested highlighting.
      (sly-ensure-presentation-overlay start end presentation))))

(defvar sly-presentation-map (make-sparse-keymap))

(defun sly-ensure-presentation-overlay (start end presentation)
  (unless (cl-find presentation (overlays-at start)
                   :key (lambda (overlay)
                          (overlay-get overlay 'sly-repl-presentation)))
    (let ((overlay (make-overlay start end (current-buffer) t nil)))
      (overlay-put overlay 'sly-repl-presentation presentation)
      (overlay-put overlay 'mouse-face 'sly-repl-output-mouseover-face)
      (overlay-put overlay 'help-echo
                   (if (eq major-mode 'sly-repl-mode)
                       "mouse-2: copy to input; mouse-3: menu"
                     "mouse-2: inspect; mouse-3: menu"))
      (overlay-put overlay 'face 'sly-repl-inputed-output-face)
      (overlay-put overlay 'keymap sly-presentation-map))))

(defun sly-remove-presentation-properties (from to presentation)
  (let ((inhibit-read-only t))
    (remove-text-properties from to
                            `(,presentation t syntax-table t rear-nonsticky t))
    (when (eq (get-text-property from 'sly-repl-presentation) presentation)
      (remove-text-properties from (1+ from) `(sly-repl-presentation t)))
    (when (eq (get-text-property (1- to) 'sly-repl-presentation) presentation)
      (remove-text-properties (1- to) to `(sly-repl-presentation t)))
    (dolist (overlay (overlays-at from))
      (when (eq (overlay-get overlay 'sly-repl-presentation) presentation)
        (delete-overlay overlay)))))

(defun sly-insert-presentation (string output-id &optional rectangle)
  "Insert STRING in current buffer and mark it as a presentation
corresponding to OUTPUT-ID.  If RECTANGLE is true, indent multi-line
strings to line up below the current point."
  (cl-labels ((insert-it ()
                       (if rectangle
                           (sly-insert-indented string)
                         (insert string))))
    (let ((start (point)))
      (insert-it)
      (sly-add-presentation-properties start (point) output-id t))))

(defun sly-presentation-whole-p (presentation start end &optional object)
  (let ((object (or object (current-buffer))))
    (string= (etypecase object
               (buffer (with-current-buffer object
                         (buffer-substring-no-properties start end)))
               (string (substring-no-properties object start end)))
             (sly-presentation-text presentation))))

(defun sly-presentations-around-point (point &optional object)
  (let ((object (or object (current-buffer))))
    (loop for (key value . rest) on (text-properties-at point object) by 'cddr
          when (sly-presentation-p key)
          collect key)))

(defun sly-presentation-start-p (tag)
  (memq tag '(:start :start-and-end)))

(defun sly-presentation-stop-p (tag)
  (memq tag '(:end :start-and-end)))

(cl-defun sly-presentation-start (point presentation
                                          &optional (object (current-buffer)))
  "Find start of `presentation' at `point' in `object'.
Return buffer index and whether a start-tag was found."
  (let* ((this-presentation (get-text-property point presentation object)))
    (while (not (sly-presentation-start-p this-presentation))
      (let ((change-point (previous-single-property-change
                           point presentation object (point-min))))
        (unless change-point
          (return-from sly-presentation-start
            (values (etypecase object
                      (buffer (with-current-buffer object 1))
                      (string 0))
                    nil)))
        (setq this-presentation (get-text-property change-point
                                                   presentation object))
        (unless this-presentation
          (return-from sly-presentation-start
            (values point nil)))
        (setq point change-point)))
    (values point t)))

(cl-defun sly-presentation-end (point presentation
                                        &optional (object (current-buffer)))
  "Find end of presentation at `point' in `object'.  Return buffer
index (after last character of the presentation) and whether an
end-tag was found."
  (let* ((this-presentation (get-text-property point presentation object)))
    (while (not (sly-presentation-stop-p this-presentation))
      (let ((change-point (next-single-property-change
                           point presentation object)))
        (unless change-point
          (return-from sly-presentation-end
            (values (etypecase object
                      (buffer (with-current-buffer object (point-max)))
                      (string (length object)))
                    nil)))
        (setq point change-point)
        (setq this-presentation (get-text-property point
                                                   presentation object))))
    (if this-presentation
        (let ((after-end (next-single-property-change point
                                                      presentation object)))
          (if (not after-end)
              (values (etypecase object
                        (buffer (with-current-buffer object (point-max)))
                        (string (length object)))
                      t)
            (values after-end t)))
      (values point nil))))

(cl-defun sly-presentation-bounds (point presentation
                                           &optional (object (current-buffer)))
  "Return start index and end index of `presentation' around `point'
in `object', and whether the presentation is complete."
  (multiple-value-bind (start good-start)
      (sly-presentation-start point presentation object)
    (multiple-value-bind (end good-end)
        (sly-presentation-end point presentation object)
      (values start end
              (and good-start good-end
                   (sly-presentation-whole-p presentation
                                               start end object))))))

(defun sly-presentation-around-point (point &optional object)
  "Return presentation, start index, end index, and whether the
presentation is complete."
  (let ((object (or object (current-buffer)))
        (innermost-presentation nil)
        (innermost-start 0)
        (innermost-end most-positive-fixnum))
    (dolist (presentation (sly-presentations-around-point point object))
      (multiple-value-bind (start end whole-p)
          (sly-presentation-bounds point presentation object)
        (when whole-p
          (when (< (- end start) (- innermost-end innermost-start))
            (setq innermost-start start
                  innermost-end end
                  innermost-presentation presentation)))))
    (values innermost-presentation
            innermost-start innermost-end)))

(defun sly-presentation-around-or-before-point (point &optional object)
  (let ((object (or object (current-buffer))))
    (multiple-value-bind (presentation start end whole-p)
        (sly-presentation-around-point point object)
      (if (or presentation (= point (point-min)))
          (values presentation start end whole-p)
        (sly-presentation-around-point (1- point) object)))))

(defun sly-presentation-around-or-before-point-or-error (point)
  (multiple-value-bind (presentation start end whole-p)
      (sly-presentation-around-or-before-point point)
    (unless presentation
      (error "No presentation at point"))
    (values presentation start end whole-p)))

(cl-defun sly-for-each-presentation-in-region (from to function
                                                      &optional (object (current-buffer)))
  "Call `function' with arguments `presentation', `start', `end',
`whole-p' for every presentation in the region `from'--`to' in the
string or buffer `object'."
  (cl-labels ((handle-presentation (presentation point)
                                   (multiple-value-bind (start end whole-p)
                                       (sly-presentation-bounds point presentation object)
                                     (funcall function presentation start end whole-p))))
    ;; Handle presentations active at `from'.
    (dolist (presentation (sly-presentations-around-point from object))
      (handle-presentation presentation from))
    ;; Use the `sly-repl-presentation' property to search for new presentations.
    (let ((point from))
      (while (< point to)
        (setq point (next-single-property-change point 'sly-repl-presentation
                                                 object to))
        (let* ((presentation (get-text-property point 'sly-repl-presentation object))
               (status (get-text-property point presentation object)))
          (when (sly-presentation-start-p status)
            (handle-presentation presentation point)))))))

;; XEmacs compatibility hack, from message by Stephen J. Turnbull on
;; xemacs-beta@xemacs.org of 18 Mar 2002
(unless (boundp 'undo-in-progress)
  (defvar undo-in-progress nil
    "Placeholder defvar for XEmacs compatibility from SLY.")
  (defadvice undo-more (around sly activate)
    (let ((undo-in-progress t)) ad-do-it)))

(defun sly-after-change-function (start end &rest ignore)
  "Check all presentations within and adjacent to the change.
When a presentation has been altered, change it to plain text."
  (let ((inhibit-modification-hooks t))
    (let ((real-start (max 1 (1- start)))
          (real-end   (min (1+ (buffer-size)) (1+ end)))
          (any-change nil))
      ;; positions around the change
      (sly-for-each-presentation-in-region
       real-start real-end
       (lambda (presentation from to whole-p)
         (cond
          (whole-p
           (sly-ensure-presentation-overlay from to presentation))
          ((not undo-in-progress)
           (sly-remove-presentation-properties from to
                                                 presentation)
           (setq any-change t)))))
      (when any-change
        (undo-boundary)))))

(defun sly-presentation-around-click (event)
  "Return the presentation around the position of the mouse-click EVENT.
If there is no presentation, signal an error.
Also return the start position, end position, and buffer of the presentation."
  (when (and (featurep 'xemacs) (not (button-press-event-p event)))
    (error "Command must be bound to a button-press-event"))
  (let ((point (if (featurep 'xemacs) (event-point event) (posn-point (event-end event))))
        (window (if (featurep 'xemacs) (event-window event) (caadr event))))
    (with-current-buffer (window-buffer window)
      (multiple-value-bind (presentation start end)
          (sly-presentation-around-point point)
        (unless presentation
          (error "No presentation at click"))
        (values presentation start end (current-buffer))))))

(defun sly-check-presentation (from to buffer presentation)
  (unless (sly-eval `(cl:nth-value 1 (swank:lookup-presented-object
                                        ',(sly-presentation-id presentation))))
    (with-current-buffer buffer
      (sly-remove-presentation-properties from to presentation))))

(defun sly-copy-or-inspect-presentation-at-mouse (event)
  (interactive "e") ; no "@" -- we don't want to select the clicked-at window
  (multiple-value-bind (presentation start end buffer)
      (sly-presentation-around-click event)
    (sly-check-presentation start end buffer presentation)
    (if (with-current-buffer buffer
          (eq major-mode 'sly-repl-mode))
        (sly-copy-presentation-at-mouse-to-repl event)
      (sly-inspect-presentation-at-mouse event))))

(defun sly-inspect-presentation (presentation start end buffer)
  (let ((reset-p
	 (with-current-buffer buffer
	   (not (eq major-mode 'sly-inspector-mode)))))
    (sly-eval-async `(swank:inspect-presentation ',(sly-presentation-id presentation) ,reset-p)
      'sly-open-inspector)))

(defun sly-inspect-presentation-at-mouse (event)
  (interactive "e")
  (multiple-value-bind (presentation start end buffer)
      (sly-presentation-around-click event)
    (sly-inspect-presentation presentation start end buffer)))

(defun sly-inspect-presentation-at-point (point)
  (interactive "d")
  (multiple-value-bind (presentation start end)
      (sly-presentation-around-or-before-point-or-error point)
    (sly-inspect-presentation presentation start end (current-buffer))))


(defun sly-M-.-presentation (presentation start end buffer &optional where)
  (let* ((id (sly-presentation-id presentation))
	 (presentation-string (format "Presentation %s" id))
	 (location (sly-eval `(swank:find-definition-for-thing
				 (swank:lookup-presented-object
				  ',(sly-presentation-id presentation))))))
    (unless (eq (car location) :error)
      (sly-edit-definition-cont
       (and location (list (make-sly-xref :dspec `(,presentation-string)
                                            :location location)))
       presentation-string
       where))))

(defun sly-M-.-presentation-at-mouse (event)
  (interactive "e")
  (multiple-value-bind (presentation start end buffer)
      (sly-presentation-around-click event)
    (sly-M-.-presentation presentation start end buffer)))

(defun sly-M-.-presentation-at-point (point)
  (interactive "d")
  (multiple-value-bind (presentation start end)
      (sly-presentation-around-or-before-point-or-error point)
    (sly-M-.-presentation presentation start end (current-buffer))))

(defun sly-edit-presentation (name &optional where)
  (if (or current-prefix-arg (not (equal (sly-symbol-at-point) name)))
      nil ; NAME came from user explicitly, so decline.
    (multiple-value-bind (presentation start end whole-p)
        (sly-presentation-around-or-before-point (point))
      (when presentation
        (sly-M-.-presentation presentation start end (current-buffer) where)))))


(defun sly-copy-presentation-to-repl (presentation start end buffer)
  (with-current-buffer buffer
    (sly-repl-send-string
     (format "%s"
             `(cl:nth-value
               0
               (swank:lookup-presented-object
                ',(sly-presentation-id presentation)))))
    (sly-repl)))

(defun sly-copy-presentation-at-mouse-to-repl (event)
  (interactive "e")
  (multiple-value-bind (presentation start end buffer)
      (sly-presentation-around-click event)
    (sly-copy-presentation-to-repl presentation start end buffer)))

(defun sly-copy-presentation-at-point-to-repl (point)
  (interactive "d")
  (multiple-value-bind (presentation start end)
      (sly-presentation-around-or-before-point-or-error point)
    (sly-copy-presentation-to-repl presentation start end (current-buffer))))

(defun sly-copy-presentation-at-mouse-to-point (event)
  (interactive "e")
  (multiple-value-bind (presentation start end buffer)
      (sly-presentation-around-click event)
    (let ((presentation-text
           (with-current-buffer buffer
             (buffer-substring start end))))
      (when (not (string-match "\\s-"
                               (buffer-substring (1- (point)) (point))))
        (insert " "))
      (insert presentation-text)
      (sly-after-change-function (point) (point))
      (when (and (not (eolp)) (not (looking-at "\\s-")))
        (insert " ")))))

(defun sly-copy-presentation-to-kill-ring (presentation start end buffer)
  (let ((presentation-text
         (with-current-buffer buffer
           (buffer-substring start end))))
    (kill-new presentation-text)
    (message "Saved presentation \"%s\" to kill ring" presentation-text)))

(defun sly-copy-presentation-at-mouse-to-kill-ring (event)
  (interactive "e")
  (multiple-value-bind (presentation start end buffer)
      (sly-presentation-around-click event)
    (sly-copy-presentation-to-kill-ring presentation start end buffer)))

(defun sly-copy-presentation-at-point-to-kill-ring (point)
  (interactive "d")
  (multiple-value-bind (presentation start end)
      (sly-presentation-around-or-before-point-or-error point)
    (sly-copy-presentation-to-kill-ring presentation start end (current-buffer))))

(defun sly-describe-presentation (presentation)
  (sly-eval-describe
   `(swank::describe-to-string
     (swank:lookup-presented-object ',(sly-presentation-id presentation)))))

(defun sly-describe-presentation-at-mouse (event)
  (interactive "@e")
  (multiple-value-bind (presentation) (sly-presentation-around-click event)
    (sly-describe-presentation presentation)))

(defun sly-describe-presentation-at-point (point)
  (interactive "d")
  (multiple-value-bind (presentation)
      (sly-presentation-around-or-before-point-or-error point)
    (sly-describe-presentation presentation)))

(defun sly-pretty-print-presentation (presentation)
  (sly-eval-describe
   `(swank::swank-pprint
     (cl:list
      (swank:lookup-presented-object ',(sly-presentation-id presentation))))))

(defun sly-pretty-print-presentation-at-mouse (event)
  (interactive "@e")
  (multiple-value-bind (presentation) (sly-presentation-around-click event)
    (sly-pretty-print-presentation presentation)))

(defun sly-pretty-print-presentation-at-point (point)
  (interactive "d")
  (multiple-value-bind (presentation)
      (sly-presentation-around-or-before-point-or-error point)
    (sly-pretty-print-presentation presentation)))

(defun sly-mark-presentation (point)
  (interactive "d")
  (multiple-value-bind (presentation start end)
      (sly-presentation-around-or-before-point-or-error point)
    (goto-char start)
    (push-mark end nil t)))

(defun sly-previous-presentation (&optional arg)
  "Move point to the beginning of the first presentation before point.
With ARG, do this that many times.
A negative argument means move forward instead."
  (interactive "p")
  (unless arg (setq arg 1))
  (sly-next-presentation (- arg)))

(defun sly-next-presentation (&optional arg)
  "Move point to the beginning of the next presentation after point.
With ARG, do this that many times.
A negative argument means move backward instead."
  (interactive "p")
  (unless arg (setq arg 1))
  (cond
   ((plusp arg)
    (dotimes (i arg)
      ;; First skip outside the current surrounding presentation (if any)
      (multiple-value-bind (presentation start end)
	  (sly-presentation-around-point (point))
	(when presentation
	  (goto-char end)))
      (let ((p (next-single-property-change (point) 'sly-repl-presentation)))
	(unless p
	  (error "No next presentation"))
	(multiple-value-bind (presentation start end)
	    (sly-presentation-around-or-before-point-or-error p)
	  (goto-char start)))))
   ((minusp arg)
    (dotimes (i (- arg))
      ;; First skip outside the current surrounding presentation (if any)
      (multiple-value-bind (presentation start end)
	  (sly-presentation-around-point (point))
	(when presentation
	  (goto-char start)))
      (let ((p (previous-single-property-change (point) 'sly-repl-presentation)))
	(unless p
	  (error "No previous presentation"))
	(multiple-value-bind (presentation start end)
	    (sly-presentation-around-or-before-point-or-error p)
	  (goto-char start)))))))

(define-key  sly-presentation-map [mouse-2] 'sly-copy-or-inspect-presentation-at-mouse)
(define-key  sly-presentation-map [mouse-3] 'sly-presentation-menu)

(when (featurep 'xemacs)
  (define-key  sly-presentation-map [button2] 'sly-copy-or-inspect-presentation-at-mouse)
  (define-key  sly-presentation-map [button3] 'sly-presentation-menu))

;; protocol for handling up a menu.
;; 1. Send lisp message asking for menu choices for this object.
;;    Get back list of strings.
;; 2. Let used choose
;; 3. Call back to execute menu choice, passing nth and string of choice

(defun sly-menu-choices-for-presentation (presentation buffer from to choice-to-lambda)
  "Return a menu for `presentation' at `from'--`to' in `buffer', suitable for `x-popup-menu'."
  (let* ((what (sly-presentation-id presentation))
         (choices (with-current-buffer buffer
                    (sly-eval
                     `(swank::menu-choices-for-presentation-id ',what)))))
    (cl-labels ((savel (f) ;; IMPORTANT - xemacs can't handle lambdas in x-popup-menu. So give them a name
                     (let ((sym (cl-gensym)))
                       (setf (gethash sym choice-to-lambda) f)
                       sym)))
      (etypecase choices
        (list
         `(,(format "Presentation %s" (truncate-string-to-width
                                       (sly-presentation-text presentation)
                                       30 nil nil t))
           (""
            ("Find Definition" . ,(savel 'sly-M-.-presentation-at-mouse))
            ("Inspect" . ,(savel 'sly-inspect-presentation-at-mouse))
            ("Describe" . ,(savel 'sly-describe-presentation-at-mouse))
            ("Pretty-print" . ,(savel 'sly-pretty-print-presentation-at-mouse))
            ("Copy to REPL" . ,(savel 'sly-copy-presentation-at-mouse-to-repl))
            ("Copy to kill ring" . ,(savel 'sly-copy-presentation-at-mouse-to-kill-ring))
            ,@(unless buffer-read-only
                `(("Copy to point" . ,(savel 'sly-copy-presentation-at-mouse-to-point))))
            ,@(let ((nchoice 0))
                (mapcar
                 (lambda (choice)
                   (incf nchoice)
                   (cons choice
                         (savel `(lambda ()
                                   (interactive)
                                   (sly-eval
                                    '(swank::execute-menu-choice-for-presentation-id
                                      ',what ,nchoice ,(nth (1- nchoice) choices)))))))
                 choices)))))
        (symbol                           ; not-present
         (with-current-buffer buffer
           (sly-remove-presentation-properties from to presentation))
         (sit-for 0)                      ; allow redisplay
         `("Object no longer recorded"
           ("sorry" . ,(if (featurep 'xemacs) nil '(nil)))))))))

(defun sly-presentation-menu (event)
  (interactive "e")
  (let* ((point (if (featurep 'xemacs) (event-point event)
                  (posn-point (event-end event))))
         (window (if (featurep 'xemacs) (event-window event) (caadr event)))
         (buffer (window-buffer window))
         (choice-to-lambda (make-hash-table)))
    (multiple-value-bind (presentation from to)
        (with-current-buffer buffer
          (sly-presentation-around-point point))
      (unless presentation
        (error "No presentation at event position"))
      (let ((menu (sly-menu-choices-for-presentation
                   presentation buffer from to choice-to-lambda)))
        (let ((choice (x-popup-menu event menu)))
          (when choice
            (call-interactively (gethash choice choice-to-lambda))))))))

(defun sly-presentation-expression (presentation)
  "Return a string that contains a CL s-expression accessing
the presented object."
  (let ((id (sly-presentation-id presentation)))
    (etypecase id
      (number
       ;; Make sure it works even if *read-base* is not 10.
       (format "(swank:lookup-presented-object-or-lose %d.)" id))
      (list
       ;; for frame variables and inspector parts
       (format "(swank:lookup-presented-object-or-lose '%s)" id)))))

(defun sly-buffer-substring-with-reified-output (start end)
  (let ((str-props (buffer-substring start end))
        (str-no-props (buffer-substring-no-properties start end)))
    (sly-reify-old-output str-props str-no-props)))

(defun sly-reify-old-output (str-props str-no-props)
  (let ((pos (sly-property-position 'sly-repl-presentation str-props)))
    (if (null pos)
        str-no-props
      (multiple-value-bind (presentation start-pos end-pos whole-p)
          (sly-presentation-around-point pos str-props)
        (if (not presentation)
            str-no-props
          (concat (substring str-no-props 0 pos)
                  ;; Eval in the reader so that we play nice with quote.
                  ;; -luke (19/May/2005)
                  "#." (sly-presentation-expression presentation)
                  (sly-reify-old-output (substring str-props end-pos)
                                          (substring str-no-props end-pos))))))))



(defun sly-repl-grab-old-output (replace)
  "Resend the old REPL output at point.
If replace it non-nil the current input is replaced with the old
output; otherwise the new input is appended."
  (multiple-value-bind (presentation beg end)
      (sly-presentation-around-or-before-point (point))
    (sly-check-presentation beg end (current-buffer) presentation)
    (let ((old-output (buffer-substring beg end))) ;;keep properties
      ;; Append the old input or replace the current input
      (cond (replace (goto-char sly-repl-input-start-mark))
            (t (goto-char (point-max))
               (unless (eq (char-before) ?\ )
                 (insert " "))))
      (delete-region (point) (point-max))
      (let ((inhibit-read-only t))
        (insert old-output)))))

;;; Presentation-related key bindings, non-context menu

(defvar sly-presentation-command-map nil
  "Keymap for presentation-related commands. Bound to a prefix key.")

(defvar sly-presentation-bindings
  '((?i sly-inspect-presentation-at-point)
    (?d sly-describe-presentation-at-point)
    (?w sly-copy-presentation-at-point-to-kill-ring)
    (?r sly-copy-presentation-at-point-to-repl)
    (?p sly-previous-presentation)
    (?n sly-next-presentation)
    (?\  sly-mark-presentation)))

(defun sly-presentation-init-keymaps ()
  (sly-init-keymap 'sly-presentation-command-map nil t
		     sly-presentation-bindings)
  (define-key sly-presentation-command-map "\M-o" 'sly-clear-presentations)
  ;; C-c C-v is the prefix for the presentation-command map.
  (define-key sly-prefix-map "\C-v" sly-presentation-command-map))

(defun sly-presentation-around-or-before-point-p ()
  (multiple-value-bind (presentation beg end)
      (sly-presentation-around-or-before-point (point))
    presentation))

(defvar sly-presentation-easy-menu
  (let ((P '(sly-presentation-around-or-before-point-p)))
    `("Presentations"
      [ "Find Definition" sly-M-.-presentation-at-point ,P ]
      [ "Inspect" sly-inspect-presentation-at-point ,P ]
      [ "Describe" sly-describe-presentation-at-point ,P ]
      [ "Pretty-print" sly-pretty-print-presentation-at-point ,P ]
      [ "Copy to REPL" sly-copy-presentation-at-point-to-repl ,P ]
      [ "Copy to kill ring" sly-copy-presentation-at-point-to-kill-ring ,P ]
      [ "Mark" sly-mark-presentation ,P ]
      "--"
      [ "Previous presentation" sly-previous-presentation ]
      [ "Next presentation" sly-next-presentation ]
      "--"
      [ "Clear all presentations" sly-clear-presentations ])))

(defun sly-presentation-add-easy-menu ()
  (easy-menu-define menubar-sly-presentation sly-mode-map "Presentations" sly-presentation-easy-menu)
  (easy-menu-define menubar-sly-presentation sly-repl-mode-map "Presentations" sly-presentation-easy-menu)
  (easy-menu-define menubar-sly-presentation sldb-mode-map "Presentations" sly-presentation-easy-menu)
  (easy-menu-define menubar-sly-presentation sly-inspector-mode-map "Presentations" sly-presentation-easy-menu)
  (easy-menu-add sly-presentation-easy-menu 'sly-mode-map)
  (easy-menu-add sly-presentation-easy-menu 'sly-repl-mode-map)
  (easy-menu-add sly-presentation-easy-menu 'sldb-mode-map)
  (easy-menu-add sly-presentation-easy-menu 'sly-inspector-mode-map))

;;; hook functions (hard to isolate stuff)

(defun sly-dispatch-presentation-event (event)
  (destructure-case event
    ((:presentation-start id &optional target)
     (sly-mark-presentation-start id target)
     t)
    ((:presentation-end id &optional target)
     (sly-mark-presentation-end id target)
     t)
    (t nil)))

(defun sly-presentation-write-result (string)
  (with-current-buffer (sly-output-buffer)
    (let ((marker (sly-output-target-marker :repl-result))
          (saved-point (point-marker)))
      (goto-char marker)
      (sly-propertize-region `(face sly-repl-result-face
                                      rear-nonsticky (face))
        (insert string))
      ;; Move the input-start marker after the REPL result.
      (set-marker marker (point))
      (set-marker sly-output-end (point))
      ;; Restore point before insertion but only it if was farther
      ;; than `marker'. Omitting this breaks REPL test
      ;; `repl-type-ahead'.
      (when (> saved-point (point))
        (goto-char saved-point)))
    (sly-repl-show-maximum-output)))

(defun sly-presentation-write (string &optional target)
  (case target
    ((nil)                              ; Regular process output
     (sly-repl-emit string))
    (:repl-result
     (sly-presentation-write-result string))
    (t (sly-emit-to-target string target))))

(defun sly-presentation-current-input (&optional until-point-p)
  "Return the current input as string.
The input is the region from after the last prompt to the end of
buffer. Presentations of old results are expanded into code."
  (sly-buffer-substring-with-reified-output  sly-repl-input-start-mark
					       (point-max)))

(defun sly-presentation-on-return-pressed (end-of-input)
  (when (and (car (sly-presentation-around-or-before-point (point)))
             (< (point) sly-repl-input-start-mark))
    (sly-repl-grab-old-output end-of-input)
    (sly-repl-recenter-if-needed)
    t))

(defun sly-presentation-bridge-insert (process output)
  (sly-output-filter process (or output "")))

(defun sly-presentation-on-stream-open (stream)
  (install-bridge)
  (setq bridge-insert-function #'sly-presentation-bridge-insert)
  (setq bridge-destination-insert nil)
  (setq bridge-source-insert nil)
  (setq bridge-handlers
	(list* '("<" . sly-mark-presentation-start-handler)
	       '(">" . sly-mark-presentation-end-handler)
	       bridge-handlers)))

(defun sly-clear-presentations ()
  "Forget all objects associated to SLY presentations.
This allows the garbage collector to remove these objects
even on Common Lisp implementations without weak hash tables."
  (interactive)
  (sly-eval-async `(swank:clear-repl-results))
  (unless (eql major-mode 'sly-repl-mode)
    (sly-switch-to-output-buffer))
  (sly-for-each-presentation-in-region 1 (1+ (buffer-size))
					 (lambda (presentation from to whole-p)
					   (sly-remove-presentation-properties from to
										 presentation))))

(defun sly-presentation-inspector-insert-ispec (ispec)
  (if (stringp ispec)
      (insert ispec)
    (destructure-case ispec
      ((:value string id)
       (sly-propertize-region
           (list 'sly-part-number id
                 'mouse-face 'highlight
                 'face 'sly-inspector-value-face)
         (sly-insert-presentation string `(:inspected-part ,id) t)))
      ((:label string)
       (insert (sly-inspector-fontify label string)))
      ((:action string id)
       (sly-insert-propertized (list 'sly-action-number id
                                       'mouse-face 'highlight
                                       'face 'sly-inspector-action-face)
                                 string)))))

(defun sly-presentation-sldb-insert-frame-variable-value (value frame index)
  (sly-insert-presentation
   (sldb-in-face local-value value)
   `(:frame-var ,sly-current-thread ,(car frame) ,index) t))

(provide 'sly-presentations)
