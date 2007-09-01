;;; slime-fancy.el --- Load all stable fancy SLIME contribs
;;
;; Authors: Matthias Koeppe  <mkoeppe@mail.math.uni-magdeburg.de>
;; 
;; License: GNU GPL (same license as Emacs)
;;
;;; Installation:
;;
;; Add this to your .emacs: 
;;
;;   (add-to-list 'load-path "<directory-of-this-file>")
;;   (add-hook 'slime-load-hook (lambda () (require 'slime-fancy)))
;;
;; We load all SLIME contribs that are currently working,
;; and which only "upgrade" the behavior of SLIME in some way.
;; This includes:
;;   * Adding new commands, keybindings, menu items
;;   * Making things clickable that would otherwise be just plain text

;; Better arglist display, can be turned off by customization.
(require 'slime-autodoc)

;; Adds new commands and installs compound-prefix-completion as
;; default completion command.  Behaves similar to standard Emacs
;; completion, unless dashes are present. --mkoeppe
(require 'slime-c-p-c)

;; Just adds commands.
(require 'slime-editing-commands)

;; Makes the inspector fancier.
(require 'slime-fancy-inspector)

;; Just adds the command C-c M-i.  We do not make fuzzy completion the
;; default completion invoked by TAB. --mkoeppe
(require 'slime-fuzzy)

(require 'slime-highlight-edits)

;; Load slime-presentations even though they seem to be a
;; controversial feature, as they can be easily turned off by
;; customizing swank:*record-repl-results*. --mkoeppe
(require 'slime-presentations)

;;; Do not load slime-presentation-streams, as this is an experimental
;;; feature that installs patches into some Lisps. --mkoeppe
;;(require 'slime-presentation-streams)

(require 'slime-scratch)

;;; Do not load slime-typeout-frame, as simply loading causes display of a
;;; typeout frame, which cannot be turned off. --mkoeppe
;;(require 'slime-typeout-frame)

;; Just adds commands.
(require 'slime-xref-browser)

(provide 'slime-fancy)

