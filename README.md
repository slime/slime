[![Build Status](https://travis-ci.org/slime/slime.png?branch=master)](https://travis-ci.org/slime/slime)

Overview
--------

SLIME is the Superior Lisp Interaction Mode for Emacs. It is implemented
in two main parts: the Emacs Lisp side (`slime.el`), and the support
library for the Common Lisp (`swank.lisp` and `swank-*.lisp`)

For a real description, see the manual in `doc/` or browse an online
version [here](http://common-lisp.net/project/slime/doc/html/).

Quick setup instructions
------------------------

Add this to your `~/.emacs` file and fill in the appropriate filenames:

```el
;; setup load-path and autoloads
(add-to-list 'load-path "~/dir/to/cloned/slime")
(require 'slime-autoloads)

;; Set your lisp system and, optionally, some contribs
(setq inferior-lisp-program "/opt/sbcl/bin/sbcl")
(setq slime-contribs '(slime-fancy))
```

Use `M-x slime` to fire up and connect to an inferior Lisp.  SLIME will
now automatically be available in your Lisp source buffers.

Contribs
--------

SLIME comes with additional contributed packages or "contribs". When SLIME
is loaded it loads the contribs you set up before in `slime-contribs`. You
can use the command `slime-setup` to reload contribs.

The most-often used contrib is `slime-fancy`, which primarily installs a
popular set of other contributed packages. It includes a better REPL, and
many more nice features.

Licence
-------

SLIME is free software. All files, unless explicitly stated otherwise, are
public domain.

Contact
-------
If you have problems, first have a look at the list of
[known issues and workarounds][6]. 

Questions and comments are best directed to the mailing list at
`slime-devel@common-lisp.net`, but you have to [subscribe][3] first. The
mailing list archive is also available on [Gmane][4].

See the [CONTRIBUTING.md][5] file for instructions on how to contribute.

[3]: http://www.common-lisp.net/project/slime/#mailinglist
[4]: http://news.gmane.org/gmane.lisp.slime.devel
[5]: https://github.com/slime/slime/blob/master/CONTRIBUTING.md
[6]: https://github.com/slime/slime/issues?labels=workaround&state=closed
