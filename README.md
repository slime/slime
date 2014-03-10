[![Build Status](https://travis-ci.org/sly/sly.png?branch=master)](https://travis-ci.org/sly/sly)

Overview
--------

SLY is the Superior Lisp Interaction Mode for Emacs. It is implemented
in two main parts: the Emacs Lisp side (`sly.el`), and the support
library for the Common Lisp (`swank.lisp` and `swank-*.lisp`)

For a real description, see the manual in `doc/` or browse an online
version [here](http://common-lisp.net/project/sly/doc/html/).

Quick setup instructions
------------------------

Add this to your `~/.emacs` file and fill in the appropriate filenames:

```el
;; setup load-path and autoloads
(add-to-list 'load-path "~/dir/to/cloned/sly")
(require 'sly-autoloads)

;; Set your lisp system and, optionally, some contribs
(setq inferior-lisp-program "/opt/sbcl/bin/sbcl")
(setq sly-contribs '(sly-fancy))
```

Use `M-x sly` to fire up and connect to an inferior Lisp.  SLY will
now automatically be available in your Lisp source buffers.

Contribs
--------

SLY comes with additional contributed packages or "contribs". When SLY
is loaded it loads the contribs you set up before in `sly-contribs`. You
can use the command `sly-setup` to reload contribs.

The most-often used contrib is `sly-fancy`, which primarily installs a
popular set of other contributed packages. It includes a better REPL, and
many more nice features.

Licence
-------

SLY is free software. All files, unless explicitly stated otherwise, are
public domain.

Contact
-------

Questions and comments are best directed to the mailing list at
`sly-devel@common-lisp.net`, but you have to [subscribe][3] first. The
mailing list archive is also available on [Gmane][4].

See the [CONTRIBUTING.md][5] file for instructions on how to contribute.

[3]: http://www.common-lisp.net/project/sly/#mailinglist
[4]: http://news.gmane.org/gmane.lisp.sly.devel
[5]: https://github.com/sly/sly/blob/master/CONTRIBUTING.md
