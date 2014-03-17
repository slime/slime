[![Build Status](https://travis-ci.org/sly/sly.png?branch=master)](https://travis-ci.org/sly/sly)

Overview
--------

SLY is a fork of [SLIME][1].

For a real description, see the manual in `doc/`.

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

Use `M-x sly` to fire up and connect to an inferior Lisp.  SLY will now
automatically be available in your Lisp source buffers.

`sly-fancy` refers to the most commonly used contrib which primarily installs a
popular set of other contributed packages. It includes a better REPL, and many
more nice features.

SLIME incompatibilities
-----------------------

* SLY doesn't contain an equivalent to the `slime-presentations` contrib.

* The `sly-mrepl` contrib, active by default in `sly-fancy` is not fully
  compatible with SLIME's `swank-mrepl.lisp` since SLIME's support for multiple
  REPLs is only experimental.

Licence
-------

SLY is free software. All files, unless explicitly stated otherwise, are
public domain.

Contributing
------------

[Open an issue or a pull request][4], but first see the [CONTRIBUTING.md][5]
file for instructions on how to contribute.

[1]: http://www.common-lisp.net/project/slime/
[4]: https://github.com/capitaomorte/sly/issues
[5]: https://github.com/capitaomorte/sly/blob/master/CONTRIBUTING.md

<!-- Local Variables: -->
<!-- fill-column: 80 -->
<!-- End: -->
