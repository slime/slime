[![Build Status](https://travis-ci.org/capitaomorte/sly.png?branch=master)](https://travis-ci.org/capitaomorte/sly)

```
          _____    __   __  __        
         / ___/   / /   \ \/ /               |\      _,,,---,,_
         \__ \   / /     \  /                /,`.-'`'    -.  ;-;;,_
        ___/ /  / /___   / /                |,4-  ) )-,_..;\ (  `'-'
       /____/  /_____/  /_/                '---''(_/--'  `-'\_)

```

SLY is a Common Lisp IDE for Emacs.

See it in action [this screencast][7]. SLY is currently **alpha** status but
read the [NEWS.md][6] file for the latest news.

SLY is a fork of [SLIME][1], which is the work of Eric Marsden, Luke Gorrie,
Helmut Eller Tobias, C. Rittweiler and [many-others][8].

Quick setup instructions
------------------------

Add this to your `~/.emacs` file and fill in the appropriate filenames:

```el
(add-to-list 'load-path "~/dir/to/cloned/sly")
(require 'sly-autoloads)
(setq inferior-lisp-program "/opt/sbcl/bin/sbcl")
```

Use `M-x sly` to fire up and connect to an inferior Lisp.  SLY will now
automatically be available in your Lisp source buffers.

This also works
```
$ sbcl
...
* (push #p"~/dir/to/cloned/sly" asdf:*central-registry*)
* (asdf:load-system :swank)
* (swank:create-server :port 4008)
```

Now in Emacs you can do `sly-connect` and give it the host and the 4008 port as
a destination.

Licence
-------

SLY is free software. All files, unless explicitly stated otherwise, are
public domain. ASCII artwork by Felix Lee and others.

Contributing
------------

[Open an issue or a pull request][4], but first see the [CONTRIBUTING.md][5]
file for instructions on how to contribute.

[1]: http://www.common-lisp.net/project/slime/
[4]: https://github.com/capitaomorte/sly/issues
[5]: https://github.com/capitaomorte/sly/blob/master/CONTRIBUTING.md
[6]: https://github.com/capitaomorte/sly/blob/master/NEWS.md
[7]: https://www.youtube.com/watch?v=xqWkVvubnSI
[8]: http://common-lisp.net/project/slime/doc/html/Credits.html#Credits

<!-- Local Variables: -->
<!-- fill-column: 80 -->
<!-- End: -->
