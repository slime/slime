[![Build Status](https://travis-ci.org/capitaomorte/sly.png?branch=master)](https://travis-ci.org/capitaomorte/sly)

```
          _____    __   __  __        
         / ___/   / /   \ \/ /               |\      _,,,---,,_
         \__ \   / /     \  /                /,`.-'`'    -.  ;-;;,_
        ___/ /  / /___   / /                |,4-  ) )-,_..;\ (  `'-'
       /____/  /_____/  /_/                '---''(_/--'  `-'\_)

```

SLY is a Common Lisp IDE for Emacs.

See it in action [this screencast][7]. 

SLY is a fork of [SLIME][1]. Read about the reasons for forking [here][2] and
see the [NEWS.md][6] for a listing of the differences between the two
projects. SLY is currently **alpha** status

Quick setup instructions
------------------------

Add this to your `~/.emacs` file and fill in the appropriate file names:

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

License
-------

SLY is free software. All files, unless explicitly stated otherwise, are
public domain. ASCII artwork is copyright by Felix Lee and others.

Fork
----

SLIME is the work of Eric Marsden, Luke Gorrie, Helmut Eller, Tobias
C. Rittweiler and [many others][8]. I forked SLIME it because I used it daily,
for work, had a long list of hacks developed for myself, and wanted to share
them with others.

In 2013, SLIME development was stalling and patches rotting, so in 2014, I
helped move SLIME to Github, set up its Travis CI system, fixed long-standing
bugs and submitted many changes.

However, the changes that SLY brings to the table are too deep at the Elisp and
Lisp level to be accepted to SLIME, given its current focus on stability. For
the record, I find this perfectly reasonable. As an example, SLY frees itself
from the Emacs 23 shackles and supports Emacs 24.3 only allowing for much
cleaner code and liberal use of lexical binding.

The list of technical reasons is bigger than this though, and you can read up on
them in the [CONTRIBUTING.md][9] file.

Contributing
------------

[Open an issue or a pull request][4], but at least have a quick look at the
first part [CONTRIBUTING.md][5] file for instructions on how to contribute.

[1]: http://www.common-lisp.net/project/slime/
[2]: https://github.com/capitaomorte/sly/blob/master/README.md#fork
[4]: https://github.com/capitaomorte/sly/issues
[5]: https://github.com/capitaomorte/sly/blob/master/CONTRIBUTING.md
[6]: https://github.com/capitaomorte/sly/blob/master/NEWS.md
[7]: https://www.youtube.com/watch?v=xqWkVvubnSI
[8]: http://common-lisp.net/project/slime/doc/html/Credits.html#Credits
[9]: https://github.com/capitaomorte/sly/blob/master/CONTRIBUTING.md#architecture

<!-- Local Variables: -->
<!-- fill-column: 80 -->
<!-- End: -->
