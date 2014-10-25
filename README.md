[![Build Status](https://travis-ci.org/capitaomorte/sly.png?branch=master)](https://travis-ci.org/capitaomorte/sly)
[![MELPA](http://melpa.org/packages/sly-badge.svg)](http://melpa.org/#/sly)

```
          _____    __   __  __        
         / ___/   / /   \ \/ /               |\      _,,,---,,_
         \__ \   / /     \  /                /,`.-'`'    -.  ;-;;,_
        ___/ /  / /___   / /                |,4-  ) )-,_..;\ (  `'-'
       /____/  /_____/  /_/                '---''(_/--'  `-'\_)

```

SLY is Sylvester the Cat's Common Lisp IDE for Emacs. See it in action in
[this screencast][7].

SLY is a direct fork of [SLIME][1], and contains the following improvements over
it:

* Completely redesigned REPL based on Emacs's own full-featured `comint.el`
* Live code annotations via a new `sly-stickers` contrib
* Consistent interactive button interface. Everything can be copied to the REPL.
* Multiple inspectors with independent history
* Regexp-capable `M-x sly-apropos`
* Contribs are first class SLY citizens and enabled by default
* Use ASDF to loads contribs on demand.

SLY tracks SLIME's bugfixes and all its familar features (debugger, inspector,
xref, etc...) are still available , but with better integration.

Read about the reasons for forking [here][2] and see the [NEWS.md][6] for
complete list of differences between the two projects.

SLY is currently *alpha* status. The
[documentation can be found here][documentation], but beware
[it's still a little out of date](https://github.com/capitaomorte/sly/issues/9).

Install from MELPA
------------------

Ensure that [MELPA][10] is setup as usual and that `inferior-lisp-program` points 
to a valid lisp:

```el
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(setq inferior-lisp-program "/opt/sbcl/bin/sbcl")
```

Now do `M-x package-install` and enter `sly` when prompted. Use `M-x sly` to
fire up SLY and connect to Lisp. You will get a friendly REPL. SLY's `sly-mode`
will automatically come up in every `.lisp` file.

Install from Git
----------------

Clone this repository, add this to your `~/.emacs` file and fill in the
appropriate file names:

```el
(add-to-list 'load-path "~/dir/to/cloned/sly")
(require 'sly-autoloads)
(setq inferior-lisp-program "/opt/sbcl/bin/sbcl")
```

`M-x sly` becomes available immediately. If you wish to byte-compile SLY
yourself (not needed generally) you can do `make compile contrib-compile` in the
dir where you cloned SLY.

Running the server standalone
-----------------------------

This also works
```
$ sbcl
...
* (push #p"~/dir/to/sly" asdf:*central-registry*)
* (asdf:load-system :slynk)
* (slynk:create-server :port 4008)
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
C. Rittweiler and [many others][8]. I forked SLIME because I used it daily,
for work, had a long list of hacks developed for myself, and wanted to share
them with others.

In 2013, SLIME development was stalling, patches and issues rotting. In early 
2014,  Luís Oliveira and myself moved SLIME to Github and set up its Travis CI 
system. I brought in the old bug reports from the Launchpad tracker, fixed 
long-standing problems and submitted many changes, particularly to the 
under-curated but popular "contrib" section.

Now, the changes that SLY brings to the table are too deep at the Elisp and
Lisp level to be accepted to SLIME, given its current focus on stability (for
the record, I find this perfectly reasonable). The new features such as multiple
inspectors cannot be realized well using only the existing "contrib" system. 
Finally, SLY frees itself from the Emacs 23 shackles and supports Emacs 24.3 only 
allowing for much cleaner code and liberal use of lexical binding.

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
[10]: https://github.com/milkypostman/melpa
[documentation]: http://capitaomorte.github.io/sly

<!-- Local Variables: -->
<!-- fill-column: 80 -->
<!-- End: -->
