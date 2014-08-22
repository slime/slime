# The SLY Hacker's Handbook


## Lisp code file structure

The Lisp code is organised into these files:

* `lib/lisp/swank-backend.lisp`: Definition of the interface to non-portable
features.  Stand-alone.

* `lib/lisp/backebd/swank-<cmucl|...>.lisp`: Backend implementation
for a specific Common Lisp system.  Uses swank-backend.lisp.

* `lib/lisp/swank.lisp`: The top-level server program, built from the other
components.  Uses swank-backend.lisp as an interface to the actual
backends.

* `sly.el`: The the Emacs frontend that the user actually interacts
with and that connects to the SWANK server to send expressions to, and
retrieve information from the running Common Lisp system.

* `contrib/*`: Lisp related code for fancy add-ons to SLY.


## Architecture changes betwen SLY and SLIME

As of time of writing (SLY 1.0, SLIME 2.9) there aren't many big
differences between the two, except for the following list. If it's
not mentioned here, it's a safe bet that some particular mechanism
you're interested in stayed the same.

### SWANK-loading method

- SLIME immediately tells the Lisp process started by Emacs to use its
  own "swank-loader.lisp" to compile and load all possibly available
  lisp under its directory (including contrib's) before the SWANK
  server is created with `SWANK:CREATE-SERVER`.

- SLY, by default, looks for ASDF capabilities in process and, if
  they're available loads, SWANK like a regular ASDF
  system. Supporting Lisp code for contribs is loaded on demand by
  `sly-load-contribs`.

- Additionally, SLY will fallback to it's own version of
  `swank-loader.lisp` if it can't find ASDF in the Lisp process. This
  version of `swank-loader.lisp` also supports on-demand loading of
  contrib's code, so that any of the two methods is transparent from
  Emacs's perspective.

### mREPL

`slime-mrepl` is an experimental SLIME contrib that inspired
`sly-mrepl`, which is a much enhanced version of it and the default
REPL for SLY. The main difference to the popular `slime-repl` contrib
is that `sly-mrepl` is based on Emacs's own `comint.el` so that that
SLY does not need to worry about funcionality like history navigation
and persistent history, which are consistent with other Emacs modes
based on `comint.el`.

`sly-mrepl` allows multiple REPL's through the use of channels, which
are abstraction pioneered in SLIME. Channels are like separate
mailboxes in the Lisp runtime, and it's slightly different from the
regular `:emacs-rex` RPC calls in that they directly invoke a remote
method but expect no reply. Switch to the `*sly-events*` buffer to see
what's going on.

### Display-related code

SLIME's age and historical compatibility with XEmacs means it
reinvented (and possibly invented) many buffer/window/display managing
techniques that are avaiable today in GNU Emacs's core. Interactive
buttons, display-related and completion-code have all been pruned as
much as possible and now reuse Emacs' own libraries.

Hopefully this will make SLY's code focus on SLY's "business logic"
and easier to read. 


## Coding style

I have yet to write this, in the meantime try to be sensible and
emulate or improve on SLY's existing style.

I also just keep a sentence of the previous Coding Guide that I like
very much.

> Remember that to rewrite a program better is the sincerest form of
> code appreciation. When you can see a way to rewrite a part of SLY
> better, please do so!


## Pull requests

* Read [how to properly contribute to open source projects on Github][1].
* Use a topic branch to easily amend a pull request later, if necessary.
* Commit messages should use the syntax of GNU ChangLog entries.
* Open a [pull request][2] that relates to *only* one subject with a
  clear title and description in grammatically correct, complete
  sentences.

[1]: http://gun.io/blog/how-to-github-fork-branch-and-pull-request
[2]: https://help.github.com/articles/using-pull-requests
[3]: http://www.gnu.org/prep/standards/html_node/Style-of-Change-Logs.html#Style-of-Change-Logs
