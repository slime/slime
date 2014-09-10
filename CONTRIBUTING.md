# The SLY Hacker's Handbook

## Reporting bugs

The most important thing when reporting bugs is making sure that the
developer has a way to reproduce it. To do this, he needs to rule out
interference from external factors like other Emacs extensions or
other Lisp-side code. Here's a great example of a bug report

```
$ emacs --version
Emacs 24.3
$ sbcl --version
SBCL 1.2.1
$ cd sly
sly $ emacs -Q -L . -l sly-autoloads --eval '(setq inferior-lisp-program "sbcl")' -f sly

I get the REPL but when I try to X, I get Y
OR
I don't get the REPL at all because frankinbogen!
```


## Coding style

This section is very empty, in the meantime try to be sensible and
emulate or improve on SLY's existing style.

### Commit messages

ChangeLog files are gone! However, the syntax of ChangeLogs is very
useful to everybody and Emacs supports it perfectly:

* in Emacs, for every snippet that you've changed, type `C-x 4 a` (or
  `add-change-log-entry-other-window`)

* Emacs will open up a ChangeLog buffer, but this is just a dummy
  buffer that you can ignore. However, the content inside it should be
  pasted (sans indentation) to the commit message.

* As an added bonus, if you are using Emacs >= 24.4 and `vc-dir` to
  prepare your commits, Emacs does that for you automatically.

The benefits of this format are great. One can still use `M-x
vc-print-log` in a source file and browse through its ChangeLog
without the hassle of ChangeLog conflicts.

### General philosophy

I keep a sentence of the previous Coding Guide that I like very much.

> Remember that to rewrite a program better is the sincerest form of
> code appreciation. When you can see a way to rewrite a part of SLY
> better, please do so!


## Lisp code file structure

The Lisp code is organized into these files:

* `lib/lisp/slynk-backend.lisp`: Definition of the interface to non-portable
features.  Stand-alone.

* `lib/lisp/backebd/slynk-<cmucl|...>.lisp`: Back-end implementation
for a specific Common Lisp system.  Uses slynk-backend.lisp.

* `lib/lisp/slynk.lisp`: The top-level server program, built from the other
components.  Uses slynk-backend.lisp as an interface to the actual
backends.

* `sly.el`: The Emacs front-end that the user actually interacts
with and that connects to the SLYNK server to send expressions to, and
retrieve information from the running Common Lisp system.

* `contrib/*`: Lisp related code for fancy add-ons to SLY.


## Architecture

As of time of writing (SLY 1.0, SLIME 2.9) there aren't many big
differences between the two, except for the following list. If it's
not mentioned here, it's a safe bet that some particular mechanism
you're interested in stayed the same.

### SLYNK-loading method

SLIME immediately tells the Lisp process started by Emacs to use its
own "slynk-loader.lisp" to compile and load all possibly available
lisp under its directory (including contrib's) before the SLYNK server
is created with `SLYNK:CREATE-SERVER`.

In SLY, the elisp variable `sly-init-function` is set to
`sly-init-using-asdf` by default, meaning that `M-x sly` will try to
load SLYNK via `ASDF:LOAD-SYSTEM`.

Contribs are also represented as ASDF system, so subsequent contrib
requests also load these on-demand via `ASDF:LOAD-SYSTEM` rather than
always forcing them on the user's Lisp run-time.

The previous item allows the developer to write completely independent
third-party extensions to SLY, with both SLY and SLYNK. See the URL
http://github.com/capitaomorte/sly-hello-world for an example
extension.

Additionally, if SLY detects that ASDF is not available in the Lisp
run-time, it will fallback to the old `slynk-loader.lisp` mechanism,
which has also been revised to support the previous two use cases. Any
of the two methods is transparent from Emacs's perspective.

### mREPL

`slime-mrepl` is an experimental SLIME contrib that inspired
`sly-mrepl`, which is a much enhanced version of it and the default
REPL for SLY. The main difference to the popular `slime-repl` contrib
is that `sly-mrepl` is based on Emacs's own `comint.el` so that that
SLY does not need to worry about functionality like history navigation
and persistent history, which are consistent with other Emacs modes
based on `comint.el`.

`sly-mrepl` allows multiple REPLs through the use of channels, which
are abstraction pioneered in SLIME. Channels are like separate
mailboxes in the Lisp run-time, and it's slightly different from the
regular `:emacs-rex` RPC calls in that they directly invoke a remote
method but expect no reply. Switch to the `*sly-events*` buffer to see
what's going on.

### Display-related code

SLIME's age and historical compatibility with XEmacs means it
reinvented (and possibly invented) many buffer/window/display managing
techniques that are available today in GNU Emacs's core. Interactive
buttons, display-related and completion-code have all been pruned as
much as possible and now reuse Emacs' own libraries.

Hopefully this will make SLY's code focus on SLY's "business logic"
and easier to read. 


## Pull requests

* Read [how to properly contribute to open source projects on Github][1].
* Use a topic branch to easily amend a pull request later, if necessary.
* Commit messages should use the syntax of GNU ChangeLog entries.
* Open a [pull request][2] that relates to *only* one subject with a
  clear title and description in grammatically correct, complete
  sentences.

[1]: http://gun.io/blog/how-to-github-fork-branch-and-pull-request
[2]: https://help.github.com/articles/using-pull-requests
[3]: http://www.gnu.org/prep/standards/html_node/Style-of-Change-Logs.html#Style-of-Change-Logs
