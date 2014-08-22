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

* `sly.el`: The Superior Lisp Inferior Mode for Emacs, i.e. the
Emacs frontend that the user actually interacts with and that connects
to the SWANK server to send expressions to, and retrieve information
from the running Common Lisp system.

* `contrib/*`: Lisp related code for fancy add-ons to SLY.


## Coding style

_Remember that to rewrite a program better is the sincerest form of
code appreciation. When you can see a way to rewrite a part of SLY
better, please do so!_


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
