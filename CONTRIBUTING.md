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

The code is organized into these files:

* `slynk/slynk-backend.lisp`: Definition of the interface to non-portable
features.  Stand-alone.

* `slynk/backend/slynk-<cmucl|...>.lisp`: Back-end implementation
for a specific Common Lisp system.  Uses slynk-backend.lisp.

* `slynk/slynk.lisp`: The top-level server program, built from the other
components.  Uses `slynk-backend.lisp` as an interface to the actual
backends.

* `sly.el`: The Emacs front-end that the user actually interacts
with and that connects to the Slynk server to send expressions to, and
retrieve information from the running Common Lisp system.

* `contrib/sly-<extension>.el`: Elisp code for SLY extensions.

* `contrib/slynk-<extension>.lisp`: Supporting Common Lisp related
code for a particular extension.


## SLY-Slynk RPC protocol

The info in this section would be something for a future "Slynk
Programmer's Guide" to be included in the regular manual or a separate
one.

Follows a brief description of the SLY-Slynk protocol. The protocol is
*s-exp messages* over *s-exp primitives* over *UTF-8* over *TCP*.
Let's start top-down:

### S-exp messages

Most messages in the top group look like Lisp function calls. The
functions are known as "Slyfuns" and are defined with a `DEFSLYFUN`
operator in the `slynk-*.lisp` side. These are the "remote procedures"
of the RPC protocol. There must be about 100 or so of them, maybe
more, I haven't counted. Slyfuns appear in both Slynk's core and in
supporting contrib's Slynk code.

For a future reference manual, I think there has to be a way to
automatically harvest the `DEFSLYFUN` definitions and their
docstrings.

Another type of message contains calls to "channel methods". These are
slightly different from Slyfuns. Their return value is ignored, but
otherwise they also work like function calls. They're good for
expressing a reply-free evaluation in the context of a "channel".

These are defined with `sly-define-channel-method` and
`DEFINE-CHANNEL-METHOD` and on the SLY and Slynk sides, respectively.

The only use right now is in `sly-mrepl.el`,

### S-exp primitives

This is a much smaller set of primitives, the most common is
`:EMACS-REX`, "rex" is for "Remote EXecution".

Informally it's saying: "here is Slyfun X's call number 3487 with
argumentss Y, for evaluation in thread Z" ). The asynchronous reply
`:RETURN`, if it ever arrives, will be "your call 3487 returned the
following sexp".

```lisp
(:emacs-rex
 (slynk:connection-info)
 nil t 1)
(:return
 (:ok
  (:pid 16576 :style :spawn :encoding
        :lisp-implementation
        (:type "International Allegro CL Enterprise Edition" :name "allegro" :version "8.1 [Windows] (Sep 3, 2008 19:38)" :program nil)
        :package
        (:name "COMMON-LISP-USER" :prompt "CL-USER")
        :version "1.0.0-alpha"))
 1)
 ```

The return value, read into Elisp sexps is what is passed to the
callback argument to the Elisp function `sly-eval-async`. Here's the
way to get the PID of the underlying Slynk process.

```elisp
(sly-eval-async '(slynk:connection-info)
   (lambda (info) (plist-get info :pid)))
```

The primitives `:CHANNEL-SEND` and `:EMACS-CHANNEL-SEND` implement
channel methods. Channels are named by number, and normally have a
special serving thread in the Common Lisp implementation of
Slynk. Here is an extract showing the `:PROCESS`, `:WRITE-VALUES` and
`:PROMPT` channel methods for the REPL.

```lisp
(:emacs-channel-send 1
                     (:process "(list 1 2 3)"))
(:channel-send 1
               (:write-values
                (("(1 2 3)" 2))))
(:channel-send 1
               (:prompt "COMMON-LISP-USER" "CL-USER" 0))
```

There are also debugger-specific primitives, like `:DEBUG-ACTIVATE`
and `:DEBUG-RETURN`. Then there are indentation-specific primitives
like `:INDENTATION-UPDATE`. These could/should become
`:EMACS-CHANNEL-SEND`s in the future (but that would probably finally
break Swank compatibility).

### UTF-8 and TCP

UTF-8 is relevant because the information in the wire are text-encoded
sexp's that sometimes carry strings with chunks of code, for example,
and these can have funky characters.

TCP is well TCP, a host and a port and reliable transfer make SLY work
well over any IP network.

### Common Lisp bias

*Note: This section is very incomplete*

SLY has is primarily a Common-Lisp IDE and the supporting Slynk have
strong Common-lisp bias. There have been many attempts, some quite
successful at creating Slynk backends for other languages.

I believe that some of the Slyfuns will always be Common-Lisp specific
and should be marked as such. Others can perhaps be more naturally
adapted to other languages.

It's very important that a future reference manual have this in
consideration: remove the CL bias from the protocol's description, at
least from some of its layers, so that things like
[swank-js](https://github.com/swank-js/swank-js) can one day be more
easily implemented.


## Architecture changes from SLIME to SLY

As of time of writing (SLY 1.0, SLIME 2.9) the following list
summarizes the main architecture differences between SLY and SLIME. If
it's not mentioned here, it's a safe bet that some particular
mechanism you're interested in stayed the same and any SLIME
documentation is applicable to SLY.

### Swank is now called Slynk

SLY can be loaded alongside SLIME both in the same Emacs or Lisp
image. This interoperability meant that SLY's Lisp server had to be
renamed to "Slynk".

SLY can still speak the Swank protocol, and should be able to connect
to any other non-Lisp backends such as Christopher Rhodes' [swankr][4]
or have non-SLIME clients connect to it such as Robert Brown's
[swank-client][5].

This is done via a contrib called `sly-retro` and its `slynk-retro`
counterpart. The contrib's code is loaded by `M-x sly` or `M-x
sly-connect` *on demand*, meaning that it is possible to start the
Slynk server without the contrib's Lisp counterpart. See the section
called "Slynk-loading method"" for how this works in SLY.

*If* it is loaded, `sly-retro` ensures that messages leaving SLY still
look like

     (:emacs-rex (swank:connection-info) nil t 1)

It also ensures that incoming messages are directed to the `SLYNK` and
`SLYNK-BACKEND` packages. This particular redirection is done via
package nicknames and a trick in `lib/lisp/slynk-rpc.lisp`. The trick
is necessary only for the first bootstrapping messages, because on
startup the `sly-retro` contrib hasn't kicked in and nicknames are not
immediately setup.

The nicknames pose a compatibility hazard if the user tries to load
SLIME's Swank server into the Lisp image where Slynk is already
setup. Therefore, users wishing to run both servers alongside in the
same Lisp image must take care to not load the `sly-retro` contrib,
which takes only a line of Emacs-Lisp code:

     (setq sly-contribs (delete 'sly-retro sly-contribs))

[4]: https://github.com/gigamonkey/swankr
[5]: https://github.com/brown/swank-client

### Slynk-loading method

In SLIME, `M-x slime` immediately tells the Lisp process started by
Emacs to use SLIME's own `slynk-loader.lisp` program to compile and
load all possibly available lisp under its directory (including
contrib's) before the Slynk server is created with
`SLYNK:CREATE-SERVER`.

In SLY, the elisp variable `sly-init-function` is set to
`sly-init-using-asdf` by default, meaning that `M-x sly` will try to
load Slynk via `ASDF:LOAD-SYSTEM`. But this will load only Slynk and
no contribs.

Slynk contribs are also represented as ASDF systems, and
`sly-load-contribs` will add the contrib's path to the ASDF load
path. The `SLYNK:REQUIRE-MODULE` abstraction will call then
`ASDF:LOAD-SYSTEM`.

This way, contrib's Lisp code is available on demand but not forced
them on the user's Lisp run-time.

This also allows the developer to write completely independent
third-party extensions to SLY, with both Emacs and Lisp parts. See the
URL http://github.com/capitaomorte/sly-hello-world for an example
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
method but expect no reply.

In `slynk-mrepl.lisp`, the `mrepl` class multiple inherits from
`swank:channel` and `swank:listener`. The first takes care of
channel-based communication and the second has the REPL-specific
context.

See the section on the "RPC protocl" and switch to the `*sly-events*`
buffer to see what's going on. 

### Display-related code

SLIME's age and historical compatibility with XEmacs means it
reinvented (and possibly invented) many buffer/window/display managing
techniques that are available today in GNU Emacs's core. Interactive
buttons, display-related and completion-code have all been pruned as
much as possible and now reuse Emacs' own libraries.

Hopefully this will make SLY's code focus on SLY's "business logic"
and easier to read.

### Channels

TODO

### Listeners

TODO


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
