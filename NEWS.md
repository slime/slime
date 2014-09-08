SLY News
========

SLY 1.0.0-alpha
---------------

### On par with upcoming SLIME 2.10

SLY contains all the bugfixes and backend enhancements contributed to
SLIME.

### Completely redesigned REPL

The `sly-mrepl` contrib is a extensively redesigned
Read-Eval-Print-Loop for SLY.

Multiple independent REPL's can be created with the `sly-mrepl-new`
command.

`sly-mrepl` is fully based on Emacs's `comint.el` and as such has a
more familiar interface for history navigation. `C-r` and `C-M-r`, when
used at the prompt, should provide a bash/zsh-like experience.

The prompt gives a visual indication of long-running evaluations that
haven't returned yet.

The prompt gives a visual indication of the number of debugger levels
currently caused by the last evaluation.

Every return part can be inspected and re-returned as the last value.

`C-c ~` on any lisp file switches to the REPL synchronizing its
`*PACKAGE*` and `*DEFAULT-PATHNAME-DEFAULTS*` with that of the file's.

Output redirection is automatically setup. The first REPL created is
the target for all threads' output onto the standard output
streams. REPLs created afterward only see their own output. To turn
it off configure the SLYNK-side variable
`SLYNK-MREPL:*GLOBALLY-REDIRECT-IO*`. Any REPL created after that
will not gather other threads' output, and threads associated with a
REPL will output nowhere.

Dedicated stream for output is automatically set up. Configure the
`SLYNK-MREPL:*USE-DEDICATED-OUTPUT-STREAM*` if it doesn't suit you.

There is saner scrolling behavior as provided by the `comint.el`
substrate. The variables `comint-scroll-show-maximum-output`,
`comint-scroll-to-bottom-on-input` and
`comint-scroll-to-bottom-on-output` (which see) are set to `nil` by
default, but the user might reconfigure them to her liking in the
`sly-mrepl-hook`.

There are Sylvesters. See `sly-mrepl-pop-sylvester`.

### Regexp-capable M-x sly-apropos

If SLY detects that [`cl-ppcre`](http://weitz.de/cl-ppcre/) is
available in the Lisp side it will try to use it for "apropos"
searches, otherwise the user is hinted at this possibility. As regexp
searches are slower, this is only triggered if the pattern is a valid
regular-expression.

This is the default implementation of the new
`SLYNK-BACKEND:MAKE-APROPOS-MATCHER` interface that particular
implementations may wish to override.

### Contribs enabled by default

By default, SLY enables the `sly-fancy` meta-contrib. This contains
`sly-mrepl`, `sly-autodoc`, `sly-fancy-inspector`, `sly-fancy-trace`,
`sly-fuzzy`, `sly-scratch`, `sly-package-fu`, `sly-fontifying-fu`,
`sly-trace-dialog`, `sly-indentation` and `sly-tramp`.

### SLY uses ASDF and loads contribs on demand.

If the user sets `sly-contribs` to `sly-mrepl` she can be sure that no
Lisp code related to other contribs appears in your run-time. Even if
ASDF is unavailable, an improved version of the `slynk-loader.lisp`
program will also behave non-intrusively.

This change also enables developers to write completely independent
third-party extensions like
[in this example](http://github.com/capitaomorte/sly-hello-world).

See the CONTRIBUTING.md file for more details on architecture changes.

### More consistent interface

The SLY-DB, Inspector, XREF and Apropos buffers have been
redesigned to use a common class of push button with consistent
interfaces.

This means, for instance, that the `i` and `p` commands ("inspect" and
"pretty-print") are available in every one of these buffers'
interactive parts. The `v` ("view-source") command is only available
where it makes sense for the represented object. These is known as a
button's "part-action".

The same interfaces are also available in the "mREPL" and "Trace
Dialog" buffers.

`sly-mode` is now activated in every buffer related to SLY is now,
meaning global keybindings like `C-c T` and `C-c I` work everywhere.

### Multiple inspectors

Interactive commands for inspecting Lisp objects can be prefixed with
`C-u` to prompt the user for an inspector name. Separate inspector
streams are kept. An idea by Olof-Joachim Frahm
(http://macrolet.net/).

### Copy function call to REPL

An experimental feature: from the Trace Dialog or SLY-DB buffers, a
new button action called "Copy call to REPL" is offered and bound to 

If SLY can calculate the arguments and the function symbol of the
function call represented in the backtrace of trace entry, it will
return them to the REPL, along with an uncommitted input line that
refers back to them and calls the function.

### Other miscellaneous enhancements over SLIME

Faces have been revised and are based on Emacs's standard
faces. Hopefully, SLY will remain readable even when the user
switches themes.

Popping up windows and buffers has been much improved. Windows are
reused if the buffer names match or are similar, meaning that no
longer will the SLY-DB "jump around" in multi-window configurations when
selecting a restart that signals another error.

Interactive expression evaluation will use a separate buffer when the
results is too big to fit in the echo area.

SLY's mode-line string is placed at the right side of the mode-line.

SLY intrudes less than SLIME in the Emacs name-space, and uses more
standard idoms. Macros like `with-struct` and `sly-define-keys` have
been removed.

Buffer names have been consolidated: every buffer name related to SLY
obeys the same structure, stating the type, connection name and any
pertinent additional info.

Reading from the minibuffer has been improved. SLY uses `ido`
completion by default, but it can customized via
`sly-complete-symbol-function`.

Messages and warnings prefix themselves accordingly with "[sly]".

SLY asks the user to confirm the Lisp to kill with `M-x sly-quit` or
disconnect with `M-x sly-disconnect`. It doesn't ask any irrelevant
questions when there is no selected connection beforehand. Github
issue #5.

Fixed source locations when recompiling from an xref buffer.  This is
outstanding https://github.com/slime/slime/pull/175 pull-request in
SLIME. Thanks Bart Botta.

Fixed bugs in `contrib/sly-package-fu.el`. This is the outstanding
https://github.com/slime/slime/pull/145 pull-request in SLIME. Thanks
Leo Liu.

### Anti-NEWS

SLY 1.0-Alpha supports Emacs 24.3 only. SLY 1.0 is expected to only
support Emacs 24.4.

There is very limited backward compatibility SLIME and only in the
SLIME->SLY direction, meaning a contrib-less SLIME may connect to a
SLYNK server started by SLY, but any other combination will probably
fail beyond very basic functionality.

The portable profiling commands have been removed from the SLY menu
`metering.lisp` and the profiling interfaces in `slynk-backend.lisp`
have been kept. SLY 1.0.0 will hopefully have a better integrated
portable profiler.

The `slime-presentations` has been removed. The consistent button
interface is better.

The `slime-c-p-c` contrib has been removed, as it contained a lot of
non-standard window-managing code. Its functionality has been merged
into `sly-fuzzy` and setting `sly-complete-symbol-function` to
`sly-c-p-c-complete-symbol` should give you the previous behavior.
