SLY News
--------

## SLY 1.0-Alpha (21-08-2014)

### On par with SLIME 2.9

### Completely redesigned REPL

The `sly-mrepl` contrib is a extensively redesigned
Read-Eval-Print-Loop for SLY. Multiple independent REPL's can be
created with the `sly-mrepl-new` command. `sly-mrepl` is fully based
on Emacs's `comint.el` and as such has a more familar interface for
history navigation.

### Regexp-capable M-x sly-apropos

If SLY detects that [`cl-ppcre`](http://weitz.de/cl-ppcre/) is
available in the Lisp side it will try to use it for "apropos" searches.

### Contribs enabled by default

By default, SLY enables the `sly-fancy` meta-contrib. This contains
`sly-mrepl`, `sly-autodoc`, `sly-fancy-inspector`, `sly-fancy-trace`,
`sly-fuzzy`, `sly-scratch`, `sly-package-fu`, `sly-fontifying-fu`,
`sly-trace-dialog`, `sly-indentation` and `sly-tramp`.

### More consistent interface

The "SLDB", "Inspector", "XREF" and "Apropos" buffers have been
redesigned to use a common classes of push button with consistent
interfaces. Buttons representing Lisp-side objects offer the same
right-click menu and keybindings, for all operations applicable to
represented object.

The same interfaces are also available in the "mREPL" and "Trace
Dialog" buffers.

`sly-mode` is now activated in every buffer related to SLY is now,
meaning global keybindings like `C-c T` and `C-c I` work everywhere.

### Multiple inspectors

Interactive commands for inspecting Lisp objects can be prefixed with
`C-u` to prompt the user for an inspector name. Separate inspector
streams are kept. An idea by Olof-Joachim Frahm
(http://macrolet.net/).

### Uses ASDF to load contribs by default

### Merged outstanding SLIME pull requests

  https://github.com/slime/slime/pull/175
  https://github.com/slime/slime/pull/145

### Other miscelaneous enhancements over SLIME

Faces have been revised and are based on Emac's standard
faces. Hopefully, SLY will remain readable even when the user
switches themes.

SLY's modeline string is placed at the right side of the modeline.

SLY intrudes less than SLIME in the Emacs namespace, and uses more
standard idoms. Macros like `with-struct` and `sly-define-keys` have
been removed.

Buffer names have been consolidated: every buffer name related to SLY
obeys the same structure, stating the type, connection name and any
pertinent additional info.

Reading from the minibuffer has been improved. SLY uses `ido`
completion by default, but it can customized via
`sly-complete-symbol-function`.

Interactive expression evaluation will use a separate buffer when the
results is too big to fit in the echo area.

Messages and warnings prefix themselves accordingly with "[sly]".

### Anti-NEWS

SLY 1.0-Alpha supports Emacs 24.3 only. SLY 1.0 is expected to only
support Emacs 24.4.

There is very limited backward compatibility SLIME and only in the
SLIME->SLY direction, meaning a contrib-less SLIME may connect to a
SWANK server started by SLY, but any other combination will probably
fail beyond very basic functionality.

The `slime-presentations` has been removed. The consistent button
interface is better.

The `slime-c-p-c` contrib has been removed, as it contained a lot of
non-standard window-managing code. Its functionality has been merged
into `sly-fuzzy` and setting `sly-complete-symbol-function` to
`sly-c-p-c-complete-symbol` should give you the previous behaviour.









