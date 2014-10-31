# tramp-term.el

Provides a quick way to launch an ssh session in an ansi-term with directory
tracking pre-configured.  This makes opening remote files via TRAMP directly
from the ssh session as easy as <kbd>C-x</kbd> <kbd>C-f</kbd>.

## Installation

It is strongly recommended to install tramp-term with `package.el` (included in
emacs 24+).  Until a package is available on [MELPA][1] or [marmalade][2] you
can clone this repo and use `package-install-file` to install tramp-term
directly.

[1]: http://melpa.org/
[2]: http://marmalade-repo.org/

## Basic usage

Add the following to your init file to make tramp-term available:

    (require 'tramp-term)

Only one command is provided.  Run it to be prompted in the minibuffer for a
user (optional) and host to connect to:

    M-x tramp-term

A list of hosts will be provided for auto-completion.  These hosts are taken
from the configuration in ~/.ssh/config.

Once the ssh session has been established simply open files as you would in a
normal ansi-term or shell buffer with `find-file`:

    C-x C-f

As you change directories, emacs' `default-directory` will be kept in sync,
including the TRAMP syntax to open the remote files.

### Configuration

* `tt-after-initialized-hook` - Hook called after tramp-term has been
  initialized on the remote host.  Hooks should expect a single
  argument which contains the hostname used to connect to the remote
  machine.

### Limitations

Currently tramp-term only supports bash shells on the remote host.  This would
be pretty easy to extend but could be tricky to make configurable since both
users and hosts would have to be taken into account.  If you'd like this, take a
crack at it and submit a pull request.  The bash specific code lives in
`tt--initialize`.  To make this work generically, `tt--initialize` would have to
dispatch to different initialization functions based on the configuration.  Here
is an example of that function that works with tcsh instead:

```elisp
(defun tt--initialize (hostname)
  "Send bash commands to set up tramp integration."
  (term-send-raw-string (format "
alias precmd 'echo \"\\033AnSiTu\" \"$USER\"; echo \"\\033AnSiTc\" \"$PWD\"; echo \"\\033AnSiTh\" \"%s\"'
clear
" hostname)))
```
