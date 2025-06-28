[![MELPA](https://melpa.org/packages/tramp-term-badge.svg)](https://melpa.org/#/tramp-term)
# tramp-term.el

Provides a quick way to launch an ssh session in an ansi-term with directory
tracking pre-configured.  This makes opening remote files via TRAMP directly
from the ssh session as easy as <kbd>C-x</kbd> <kbd>C-f</kbd>.


## Installation

It is strongly recommended to install tramp-term with `package.el` (included in
emacs 24+).  A package is currently available on available on [MELPA][1].

[1]: http://melpa.org/

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

### Shell Support

tramp-term supports the following shells on remote hosts:

* **Bash** (default)
* **Zsh** 
* **Tcsh** (including csh)

The shell type can be configured in several ways:

1. **Auto-detection** (recommended): Set `tramp-term-default-shell` to `'auto` to automatically detect the remote shell
2. **Manual configuration**: Set a default shell type or configure per-host shells
3. **Interactive selection**: If auto-detection fails, you'll be prompted to select the shell type

### Configuration

* `tramp-term-default-shell` - Default shell to use for connections. Options:
  - `'bash` (default)
  - `'auto` (auto-detect remote shell)
  - `'zsh`
  - `'tcsh`

* `tramp-term-host-shells` - Alist of (hostname . shell-type) pairs for remembered shell types. This is automatically populated when shells are detected or manually set.

* `tramp-term-after-initialized-hook` - Hook called after tramp-term has been
  initialized on the remote host.  Hooks should expect a single
  argument which contains the hostname used to connect to the remote
  machine.

#### Auto-Detection

When `tramp-term-default-shell` is set to `'auto`, tramp-term will:

1. Attempt to detect the remote shell using `$0` variable
2. Cache the detected shell type for future connections to the same host
3. Fall back to user selection if detection fails

### Example Configuration

```elisp
;; Enable auto-detection for all hosts
(setq tramp-term-default-shell 'auto)

;; Or set specific shells for specific hosts
(setq tramp-term-host-shells '(("server1.example.com" . zsh)
                               ("server2.example.com" . tcsh)))
```
