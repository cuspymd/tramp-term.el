;;; tramp-term.el --- Automatic setup of directory tracking in ssh sessions.

;; Copyright (C) 2014 Randy Morris

;; Author: Randy Morris <randy.morris@archlinux.us>
;; Version: 0.1
;; Keywords: tramp, ssh
;; URL: https://github.com/randymorris/tramp-term.el

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This package provides a way to initiate ssh sessions within emacs
;; and have directory tracking automatically set up for editing files
;; with TRAMP.  No configuration is required on the remote host.
;;
;; Currently this is designed to work with a bash shell on the remote
;; host, however with a little work this could be changed to allow
;; different shells via a per-host configuration.
;;
;; The ideas presented here and various bits of code within were
;; lifted directly from http://www.emacswiki.org/emacs-se/AnsiTermHints.
;; Consider the editors of that page as contributors to this package.

;;; Code:

(require 'term)
(require 'tramp)

(defvar tt-after-initialized-hook nil
  "Hook called after tramp has been initialized on the remote
  host.  Hooks should expect a single arg which contains the
  hostname used to connect to the remote machine.")

;;;###autoload
(defun tramp-term ()
  "Create an ansi-term running ssh session and automatically
enable tramp integration in that terminal."
  (interactive)
  (let* ((host (tt--select-host))
         (hostname (car (last host)))
         (prompt-bound nil))
    (if (> (length host) 2)
        (message "Invalid host string")
      (unless (eql (catch 'tt--abort (tt--do-ssh-login host)) 'tt--abort)
        (tt--initialize hostname)
        (run-hook-with-args 'tt-after-initialized-hook hostname)
        (message "tramp-term initialized")))))

;; I imagine TRAMP has utility functions that would replace most of
;; this.  Needs investigation.
(defun tt--do-ssh-login (host)
  "Perform the ssh login dance."
  (let* ((user "")
         (hostname (car (last host))))
    (when (= (length host) 2)
      (setq user (format "%s@" (car host))))
    (tt--create-term hostname "ssh" (format "%s%s" user hostname)))
  (save-excursion
    (let ((bound 0))
      (while (not (tt-find-shell-prompt bound))
        (let ((yesno-prompt (tt-find-yesno-prompt bound))
              (passwd-prompt (tt-find-passwd-prompt bound))
              (service-unknown (tt-find-service-unknown bound)))
          (cond (yesno-prompt
                 (tt--confirm)
                 (setq bound (1+ yesno-prompt)))
                (passwd-prompt
                 (tt--handle-passwd-prompt)
                 (setq bound (1+ passwd-prompt)))
                (service-unknown (throw 'tt--abort 'tt--abort))
                (t (sleep-for 0.1))))))))

(defun tt-find-shell-prompt (bound)
  (re-search-backward tramp-shell-prompt-pattern bound t))

(defun tt-find-yesno-prompt (bound)
  (re-search-backward tramp-yesno-prompt-regexp bound t))

(defun tt-find-passwd-prompt (bound)
  (re-search-backward tramp-password-prompt-regexp bound t))

(defun tt-find-service-unknown (bound)
  (re-search-backward "Name or service not known" bound t))

(defun tt--handle-passwd-prompt ()
  "Reads a password from the user and sends it to the server."
  (term-send-raw-string
   (concat (read-passwd "Password: ") (kbd "RET"))))

(defun tt--confirm ()
  "Prompts the user to continue, aborts if they decline."
  (if (yes-or-no-p "Continue? ")
      (term-send-raw-string (concat "yes" (kbd "RET")))
    (term-send-raw-string (concat "no" (kbd "RET")))
    (throw 'tt--abort 'tt--abort)))

(defun tt--initialize (hostname)
  "Send bash commands to set up tramp integration."
  (term-send-raw-string (format "
function set-eterm-dir {
    echo -e \"\\033AnSiTu\" \"$USER\"
    echo -e \"\\033AnSiTc\" \"$PWD\"
    echo -e \"\\033AnSiTh\" \"%s\"
    history -a
}
PROMPT_COMMAND=set-eterm-dir
clear
" hostname)))

(defun tt--select-host ()
  "Return a host from a list of hosts."
  (let ((crm-separator "@"))
    (completing-read-multiple "[user@]host: " (tt--parse-hosts "~/.ssh/config"))))

(defun tt--parse-hosts (ssh-config)
  "Parse any host directives from SSH-CONFIG file and return them
as a list of strings"
  (mapcar 'cadr (delete nil (tramp-parse-sconfig ssh-config))))

(defun tt--create-term (new-buffer-name cmd &rest switches)
  "Create an ansi-term running an arbitrary command, including
extra parameters."
  (let* ((new-buffer-name (format "*%s*" new-buffer-name))
         (new-buffer-name (generate-new-buffer-name new-buffer-name))
         (new-buffer-name (replace-regexp-in-string "\*" "" new-buffer-name))
         (new-buffer-name (apply 'make-term new-buffer-name cmd nil switches)))
    (set-buffer new-buffer-name)
    (term-mode)
    (term-char-mode)
    (term-set-escape-char ?\C-x)
    (switch-to-buffer new-buffer-name)))

(provide 'tramp-term)
;;; tramp-term.el ends here
