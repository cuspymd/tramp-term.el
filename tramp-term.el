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
      (tt--do-ssh-login host)
      (tt--initialize hostname)
      (run-hook-with-args 'tt-after-initialized-hook hostname)
      (message "tramp-term initialized"))))

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
    (while (not (re-search-backward tramp-shell-prompt-pattern nil t))
      (let ((prompt-pos (re-search-backward tramp-password-prompt-regexp prompt-bound t)))
        (if (not prompt-pos)
            (sleep-for 0.1)
          (setq prompt-bound (1+ prompt-pos))
          (term-send-raw-string (concat (read-passwd "Password: ") (kbd "RET"))))))))

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
  (let ((ssh-config "~/.ssh/config")
        (prompt "[user@]host: "))
    (if (file-exists-p ssh-config)
        (let ((crm-separator "@"))
          (completing-read-multiple prompt (tt--parse-hosts ssh-config)))
      (list (completing-read prompt nil)))))

;; This already exists somewhere in TRAMP for sure, I just had the
;; time to un-invent this wheel yet.  See the completion resulting
;; from "C-x C-f /ssh:<Tab>".
(defun tt--parse-hosts (ssh-config)
  "Parse any host directives from SSH-CONFIG file and return them
as a list of strings"
  (with-temp-buffer
    (insert-file-contents ssh-config)
    (let ((beg (point)))
      (while (search-forward-regexp "^host[[:blank:]]+" nil t)
        (delete-region beg (point))
        (end-of-line)
        (insert " ")
        (setq beg (point))))
    (delete-region (1- (point)) (point-max))
    (replace-regexp "[[:blank:]]+" "\" \"" nil (point-min) (point-max))
    (read (concat "(\"" (buffer-string) "\")"))))

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
