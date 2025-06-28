;;; tramp-term.el --- Automatic setup of directory tracking in ssh sessions

;; Copyright (C) 2014 Randy Morris

;; Author: Randy Morris <randy.morris@archlinux.us>
;; Version: 0.5
;; Keywords: comm, terminals
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

;; This package provides a way to initiate ssh sessions within Emacs
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

(defgroup tramp-term nil
  "Automatic setup of directory tracking in ssh sessions."
  :group 'comm)

(defcustom tramp-term-default-shell 'bash
  "Default shell to use for tramp-term connections.
When set to 'auto, will attempt to auto-detect the remote shell.
Supported shells: bash, zsh, tcsh."
  :type '(choice (const :tag "Bash (default)" bash)
                 (const :tag "Auto-detect" auto)
                 (const :tag "Zsh" zsh)
                 (const :tag "Tcsh" tcsh))
  :group 'tramp-term)

(defcustom tramp-term-host-shells '()
  "Alist of (hostname . shell-type) pairs for remembered shell types.
This is automatically populated when shells are detected or manually set."
  :type '(alist :key-type string :value-type symbol)
  :group 'tramp-term)

(defvar tramp-term-after-initialized-hook nil
  "Hook called after tramp has been initialized on the remote host.
Hooks should expect a single arg which contains the
hostname used to connect to the remote machine.")

;;;###autoload
(defun tramp-term (&optional host-arg)
  "Create an `ansi-term' running ssh session.
And automatically enable tramp integration in that terminal.
Optional argument HOST-ARG is a list or one or two elements,
the last of which is the host name."
  (interactive)
  (let* ((host (or host-arg (tramp-term--select-host)))
         (hostname (car (last host)))
         (prompt-bound nil))
    (if (or (> (length host) 2)
            (eql (length hostname) 0))
        (message "Invalid host string")
      (unless (eql (catch 'tramp-term--abort (tramp-term--do-ssh-login host)) 'tramp-term--abort)
        (tramp-term--initialize hostname)
        (run-hook-with-args 'tramp-term-after-initialized-hook hostname)
        (message "tramp-term initialized")))))

;; I imagine TRAMP has utility functions that would replace most of
;; this.  Needs investigation.
(defun tramp-term--do-ssh-login (host)
  "Perform the ssh login at HOST."
  (let* ((user "")
         (hostname (car (last host))))
    (when (= (length host) 2)
      (setq user (format "%s@" (car host))))
    (tramp-term--create-term hostname "ssh" (format "%s%s" user hostname)))
  (save-excursion
    (let ((bound 0))
      (while (not (tramp-term--find-shell-prompt bound))
        (let ((yesno-prompt (tramp-term--find-yesno-prompt bound))
              (passwd-prompt (tramp-term--find-passwd-prompt bound))
              (service-unknown (tramp-term--find-service-unknown bound)))
          (cond (yesno-prompt
                 (tramp-term--confirm)
                 (setq bound (1+ yesno-prompt)))
                (passwd-prompt
                 (tramp-term--handle-passwd-prompt)
                 (setq bound (1+ passwd-prompt)))
                (service-unknown (throw 'tramp-term--abort 'tramp-term--abort))
                (t (sleep-for 0.1))))))))

(defun tramp-term--find-shell-prompt (bound)
  "Find shell prompt with a buffer position BOUND."
  (re-search-backward tramp-shell-prompt-pattern bound t))

(defun tramp-term--find-yesno-prompt (bound)
  "Find yesno prompt with a buffer position BOUND."
  (re-search-backward tramp-yesno-prompt-regexp bound t))

(defun tramp-term--find-passwd-prompt (bound)
  "Find password prompt with a buffer position BOUND."
  (re-search-backward tramp-password-prompt-regexp bound t))

(defun tramp-term--find-service-unknown (bound)
  "Find service unknown with a buffer position BOUND."
  (re-search-backward "Name or service not known" bound t))

(defun tramp-term--handle-passwd-prompt ()
  "Read a password from the user and sends it to the server."
  (term-send-raw-string
   (concat (read-passwd "Password: ") (kbd "RET"))))

(defun tramp-term--confirm ()
  "Prompts the user to continue, aborts if they decline."
  (if (yes-or-no-p "Continue? ")
      (term-send-raw-string (concat "yes" (kbd "RET")))
    (term-send-raw-string (concat "no" (kbd "RET")))
    (throw 'tramp-term--abort 'tramp-term--abort)))

(defun tramp-term--initialize (hostname)
  "Set up tramp integration for HOSTNAME using appropriate shell."
  (let* ((cached-shell (cdr (assoc hostname tramp-term-host-shells)))
         (shell-type (or cached-shell 
                        (if (eq tramp-term-default-shell 'auto)
                            (tramp-term--detect-shell hostname)
                          tramp-term-default-shell))))
    (when (and (not cached-shell) shell-type)
      (add-to-list 'tramp-term-host-shells (cons hostname shell-type))
      (customize-save-variable 'tramp-term-host-shells tramp-term-host-shells))
    (cond
     ((eq shell-type 'bash) (tramp-term--initialize-bash hostname))
     ((eq shell-type 'zsh) (tramp-term--initialize-zsh hostname))
     ((eq shell-type 'tcsh) (tramp-term--initialize-tcsh hostname))
     (t (tramp-term--initialize-bash hostname)))))

(defun tramp-term--detect-shell (hostname)
  "Attempt to detect the shell type for HOSTNAME."
  (message "Auto-detecting shell for %s..." hostname)
  (let ((detection-marker (format "TRAMP_SHELL_DETECT_%d:" (random 10000)))
        (shell-type nil))
    ;; Send detection command with unique marker
    (term-send-raw-string (format "echo \"%s$0\"\n" detection-marker))
    (sleep-for 1.0)  ; Wait for response
    
    ;; Try to parse the output
    (setq shell-type (tramp-term--parse-shell-output detection-marker))
    
    (if shell-type
        (progn
          (message "Detected %s shell for %s" shell-type hostname)
          shell-type)
      (progn
        (message "Auto-detection failed for %s, asking user..." hostname)
        (tramp-term--prompt-for-shell hostname)))))

(defun tramp-term--parse-shell-output (detection-marker)
  "Parse terminal buffer to find shell type using DETECTION-MARKER."
  (save-excursion
    (goto-char (point-max))
    (when (re-search-backward (regexp-quote detection-marker) nil t)
      (let ((line-start (line-beginning-position))
            (line-end (line-end-position)))
        (goto-char line-start)
        (when (re-search-forward (concat (regexp-quote detection-marker) "\\(.+\\)") line-end t)
          (let ((shell-path (match-string 1)))
            (tramp-term--classify-shell shell-path)))))))

(defun tramp-term--classify-shell (shell-path)
  "Classify shell type from SHELL-PATH string."
  (when (stringp shell-path)
    (let ((shell-path (string-trim shell-path)))
      (cond
       ;; Login shells with dash prefix
       ((string-match-p "^-?bash$\\|/bash$" shell-path) 'bash)
       ((string-match-p "^-?zsh$\\|/zsh$" shell-path) 'zsh)
       ((string-match-p "^-?tcsh$\\|/tcsh$\\|^-?csh$\\|/csh$" shell-path) 'tcsh)
       ;; Full paths
       ((string-match-p "/bin/bash\\|/usr/bin/bash\\|/usr/local/bin/bash" shell-path) 'bash)
       ((string-match-p "/bin/zsh\\|/usr/bin/zsh\\|/usr/local/bin/zsh" shell-path) 'zsh)
       ((string-match-p "/bin/tcsh\\|/usr/bin/tcsh\\|/usr/local/bin/tcsh" shell-path) 'tcsh)
       ((string-match-p "/bin/csh\\|/usr/bin/csh\\|/usr/local/bin/csh" shell-path) 'tcsh)
       ;; Default fallback
       (t nil)))))

(defun tramp-term--prompt-for-shell (hostname)
  "Prompt user to select shell type for HOSTNAME."
  (let ((shell-choice 
         (completing-read 
          (format "Select shell for %s: " hostname)
          '("bash" "zsh" "tcsh")
          nil t nil nil "bash")))
    (intern shell-choice)))

(defun tramp-term--initialize-bash (hostname)
  "Send bash commands to set up tramp integration for HOSTNAME."
  (term-send-raw-string (format "
function set-eterm-dir {
    echo -e \"\\033AnSiTu\" \"%s$USER\"
    echo -e \"\\033AnSiTc\" \"$PWD\"
    echo -e \"\\033AnSiTh\" \"%s\"
    history -a
}
PROMPT_COMMAND=\"${PROMPT_COMMAND:+$PROMPT_COMMAND ;} set-eterm-dir\"
clear
" (if (version= emacs-version "26.1") "ssh:" "") hostname)))

(defun tramp-term--initialize-zsh (hostname)
  "Send zsh commands to set up tramp integration for HOSTNAME."
  (term-send-raw-string (format "
set-eterm-dir() {
    echo -e \"\\033AnSiTu\" \"%s$USER\"
    echo -e \"\\033AnSiTc\" \"$PWD\"
    echo -e \"\\033AnSiTh\" \"%s\"
}
precmd_functions+=(set-eterm-dir)
clear
" (if (version= emacs-version "26.1") "ssh:" "") hostname)))

(defun tramp-term--initialize-tcsh (hostname)
  "Send tcsh commands to set up tramp integration for HOSTNAME."
  (term-send-raw-string (format "
alias set-eterm-dir '/bin/echo -e \"\\033AnSiTu\" \"%s$USER\"; /bin/echo -e \"\\033AnSiTc\" \"$PWD\"; /bin/echo -e \"\\033AnSiTh\" \"%s\"'
alias cd 'chdir \\!*; set-eterm-dir'
alias pushd 'pushd \\!*; set-eterm-dir'
alias popd 'popd \\!*; set-eterm-dir'
set-eterm-dir
clear
" (if (version= emacs-version "26.1") "ssh:" "") hostname)))

(defun tramp-term--select-host ()
  "Return a host from a list of hosts."
  (let* ((crm-separator "@")
         (default-host (tramp-term-default-host)))
    (completing-read-multiple
     (tramp-term-prompt default-host)
     (tramp-term--parse-hosts "~/.ssh/config")
     nil
     nil
     nil
     nil
     default-host
     )))

(defun tramp-term-prompt (default-host)
  "Make prompt string with DEFAULT-HOST."
  (let ((default-string (when default-host
                         (format " (default %s)" default-host))))
    (concat "[user@]host" default-string ": ")))

(defun tramp-term-default-host ()
  "Return default host based on `default-directory' which is a tramp file."
  (when (tramp-tramp-file-p default-directory)
    (let* ((user (file-remote-p default-directory 'user))
           (host (file-remote-p default-directory 'host)))
      (if user (format "%s@%s" user host) host))))

(defun tramp-term--parse-hosts (ssh-config)
  "Parse any host directives from SSH-CONFIG file and return them as a list of strings."
  (mapcar 'cadr (delete nil (tramp-parse-sconfig ssh-config))))

(defun tramp-term--create-term (new-buffer-name cmd &rest switches)
  "Create an `ansi-term' running an arbitrary CMD with NEW-BUFFER-NAME.
Including extra parameters SWITCHES."
  (let ((new-buffer-name (generate-new-buffer-name (format "*%s*" new-buffer-name))))
    (with-current-buffer (make-term new-buffer-name cmd nil (car switches))
      (rename-buffer new-buffer-name)   ; Undo the extra "*"s that
                                        ; make-term insists on adding
      (term-mode)
      (term-char-mode)
      (term-set-escape-char ?\C-x))
    (switch-to-buffer new-buffer-name)))

(provide 'tramp-term)
;;; tramp-term.el ends here
