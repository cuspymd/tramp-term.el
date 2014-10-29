;; Various bits of code here were lifted from
;; http://www.emacswiki.org/emacs-se/AnsiTermHints
;;
;; The intent of this is to wrap the tramp directory tracking
;; in a convenient and reusable way.

(require 'term)

(defvar tt-after-tramp-initialized-hook nil
  "Hook called after tramp has been initialized on the remote
  host.  Each hook is passed a single arg which contains the
  hostname used to connect to the remote machine.")

;;;###autoload
(defun tramp-term ()
  "Create an ansi-term running ssh sesstion and automatically
enable tramp integration in that terminal."
  (interactive)
  (let* ((host (tt--select-host))
	 (hostname (car (last host)))
	 (prompt-bound nil))
    (if (> (length host) 2)
        (message "Invalid host string")
      (tt--do-ssh-login host)
      (tt--initialize-tramp hostname)
      (run-hook-with-args 'tt-after-tramp-initialized-hook hostname)
      (message "tramp-term initialized"))))

(defun tt--do-ssh-login (host)
  "Perform the ssh login dance.  Supports password or cert logins
to HOSTNMAE."
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

(defun tt--initialize-tramp (hostname)
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
    (completing-read-multiple "[user@]host: " (tt--parse-hosts-from-ssh-config))))

(defun tt--parse-hosts-from-ssh-config ()
  "Parse any host directives from ~/.ssh/config and return them
as a list of strings"
  (with-temp-buffer
    (insert-file-contents "~/.ssh/config")
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
