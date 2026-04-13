;;; web.el --- Work with the web -*- lexical-binding: t; -*-

;;; Commentary:
;; Packages and functions that work with the web

;;; Code:
(use-package browse-url
  :config
  ;; Convert anything which looks like CP-<digits> to
  ;; https://veeam-vdc.atlassian.net/browse/CP-<digits>
  (defun meain/browse-jira (url &rest _)
    (let ((jira-id (replace-regexp-in-string "^http://\\(\\S+\\)" "\\1" url)))
      (browse-url (concat "https://veeam-vdc.atlassian.net/browse/" jira-id))))
  (setq browse-url-handlers
        '(("^http://CP-[0-9]+" . meain/browse-jira) ; Control Plane
          ("^http://DP-[0-9]+" . meain/browse-jira)))) ; Data Plane

;; Open current file in Github
(use-package emacs
  :after evil-leader
  :commands (meain/github-url meain/github-pr-url)
  :config
  (defun meain/github-pr-url ()
    "Open the Github PR page for the current file and line."
    (interactive)
    (let* ((is-jj (not (string-empty-p
                        (string-trim
                         (shell-command-to-string "jj root 2>/dev/null")))))
           (project-root (if is-jj
                             (file-name-as-directory
                              (string-trim (shell-command-to-string "jj root")))
                           (locate-dominating-file default-directory ".git")))
           (relative-path (if project-root
                              (file-relative-name (buffer-file-name) project-root)
                            (buffer-file-name))))
      (message "%s"
               (shell-command-to-string
                (format ",git-pr-for-line %s %s"
                        relative-path
                        (line-number-at-pos))))))

  (defun meain/github-url--git-info (use-master)
    "Return (REMOTE-URL REF) for a git repository.
USE-MASTER returns the default branch instead of current commit."
    (unless use-master
      (when (< (length
                (shell-command-to-string
                 (concat
                  "git branch -r --contains "
                  (meain/cmd-head "git log --format='%H' -n 1"))))
               1)
        (user-error "Current commit not available upstream")))
    (let* ((raw-url (meain/cmd-head "git config --get remote.origin.url"))
           (ref (if use-master
                    (replace-regexp-in-string
                     "^origin/" ""
                     (meain/cmd-head "git symbolic-ref --short refs/remotes/origin/HEAD"))
                  (meain/cmd-head "git log --format='%H' -n 1"))))
      (list raw-url ref)))

  (defun meain/github-url--jj-info (use-master)
    "Return (REMOTE-URL REF) for a jj repository.
USE-MASTER returns the trunk bookmark instead of current commit."
    (unless use-master
      (when (string-empty-p
             (string-trim
              (shell-command-to-string
               "jj log -r '@ & ancestors(remote_bookmarks())' --no-graph -T 'commit_id'")))
        (user-error "Current commit not available upstream")))
    (let* ((raw-url (nth 1 (split-string (meain/cmd-head "jj git remote list"))))
           (ref (if use-master
                    (string-trim-right
                     (string-trim
                      (shell-command-to-string
                       "jj log -r 'trunk()' --no-graph -T 'bookmarks'"))
                     "\*")
                  (string-trim
                   (shell-command-to-string
                    "jj log -r @ --no-graph -T 'commit_id'")))))
      (list raw-url ref)))

  (defun meain/github-url (&optional use-master)
    "Link to the currently selected code in GitHub.  Pass `USE-MASTER' to use master branch.
If region is active, link covers the region."
    (interactive "P")
    (save-restriction
      (widen)
      (let* ((is-jj (not (string-empty-p
                          (string-trim
                           (shell-command-to-string "jj root 2>/dev/null")))))
             (info (if is-jj
                       (meain/github-url--jj-info use-master)
                     (meain/github-url--git-info use-master)))
             (git-url (replace-regexp-in-string
                       "^git@github.com:\\(.*\\)\\.git$" "https://github.com/\\1"
                       (car info)))
             (git-ref (cadr info))
             (file-path (file-relative-name (or buffer-file-name default-directory)
                                            (project-root (project-current))))
             (line-frag
              (unless (equal major-mode 'dired-mode)
                (if (use-region-p)
                    (let* ((start (line-number-at-pos (region-beginning)))
                           (end (line-number-at-pos (1- (region-end)))))
                      (if (= start end)
                          (format "#L%s" start)
                        (format "#L%s-L%s" start end)))
                  (format "#L%s" (line-number-at-pos)))))
             (web-url (concat git-url "/blob/" git-ref "/" file-path (or line-frag ""))))
        (message "%s copied to clipboard." web-url)
        (meain/copy-to-clipboard web-url))))
  :init
  (evil-leader/set-key "g l" 'meain/github-url))


;; Link opening
(use-package ace-link
  :ensure t
  :commands ace-link
  :init (global-set-key (kbd "M-f l") 'ace-link))

(provide 'web)
;;; web.el ends here
