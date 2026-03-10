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
    (let* ((project-root (locate-dominating-file default-directory ".git")) ; Find the project root
           (relative-path (if project-root
                              (file-relative-name (buffer-file-name) project-root) ; Get relative path
                            (buffer-file-name)))) ; Fallback to absolute path
      (message "%s"
               (shell-command-to-string
                (format ",git-pr-for-line %s %s"
                        relative-path
                        (line-number-at-pos))))))

  (defun meain/github-url (&optional use-master)
    "Link to the currently selected code in GitHub.  Pass `USE-MASTER'
to use master branch.  If region is active, link covers the region."
    (interactive "P")
    (save-restriction
      (widen)
      (let* ((is-jj (not (string-empty-p (shell-command-to-string "jj root 2>/dev/null"))))
             (project-root (if is-jj
                               (string-trim (shell-command-to-string "jj root"))
                             (car (project-roots (project-current)))))
             (git-url (replace-regexp-in-string
                       "^git@github.com:\\(.*\\)\\.git$" "https://github.com/\\1"
                       (if is-jj
                           (string-trim (shell-command-to-string "jj config get --repo git.fetch 2>/dev/null || jj git remote list | awk '/origin/ {print $2}'"))
                         (meain/cmd-head "git config --get remote.origin.url"))))
             (git-ref (cond
                       (use-master
                        (if is-jj
                            (string-trim (shell-command-to-string "jj log -r 'trunk()' --no-graph -T 'bookmarks'"))
                          (replace-regexp-in-string
                           "^origin/" ""
                           (meain/cmd-head "git symbolic-ref --short refs/remotes/origin/HEAD"))))
                       (is-jj
                        (string-trim (shell-command-to-string "jj log -r @ --no-graph -T 'commit_id'")))
                       (t
                        ;; Check if the commit is even available upstream
                        (let ((commit (meain/cmd-head "git log --format='%H' -n 1")))
                          (when (< (length
                                    (shell-command-to-string
                                     (concat "git branch -r --contains " commit)))
                                   1)
                            (user-error "Current commit not available upstream"))
                          commit))))
             (file-path (file-relative-name (or buffer-file-name default-directory)
                                            project-root))
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
