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
    "Link to the currently selected code in GitHub.  Pass `USE-MASTER' to use master branch.
If region is active, link covers the region."
    (interactive "P")
    (save-restriction
      (widen)
      ;; Check if the commit is even available upstream and
      ;; only generate a link if it is.
      (unless use-master
        (when (< (length
                  (shell-command-to-string
                   (concat
                    "git branch -r --contains "
                    (meain/cmd-head "git log --format='%H' -n 1"))))
                 1)
          (user-error "Current latest commit not available upstream")))

      (let* ((git-url (replace-regexp-in-string
                       "^git@github.com:\\(.*\\)\\.git$" "https://github.com/\\1"
                       (meain/cmd-head "git config --get remote.origin.url")))
             (git-ref (if use-master
                          (replace-regexp-in-string
                           "^origin/" ""
                           (meain/cmd-head "git symbolic-ref --short refs/remotes/origin/HEAD"))
                        (meain/cmd-head "git log --format='%H' -n 1")))
             (file-path (file-relative-name (or buffer-file-name default-directory)
                                            (car (project-roots (project-current)))))
             (line-frag
              (unless (equal major-mode 'dired-mode)
                (if (use-region-p)
                    (let* ((start (line-number-at-pos (region-beginning)))
                           (end (1- (line-number-at-pos (region-end)))))
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
