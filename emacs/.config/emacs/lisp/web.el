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

  (defun meain/github-url (&optional use-branch)
    "Open the Github page for the current file.  Pass USE-BRANCH to use branch name instead of commit hash."
    (interactive "P")
    (save-restriction
      (widen)
      (let* ((git-url (replace-regexp-in-string
                       "\.git$"
                       ""
                       (s-replace "git@github\.com:"
                                  "https://github.com/"
                                  (car (split-string
                                        (shell-command-to-string
                                         "git config --get remote.origin.url") "\n")))))
             (git-branch (car (split-string
                               (shell-command-to-string
                                (if use-branch
                                    "git rev-parse --abbrev-ref HEAD"
                                  "git log --format='%H' -n 1"
                                  ))
                               "\n")))
             (web-url (format "%s/blob/%s/%s%s"
                              git-url
                              git-branch
                              (file-relative-name (if (equal major-mode 'dired-mode)
                                                      default-directory
                                                    buffer-file-name)
                                                  (car (project-roots (project-current))))
                              (if (equal major-mode 'dired-mode)
                                  ""
                                (format "#L%s" (line-number-at-pos))))))
        (progn
          (message "%s coped to clipboard." web-url)
          (meain/copy-to-clipboard web-url)))))
  :init
  (evil-leader/set-key "g l" 'meain/github-url))

(provide 'web)
;;; web.el ends here
