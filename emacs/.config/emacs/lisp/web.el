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
  ;; Convert anything which looks like #<digits> to a GitHub issue/PR URL
  ;; by looking up the git remote for the current buffer's repo
  (defun meain/browse-github-issue (url &rest _)
    (let* ((issue-number (replace-regexp-in-string "^http://#\\([0-9]+\\)" "\\1" url))
           (default-directory (or (when buffer-file-name (vc-root-dir))
                                  default-directory))
           (remote-url (string-trim
                        (shell-command-to-string "git remote get-url origin")))
           (repo-path (replace-regexp-in-string
                       "\\(?:git@github\\.com:\\|https://github\\.com/\\)\\(.*?\\)\\(?:\\.git\\)?$"
                       "\\1"
                       remote-url)))
      (browse-url (concat "https://github.com/" repo-path "/issues/" issue-number))))
  (setq browse-url-handlers
        '(("^http://CP-[0-9]+" . meain/browse-jira) ; Control Plane
          ("^http://DP-[0-9]+" . meain/browse-jira)  ; Data Plane
          ("^http://#[0-9]+"   . meain/browse-github-issue)))) ; GitHub issue/PR

;; Embark target finder so `embark-act' on CP-1234/DP-1234/#1234 at
;; point offers the usual url actions (browse-url, embark-download-url,
;; etc.), which then dispatch through the browse-url-handlers above.
(defun meain/embark-target-jira-github-at-point ()
  "Target a CP-/DP- Jira ticket or #NNN GitHub issue at point as a url."
  (when (thing-at-point-looking-at "\\(?:CP\\|DP\\)-[0-9]+\\|#[0-9]+" 20)
    (let ((match (match-string 0))
          (bounds (cons (match-beginning 0) (match-end 0))))
      `(url ,(concat "http://" match) . ,bounds))))

(with-eval-after-load 'embark
  (add-to-list 'embark-target-finders #'meain/embark-target-jira-github-at-point))

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
  (let* ((raw-url (meain/cmd-head "git config --get remote.origin.url"))
         (ref (if (or use-master
                      (< (length
                          (shell-command-to-string
                           (concat
                            "git branch -r --contains "
                            (meain/cmd-head "git log --format='%H' -n 1"))))
                         1))
                  (replace-regexp-in-string
                   "^origin/" ""
                   (meain/cmd-head "git symbolic-ref --short refs/remotes/origin/HEAD"))
                (meain/cmd-head "git log --format='%H' -n 1"))))
    (list raw-url ref)))

(defun meain/github-url--jj-info (use-master)
  "Return (REMOTE-URL REF) for a jj repository.
USE-MASTER returns the trunk bookmark instead of current commit."
  (let* ((raw-url (nth 1 (split-string (meain/cmd-head "jj git remote list"))))
         (ref (if (or use-master
                      (string-empty-p
                       (string-trim
                        (shell-command-to-string
                         "jj log -r '@ & ancestors(remote_bookmarks())' --no-graph -T 'commit_id'"))))
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
If a commit is not available upstream. `USE-MASTER' is set to t
automatically. If region is active, link covers the region."
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

;; Alternative: https://github.com/federicotdn/verb
(use-package restclient
  :ensure t
  :defer t
  :mode ("\\.rest\\'". restclient-mode)
  :config (add-hook 'restclient-mode-hook (lambda ()
                                            (setq imenu-generic-expression '((nil "^#+\s+.+" 0))))))
(use-package restclient-jq
  :ensure t
  :after restclient
  :defer
  :init
  (add-hook 'restclient-mode-hook (lambda () (require 'restclient-jq))))

;; Link opening
(use-package ace-link
  :ensure t
  :commands ace-link
  :init (global-set-key (kbd "M-f l") 'ace-link))

(provide 'web)
;;; web.el ends here
