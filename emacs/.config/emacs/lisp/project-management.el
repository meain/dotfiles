;;; project-management.el --- Manage projects within emacs -*- lexical-binding: t; -*-

;;; Commentary:
;; All things related to managing multiple projects

;;; Code:
(use-package project
  :defer t
  :after (evil evil-leader)
  :commands (project-switch-project project-find-file project-roots project-current)
  :config
  (setq project-vc-ignores '("target/" "node_modules/")) ; default ignores
  (setq project-switch-commands 'project-find-file) ; start `project-find-file' by default

  ;; Find root for go projects without vcs
  (defun meain/project-try-explicit (dir)
    "Find root based on go.mod for `dir'."
    (locate-dominating-file dir "go.mod"))
  (cl-defmethod project-root ((project string))
    project)
  (add-hook 'project-find-functions
            #'meain/project-try-explicit 100)

  (defun meain/project-name ()
    (if (project-current)
        (file-name-nondirectory (directory-file-name
                                 (project-root (project-current))))))

  (defun meain/find-file-git-changed ()
    "Fuzzy find git changed files."
    (interactive)
    (let* ((untracked-files (shell-command-to-string "git ls-files --others --exclude-standard"))
           (changed-files (shell-command-to-string "git diff --name-only"))
           (files (split-string (concat untracked-files "\n" changed-files) "\n" t)))
      (find-file (completing-read "Pick file: " files))))

  (defun meain/find-file-semantic ()
    (interactive)
    (let* ((user-query (read-string "Search for: "))
           (default-directory (project-root (project-current)))
           (refer-output (shell-command-to-string (concat "refer search '"user-query "'")))
           (files (seq-map (lambda (x) (cadr (split-string x " " t)))
                           (split-string refer-output "\n" t))))
      (find-file (completing-read "Pick file: " files))))

  (defun meain/refresh-semantic-search-index ()
    (interactive)
    (let ((default-directory (project-root (project-current))))
      (async-shell-command "refer add . && refer reindex" "*semantic-index-refresh*")))

  :init
  (evil-leader/set-key "p p"
    (alambda (call-interactively 'project-switch-project)
             (project-find-file)))

  (define-key evil-normal-state-map (kbd "<SPC> <RET>") 'meain/find-file-git-changed)
  (define-key evil-normal-state-map (kbd "<M-RET>") (alambda
                                                     (meain/find-file-semantic)
                                                     (meain/refresh-semantic-search-index)))
  (define-key evil-normal-state-map (kbd "<RET>") 'project-find-file))

(use-package ibuffer-project
  :ensure t
  :after (ibuffer project)
  :config
  (add-to-list 'ibuffer-project-root-functions '(file-remote-p . "Remote"))
  (add-hook 'ibuffer-hook
            (lambda ()
              (setq ibuffer-filter-groups (ibuffer-project-generate-filter-groups))
              (unless (eq ibuffer-sorting-mode 'project-file-relative)
                (ibuffer-do-sort-by-project-file-relative))))
  (setq ibuffer-formats '((mark modified read-only " "
                                (name 18 18 :left :elide) " " (size 9 -1 :right) " "
                                (mode 16 16 :left :elide) " " project-file-relative)))
  :init (add-hook 'ibuffer-hook
                  (lambda ()
                    (setq ibuffer-filter-groups (ibuffer-project-generate-filter-groups)))))

;; Direnv support
(use-package envrc
  :ensure t
  :config (envrc-global-mode))

(provide 'project-management)
;;; project-management.el ends here
