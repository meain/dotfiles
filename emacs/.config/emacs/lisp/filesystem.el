;;; filesystem.el --- Deal with filesystem -*- lexical-binding: t; -*-

;;; Commentary:
;; All packages that lets you view/modify filesystems

;;; Code:
;; dired
(use-package dired
  :defer 1
  :after evil
  :config
  (require 'dired-x) ;; for dired-omit-files
  (setq delete-by-moving-to-trash t)
  (setq trash-directory "~/.Trash")
  (setq dired-listing-switches "-AGFhlgo")
  (setq dired-dwim-target t)
  ;; (setq dired-kill-when-opening-new-dired-buffer t)
  (define-key dired-mode-map (kbd "-") 'dired-up-directory)
  (add-hook 'dired-mode-hook 'dired-omit-mode)
  (add-hook 'dired-mode-hook 'hl-line-mode)
  (add-hook 'dired-mode-hook 'dired-hide-details-mode)
  (define-key evil-normal-state-map (kbd "-") 'dired-jump)
  (define-key evil-normal-state-map (kbd "_") 'find-file)

  (add-hook 'dired-mode-hook (lambda ()
                               (setq imenu-generic-expression '((nil "\s+.+$" 0)))))

  ;; TODO: make it work with directories
  (defun dired-dim-git-ignores ()
    "Dim out .gitignore contents"
    (when-let (((require 'vc))
               (ignores (magit-ignored-files))
               (exts (make-local-variable 'completion-ignored-extensions)))
      (dolist (item ignores) (add-to-list exts item))))
  (add-hook 'dired-mode-hook #'dired-dim-git-ignores))

(use-package dired-x
  :after dired
  :defer t
  :config
  (setq dired-omit-files "\\.DS_Store$\\|__pycache__$\\|.pytest_cache$\\|\\.mypy_cache$\\|\\.egg-info$"))

;; tramp dired
(use-package tramp
  :commands (meain/tramp-open)
  :config
  (put 'temporary-file-directory 'standard-value (list temporary-file-directory))
  (setq remote-file-name-inhibit-cache nil)
  (setq tramp-default-method "ssh")
  (setq vc-ignore-dir-regexp (format "%s\\|%s" vc-ignore-dir-regexp tramp-file-name-regexp))
  (setq tramp-verbose 3)
  (defun meain/tramp-open ()
    "Open dired in a server by selecting a host via autocomplete."
    (interactive)
    (dired (concat "/ssh:" (meain/ssh-host-picker) ":"))))

(provide 'filesystem)
;;; filesystem.el ends here
