;;; filesystem.el --- Deal with filesystem -*- lexical-binding: t; -*-

;;; Commentary:
;; All packages that lets you view/modify filesystems

;;; Code:
(use-package emacs
  :commands (meain/delete-current-file meain/rename-current-file meain/move-current-file)
  :config
  ;; Delete current file
  (defun meain/delete-current-file ()
    "Delete current file and close buffer."
    (interactive)
    (delete-file (buffer-file-name))
    (meain/kill-current-buffer-unless-scratch))

  ;; Quick file rename
  (defun meain/rename-current-file ()
    "Rename current file in the same directory."
    (interactive)
    (let ((newname (read-string "New name: " (file-name-nondirectory (buffer-file-name)))))
      (rename-file (buffer-file-name) (concat (file-name-directory (buffer-file-name)) newname))
      (find-alternate-file (concat (file-name-directory (buffer-file-name)) newname))))

  (defun meain/move-current-file ()
    "Rename the current visiting file and switch buffer focus to it."
    (interactive)
    (if (null (buffer-file-name))
        (user-error "Buffer does not have a filename: %s" (current-buffer)))
    (let* ((prompt (format "Rename %s to: "
                           (file-name-nondirectory (buffer-file-name))))
           (new-filename (expand-file-name (read-file-name prompt))))
      (if (null (file-writable-p new-filename))
          (user-error "New file not writable: %s" new-filename))
      (rename-file (buffer-file-name) new-filename 1)
      (find-alternate-file new-filename)
      (message "Renamed to and now visiting: %s" (abbreviate-file-name new-filename)))))

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
                               (setq imenu-generic-expression '((nil "\s+.+$" 0))))))

  ;; ;; TODO: make it work with directories
  ;; (defun dired-dim-git-ignores ()
  ;;   "Dim out .gitignore contents"
  ;;   (when-let (((require 'vc))
  ;;              ((require 'magit))
  ;;              (ignores (magit-ignored-files))
  ;;              (exts (make-local-variable 'completion-ignored-extensions)))
  ;;     (dolist (item ignores) (add-to-list exts item))))
  ;; (add-hook 'dired-mode-hook #'dired-dim-git-ignores)

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

;; Enable recentf
(use-package recentf
  :defer t
  :init
  (recentf-mode t))

;; Save buffer
(use-package emacs
  :after evil
  :init
  (define-key evil-normal-state-map
              (kbd "<SPC> <SPC>")
              (lambda ()
                (interactive)
                (cond
                 ((equal (buffer-name) "*scratch*")
                  (let ((filename (concat
                                   "/tmp/emacs-scratch-"
                                   (format-time-string "%Y-%m-%d-%H-%M-%S")
                                   ".el")))
                    (with-temp-file filename
                      (message "Saving to %s" filename)
                      (insert (with-current-buffer "*scratch*" (buffer-string))))))
                 ((bound-and-true-p gptel-mode)
                  ;; See if the buffer already has a filename, if so
                  ;; use else, else call the gptel save buffer
                  ;; function.
                  (if (buffer-file-name)
                      (save-buffer)
                    (call-interactively 'meain/gptel-rename-chat-buffer)))
                 (t (call-interactively 'evil-write))))))

(provide 'filesystem)
;;; filesystem.el ends here
