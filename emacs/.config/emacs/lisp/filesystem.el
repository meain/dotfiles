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
                               (setq imenu-generic-expression '((nil "\s+.+$" 0)))))

  (defun dired-dim-git-ignores ()
    "Dim out git-ignored files in dired buffer."
    (when-let* ((git-root (let ((jj-root (string-trim (shell-command-to-string "jj git root 2>/dev/null"))))
                            (if (and jj-root (not (string-empty-p jj-root)))
                                jj-root
                              (when-let ((repo-root (locate-dominating-file default-directory ".git")))
                                (expand-file-name ".git" repo-root)))))
                ((file-directory-p git-root))
                (work-tree (expand-file-name default-directory)))
      (let* ((output (shell-command-to-string
                      (concat "git --git-dir=" (shell-quote-argument git-root)
                              " --work-tree=" (shell-quote-argument work-tree)
                              " ls-files --ignored --exclude-standard --others --directory")))
             (ignores (split-string output "\n" t)))
        (save-excursion
          (goto-char (point-min))
          (while (not (eobp))
            (when-let ((file (dired-get-filename 'relative 'noerror)))
              (when (or (member file ignores)
                        (member (concat file "/") ignores)
                        ;; Handle directory entries
                        (cl-some (lambda (ignore)
                                   (string-prefix-p (file-name-as-directory file) ignore))
                                 ignores))
                (let ((ov (make-overlay (line-beginning-position)
                                        (1+ (line-end-position)))))
                  (overlay-put ov 'face 'shadow)
                  (overlay-put ov 'dired-git-ignored t))))
            (forward-line 1))))))
  (defun dired-clear-git-ignore-overlays ()
    "Remove git-ignore dimming overlays."
    (remove-overlays (point-min) (point-max) 'dired-git-ignored t))
  (add-hook 'dired-mode-hook
            (lambda ()
              (add-hook 'dired-after-readin-hook #'dired-dim-git-ignores nil t)))
  (add-hook 'dired-mode-hook
            (lambda ()
              (add-hook 'before-revert-hook #'dired-clear-git-ignore-overlays nil t))))

(use-package dired-x
  :after dired
  :defer t
  :config
  (let ((omit-patterns
         '("\\.DS_Store$"
           "Thumbs\\.db$"
           "__pycache__$"
           "\\.pytest_cache$"
           "\\.mypy_cache$"
           "\\.egg-info$"
           "\\.tox$"
           "\\.venv$"
           "\\.coverage$"
           "\\.cache$"
           "\\.sass-cache$"
           "\\.next$"
           "\\.nuxt$"
           "\\.git$"
           "\\.jj$"
           "\\.svn$"
           "\\.elc$"
           "\\.o$"
           "\\.so$")))
    (setq dired-omit-files
          (mapconcat #'identity omit-patterns "\\|"))))

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
(use-package recentf :defer t :init (recentf-mode t))

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
