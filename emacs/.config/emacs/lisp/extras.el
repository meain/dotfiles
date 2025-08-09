;;; extras.el --- Packages that dont fit in other places -*- lexical-binding: t; -*-

;;; Commentary:
;; Packages that dont fit in other places.

;;; Code:
;; Read RSS
(use-package gnus
  :commands gnus
  :config
  (evil-define-key 'normal gnus-article-mode-map (kbd "M-n") 'gnus-summary-next-article) ;; <space> is always available
  (evil-define-key 'normal gnus-summary-mode-map (kbd "M-n") 'gnus-summary-next-article)
  (evil-define-key 'normal gnus-article-mode-map (kbd "p") 'gnus-summary-refer-parent-article)
  (evil-define-key 'normal gnus-summary-mode-map (kbd "p") 'gnus-summary-refer-parent-article)
  (evil-define-key 'normal gnus-summary-mode-map (kbd "P") 'gnus-summary-refer-thread) ; fetch all the messages in thread (useful for bookmarked)
  (evil-define-key 'normal gnus-summary-mode-map (kbd "a") 'gnus-summary-kill-thread)
  (evil-define-key 'normal gnus-group-mode-map (kbd "a") 'gnus-group-catchup-current-all)
  (setq gnus-select-method '(nnnil ""))
  (setq gnus-directory "~/.config/emacs/news")
  (setq gnus-secondary-select-methods '((nntp "news.gmane.io")))
  (add-hook 'gnus-group-mode-hook 'hl-line-mode)
  (add-hook 'gnus-summary-mode-hook 'hl-line-mode))

;; erc
(use-package erc
  :commands (erc)
  :config
  (setq erc-timestamp-format "[%I:%M %p]"))

;; command log
(use-package command-log-mode
  :commands global-command-log-mode
  :ensure t
  :init
  (defun meain/command-log-start ()
    "Enable command-log-mode and open command-log buffer."
    (interactive)
    (global-command-log-mode)
    (clm/open-command-log-buffer)))

;; Keycast mode for demos
(use-package keycast
  :ensure t
  :defer t
  :commands (keycast-mode keycast-background-mode keycast-log-mode keycast-tab-bar-mode)
  :config
  (add-hook 'keycast-mode-hook (lambda () (setq header-line-format nil)))
  (defvar keycast-background-mode)
  ;; TODO: fix not clearing the last thing on exit
  (define-minor-mode keycast-background-mode
    "Activate keycast mode in the background.  Enables variable to be used in header-line."
    :global t
    (cond
     (keycast-background-mode
      (add-hook 'post-command-hook #'keycast--update t)
      (add-hook 'minibuffer-exit-hook #'keycast--minibuffer-exit t))
     ((not (keycast--mode-active-p))
      (remove-hook 'post-command-hook #'keycast--update)
      (remove-hook 'minibuffer-exit-hook #'keycast--minibuffer-exit)))))

;; RFC reader
(use-package rfc-mode
  :ensure t
  :commands (rfc-mode-browse rfc-mode-read)
  :config
  (setq rfc-mode-directory (expand-file-name "~/.local/share/rfc/"))
  (add-hook 'rfc-mode-hook 'writeroom-mode))

(use-package redacted
  :ensure t
  :commands (redacted-mode)
  :config (add-hook 'redacted-mode-hook (lambda () (read-only-mode (if redacted-mode 1 -1)))))

(provide 'extras)
;;; extras.el ends here
