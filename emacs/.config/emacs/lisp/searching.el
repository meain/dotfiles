;;; searching.el --- Searching and jumping to stuff -*- lexical-binding: t; -*-

;;; Commentary:
;; All packages related to searching for text/code and jumping to them.

;;; Code:
;; rg.el
(use-package rg
  :ensure t
  :commands rg
  :after evil-leader
  :init
  (evil-leader/set-key "f"
    (alambda (consult-ripgrep) (call-interactively 'rg)))
  :config (setq rg-command-line-flags '("--hidden" "--follow")))

;; dumber-jump
(use-package dumber-jump
  :ensure t
  :defer t
  :after evil-leader
  :config
  (add-hook 'xref-backend-functions #'dumber-jump-xref-activate))

;; Xref customization
(use-package xref
  :after (evil)
  :defer t
  :config
  (define-key evil-normal-state-map (kbd "M-.") #'xref-find-definitions)
  (define-key evil-normal-state-map (kbd "M-?") #'xref-find-references)
  (define-key evil-normal-state-map (kbd "g d") 'xref-find-definitions)
  (define-key evil-normal-state-map (kbd "g D") 'xref-find-implementations)
  (define-key evil-normal-state-map (kbd "g r") 'xref-find-references)
  ;; (setq xref-show-definitions-function 'xref-show-definitions-completing-read)
  (setq xref-auto-jump-to-first-xref 'move) ;; Use 'show to open it
  (setq xref-auto-jump-to-first-definition 'move))

;; Tagbar alternative
(use-package imenu
  :defer t
  :after (consult)
  :commands imenu
  :config
  (setq imenu-auto-rescan t)
  (setq imenu-max-item-length 300)
  (advice-add 'consult-imenu
              :before
              (lambda ()
                ;; We want the previous .rest buffer if http response buffer
                (if (equal (buffer-name) "*HTTP Response*")
                    (previous-window-any-frame))))
  :init
  (global-set-key (kbd "M-i") 'consult-imenu))
(use-package flimenu
  :ensure t
  :defer t
  :after imenu
  :config (flimenu-global-mode 1))
(use-package imenu-list
  :ensure t
  :defer t
  :after (imenu consult)
  :commands imenu-list-smart-toggle
  :config
  (setq imenu-list-focus-after-activation t)
  (setq imenu-list-after-jump-hook nil)
  (setq imenu-list-auto-resize t))

(provide 'searching)
;;; searching.el ends here
