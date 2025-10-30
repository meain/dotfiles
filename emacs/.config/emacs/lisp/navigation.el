;;; navigation.el --- Navigation related packages -*- lexical-binding: t; -*-

;;; Commentary:
;; Navigate to places (not much searching).  Also include visual aids in languages.

;;; Code:
;; Matchit
(use-package evil-matchit
  :ensure t
  :defer t
  :config (global-evil-matchit-mode 1))

;; Code folding
(use-package origami
  :ensure t
  :after (evil evil-leader)
  :defer t
  :config (global-origami-mode)
  :commands (evil-toggle-fold)
  :init
  (define-key evil-normal-state-map (kbd "<SPC> TAB") 'evil-toggle-fold)
  (evil-leader/set-key "o" 'evil-toggle-fold))

;; Saveplace
(use-package saveplace
  :defer t
  :init
  (save-place-mode t)
  (setq save-place-file "~/.local/share/emacs/saveplace"))

;; Fancier tab management
(use-package tab-bar
  :after evil-leader
  :defer t
  :commands (tab-close tab-new tab-next tab-bar-rename-tab
                       meain/switch-tab-dwim meain/create-or-delete-tab
                       tab-bar-switch-to-tab)
  :config
  (setq tab-bar-history-limit 100)
  ;; (tab-bar-history-mode t)
  ;; (global-set-key (kbd "M-f <left>") 'tab-bar-history-back)
  ;; (global-set-key (kbd "M-f <right>") 'tab-bar-history-forward)

  (global-set-key (kbd "M-f ,") 'tab-bar-rename-tab)
  (evil-leader/set-key "t" 'meain/switch-tab-dwim)
  (evil-leader/set-key "T" 'meain/create-or-delete-tab)
  (evil-leader/set-key "C" (lambda () (interactive) (tab-bar-switch-to-tab "-scratch")))
  (evil-leader/set-key "s c" (lambda () (interactive) (tab-bar-switch-to-tab "-scratch")))
  (global-set-key (kbd "M-f s") 'meain/switch-tab-dwim)
  :config
  (setq tab-bar-close-button-show nil)
  (setq tab-bar-close-last-tab-choice 'tab-bar-mode-disable)
  (setq tab-bar-close-tab-select 'recent)
  (setq tab-bar-new-tab-choice t)
  (setq tab-bar-new-tab-to 'right)
  (setq tab-bar-position nil)
  (setq tab-bar-show nil)
  (setq tab-bar-tab-hints nil)
  (setq tab-bar-tab-name-function 'tab-bar-tab-name-all)
  (tab-bar-mode -1)
  (tab-bar-history-mode -1)
  (defun meain/create-or-delete-tab (&optional close)
    "Create or close tab"
    (interactive "P")
    (let ((tabs (cl-remove-if (lambda (x)
                                (equal x "-scratch"))
                              (mapcar (lambda (tab)
                                        (alist-get 'name tab))
                                      (tab-bar--tabs-recent)))))
      (if close
          (if (eq tabs nil)
              (message "Not closing last tab")
            (tab-close))
        (tab-new))))
  (defun meain/switch-tab-dwim (&optional show-hidden)
      "Switch between available tabs.
If only two tabs, we switch to the other tab, and if we have more than
two tabs, prompt for which tab to switch to.

You can mark a tab as hidden by prefixing it with a -. These won't be
shown unless the `SHOW-HIDDEN' arg is provided."
    (interactive "P")
    (let ((tabs (cl-remove-if (lambda (x)
                                (and (not show-hidden)
                                     (string-prefix-p "-" x)))
                              (mapcar (lambda (tab)
                                        (alist-get 'name tab))
                                      (tab-bar--tabs-recent)))))
      (if (> (length tabs) 1)
          (tab-bar-switch-to-tab (completing-read "Select tab: " tabs nil t nil 'meain/tab-switch-history))
        (cond
         ((eq tabs nil)
          (message (concat "Only one tab present. Use `"
                           (substitute-command-keys "\\[meain/create-or-delete-tab]")
                           "` to create another tab.")))
         (t (tab-bar-switch-to-tab (car tabs))))))))

(use-package indent-guide
  :ensure t
  :after (evil-leader)
  :commands (indent-guide-global-mode indent-guide-mode)
  :init
  (setq indent-guide-delay nil)
  (setq indent-guide-char "¦") ; Other chars │
  (setq indent-guide-recursive t)
  (evil-leader/set-key "b I" 'indent-guide-global-mode)
  :config
  (set-face-attribute 'indent-guide-face nil :foreground "#DDD"))

;; Neotree
(use-package neotree
  :ensure t
  :commands neotree
  :config
  (setq neo-window-fixed-size nil)
  (setq neo-theme 'classic)
  (add-hook 'neo-after-create-hook 'hl-line-mode))

(provide 'navigation)
;;; navigation.el ends here
