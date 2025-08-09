;;; evil-m.el --- Evil core related packages -*- lexical-binding: t; -*-

;;; Commentary:
;; Primary even configuratino

;;; Code:
;; Evil mode (set this up first)
(use-package evil
  :ensure t

  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-minibuffer nil) ; messes with esc to quit
  (setq evil-undo-system 'undo-tree)
  (setq evil-kill-on-visual-paste nil)
  (setq evil-respect-visual-line-mode nil)
  (setq evil-symbol-word-search t)

  :config
  (evil-mode t)
  (defalias #'forward-evil-word #'forward-evil-symbol)

  ;; Enable/disable certain jump targets for C-o and C-i
  (evil-set-command-property 'evil-visual-char :jump t)
  (evil-set-command-property 'evil-visual-line :jump t)
  (evil-set-command-property 'evil-backward-paragraph :jump nil)
  (evil-set-command-property 'evil-forward-paragraph :jump nil)
  (evil-set-command-property 'evil-search-next :jump nil)
  (evil-set-command-property 'evil-search-previous :jump nil)

  ;; Up/Down on visual instead of actual lines
  (define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)
  (define-key evil-normal-state-map (kbd "Y") 'meain/yank-till-line-end)

  ;; Buffer navigation
  (define-key evil-normal-state-map (kbd "C-S-o") 'previous-buffer)
  (define-key evil-normal-state-map (kbd "C-S-i") 'next-buffer)

  (define-key evil-normal-state-map "Q" 'evil-record-macro)
  (define-key evil-visual-state-map
              (kbd ";")
              (lambda ()
                (interactive)
                (call-interactively 'eval-region)
                (evil-force-normal-state)))

  ;; (advice-add 'evil-jump-forward :around #'meain/recenter-advice)
  ;; (advice-add 'evil-jump-backward :around #'meain/recenter-advice)
  ;; (advice-add 'evil-search-next :around #'meain/recenter-top-advice)
  ;; (advice-add 'evil-search-previous :around #'meain/recenter-top-advice)

  (defun meain/yank-till-line-end ()
    "Yank till end of line."
    (interactive)
    (evil-yank (point) ;; subtracting 1 for newline
               (- (save-excursion (forward-line) (point)) 1))))

;; Evil leader
(use-package evil-leader
  :ensure t
  :after evil
  :config
  (global-evil-leader-mode)
  (evil-leader/set-leader "s")

  ;; server edit
  (evil-leader/set-key "s s" 'server-edit)

  ;; buffer commands
  (evil-leader/set-key "b k" 'kill-buffer)
  (evil-leader/set-key "b o" 'previous-buffer)
  (evil-leader/set-key "b f" 'find-file)
  (evil-leader/set-key "b d" 'delete-frame)

  ;; Line folding
  (evil-leader/set-key "b w" 'toggle-truncate-lines)

  ;; Auto fill mode enable
  (evil-leader/set-key "b F" 'auto-fill-mode))

;; Evil commentary
(use-package evil-commentary
  :ensure t
  :defer nil
  :config (evil-commentary-mode))

;; Evil surround
(use-package evil-surround
  :ensure t
  :defer nil
  :config (global-evil-surround-mode 1))

;; Evil text objects
(use-package evil-textobj-line :ensure t)
(use-package evil-textobj-syntax :ensure t)
(use-package evil-indent-plus
  :ensure t
  :defer nil
  :config
  (define-key evil-inner-text-objects-map "i" 'evil-indent-plus-i-indent)
  (define-key evil-outer-text-objects-map "i" 'evil-indent-plus-a-indent)
  (define-key evil-inner-text-objects-map "I" 'evil-indent-plus-i-indent-up-down)
  (define-key evil-outer-text-objects-map "I" 'evil-indent-plus-a-indent-up-down))

;; Evil number increment
(use-package evil-numbers
  :ensure t
  :defer t
  :after (evil)
  :commands (evil-numbers/inc-at-pt-incremental evil-numbers/dec-at-pt-incremental)
  :init
  ;; cannot directly use C-x (in use by emacs)
  (define-key evil-normal-state-map (kbd "g C-a") 'evil-numbers/inc-at-pt-incremental)
  (define-key evil-normal-state-map (kbd "g C-x") 'evil-numbers/dec-at-pt-incremental))

;; Evil keybindings for a lot of things
(use-package evil-collection
  :defer nil
  :ensure t
  :after evil
  :config
  (setq evil-collection-magit-want-horizontal-movement t)
  (setq evil-collection-magit-use-y-for-yank t)
  (evil-collection-init))

;; Quit out of everything with esc
(defun meain/keyboard-quit ()
  "Quit out of whatever."
  (interactive)
  ;; Delete frame if it is a minbuffer only popup
  (if (and (equal (cdr (assq 'name (frame-parameters))) "emacs-popup")
           (equal (cdr (assq 'minibuffer (frame-parameters))) 'only))
      (delete-frame))
  (keyboard-escape-quit)
  (minibuffer-keyboard-quit)
  (keyboard-quit))
(global-set-key [escape] 'meain/keyboard-quit)

;; Highlight yanked region
(defun meain/evil-yank-advice (orig-fn beg end &rest args)
  "Advice to be added to `evil-yank' to highlight yanked region.
Pass ORIG-FN, BEG, END, TYPE, ARGS."
  (pulse-momentary-highlight-region beg end 'mode-line)
  (apply orig-fn beg end args))
(advice-add 'evil-yank :around 'meain/evil-yank-advice)

(provide 'evil-m)
;;; evil-m.el ends here
