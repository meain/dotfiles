;;; settings.el --- Basic settings -*- lexical-binding: t; -*-

;;; Commentary:
;; Basic settings for Emacs, mostly related to system packages

;;; Code:
(setq user-mail-address "mail@meain.io" user-full-name "Abin Simon")

;; Consistent window title
(setq frame-title-format '("Emacs")) ; needed by hammerspoon

;; Don't automatically add newlines at end of files
(setq mode-require-final-newline nil)

;; Fix some cmd keybinds
(global-set-key (kbd "s-v") 'yank)
(global-set-key (kbd "s-a") 'mark-whole-buffer)
(global-set-key (kbd "s-c") 'kill-ring-save)

;; Quicker yes or no
(fset 'yes-or-no-p 'y-or-n-p)

;; UTF-8 please
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; vim like scroll behaviour
(setq scroll-conservatively 100)
(setq scroll-step 1)
(setq scroll-margin 3)

;; Backup and autosave somewhere else
(setq backup-directory-alist `((".*" . "~/.local/share/emacs/backup")))
(setq auto-save-file-name-transforms `((".*" "~/.local/share/emacs/autosave" t)))

;; Don't create lockfiles
(setq create-lockfiles nil)

;; Don't clutter my emacs conf
;; Will have to call load-file in case I actually need this on next startup
;; Just keeping it as a file so that I can copy paste
(setq custom-file "~/.config/emacs/custom-config.el")
(load-file "~/.config/emacs/custom-config.el") ;; TODO: only load `safe-local-variable-values'

;; Tab settings
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

;; Delete visual selection when I start typing
(delete-selection-mode 1)

;; Meta-f keybinds (tmux memories)
(global-unset-key (kbd "M-f")) ; have to unset first

;; Disable visual line mode (this causes issues with $ and a few other things in evil)
(global-visual-line-mode -1)

;; Let Emacs know that I can handle `narrow'
(put 'narrow-to-region 'disabled nil)

;; Highlight trailing whitespace
(setq-default show-trailing-whitespace t) 
(set-face-background 'trailing-whitespace "#FFDDDD") ; give it a non jarring color

;; move the mouse out of the way on cursor
(mouse-avoidance-mode 'cat-and-mouse)

;; Cursor blink
(blink-cursor-mode -1)

;; Follow symlinks for vc
(setq vc-follow-symlinks t)

;; Do not wrap lines by default
(setq-default truncate-lines 1)

;; Use mouse to do some stuff when you are lazy
;; TODO: Causes Emacs to freeze when open
(context-menu-mode nil)

;; Automatic chmod +x when you save a file that starts with a #! shebang:
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; Font size changes
(global-set-key (kbd "s-=") 'text-scale-increase)
(global-set-key (kbd "s--") 'text-scale-decrease)
(global-set-key (kbd "s-_") (ilambda text-scale-set 0)) ; s-0 is used by wm

;; Revert buffer quickly (fix for dealing with eglot loosing it)
(global-set-key (kbd "M-f r") (lambda ()
                                (interactive)
                                (save-buffer)
                                (revert-buffer-quick)))

(provide 'settings)
;;; settings.el ends here
