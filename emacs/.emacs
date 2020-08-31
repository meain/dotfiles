;;; package -- meain's emacs config

;;; Commentary:
;;; Well, this is a vimmer's Emacs config.  Nothing fancy though.

;; TODO:
;; - yank highlight
;; - figure out using terminal
;; - flyspell auto correct last word

;;; Code:

;;; [Basic config] =============================================

;; Disable ugly stuff
(when window-system
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (menu-bar-mode -1))
(setq inhibit-startup-screen t)

;; Font
(set-frame-font "Iosevka 15" nil t)

;; Quicker yes or no
(fset 'yes-or-no-p 'y-or-n-p)

;; Disable line wrapping
(setq-default truncate-lines 1)

;; Cursor blink
(blink-cursor-mode -1)

;; Quit out of everythign with esc
(defun meain/keyboard-quit ()
  "Quit out of whatever."
  (interactive)
  (minibuffer-keyboard-quit)
  (keyboard-quit))
(global-set-key [escape]
		'meain/keyboard-quit)

;;; [Package setup] ============================================

;; Set up package & melpa
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/")
	     t)
(package-initialize)

;; Set up use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))

;; Theme
(use-package modus-operandi-theme
  :ensure t
  :init (load-theme 'modus-operandi t))

;; Persistant undo
(use-package undo-tree
  :ensure t
  :init (progn
	  (global-undo-tree-mode)
	  (setq undo-tree-auto-save-history t)
	  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))))

;;; [evil packages] =================================================

;; Evil mode
(use-package evil
  :ensure t
  :init (evil-mode t))

;; Evil leader
(use-package evil-leader
  :ensure t
  :init (progn
	  (global-evil-leader-mode)
	  (evil-leader/set-leader "s")))

;; Evil commentary
(use-package evil-commentary
  :ensure t
  :init (evil-commentary-mode))

;; Evil surround
(use-package evil-surround
  :ensure t
  :init (global-evil-surround-mode 1))

;; Evil keybindings
;; (define-key evil-normal-state-map (kbd "q") 'bury-buffer)

;; Company for autocompletions
(use-package company
  :ensure t
  :init (company-mode))

;; Ivy && Counsel
(use-package counsel :ensure t)
(use-package flx :ensure t)
(use-package ivy
  :ensure t
  :config (progn
	    (ivy-mode 1)
	    (setq ivy-use-virtual-buffers t) ; extend searching to bookmarks and
	    (setq ivy-height 10) ; Set height of the ivy window
	    (setq ivy-count-format "(%d/%d) ") ; count format, from the ivy help page
	    (setq ivy-display-style 'fancy)
	    (setq ivy-format-functions-alist '((t . ivy-format-function-line))) ; Make highlight extend all the way to the right
	    (setq ivy-re-builders-alist '((ivy-switch-buffer . ivy--regex-plus)
					  (t . ivy--regex-fuzzy)))
	    (evil-leader/set-key "f" 'counsel-rg)
	    (evil-leader/set-key "b" 'ivy-switch-buffer)
	    (evil-leader/set-key "s" 'counsel-git)
	    (evil-leader/set-key "r" 'counsel-recentf)
	    (define-key evil-normal-state-map (kbd "C-d f") 'counsel-describe-function)
	    (define-key evil-normal-state-map (kbd "C-d v") 'counsel-describe-variable)
	    (define-key evil-normal-state-map (kbd "C-d o") 'counsel-describe-symbol)
	    (define-key evil-normal-state-map (kbd "C-d l") 'counsel-find-library)
	    (define-key evil-normal-state-map (kbd "C-d i") 'counsel-info-lookup-symbol)
	    (evil-leader/set-key "l" 'counsel-M-x)
	    (define-key evil-normal-state-map (kbd "M-f") 'counsel-M-x)))

;; ivy-rich
(use-package ivy-rich
  :ensure t
  :init (ivy-rich-mode))

;; dumb-jump
(use-package dumb-jump
  :ensure t
  :config (progn
	    (evil-leader/set-key "J" 'dumb-jump-go)))


;; Window mappings
(global-set-key (kbd "M-l")
		(defun meain/window-right ()
		  (interactive)
		  (evil-force-normal-state)
		  (evil-window-right 1)))
(global-set-key (kbd "M-h")
		(defun meain/window-left ()
		  (interactive)
		  (evil-force-normal-state)
		  (evil-window-left 1)))
(global-set-key (kbd "M-k")
		(defun meain/window-top ()
		  (interactive)
		  (evil-force-normal-state)
		  (evil-window-top 1)))
(global-set-key (kbd "M-j")
		(defun meain/window-bottom ()
		  (interactive)
		  (evil-force-normal-state)
		  (evil-window-bottom 1)))
(global-set-key (kbd "M-b")
		(defun meain/horizontal-split ()
		  (interactive)
		  (evil-force-normal-state)
		  (split-window-below)
		  (windmove-down)))
(global-set-key (kbd "M-v")
		(defun meain/vertical-split ()
		  (interactive)
		  (evil-force-normal-state)
		  (split-window-right)
		  (windmove-right)))
(global-set-key (kbd "M-w")
		'evil-window-delete)

;; File manipulation mappings
(evil-leader/set-key "<SPC>" 'save-buffer)

;; Other keybindings
(define-key evil-normal-state-map (kbd "\\") 'evil-jump-backward)
(define-key evil-normal-state-map (kbd "~") 'evil-jump-forward)

;; Up/Down don't bother wrap
(define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)

;; Quick open config file
(evil-leader/set-key "c"
  (defun meain/load-config ()
    (interactive)
    (find-file "~/.emacs")))

;; Full screen emacs
(global-set-key (kbd "<s-return>")
		'toggle-frame-fullscreen)

;; Open dired
(define-key evil-normal-state-map (kbd "-") 'counsel-find-file)

;; Fullscreen current buffer
(defun meain/toggle-maximize-buffer ()
  "Maximize the current buffer."
  (interactive)
  (if (= 1 (length (window-list)))
      (jump-to-register '_)
    (progn
      (window-configuration-to-register '_)
      (delete-other-windows))))
(define-key evil-normal-state-map (kbd "``") `meain/toggle-maximize-buffer)

;; Eval region
(define-key evil-visual-state-map (kbd ";") 'eval-region)

;;; [Non evel packages] =================================================

;; Code formatting
(use-package format-all :ensure t)
(use-package srefactor
  :ensure t
  :init (require 'srefactor-lisp))
(defun meain/auto-format ()
  "Custom auto-format based on filetype."
  (interactive)
  (if (eq major-mode 'emacs-lisp-mode)
      (srefactor-lisp-format-buffer)
    (format-all-buffer)))
(define-key evil-normal-state-map (kbd ",,") 'meain/auto-format)

;; Yank highlights
(use-package volatile-highlights
  :ensure t
  :init (progn
	  (volatile-highlights-mode t)
	  (vhl/define-extension 'evil 'evil-yank 'evil-yank-line
				'evil-delete 'evil-paste-after)
	  (vhl/install-extension 'evil)))

;; Projectile
;; (use-package projectile
;;   :ensure t
;;   :config (progn
;; 	    (defun meain/fuzzy-file-open ()
;; 	      "Switch between helm and projectil based on directory."
;; 	      (interactive)
;; 	      (if (string= (shell-command-to-string (concat "git -C " default-directory " rev-parse"))
;; 			   "")
;; 		  (projectile-find-file)
;; 		(helm-recentf)))
;; 	    (evil-leader/set-key "f" 'meain/fuzzy-file-open)))

;; Magit
;; (use-package magit
;;   :ensure t
;;   :init (evil-leader/set-key "g" 'magit-status))

;; Flycheck
(use-package flycheck
  :ensure t
  :init (progn
	  (global-flycheck-mode)
	  (setq flycheck-checker-error-threshold 1500)
	  (evil-leader/set-key "k" 'flycheck-previous-error)
	  (evil-leader/set-key "j" 'flycheck-next-error)))

;; drag-stuff
(use-package drag-stuff
  :ensure t
  :init (progn
	  (drag-stuff-mode t)
	  (drag-stuff-global-mode 1)):config
  (progn
    (define-key evil-visual-state-map (kbd "<up>") 'drag-stuff-up)
    (define-key evil-visual-state-map (kbd "<down>") 'drag-stuff-down)
    (define-key evil-visual-state-map (kbd "<left>") 'drag-stuff-left)
    (define-key evil-visual-state-map (kbd "<right>") 'drag-stuff-right)))

;;; [Language pugins] ===============================================

;; Rust
(use-package rust-mode :ensure t)

;; Lua
(use-package lua-mode :ensure t)

;;; [Other plugins] ================================================

;; Try
(use-package try :ensure t)

;; Sane term
(use-package sane-term
  :ensure t
  :init (progn
	  (global-set-key (kbd "M-t")
			  'sane-term)))

;; Saveplace
(use-package saveplace
  :ensure t
  :init (progn
	  (save-place-mode t)
	  (setq save-place-file "~/.emacs.d/saveplace")))



;; Emacs dump
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes '("928ffa547ce426cdcea1f3ea8b5a25fd4808ca2bc2ab2f964621a8691406d94c"
			"0423ec89db11589d86cbd6f0c9e5417594a4506cf26e29ff400797fdcb25732b"
			default))
 '(flycheck-checker-error-threshold 1000)
 '(helm-completion-style 'emacs)
 '(package-selected-packages '(company flx lua-mode counsel ivy projectile
				       sane-term try drag-stuff diff-hl flycheck
				       magit evil-surround volatile-highlights shell-pop
				       evil-commentary rust-mode modus-operandi-theme
				       modus-vivendi-theme helm dumb-jump srefactor
				       use-package evil)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
