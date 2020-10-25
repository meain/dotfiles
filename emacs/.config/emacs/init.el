;;; init -- meain's Emacs config

;;; Commentary:
;; Well, this is a vimmer's Emacs config.  Nothing fancy though.

;;; Code:

;; increase gc threshold (speeds up initial load)
(setq gc-cons-threshold (* 50 1000 1000))

;;; [PACKAGE SETUP] =============================================

(setq user-mail-address "mail@meain.io" user-full-name
      "Abin Simon")

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
(setq use-package-always-demand (daemonp)) ;; eager load if daemon

;; Setup quelpa
(use-package quelpa :ensure t)


;;; [BASE EVIL] =================================================

;; Evil mode (set this up first)
(use-package evil
  :ensure t
  :init (progn
	  (setq evil-want-integration t)
	  (setq evil-want-keybinding nil)
	  (setq evil-want-C-u-scroll t)
	  (setq evil-undo-system 'undo-tree)
	  (setq-default evil-symbol-word-search t)):config
  (progn
    (evil-mode t)
    (defalias #'forward-evil-word #'forward-evil-symbol)))

;; Evil leader
(use-package evil-leader
  :ensure t
  :init (progn
	  (global-evil-leader-mode)
	  (evil-leader/set-leader "s")))

(use-package evil-collection
  :ensure t
  :after evil
  :config (evil-collection-init))

;;; [BASIC SETTINGS] =============================================

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

;; Use cl-lib
(require 'cl-lib)

;; Backup and autosave somewhere else
(setq backup-directory-alist `((".*" . "~/.cache/emacs/backup")))
(setq auto-save-file-name-transforms `((".*" "~/.cache/emacs/autosave" t)))

;; Don't create lockfiles
(setq create-lockfiles nil)

;; Don't clutter my emacs conf
;; Will have to call load-file in case I actually need this on next startup
;; Just keeping it as a file so that I can copy paste
(setq custom-file "~/.config/emacs/custom-config.el")

;;; [VISUAL CONFIG] ==============================================

;; Disable useless stuff
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(setq inhibit-startup-screen t)
;; (set-frame-parameter nil 'undecorated t)

;; Font (set-frame-font  "Profont for Powerline 16")
(add-to-list 'default-frame-alist
	     '(font . "Profont for Powerline 16"))
(set-face-attribute 'default nil :font "Profont for Powerline 16")
(set-face-attribute 'fixed-pitch nil :font "Profont for Powerline 16")
(set-face-attribute 'variable-pitch nil :font "Profont for Powerline 16")
(defun meain/select-font ()
  "Set font after selection using ivy."
  (interactive)
  (ivy-read "Choose font:"
	    (font-family-list)
	    :action (lambda (x)
		      (set-frame-font x))))

;; Bell: audio -> visual
(setq visible-bell nil)
(setq ring-bell-function (lambda ()
			   (unless (memq this-command
					 '(isearch-abort abort-recursive-edit exit-minibuffer
							 keyboard-quit))
			     (invert-face 'mode-line)
			     (run-with-timer 0.1 nil 'invert-face 'mode-line))))

;; emoji support
(defun meain/set-emoji-font ()
  "Setup proper emoji font."
  (let ((font (cond
	       ((string-equal system-type "darwin") "Apple Color Emoji 10")
	       ((string-equal system-type "gnu/linux") "Symbola 10"))))
    (set-fontset-font t 'unicode font nil 'prepend)))
(defun meain/set-emoji-font-in-frame (frame)
  "Hook to be called for setting emoji font in FRAME."
  (with-selected-frame frame
    (meain/set-emoji-font))
  (remove-hook 'after-make-frame-functions 'meain/set-emoji-font-in-frame))
(if (daemonp)
    (add-hook 'after-make-frame-functions 'meain/set-emoji-font-in-frame)
  (meain/set-emoji-font))

;; Theme
(use-package modus-operandi-theme
  :ensure t
  :init (load-theme 'modus-operandi t))

;; Diminish
(use-package diminish
  :ensure t
  :defer t
  :init (progn
	  (diminish 'eldoc-mode)
	  (diminish 'auto-revert-mode)))

;;; [BASIC BUILTINS] ===========================================

;; Show open and closing brackets
(show-paren-mode t)
(setq show-paren-delay 0)

;; Keep files in sync with filesystem
(global-auto-revert-mode t)

;; Disable line wrapping
(setq-default truncate-lines 1)

;; Cursor blink
(blink-cursor-mode -1)

;; Follow symlinks for vc
(setq vc-follow-symlinks t)

;; auto-pair
(electric-pair-mode t)

;; eldoc load
(require 'eldoc)

;; dired
(use-package dired
  :config (progn
	    (setq delete-by-moving-to-trash t)
	    (setq dired-listing-switches "-AGFhl")
	    (setq dired-dwim-target t)
	    (define-key dired-mode-map (kbd "-") 'dired-up-directory)
	    (add-hook 'dired-mode-hook 'hl-line-mode)))

;;; [EVIL CONFIG] ================================================

;; Evil commentary
(use-package evil-commentary
  :ensure t
  :diminish :init
  (evil-commentary-mode))

;; Evil surround
(use-package evil-surround
  :ensure t
  :init (global-evil-surround-mode 1))

;; Evil text objects
(use-package evil-textobj-line :ensure t)
(use-package evil-textobj-syntax :ensure t)

;; Evil number increment

(unless (package-installed-p 'evil-numbers)
  (quelpa '(evil-numbers :repo "janpath/evil-numbers"
			 :fetcher github)))
;; cannot directly use C-x (in use by emacs)
(define-key evil-normal-state-map (kbd "g C-a") 'evil-numbers/inc-at-pt-incremental)
(define-key evil-normal-state-map (kbd "g C-x") 'evil-numbers/dec-at-pt-incremental)

;; Save buffer
(define-key evil-normal-state-map (kbd "<SPC> <SPC>") 'evil-write)

;; Hit universal arg without ctrl
(evil-leader/set-key "u" 'universal-argument)
(global-set-key (kbd "M-u")
		'universal-argument)

;; Eval last sexp
(evil-leader/set-key "x" 'eval-last-sexp)

;; Window mappings
(global-set-key (kbd "M-l")
		(lambda ()
		  (interactive)
		  (evil-force-normal-state)
		  (evil-window-right 1)))
(global-set-key (kbd "M-h")
		(lambda ()
		  (interactive)
		  (evil-force-normal-state)
		  (evil-window-left 1)))
(global-set-key (kbd "M-k")
		(lambda ()
		  (interactive)
		  (evil-force-normal-state)
		  (evil-window-up 1)))
(global-set-key (kbd "M-j")
		(lambda ()
		  (interactive)
		  (evil-force-normal-state)
		  (evil-window-down 1)))
(global-set-key (kbd "M-b")
		(lambda ()
		  (interactive)
		  (evil-force-normal-state)
		  (split-window-below)
		  (windmove-down)))
(global-set-key (kbd "M-v")
		(lambda ()
		  (interactive)
		  (evil-force-normal-state)
		  (split-window-right)
		  (windmove-right)))
(global-set-key (kbd "M-w")
		'delete-window)
(global-set-key (kbd "M-o")
		(lambda (&optional alternate)
		  (interactive "P")
		  (if alternate
		      (call-interactively 'delete-other-windows)
		    (other-window 1))))

;; Shrink and enlarge windows (not contextual as of now)
;; https://www.emacswiki.org/emacs/WindowResize
(global-set-key (kbd "M-H")
		'shrink-window-horizontally)
(global-set-key (kbd "M-L")
		'enlarge-window-horizontally)
(global-set-key (kbd "M-K")
		'shrink-window)
(global-set-key (kbd "M-J")
		'enlarge-window)

;; File manipulation mappings
(evil-leader/set-key "<SPC>" 'save-buffer)

;; Other keybindings
(define-key evil-normal-state-map (kbd "\\") 'evil-jump-backward)
(define-key evil-normal-state-map (kbd "~") 'evil-jump-forward)

;; Up/Down don't bother wrap
(define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)

;; Remap macro recoring key
(define-key evil-normal-state-map "Q" 'evil-record-macro)

;; Eval region
;; Figure out a way to auto exit normal mode after eval
(define-key evil-visual-state-map (kbd ";") 'eval-region)

;; Quick quit
(defun meain/kill-current-buffer-unless-scratch ()
  "Kill current buffer if it is not scratch."
  (interactive)
  (if (= (length (mapcar #'window-buffer
			 (window-list))) 1)
      (switch-to-buffer "*scratch*")
    (cond
     ((derived-mode-p 'prog-mode)
      (evil-quit))
     ((equal major-mode 'imenu-list-major-mode)
      (evil-quit))
     ((equal major-mode 'vterm-mode)
      (vterm-toggle))
     (t (previous-buffer)))))
(define-key evil-normal-state-map (kbd "q") 'meain/kill-current-buffer-unless-scratch)

;; Y to y$
(defun meain/yank-till-line-end ()
  "Yank till end of line."
  (interactive)
  (evil-yank (point)
	     ;; subtracting 1 for newline
	     (- (save-excursion
		  (forward-line)
		  (point))
		1)))
(define-key evil-normal-state-map (kbd "Y") 'meain/yank-till-line-end)

;; Quit out of everythign with esc
(defun meain/keyboard-quit ()
  "Quit out of whatever."
  (interactive)
  (keyboard-escape-quit)
  (minibuffer-keyboard-quit)
  (keyboard-quit))
(global-set-key [escape]
		'meain/keyboard-quit)

;; Quick replace
(define-key evil-normal-state-map (kbd "<SPC> ;") (lambda ()
						    (interactive)
						    (evil-ex "%s/")))
(define-key evil-visual-state-map (kbd "<SPC> ;") (lambda ()
						    (interactive)
						    (evil-ex "'<,'>s/")))

;;; [OTHER PACKAGES] =============================================

;; flymake
(use-package flymake
  :init (add-hook 'find-file-hook 'flymake-find-file-hook))
(use-package flymake-diagnostic-at-point
  :ensure t
  :after flymake
  :config (progn
	    (setq flymake-diagnostic-at-point-error-prefix
		  "! ")
	    (setq flymake-diagnostic-at-point-display-diagnostic-function
		  'flymake-diagnostic-at-point-display-minibuffer)
	    (add-hook 'flymake-mode-hook #'flymake-diagnostic-at-point-mode)
	    (evil-leader/set-key "j" 'flymake-goto-next-error)
	    (evil-leader/set-key "k" 'flymake-goto-prev-error)))

;; Company for autocompletions
(use-package company
  :ensure t
  :diminish :init
  (global-company-mode)
  :config (progn
	    (setq company-dabbrev-downcase nil) ;; Do not lowercase my completions
	    (setq company-idle-delay 0)
	    (setq company-minimum-prefix-length 3)))

;; Ivy && Counsel
(use-package counsel :ensure t)
(use-package flx :ensure t) ;; ivy--regex-plus uses this
(use-package ivy
  :after flx
  :ensure t
  :diminish :config
  (progn
    (defun meain/buffer-switcher (&optional alternate)
      "Choose between ivy-switch-buffer or ibuffer-other-window based on ALTERNATE."
      (interactive "P")
      (if alternate
	  (ibuffer-other-window)
	(ivy-switch-buffer)))
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t) ; extend searching to bookmarks and
    (setq ivy-height 10) ; Set height of the ivy window
    (setq ivy-count-format "(%d/%d) ") ; count format, from the ivy help page
    (setq ivy-display-style 'fancy)
    (setq ivy-format-functions-alist '((t . ivy-format-function-line))) ; Make highlight extend all the way to the right
    (setq ivy-re-builders-alist '((ivy-switch-buffer . ivy--regex-plus)
				  (counsel-rg . ivy--regex-plus)
				  (t . ivy--regex-fuzzy)))
    (global-set-key (kbd "M-c")
		    'meain/buffer-switcher)
    (evil-leader/set-key "b b" 'meain/buffer-switcher)
    (evil-leader/set-key "r" 'counsel-recentf)
    (global-set-key (kbd "M-x")
		    'counsel-M-x)
    (evil-leader/set-key "l" 'counsel-M-x)
    (evil-leader/set-key "h f" 'counsel-describe-function)
    (evil-leader/set-key "h v" 'counsel-describe-variable)
    (evil-leader/set-key "h o" 'counsel-describe-symbol)
    (evil-leader/set-key "h l" 'counsel-find-library)
    (evil-leader/set-key "h i" 'counsel-info-lookup-symbol)
    (define-key evil-normal-state-map (kbd "M-f") 'counsel-M-x)
    (define-key evil-normal-state-map (kbd "_") 'dired-jump)
    (define-key evil-normal-state-map (kbd "-") 'counsel-find-file)
    (define-key ivy-minibuffer-map (kbd "C-j") 'ivy-immediate-done)))

;; ivy-rich
(use-package ivy-rich
  :ensure t
  :init (ivy-rich-mode))

;; amx (better mx)
(use-package amx
  :ensure t
  :init (amx-mode))

;; Helpful package
(use-package helpful
  :ensure t
  :commands (helpful-callable helpful-variable helpful-at-point
			      helpful-key):config
  (progn
    (evil-leader/set-key "h p" 'helpful-at-point)
    (evil-leader/set-key "h k" 'helpful-key)
    (setq counsel-describe-function-function #'helpful-callable)
    (setq counsel-describe-variable-function #'helpful-variable)))

;; ibuffer-projectile
(use-package ibuffer-projectile
  :ensure t
  :config (progn
	    (add-hook 'ibuffer-hook
		      (lambda ()
			(ibuffer-projectile-set-filter-groups)
			(unless (eq ibuffer-sorting-mode 'alphabetic)
			  (ibuffer-do-sort-by-alphabetic))))
	    (setq ibuffer-formats '((mark modified
					  read-only
					  " "
					  (name 18 18 :left :elide)
					  " "
					  (size 9 -1 :right)
					  " "
					  (mode 16 16 :left :elide)
					  " "
					  project-relative-file)))))


;; rg.el
(use-package rg
  :ensure t
  :commands rg
  :init (progn
	  (defun meain/rg-search (&optional alternate)
	    "Choose between counsel-rg or rg based on ALTERNATE."
	    (interactive "P")
	    (if alternate
		(call-interactively 'rg)
	      (counsel-rg)))
	  (evil-leader/set-key "f" 'meain/rg-search)))

;; dumb-jump
(use-package dumb-jump
  :ensure t
  :commands dumb-jumb-go
  :init (evil-leader/set-key "J" 'dumb-jump-go))


;; Code formatting
(use-package srefactor
  :commands srefactor-lisp-format-buffer
  :ensure t
  :init (require 'srefactor-lisp))
(use-package format-all
  :commands format-all-buffer
  :ensure t
  :init (progn
	  (defun meain/auto-format ()
	    "Custom auto-format based on filetype."
	    (interactive)
	    (if (eq major-mode 'emacs-lisp-mode)
		(srefactor-lisp-format-buffer)
	      (format-all-buffer)))
	  (define-key evil-normal-state-map (kbd ",,") 'meain/auto-format)))

;; Projectile
(use-package projectile
  :ensure t
  :diminish :commands
  (projectile-switch-project projectile-find-file)
  :config (progn
	    (setq projectile-mode-line "Projectile") ; might speed up tramp
	    (projectile-mode 1)
	    (evil-leader/set-key "p" 'projectile-switch-project)
	    (setq projectile-completion-system 'ivy)
	    (setq projectile-sort-order 'recently-active)
	    (define-key evil-normal-state-map (kbd "<RET>") 'projectile-find-file)))

;; LSP
(use-package eglot
  :commands eglot-ensure
  :ensure t
  :hook ((python-mode . eglot-ensure)
	 (rust-mode . eglot-ensure)):init
  (progn
    (add-hook 'eglot-managed-mode-hook
	      (lambda ()
		(eldoc-mode -1)
		(define-key evil-normal-state-map (kbd "K") 'eldoc-print-current-symbol-info)
		(define-key evil-normal-state-map (kbd "g d") 'xref-find-definitions)
		(define-key evil-normal-state-map (kbd "g r") 'xref-find-references)
		(define-key evil-normal-state-map (kbd "g R") 'eglot-rename)
		(define-key evil-normal-state-map (kbd "g ,") 'eglot-format-buffer)
		(define-key evil-normal-state-map (kbd "g a") 'eglot-code-actions)))))

;; xref changes
(evil-define-key 'normal
  xref--xref-buffer-mode-map
  (kbd "j")
  'xref-next-line)
(evil-define-key 'normal
  xref--xref-buffer-mode-map
  (kbd "k")
  'xref-prev-line)
(evil-define-key 'normal
  xref--xref-buffer-mode-map
  (kbd "<RET>")
  'xref-quit-and-goto-xref)
(evil-define-key 'normal
  xref--xref-buffer-mode-map
  (kbd "<SPC>")
  'xref-goto-xref)
;; start in insert mode for xref buffers
(evil-set-initial-state 'xref--xref-buffer-mode
			'insert)

;; Tagbar alternative
(use-package imenu :ensure t
  :commands imenu)
(use-package imenu-list
  :ensure t
  :commands imenu-list-smart-toggle
  :init (global-set-key (kbd "M--")
			(lambda (&optional alternate)
			  (interactive "P")
			  (if alternate
			      (imenu-list-smart-toggle)
			    (call-interactively 'imenu)))):config
  (progn
    (setq imenu-list-focus-after-activation t)
    (setq imenu-list-after-jump-hook nil)
    (setq imenu-list-auto-resize t)))

;; Flat imenu
(use-package flimenu
  :ensure t
  :after imenu-list
  :config (flimenu-global-mode 1))

;; Magit
(use-package magit
  :ensure t
  :commands magit-status
  :init (evil-leader/set-key "g" 'magit-status))
(use-package evil-magit
  :ensure t
  :after magit
  :config (progn
	    (evil-magit-init)
	    (evil-define-key* evil-magit-state
			      magit-mode-map
			      [escape]
			      nil)))

;; Git messenger
(use-package git-messenger
  :ensure t
  :commands git-messenger:popup-message
  :init (evil-leader/set-key "G" 'git-messenger:popup-message))

;; Quick open scratch buffers
(use-package scratch
  :ensure t
  :commands scratch
  :init (progn
	  (defun meain/scratch (&optional alternate)
	    (interactive "P")
	    (if alternate
		(call-interactively 'scratch)
	      (switch-to-buffer "*scratch*")))
	  (evil-leader/set-key "c" 'meain/scratch)))

;; Highlight color codes
(use-package rainbow-mode
  :ensure t
  :defer t
  :init (rainbow-mode 1))

;; Code folding
(use-package origami
  :ensure t
  :commands evil-toggle-fold
  :init (progn
	  (global-origami-mode)
	  (evil-leader/set-key "o" 'evil-toggle-fold)))

;; drag-stuff
(use-package drag-stuff
  :ensure t
  :diminish :commands
  (drag-stuff-up drag-stuff-down drag-stuff-left
		 drag-stuff-right)
  :init (progn
	  (define-key evil-visual-state-map (kbd "<up>") 'drag-stuff-up)
	  (define-key evil-visual-state-map (kbd "<down>") 'drag-stuff-down)
	  (define-key evil-visual-state-map (kbd "<left>") 'drag-stuff-left)
	  (define-key evil-visual-state-map (kbd "<right>") 'drag-stuff-right)):config
  (progn
    (drag-stuff-mode t)
    (drag-stuff-global-mode 1)))

;; Saveplace
(use-package saveplace
  :ensure t
  :init (progn
	  (save-place-mode t)
	  (setq save-place-file "~/.cache/emacs/saveplace")))

;; Persistant undo using undo-tree
(use-package undo-tree
  :ensure t
  :diminish :config
  (progn
    (setq undo-limit 80000000)
    (setq evil-want-fine-undo nil)
    (setq undo-tree-auto-save-history t)
    (setq undo-tree-history-directory-alist '(("." . "~/.cache/emacs/undo"))))
  :init (global-undo-tree-mode t))

;; Fancier tab managerment
(use-package tab-bar
  :ensure t
  :commands (tab-close tab-new tab-next):init
  (progn
    (defun meain/switch-tab-dwim (&optional close)
      "Switch between available tabs.  Pass CLOSE as t to close the current tab if it is not the last one."
      (interactive "P")
      (let ((tabs (mapcar (lambda (tab)
			    (alist-get 'name tab))
			  (tab-bar--tabs-recent))))
	(if close
	    (if (eq tabs nil)
		(message "Not closing last tab")
	      (tab-close))
	  (cond
	   ((eq tabs nil)
	    (message (concat "Only one tab present. Use `"
			     (substitute-command-keys "\\[tab-new]")
			     "` to create another tab.")))
	   ((eq (length tabs) 1)
	    (tab-next))
	   (t (ivy-read "Select tab: " tabs :action 'tab-bar-switch-to-tab))))))
    (evil-leader/set-key "t" 'meain/switch-tab-dwim)
    (evil-leader/set-key "T" 'tab-new))
  :config (progn
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
	    (tab-bar-history-mode -1)))

;; which-key mode (until I fully figure out emacs)
(use-package which-key
  :ensure t
  :config (which-key-mode))

;; Expand region
(use-package expand-region
  :ensure t
  :config (progn
	    ;; make evil jump list work with expand-region
	    (evil-set-command-property 'er/expand-region
				       :jump t)
	    (global-set-key (kbd "M-i")
			    'er/expand-region)))

;; dtrt (atuo find indend setting)
(use-package dtrt-indent
  :ensure t
  :config (dtrt-indent-global-mode))

(use-package indent-guide
  :ensure t
  :commands (indent-guide-global-mode indent-guide-mode):init
  (progn
    (setq indent-guide-delay nil)
    (setq indent-guide-char "¦") ; Other chars │
    (setq indent-guide-recursive t)
    (evil-leader/set-key "b I" 'indent-guide-global-mode))
  :config (progn
	    (set-face-attribute 'indent-guide-face nil
				:foreground "#DDD")))

;; we need vterm
(use-package vterm
  :ensure t
  :commands vterm
  :config (setq vterm-max-scrollback 100000))
(use-package vterm-toggle
  :ensure t
  :commands vterm-toggle
  :init (progn
	  (global-set-key (kbd "M-;")
			  'vterm-toggle)):config
  (progn
    (setq vterm-toggle-scope 'projectile)
    (setq vterm-toggle-projectile-root t)
    (setq vterm-toggle-fullscreen-p nil)
    ;; always open vterm in a bottom window
    (add-to-list 'display-buffer-alist
		 '((lambda (bufname _)
		     (with-current-buffer bufname
		       (equal major-mode 'vterm-mode)))
		   (display-buffer-reuse-window display-buffer-at-bottom)
		   (reusable-frames . visible)
		   (window-height . 0.8)))
    (evil-set-initial-state 'vterm-mode 'insert)
    (define-key vterm-mode-map (kbd "M-;") 'vterm-toggle)
    (define-key vterm-mode-map (kbd "M-k") 'previous-window-any-frame)))

;; ranger in emacs
(use-package ranger
  :ensure t
  :commands ranger
  :config (use-package image-dired+
	    :ensure t
	    :config (image-diredx-async-mode)))

;;; [FILETYPE PUGINS] ===============================================

(use-package rust-mode :ensure t
  :defer t)
(use-package lua-mode :ensure t
  :defer t)
(use-package jinja2-mode :ensure t
  :defer t)
(use-package json-mode :ensure t
  :defer t)
(use-package config-general-mode :ensure t
  :defer t)  ;; config files
(use-package markdown-mode
  :ensure t
  :defer t
  :config (progn
	    (setq markdown-enable-html -1)
	    (setq markdown-fontify-code-blocks-natively
		  t)))
(use-package grip-mode :ensure t
  :commands grip-mode) ;; markdown preview
(use-package csv-mode
  :ensure t
  :defer t
  :config (progn
	    (setq csv-align-mode t)
	    (set-face-attribute 'csv-separator-face nil
				:background "gray100"
				:foreground "#000000")))
(use-package yaml-mode :ensure t
  :defer t)

;;; [EXTRA PLUGINS] =================================================

;; Try
(use-package try :ensure t
  :commands try)

;; notmuch
(use-package notmuch
  :ensure t
  :commands notmuch
  :init (progn
	  (evil-define-key 'normal
	    notmuch-search-mode-map
	    (kbd "u")
	    'evil-collection-notmuch-search-toggle-unread)
	  (evil-leader/set-key "a n" 'notmuch)
	  (setq notmuch-search-oldest-first nil)
	  (setq message-auto-save-directory "/Users/meain/.local/share/mail")
	  (setq notmuch-saved-searches (quote ((:name "imbox" :query "tag:imbox AND tag:inbox"
						      :key "i"
						      :sort-order newest-first)
					       (:name "meain" :query "query=to:mail@meain.io AND tag:inbox"
						      :key "m"
						      :sort-order oldest-first)
					       (:name "github" :query "tag:github AND tag:inbox"
						      :key "g"
						      :sort-order oldest-first)
					       (:name "sent" :query "tag:sent"
						      :key "s"
						      :sort-order newest-first)
					       (:name "drafts" :query "tag:draft AND tag:inbox"
						      :key "d")
					       (:name "unread" :query "tag:unread AND tag:inbox AND -tag:python"
						      :key "u")
					       (:name "python" :query "tag:python AND tag:inbox"
						      :key "p")
					       (:name "newsletter" :query "tag:newsletter AND tag:inbox"
						      :key "n")
					       (:name "known" :query "tag:known AND tag:inbox"
						      :key "k")
					       (:name "archiveable" :query "tag:bullshit AND tag:known AND tag:nonimportant AND tag:inbox"
						      :key "a")
					       (:name "all mail" :query "*"
						      :key "A"
						      :sort-order newest-first))))))

;; elfeed
(use-package elfeed
  :ensure t
  :commands (elfeed elfeed-update):init
  (progn
    (run-at-time nil
		 (* 3 60 60)
		 (lambda ()
		   (elfeed-update)
		   (elfeed-db-save))))
  :config (progn
	    (evil-leader/set-key "a e" 'elfeed)
	    (evil-define-key 'normal
	      elfeed-search-mode-map
	      (kbd "o")
	      'elfeed-search-browse-url)
	    (evil-define-key 'visual
	      elfeed-search-mode-map
	      (kbd "o")
	      'elfeed-search-browse-url)
	    (evil-define-key 'normal
	      elfeed-search-mode-map
	      (kbd "d")
	      'meain/elfeed-search-filter)
	    (evil-define-key 'normal
	      elfeed-search-mode-map
	      (kbd "D")
	      (lambda ()
		(interactive)
		(setq elfeed-search-filter "@2-months-ago +unread")
		(elfeed-search-update :force)))
	    (evil-define-key 'normal
	      elfeed-search-mode-map
	      (kbd "q")
	      'elfeed-db-unload)
	    (defun meain/elfeed-search-filter ()
	      (interactive)
	      (ivy-read "Apply tag: "
			(elfeed-db-get-all-tags)
			:action (lambda (x)
				  (setq elfeed-search-filter (concatenate 'string "@2-months-ago +unread +"
									  x))
				  (elfeed-search-update :force))))
	    (setq-default elfeed-search-filter "@2-months-ago +unread ")
	    (setq elfeed-use-curl t)
	    (setq elfeed-curl-max-connections 10)
	    (setq elfeed-db-directory "~/.config/emacs/elfeed/")
	    (setq elfeed-enclosure-default-dir "~/Downloads/")
	    (setq elfeed-show-entry-switch #'meain/elfeed-display-buffer)
	    (add-to-list 'display-buffer-alist
			 '((lambda (bufname _)
			     (with-current-buffer bufname
			       (equal major-mode 'elfeed-show-mode)))
			   (display-buffer-reuse-window display-buffer-at-bottom)
			   (reusable-frames . visible)
			   (window-height . 0.7)))
	    (defun meain/elfeed-search-print (entry)
	      "Print ENTRY to the buffer."
	      (let* ((feed-width 25)
		     (tags-width 35)
		     (title (or (elfeed-meta entry :title)
				(elfeed-entry-title entry)
				""))
		     (title-faces (elfeed-search--faces (elfeed-entry-tags entry)))
		     (feed (elfeed-entry-feed entry))
		     (feed-title (when feed
				   (or (elfeed-meta feed :title)
				       (elfeed-feed-title feed))))
		     (tags (mapcar #'symbol-name
				   (elfeed-entry-tags entry)))
		     (tags-str (concat " ("
				       (mapconcat 'identity tags ",")
				       ")"))
		     (title-width (- (window-width)
				     feed-width
				     tags-width
				     4))
		     (title-column (elfeed-format-column title
							 (elfeed-clamp elfeed-search-title-min-width
								       title-width elfeed-search-title-max-width)
							 :left))
		     (tag-column (elfeed-format-column tags-str
						       (elfeed-clamp (length tags-str)
								     tags-width
								     tags-width)
						       :left))
		     (feed-column (elfeed-format-column feed-title
							(elfeed-clamp feed-width feed-width feed-width)
							:left)))
		(insert (propertize feed-column 'face 'elfeed-search-feed-face)
			" ")
		(insert (propertize title 'face title-faces 'kbd-help
				    title))
		(insert (propertize tag-column 'face 'elfeed-search-tag-face)
			" ")))
	    (setq elfeed-search-print-entry-function 'meain/elfeed-search-print)
	    (defun meain/elfeed-display-buffer (buf &optional act)
	      (pop-to-buffer buf)
	      (set-window-text-height (get-buffer-window)
				      (round (* 0.7
						(frame-height)))
				      (previous-window-any-frame)))
	    (defun meain/elfeed-enclosure-download (base-dir extension)
	      "Download podcast to `BASE-DIR' with proper heirary using feed and title using `EXTENSION'"
	      (start-process "*elfeed-enclosure-download*"
			     "*elfeed-enclosure-download*"
			     "downloader"
			     (elt (car (elfeed-entry-enclosures elfeed-show-entry))
				  0)
			     (format "%s/%s/%s%s"
				     base-dir
				     (elfeed-feed-title (elfeed-entry-feed elfeed-show-entry))
				     (elfeed-entry-title elfeed-show-entry)
				     extension))
	      (message "Download started for %s - %s"
		       (elfeed-feed-title (elfeed-entry-feed elfeed-show-entry))
		       (elfeed-entry-title elfeed-show-entry)))
	    (defun meain/elfeed-podcast-download-to-local ()
	      "Download current feed(podcast) to usual dir."
	      (interactive)
	      (meain/elfeed-enclosure-download "/Users/meain/Desktop/newsboat/podcasts"
					       ".mp3"))
	    (load-file "~/.config/emacs/elfeed-feeds.el")))

;; command log
(use-package command-log-mode
  :commands global-command-log-mode
  :ensure t
  :init (progn
	  (defun meain/command-log-start ()
	    "Enable command-log-mode and open command-log buffer."
	    (interactive)
	    (global-command-log-mode)
	    (clm/open-command-log-buffer))))

;; Beacon mode
(use-package beacon
  :ensure t
  :defer t
  :init (beacon-mode t))

;; Writing mode
(use-package writeroom-mode :ensure t
  :commands writeroom-mode)

;; tramp dired
(use-package tramp
  :ensure t
  :init (progn
	  (setq remote-file-name-inhibit-cache nil)
	  (setq vc-ignore-dir-regexp (format "%s\\|%s" vc-ignore-dir-regexp tramp-file-name-regexp))
	  (setq tramp-verbose 1)
	  (defun meain/ssh-access ()
	    "Opern dired in a server by selcting a host via autocomplete."
	    (interactive)
	    (dired (concatenate 'string
				"/ssh:"
				(meain/ssh-host-picker)
				":")))))

;; tramp-term
(use-package tramp-term
  :ensure t
  :commands tramp-term
  :init (defun meain/ssh-term ()
	  "SSH into a server by selcting a host via autocomplete."
	  (interactive)
	  (tramp-term (list (meain/ssh-host-picker)))))

;;; [CUSTOM FUNCTIONS] ==============================================

;; host picker
(defun meain/ssh-host-picker ()
  "Interactively pick ssh host."
  (with-temp-buffer
    (insert-file-contents "~/.ssh/config")
    (ivy-read "Choose host: "
	      (mapcar (lambda (x)
			(replace-regexp-in-string (regexp-quote "Host ")
						  ""
						  x))
		      (remove-if-not #'(lambda (x)
					 (s-starts-with-p "Host" x))
				     (split-string (buffer-string)
						   "\n")))
	      :require-match t
	      :action (lambda (x)
			(format "%s" x)))))

;; split between hirizontal and vertical
(defun meain/window-split-toggle ()
  "Toggle between horizontal and vertical split with two windows."
  (interactive)
  (if (> (length (window-list)) 2)
      (error "Can't toggle with more than 2 windows!")
    (let ((func (if (window-full-height-p)
		    #'split-window-vertically
		  #'split-window-horizontally)))
      (delete-other-windows)
      (funcall func)
      (save-selected-window (other-window 1)
			    (switch-to-buffer (other-buffer))))))

;; Quick open config file
(evil-leader/set-key "e"
  (defun meain/load-config ()
    "Load emacs config for editing."
    (interactive)
    (find-file "~/.dotfiles/emacs/.config/emacs/init.el")))

;; Full screen emacs
(global-set-key (kbd "<s-return>")
		'toggle-frame-fullscreen)

;; Fullscreen current buffer
(defvar meain/window-configuration nil)
(define-minor-mode meain/monacle-mode
  "Zoom in and out of single window."
  :lighter " [M]"
  :global nil
  (if (one-window-p)
      (when meain/window-configuration
	(set-window-configuration meain/window-configuration))
    (setq meain/window-configuration (current-window-configuration))
    (delete-other-windows)))
(evil-leader/set-key "m" 'meain/monacle-mode)

;; vime functionality within emacs
(use-package uuid :ensure t
  :commands uuid-string)
(defun meain/vime-name-append (filename)
  "Util function used to parse :name block for vime entries.  FILENAME is the name of the vime file."
  (with-temp-buffer
    (insert-file-contents (concatenate 'string "~/.cache/vime/" filename))
    (concatenate 'string
		 filename
		 (if (s-starts-with-p ":name"
				      (car (split-string (buffer-string)
							 "\n")))
		     (replace-regexp-in-string (regexp-quote ":name")
					       ""
					       (car (split-string (buffer-string)
								  "\n")))
		   ""))))
(defun meain/vime (&optional listitems)
  "Load a random file inside ~/.cache/vime dir.  Used as a temp notes dir.
Pass in `LISTITEMS to decide if you wanna create a new item or search for existing items."
  (interactive "P")
  (if listitems
      (ivy-read "Choose file: "
		(mapcar (lambda (x)
			  (meain/vime-name-append (car x)))
			(sort (remove-if-not #'(lambda (x)
						 (eq (nth 1 x) nil))
					     (directory-files-and-attributes "~/.cache/vime"))
			      #'(lambda (x y)
				  (time-less-p (nth 6 y)
					       (nth 6 x)))))
		:preselect (ivy-thing-at-point):require-match
		t
		:action (lambda (x)
			  (find-file (concat "~/.cache/vime/"
					     (car (split-string x))))):caller'meain/vime-open)
    (find-file (concat "~/.cache/vime/_"
		       (substring (uuid-string)
				  0
				  4)))))
(evil-leader/set-key "v" 'meain/vime)

;; Narrow region
(defun meain/narrow-region-dwim ()
  "Narrow or widen the region (dwim)."
  (interactive)
  (if (eq evil-state 'visual)
      (call-interactively 'narrow-to-region)
    (call-interactively 'widen)))
(global-set-key (kbd "M-N")
		'meain/narrow-region-dwim)

;; Buffer keybinds
(evil-leader/set-key "b k" 'kill-buffer)
(evil-leader/set-key "b o" 'previous-buffer)

;; Bookmarks
(setq bookmark-save-flag 1)
(evil-leader/set-key "b m" 'bookmark-jump)
(evil-leader/set-key "b M" 'bookmark-set)

;; Better modeline
(setq-default mode-line-format (list '(:eval (if (eq 'emacs evil-state)
						 "! "
					       ": ")) ;; vim or emacs mode
				     ;; (car (cdr (reverse (split-string (buffer-file-name) "/"))))
				     ;; the buffer name; the file name as a tool tip

				     '(:eval (list (if (eq buffer-file-name nil)
						       ""
						     (concatenate 'string
								  (car (cdr (reverse (split-string (buffer-file-name)
												   "/"))))
								  "/"))
						   (propertize "%b"
							       'face
							       'font-lock-type-face
							       'help-echo
							       (buffer-file-name))))
				     '(:eval (when-let (vc vc-mode) ;; git branch
					       (list " @"
						     (propertize (substring vc 5)
								 'face
								 'font-lock-comment-face)
						     " ")))
				     ;; spacer

				     '(:eval (propertize " "
							 'display
							 `((space :align-to (- (+ right right-fringe right-margin)
									       ,(+ 2
										   (+ (string-width (format-mode-line "%l:%c"))
										      (string-width (format-mode-line "%m")))))))))
				     (propertize "%l:%c" 'face 'font-lock-variable-name-face) ;; position in file
				     (propertize " %m " 'face 'font-lock-constant-face) ;; current mode
				     ))

;; Print emacs startup time
(add-hook 'emacs-startup-hook
	  (lambda ()
	    (message "Emacs ready in %s with %d garbage collections."
		     (format "%.2f seconds"
			     (float-time (time-subtract after-init-time before-init-time)))
		     gcs-done)))

;; drop gc threshold back
(setq gc-cons-threshold 800000)

(provide 'init)
;;; init ends here
