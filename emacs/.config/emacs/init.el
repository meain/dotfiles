;;; init -- meain's Emacs config

;;; Commentary:
;; Well, this is a vimmer's Emacs config.  Nothing fancy though.

;;; Code:

;; increase gc threshold (speeds up initial load)
(setq gc-cons-threshold 400000000)

;;; [PACKAGE SETUP] =============================================

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

;; Setup quelpa
(use-package quelpa :ensure t)


;;; [BASE EVIL] =================================================

;; Evil mode (set this up first)
(use-package evil
  :ensure t
  :init (evil-mode t))

;; Evil leader
(use-package evil-leader
  :ensure t
  :init (progn
	  (global-evil-leader-mode)
	  (evil-leader/set-leader "s")))

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

;;; [VISUAL CONFIG] ==============================================

;; Disable useless stuff
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(setq inhibit-startup-screen t)

;; Font (set-frame-font  "Profont for Powerline 16")
(add-to-list 'default-frame-alist
	     '(font . "Profont for Powerline 16"))
(set-face-attribute 'default t :font "Profont for Powerline 16")
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
	       ((string-equal system-type "darwin") "Apple Color Emoji")
	       ((string-equal system-type "gnu/linux") "Symbola"))))
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
(require 'dired)
(define-key dired-mode-map (kbd "-") 'dired-up-directory)

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

;; Hit universal arg without ctrl
(evil-leader/set-key "u" 'universal-argument)
(global-set-key (kbd "M-u")
		'universal-argument)

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
		'other-window)

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
(require 'flymake)
(use-package flymake-diagnostic-at-point
  :ensure t
  :after flymake
  :config (progn
	    (setq flymake-diagnostic-at-point-error-prefix
		  "! ")
	    (setq flymake-diagnostic-at-point-display-diagnostic-function
		  'flymake-diagnostic-at-point-display-minibuffer)
	    (add-hook 'flymake-mode-hook #'flymake-diagnostic-at-point-mode)))
(add-hook 'find-file-hook 'flymake-find-file-hook)
(evil-leader/set-key "j" 'flymake-goto-next-error)
(evil-leader/set-key "k" 'flymake-goto-prev-error)

;; Company for autocompletions
(use-package company
  :ensure t
  :diminish :init
  (global-company-mode)
  :config (setq company-idle-delay 0))

;; Ivy && Counsel
(use-package counsel :ensure t)
(use-package flx :ensure t)
(use-package ivy
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
    (define-key evil-normal-state-map (kbd "<SPC> <SPC>") 'meain/buffer-switcher)
    (evil-leader/set-key "r" 'counsel-recentf)
    (evil-leader/set-key "l" 'counsel-M-x)
    (evil-leader/set-key "h f" 'counsel-describe-function)
    (evil-leader/set-key "h v" 'counsel-describe-variable)
    (evil-leader/set-key "h o" 'counsel-describe-symbol)
    (evil-leader/set-key "h l" 'counsel-find-library)
    (evil-leader/set-key "h i" 'counsel-info-lookup-symbol)
    (define-key evil-normal-state-map (kbd "M-f") 'counsel-M-x)
    (define-key evil-normal-state-map (kbd "-") 'dired-jump)
    (define-key evil-normal-state-map (kbd "_") 'counsel-find-file)
    (define-key ivy-minibuffer-map (kbd "C-j") 'ivy-immediate-done)))

;; ivy-rich
(use-package ivy-rich
  :ensure t
  :init (ivy-rich-mode))

;; amx (better mx)
(use-package amx
  :ensure t
  :init (amx-mode))

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
  :config (progn
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
  :config (progn
	    (evil-leader/set-key "J" 'dumb-jump-go)))


;; Code formatting
(use-package srefactor
  :ensure t
  :init (require 'srefactor-lisp))
(use-package format-all
  :ensure t
  :after srefactor
  :config (progn
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
  :diminish :init
  (projectile-mode 1)
  (evil-leader/set-key "p" 'projectile-switch-project)
  (setq projectile-completion-system 'ivy)
  (setq projectile-sort-order 'recently-active)
  (define-key evil-normal-state-map (kbd "<RET>") 'projectile-find-file))

;; LSP
(use-package eglot
  :ensure t
  :hook ((python-mode . eglot-ensure)
	 (rust-mode . eglot-ensure)):init
  (progn
    (add-hook 'eglot-managed-mode-hook
	      (lambda ()
		(eldoc-mode -1)))
    (define-key evil-normal-state-map (kbd "K") 'eldoc)
    (define-key evil-normal-state-map (kbd "g d") 'xref-find-definitions)
    (define-key evil-normal-state-map (kbd "g r") 'xref-find-references)
    (define-key evil-normal-state-map (kbd "g R") 'eglot-rename)
    (define-key evil-normal-state-map (kbd "g ,") 'eglot-format-buffer)
    (define-key evil-normal-state-map (kbd "g a") 'eglot-code-actions)))

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
(require 'imenu)
(use-package imenu-list
  :ensure t
  :config (progn
	    (setq imenu-list-focus-after-activation t)
	    (setq imenu-list-after-jump-hook nil)
	    (setq imenu-list-auto-resize t)
	    (global-set-key (kbd "M--")
			    (lambda (&optional alternate)
			      (interactive "P")
			      (if alternate
				  (imenu-list-smart-toggle)
				(call-interactively 'imenu))))))

;; Flat imenu
(use-package flimenu
  :ensure t
  :after imenu
  :config (flimenu-global-mode 1))

;; Neotree
(use-package neotree
  :ensure t
  :init (progn
	  (define-key evil-normal-state-map (kbd "<tab>") 'neotree-toggle)
	  (defun meain/neotree-mode-hook ()
	    (define-key neotree-mode-map (kbd "k") 'neotree-previous-line)
	    (define-key neotree-mode-map (kbd "j") 'neotree-next-line)
	    (define-key neotree-mode-map (kbd "<return>") 'neotree-enter)
	    (define-key neotree-mode-map (kbd "<spc>") 'neotree-quick-look)
	    (define-key neotree-mode-map (kbd "R") 'neotree-refresh)
	    (define-key neotree-mode-map (kbd "r") 'neotree-rename-node)
	    (define-key neotree-mode-map (kbd "c") 'neotree-create-node)
	    (define-key neotree-mode-map (kbd "d") 'neotree-delete-node))
	  (setq neo-theme 'ascii)
	  (add-hook 'neotree-mode-hook 'meain/neotree-mode-hook)))

;; Magit
(use-package magit
  :ensure t
  :init (evil-leader/set-key "g" 'magit-status))

;; Quick open scratch buffers
(use-package scratch
  :ensure :config
  (defun meain/scratch-buffer-setup ()
    "Add contents to `scratch' buffer and name it accordingly."
    (let* ((mode (format "%s" major-mode))
	   (string (concat "Scratch buffer for: " mode "\n\n")))
      (when scratch-buffer
	(save-excursion
	  (insert string)
	  (goto-char (point-min))
	  (comment-region (point-at-bol)
			  (point-at-eol)))
	(forward-line 2))
      (rename-buffer (concat "*Scratch for " mode "*")
		     t)))
  :hook (scratch-create-buffer-hook . meain/scratch-buffer-setup):init
  (evil-leader/set-key "c" 'scratch))


;; Highlight color codes
(use-package rainbow-mode
  :ensure t
  :init (rainbow-mode 1))

;; Code folding
(use-package origami
  :ensure t
  :init (progn
	  (global-origami-mode)
	  (evil-leader/set-key "o" 'evil-toggle-fold)))

;; drag-stuff
(use-package drag-stuff
  :ensure t
  :diminish :init
  (progn
    (drag-stuff-mode t)
    (drag-stuff-global-mode 1))
  :config (progn
	    (define-key evil-visual-state-map (kbd "<up>") 'drag-stuff-up)
	    (define-key evil-visual-state-map (kbd "<down>") 'drag-stuff-down)
	    (define-key evil-visual-state-map (kbd "<left>") 'drag-stuff-left)
	    (define-key evil-visual-state-map (kbd "<right>") 'drag-stuff-right)))

;; Saveplace
(use-package saveplace
  :ensure t
  :init (progn
	  (save-place-mode t)
	  (setq save-place-file "~/.cache/emacs/saveplace")))

;; Persistant undo using undo-tree
(use-package undo-tree
  :ensure t
  :diminish :init
  (progn
    (global-undo-tree-mode)
    (setq undo-tree-auto-save-history t)
    (setq undo-tree-history-directory-alist '(("." . "~/.cache/emacs/undo")))))

;; Fancier tab managerment
(use-package tab-bar
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
	    (tab-bar-history-mode -1)
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
		   (t (ivy-read "Select tab: " tabs :action 'tab-bar-switch-to-tab))))))):init
  (evil-leader/set-key "t" 'meain/switch-tab-dwim)
  (evil-leader/set-key "T" 'tab-new))

;; which-key mode (until I fully figure out emacs)
(use-package which-key
  :ensure t
  :config (which-key-mode))

;; Expand region
(use-package expand-region
  :ensure t
  :config (global-set-key (kbd "M-i")
			  'er/expand-region))

;; dtrt (atuo find indend setting)
(quelpa '(dtrt :repo "jscheid/dtrt-indent"
	       :fetcher github))
(dtrt-indent-global-mode)

;; we need vterm
(use-package vterm :ensure t)
(use-package vterm-toggle
  :ensure t
  :config (progn
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
			   (window-height . 0.5)))
	    (evil-set-initial-state 'vterm-mode 'insert)
	    (global-set-key (kbd "M-;")
			    'vterm-toggle)
	    (define-key vterm-mode-map (kbd "M-;") 'vterm-toggle)))

;;; [LANGUAGE PUGINS] ===============================================

(use-package rust-mode :ensure t)
(use-package lua-mode :ensure t)
(use-package jinja2-mode :ensure t)
(use-package json-mode :ensure t)
(use-package config-general-mode :ensure t)  ;; config files
(use-package markdown-mode
  :ensure t
  :config (progn
	    (setq markdown-enable-html -1)
	    (setq markdown-fontify-code-blocks-natively
		  t)))
(use-package grip-mode :ensure t) ;; markdown preview

;;; [EXTRA PLUGINS] =================================================

;; Try
(use-package try :ensure t)

;; notmuch
(use-package notmuch
  :ensure t
  :init (setq message-auto-save-directory "/Users/meain/.local/share/mail"))

;; elfeed
(use-package elfeed
  :ensure t
  :init (setq elfeed-feeds (with-temp-buffer
			     (insert-file-contents "~/.config/newsboat/urls")
			     (mapcar (lambda (x)
				       (car (split-string x)))
				     (remove-if-not #'(lambda (x)
							(string-match-p "^https://" x))
						    (split-string (buffer-string)
								  "\n"
								  t))))))

;;; [CUSTOM FUNCTIONS] ==============================================

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
(define-key evil-normal-state-map (kbd "``") `meain/monacle-mode)

;; vime functionality within emacs
(use-package uuid :ensure t)
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
										   (+ (string-width (format-mode-line "%l"))
										      (string-width (format-mode-line "%m")))))))))
				     (propertize "%l" 'face 'font-lock-constant-face) ;; position in file
				     (propertize " %m " 'face 'font-lock-string-face) ;; current mode
				     ))


;; drop gc threshold back
(setq gc-cons-threshold 800000)

(provide 'init)
;;; init ends here
