;;; .emacs -- meain's Emacs config

;;; Commentary:
;; Well, this is a vimmer's Emacs config.  Nothing fancy though.

;;; Code:

;; increase gc threshold (speeds up initial load)
(setq gc-cons-threshold 400000000)

;;; [Basic config] =============================================

;; Disable useless stuff
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(setq inhibit-startup-screen t)

;; Font
(set-frame-font "VictorMono Nerd Font 14" nil t)

;; Quicker yes or no
(fset 'yes-or-no-p 'y-or-n-p)

;; Use cl-lib
(require 'cl-lib)

;; UTF-8 please
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Diminish default modes
(diminish 'eldoc-mode)
(diminish 'auto-revert-mode)

;; Show open and closing brackets
(show-paren-mode t)
(setq show-paren-delay 0)

;; vim like scroll behaviour
(setq scroll-step 1)
(setq scroll-margin 3)

;; Bell: audio -> visual
(setq visible-bell nil)
(setq ring-bell-function (lambda ()
			   (unless (memq this-command
					 '(isearch-abort abort-recursive-edit exit-minibuffer
							 keyboard-quit))
			     (invert-face 'mode-line)
			     (run-with-timer 0.1 nil 'invert-face 'mode-line))))

;; Disable line wrapping
(setq-default truncate-lines 1)

;; Cursor blink
(blink-cursor-mode -1)

;; Follow symlinks for vc
(setq vc-follow-symlinks nil)

;; Backup and autosave somewhere else
(setq backup-directory-alist `((".*" . "/tmp/emacsbackup")))
(setq auto-save-file-name-transforms `((".*" "/tmp/emacsautosave" t)))

;; Don't create lockfiles
(setq create-lockfiles nil)

;; Show matching paran
(setq show-paran-mode 1)

;; emoji support
(let ((font (if (= emacs-major-version 25)
		"Symbola"
	      (cond
	       ((string-equal system-type "darwin") "Apple Color Emoji")
	       ((string-equal system-type "gnu/linux") "Symbola")))))
  (set-fontset-font t 'unicode font nil 'prepend))

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

;; Diminish
(use-package diminish :ensure t)

;;; [Evil packages] =================================================

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
  :diminish :init
  (evil-commentary-mode))

;; Evil surround
(use-package evil-surround
  :ensure t
  :init (global-evil-surround-mode 1))

;; Delete in evil-mode does not write to clipboard
;; (defun meain/evil-delete-advice (orig-fn beg end &optional type _ &rest args)
;;     "Make d, c, x to not write to clipboard.  And ORIG-FN, BEG, END, TYPE, ARGS should appear in docstring."
;;     (apply orig-fn beg end type ?_ args))
;; (advice-add 'evil-delete :around 'meain/evil-delete-advice)
;; (advice-add 'evil-change :around 'meain/evil-delete-advice)

;;; [Non evil packages] =================================================

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
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t) ; extend searching to bookmarks and
    (setq ivy-height 10) ; Set height of the ivy window
    (setq ivy-count-format "(%d/%d) ") ; count format, from the ivy help page
    (setq ivy-display-style 'fancy)
    (setq ivy-format-functions-alist '((t . ivy-format-function-line))) ; Make highlight extend all the way to the right
    (setq ivy-re-builders-alist '((ivy-switch-buffer . ivy--regex-plus)
				  (t . ivy--regex-fuzzy)))
    (evil-leader/set-key "f" 'counsel-rg)
    (evil-leader/set-key "b" 'ibuffer-other-window)
    (evil-leader/set-key "s" 'ivy-switch-buffer)
    (evil-leader/set-key "r" 'counsel-recentf)
    (evil-leader/set-key "l" 'counsel-M-x)
    (evil-leader/set-key "h f" 'counsel-describe-function)
    (evil-leader/set-key "h v" 'counsel-describe-variable)
    (evil-leader/set-key "h o" 'counsel-describe-symbol)
    (evil-leader/set-key "h l" 'counsel-find-library)
    (evil-leader/set-key "h i" 'counsel-info-lookup-symbol)
    (define-key evil-normal-state-map (kbd "M-f") 'counsel-M-x)
    (define-key evil-normal-state-map (kbd "-") 'counsel-find-file)))

;; ivy-rich
(use-package ivy-rich
  :ensure t
  :init (ivy-rich-mode))

;; amx (better mx)
(use-package amx
  :ensure t
  :init (amx-mode))

;; dumb-jump
(use-package dumb-jump
  :ensure t
  :config (progn
	    (evil-leader/set-key "j" 'dumb-jump-go)))


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

;; Projectile
(use-package projectile
  :ensure t
  :diminish :init
  (projectile-mode 1)
  (evil-leader/set-key "p" 'projectile-switch-project)
  (setq projectile-completion-system 'ivy)
  (setq projectile-sort-order 'recently-active)
  (define-key evil-normal-state-map (kbd "<RET>") 'projectile-find-file))

;; Flycheck
(use-package flycheck
  :ensure t
  :config (define-fringe-bitmap 'flycheck-fringe-bitmap-empty
	    (vector #b00000000 #b00000000 #b00000000 #b00000000
		    #b00000000 #b00000000 #b00000000 #b00000000
		    #b00000000 #b00000000 #b00000000 #b00000000
		    #b00000000 #b00000000 #b00000000 #b00000000
		    #b00000000))(flycheck-define-error-level 'error :severity 100
	    :compilation-level 2
	    :overlay-category 'flycheck-error-overlay
	    :fringe-bitmap 'flycheck-fringe-bitmap-empty
	    :fringe-face 'flycheck-fringe-error
	    :error-list-face 'flycheck-error-list-error)(flycheck-define-error-level 'warning :severity 10
	    :compilation-level 1
	    :overlay-category 'flycheck-warning-overlay
	    :fringe-bitmap 'flycheck-fringe-bitmap-empty
	    :fringe-face 'flycheck-fringe-warning
	    :error-list-face 'flycheck-error-list-warning)(flycheck-define-error-level 'info :severity -10
	    :compilation-level 0
	    :overlay-category 'flycheck-info-overlay
	    :fringe-bitmap 'flycheck-fringe-bitmap-empty
	    :fringe-face 'flycheck-fringe-info
	    :error-list-face 'flycheck-error-list-info):init
  (progn
    (global-flycheck-mode)
    (setq flycheck-checker-error-threshold 1500)
    (evil-leader/set-key "j" 'flycheck-previous-error)
    (evil-leader/set-key "k" 'flycheck-next-error)))

;; LSP
(use-package eglot
  :ensure t
  :hook ((python-mode . eglot-ensure)
	 (rust-mode . eglot-ensure)):init
  (progn
    (add-hook 'eglot-managed-mode-hook
	      (lambda ()
		(flymake-mode -1))) ;; TODO: figure out a better way
    (define-key evil-normal-state-map (kbd "K") 'eldoc)
    (define-key evil-normal-state-map (kbd "g d") 'xref-find-definitions)
    (define-key evil-normal-state-map (kbd "g r") 'xref-find-references)
    (define-key evil-normal-state-map (kbd "g R") 'eglot-rename)
    (define-key evil-normal-state-map (kbd "g ,") 'eglot-format-buffer)
    (define-key evil-normal-state-map (kbd "g a") 'eglot-code-actions)))
;; (use-package lsp-mode
;;   :ensure t
;;   :hook ((python-mode . lsp-deferred)
;; 	 (rust-mode . lsp-deferred)):commands
;;   (lsp lsp-deferred)
;;   :init (progn
;; 	  (define-key evil-normal-state-map (kbd "g d") 'lsp-find-definition)
;; 	  (define-key evil-normal-state-map (kbd "g r") 'lsp-find-references)
;; 	  (define-key evil-normal-state-map (kbd "g R") 'lsp-rename)
;; 	  (define-key evil-normal-state-map (kbd "g ,") 'lsp-format-buffer)
;; 	  (define-key evil-normal-state-map (kbd "g a") 'lsp-execute-code-action)))
;; (use-package lsp-ui :ensure t
;;   :commands lsp-ui-mode)
;; (use-package lsp-ivy :ensure t
;;   :commands lsp-ivy-workspace-symbol)

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


;;; [Language pugins] ===============================================

(use-package rust-mode :ensure t)  ;; rust
(use-package lua-mode :ensure t)  ;; lua
(use-package jinja2-mode :ensure t)  ;; jinja2

;;; [Other plugins] ================================================

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

;; Try
(use-package try :ensure t)

;; Saveplace
(use-package saveplace
  :ensure t
  :init (progn
	  (save-place-mode t)
	  (setq save-place-file "~/.emacs.d/saveplace")))

;; Persistant undo using undo-tree
(use-package undo-tree
  :ensure t
  :diminish :init
  (progn
    (global-undo-tree-mode)
    (setq undo-tree-auto-save-history t)
    (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))))

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




;;; [Extra keybindings] ===============================================================

;; Hit universal arg without ctrl
(evil-leader/set-key "u" 'universal-argument)

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
		  (evil-window-up 1)))
(global-set-key (kbd "M-j")
		(defun meain/window-bottom ()
		  (interactive)
		  (evil-force-normal-state)
		  (evil-window-down 1)))
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
		'delete-window)

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

;; folding
(use-package origami
  :ensure t
  :init (progn
	  (global-origami-mode)
	  (evil-leader/set-key "o" 'evil-toggle-fold)))

;; Quick open config file
(evil-leader/set-key "e"
  (defun meain/load-config ()
    (interactive)
    (find-file "~/.emacs")))

;; Full screen emacs
(global-set-key (kbd "<s-return>")
		'toggle-frame-fullscreen)

;; Fullscreen current buffer
(use-package emacs
  :config (defvar meain/window-configuration nil)(define-minor-mode meain/monacle-mode
						   "Zoom in and out of single window."
						   :lighter " [M]"
						   :global nil
						   (if (one-window-p)
						       (when meain/window-configuration
							 (set-window-configuration meain/window-configuration))
						     (setq meain/window-configuration (current-window-configuration))
						     (delete-other-windows))))
(define-key evil-normal-state-map (kbd "``") `meain/monacle-mode)

;; Eval region
;; Figure out a way to auto exit normal mode after eval
(define-key evil-visual-state-map (kbd ";") 'eval-region)

;; Rename buffer
(evil-leader/set-key "R" 'rename-buffer)

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
(defun meain/scratch-to-vime ()
  "Copy contents of current *scratch* to vime."
  (interactive)
  (with-current-buffer "*scratch*"
    (append-to-file (concatenate 'string
				 ":name *scratch* on "
				 (format-time-string "%Y-%m-%d %H:%m")
				 "\n"
				 (buffer-string))
		    nil
		    (concat "~/.cache/vime/_"
			    (substring (uuid-string)
				       0
				       4)))
    (erase-buffer)))
(evil-leader/set-key "v" 'meain/vime)

;; Quick quit
(defun meain/kill-current-buffer-unless-scratch ()
  "Kill current buffer if it is not scratch."
  (interactive)
  (if (= (length (mapcar #'window-buffer
			 (window-list))) 1)
      (switch-to-buffer "*scratch*")
    (evil-quit)))
(evil-leader/set-key "q" 'meain/kill-current-buffer-unless-scratch)
(define-key evil-normal-state-map (kbd "q") 'meain/kill-current-buffer-unless-scratch)


;; Fancier tab managerment
(use-package tab-bar
  :config (setq tab-bar-close-button-show nil)(setq tab-bar-close-last-tab-choice 'tab-bar-mode-disable)(setq tab-bar-close-tab-select 'recent)(setq tab-bar-new-tab-choice t)(setq tab-bar-new-tab-to 'right)(setq tab-bar-position nil)(setq tab-bar-show nil)(setq tab-bar-tab-hints nil)(setq tab-bar-tab-name-function 'tab-bar-tab-name-all)(tab-bar-mode -1)(tab-bar-history-mode -1)(defun meain/switch-tab-dwim ()
																																															      "Switch between available tabs"
																																															      (interactive)
																																															      (let ((tabs (mapcar (lambda (tab)
																																																		    (alist-get 'name tab))
																																																		  (tab-bar--tabs-recent))))
																																																(cond
																																																 ((eq tabs nil)
																																																  (tab-new))
																																																 ((eq (length tabs) 1)
																																																  (tab-next))
																																																 (t (ivy-read "Select tab: " tabs :require t
																																																	      :action 'tab-bar-switch-to-tab))))):init
  (evil-leader/set-key "t" 'meain/switch-tab-dwim))

;; Better modeline
(defun simple-mode-line-render (left right)
  "Return a string of `window-width' length containing LEFT, and RIGHT aligned respectively."
  (let* ((available-width (- (window-width)
			     (length left)
			     2)))
    (format (format " %%s %%%ds " available-width)
	    left
	    right)))
(setq-default mode-line-format (list '(:eval (if (eq 'emacs evil-state)
						 "! "
					       ": ")) ;; vim or emacs mode

				     ;; the buffer name; the file name as a tool tip
				     (propertize "%b"
						 'face
						 'font-lock-type-face
						 'help-echo
						 (buffer-file-name))
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
									       ,(+ 3
										   (+ (string-width (format-mode-line "%l"))
										      (string-width mode-name))))))))
				     (propertize "%l" 'face 'font-lock-constant-face) ;; position in file
				     (propertize " %m " 'face 'font-lock-string-face) ;; current mode
				     ))

;; drop gc threshold back
(setq gc-cons-threshold 800000)

(provide '.emacs)
;;; .emacs ends here
