;;; init -- meain's Emacs config

;;; Commentary:
;; Well, this is a vimmer's Emacs config.  Nothing fancy though.

;;; Code:

;; native comp stuff
(setq comp-speed 2)

;; increase gc threshold (speeds up initial load)
(setq gc-cons-threshold (* 100 1000 1000))

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
;; (setq use-package-always-demand (daemonp)) ;; eager load if daemon

;; Setup quelpa
(use-package quelpa :ensure t
  :defer t)


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
    (defalias #'forward-evil-word #'forward-evil-symbol)
    (evil-set-command-property 'evil-visual-char
                               :jump t)
    (evil-set-command-property 'evil-visual-line
                               :jump t)))

;; Evil leader
(use-package evil-leader
  :ensure t
  :config (progn
            (global-evil-leader-mode)
            (evil-leader/set-leader "s")))

(use-package evil-collection
  :ensure t
  :after evil
  :config (evil-collection-init))

;;; [BASIC SETTINGS] =============================================

;; Consistent window title
(setq frame-title-format '("Emacs")) ; needed by hammerspoon

;; Scratch initial content
(setq initial-scratch-message "; scratch")

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

;; Tab settings
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

;;; [VISUAL CONFIG] ==============================================

;; Disable useless stuff
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(setq inhibit-startup-screen t)
;; (set-frame-parameter nil 'undecorated t)

;; Font (set-frame-font  "Fairfax HD 16")
(setq font-family-default "Fairfax HD 16")
(add-to-list 'default-frame-alist
             `(font . ,font-family-default))
(set-face-attribute 'default nil :font font-family-default)
(set-face-attribute 'fixed-pitch nil :font font-family-default)
(set-face-attribute 'variable-pitch nil :font font-family-default)
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
(setq auto-revert-interval 2)
(setq auto-revert-check-vc-info t)
(setq global-auto-revert-non-file-buffers
      t)
(setq auto-revert-verbose t)

;; Disable line wrapping
(setq-default truncate-lines 1)
(evil-leader/set-key "b W" 'toggle-truncate-lines)

;; Cursor blink
(blink-cursor-mode -1)

;; Follow symlinks for vc
(setq vc-follow-symlinks t)

;; auto-pair
(electric-pair-mode t)

;; eldoc load
(require 'eldoc)
(global-eldoc-mode nil)

;; dired
(use-package dired
  :hook (dired-mode . dired-hide-details-mode):config
  (progn
    (setq delete-by-moving-to-trash t)
    (setq trash-directory "~/.Trash")
    (setq dired-listing-switches "-AGFhlgo")
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

;; Window mappings
(global-set-key (kbd "M-l")
                (defun meain/move-swap-right (&optional swap)
                  (interactive "P")
                  (evil-force-normal-state)
                  (if swap
                      (windmove-swap-states-right)
                    (windmove-right))))
(global-set-key (kbd "M-h")
                (defun meain/move-swap-left (&optional swap)
                  (interactive "P")
                  (evil-force-normal-state)
                  (if swap
                      (windmove-swap-states-left)
                    (windmove-left))))
(global-set-key (kbd "M-k")
                (defun meain/move-swap-up (&optional swap)
                  (interactive "P")
                  (evil-force-normal-state)
                  (if swap
                      (windmove-swap-states-up)
                    (windmove-up))))
(global-set-key (kbd "M-j")
                (defun meain/move-swap-down (&optional swap)
                  (interactive "P")
                  (evil-force-normal-state)
                  (if swap
                      (windmove-swap-states-down)
                    (windmove-down))))
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
     ((member major-mode '(imenu-list-major-mode magit-mode))
      (evil-quit))
     ((equal major-mode 'vterm-mode)
      (progn
        (evil-insert 1)
        (vterm-reset-cursor-point)))
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

;; Highlight yanked region
(defun meain/evil-yank-advice (orig-fn beg end &rest args)
  "Advice to be added to `evil-yank' to highlight yanked region.  Pass ORIG-FN, BEG, END, TYPE, ARGS."
  (pulse-momentary-highlight-region beg end
                                    'company-template-field)
  (apply orig-fn beg end args))
(advice-add 'evil-yank :around 'meain/evil-yank-advice)

;; Recompile binding
(defun meain/recompile-or-compile (&optional arg)
  "Compile or recompile based on universal `ARG'."
  (interactive "P")
  (if arg
      (call-interactively 'compile)
    (recompile)))
(evil-leader/set-key "r" 'meain/recompile-or-compile)

;;; [OTHER PACKAGES] =============================================

;; abbrev mode
(use-package abbrev
  :init (setq-default abbrev-mode t))

;; flymake
(use-package flymake
  :defer 1
  :config (progn
            (add-hook 'find-file-hook 'flymake-find-file-hook)
            (evil-set-command-property 'flymake-goto-next-error
                                       :jump t)
            (evil-set-command-property 'flymake-goto-prev-error
                                       :jump t)))
(use-package flymake-diagnostic-at-point
  :ensure t
  :after flymake
  :init (progn
          (setq flymake-diagnostic-at-point-error-prefix
                "! ")
          (setq flymake-diagnostic-at-point-display-diagnostic-function
                'flymake-diagnostic-at-point-display-minibuffer)
          (evil-leader/set-key "j" 'flymake-goto-next-error)
          (evil-leader/set-key "k" 'flymake-goto-prev-error)):config
  (add-hook 'flymake-mode-hook #'flymake-diagnostic-at-point-mode))

;; Company for autocompletions
(use-package company
  :ensure t
  :diminish :init
  (global-company-mode)
  :config (progn
            (setq company-dabbrev-downcase nil) ;; Do not lowercase my completions
            (setq company-idle-delay 0)
            (setq company-tooltip-maximum-width 35)
            (setq company-tooltip-align-annotations t)
            (setq company-minimum-prefix-length 2)))

;; Company quickhelp
(use-package company-quickhelp ; Show help in tooltip
  :ensure t
  :defer t
  :after company
  :init (company-quickhelp-mode))

;; Ivy && Counsel
(use-package counsel :ensure t)
(use-package flx :ensure t) ;; ivy--regex-plus uses this
(use-package ivy
  :after flx
  :ensure t
  :diminish t
  :config (progn
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
            (evil-leader/set-key "R" 'counsel-recentf)
            (global-set-key (kbd "M-x")
                            'counsel-M-x)
            (evil-leader/set-key "l" 'counsel-M-x)
            (evil-leader/set-key "h f" 'counsel-describe-function)
            (evil-leader/set-key "h v" 'counsel-describe-variable)
            (evil-leader/set-key "h o" 'counsel-describe-symbol)
            (evil-leader/set-key "h l" 'counsel-find-library)
            (evil-leader/set-key "h i" 'counsel-info-lookup-symbol)
            (define-key evil-normal-state-map (kbd "_") 'dired-jump)
            (define-key evil-normal-state-map (kbd "-") 'counsel-find-file)
            (define-key ivy-minibuffer-map (kbd "C-j") 'ivy-immediate-done)))

;; ivy-rich
(use-package ivy-rich
  :ensure t
  :config (ivy-rich-mode))

;; amx (better mx)
(use-package amx
  :ensure t
  :init (amx-mode))

;; Helpful package
(use-package helpful
  :ensure t
  :commands (helpful-callable helpful-variable helpful-at-point
                              helpful-key):init
  (progn
    (evil-leader/set-key "h p" 'helpful-at-point)
    (evil-leader/set-key "h k" 'helpful-key)
    (setq counsel-describe-function-function #'helpful-callable)
    (setq counsel-describe-variable-function #'helpful-variable)))

;; ibuffer
(use-package ibuffer
  :init (setq ibuffer-expert t))

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
          (setq counsel-rg-base-command '("rg" "-M" "240" "--with-filename" "--no-heading"
                                          "--line-number" "--color" "never" "--hidden"
                                          "--follow" "--glob" "!.git/*" "%s"))
          (setq rg-command-line-flags '("--hidden" "--follow"))
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
  :config (require 'srefactor-lisp))
(use-package format-all
  :commands format-all-buffer
  :ensure t
  :init (progn
          (defun meain/auto-format ()
            "Custom auto-format based on filetype."
            (interactive)
            (if (eq major-mode 'emacs-lisp-mode)
                (srefactor-lisp-format-buffer)
              (call-interactively 'format-all-buffer)))
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
         (rust-mode . eglot-ensure)
         (js-mod . eglot-ensure)
         (lua-mode . eglot-ensure)
         (go-mode . eglot-ensure)):config
  (progn
    (add-to-list 'eglot-server-programs
                 '(lua-mode . ("lua-lsp")))
    (add-to-list 'eglot-server-programs
                 '(rust-mode . ("rust-analyzer")))
    (add-to-list 'eglot-server-programs
                 '(python-mode . ("~/.bin/vpyls")))
    (add-hook 'eglot-managed-mode-hook
              (lambda ()
                (eldoc-mode -1)))
    (define-key evil-normal-state-map (kbd "K") 'eldoc-print-current-symbol-info)
    (define-key evil-normal-state-map (kbd "g k") 'eldoc-print-current-symbol-info)
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
  :diminish :config
  (which-key-mode))

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
  :diminish :config
  (dtrt-indent-global-mode))

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

;; vterm setup
(use-package vterm
  :ensure t
  :defer t
  :init (progn
          (setq vterm-max-scrollback 100000)
          (setq vterm-kill-buffer-on-exit t)
          (global-set-key (kbd "M-;")
                          'meain/shell-toggle)
          (defun meain/shell-name ()
            "Get the name of the shell based on project info."
            (format "*popup-shell-%s*"
                    (if (projectile-project-p)
                        (projectile-project-name)
                      "-")))
          (defun meain/shell-toggle (&optional rerun-previous)
            "Create/toggle shell for current project."
            (interactive "P")
            (let ((shell-buffers (remove-if-not (lambda (x)
                                                  (s-starts-with-p (meain/shell-name)
                                                                   (buffer-name x)))
                                                (buffer-list))))
              (cond
               ((equal major-mode 'vterm-mode)
                (progn
                  (if rerun-previous
                      (progn
                        (vterm-clear)
                        (vterm-clear-scrollback))
                    (delete-window))))
               ((equal (length shell-buffers) 0)
                (meain/shell-new))
               (t (progn
                    (pop-to-buffer (car shell-buffers))
                    (if rerun-previous
                        (progn
                          (vterm-clear)
                          (vterm-clear-scrollback)
                          (vterm-send-up)
                          (vterm-send-return))))))))
          (defun meain/shell-new ()
            "Create a new shell for the current project."
            (interactive)
            (if (equal major-mode 'vterm-mode)
                (delete-window))
            (setq default-directory (cond
                                     ((projectile-project-p)
                                      (projectile-project-root))
                                     (t "~/")))
            (vterm (meain/shell-name)))
          (defun meain/shell-other (&optional alternate)
            "Switch to previous shell in current project. Use ALTERNATE to get a list of shell in current project."
            (interactive "P")
            (let ((shell-buffers (remove-if-not (lambda (x)
                                                  (s-starts-with-p (meain/shell-name)
                                                                   (buffer-name x)))
                                                (buffer-list))))
              (cond
               ((equal (length shell-buffers) 0)
                (message "No shells bruh!"))
               ((equal (length shell-buffers) 1)
                (message "Only one shell"))
               (alternate (ivy-read "Choose shell: "
                                    (mapcar (lambda (x)
                                              (buffer-name x))
                                            shell-buffers)
                                    :action (lambda (x)
                                              (switch-to-buffer x))))
               (t (switch-to-buffer (car (cdr shell-buffers)))))))
          (defun meain/run-in-vterm-kill (process event)
            "A process sentinel. Kills PROCESS's buffer if it is live."
            (let ((b (process-buffer process)))
              (and (buffer-live-p b)
                   (kill-buffer b)
                   (delete-window))))
          (defun meain/run-in-vterm (command)
            "Execute string COMMAND in a new vterm and kill the shell once done.  Useful for interactive items."
            (interactive (list (let* ((f (cond
                                          (buffer-file-name)
                                          ((eq major-mode 'dired-mode)
                                           (dired-get-filename nil t))))
                                      (filename (concat " "
                                                        (shell-quote-argument (and f
                                                                                   (file-relative-name f))))))
                                 (read-shell-command "Terminal command: "
                                                     (cons filename 0)
                                                     (cons 'shell-command-history 1)
                                                     (list filename)))))
            (with-current-buffer (vterm (concat "*popup-shell-" command "*"))
              (set-process-sentinel vterm--process #'meain/run-in-vterm-kill)
              (vterm-send-string (concatenate 'string command ";exit 0"))
              (vterm-send-return)))):config
  (progn
    (evil-set-initial-state 'vterm-mode 'insert)
    (define-key vterm-mode-map (kbd "M-c") 'meain/shell-new)
    (define-key vterm-mode-map (kbd "M-m") 'meain/shell-other)
    (define-key vterm-mode-map (kbd "M-w") 'delete-window)
    (define-key vterm-mode-map (kbd "M-u") 'universal-argument)
    (define-key vterm-mode-map (kbd "M-k") 'previous-window-any-frame)
    (add-to-list 'display-buffer-alist
                 '((lambda (bufname _)
                     (s-starts-with-p "*popup-shell" bufname))
                   (display-buffer-reuse-window display-buffer-at-bottom)
                   (reusable-frames . visible)
                   (window-height . 0.8)))))

;; ranger in emacs
(use-package ranger
  :ensure t
  :commands ranger
  :config (use-package image-dired+
            :ensure t
            :config (image-diredx-async-mode)))

;; outline mode settings (code folding)
(use-package outline
  :diminish :config
  (progn
    (set-display-table-slot standard-display-table
                            'selective-display
                            (string-to-vector " ..."))
    (add-hook 'prog-mode-hook 'outline-minor-mode)
    (evil-leader/set-key "d" 'outline-toggle-children)))

;; editorconfig
(use-package editorconfig
  :ensure t
  :config (editorconfig-mode 1))

;; eros for eval
(use-package eros
  :ensure t
  :commands (eros-eval-last-sexp):init
  (progn
    (defun meain/eval-last-sexp (&optional alternate)
      "Do `eval-last-sexp'.  Pass ALTERNATE to go to end of line and do the same."
      (interactive "P")
      (if alternate
          (save-excursion
            (end-of-line)
            (eros-eval-last-sexp nil))
        (save-excursion
          (search-forward ")")
          (eros-eval-last-sexp nil))))
    (evil-leader/set-key ";" 'meain/eval-last-sexp))
  :config (eros-mode))

;; Virtualenv
(use-package virtualenvwrapper
  :ensure t
  :command venv-workon
  :init (setq venv-location "~/.cache/virtual_envs"))

;; Quick run current test
(defun meain/test-runner-full ()
  "Run the full test suite using toffee."
  (interactive)
  (compile (shell-command-to-string (format "toffee --full '%s' || exit 1'"
                                            (buffer-file-name)))))
(defun meain/test-runner (&optional full-file)
  "Run the nearest test using toffee.  Pass `FULL-FILE' to run all test in file."
  (interactive "P")
  (compile (shell-command-to-string (if full-file
                                        (format "toffee '%s' || exit 1'"
                                                (buffer-file-name))
                                      (format "toffee '%s' '%s' || exit 1'"
                                              (buffer-file-name)
                                              (line-number-at-pos))))))
(evil-leader/set-key "d" 'meain/test-runner)
(evil-leader/set-key "D" 'meain/test-runner-full)

;;; [FILETYPE PUGINS] ===============================================

(use-package rust-mode :ensure t
  :defer t)
(use-package go-mode :ensure t
  :defer t)
(use-package lua-mode :ensure t
  :defer t)
(use-package jinja2-mode :ensure t
  :defer t)
(use-package json-mode :ensure t
  :defer t)
(use-package config-general-mode :ensure t
  :defer t)  ;; config files
(use-package vimrc-mode :ensure t
  :defer t)
(use-package markdown-mode
  :ensure t
  :defer t
  :mode ("\\.md\\'" . gfm-mode):config
  (progn
    (setq markdown-enable-html -1)
    (setq markdown-command "pandoc -t html5")
    (setq markdown-fontify-code-blocks-natively
          t)))
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
(use-package dockerfile-mode :ensure t
  :defer t)
(use-package docker-compose-mode :ensure t
  :defer t)
(use-package org
  :defer t
  :init (progn
          (evil-define-key 'normal
            org-mode-map
            (kbd "M-l")
            (lambda ()
              (interactive)
              (evil-force-normal-state)
              (evil-window-right 1)))
          (evil-define-key 'normal
            org-mode-map
            (kbd "M-h")
            (lambda ()
              (interactive)
              (evil-force-normal-state)
              (evil-window-left 1)))
          (evil-define-key 'normal
            org-mode-map
            (kbd "M-k")
            (lambda ()
              (interactive)
              (evil-force-normal-state)
              (evil-window-up 1)))
          (evil-define-key 'normal
            org-mode-map
            (kbd "M-j")
            (lambda ()
              (interactive)
              (evil-force-normal-state)
              (evil-window-down 1)))
          (evil-define-key 'normal
            org-mode-map
            (kbd "gk")
            'org-backward-heading-same-level)
          (evil-define-key 'normal
            org-mode-map
            (kbd "gj")
            'org-forward-heading-same-level)
          (evil-define-key 'normal
            org-mode-map
            (kbd "gK")
            'org-move-subtree-up)
          (evil-define-key 'normal
            org-mode-map
            (kbd "gJ")
            'org-move-subtree-down)
          (evil-define-key 'normal
            org-mode-map
            (kbd "gL")
            'org-promote-subtree)
          (evil-define-key 'normal
            org-mode-map
            (kbd "gH")
            'org-demote-subtree)
          (evil-define-key 'normal
            org-mode-map
            (kbd "gt")
            'org-todo)
          (evil-define-key 'normal
            org-mode-map
            (kbd "gr")
            'org-ctrl-c-ctrl-c)))

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
          (evil-define-key 'normal
            notmuch-show-mode-map
            (kbd "U")
            'notmuch-show-browse-urls)
          (evil-define-key 'normal
            notmuch-show-mode-map
            (kbd "u")
            'meain/notmuch-show-close-all-but-unread)
          (evil-define-key 'normal
            notmuch-show-mode-map
            (kbd "M-k")
            (lambda ()
              (interactive)
              (evil-force-normal-state)
              (evil-window-up 1)))
          (evil-define-key 'normal
            notmuch-show-mode-map
            (kbd "M-j")
            (lambda ()
              (interactive)
              (evil-force-normal-state)
              (evil-window-down 1)))
          (evil-leader/set-key "a n" 'notmuch)
          (defun meain/notmuch-show-close-all-but-unread ()
            "Close all messages until the first unread item."
            (interactive)
            (goto-char (point-min))
            (cl-loop do
                     (notmuch-show-message-visible (notmuch-show-get-message-properties)
                                                   nil)
                     until
                     (or (not (notmuch-show-goto-message-next))
                         (member "unread" (plist-get (notmuch-show-get-message-properties)
                                                     :tags))))
            (force-window-update))
          ;; (add-hook 'notmuch-show-mode-hook 'meain/notmuch-show-close-all-but-unread)
          (setq notmuch-search-oldest-first nil)
          (setq notmuch-message-headers-visible nil)
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
                                               (:name "unread" :query "tag:unread AND tag:inbox AND -tag:python AND -tag:newsletter"
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

;; sending emails
(setq message-kill-buffer-on-exit t) ; kill buffer after sending mail
(setq mail-specify-envelope-from t) ; Settings to work with msmtp
(setq message-sendmail-envelope-from 'header)
(setq mail-envelope-from 'header)
(setq notmuch-fcc-dirs "[Gmail].Sent Mail") ; stores sent mail to the specified directory
(setq message-directory "[Gmail].Drafts") ; stores postponed messages to the specified directory
(setq sendmail-program "msmtp")
(setq send-mail-function 'smtpmail-send-it)
(setq message-sendmail-f-is-evil t)
(setq message-sendmail-extra-arguments '("--read-envelope-from"))
(setq message-send-mail-function 'message-send-mail-with-sendmail)

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
            (setq browse-url-generic-program "open")
            (setq browse-url-generic-args (list "-g"))
            (evil-define-key 'normal
              elfeed-search-mode-map
              (kbd "o")
              (lambda (&optional alternate)
                (interactive)
                (elfeed-search-browse-url t)))
            (evil-define-key 'normal
              elfeed-search-mode-map
              (kbd "O")
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
              (setq elfeed-search-filter "@2-months-ago +unread")
              (elfeed-search-update :force)
              (ivy-read "Apply tag: "
                        (remove-if (lambda (x)
                                     (equalp x 'unread))
                                   (delete-dups (flatten-list (cl-list* (with-current-buffer "*elfeed-search*"
                                                                          (cl-loop for
                                                                                   entry
                                                                                   in
                                                                                   elfeed-search-entries
                                                                                   collect
                                                                                   (elfeed-entry-tags entry)))))))
                        :action (lambda (x)
                                  (setq elfeed-search-filter (concatenate 'string "@2-months-ago +unread +"
                                                                          x))
                                  (elfeed-search-update :force)
                                  (evil-goto-first-line))))
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
  :diminish :init
  (beacon-mode t))

;; Writing mode
(use-package writeroom-mode :ensure t
  :commands writeroom-mode)

;; tramp dired
(use-package tramp
  :defer t
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

;; wakatime
(use-package wakatime-mode
  :ensure t
  :diminish :config
  (global-wakatime-mode))

;; Markdown preivew
(use-package simple-httpd
  :ensure t
  :config (setq httpd-port 7070)(setq httpd-host "localhost"))
(use-package impatient-mode :ensure t
  :commands impatient-mode)
(defun meain/markdown-filter (buffer)
  "Tempate for making pandoc output of BUFFER to markdown page."
  (princ (with-temp-buffer
           (let ((tmp (buffer-name)))
             (set-buffer buffer)
             (set-buffer (markdown tmp))
             (format "<!DOCTYPE html><html><title>Markdown preview</title><link rel=\"stylesheet\" href = \"https://cdnjs.cloudflare.com/ajax/libs/github-markdown-css/3.0.1/github-markdown.min.css\"/>
<body><article class=\"markdown-body\" style=\"box-sizing: border-box;min-width: 200px;max-width: 980px;margin: 0 auto;padding: 45px;\">%s</article></body></html>"
                     (buffer-string))))
         (current-buffer)))
(defun meain/markdown-preview ()
  "Preview markdown file in browser."
  (interactive)
  (unless (process-status "httpd")
    (httpd-start))
  (impatient-mode)
  (imp-set-user-filter 'meain/markdown-filter)
  (imp-visit-buffer))


;; Persistent scratch
;; need defer to not mess with statusline update
(use-package persistent-scratch
  :ensure t
  :defer 1
  :config (progn
            (persistent-scratch-setup-default)
            (persistent-scratch-autosave-mode 1)))

;; Git info in dired buffer
(use-package dired-git-info
  :ensure t
  :commands (dired-git-info-mode))

;; Restclient
(use-package restclient
  :ensure t
  :defer t
  :mode (("\\.rest\\'" . restclient-mode)))


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

;; Quick open today file
(evil-leader/set-key "a t"
  (defun meain/load-today ()
    "Load emacs config for editing."
    (interactive)
    (find-file "~/.cache/today.org")))

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
(global-set-key (kbd "M-m")
                'meain/monacle-mode)
(evil-leader/set-key "b m" 'meain/monacle-mode)

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
    (progn
      (find-file (concat "~/.cache/vime/_"
                         (substring (uuid-string)
                                    0
                                    4)))
      (insert ":name ")
      (evil-insert 1))))
(evil-leader/set-key "v" 'meain/vime)

;; Open note
(defun meain/nested-list-dir (directory)
  "List items two level deep in DIRECTORY."
  (apply 'concatenate
         'list
         (mapcar (lambda (x)
                   (mapcar (lambda (y)
                             (concatenate 'string
                                          (car x)
                                          "/"
                                          (car y)))
                           (remove-if #'(lambda (x)
                                          (or (eq (nth 1 x) t)
                                              (equal (substring (nth 0 x)
                                                                0
                                                                1) ".")))
                                      (directory-files-and-attributes (concatenate 'string
                                                                                   directory
                                                                                   "/"
                                                                                   (car x))))))
                 (remove-if #'(lambda (x)
                                (or (eq (nth 1 x) nil)
                                    (equal (substring (nth 0 x)
                                                      0
                                                      1) ".")
                                    (equal (nth 0 x) "archive")
                                    (equal (nth 0 x) "temp")))
                            (directory-files-and-attributes directory)))))

(defun meain/open-note ()
  "Quick open a note from `.notes` directory."
  (interactive)
  (ivy-read "Choose note: "
            (meain/nested-list-dir "~/.notes")
            :require-match t
            :action (lambda (x)
                      (find-file (concatenate 'string "~/.notes/" x)))))
(evil-leader/set-key "a N" 'meain/open-note)

;; dasht docs
(defun meain/dasht-docs (start end)
  "Look up word at point in dasht.
START and END comes from it being interactive."
  (interactive "r")
  (let ((thing (if (use-region-p)
                   (buffer-substring start end)
                 (thing-at-point 'symbol))))
    (if (eq (length thing) 0)
        (message "Nothing to look up.")
      (progn
        (let ((lookup-term (read-from-minibuffer "Lookup term: " thing)))
          (ivy-read "Docset: "
                    (split-string (shell-command-to-string "dasht-docsets"))
                    :require-match t
                    :preselect (car (split-string (message "%s" major-mode)
                                                  "-")):action
                    (lambda (ds)
                      (message "Looking up %s in %s" lookup-term
                               ds)
                      (meain/run-in-vterm (concatenate 'string "docs " lookup-term " "
                                                       ds)))))))))
(evil-leader/set-key "a d" 'meain/dasht-docs)

;; Quick edit (for use with hammerspoon quick edit)
(defun meain/quick-edit ()
  "Util function for use with hammerspoon quick edit functionality."
  (interactive)
  (let ((qed-buffer-name (concatenate 'string
                                      "qed-"
                                      (substring (uuid-string)
                                                 0
                                                 4))))
    (generate-new-buffer qed-buffer-name)
    (switch-to-buffer qed-buffer-name)
    (evil-paste-after 1)
    (gfm-mode)))

;; vim-printer remake in elisp
(defun meain/quick-print (&optional print-above)
  "Quickly print the variable your cursor is under.  Pass PRINT-ABOVE to print above current line."
  (interactive "P")
  (let ((thing-to-print (symbol-at-point)))
    (if print-above
        (progn
          (beginning-of-line)
          (newline-and-indent)
          (previous-line)
          (indent-relative))
      (progn
        (end-of-line)
        (newline-and-indent)))
    (insert (cond
             ((equal major-mode 'emacs-lisp-mode)
              (format "(message \"%s: %%s\" %s)" thing-to-print
                      thing-to-print thing-to-print))
             ((equal major-mode 'rust-mode)
              (format "println!(\"%s: {:?}\", %s);" thing-to-print
                      thing-to-print))
             ((equal major-mode 'go-mode)
              (format "fmt.Println(\"%s:\", %s);" thing-to-print
                      thing-to-print))
             ((equal major-mode 'lua-mode)
              (format "print(\"%s:\", %s)" thing-to-print
                      thing-to-print))
             ((equal major-mode 'js-mode)
              (format "console.log(\"%s:\", %s)" thing-to-print
                      thing-to-print))
             ((equal major-mode 'shell-script-mode)
              (format "echo \"%s:\" %s" thing-to-print thing-to-print))
             ((equal major-mode 'python-mode)
              (format "print(\"%s:\", %s)" thing-to-print
                      thing-to-print))))))
(define-key evil-normal-state-map (kbd "g p") 'meain/quick-print)

;; More shell "apps"
(evil-leader/set-key "a H"
  (lambda ()
    (interactive)
    (meain/run-in-vterm "htop")))
(evil-leader/set-key "a J"
  (lambda ()
    "Start writing journal entry.  `journal' invokes emacsclient and gives control back over to Emacs."
    (interactive)
    (start-process-shell-command "journal" "*journal*"
                                 "journal")))

;; Function to close Emacs in a more "proper" way
(defun meain/kill-all-buffers ()
  "Kill all active buffers."
  (interactive)
  (mapcar 'kill-buffer
          (buffer-list))
  (delete-other-windows))

;; Narrow region
(defun meain/narrow-region-dwim ()
  "Narrow or widen the region (dwim)."
  (interactive)
  (if (eq evil-state 'visual)
      (call-interactively 'narrow-to-region)
    (call-interactively 'widen)))
(global-set-key (kbd "M-N")
                'meain/narrow-region-dwim)

;; Buffer/Frame/Window keybinds
(evil-leader/set-key "b k" 'kill-buffer)
(evil-leader/set-key "b o" 'previous-buffer)
(evil-leader/set-key "b f" 'find-file)
(evil-leader/set-key "s" 'server-edit)
(evil-leader/set-key "b d" 'delete-frame)

;; Bookmarks
(setq bookmark-save-flag 1)
(evil-leader/set-key "m" 'bookmark-jump)
(evil-leader/set-key "M" 'bookmark-set)

;; Meta-f keybinds
(global-unset-key (kbd "M-f")) ; have to unset first
(global-set-key (kbd "M-f M-b")
                'meain/buffer-switcher)

;; setting proper default-dir
(defun meain/set-proper-default-dir ()
  "Function to set the `default-directory' value as the project root if available."
  (interactive)
  (setq default-directory (cond
                           ((projectile-project-p)
                            (projectile-project-root))
                           (t "~/"))))
(add-hook 'find-file-hook 'meain/set-proper-default-dir)

;; Open current file in Github
(defun meain/github-url (&optional no-linenumber)
  "Open the Github page for the current file.  Pass NO-LINENUMBER to not add a line number."
  (interactive "P")
  (let* ((git-url (replace-regexp-in-string "\.git$"
                                            ""
                                            (string-replace "git@github.com:"
                                                            "https://github.com/"
                                                            (car (split-string (shell-command-to-string "git config --get remote.origin.url")
                                                                               "\n")))))
         (git-branch (car (split-string (shell-command-to-string "git rev-parse --abbrev-ref HEAD")
                                        "\n")))
         (web-url (format "%s/blob/%s/%s%s"
                          git-url
                          git-branch
                          (file-relative-name buffer-file-name
                                              (projectile-project-root))
                          (if no-linenumber
                              ""
                            (format "#L%s"
                                    (line-number-at-pos))))))
    (progn
      (message "%s coped to clipboard." web-url)
      (start-process-shell-command "pbcopy-gh"
                                   "*pbcopy-gh*"
                                   (format "echo '%s'|pbcopy" web-url))
      (start-process-shell-command "gh"
                                   "*gh*"
                                   (format "open '%s'" web-url)))))
(evil-leader/set-key "b G" 'meain/github-url)

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
                                                               (if (buffer-modified-p)
                                                                   'font-lock-builtin-face
                                                                 'font-lock-type-face)
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

;; Start emacs server
(server-start)

;; drop gc threshold back
(setq gc-cons-threshold 800000)

(provide 'init)
;;; init ends here
