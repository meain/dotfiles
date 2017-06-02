;;; .emacs --- my personal emacs configuration file

;;; Commentary:

;; This is a vim user's Emacs configuration and I warn you that its fully littered with Vim stuff.

;;; Code:

;; Set font
(set-frame-font "Monaco 14")

;; Remove unnecessary stuff
(when (display-graphic-p)
  (scroll-bar-mode -1)
  (tool-bar-mode -1))
(menu-bar-mode -1)

;; (setq initial-scratch-message "   : meain")
(setq initial-major-mode 'markdown-mode)
(defun immortal-scratch ()
  "Make scratch buffer immortal."
  (if (eq (current-buffer) (get-buffer "*scratch*"))
    (progn (bury-buffer)
           nil)
    t))
(add-hook 'kill-buffer-query-functions 'immortal-scratch)

;; disable backup
(setq backup-inhibited t)

;; No backup files
(setq make-backup-files nil)

;; disable auto save
(setq auto-save-default nil)

;; no line wrapping
(setq-default truncate-lines 1)

;; line highlight
(global-hl-line-mode 1)

;; change all prompts to y or n
(fset 'yes-or-no-p 'y-or-n-p)

;; set default tab width
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

;; set up package
(require 'package)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("elpy" . "https://jorgenschaefer.github.io/packages/"))

(setq package-enable-at-startup nil)
(package-initialize)
;; Handle installing packages in emacs
(defun ensure-package-installed (&rest packages)
  "Assure every package is installed, ask for installation if it’s not.
Return a list of installed PACKAGES or nil for every skipped package."
  (mapcar
    (lambda (package)
      (if (package-installed-p package)
        nil
        (package-install package)))
    packages))

;; make sure to have downloaded archive description.
(or (file-exists-p package-user-dir)
    (package-refresh-contents))

;; activate installed packages
(package-initialize)

(ensure-package-installed 'evil
                          'projectile
                          'magit
                          'ag
                          'helm
                          'sane-term
                          'web-mode
                          'evil-leader
                          'smooth-scrolling
                          'flycheck
                          'which-key
                          'autopair
                          'neotree
                          'saveplace
                          'ido
                          'auto-complete
                          'evil-commentary
                          'yasnippet
                          'evil-surround
                          'evil-search-highlight-persist
                          'git-gutter
                          'drag-stuff
                          'osx-clipboard
                          'evil-surround
                          'gruvbox-theme
                          'spaceline
                          'diff-hl
                          'try
                          'persistent-scratch
                          'fiplr
                          'neotree
                          'minimap
                          'rainbow-mode
                          'labburn-theme
                          'jazz-theme
                          'rainbow-delimiters
                          'elpy)

;; fancy start screen
(require 'dashboard)
(dashboard-setup-startup-hook)
(setq dashboard-banner-logo-title ":meain")
(setq dashboard-startup-banner 'logo)
(setq dashboard-items '((recents  . 10)
                        (bookmarks . 5)
                        (projects . 5)
                        (agenda . 5)))

;; Evil mode
(require 'evil)
(evil-mode t)

;; enable elpy, yeah its mostly python here
(require 'elpy)
(elpy-enable)

;; Theme
(if (display-graphic-p)
(load-theme 'jazz t) (load-theme 'gruvbox t))

;; enable which-key
(require 'which-key)
(which-key-mode t)

;; hmm, we should prpbably have line numbers
;; flydiff does not work pretty without it
(global-linum-mode t)

;; hook into osx clipboard
(require 'osx-clipboard)
(osx-clipboard-mode)

;; Set up leader key in emacs
(require 'evil-leader)
(global-evil-leader-mode)
(evil-leader/set-leader "<SPC>")

;; Vim like search hilighting
(require 'evil-search-highlight-persist)
(global-evil-search-highlight-persist t)
(evil-leader/set-key "/" 'evil-search-highlight-persist-remove-all)

;; Use leader<space> to save file
(evil-leader/set-key "SPC" 'save-buffer)

;; Easer opening of M-x
(evil-leader/set-key "l" 'helm-M-x)

;; Magit call
(evil-leader/set-key "g" 'magit-status)

;; buffer list easy open
(evil-leader/set-key "b" 'helm-buffers-list)

;; shortcut to save current buffer
(evil-leader/set-key "w" 'evil-save)

;; easy quitting of buffer/window
(evil-leader/set-key "q" 'evil-delete-buffer)

;; switch to scratch buffer
(evil-leader/set-key "s" (lambda() (interactive) (switch-to-buffer "*scratch*")))

;; switch to dashboard buffer
(evil-leader/set-key "d" (lambda() (interactive) (switch-to-buffer "*dashboard*")))

;; quick switch to previous buffer
(evil-leader/set-key "f" 'switch-to-prev-buffer)

;; switch projects
(evil-leader/set-key "p" 'projectile-switch-project)

;; start shell with leader t
(require 'sane-term)
(evil-leader/set-key "t" 'sane-term)
(add-hook 'term-mode-hook (lambda () (linum-mode -1)))

;; tag jumping
(evil-leader/set-key ";" 'evil-jump-to-tag)
(evil-leader/set-key "'" 'evil-jump-backward)

;; vinegarish
(define-key evil-normal-state-map (kbd "-") 'helm-find-files)

;; Set up helm
(require 'helm-config)
(helm-mode 1)
(global-set-key (kbd "M-x") 'helm-M-x)
;; make everything fuzzy
(setq helm-M-x-fuzzy-match t)
(setq helm-buffers-fuzzy-matching t)
(setq helm-recentf-fuzzy-match t)

;; Smoother scrolling
(require 'smooth-scrolling)
(smooth-scrolling-mode 1)

;; Powerline
(require 'spaceline)
(require 'spaceline-config)
(spaceline-spacemacs-theme)
;; (spaceline-all-the-icons-theme)
(spaceline-toggle-minor-modes-off)
(spaceline-toggle-evil-state)
(spaceline-toggle-hud-off)
(spaceline-toggle-buffer-size)
(setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
(setq powerline-height 20)
(setq powerline-raw " ")
(setq powerline-default-separator "zigzag")
(spaceline-compile)
(setq ns-use-srgb-colorspace nil)

;; j/k for wrapped lines
(define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)

;; other evil remaps
(evil-leader/set-key "q" `evil-quit)
(evil-leader/set-key "w" `evil-write)

;; flycheck
(require 'flycheck)
(global-flycheck-mode)

;; esc quits
(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
    (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
(global-set-key [escape] 'evil-exit-emacs-state)

;; Page up and down
(define-key evil-normal-state-map (kbd "9") (lambda ()
                                              (interactive)
                                              (evil-scroll-up nil)))
(define-key evil-normal-state-map (kbd "8") (lambda ()
                                              (interactive)
                                              (evil-scroll-down nil)))

;; Automateic indentation
(define-key global-map (kbd "RET") 'newline-and-indent)

;; opening recent files
(require 'recentf)
(require 'fiplr)

(defun recent-selector-open ()
  "Switch between helm and fiplr based on directory."
  (interactive)
  (if (string= (shell-command-to-string (concat "git -C " default-directory " rev-parse")) "") (fiplr-find-file) (helm-recentf)))
(define-key evil-normal-state-map (kbd ", ,") 'recent-selector-open)

;; helm-recentf-fuzzy-match var is broken: redeclare it manually
(setq helm-source-recentf
      (helm-make-source "Recentf" 'helm-recentf-source
                        :fuzzy-match t))

;; Autopair
(require 'autopair)
(autopair-global-mode)

;; Projectile
(require 'projectile)
(projectile-global-mode)

;; Save whitespace before save
(add-hook 'before-save-hook 'whitespace-cleanup)

;; Remember cursor positon
(require 'saveplace)
(save-place-mode t)
(setq save-place-file "~/.emacs.d/saveplace")

;; Autocompletion plugin
;; Enable snippets
(require 'auto-complete)
(global-auto-complete-mode t)
(ac-config-default)
(require 'yasnippet)
(yas-global-mode 1)
(add-to-list 'load-path "~/.emacs.d/plugins/yasnippet")

;; persistant scratch buffer - so cool
(persistent-scratch-setup-default)

;; Comment out stuff easily
(evil-commentary-mode)

;;Surround pllugin
(require 'evil-surround)
(global-evil-surround-mode 1)

;; undotree
(global-undo-tree-mode)
(setq undo-tree-auto-save-history t)
(setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))

;; Like drag visuals
(require 'drag-stuff)
(drag-stuff-mode t)
(drag-stuff-global-mode 1)
(drag-stuff-define-keys)

;; easier viewing of delimiters
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; Split like vim
(evil-leader/set-key "h" (lambda () (interactive) (split-window-below) (windmove-down)))
(evil-leader/set-key "v" (lambda () (interactive) (split-window-right) (windmove-right)))

;; Git gutter
(diff-hl-flydiff-mode t)
(global-diff-hl-mode t)

;; Easier buffer switching
(define-key evil-normal-state-map (kbd "M-h") `windmove-left)
(define-key evil-normal-state-map (kbd "M-j") `windmove-down)
(define-key evil-normal-state-map (kbd "M-k") `windmove-up)
(define-key evil-normal-state-map (kbd "M-l") `windmove-right)

;; remap ; to :
(define-key evil-normal-state-map (kbd ";") 'evil-ex)

;; get :<caps> to work
(evil-ex-define-cmd "Q" `kill-buffer-and-window)
(evil-ex-define-cmd "W" `save-buffer)
(evil-ex-define-cmd "WQ" (lambda () (interactive) (save-buffer) (kill-buffer-and-window)))
(evil-ex-define-cmd "Wq" (lambda () (interactive) (save-buffer) (kill-buffer-and-window)))

;; Disable the annoying bell
(setq ring-bell-function 'ignore)

;; Maximize current buffer
(defun toggle-maximize-buffer ()
  "Maximize the current buffer."
  (interactive)
  (if (= 1 (length (window-list)))
    (jump-to-register '_)
    (progn
      (window-configuration-to-register '_)
      (delete-other-windows))))
(define-key evil-normal-state-map (kbd "_") `toggle-maximize-buffer)

;; F'ing clipboard
(defun bb/evil-delete (orig-fn beg end &optional type _ &rest args)
  "Make d, c, x to not write to clipboard.  And ORIG-FN, BEG, END, TYPE, ARGS should appear in docstring."
  (apply orig-fn beg end type ?_ args))
(advice-add 'evil-delete :around 'bb/evil-delete)

;; Start maximized
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("3a5f04a517096b08b08ef39db6d12bd55c04ed3d43b344cf8bd855bde6d3a1ae" "d3a7eea7ebc9a82b42c47e49517f7a1454116487f6907cf2f5c2df4b09b50fc1" "47744f6c8133824bdd104acc4280dbed4b34b85faa05ac2600f716b0226fb3f6" "a1289424bbc0e9f9877aa2c9a03c7dfd2835ea51d8781a0bf9e2415101f70a7e" "99473228af8c280ed5534952a1a687732c2450d076528c6363ec23febccccd7b" "bc4b650c41b16b98166b35da94b366c6a9e1e7883bbf4937c897fb7bd05aa619" "83db918b06f0b1df1153f21c0d47250556c7ffb5b5e6906d21749f41737babb7" "e87a2bd5abc8448f8676365692e908b709b93f2d3869c42a4371223aab7d9cf8" default)))
 '(inhibit-startup-screen t)
 '(initial-frame-alist (quote ((fullscreen . maximized))))
 '(package-selected-packages
   (quote
    (jazz-theme dashboard spaceline-all-the-icons all-the-icons-dired all-the-icons try elscreen-multi-term sane-term nclip clippy clipmon osx-clipboard undohist web-mode term+ snippet vimrc-mode auctex-latexmk magic-latex-buffer init-open-recentf magit-find-file find-things-fast helm-fuzzy-find highlight-current-line rainbow-mode neotree linum-relative drag-stuff git-gutter evil-surround evil-commentary autopair simpleclip flycheck smooth-scrolling projectile magit helm gruvbox-theme evil-search-highlight-persist evil-leader auto-complete)))
 '(send-mail-function (quote smtpmail-send-it)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(trailing-whitespace ((t (:background "dim gray")))))

(provide '.emacs)
;;; .emacs ends here
