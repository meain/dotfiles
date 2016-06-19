;; Set font
(set-default-font "monaco 12")

(require 'package)

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))

(setq package-enable-at-startup nil)
(package-initialize)

;; Handle installing packages in emacs
(defun ensure-package-installed (&rest packages)
  "Assure every package is installed, ask for installation if it’s not.

Return a list of installed packages or nil for every skipped package."
  (mapcar
   (lambda (package)
     (if (package-installed-p package)
         nil
       (if (y-or-n-p (format "Package %s is missing. Install it? " package))
           (package-install package)
         package)))
   packages))

;; make sure to have downloaded archive description.
;; Or use package-archive-contents as suggested by Nicolas Dudebout
(or (file-exists-p package-user-dir)
    (package-refresh-contents))

;; activate installed packages
(package-initialize)

(ensure-package-installed 'evil
                          'projectile
                          'magit
			  'helm
			  `powerline
			  `powerline-evil)

;; Evil mode
(require 'evil)
(evil-mode t)

;; Theme
(load-theme 'misterioso t)

;; Set up leader key in emacs
(global-evil-leader-mode)
(evil-leader/set-leader "<SPC>")

;; Vim like search hilighting
(require 'evil-search-highlight-persist)
(global-evil-search-highlight-persist t)
(evil-leader/set-key "/" 'evil-search-highlight-persist-remove-all)

;; Use leader<space> to save file
(evil-leader/set-key "SPC" 'save-buffer)

;; Easer opening of M-x
(evil-leader/set-key "l" `helm-M-x)

;; Magit call
(evil-leader/set-key "g" `magit-status)

;; Set up helm
(require 'helm-config)
(helm-mode 1)
(global-set-key (kbd "M-x") 'helm-M-x)
(setq helm-M-x-fuzzy-match t) ;; optional fuzzy matching for helm-M-x
(define-key evil-normal-state-map (kbd ", ,") 'helm-projectile)

;; Smoother scrolling
(require 'smooth-scrolling)
(smooth-scrolling-mode 1)

;; Power line ;)
(require 'powerline)
(require 'powerline-evil)
(powerline-evil-vim-color-theme)

;; j/k for wrapped lines
(define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)

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

;; Simple clipboard
(require 'simpleclip)
(simpleclip-mode 1)

;; Autopair
(require 'autopair)
(autopair-global-mode)

;; No backup files
(setq make-backup-files nil)

;; Remember cursor positon
(require 'saveplace)
(setq-default save-place t)

;; Remove unnecessary stuff
(scroll-bar-mode -1)
(menu-bar-mode -1) 
(tool-bar-mode -1) 

;; CtrlP ish thing
(require 'ido)
(ido-mode t)
(define-key evil-normal-state-map (kbd ", m") `ido-find-file)

;; Remap to kill all other buffers
(evil-leader/set-key "o" (kbd "C-x 1"))

;; Easer help access - i wil need it
(evil-leader/set-key "f" (kbd "C-h f"))

;; Autocompletion plugin
(require `auto-complete)
(ac-config-default)

;; Comment out stuff easily
(evil-commentary-mode)

;;Surround pllugin
(require 'evil-surround)
(global-evil-surround-mode 1)

;; Git gutter
(global-git-gutter-mode +1)

;; Like drag visuals
(drag-stuff-mode t)
(drag-stuff-global-mode 1)

;; Split like vim
(evil-leader/set-key "h" (lambda () (interactive) (split-window-below) (windmove-down)))
(evil-leader/set-key "v" (lambda () (interactive) (split-window-right) (windmove-right)))

;; Relative and absolutr numberig
(require 'linum-relative)
(linum-relative-on)
(setq linum-relative-current-symbol "")

;; Easier buffer switching
(define-key evil-normal-state-map (kbd "H") `windmove-left)
(define-key evil-normal-state-map (kbd "J") `windmove-down)
(define-key evil-normal-state-map (kbd "K") `windmove-up)
(define-key evil-normal-state-map (kbd "L") `windmove-right)

;; Remap ; to :
(define-key evil-normal-state-map (kbd ";") 'evil-ex)

;; get :<caps> to work
(evil-ex-define-cmd "Q" `kill-buffer-and-window)
(evil-ex-define-cmd "W" `save-buffer)
(evil-ex-define-cmd "WQ" (lambda () (interactive) (save-buffer) (kill-buffer-and-window)))
(evil-ex-define-cmd "Wq" (lambda () (interactive) (save-buffer) (kill-buffer-and-window)))

;; Disable the annoying bell
(setq ring-bell-function 'ignore)

;; Don't touch my clipboard
(setq x-select-enable-clipboard nil)

;; Start maximized
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("e87a2bd5abc8448f8676365692e908b709b93f2d3869c42a4371223aab7d9cf8" default)))
 '(global-linum-mode t)
 '(inhibit-startup-screen t)
 '(initial-frame-alist (quote ((fullscreen . maximized)))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
