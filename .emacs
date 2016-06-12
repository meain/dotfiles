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
                          'magit)

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

;; Set up helm
(require 'helm-config)
(helm-mode 1)
(global-set-key (kbd "M-x") 'helm-M-x)
(setq helm-M-x-fuzzy-match t) ;; optional fuzzy matching for helm-M-x
(define-key evil-normal-state-map (kbd ", m") 'helm-mini)

;;Line numering -relative
(add-hook 'prog-mode-hook 'relative-line-numbers-mode t)
(add-hook 'prog-mode-hook 'line-number-mode t)
(add-hook 'prog-mode-hook 'column-number-mode t)

;; Smoother scrolling
(require 'smooth-scrolling)
(smooth-scrolling-mode 1)

;; Power line ;)
(require 'powerline)
(powerline-evil-vim-color-theme)
(display-time-mode t)

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

;; Start maximized
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t)
 '(initial-frame-alist (quote ((fullscreen . maximized)))))

;; Page up and down
(define-key evil-normal-state-map (kbd "9") (lambda ()
                    (interactive)
                    (evil-scroll-up nil)))
(define-key evil-normal-state-map (kbd "8") (lambda ()
                        (interactive)
                        (evil-scroll-down nil)))

;; Automateic indentation
(define-key global-map (kbd "RET") 'newline-and-indent)

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
(evil-leader/set-key ", ," `ido-find-file)
