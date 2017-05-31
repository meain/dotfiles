;; Set font
(set-default-font "Monaco 16")

;; (setq initial-scratch-message "   : meain")
(setq initial-major-mode 'markdown-mode)
(defun immortal-scratch ()
  "Make scratch buffer immortal."
  (if (eq (current-buffer) (get-buffer "*scratch*"))
  (progn (bury-buffer)
     nil)
t))
(add-hook 'kill-buffer-query-functions 'immortal-scratch)


;;disable backup
(setq backup-inhibited t)

;;disable auto save
(setq auto-save-default nil)

;; no line wrapping
(setq-default truncate-lines 1)

;; line highlight
(global-hl-line-mode 1)

;; change all prompts to y or n
(fset 'yes-or-no-p 'y-or-n-p)

;; set default tab width
(setq-default tab-width 4)

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
              `powerline-evil
              `evil-leader
              `smooth-scrolling
              `flycheck
              `autopair
              `saveplace
              `ido
              `auto-complete
              `evil-commentary
              `evil-surround
              `evil-search-highlight-persist
              `git-gutter
              `drag-stuff
              `evil-surround
              `gruvbox-theme
              `spaceline
              `diff-hl
              `markdown-mode+
              `persistent-scratch
              `neotree
              `minimap
              `rainbow-mode
              `rainbow-delimiters
              `elpy)

;; Evil mode
(require 'evil)
(evil-mode t)

;; enable elpy, yeah its mostly python here
(elpy-enable)

;; yank and copy to clipboard (rest everything into the kill rig)
;; stil have issue with c and x
(evil-define-operator evil-delete-into-null-register (beg end type register yank-handler)
  "Delete text from BEG to END with TYPE. Do not save it in any register."
  (interactive "<R><x><y>")
  (evil-delete beg end type ?_ yank-handler))
(define-key evil-normal-state-map "d" 'evil-delete-into-null-register)

;; Theme
(load-theme 'gruvbox t)

;; Set up leader key in emacs
(require `evil-leader)
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

;; buffer list easy open
(evil-leader/set-key "b" `helm-buffers-list)

;; shortcut to save current buffer
(evil-leader/set-key "w" `evil-save)

;; easy quitting of buffer/window
(evil-leader/set-key "q" `evil-delete-buffer)

;; start shell with leader t
(evil-leader/set-key "t" `term)

;; Set up helm
(require 'helm-config)
(helm-mode 1)
(global-set-key (kbd "M-x") 'helm-M-x)
(setq helm-M-x-fuzzy-match t) ;; optional fuzzy matching for helm-M-x

;; Smoother scrolling
(require 'smooth-scrolling)
(smooth-scrolling-mode 1)

;; Powerline
(require 'spaceline)
(require 'spaceline-config)
(spaceline-spacemacs-theme)
(spaceline-toggle-minor-modes-off)
(spaceline-toggle-hud-off)
(setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
(setq powerline-height 20)
(setq powerline-raw " ")
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
(require `recentf)
(define-key evil-normal-state-map (kbd ", ,") `helm-recentf)
;; helm-recentf-fuzzy-match var is broken: redeclare it manually
(setq helm-source-recentf
  (helm-make-source "Recentf" 'helm-recentf-source
    :fuzzy-match t))

;; Autopair
(require 'autopair)
(autopair-global-mode)

;; No backup files
(setq make-backup-files nil)

;; Remember cursor positon
(require 'saveplace)
(setq save-place-file "~/.emacs.d/saveplace") ;; keep my ~/ clean
(setq-default save-place t)                   ;; activate it for all buffers


;; Remove unnecessary stuff
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)

;; Remap to kill all other buffers
(evil-leader/set-key "o" (kbd "C-x 1"))

;; Easer help access - i wil need it
(evil-leader/set-key "f" (kbd "C-h f"))

;; Autocompletion plugin
(require `auto-complete)
(ac-config-default)

;; persistant scratch buffer - so cool
(persistent-scratch-setup-default)

;; Comment out stuff easily
(evil-commentary-mode)

;;Surround pllugin
(require 'evil-surround)
(global-evil-surround-mode 1)

;; Like drag visuals
(require 'drag-stuff)
(drag-stuff-mode t)
(drag-stuff-global-mode 1)
(drag-stuff-define-keys)

;; easier viewing of delimiters
(require `rainbow-delimiters)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; Split like vim
(evil-leader/set-key "h" (lambda () (interactive) (split-window-below) (windmove-down)))
(evil-leader/set-key "v" (lambda () (interactive) (split-window-right) (windmove-right)))

(global-linum-mode)

;; Git gutter
(diff-hl-flydiff-mode t)
(global-diff-hl-mode t)

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

;; for now, the clipboard thing is messy
(defun copy-to-clipboard ()
  (interactive)
  (if (display-graphic-p)
      (progn
        (message "Yanked region to x-clipboard!")
        (call-interactively 'clipboard-kill-ring-save)
        )
    (if (region-active-p)
        (progn
          (shell-command-on-region (region-beginning) (region-end) "xsel -i -b")
          (message "Yanked region to clipboard!")
          (deactivate-mark))
      (message "No region active; can't yank to clipboard!")))
  )
(evil-define-command paste-from-clipboard()
  (if (display-graphic-p)
      (progn
        (clipboard-yank)
        (message "graphics active")
        )
    (insert (shell-command-to-string "xsel -o -b"))
    )
  )
(global-set-key [f8] 'copy-to-clipboard)
(global-set-key [f9] 'paste-from-clipboard)


;; Maximize current buffer
(defun toggle-maximize-buffer () "Maximize buffer"
  (interactive)
  (if (= 1 (length (window-list)))
      (jump-to-register '_)
    (progn
      (window-configuration-to-register '_)
      (delete-other-windows))))
(define-key evil-normal-state-map (kbd "-") `toggle-maximize-buffer)


;; Visible whitespace
(setq whitespace-style '(trailing tabs newline tab-mark newline-mark))

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
 '(initial-frame-alist (quote ((fullscreen . maximized))))
 '(package-selected-packages
   (quote
	(snippet vimrc-mode auctex-latexmk magic-latex-buffer init-open-recentf magit-find-file find-things-fast helm-fuzzy-find highlight-current-line rainbow-mode neotree linum-relative drag-stuff git-gutter evil-surround evil-commentary autopair simpleclip flycheck smooth-scrolling projectile powerline-evil magit helm gruvbox-theme evil-search-highlight-persist evil-leader auto-complete)))
 '(send-mail-function (quote smtpmail-send-it)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(trailing-whitespace ((t (:background "dim gray")))))
