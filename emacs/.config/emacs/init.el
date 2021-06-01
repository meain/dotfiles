;;; init -- meain's Emacs config

;;; Commentary:
;; Well, this is a vimmer's Emacs config.  Nothing fancy though.

;;; Code:

;;; [PACKAGE SETUP] =============================================

;; Basic setup
(setq user-mail-address "mail@meain.io" user-full-name
      "Abin Simon")

;; Setup straight.el
(setq straight-repository-branch "develop")
(setq straight-vc-git-default-protocol 'ssh)
(defvar bootstrap-version)
(let ((bootstrap-file (expand-file-name "straight/repos/straight.el/bootstrap.el"
                                        user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer (url-retrieve-synchronously "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
                                                     'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(straight-use-package 'use-package)
(setq use-package-always-demand (getenv "LOAD_FULL_EMACS"))

;; Get proper PATH
(use-package exec-path-from-shell
  :straight t
  :config (exec-path-from-shell-initialize))

;;; [BASE EVIL] =================================================

;; Evil mode (set this up first)
(use-package evil
  :straight t
  :init (progn
          (setq evil-want-integration t)
          (setq evil-want-keybinding nil)
          (setq evil-want-C-u-scroll t)
          (setq evil-undo-system 'undo-tree)
          (setq evil-kill-on-visual-paste nil)
          (setq evil-symbol-word-search t)):config
  (progn
    (evil-mode t)
    (defalias #'forward-evil-word #'forward-evil-symbol)
    (evil-set-command-property 'evil-visual-char
                               :jump t)
    (evil-set-command-property 'evil-visual-line
                               :jump t)))

;; Evil leader
(use-package evil-leader
  :straight t
  :config (progn
            (global-evil-leader-mode)
            (evil-leader/set-leader "s")))

;; Some keybindings
(evil-leader/set-key "l" 'execute-extended-command)
(evil-leader/set-key "h l" 'find-library)

;;; [BASIC SETTINGS] =============================================

;; Use cl-lib
(require 'cl-lib)

;; Consistent window title
(setq frame-title-format '("Emacs")) ; needed by hammerspoon

;; Fix some cmd keybinds
(global-set-key (kbd "s-v")
                'yank)
(global-set-key (kbd "s-a")
                'mark-whole-buffer)
(global-set-key (kbd "s-c")
                'kill-ring-save)

;; Scratch initial content
(defun meain/get-scratch-message ()
  "Pull a random fortue entry and format it for `lisp-interaction' mode as a comment."
  (concat (mapconcat 'identity
                     (mapcar (lambda (x)
                               (cl-concatenate 'string ";; " x))
                             (cl-remove-if (lambda (x)
                                             (equal x ""))
                                           (split-string (shell-command-to-string "shuf -n1 ~/.local/share/quotes")
                                                         "\n")))
                     "\n")
          "\n"))
(setq initial-scratch-message (meain/get-scratch-message))

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

;; Delete visual selection when I start typing
(delete-selection-mode 1)

;; Meta-f keybinds (tmux memmories)
(global-unset-key (kbd "M-f")) ; have to unset first

;;; [VISUAL CONFIG] ==============================================

;; Change font everywhere
(defun meain/setup-fonts ()
  "Setup all the proper fonts for everything."
  (set-face-attribute 'default nil :font meain/font-family-default)
  (set-face-attribute 'fixed-pitch nil :font meain/font-family-default)
  (set-face-attribute 'variable-pitch nil :font meain/font-family-default))
(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (with-selected-frame frame
                  (meain/setup-fonts))))
  (meain/setup-fonts))
(defun meain/select-font ()
  "Set font after selection using ivy."
  (interactive)
  (set-frame-font (completing-read "Choose font: "
                                   (font-family-list))))

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
  :straight t
  :init (load-theme 'modus-operandi t))

;; Diminish
(use-package diminish
  :straight t
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

;; auto-fill
(evil-leader/set-key "b F" 'auto-fill-mode)

;; Cursor blink
(blink-cursor-mode -1)

;; Follow symlinks for vc
(setq vc-follow-symlinks t)

;; auto-pair
(electric-pair-mode t)

;; macro for alternate pattern
(defmacro meain/with-alternate (original alternate)
  "Macro for easily creating commands with alternate on `universal-argument'.
Pass ORIGINAL and ALTERNATE options."
  `(lambda (&optional use-alternate)
     (interactive "P")
     (if use-alternate ,alternate ,original)))

;;; [EVIL CONFIG] ================================================

;; Evil commentary
(use-package evil-commentary
  :straight t
  :diminish :init
  (evil-commentary-mode))

;; Evil surround
(use-package evil-surround
  :straight t
  :init (global-evil-surround-mode 1))

;; Evil text objects
(use-package evil-textobj-line :straight t)
(use-package evil-textobj-syntax :straight t)

;; Evil number increment
(use-package evil-numbers
  :straight t
  :config (progn
            ;; cannot directly use C-x (in use by emacs)
            (define-key evil-normal-state-map (kbd "g C-a") 'evil-numbers/inc-at-pt-incremental)
            (define-key evil-normal-state-map (kbd "g C-x") 'evil-numbers/dec-at-pt-incremental)))

;; Save buffer
(define-key evil-normal-state-map (kbd "<SPC> <SPC>") 'evil-write)

;; Hit universal arg without ctrl
(evil-leader/set-key "u" 'universal-argument)
(global-set-key (kbd "M-u")
                'universal-argument)

;; Window mappings
(defun meain/move-swap-right (&optional swap)
  "Move to window on right or move window to right if SWAP."
  (interactive "P")
  (if swap
      (windmove-swap-states-right)
    (windmove-right)))
(global-set-key (kbd "M-l")
                'meain/move-swap-right)
(defun meain/move-swap-left (&optional swap)
  "Move to window on left or move window to left if SWAP."
  (interactive "P")
  (if swap
      (windmove-swap-states-left)
    (windmove-left)))
(global-set-key (kbd "M-h")
                'meain/move-swap-left)
(defun meain/move-swap-up (&optional swap)
  "Move to window on top or move window to top if SWAP."
  (interactive "P")
  (if swap
      (windmove-swap-states-up)
    (windmove-up)))
(global-set-key (kbd "M-k")
                'meain/move-swap-up)
(defun meain/move-swap-down (&optional swap)
  "Move to window on bottom or move window to bottom if SWAP."
  (interactive "P")
  (if swap
      (windmove-swap-states-down)
    (windmove-down))
  'meain/move-swap-down)
(global-set-key (kbd "M-j")
                'meain/move-swap-down)
(global-set-key (kbd "M-b")
                (lambda (&optional open-term)
                  (interactive "P")
                  (split-window-below)
                  (windmove-down)
                  (when open-term
                    (vterm t))))
(global-set-key (kbd "M-v")
                (lambda (&optional open-term)
                  (interactive "P")
                  (split-window-right)
                  (windmove-right)
                  (when open-term
                    (vterm t))))
(global-set-key (kbd "M-w")
                'delete-window)
(global-set-key (kbd "M-o")
                (meain/with-alternate (other-window 1)
                                      (call-interactively 'delete-other-windows)))

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

;; Easier C-c C-c
(evil-leader/set-key "i"
  '(lambda ()
     (interactive)
     (execute-kbd-macro (kbd "C-c C-c"))))

;; Up/Down don't bother wrap
(define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)

;; Remap macro recoring key
(define-key evil-normal-state-map "Q" 'evil-record-macro)

;; Eval region
(define-key evil-visual-state-map (kbd ";") (lambda ()
                                              (interactive)
                                              (call-interactively 'eval-region)
                                              (evil-force-normal-state)))

;; Quick quit
(defun meain/create-or-switch-to-scratch ()
  "Switch to scratch buffer if exists, else create a scratch buffer with our config."
  (cond
   ((get-buffer "*scratch*")
    (switch-to-buffer "*scratch*"))
   (t (progn
        (switch-to-buffer "*scratch*")
        (setq default-directory "~/")
        (lisp-interaction-mode)
        (insert (meain/get-scratch-message))))))
(defun meain/kill-current-buffer-unless-scratch ()
  "Kill current buffer if it is not scratch."
  (interactive)
  (if (= (length (mapcar #'window-buffer
                         (window-list))) 1)
      (meain/create-or-switch-to-scratch)
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
(evil-set-initial-state 'comint-mode 'normal)
(defun meain/recompile-or-compile (&optional arg)
  "Compile or recompile based on universal `ARG'."
  (interactive "P")
  (if arg
      (call-interactively 'compile)
    (compile compile-command t)))
(evil-leader/set-key "r" 'meain/recompile-or-compile)

;; Simplify how Async Shell Command buffers get displayed
;; (add-to-list 'display-buffer-alist
;;   '("\\*Async Shell Command\\*.*" display-buffer-no-window))
(add-to-list 'display-buffer-alist
             '("\\*Async Shell Command\\*.*"
               (display-buffer-reuse-window display-buffer-at-bottom)
               (reusable-frames . visible)
               (window-height . 0.1)))

;;; [OTHER PACKAGES] =============================================

;; project (eglot dependency)
(use-package project :straight t)

;; eldoc load
(use-package eldoc
  :init (setq eldoc-echo-area-use-multiline-p nil):config
  (global-eldoc-mode nil))

;; dired
(use-package dired
  :defer t
  :config (progn
            (setq delete-by-moving-to-trash t)
            (setq trash-directory "~/.Trash")
            (setq dired-listing-switches "-AGFhlgo")
            (setq dired-dwim-target t)
            (define-key dired-mode-map (kbd "-") 'dired-up-directory)
            (evil-define-key 'normal
              dired-mode-map
              (kbd "+")
              'dired-create-empty-file)
            (setq dired-omit-files "\\.DS_Store$\\|__pycache__$\\|.pytest_cache$\\|\\.mypy_cache$\\|\\.egg-info$")
            (add-hook 'dired-mode-hook 'dired-omit-mode)
            (add-hook 'dired-mode-hook 'hl-line-mode)
            (add-hook 'dired-mode-hook 'dired-hide-details-mode)
            (define-key evil-normal-state-map (kbd "-") 'dired-jump)
            (define-key evil-normal-state-map (kbd "_") 'find-file)))


(use-package aas
  :straight t
  :defer 1
  :hook (text-mode . ass-activate-for-major-mode):hook
  (org-mode . ass-activate-for-major-mode)
  :hook (prog-mode . ass-activate-for-major-mode):hook
  (python-mode . ass-activate-for-major-mode)
  :config (progn
            (aas-set-snippets 'text-mode
                              ";isodate"
                              (lambda ()
                                (interactive)
                                (insert (format-time-string "%a, %d %b %Y %T %z")))
                              ";date"
                              (lambda ()
                                (interactive)
                                (insert (format-time-string "%a %d %b %Y")))
                              ";sdate"
                              (lambda ()
                                (interactive)
                                (insert (format-time-string "%d %b %Y")))
                              ";d/"
                              (lambda ()
                                (interactive)
                                (insert (format-time-string "%D")))
                              ";time"
                              (lambda ()
                                (interactive)
                                (insert (format-time-string "%T")))
                              ";filename"
                              (lambda ()
                                (interactive)
                                (insert (file-name-nondirectory (buffer-file-name)))))
            (aas-set-snippets 'prog-mode
                              ";isodate"
                              (lambda ()
                                (interactive)
                                (insert (format-time-string "%a, %d %b %Y %T %z")))
                              ";date"
                              (lambda ()
                                (interactive)
                                (insert (format-time-string "%a %d %b %Y")))
                              ";sdate"
                              (lambda ()
                                (interactive)
                                (insert (format-time-string "%d %b %Y")))
                              ";d/"
                              (lambda ()
                                (interactive)
                                (insert (format-time-string "%D")))
                              ";time"
                              (lambda ()
                                (interactive)
                                (insert (format-time-string "%T")))
                              ";filename"
                              (lambda ()
                                (interactive)
                                (insert (file-name-nondirectory (buffer-file-name)))))
            (aas-set-snippets 'web-mode
                              ";html"
                              (lambda ()
                                (interactive)
                                (insert-file-contents (expand-file-name "~/.config/datafiles/html_starter"))))
            (aas-set-snippets 'html-mode
                              ";html"
                              (lambda ()
                                (interactive)
                                (insert-file-contents (expand-file-name "~/.config/datafiles/html_starter"))))
            (aas-set-snippets 'markdown-mode
                              ";bang"
                              (lambda ()
                                (interactive)
                                (insert (concat "---\ntitle: "
                                                (file-name-nondirectory (file-name-sans-extension (buffer-file-name)))
                                                "\ncreated: "
                                                (format-time-string "%a %d %b %Y %T")
                                                "\n---\n"))))
            (aas-set-snippets 'python-mode ";ip" "__import__('ipdb').set_trace()")
            (aas-set-snippets 'org-mode ";el" "#+BEGIN_SRC emacs-lisp\n\n#+END_SRC"
                              ";py" "#+BEGIN_SRC python\n\n#+END_SRC" ";co"
                              "#+BEGIN_SRC\n\n#+END_SRC")))


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
  :straight t
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
  :straight t
  :defer 1
  :diminish :config
  (progn
    (setq company-dabbrev-downcase nil) ;; Do not lowercase my completions
    (setq company-idle-delay 0)
    (setq company-tooltip-maximum-width 35)
    (setq company-tooltip-align-annotations t)
    (setq company-minimum-prefix-length 2)
    (setq company-format-margin-function nil)
    (global-company-mode)
    ;; company-tng-mode provides better autocomplete behaviour
    (company-tng-mode)))

;; Company quickhelp
(use-package company-quickhelp ; Show help in tooltip
  :straight t
  :after company
  :config (company-quickhelp-mode))

;; Completions
(use-package selectrum
  :straight t
  :config (progn
            (setq selectrum-should-sort t)
            (setq read-file-name-completion-ignore-case
                  t)
            (setq read-buffer-completion-ignore-case t)
            (setq completion-ignore-case t)
            (setq selectrum-display-style '(horizontal))
            (selectrum-mode +1)):bind
  (:map selectrum-minibuffer-map
        ("<S-backspace>" . selectrum-backward-kill-sexp)))
(use-package selectrum-prescient
  :straight t
  :config (progn
            (setq prescient-filter-method '(literal-prefix prefix fuzzy regexp))
            (selectrum-prescient-mode +1)
            (prescient-persist-mode +1)))
(use-package marginalia
  :straight t
  :bind (:map minibuffer-local-map
              ("C-b" . marginalia-cycle)):config
  (progn
    ;; (setq marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
    (setq marginalia-annotators '(marginalia-annotators-heavy))
    (marginalia-mode)))

;; Consult without consultation fees
(use-package consult :straight t)

;; Helpful package
(use-package helpful
  :straight t
  :commands (helpful-callable helpful-variable helpful-at-point
                              helpful-key):init
  (progn
    (evil-leader/set-key "h p" 'helpful-at-point)
    (evil-leader/set-key "h k" 'helpful-key)
    (evil-leader/set-key "h f" 'helpful-function)
    (evil-leader/set-key "h v" 'helpful-variable)
    (evil-leader/set-key "h o" 'helpful-symbol)))

;; ibuffer
(use-package ibuffer
  :init (setq ibuffer-expert t):config
  (progn
    (global-set-key (kbd "M-c")
                    (meain/with-alternate (call-interactively 'switch-to-buffer)
                                          (ibuffer-other-window)))
    (evil-leader/set-key "b b"
      (meain/with-alternate (call-interactively 'switch-to-buffer)
                            (ibuffer-other-window)))))

;; rg.el
(use-package rg
  :straight t
  :commands rg
  :init (progn
          (setq rg-command-line-flags '("--hidden" "--follow"))
          (evil-leader/set-key "f"
            (meain/with-alternate (let ((selectrum-display-style '(vertical)))
                                    (consult-ripgrep))
                                  (call-interactively 'rg)))))

;; dumb-jump
(use-package dumb-jump
  :straight t
  :commands dumb-jumb-go
  :init (evil-leader/set-key "J" 'dumb-jump-go))


;; Code formatting
(use-package srefactor
  :straight t
  :config (require 'srefactor-lisp))
(use-package format-all
  :defer 1
  :straight t
  :config (progn
            (define-format-all-formatter fixjson
              ;; Use fixjson for formatting json files
              (:executable "fixjson")
              (:install "npm i -g fixjson")
              (:languages "JSON")
              (:format (format-all--buffer-easy executable)))
            (setq format-all-formatters '(("HTML" prettier)
                                          ("Go" goimports)
                                          ("JSON" fixjson)))
            (add-hook 'prog-mode-hook 'format-all-ensure-formatter)):init
  (progn
    (defun meain/auto-format ()
      "Custom auto-format based on filetype."
      (interactive)
      (if (eq major-mode 'emacs-lisp-mode)
          (srefactor-lisp-format-buffer)
        (call-interactively 'format-all-buffer)))
    (define-key evil-normal-state-map (kbd ",,") 'meain/auto-format)))

;; Projectile
(use-package projectile
  :straight t
  :diminish :commands
  (projectile-switch-project projectile-find-file)
  :init (evil-leader/set-key "p" 'projectile-switch-project):config
  (progn
    (define-key evil-normal-state-map (kbd "<RET>") (lambda ()
                                                      (interactive)
                                                      (if (projectile-project-p)
                                                          (projectile-find-file)
                                                        (projectile-switch-project))))
    (setq projectile-mode-line "Projectile") ; might speed up tramp
    (projectile-mode 1)
    (setq projectile-sort-order 'recently-active)))

;; ibuffer-projectile
(use-package ibuffer-projectile
  :straight t
  :defer 1
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



;; LSP
(use-package eglot
  :commands eglot-ensure
  :after project
  :straight t
  :hook ((python-mode . eglot-ensure)
         (rust-mode . eglot-ensure)
         (shell-script-mode . eglot-ensure)
         (sh-mode . eglot-ensure)
         (js-mod . eglot-ensure)
         (lua-mode . eglot-ensure)
         (go-mode . eglot-ensure)):config
  (progn
    (add-to-list 'eglot-server-programs
                 '(lua-mode . ("~/.luarocks/bin/lua-lsp")))
    ;; Can be enabled on fiction like things
    (add-to-list 'eglot-server-programs
                 '(markdown-mode . ("unified-language-server" "--parser=remark-parse"
                                    "--stdio")))
    (add-to-list 'eglot-server-programs
                 '(rust-mode . ("rust-analyzer")))
    (add-to-list 'eglot-server-programs
                 '(python-mode . ("~/.bin/vpyls")))
    (add-to-list 'eglot-server-programs
                 '(nix-mode . ("rnix-lsp")))
    (add-to-list 'display-buffer-alist
                 '("\\*sqls\\*"
                   (display-buffer-reuse-window display-buffer-at-bottom)
                   (reusable-frames . visible)
                   (window-height . 0.3)))
    (defclass eglot-sqls (eglot-lsp-server)
      ()
      :documentation "SQL's Language Server")
    (add-to-list 'eglot-server-programs
                 '(sql-mode . (eglot-sqls "sqls" "-config" "~/.config/sqls/config.yaml")))
    (cl-defmethod eglot-execute-command
      ((server eglot-sqls)
       (command (eql executeQuery))
       arguments)
      "For executeQuery."
      (let* ((beg (eglot--pos-to-lsp-position (if (use-region-p)
                                                  (region-beginning)
                                                (point-min))))
             (end (eglot--pos-to-lsp-position (if (use-region-p)
                                                  (region-end)
                                                (point-max))))
             (res (jsonrpc-request server
                                   :workspace/executeCommand `(:command ,(format "%s" command)
                                                                        :arguments ,arguments
                                                                        :timeout 0.5
                                                                        :range (:start ,beg :end ,end))))
             (buffer (generate-new-buffer "*sqls*")))
        (with-current-buffer buffer
          (eglot--apply-text-edits `[(:range (:start (:line 0 :character 0)
                                                     :end (:line 0 :character 0))
                                             :newText ,res)])
          (org-mode))
        (pop-to-buffer buffer)))
    (cl-defmethod eglot-execute-command
      ((server eglot-sqls)
       (_cmd (eql switchDatabase))
       arguments)
      "For switchDatabase."
      (let* ((res (jsonrpc-request server
                                   :workspace/executeCommand `(:command "showDatabases" :arguments ,arguments
                                                                        :timeout 0.5)))
             (menu-items (split-string res "\n"))
             (menu `("Eglot code actions:"
                     ("dummy" ,@menu-items)))
             (db (if (listp last-nonmenu-event)
                     (x-popup-menu last-nonmenu-event menu)
                   (completing-read "[eglot] Pick an database: "
                                    menu-items
                                    nil
                                    t
                                    nil
                                    nil
                                    (car menu-items)))))
        (jsonrpc-request server
                         :workspace/executeCommand `(:command "switchDatabase"
                                                              :arguments [,db]:timeout
                                                              0.5))))
    (cl-defmethod eglot-execute-command
      ((server eglot-sqls)
       (_cmd (eql switchConnections))
       arguments)
      "For switchConnections"
      (let* ((res (jsonrpc-request server
                                   :workspace/executeCommand `(:command "switchConnections" :arguments ,arguments
                                                                        :timeout 0.5)))
             (menu-items (split-string res "\n"))
             (menu `("Eglot code actions:"
                     ("dummy" ,@menu-items)))
             (db (if (listp last-nonmenu-event)
                     (x-popup-menu last-nonmenu-event menu)
                   (completing-read "[eglot] Pick an connection "
                                    menu-items
                                    nil
                                    t
                                    nil
                                    nil
                                    (car menu-items)))))
        (jsonrpc-request server
                         :workspace/executeCommand `(:command "switchConnections"
                                                              :arguments [,db]:timeout
                                                              0.5))))
    (define-key evil-normal-state-map (kbd "K") 'eldoc-print-current-symbol-info)
    (define-key evil-normal-state-map (kbd "g k") 'eldoc-print-current-symbol-info)
    (define-key evil-normal-state-map (kbd "g d") 'xref-find-definitions)
    (define-key evil-normal-state-map (kbd "g r") 'xref-find-references)
    (define-key evil-normal-state-map (kbd "g R") 'eglot-rename)
    (define-key evil-normal-state-map (kbd "g ,") 'eglot-format-buffer)
    (define-key evil-normal-state-map (kbd "g a") 'eglot-code-actions)))

;; Tagbar alternative
(use-package imenu :straight t
  :defer t
  :commands imenu)
(use-package flimenu
  :straight t
  :after imenu
  :config (flimenu-global-mode 1))
(use-package imenu-list
  :straight t
  :defer t
  :commands imenu-list-smart-toggle
  :init (global-set-key (kbd "M--")
                        (meain/with-alternate (call-interactively 'imenu)
                                              (imenu-list-smart-toggle))):config
  (progn
    (setq imenu-list-focus-after-activation t)
    (setq imenu-list-after-jump-hook nil)
    (setq imenu-list-auto-resize t)))

;; Flat imenu
(use-package flimenu
  :straight t
  :after imenu-list
  :config (flimenu-global-mode 1))

;; Magit
(use-package magit
  :straight t
  :commands (magit-status magit-commit-create):init
  (progn
    (evil-leader/set-key "gg" 'magit-status)
    (evil-leader/set-key "gc" 'magit-commit-create))
  :config (progn
            (setq magit-diff-refine-hunk (quote all))
            (define-key magit-mode-map (kbd "M-w") 'delete-window)
            (setq magit-completing-read-function #'selectrum-completing-read)))

;; Magit forge
(use-package forge :straight t
  :after magit)

;; Diff hl
(use-package diff-hl
  :straight t
  :defer 1
  :config (progn
            (custom-set-faces '(diff-hl-change ((t (:background "#ede5cb"))))
                              '(diff-hl-insert ((t (:background "#c7ddc7"))))
                              '(diff-hl-delete ((t (:background "#edc4c4")))))
            (let* ((height (frame-char-height))
                   (width 2)
                   (bits (make-vector height 0)))
              (define-fringe-bitmap 'my-diff-hl-bitmap bits
                height width))
            (setq diff-hl-fringe-bmp-function (lambda (type pos)
                                                'my-diff-hl-bitmap))
            (diff-hl-flydiff-mode)
            (global-diff-hl-mode)
            (add-hook 'dired-mode-hook 'diff-hl-dired-mode-unless-remote)
            (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
            (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
            (evil-leader/set-key "gr" 'diff-hl-revert-hunk)
            (evil-leader/set-key "gj" 'diff-hl-next-hunk)
            (evil-leader/set-key "gk" 'diff-hl-previous-hunk)
            (evil-leader/set-key "gn" 'diff-hl-next-hunk)
            (evil-leader/set-key "gp" 'diff-hl-previous-hunk)))

;; Git messenger
(use-package git-messenger
  :straight t
  :commands git-messenger:popup-message
  :init (evil-leader/set-key "G" 'git-messenger:popup-message))

;; Matchit
(use-package evil-matchit
  :straight t
  :defer 1
  :config (global-evil-matchit-mode 1))

;; Highlight color codes
(use-package rainbow-mode
  :straight t
  :defer t
  :init (rainbow-mode 1))

;; Code folding
(use-package origami
  :straight t
  :commands evil-toggle-fold
  :init (progn
          (global-origami-mode)
          (define-key evil-normal-state-map (kbd "<SPC> TAB") 'evil-toggle-fold)
          (evil-leader/set-key "o" 'evil-toggle-fold)))

;; drag-stuff
(use-package drag-stuff
  :straight t
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
  :straight t
  :init (progn
          (save-place-mode t)
          (setq save-place-file "~/.cache/emacs/saveplace")))

;; Persistant undo using undo-tree
(use-package undo-tree
  :straight t
  :diminish :config
  (progn
    (setq undo-limit 80000000)
    (setq evil-want-fine-undo nil)
    (setq undo-tree-auto-save-history t)
    (setq undo-tree-history-directory-alist '(("." . "~/.cache/emacs/undo"))))
  :init (global-undo-tree-mode t))

;; Fancier tab managerment
(use-package tab-bar
  :straight t
  :commands (tab-close tab-new tab-next):init
  (progn
    (defun meain/create-or-delete-tab (&optional close)
      "Create or close tab"
      (interactive "P")
      (let ((tabs (cl-remove-if (lambda (x)
                                  (equal x "scratch"))
                                (mapcar (lambda (tab)
                                          (alist-get 'name tab))
                                        (tab-bar--tabs-recent)))))
        (if close
            (if (eq tabs nil)
                (message "Not closing last tab")
              (tab-close))
          (tab-new))))
    (defun meain/switch-tab-dwim (&optional chooser)
      "Switch between available tabs.  Pass CLOSE as t to close the current tab if it is not the last one."
      (interactive "P")
      (let ((tabs (cl-remove-if (lambda (x)
                                  (equal x "scratch"))
                                (mapcar (lambda (tab)
                                          (alist-get 'name tab))
                                        (tab-bar--tabs-recent)))))
        (if chooser
            (tab-bar-switch-to-tab (completing-read "Select tab: " tabs))
          (cond
           ((eq tabs nil)
            (message (concat "Only one tab present. Use `"
                             (substitute-command-keys "\\[meain/create-or-delete-tab]")
                             "` to create another tab.")))
           (t (tab-bar-switch-to-tab (car tabs)))))))
    (evil-leader/set-key "t" 'meain/switch-tab-dwim)
    (evil-leader/set-key "T" 'meain/create-or-delete-tab)
    (evil-leader/set-key "C"
      (lambda ()
        (interactive)
        ;; TODO: make notmuch and elfeed automatically open up in scratch tab
        (tab-bar-switch-to-tab "scratch")))
    (global-set-key (kbd "M-f s")
                    'meain/switch-tab-dwim))
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
            (global-set-key (kbd "M-f ,")
                            'tab-bar-rename-tab)))

;; which-key mode (until I fully figure out emacs)
(use-package which-key
  :straight t
  :diminish :config
  (which-key-mode))

;; Expand region
(use-package expand-region
  :straight t
  :config (progn
            ;; make evil jump list work with expand-region
            (evil-set-command-property 'er/expand-region
                                       :jump t)
            (global-set-key (kbd "M-i")
                            'er/expand-region)))

;; dtrt (atuo find indend setting)
(use-package dtrt-indent
  :straight t
  :diminish :config
  (dtrt-indent-global-mode))

(use-package indent-guide
  :straight t
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
  :straight t
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
               ((s-starts-with-p (meain/shell-name)
                                 (buffer-name (current-buffer)))
                (progn
                  (if rerun-previous
                      (progn
                        (vterm-clear)
                        (vterm-clear-scrollback))
                    (delete-window))))
               ((equal (length shell-buffers) 0)
                (meain/shell-new t))
               (t (progn
                    (pop-to-buffer (car shell-buffers))
                    (if rerun-previous
                        (progn
                          (vterm-clear)
                          (vterm-clear-scrollback)
                          (vterm-send-up)
                          (vterm-send-return))))))))
          (defun meain/shell-new (&optional always-create)
            "Create a new shell for the current project."
            (interactive)
            (setq default-directory (cond
                                     ((projectile-project-p)
                                      (projectile-project-root))
                                     (t "~/")))
            (if (or always-create
                    (s-starts-with-p "*popup-shell"
                                     (buffer-name)))
                (progn
                  (if (s-starts-with-p "*popup-shell"
                                       (buffer-name))
                      (delete-window))
                  (vterm (meain/shell-name)))
              (call-interactively 'switch-to-buffer)))
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
               (alternate (switch-to-buffer (completing-read "Choose shell: "
                                                             (mapcar (lambda (x)
                                                                       (buffer-name x))
                                                                     shell-buffers))))
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
    (define-key vterm-mode-map (kbd "M-l") 'meain/move-swap-right)
    (define-key vterm-mode-map (kbd "M-h") 'meain/move-swap-left)
    (define-key vterm-mode-map (kbd "M-k") 'meain/move-swap-up)
    (define-key vterm-mode-map (kbd "M-j") 'meain/move-swap-down)
    (define-key vterm-mode-map (kbd "M-H") 'shrink-window-horizontally)
    (define-key vterm-mode-map (kbd "M-L") 'enlarge-window-horizontally)
    (define-key vterm-mode-map (kbd "M-K") 'shrink-window)
    (define-key vterm-mode-map (kbd "M-J") 'enlarge-window)
    ;; (define-key vterm-mode-map (kbd "M-f l") 'ace-link)
    (define-key vterm-mode-map (kbd "M-b") (lambda (&optional open-term)
                                             (interactive "P")
                                             (split-window-below)
                                             (windmove-down)
                                             (when open-term
                                               (vterm t))))
    (define-key vterm-mode-map (kbd "M-v") (lambda (&optional open-term)
                                             (interactive "P")
                                             (split-window-right)
                                             (windmove-right)
                                             (when open-term
                                               (vterm t))))
    (add-to-list 'display-buffer-alist
                 '((lambda (bufname _)
                     (s-starts-with-p "*popup-shell" bufname))
                   (display-buffer-reuse-window display-buffer-at-bottom)
                   (reusable-frames . visible)
                   (window-height . 0.3)))
    (defun meain/clear-and-exec ()
      (interactive)
      (vterm-clear)
      (vterm-clear-scrollback)
      (vterm-send-return))
    (define-key vterm-mode-map [(S-return)] 'meain/clear-and-exec)
    (defun meain/vterm--kill-vterm-buffer-and-window (process event)
      "Kill buffer and window on vterm PROCESS termination.  EVENT is the close event."
      (when (not (process-live-p process))
        (let ((buf (process-buffer process)))
          (when (buffer-live-p buf)
            (with-current-buffer buf
              (kill-buffer)
              (ignore-errors (delete-window))
              (message "VTerm closed."))))))
    (add-hook 'vterm-mode-hook
              (lambda ()
                (set-process-sentinel (get-buffer-process (buffer-name))
                                      #'meain/vterm--kill-vterm-buffer-and-window)))))

;; ranger in emacs
(use-package ranger
  :straight t
  :commands ranger
  :config (use-package image-dired+
            :straight t
            :config (image-diredx-async-mode)))

;; editorconfig
(use-package editorconfig
  :straight t
  :config (editorconfig-mode 1))

;; eros for eval
(use-package eros
  :straight t
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
  :straight t
  :commands venv-workon
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
  (message "%s"
           (if full-file
               (format "toffee '%s' || exit 1"
                       (buffer-file-name))
             (format "toffee '%s' '%s' || exit 1"
                     (buffer-file-name)
                     (line-number-at-pos))))
  (compile (shell-command-to-string (if full-file
                                        (format "toffee '%s' || exit 1"
                                                (buffer-file-name))
                                      (format "toffee '%s' '%s' || exit 1"
                                              (buffer-file-name)
                                              (line-number-at-pos))))))
(evil-leader/set-key "d" 'meain/test-runner)
(evil-leader/set-key "D" 'meain/test-runner-full)

;; Neotree
(use-package neotree :straight t
  :commands neotree)

;; Evil keybindings for a lot of things
(use-package evil-collection
  :straight t
  :after evil
  :config (evil-collection-init))

;; Highlight TODO items
(use-package hl-todo
  :straight t
  :defer 1
  :config (progn
            (setq hl-todo-keyword-faces '(("TODO" . "#FF0000")
                                          ("FIXME" . "#FF0000")
                                          ("GOTCHA" . "#FF4500")
                                          ("STUB" . "#1E90FF")))
            (global-hl-todo-mode)))


;;; [FILETYPE PUGINS] ===============================================

(use-package rust-mode :straight t
  :defer t)
(use-package go-mode :straight t
  :defer t)
(use-package lua-mode :straight t
  :defer t)
(use-package web-mode :straight t
  :defer t)
(use-package jinja2-mode :straight t
  :defer t)
(use-package json-mode :straight t
  :defer t)
(use-package config-general-mode :straight t
  :defer t)  ;; config files
(use-package vimrc-mode :straight t
  :defer t)
(use-package markdown-mode
  :straight t
  :defer t
  :mode ("\\.md\\'" . gfm-mode):config
  (progn
    (setq markdown-enable-html -1)
    (setq markdown-command "pandoc -t html5")
    (setq markdown-fontify-code-blocks-natively
          t)))
(use-package nix-mode :straight t
  :defer t
  :mode "\\.nix\\'")
(use-package csv-mode
  :straight t
  :defer t
  :config (progn
            (setq csv-align-mode t)
            (set-face-attribute 'csv-separator-face nil
                                :background "gray100"
                                :foreground "#000000")))
(use-package yaml-mode :straight t
  :defer t)
(use-package dockerfile-mode
  :straight t
  :defer t
  :init (add-hook 'find-file-hook
                  (lambda ()
                    (if (s-starts-with-p "Dockerfile"
                                         (file-name-nondirectory (buffer-file-name)))
                        (dockerfile-mode)))))
(use-package docker-compose-mode :straight t
  :defer t)
(use-package org
  :defer t
  :config (progn
            (setq org-agenda-files (list "~/.local/share/org/master.org"))
            (setq org-log-done 'time)
            (setq org-todo-keywords '((sequence "TODO" "|" "DONE" "CANCELLED")))
            (global-set-key (kbd "M-f j")
                            'org-agenda-list)
            (evil-leader/set-key "m"
              (lambda ()
                (interactive)
                (find-file "~/.local/share/org/master.org")))
            (evil-define-key 'normal
              org-mode-map
              (kbd "M-l")
              'meain/move-swap-right)
            (evil-define-key 'normal
              org-mode-map
              (kbd "M-h")
              'meain/move-swap-left)
            (evil-define-key 'normal
              org-mode-map
              (kbd "M-k")
              'meain/move-swap-up)
            (evil-define-key 'normal
              org-mode-map
              (kbd "M-j")
              'meain/move-swap-down)
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
              (kbd "gH")
              'org-promote-subtree)
            (evil-define-key 'normal
              org-mode-map
              (kbd "gL")
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
(use-package try :straight t
  :commands try)

;; Winner mode
(use-package winner
  :config (progn
            (global-set-key (kbd "M-f <left>")
                            'winner-undo)
            (global-set-key (kbd "M-f <right>")
                            'winner-redo)
            (winner-mode)))

;; notmuch
(use-package notmuch
  :straight t
  :commands notmuch
  :config (progn
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
              'meain/move-swap-up)
            (evil-define-key 'normal
              notmuch-show-mode-map
              (kbd "M-j")
              'meain/move-swap-down)
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
              ;; make sure last message is open
              (notmuch-show-message-visible (notmuch-show-get-message-properties)
                                            t)
              (force-window-update))):init
  (progn
    (evil-leader/set-key "a n" 'notmuch)
    (setq notmuch-search-oldest-first nil)
    (setq notmuch-message-headers-visible nil)
    (setq message-auto-save-directory "/Users/meain/.local/share/mail")
    (setq notmuch-saved-searches (quote ((:name "imbox" :query "tag:imbox AND tag:inbox"
                                                :key "i"
                                                :sort-order newest-first)
                                         (:name "read" :query "tag:inbox AND -tag:imbox AND -tag:newsletter AND -tag:python and -tag:unread AND -tag:jobhunt AND -tag:git"
                                                :key "r"
                                                :sort-order oldest-first)
                                         (:name "meain" :query "query=to:mail@meain.io AND tag:inbox"
                                                :key "m"
                                                :sort-order oldest-first)
                                         (:name "github" :query "tag:github AND tag:inbox"
                                                :key "h"
                                                :sort-order oldest-first)
                                         (:name "sent" :query "tag:sent"
                                                :key "s"
                                                :sort-order newest-first)
                                         (:name "drafts" :query "tag:draft AND tag:inbox"
                                                :key "d")
                                         (:name "unread" :query "tag:unread AND tag:inbox AND -tag:python AND -tag:git AND -tag:newsletter AND -tag:jobhunt"
                                                :key "u")
                                         (:name "python" :query "tag:python AND tag:inbox"
                                                :key "p")
                                         (:name "git" :query "tag:git AND tag:inbox"
                                                :key "g")
                                         (:name "newsletter" :query "tag:newsletter AND tag:inbox"
                                                :key "n")
                                         (:name "jobhunt" :query "tag:jobhunt AND tag:inbox"
                                                :key "j")
                                         (:name "known" :query "tag:known AND tag:inbox"
                                                :key "k")
                                         (:name "archiveable" :query "tag:bullshit AND tag:known AND tag:nonimportant AND tag:inbox"
                                                :key "a")
                                         (:name "all mail" :query "*"
                                                :key "A"
                                                :sort-order newest-first))))))

;; sending emails
(setq mail-signature t)
(setq mail-signature-file "~/.config/datafiles/mailsignature")
(setq message-kill-buffer-on-exit t) ; kill buffer after sending mail
(setq mail-specify-envelope-from t) ; Settings to work with msmtp
(setq message-sendmail-envelope-from 'header)
(setq mail-envelope-from 'header)
(setq notmuch-fcc-dirs "\"[Gmail].Sent Mail\"") ; stores sent mail to the specified directory
(setq message-directory "[Gmail].Drafts") ; stores postponed messages to the specified directory
(setq sendmail-program "msmtp")
(setq send-mail-function 'smtpmail-send-it)
(setq message-sendmail-f-is-evil t)
(setq message-default-mail-headers "Cc: \nBcc: \n")
(setq message-sendmail-extra-arguments '("--read-envelope-from"))
(setq message-send-mail-function 'message-send-mail-with-sendmail)

;; elfeed
(use-package elfeed
  :straight t
  :commands (elfeed elfeed-update):init
  (progn
    (run-at-time nil
                 (* 6 60 60)
                 (lambda ()
                   (elfeed-update)
                   (elfeed-db-save))))
  :init (evil-leader/set-key "a e" 'elfeed):config
  (progn
    (setq elfeed-sort-order 'ascending)
    (setq browse-url-browser-function '(lambda (url &rest args)
                                         (interactive)
                                         (start-process "*open*" "*open*" "open" "-g"
                                                        url)
                                         (message "Opened %s" url)))
    (setq browse-url-generic-program "open")
    (setq browse-url-generic-args (list "-g"))
    (evil-define-key 'normal
      elfeed-search-mode-map
      (kbd "o")
      (lambda ()
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
      (kbd "c")
      'meain/elfeed-search-filter-by-name)
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
      (let ((tag (completing-read "Apply tag: "
                                  (remove-if (lambda (x)
                                               (equalp x 'unread))
                                             (delete-dups (flatten-list (cl-list* (with-current-buffer "*elfeed-search*"
                                                                                    (cl-loop for
                                                                                             entry
                                                                                             in
                                                                                             elfeed-search-entries
                                                                                             collect
                                                                                             (elfeed-entry-tags entry)))))))
                                  nil
                                  t
                                  "\\.")))
        (setq elfeed-search-filter (concatenate 'string "@2-months-ago +unread +"
                                                tag))
        (elfeed-search-update :force)
        (evil-goto-first-line)))
    (defun meain/elfeed-search-filter-by-name ()
      (interactive)
      (setq elfeed-search-filter (mapconcat 'identity
                                            (remove-if-not (lambda (x)
                                                             (or (string-prefix-p "+" x)
                                                                 (string-prefix-p "@" x)))
                                                           (split-string elfeed-search-filter))
                                            " "))
      (elfeed-search-update :force)
      (let ((site (completing-read "Look for: "
                                   (remove-if (lambda (x)
                                                (equalp x 'unread))
                                              (delete-dups (flatten-list (cl-list* (with-current-buffer "*elfeed-search*"
                                                                                     (cl-loop for
                                                                                              entry
                                                                                              in
                                                                                              elfeed-search-entries
                                                                                              collect
                                                                                              (cl-struct-slot-value (type-of (elfeed-entry-feed (car elfeed-search-entries)))
                                                                                                                    'title
                                                                                                                    (elfeed-entry-feed entry)))))))))))
        ;; Need \s- insted of just a simple space because elfeed has issues with space in title
        (setq elfeed-search-filter (concatenate 'string
                                                elfeed-search-filter
                                                " ="
                                                (mapconcat 'identity
                                                           (split-string site)
                                                           "\\s-")))
        (elfeed-search-update :force)
        (evil-goto-first-line)))
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
  :straight t
  :init (progn
          (defun meain/command-log-start ()
            "Enable command-log-mode and open command-log buffer."
            (interactive)
            (global-command-log-mode)
            (clm/open-command-log-buffer))))

;; Beacon mode
(use-package beacon
  :straight t
  :defer t
  :diminish :init
  (beacon-mode t))

;; Focus mode
(use-package focus :straight t
  :commands focus-mode)

;; Writing mode
(use-package writeroom-mode
  :straight t
  :commands writeroom-mode
  :config (progn
            (add-hook 'writeroom-mode-enable-hook
                      (lambda ()
                        (interactive)
                        (focus-mode t)))
            (add-hook 'writeroom-mode-disable-hook
                      (lambda ()
                        (interactive)
                        (focus-mode -1)))
            (remove 'writeroom-set-fullscreen writeroom-global-effects)))

;; tramp dired
(use-package tramp
  :disabled t
  :straight t
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
  :after tramp
  :straight t
  :commands tramp-term
  :init (defun meain/ssh-term ()
          "SSH into a server by selcting a host via autocomplete."
          (interactive)
          (tramp-term (list (meain/ssh-host-picker)))))

;; connect to docker via tramp
(use-package docker-tramp :straight t
  :defer t
  :after tramp)

;; timing stuff
(use-package activity-watch-mode
  :straight t
  :diminish :config
  (global-activity-watch-mode))

;; Markdown preivew
(defun meain/markdown-preview ()
  "Preview markdown.  Using pandoc under the hood."
  (interactive)
  (if (get-buffer "*markdown-preview*")
      (kill-buffer "*markdown-preview*")
    (start-process "*markdown-preview*" "*markdown-preview*"
                   "markdown-preview" buffer-file-name)))

;; Git info in dired buffer
(use-package dired-git-info
  :straight t
  :commands (dired-git-info-mode))

;; Restclient
(use-package restclient
  :straight t
  :defer t
  :mode (("\\.rest\\'" . restclient-mode)))

;; Link opening
(use-package ace-link
  :straight t
  :commands ace-link
  :init (global-set-key (kbd "M-f l")
                        'ace-link))

;; Docker
(use-package docker :straight t
  :defer t)

;; Kubernetes
(use-package kubel
  :straight t
  :defer t
  :config (progn
            (setq kubel-use-namespace-list t)
            (use-package kubel-evil
              :straight t
              :config (add-hook 'kubel-mode 'kubel-evil-mode))))

;; Window layout changer
(use-package rotate
  :straight t
  :commands (rotate-layout rotate-window):config
  (define-key evil-normal-state-map (kbd "M-f <SPC>") 'rotate-layout))

;; Remember
(use-package remember
  :commands remember
  :config (setq remember-data-file "~/.config/emacs/remember-notes"
                remember-notes-initial-major-mode 'org-mode
                remember-notes-auto-save-visited-file-name
                t))

;; Tree sitter
(use-package tree-sitter
  :straight t
  :config (progn
            (global-tree-sitter-mode)
            (defun meain/ts-get-class-like-thing ()
              (cond
               ((eq major-mode 'rust-mode) 'impl_item)
               ((eq major-mode 'python-mode) 'class_definition)))
            (defun meain/ts-get-func-like-thing ()
              (cond
               ((eq major-mode 'rust-mode) 'function_item)
               ((eq major-mode 'python-mode) 'function_definition)))
            (defun meain/ts-get-class-like-thing-name ()
              (let* ((node-at-point (tree-sitter-node-at-point (meain/ts-get-class-like-thing)))
                     (name (cond
                            ((eq node-at-point nil) "")
                            (t (format "%s."
                                       (thread-first (tsc-get-nth-named-child node-at-point 0)
                                         (tsc-node-text)))))))
                (format "%s" name)))
            (defun meain/ts-get-func-like-thing-name ()
              (let* ((node-at-point (tree-sitter-node-at-point (meain/ts-get-func-like-thing)))
                     (name (cond
                            ((eq node-at-point nil) "")
                            (t (thread-first (tsc-get-child-by-field node-at-point :name)
                                 (tsc-node-text))))))
                (format "%s" name)))
            (defun meain/highlight-function ()
              (interactive)
              (let* ((points (thread-first (tree-sitter-node-at-point (meain/ts-get-func-like-thing))
                               (tsc-node-byte-range))))
                (pulse-momentary-highlight-region (car points)
                                                  (cdr points)
                                                  'company-template-field)))
            (defun meain/highlight-class ()
              (interactive)
              (let* ((points (thread-first (tree-sitter-node-at-point (meain/ts-get-class-like-thing))
                               (tsc-node-byte-range))))
                (pulse-momentary-highlight-region (car points)
                                                  (cdr points)
                                                  'company-template-field)))))
(use-package tree-sitter-langs :straight t
  :after tree-sitter)



;;; [CUSTOM FUNCTIONS] ==============================================

;; Font size changes
(global-set-key (kbd "s-=")
                'text-scale-increase)
(global-set-key (kbd "s--")
                'text-scale-decrease)
(global-set-key (kbd "s-0")
                (lambda ()
                  (interactive)
                  (text-scale-set 0)))

;; host picker
(defun meain/ssh-host-picker ()
  "Interactively pick ssh host."
  (with-temp-buffer
    (insert-file-contents "~/.ssh/config")
    (format "%s"
            (completing-read "Choose host: "
                             (mapcar (lambda (x)
                                       (replace-regexp-in-string (regexp-quote "Host ")
                                                                 ""
                                                                 x))
                                     (remove-if-not #'(lambda (x)
                                                        (s-starts-with-p "Host" x))
                                                    (split-string (buffer-string)
                                                                  "\n")))))))

;; split between hirizontal and vertical (simpler emacs-rotate)
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

;; Quick open scratch buffers
(defun meain/scratchy ()
  "Open scratch buffer in a specific mode."
  (interactive "P")
  (let ((scratch-major-mode (completing-read "Choose mode: "
                                             '(text-mode python-mode json-mode rust-mode
                                                         markdown-mode emacs-lisp-mode web-mode javascript-mode
                                                         artist-mode)
                                             nil
                                             t
                                             nil
                                             nil
                                             (format "%s" major-mode)))
        (scratch-file-name (concatenate 'string
                                        "~/.cache/scratch/"
                                        (substring (uuid-string)
                                                   0
                                                   4))))
    (find-file scratch-file-name)
    (funcall (intern scratch-major-mode))
    (if (eq (intern scratch-major-mode) 'artist-mode)
        (evil-local-mode -1))))
(evil-leader/set-key "c"
  (meain/with-alternate (meain/create-or-switch-to-scratch)
                        (meain/scratchy)))

;; vime functionality within emacs
(use-package uuid :straight t
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
      (let ((selectrum-should-sort nil))
        (find-file (concat "~/.cache/vime/"
                           (car (split-string (completing-read "Choose vime: "
                                                               (mapcar (lambda (x)
                                                                         (meain/vime-name-append (car x)))
                                                                       (sort (remove-if-not #'(lambda (x)
                                                                                                (eq (nth 1 x) nil))
                                                                                            (directory-files-and-attributes "~/.cache/vime"))
                                                                             #'(lambda (x y)
                                                                                 (time-less-p (nth 6 y)
                                                                                              (nth 6 x)))))))))))
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
  (find-file (concatenate 'string
                          "~/.notes/"
                          (completing-read "Choose note: "
                                           (meain/nested-list-dir "~/.notes")))))
(evil-leader/set-key "a N" 'meain/open-note)

;; dasht docs
(defvar meain/dasht-server-port "1111" "Server port to be used for dast server.")
(defun meain/dasht-docs (start end)
  "Look up word at point in dasht.
START and END comes from it being interactive."
  (interactive "r")
  (let ((thing (if (use-region-p)
                   (buffer-substring start end)
                 (thing-at-point 'symbol))))
    (progn
      (if (eq (get-buffer "*dasht-server*") nil)
          (progn
            (message "Starting dasht-server")
            (start-process-shell-command "dasht-server"
                                         "*dasht-server*"
                                         (concat "dasht-server " meain/dasht-server-port))))
      (let* ((lookup-term (read-from-minibuffer "Lookup term: " thing))
             (dasht-server-url (concat "http://127.0.0.1:" meain/dasht-server-port))
             (full-url (concatenate 'string
                                    dasht-server-url
                                    "/?"
                                    "query="
                                    lookup-term
                                    "&docsets="
                                    (completing-read "Docset: "
                                                     (split-string (shell-command-to-string "dasht-docsets"))))))
        (message full-url)
        (eww full-url)))))
(evil-leader/set-key "a d" 'meain/dasht-docs)

;; cheat.sh
(use-package cheat-sh
  :straight t
  :commands cheat-sh-maybe-region
  cheat-sh
  :init (evil-leader/set-key "a c" 'cheat-sh-maybe-region))

;; Quick edit (for use with hammerspoon quick edit)
(defun meain/quick-edit-end ()
  "Util function to be executed on qed completion."
  (interactive)
  (mark-whole-buffer)
  (call-interactively 'kill-ring-save)
  (meain/kill-current-buffer-unless-scratch))
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

;; Quick launch htop
(evil-leader/set-key "a H"
  (lambda ()
    (interactive)
    (meain/run-in-vterm "htop")))

;; Journal entry
(add-hook 'find-file-hook
          (lambda ()
            (if (string-prefix-p (expand-file-name "~/.local/share/journal")
                                 default-directory)
                (auto-fill-mode))))
(evil-leader/set-key "a J"
  (lambda ()
    "Start writing journal entry.  `journal' invokes emacsclient and gives control back over to Emacs."
    (interactive)
    (start-process-shell-command "journal" "*journal*"
                                 "EDITOR='emacsclient' journal")))

;; Function to close Emacs in a more "proper" way
(defun meain/kill-all-buffers ()
  "Kill all active buffers."
  (interactive)
  (message "Just use `save-buffers-kill-emacs'"))

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
(global-set-key (kbd "M-f m")
                'bookmark-jump)
(global-set-key (kbd "M-f M")
                'bookmark-set)

;; Quick file rename
(defun meain/rename-current-file ()
  "Rename the current visiting file and switch buffer focus to it."
  (interactive)
  (if (null (buffer-file-name))
      (user-error "Buffer does not have a filename: %s"
                  (current-buffer)))
  (let ((new-filename (meain/expand-filename-prompt (format "Rename %s to: "
                                                            (file-name-nondirectory (buffer-file-name))))))
    (if (null (file-writable-p new-filename))
        (user-error "New file not writable: %s" new-filename))
    (rename-file (buffer-file-name)
                 new-filename
                 1)
    (find-alternate-file new-filename)
    (message "Renamed to and now visiting: %s"
             (abbreviate-file-name new-filename))))
(defun meain/expand-filename-prompt (prompt)
  "Return expanded filename PROMPT."
  (expand-file-name (read-file-name prompt)))
(defalias 'rename 'meain/rename-current-file)

;; setting proper default-dir
(defun meain/set-proper-default-dir ()
  "Function to set the `default-directory' value as the project root if available."
  (interactive)
  (setq default-directory (cond
                           ((projectile-project-p)
                            (projectile-project-root))
                           (t "~/"))))
(add-hook 'find-file-hook 'meain/set-proper-default-dir)

;; Quikly add markdown links to document
(defun meain/linkify-thing (start end)
  "Function to search and add markdown links to document.  START and END for position."
  (interactive "r")
  (let* ((thang (if (use-region-p)
                    (buffer-substring start end)
                  (thing-at-point 'symbol)))
         (json-object-type 'plist)
         (json-array-type 'list)
         (lurl (car (split-string (completing-read (format "Choose URL (%s): " thang)
                                                   (mapcar (lambda (entry)
                                                             (string-join (list (plist-get entry :url)
                                                                                " :: "
                                                                                (plist-get entry :title))))
                                                           (json-read-from-string (shell-command-to-string (string-join (list "ddgr --json '" thang "'"))))))
                                  " "))))
    (save-excursion
      (if (use-region-p)
          (kill-region start end)
        (kill-region (beginning-of-thing 'symbol) (end-of-thing 'symbol)))
      (insert (format "[%s](%s)" thang lurl)))))

;; Open current file in Github
(defun meain/github-url (&optional no-linenumber)
  "Open the Github page for the current file.  Pass NO-LINENUMBER to not add a line number."
  (interactive "P")
  (let* ((git-url (replace-regexp-in-string "\.git$"
                                            ""
                                            (s-replace "git@github\.com:"
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

;; Generate pdf from markdown document
(defun meain/markdown-pdf ()
  "Generate pdf from markdown document."
  (interactive)
  (message "Generating markdown for %s. Just give it a moment.."
           (buffer-file-name))
  (start-process-shell-command "*markdown-pdf*"
                               "*markdown-pdf*"
                               (concatenate 'string
                                            "pandocmarkdownpreview "
                                            (buffer-file-name))))

;; Run markdown code blocks (forest.el)
(defun meain/run-markdown-code-block ()
  "Run markdown code block under curosr."
  (interactive)
  (let* ((start (nth 0
                     (markdown-get-enclosing-fenced-block-construct)))
         (end (nth 1
                   (markdown-get-enclosing-fenced-block-construct)))
         (snippet-with-markers (buffer-substring start end))
         (snippet (string-join (cdr (butlast (split-string snippet-with-markers "\n")))
                               "\n"))
         (snippet-runner (car (last (split-string (car (split-string snippet-with-markers "\n"))
                                                  "[ `]+")))))
    (message "%s" start)
    (message "%s" end)
    (pulse-momentary-highlight-region start end
                                      'company-template-field)
    (message "%s"
             (markdown-get-enclosing-fenced-block-construct))
    (message "%s" snippet-with-markers)
    (message "Code: %s" snippet)
    (message "Runner: %s" snippet-runner)
    (goto-char end)
    (end-of-line)
    (newline)
    (insert "\n```output\n")
    (append-to-file snippet nil "/tmp/thing-to-run")
    (message "%s"
             (format "%s '/tmp/thing-to-run'" snippet-runner))
    (insert (shell-command-to-string (format "%s '/tmp/thing-to-run'" snippet-runner)))
    (insert "```")
    (delete-file "/tmp/thing-to-run" t)))

;; Fetch mail
(defun meain/fetchmail ()
  "Fetch email using mailsync."
  (interactive)
  (async-shell-command "mailsync"))

;; search from emacs using xwidgets
(defun meain/search-xwidget ()
  "Search from Emacs using xwidgets."
  (interactive)
  (xwidget-webkit-browse-url (concat "https://duckduckgo.com/?q="
                                     (read-string "Search term: "
                                                  (thing-at-point 'symbol)))))

(defun meain/xwidgets-dasht-docs (start end)
  "Look up word at point in dasht.
START and END comes from it being interactive."
  (interactive "r")
  ;; http://127.0.0.1:54321/?query=print&docsets=Python
  (let ((thing (if (use-region-p)
                   (buffer-substring start end)
                 (thing-at-point 'symbol))))
    (if (eq (length thing) 0)
        (message "Nothing to look up.")
      (progn
        (let ((lookup-term (read-from-minibuffer "Lookup term: " thing)))
          (xwidget-webkit-browse-url (concatenate 'string
                                                  "http://127.0.0.1:54321/?query="
                                                  lookup-term
                                                  "&docsets="
                                                  (completing-read "Docset: "
                                                                   (split-string (shell-command-to-string "dasht-docsets"))))))))))


;; Some custom text objects
;; TODO: Make a more generic textobject over any tree sitter query
(load-file "~/.config/emacs/evil-textobj-function.el")

;; Better modeline
(use-package mode-line-idle
  :straight t
  :commands (mode-line-idle))
(setq-default mode-line-format (list '(:eval (if (eq 'emacs evil-state)
                                                 "! "
                                               ": ")) ;; vim or emacs mode

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
                                                                 'minibuffer-prompt)
                                                               'help-echo
                                                               (buffer-file-name))))
                                     '(:eval (mode-line-idle 1.0
                                                             '(:propertize (:eval (when-let (vc vc-mode) ;; git branch
                                                                                    (list " @"
                                                                                          (substring vc 5))))
                                                                           face
                                                                           font-lock-comment-face)
                                                             ""))
                                     '(:eval (mode-line-idle 1.0
                                                             '(:propertize (:eval (if projectile-mode
                                                                                      (list " "
                                                                                            (let* ((explicit (cdr (car (cdr (cdr (tab-bar--current-tab))))))
                                                                                                   (name (cdr (car (cdr (tab-bar--current-tab)))))
                                                                                                   (out-name (if explicit
                                                                                                                 (concatenate 'string ":" name)
                                                                                                               (if (projectile-project-p)
                                                                                                                   (concatenate 'string
                                                                                                                                ";"
                                                                                                                                (projectile-project-name))
                                                                                                                 ""))))
                                                                                              (format "%s" out-name)))))
                                                                           face
                                                                           font-lock-comment-face)
                                                             ""))
                                     " "
                                     '(:eval (mode-line-idle 0.3
                                                             '(:eval (if tree-sitter-mode
                                                                         (meain/ts-get-class-like-thing-name)))
                                                             "#"))
                                     '(:eval (mode-line-idle 0.3
                                                             '(:eval (if tree-sitter-mode
                                                                         (meain/ts-get-func-like-thing-name)))
                                                             "$"))
                                     ;; spacer

                                     '(:eval (propertize " "
                                                         'display
                                                         `((space :align-to (- (+ right right-fringe right-margin)
                                                                               ,(+ 2
                                                                                   (+ (string-width (format-mode-line "%l:%c %p"))
                                                                                      (string-width (format-mode-line "%m")))))))))
                                     (propertize "%l:%c %p" 'face 'font-lock-variable-name-face) ;; position in file
                                     (propertize " %m " 'face 'font-lock-constant-face) ;; current mode
                                     ))

;; Print emacs startup time
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;; Start server once we have emacs running
(require 'server)
(unless (server-running-p)
  (server-start))

(provide 'init)
;;; init ends here
