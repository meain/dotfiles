;;; init -- meain's Emacs config -*- lexical-binding: t -*-

;;; Commentary:
;;                    ____,
;;                   /.---|
;;                   `    |     ___
;;                       (=\.  /-. \
;;                        |\/\_|"|  |
;;                        |_\ |;-|  ;
;;                        | / \| |_/ \
;;                        | )/\/      \
;;                        | ( '|  \   |
;;                        |    \_ /   \
;;                        |    /  \_.--\
;;                        \    |    (|\`
;;                         |   |     \
;;                         |   |      '.
;;                         |  /         \
;;                         \  \.__.__.-._)
;;
;;
;; Well, hello there! How are you doing wanderer? Looking for some
;; lisp goodness?  You might find it here, you might not.  If you do
;; find what you are looking for here, feel free take them with you,
;; give them a new life, a new filesystem, a new home.  All I ask of
;; you is to treat them with love and care.  They have always been
;; with me, playing along with my musing, catching little typos and
;; finding little bugs.  They stuck strong to my side even when the
;; Rust borrow checker came for me.  I'm not gonna lie, there has been
;; many a times where I have doubted my skills, but they have always
;; believed in me.
;;
;; If these parenthesis could talk, they would have a lot of stories
;; to tell.  Some good, some bad, some really ugly.  But at the end of
;; the day, I'm sure they are all happy to be where they are.
;;
;; If they give you any trouble, my GitHub issues is always open
;; unlike the doors of heaven.  They probably won't, these are the
;; good ones, but God sometimes have different plans, and everyone
;; gets hit with hard times.
;;
;; Good luck!

;;; Code:

;;; [PACKAGE SETUP] =============================================

;; Basic setup
(setq user-mail-address "mail@meain.io" user-full-name "Abin Simon")
(defvar openai-api-key
  (string-trim (shell-command-to-string "pass show openai/apikey 2>/dev/null") "\n" "\n")
  "OpenAI API key.")

;; Setup elpaca
(defvar elpaca-installer-version 0.6)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (defvar elpaca--core-date (list (string-to-number (format-time-string "%Y%m%d" emacs-build-time)))) ;; XXX: Hack to get elpaca to work
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (call-process "git" nil buffer t "clone"
                                       (plist-get order :repo) repo)))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; XXX: Hack since elpaca cannot figure out versions
(require 'cl-lib)
(setq elpaca-build-steps (cl-remove 'elpaca--check-version elpaca-build-steps))

;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable :elpaca use-package keyword.
  (elpaca-use-package-mode))

;; Block until current queue processed.
(elpaca-wait)

;; Use package config
(setq use-package-verbose t)
(setq use-package-always-demand (getenv "LOAD_FULL_EMACS"))
(setq use-package-compute-statistics t) ;; Run `use-package-statistics' to get load timings

;; Benchmark emacs startup (enable when necessary)
(use-package benchmark-init
  :elpaca t
  :disabled :config
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

;; Get proper PATH (not used as we are launching from shell)
;; (use-package exec-path-from-shell
;;   :elpaca t
;;   :config
;;   ;; https://github.com/purcell/exec-path-from-shell#making-exec-path-from-shell-faster
;;   (setq exec-path-from-shell-arguments '("-l")) ;; removing -i
;;   (exec-path-from-shell-initialize))

;;; [BASE EVIL] =================================================

;; Evil mode (set this up first)
(use-package evil
  :elpaca t

  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-minibuffer nil) ; messes with esc to quit
  (setq evil-undo-system 'undo-tree)
  (setq evil-kill-on-visual-paste nil)
  (setq evil-respect-visual-line-mode nil)
  (setq evil-symbol-word-search t)

  :config
  (evil-mode t)
  (defalias #'forward-evil-word #'forward-evil-symbol)

  ;; Enable/disable certain jump targets for C-o and C-i
  (evil-set-command-property 'evil-visual-char :jump t)
  (evil-set-command-property 'evil-visual-line :jump t)
  (evil-set-command-property 'evil-backward-paragraph :jump nil)
  (evil-set-command-property 'evil-forward-paragraph :jump nil)
  (evil-set-command-property 'evil-search-next :jump nil)
  (evil-set-command-property 'evil-search-previous :jump nil)

  ;; Up/Down on visual instead of actual lines
  (define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)

  (defun meain/recenter-advice (orig-fn &rest args)
    "Used to recenter the buffer after `ORIG-FN' passing down `ARGS' down to it."
    (apply orig-fn args)
    (recenter))
  (defun meain/recenter-top-advice (orig-fn &rest args)
    "Used to recenter the buffer after `ORIG-FN' passing down `ARGS' down to it."
    (apply orig-fn args)
    (recenter 13))

  ;; (advice-add 'evil-jump-forward :around #'meain/recenter-advice)
  ;; (advice-add 'evil-jump-backward :around #'meain/recenter-advice)
  ;; (advice-add 'evil-search-next :around #'meain/recenter-top-advice)
  ;; (advice-add 'evil-search-previous :around #'meain/recenter-top-advice)
  )

;; Evil leader
(use-package evil-leader
  :elpaca t
  :after evil
  :config
  (global-evil-leader-mode)
  (evil-leader/set-leader "s"))

;;; [BASIC SETTINGS] =============================================

;; Consistent window title
(setq frame-title-format '("Emacs")) ; needed by hammerspoon

;; Don't automatically add newlines at end of files
(setq mode-require-final-newline nil)

;; Fix some cmd keybinds
(global-set-key (kbd "s-v") 'yank)
(global-set-key (kbd "s-a") 'mark-whole-buffer)
(global-set-key (kbd "s-c") 'kill-ring-save)

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
(setq backup-directory-alist `((".*" . "~/.local/share/emacs/backup")))
(setq auto-save-file-name-transforms `((".*" "~/.local/share/emacs/autosave" t)))

;; Don't create lockfiles
(setq create-lockfiles nil)

;; Don't clutter my emacs conf
;; Will have to call load-file in case I actually need this on next startup
;; Just keeping it as a file so that I can copy paste
(setq custom-file "~/.config/emacs/custom-config.el")
(load-file "~/.config/emacs/custom-config.el") ;; TODO: only load `safe-local-variable-values'

;; Tab settings
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

;; Delete visual selection when I start typing
(delete-selection-mode 1)

;; Meta-f keybinds (tmux memories)
(global-unset-key (kbd "M-f")) ; have to unset first

;; Disable visual line mode (this causes issues with $ and a few other things in evil)
(global-visual-line-mode -1)

;; Let Emacs know that I can handle `narrow'
(put 'narrow-to-region 'disabled nil)

;;; [VISUAL CONFIG] ==============================================

;; Change font everywhere
(defun meain/set-fonts ()
  "Set fonts for everything."
  (set-face-attribute 'default nil :font meain/font-family-default :weight meain/font-weight-default)
  (set-face-attribute 'fixed-pitch nil :font meain/font-family-default :weight meain/font-weight-default)
  (set-face-attribute 'variable-pitch nil :font meain/font-family-default :weight meain/font-weight-default))
(defun meain/select-font ()
  "Select and set a font."
  (interactive)
  (let ((font-name (completing-read "Choose font: " (remove-duplicates (font-family-list)))))
    (let ((family (meain/get-font-prop font-name 'family))
          (weight (meain/get-font-prop font-name 'weight)))
      (set-frame-font family)
      (set-face-attribute 'default nil :font family :weight weight)
      (set-face-attribute 'fixed-pitch nil :font family :weight weight)
      (set-face-attribute 'variable-pitch nil :font family :weight weight)
      (setq-default line-spacing (meain/get-font-prop font-name 'line-spacing)))))

;; Bell: audio -> visual
(setq visible-bell nil)
(setq ring-bell-function (lambda ()
                           (unless (memq this-command
                                         '(isearch-abort abort-recursive-edit exit-minibuffer keyboard-quit))
                             (invert-face 'mode-line)
                             (invert-face 'header-line)
                             (run-with-timer 0.1 nil 'invert-face 'mode-line)
                             (run-with-timer 0.1 nil 'invert-face 'header-line))))
(defun meain/what-font-am-i-using ()
  "Show the name/details for the current font in use."
  (interactive)
  (message "%s" (face-attribute 'default :font)))

;; Theme
(use-package hima-theme
  :load-path "/home/meain/dev/src/hima-theme"
  :config
  (load-theme 'hima t))

;; Diminish
(use-package diminish
  :elpaca t
  :defer t
  :init
  (diminish 'eldoc-mode)
  (diminish 'auto-revert-mode))

;;; [BASIC BUILTINS] ===========================================

;; Use mouse to do some stuff when you are lazy
;; TODO: Causes Emacs to freeze when open
(context-menu-mode nil)

;; Show open and closing brackets
(show-paren-mode t)
(setq show-paren-delay 0)
(setq show-paren-style 'parenthesis)

;; Keep files in sync with filesystem
;; TODO: Use use-package
(global-auto-revert-mode t)

(defun meain/silence-auto-revert-errors (orig-fun &rest args)
  "Silence errors from `auto-revert-buffers'.
`ORIG-FUN' will be `auto-revert-buffer' and `ARGS' are args to it."
  (ignore-errors
    (apply orig-fun args)))
(advice-add 'auto-revert-buffers :around #'meain/silence-auto-revert-errors)

(setq auto-revert-interval 2)
(setq auto-revert-check-vc-info t)
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose t)

;; Disable line wrapping
(use-package emacs
  :after evil-leader
  :config
  (setq-default truncate-lines 1)
  (evil-leader/set-key "b w" 'toggle-truncate-lines))

;; auto-fill
(use-package emacs
  :after evil-leader
  :config
  (evil-leader/set-key "b F" 'auto-fill-mode))

;; Cursor blink
(blink-cursor-mode -1)

;; Follow symlinks for vc
(setq vc-follow-symlinks t)

;; auto-pair
(electric-pair-mode t)

;; move the mouse out of the way on cursor
(mouse-avoidance-mode 'cat-and-mouse)

;; Enable recentf
(use-package recentf
  :defer t
  :init
  (recentf-mode t)
  (add-hook 'after-init-hook
            (lambda ()
              (with-current-buffer "*scratch*"
                (goto-char (point-max))
                (insert initial-scratch-message)
                (newline 2)
                (mapcar (lambda (x)
                          (insert "\n")
                          (insert-button
                           (string-join (reverse (cl-subseq (reverse (split-string x "/")) 0 2)) "/")
                           'action (lambda (_button) (find-file x))
                           'follow-link t))
                        (cl-subseq recentf-list 0 (min 3 (length recentf-list))))))))


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
  :elpaca t
  :defer 1
  :diminish
  :config (evil-commentary-mode))

;; Evil surround
(use-package evil-surround
  :defer 1
  :elpaca t
  :config (global-evil-surround-mode 1))

;; Evil text objects
(use-package evil-textobj-line :elpaca t :defer 1)
(use-package evil-textobj-syntax :elpaca t :defer 1)
(use-package evil-indent-plus
  :elpaca t
  :defer 1
  :config
  (define-key evil-inner-text-objects-map "i" 'evil-indent-plus-i-indent)
  (define-key evil-outer-text-objects-map "i" 'evil-indent-plus-a-indent)
  (define-key evil-inner-text-objects-map "I" 'evil-indent-plus-i-indent-up)
  (define-key evil-outer-text-objects-map "I" 'evil-indent-plus-a-indent-up)
  (define-key evil-inner-text-objects-map "J" 'evil-indent-plus-i-indent-up-down)
  (define-key evil-outer-text-objects-map "J" 'evil-indent-plus-a-indent-up-down))

;; Evil number increment
(use-package evil-numbers
  :elpaca t
  :after (evil)
  :commands (evil-numbers/inc-at-pt-incremental evil-numbers/dec-at-pt-incremental)
  :init
  ;; cannot directly use C-x (in use by emacs)
  (define-key evil-normal-state-map (kbd "g C-a") 'evil-numbers/inc-at-pt-incremental)
  (define-key evil-normal-state-map (kbd "g C-x") 'evil-numbers/dec-at-pt-incremental))

;; Save buffer
(use-package emacs
  :after evil
  :init
  (define-key evil-normal-state-map (kbd "<SPC> <SPC>") 'evil-write))

;; Hit universal arg without ctrl

(use-package emacs
  :after evil-leader
  :config
  (evil-leader/set-key "u" 'universal-argument))
(global-set-key (kbd "M-u") 'universal-argument)
(define-key universal-argument-map (kbd "M-u") 'universal-argument-more)

;; Auto resize windows (useful in go buffer, folks don't stop at 80)
(use-package golden-ratio
  :elpaca t
  ;; Enable minor-mode manually when required
  :commands (golden-ratio golden-ratio-mode))

;; Window mappings (not using meain/with-alternate as I need functions)
(defun meain/move-swap-right (&optional swap)
  "Move to window on right or move window to right if SWAP."
  (interactive "P")
  (if swap
      (windmove-swap-states-right)
    (windmove-right))
  (golden-ratio))
(global-set-key (kbd "M-l") 'meain/move-swap-right)
(defun meain/move-swap-left (&optional swap)
  "Move to window on left or move window to left if SWAP."
  (interactive "P")
  (if swap
      (windmove-swap-states-left)
    (windmove-left))
  (golden-ratio))
(global-set-key (kbd "M-h") 'meain/move-swap-left)
(defun meain/move-swap-up (&optional swap)
  "Move to window on top or move window to top if SWAP."
  (interactive "P")
  (if swap
      (windmove-swap-states-up)
    (windmove-up))
  (golden-ratio))
(global-set-key (kbd "M-k") 'meain/move-swap-up)
(defun meain/move-swap-down (&optional swap)
  "Move to window on bottom or move window to bottom if SWAP."
  (interactive "P")
  (if swap
      (windmove-swap-states-down)
    (windmove-down))
  (golden-ratio))
(global-set-key (kbd "M-j") 'meain/move-swap-down)
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
(global-set-key (kbd "M-w") 'delete-window)

;; Eshell config
(use-package eshell
  :init (global-set-key (kbd "M-;") 'meain/eshell-toggle)
  :after (vc copilot)
  :commands (meain/eshell-toggle eshell)
  :config
  (add-hook 'eshell-mode-hook (lambda () (copilot-mode -1)))
  (defun meain/eshell-name ()
    "Get the name of the eshell based on project info."
    (format "*popup-eshell-%s*"
            (if (project-current)
                (meain/project-name)
              "-")))
  (defun meain/eshell-toggle ()
    (interactive)
    (if (s-starts-with-p "*popup-eshell" (buffer-name))
        (quit-window)
      (let ((existing-eshell (get-buffer (meain/eshell-name))))
        (if existing-eshell (pop-to-buffer (meain/eshell-name))
          (progn
            (eshell t)
            (rename-buffer (meain/eshell-name)))))))
  (setq eshell-prompt-function
        (lambda ()
          (concat
           (propertize (string-join
                        (reverse
                         (ntake 2 (reverse
                                   (split-string
                                    (eshell/pwd) "/")))) "/")
                       'face `(:foreground "#93a1a1"))
           (propertize (if (car (vc-git-branches))
                           (concat "[" (car (vc-git-branches)) "]")
                         "") 'face `(:foreground "#93a1a1"))
           " "
           )))

  (add-to-list 'display-buffer-alist '("\\*popup-eshell-.*"
                                       (display-buffer-reuse-window display-buffer-at-bottom)
                                       (reusable-frames . visible)
                                       (window-height . 0.3)))
  (add-hook 'eshell-mode-hook (lambda ()
                                (setenv "TERM" "xterm-256color")
                                (define-key eshell-mode-map (kbd "M-l") 'meain/move-swap-right)
                                (define-key eshell-mode-map (kbd "M-h") 'meain/move-swap-left)
                                (define-key eshell-mode-map (kbd "M-k") 'meain/move-swap-up)
                                (define-key eshell-mode-map (kbd "M-j") 'meain/move-swap-down))))

(use-package sticky-shell
  :elpaca (sticky-shell :host github
                        :repo "andyjda/sticky-shell")
  :after eshell
  :commands (sticky-shell-mode)
  :init
  (add-hook 'eshell-mode-hook 'sticky-shell-mode))

;; ansi-term config
(use-package term
  :disabled t
  :config
  (defun meain/term-exec-hook ()
    "Automatically close `ansi-term' buffer on exit."
    (let* ((buff (current-buffer))
           (proc (get-buffer-process buff)))
      (set-process-sentinel
       proc
       `(lambda (process event)
          (if (string= event "finished\n")
              (kill-buffer ,buff))))))
  (add-hook 'term-exec-hook 'meain/term-exec-hook)
  (add-hook 'term-mode-hook (lambda ()
                              (setenv "TERM" "xterm-256color")
                              (define-key term-mode-map (kbd "M-l") 'meain/move-swap-right)
                              (define-key term-mode-map (kbd "M-h") 'meain/move-swap-left)
                              (define-key term-mode-map (kbd "M-k") 'meain/move-swap-up)
                              (define-key term-mode-map (kbd "M-j") 'meain/move-swap-down))))

;; Shrink and enlarge windows (not contextual as of now)
;; https://www.emacswiki.org/emacs/WindowResize
(defmacro meain/inlambda (functionname &rest args)
  "Create an interactive lambda of existing function `FUNCTIONNAME' with `ARGS'."
  (let ((funsymbol (concat "ilambda/" (symbol-name functionname))))
    `(cons ,funsymbol (lambda () (interactive) (apply #',functionname ',args)))))
(defmacro meain/ilambda (functionname &rest args)
  "Create an interactive lambda of existing function `FUNCTIONNAME' with `ARGS'."
  `(lambda () (interactive) (apply #',functionname ',args)))
(global-set-key (kbd "M-H") (meain/inlambda shrink-window-horizontally 5))
(global-set-key (kbd "M-L") (meain/inlambda enlarge-window-horizontally 5))
(global-set-key (kbd "M-K") (meain/inlambda shrink-window 5))
(global-set-key (kbd "M-J") (meain/inlambda enlarge-window 5))

;; Switch to other frame
(use-package emacs
  :after evil-leader
  :config
  (evil-leader/set-key "a f" 'other-frame))

;; Easier C-c C-c
(use-package emacs
  :after evil-leader
  :config
  (evil-leader/set-key "i"
    '(lambda ()
       (interactive)
       (execute-kbd-macro (kbd "C-c C-c")))))

;; Remap macro recoring key
(use-package emacs
  :after evil
  :init
  (define-key evil-normal-state-map "Q" 'evil-record-macro))

;; Eval region
(use-package emacs
  :after evil
  :init
  (define-key evil-visual-state-map (kbd ";") (lambda ()
                                                (interactive)
                                                (call-interactively 'eval-region)
                                                (evil-force-normal-state))))

;; Quick quit
(defun meain/update-scratch-message ()
  "Update scratch buffer contents to reflect open buffers and unread emails."
  (interactive)
  (with-current-buffer "*scratch*"
    (save-restriction
      (widen)
      (save-excursion
        (goto-char 1)
        ;; kill-line without copying to clipboard
        (delete-region (point) (save-excursion (end-of-line 2) (point)))
        (insert (format ";; Time is %s. You have %s unread mails and %s buffers.\n;; %s"
                        (format-time-string "%l %p")
                        (car (split-string (shell-command-to-string ",mail-unread notodo|wc -l") "\n"))
                        (cl-count-if (lambda (b)
                                       (or (buffer-file-name b)
                                           (not (string-match "^ " (buffer-name b)))))
                                     (buffer-list))
                        (car (split-string (shell-command-to-string ",weather-current")
                                           "\n"))))))))
(defun meain/create-or-switch-to-scratch ()
  "Switch to scratch buffer if exists, else create a scratch buffer with our config."
  (cond
   ((get-buffer "*scratch*")
    (switch-to-buffer "*scratch*"))
   (t (progn
        (switch-to-buffer "*scratch*")
        (setq default-directory "~/")
        (lisp-interaction-mode)
        (meain/update-scratch-message)))))
(defun meain/recreate-scratch ()
  "Recreate scratch buffer by just replacing the entire thing with new fortune."
  (interactive)
  (with-current-buffer "*scratch*")
  (erase-buffer)
  (meain/update-scratch-message))

(use-package emacs
  :after evil
  :init
  (defun meain/kill-current-buffer-unless-scratch ()
    "Kill current buffer if it is not scratch."
    (interactive)
    (if (= (length (mapcar #'window-buffer
                           (window-list))) 1)
        ;; TODO: optional delete frame advice on things that close (notmuch, elfeed)
        (if (equal "emacs-popup" (cdr (assq 'name (frame-parameters))))
            (delete-frame)
          (meain/create-or-switch-to-scratch))
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
  (define-key evil-normal-state-map (kbd "q") 'meain/kill-current-buffer-unless-scratch))

;; Y to y$
(use-package emacs
  :after evil
  :init
  (defun meain/yank-till-line-end ()
    "Yank till end of line."
    (interactive)
    (evil-yank (point)
               ;; subtracting 1 for newline
               (- (save-excursion (forward-line) (point)) 1)))
  (define-key evil-normal-state-map (kbd "Y") 'meain/yank-till-line-end))

;; Quit out of everything with esc
(defun meain/keyboard-quit ()
  "Quit out of whatever."
  (interactive)
  ;; Delete frame if it is a minbuffer only popup
  (if (and (equal (cdr (assq 'name (frame-parameters))) "emacs-popup")
           (equal (cdr (assq 'minibuffer (frame-parameters))) 'only))
      (delete-frame))
  (keyboard-escape-quit)
  (minibuffer-keyboard-quit)
  (keyboard-quit))
(global-set-key [escape] 'meain/keyboard-quit)

;; Quick replace
(use-package emacs
  :after evil
  :init
  (define-key evil-normal-state-map (kbd "<SPC> ;") (cons "replace in buffer" (meain/ilambda evil-ex "%s/")))
  (define-key evil-visual-state-map (kbd "<SPC> ;") (cons "replace in buffer"(meain/ilambda evil-ex "'<,'>s/"))))

;; Highlight yanked region
(defun meain/evil-yank-advice (orig-fn beg end &rest args)
  "Advice to be added to `evil-yank' to highlight yanked region.
Pass ORIG-FN, BEG, END, TYPE, ARGS."
  (pulse-momentary-highlight-region beg end 'mode-line)
  (apply orig-fn beg end args))
(advice-add 'evil-yank :around 'meain/evil-yank-advice)

;; Recompile binding
(use-package compile
  :commands (compile recompile)
  :after (evil)
  :config
  (setq compilation-always-kill t)
  (setq compilation-ask-about-save nil)
  (setq compilation-scroll-output nil)
  (defun meain/toggle-compilation-scroll-output ()
    "Toggle `compilation-scroll-output'."
    (interactive)
    (setq compilation-scroll-output (not compilation-scroll-output))
    (message "Compilation scroll set to %s" compilation-scroll-output))
  (evil-set-initial-state 'comint-mode 'normal)
  (defun meain/prettify-compilation (&rest _)
    "Few thing to prettify compilation buffer."
    (with-current-buffer "*compilation*"
      (toggle-truncate-lines -1)
      (highlight-regexp "FAIL: .*" 'diff-refine-removed)
      (highlight-regexp "=== RUN .*" 'ffap)))
  (advice-add 'compile :after 'meain/prettify-compilation)
  (defun meain/compilation-colorcode (_buffer string)
    "Change background color of compilation `_BUFFER' to red on failure."
    (if (string-prefix-p "finished" string)
        (face-remap-add-relative 'default 'diff-hl-insert)
      (face-remap-add-relative 'default 'diff-hl-delete)))
  (add-to-list 'compilation-finish-functions 'meain/compilation-colorcode)
  (defun meain/run-default ()
    (interactive)
    (compile (concat ".mscripts/default "
                     (read-string ".mscripts/default "))))
  :init
  (evil-leader/set-key "r"
    (meain/with-alternate
     (call-interactively 'recompile)
     (call-interactively 'compile))))

(use-package compile-multi
  :elpaca t
  :defer t
  :after (compile evil-leader)
  :commands (compile-multi)
  :config

  ;; (setq compile-multi-config '())
  (defun meain/compile-multi-mscripts-targets ()
    "Targets from mscripts"
    (let ((mscripts-dir (expand-file-name (concat (project-root (project-current)) "/.mscripts"))))
      (if (file-exists-p mscripts-dir)
          (mapcar (lambda (x)
                    (cons (concat "mscripts:" (file-name-base x))
                          (concat ".mscripts/" (file-name-base x))))
                  (remove-if
                   (lambda (x) (file-directory-p (concat ".mscripts/" x)))
                   (directory-files mscripts-dir t "^[^\.]"))))))

  (push `((file-directory-p (concat (project-root (project-current)) "/.mscripts"))
          ,#'meain/compile-multi-mscripts-targets)
        compile-multi-config)

  (setq compile-multi-default-directory
        (lambda ()
          (if (boundp 'custom-src-directory)
              custom-src-directory
            default-directory)))

  (push '((file-exists-p "Makefile")
          ("make:build" . "make build")
          ("make:test" . "make test")
          ("make:all" . "make all"))
        compile-multi-config)

  (push '((file-exists-p "go.mod")
          ("go:run" . "go run ./...")
          ("go:build" . "go build ./...")
          ("go:build-tests" . "go test ./... -run xxxxx")
          ("go:test" . "go test ./..."))
        compile-multi-config)

  (defun meian/byte-compile-this-file ()
    "`byte-compile' current file."
    (byte-compile-file (buffer-file-name)))
  (push `(emacs-lisp-mode
          ("emacs:bytecompile" . ,#'meain/byte-compile-this-file))
        compile-multi-config)

  :init (evil-leader/set-key "R" 'compile-multi))

;; Simplify how Async Shell Command buffers get displayed
;; (add-to-list 'display-buffer-alist
;;   '("\\*Async Shell Command\\*.*" display-buffer-no-window))
(add-to-list 'display-buffer-alist
             '("\\*Async Shell Command\\*.*"
               (display-buffer-reuse-window display-buffer-at-bottom)
               (reusable-frames . visible)
               (window-height . 0.1)))

;; Code coverage in buffer
;; To get coverage, run `go test -coverprofile=coverage.out ./...`
;; and then convert this to lcov using
;; gcov2lcov -infile coverage.out -outfile coverage.lcov -use-absolute-source-paths
;; Now you can load this into coverlay
(use-package coverlay
  :elpaca t
  :config
  (setq coverlay:tested-line-background-color "#C9F3D2")
  (setq coverlay:untested-line-background-color "#F8CED3")
  (setq coverlay:mark-tested-lines nil))

;;; [OTHER PACKAGES] =============================================

;; project
(use-package project
  :defer t
  :after (evil evil-leader)
  :commands (project-switch-project project-find-file project-roots project-current)
  :config
  (setq project-vc-ignores '("target/" "node_modules/")) ; default ignores
  (setq project-switch-commands 'project-find-file) ; start `project-find-file' by default

  ;; Find root for go projects without vcs
  (defun meain/project-try-explicit (dir)
    "Find root based on go.mod for `dir'."
    (locate-dominating-file dir "go.mod"))
  (cl-defmethod project-root ((project string))
    project)
  (add-hook 'project-find-functions
            #'meain/project-try-explicit 100)

  (defun meain/project-name ()
    (file-name-nondirectory (directory-file-name
                             (project-root (project-current)))))
  :init
  (evil-leader/set-key "p p"
    (meain/with-alternate (call-interactively 'project-switch-project)
                          (project-find-file)))
  (define-key evil-normal-state-map (kbd "<RET>") 'project-find-file))

;; eldoc load
(use-package eldoc
  :defer t
  :after (evil)
  :config
  (setq eldoc-echo-area-use-multiline-p nil)
  (define-key evil-normal-state-map (kbd "K") 'eldoc-print-current-symbol-info)
  ;; (add-to-list 'display-buffer-alist
  ;;              '("^\\*eldoc" display-buffer-at-bottom
  ;;                (window-height . 7)))
  (global-eldoc-mode nil))

;; Show eldoc messages in a popup at point
(use-package eldoc-box
  :elpaca t
  :commands (eldoc-box-help-at-point eldoc-box-hover-mode eldoc-box-hover-at-point-mode)
  :init
  (global-set-key (kbd "M-d")
                  (lambda ()
                    (interactive)
                    (let ((eldoc-echo-area-use-multiline-p t))
                      (call-interactively #'eldoc-box-help-at-point)))))

;; Highlight addresses in files
(use-package goto-addr
  :disabled t ; vim gx can do it
  :config (global-goto-address-mode))

;; dired
(use-package dired
  :defer t
  :after evil
  :config
  (require 'dired-x) ;; for dired-omit-files
  (setq delete-by-moving-to-trash t)
  (setq trash-directory "~/.Trash")
  (setq dired-listing-switches "-AGFhlgo")
  (setq dired-dwim-target t)
  ;; (setq dired-kill-when-opening-new-dired-buffer t)
  (define-key dired-mode-map (kbd "-") 'dired-up-directory)
  (add-hook 'dired-mode-hook 'dired-omit-mode)
  (add-hook 'dired-mode-hook 'hl-line-mode)
  (add-hook 'dired-mode-hook 'dired-hide-details-mode)
  (define-key evil-normal-state-map (kbd "-") 'dired-jump)
  (define-key evil-normal-state-map (kbd "_") 'find-file)

  (add-hook 'dired-mode-hook (lambda ()
                               (setq imenu-generic-expression '((nil "\s+.+$" 0)))))

  ;; TODO: make it work with directories
  (defun dired-dim-git-ignores ()
    "Dim out .gitignore contents"
    (when-let (((require 'vc))
               (ignores (magit-ignored-files))
               (exts (make-local-variable 'completion-ignored-extensions)))
      (dolist (item ignores) (add-to-list exts item))))
  (add-hook 'dired-mode-hook #'dired-dim-git-ignores))

(use-package dired-x
  :config
  (setq dired-omit-files "\\.DS_Store$\\|__pycache__$\\|.pytest_cache$\\|\\.mypy_cache$\\|\\.egg-info$"))

;; Github like git info in dired
(use-package dired-git-info
  :elpaca t
  :defer t
  :after dired
  :commands (dired-git-info-mode dired-git-info-auto-enable))

;; Enable abbrev mode
(use-package abbrev-mode
  :defer t
  :init
  (add-hook 'text-mode-hook #'abbrev-mode)
  (add-hook 'prog-mode-hook #'abbrev-mode)
  :config
  ;; (find-file "emacs/.config/emacs/abbrev_defs")
  (setq save-abbrevs 'silent))

;; auto activating snippets
(use-package aas
  :elpaca t
  :after (tree-surgeon)
  ;; can't defer loading of this as we need it in every single spawned
  ;; buffer including scratch
  :init (add-hook 'find-file-hook #'aas-activate-for-major-mode)
  :config
  (aas-global-mode)
  (aas-set-snippets 'global
    ";--" "—"
    ";>>" "⟶"
    ";<<" "⟵"
    ";<>" "⇌"
    ";>~" "↝"
    ";<~" "↜"
    ";tm" "™"
    ";shr" "¯\\_(ツ)_/¯"
    ";ch" "[meain](https://github.com/meain)"
    ";gh" "https://github.com/"
    ";gm" "https://github.com/meain"
    ";isodate" (lambda () (interactive) (insert (format-time-string "%a, %d %b %Y %T %z")))
    ";date" (lambda () (interactive) (insert (format-time-string "%a %b %d %Y")))
    ";sdate" (lambda () (interactive) (insert (format-time-string "%d %b %Y")))
    ";d/" (lambda () (interactive) (insert (format-time-string "%D")))
    ";d-" (lambda () (interactive) (insert (format-time-string "%F")))
    ";time" (lambda () (interactive) (insert (format-time-string "%T")))
    ";filename" (lambda () (interactive) (insert (file-name-nondirectory (buffer-file-name)))))
  (aas-set-snippets 'ledger-mode
    ";e" (lambda () (interactive) (insert (format-time-string "%Y/%m/%d"))))
  (aas-set-snippets 'emacs-lisp-mode
    ";auto" ";;;###autoload"
    ";la" (lambda () (interactive) (insert "(lambda ())") (backward-char 2))
    ";li" (lambda () (interactive) (insert "(lambda () (interactive) )") (backward-char 1))
    ";j" (lambda () (interactive) (insert "(message \"%s\" )") (backward-char 1)))
  (aas-set-snippets 'sql-mode
    ";bang" "SELECT * FROM information_schema.tables;"
    ";d" (lambda ()
           (interactive)
           (insert "select * from ")
           (let
               ((company-quickhelp-delay 0.1)
                (company-tooltip-idle-delay 0.1)
                (company-idle-delay 0.1))
             (consult-company))))
  (aas-set-snippets 'web-mode
    ";bang" (lambda () (interactive) (insert-file-contents (expand-file-name "~/.config/datafiles/templates/index.html"))))
  (aas-set-snippets 'html-mode
    ";bang" (lambda () (interactive) (insert-file-contents (expand-file-name "~/.config/datafiles/templates/index.html"))))
  (aas-set-snippets 'js-mode
    ";j" (lambda () (interactive) (insert "console.log(\"\")") (backward-char 2)))
  (aas-set-snippets 'nix-mode
    ";bang" (lambda () (interactive) (insert-file-contents (expand-file-name "~/.config/datafiles/templates/default.nix"))))
  (aas-set-snippets 'markdown-mode
    ";month" (lambda () (interactive) (insert (format-time-string "%B %Y")))
    ";bang"
    (lambda ()
      (interactive)
      (insert (concat "---\ntitle: "
                      (file-name-nondirectory (file-name-sans-extension (buffer-file-name)))
                      "\ncreated: "
                      (format-time-string "%a %d %b %Y %T")
                      "\n---\n"))))
  (aas-set-snippets 'go-ts-mode
    "!+" "!="
    ";;" ":="
    ";j" (lambda () (interactive) (insert "fmt.Println(\"\")") (backward-char 2))
    ;; create `name = append(name, )` forms
    ";ap" (lambda ()
            (interactive)
            (delete-char -1)
            (kill-word -1)
            (yank)
            (insert " = append(")
            (yank)
            (insert ", )")
            (forward-char -1))
    ";rr"
    (lambda ()
      (interactive)
      (kill-word -1)
      (kill-word -1)
      (insert "for _, ")
      (yank)
      (insert " := range ")
      (yank 2)
      (insert "{\n\n")
      ;; (indent-for-tab-command)
      (insert "}")
      (forward-line -1)
      (indent-for-tab-command))
    ";ri"
    (lambda ()
      (interactive)
      (kill-word -1)
      (kill-word -1)
      (insert "for i, ")
      (yank)
      (insert " := range ")
      (yank 2)
      (insert "{\n\n")
      ;; (indent-for-tab-command)
      (insert "}")
      (forward-line -1)
      (indent-for-tab-command))
    ";lf"
    (lambda ()
      (interactive)
      (insert (concat "if err != nil { log.Fatal(\"" (read-string "Error message: ") ": %v\", err) }")))
    ";er"
    (lambda ()
      (interactive)
      (call-interactively 'tree-surgeon-go-error))
    ";ew"
    (lambda ()
      (interactive)
      (call-interactively 'tree-surgeon-go-error tree-surgeon-go-error-format-with-wrap))
    ";tr"
    (lambda ()
      (interactive)
      (let ((left (read-string "Left: "))
            (right (read-string "Right: "))
            (thing (read-string "Incorrect thing: ")))
        (insert (concat "if " left " != " right "{ t.Errorf(\"incorrect " thing "; expected '%v', got '%v'\", " right " , " left ")}"))))
    ";test"
    (lambda ()
      (interactive)
      (insert (concat "func "
                      (read-string "Test function name: ")
                      "(t *testing.T) {
    want :=
    got, err :=
    if !cmp.Equal(want, got) {
        t.Fatalf(\"values are not the same %s\", cmp.Diff(tc.want, got))
    }
    }")))
    ";ttest"
    (lambda ()
      (interactive)
      (insert (concat "func "
                      (read-string "Test function name: ")
                      "(t *testing.T) {
	tests := []struct {
		name  string
		input string
		want  _
	}{
		{},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			got, err :=
			if !cmp.Equal(tc.want, got) {
				t.Fatalf(\"values are not the same %s\", cmp.Diff(tc.want, got))
			}
		})
	}
  }"))))
  (aas-set-snippets 'python-mode
    ";ip" "__import__('ipdb').set_trace()")
  (aas-set-snippets 'org-mode
    ";el" "#+BEGIN_SRC emacs-lisp\n\n#+END_SRC"
    ";py" "#+BEGIN_SRC python\n\n#+END_SRC"
    ";co" "#+BEGIN_SRC\n\n#+END_SRC"))

;; Templates
(use-package tempel
  :elpaca t
  :commands (tempel-complete tempel-expand tempel-insert)
  :init
  (global-set-key (kbd "M-*") 'tempel-complete)
  (global-set-key (kbd "M-)") 'tempel-next)
  (global-set-key (kbd "M-(") 'tempel-previous))

;; flyspell
(use-package flyspell
  :defer t
  :commands (flyspell-prog-mode flyspell-mode flyspell-goto-next-error)
  :init
  (add-hook 'prog-mode-hook 'flyspell-prog-mode)
  (add-hook 'text-mode-hook 'flyspell-mode)
  :bind (:map flyspell-mode-map ("C-;" . flyspell-auto-correct-word))
  :config
  ;; Make flyspell work with tree-sitter
  (setq-default flyspell-prog-text-faces
                '(tree-sitter-hl-face:comment
                  tree-sitter-hl-face:doc
                  tree-sitter-hl-face:string
                  font-lock-comment-face
                  font-lock-doc-face
                  font-lock-string-face)))
(use-package flyspell-correct
  :elpaca t
  :after flyspell
  :commands (flyspell-correct-wrapper flyspell-goto-next-error)
  :bind (:map flyspell-mode-map ("C-:" . flyspell-correct-wrapper)))

;; Advanced spell checking
(use-package wucuo
  :elpaca t
  :disabled t
  :after flyspell
  :commands (wucuo-start)
  :init
  (add-hook 'prog-mode-hook #'wucuo-start)
  (add-hook 'text-mode-hook #'wucuo-start)
  :config
  (add-to-list 'wucuo-font-faces-to-check 'tree-sitter-hl-face:comment)
  (add-to-list 'wucuo-font-faces-to-check 'tree-sitter-hl-face:doc)
  (add-to-list 'wucuo-font-faces-to-check 'tree-sitter-hl-face:string)
  (add-to-list 'wucuo-font-faces-to-check 'tree-sitter-hl-face:property)
  (add-to-list 'wucuo-font-faces-to-check 'tree-sitter-hl-face:method.call)
  (add-to-list 'wucuo-font-faces-to-check 'tree-sitter-hl-face:function.call)
  (add-to-list 'wucuo-font-faces-to-check 'tree-sitter-hl-face:function.method)
  (add-to-list 'wucuo-font-faces-to-check 'tree-sitter-hl-face:constructor))

;; flymake
(use-package flymake
  :after evil
  :commands (flymake flymake-find-file-hook
                     flymake-goto-next-error
                     flymake-goto-prev-error)
  :init
  (add-hook 'find-file-hook 'flymake-find-file-hook)
  :config
  (setq flymake-show-diagnostics-at-end-of-line nil)
  (evil-set-command-property 'flymake-goto-next-error :jump t)
  (evil-set-command-property 'flymake-goto-prev-error :jump t))
(use-package flymake-diagnostic-at-point
  :elpaca t
  :after (flymake evil-leader)
  :config
  (setq flymake-diagnostic-at-point-error-prefix "! ")
  (setq flymake-diagnostic-at-point-display-diagnostic-function 'flymake-diagnostic-at-point-display-minibuffer)
  (evil-leader/set-key "j" 'flymake-goto-next-error)
  (evil-leader/set-key "k" 'flymake-goto-prev-error)
  (add-hook 'flymake-mode-hook #'flymake-diagnostic-at-point-mode))
(use-package flymake-quickdef
  :elpaca t
  :after flymake
  :config
  ;; tree-grepper based lints
  (flymake-quickdef-backend flymake-check-end-in-comma
    :pre-let ((tree-grepper-exec (executable-find "tree-grepper")))
    :pre-check (unless tree-grepper-exec (error "Cannot find tree-grepper executable"))
    :write-type 'file
    :proc-form (list tree-grepper-exec fmqd-temp-file "-q" "go" "(argument_list \",\" @no-trailing-comma .)")
    :search-regexp "^\\([^:]+\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\):no-trailing-comma:\\(.*\\)$"
    :prep-diagnostic (let* ((lnum (string-to-number (match-string 2)))
                            (col (string-to-number (match-string 3)))
                            (pos (flymake-diag-region fmqd-source lnum col))
                            (beg (car pos))
                            (end (cdr pos))
                            (msg (format "tree-grepper-exec> No trailing comma")))
                       (list fmqd-source beg end :warning msg)))
  (add-hook 'go-ts-mode-hook
            (lambda ()
              (add-hook 'flymake-diagnostic-functions 'flymake-check-end-in-comma nil t)))

  (flymake-quickdef-backend flymake-check-empty-string
    :pre-let ((tree-grepper-exec (executable-find "tree-grepper")))
    :pre-check (unless tree-grepper-exec (error "Cannot find tree-grepper executable"))
    :write-type 'file
    :proc-form (list tree-grepper-exec fmqd-temp-file "-q" "go" "((binary_expression (identifier) [\"==\" \"!=\"] (interpreted_string_literal) @_ri) @exp (#eq? @_ri \"\\\"\\\"\"))")
    :search-regexp "^\\([^:]+\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\):exp:\\(.*\\)$"
    :prep-diagnostic (let* ((lnum (string-to-number (match-string 2)))
                            (col (string-to-number (match-string 3)))
                            (pos (flymake-diag-region fmqd-source lnum col))
                            (beg (car pos))
                            (end (cdr pos))
                            (msg (format "tree-grepper-exec> Use len based checks")))
                       (list fmqd-source beg end :warning msg)))
  (add-hook 'go-ts-mode-hook
            (lambda ()
              (add-hook 'flymake-diagnostic-functions 'flymake-check-empty-string nil t)))

  ;; https://github.com/crate-ci/typos
  (flymake-quickdef-backend flymake-check-typos
    :pre-let ((typos-exec (executable-find "typos")))
    :pre-check (unless typos-exec (error "Cannot find typos executable"))
    :write-type 'file
    :proc-form (list typos-exec "--hidden" "--format" "brief" fmqd-temp-file)
    :search-regexp "^\\([^:]+\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\): \\(.*\\)$"
    :prep-diagnostic (let* ((lnum (string-to-number (match-string 2)))
                            (col (string-to-number (match-string 3)))
                            (text (match-string 4))
                            (pos (flymake-diag-region fmqd-source lnum col))
                            (beg (car pos))
                            (end (cdr pos))
                            (msg (format "typos> %s" text)))
                       (list fmqd-source beg end :warning msg)))
  (add-hook 'flymake-diagnostic-functions 'flymake-check-typos)

  ;; https://github.com/rhysd/actionlint
  (flymake-quickdef-backend flymake-check-actionlint
    :pre-let ((actionlint-exec (executable-find "actionlint")))
    :pre-check (unless actionlint-exec (error "Cannot find actionlint executable"))
    :write-type 'file
    :proc-form (list actionlint-exec "-format" "{{range $err := .}}{{$err.Filepath}}:{{$err.Line}}:{{$err.Column}}:{{$err.Message}}\n{{end}}" fmqd-temp-file)
    :search-regexp "^\\([^:]+\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\):\\(.*\\)$"
    :prep-diagnostic (let* ((lnum (string-to-number (match-string 2)))
                            (col (string-to-number (match-string 3)))
                            (text (match-string 4))
                            (pos (flymake-diag-region fmqd-source lnum col))
                            (beg (car pos))
                            (end (cdr pos))
                            (msg (format "actionlint> %s" text)))
                       (list fmqd-source beg end :warning msg)))
  (add-hook 'yaml-mode-hook
            (lambda ()
              (if (string-match-p ".*\\.github/workflows/.*\\.ya?ml" (buffer-file-name))
                  (add-hook 'flymake-diagnostic-functions 'flymake-check-actionlint nil t))))

  ;; Bandit (sec issues in python)
  (flymake-quickdef-backend flymake-check-bandit
    :pre-let ((bandit-exec (executable-find "bandit")))
    :pre-check (unless bandit-exec (error "Cannot find bandit executable"))
    :write-type 'file
    :proc-form (list bandit-exec "--format" "custom" "--msg-template"
                     "diag:{line} {severity} {test_id}: {msg}" fmqd-temp-file)
    :search-regexp "^diag:\\([[:digit:]]+\\) \\(HIGH\\|LOW\\|MEDIUM\\|UNDEFINED\\) \\([[:alpha:]][[:digit:]]+\\): \\(.*\\)$"
    :prep-diagnostic (let* ((lnum (string-to-number (match-string 1)))
                            (severity (match-string 2))
                            (code (match-string 3))
                            (text (match-string 4))
                            (pos (flymake-diag-region fmqd-source lnum))
                            (beg (car pos))
                            (end (cdr pos))
                            (type (cond
                                   ((string= severity "HIGH") :error)
                                   ((string= severity "MEDIUM") :warning)
                                   (t :note)))
                            (msg (format "bandit> %s (%s)" text code)))
                       (list fmqd-source beg end type msg)))
  (add-hook 'python-mode-hook
            (lambda ()
              (add-hook 'flymake-diagnostic-functions 'flymake-check-bandit nil t)))

  ;; https://github.com/PyCQA/pylint
  (flymake-quickdef-backend flymake-pylint
    ;; TODO: filter out warnings and errors properly
    :pre-let ((pylint-exec (executable-find "pylint")))
    :pre-check (unless pylint-exec (error "Cannot find pylint executable"))
    :write-type 'file
    :proc-form (list pylint-exec "-f" "parseable" "-r" "n"
                     "-s" "n" "--msg-template" "{line}:{column}: {msg_id}({symbol}) {msg}"
                     "-d" "E0401,W0511,C0103,C0330" fmqd-temp-file)
    :search-regexp "\\([[:digit:]]+\\):\\([[:digit:]]+\\): \\(.*\\)$"
    :prep-diagnostic (let* ((lnum (string-to-number (match-string 1)))
                            (col (string-to-number (match-string 2)))
                            (text (match-string 3))
                            (pos (flymake-diag-region fmqd-source lnum col))
                            (beg (car pos))
                            (end (cdr pos))
                            (msg (format "pylint> %s" text)))
                       (list fmqd-source beg end :warning msg)))
  (add-hook 'python-mode-hook
            (lambda ()
              (add-hook 'flymake-diagnostic-functions 'flymake-pylint nil t)))

  ;; https://github.com/golangci/golangci-lint
  ;; Most linters in golangci-lint requires the project context. We
  ;; can hack it together to make it work in a project context, but
  ;; they will need the file to be save for it to work which creates
  ;; more problems. So we are only enabling --fast linters as all of
  ;; them are ones that can work on single file.
  ;; https://github.com/golangci/golangci-lint/issues/1574#issuecomment-804500358
  ;; TODO: Find some way to run linters like errcheck, govet, staticcheck etc
  (flymake-quickdef-backend flymake-golangci
    :pre-let ((golangci-exec (executable-find "golangci-lint")))
    :pre-check (unless golangci-exec (error "Cannot find golangci-lint executable"))
    :write-type 'file ; don't really use this
    :proc-form (list golangci-exec "run"
                     "--print-issued-lines=false" "--out-format=line-number"
                     "--disable-all" "--fast" fmqd-temp-file) ; --fast ones can run on single file
    :search-regexp "[^:]*:\\([[:digit:]]+\\):\\([[:digit:]]+\\): \\(.*\\)$"
    :prep-diagnostic (let* ((lnum (string-to-number (match-string 1)))
                            (col (string-to-number (match-string 2)))
                            (text (match-string 3))
                            (pos (flymake-diag-region fmqd-source lnum col))
                            (beg (car pos))
                            (end (cdr pos))
                            (msg (format "golangci> %s" text)))
                       (list fmqd-source beg end :warning msg)))
  (add-hook 'go-mode-hook
            (lambda ()
              (add-hook 'flymake-diagnostic-functions 'flymake-golangci nil t)))

  ;; https://github.com/nerdypepper/statix
  (flymake-quickdef-backend flymake-statix
    :pre-let ((statix-exec (executable-find "statix")))
    :pre-check (unless statix-exec (error "Cannot find statix executable"))
    :write-type 'file
    :proc-form (list statix-exec "check" "--format" "errfmt" fmqd-temp-file)
    :search-regexp "^\\([^>]+\\)>\\([[:digit:]]+\\):\\([[:digit:]]+\\):\\(.*\\)$"
    :prep-diagnostic (let* ((lnum (string-to-number (match-string 2)))
                            (col (string-to-number (match-string 3)))
                            (text (match-string 4))
                            (pos (flymake-diag-region fmqd-source lnum col))
                            (beg (car pos))
                            (end (cdr pos))
                            (msg (format "statix> %s" text)))
                       (list fmqd-source beg end :warning msg)))
  (add-hook 'nix-mode-hook
            (lambda ()
              (add-hook 'flymake-diagnostic-functions 'flymake-statix nil t)))

  ;; https://github.com/hadolint/hadolint
  (flymake-quickdef-backend flymake-hadolint
    :pre-let ((hadolint-exec (executable-find "hadolint")))
    :pre-check (unless hadolint-exec (error "Cannot find hadolint executable"))
    :write-type 'file
    :proc-form (list hadolint-exec "--no-color" fmqd-temp-file)
    :search-regexp "^\\([^:]+\\):\\([[:digit:]]+\\) \\(.*\\)$"
    :prep-diagnostic (let* ((lnum (string-to-number (match-string 2)))
                            (col 0)
                            (text (match-string 3))
                            (pos (flymake-diag-region fmqd-source lnum col))
                            (beg (car pos))
                            (end (cdr pos))
                            (msg (format "hadolint> %s" text)))
                       (list fmqd-source beg end :warning msg)))
  (add-hook 'dockerfile-mode-hook
            (lambda ()
              (add-hook 'flymake-diagnostic-functions 'flymake-hadolint nil t)))

  ;; https://github.com/DavidAnson/markdownlint
  (flymake-quickdef-backend flymake-markdownlint
    :pre-let ((markdownlint-exec (executable-find "markdownlint")))
    :pre-check (unless markdownlint-exec (error "Cannot find markdownlint executable"))
    :write-type 'file
    :proc-form (list markdownlint-exec "-c" (concat (getenv "HOME") "/.config/markdownlint/config.yaml") fmqd-temp-file)
    :search-regexp "^\\([^:]+\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\) \\(.*\\)$"
    :prep-diagnostic (let* ((lnum (string-to-number (match-string 2)))
                            (col (string-to-number (match-string 3)))
                            (text (match-string 4))
                            (pos (flymake-diag-region fmqd-source lnum col))
                            (beg (car pos))
                            (end (cdr pos))
                            (msg (format "markdownlint> %s" text)))
                       (list fmqd-source beg end :warning msg)))
  (add-hook 'markdown-mode-hook
            (lambda ()
              (add-hook 'flymake-diagnostic-functions 'flymake-markdownlint nil t)))
  (add-hook 'gfm-mode-hook
            (lambda ()
              (add-hook 'flymake-diagnostic-functions 'flymake-markdownlint nil t)))

  ;; https://github.com/errata-ai/vale
  (flymake-quickdef-backend flymake-vale
    :pre-let ((vale-exec (executable-find "vale")))
    :pre-check (unless vale-exec (error "Cannot find vale executable"))
    :write-type 'file
    :proc-form (list vale-exec "--output" "line" "--config" (concat (getenv "HOME") "/.config/vale/vale.ini") fmqd-temp-file)
    :search-regexp "^\\([^:]+\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\):\\(.*\\)$"
    :prep-diagnostic (let* ((lnum (string-to-number (match-string 2)))
                            (col (string-to-number (match-string 3)))
                            (text (match-string 4))
                            (pos (flymake-diag-region fmqd-source lnum col))
                            (beg (car pos))
                            (end (cdr pos))
                            (msg (format "vale> %s" text)))
                       (list fmqd-source beg end :warning msg)))
  (add-hook 'markdown-mode-hook
            (lambda ()
              (add-hook 'flymake-diagnostic-functions 'flymake-vale nil t)))
  (add-hook 'gfm-mode-hook
            (lambda ()
              (add-hook 'flymake-diagnostic-functions 'flymake-vale nil t)))

  ;; jsonlint
  (flymake-quickdef-backend flymake-jsonlint
    :pre-let ((jsonlint-exec (executable-find "jsonlint")))
    :pre-check (unless jsonlint-exec (error "Cannot find jsonlint executable"))
    :write-type 'file
    :proc-form (list jsonlint-exec "-c" "-q" fmqd-temp-file)
    :search-regexp "^\\([^:]+\\): line \\([[:digit:]]+\\), col \\([[:digit:]]+\\), \\(.*\\)$"
    :prep-diagnostic (let* ((lnum (string-to-number (match-string 2)))
                            (col (string-to-number (match-string 3)))
                            (text (match-string 4))
                            (pos (flymake-diag-region fmqd-source lnum col))
                            (beg (car pos))
                            (end (cdr pos))
                            (msg (format "jsonlint> %s" text)))
                       (list fmqd-source beg end :warning msg)))
  (add-hook 'json-mode-hook
            (lambda ()
              (add-hook 'flymake-diagnostic-functions 'flymake-jsonlint nil t))))

;; Company for autocompletions
(use-package company
  :elpaca t
  :disabled t
  :defer 1
  :diminish
  :config
  (setq company-dabbrev-downcase nil) ;; Do not lowercase my completions
  (setq company-idle-delay 0)
  (setq company-tooltip-idle-delay 1)
  (setq company-quickhelp-delay 0.3)
  (setq company-tooltip-maximum-width 35)
  (setq company-tooltip-align-annotations t)
  (setq company-minimum-prefix-length 2)
  (setq company-format-margin-function nil)
  (global-company-mode)
  (evil-declare-change-repeat 'company-complete-common-or-cycle) ; make evil repeat working with completions
  (define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "<tab>") 'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "S-TAB") 'company-select-previous)
  (define-key company-active-map (kbd "<backtab>") 'company-select-previous)
  (setq company-frontends '(company-pseudo-tooltip-unless-just-one-frontend
                            company-preview-frontend company-echo-metadata-frontend))
  (setq company-require-match 'never)
  (defun my-company-visible-and-explicit-action-p ()
    (and (company-tooltip-visible-p) (company-explicit-action-p)))
  (defun company-ac-setup ()
    "Sets up `company-mode' to behave similarly to `auto-complete-mode'."
    (setq company-require-match nil)
    (setq company-auto-complete #'my-company-visible-and-explicit-action-p)
    (setq company-frontends '(company-echo-metadata-frontend company-pseudo-tooltip-unless-just-one-frontend-with-delay
                                                             company-preview-frontend))
    (define-key company-active-map [tab] 'company-select-next-if-tooltip-visible-or-complete-selection)
    (define-key company-active-map (kbd "TAB") 'company-select-next-if-tooltip-visible-or-complete-selection))
  (company-ac-setup))

;; consult-interface for company for use in `sql-mode'
(use-package consult-company
  :elpaca t
  :disabled t
  :defer t
  :after (consult company)
  :commands (consult-company))

;; Company quickhelp
(use-package company-quickhelp ; Show help in tooltip
  :elpaca t
  :disabled t
  :after company
  :config
  (company-quickhelp-mode)
  (setq pos-tip-foreground-color "#000000"
        pos-tip-background-color "#ffffff"))

(use-package corfu
  :elpaca (:files (:defaults "extensions/*"))
  :config
  (setq completion-cycle-threshold 3)
  (setq corfu-auto t)
  (setq corfu-cycle t)
  (setq corfu-auto-delay 0)
  (setq corfu-auto-prefix 2)
  (setq corfu-history-mode t)
  (setq corfu-count 5)
  (define-key corfu-map (kbd "RET") 'newline-and-indent) ; default: corfu-insert

  ;; Plugin in case we need in buffer overlay for completions
  ;; https://code.bsdgeek.org/adam/corfu-candidate-overlay

  (defun corfu-move-to-minibuffer ()
    "Move completion to minibuffer instead of corfu."
    (interactive)
    (let ((completion-extra-properties corfu--extra)
          completion-cycle-threshold completion-cycling)
      (apply #'consult-completion-in-region completion-in-region--data)))
  (define-key corfu-map "\M-m" #'corfu-move-to-minibuffer)

  (global-corfu-mode)
  (corfu-popupinfo-mode)

  (define-key corfu-map (kbd "M-p") #'corfu-popupinfo-scroll-down) ;; corfu-next
  (define-key corfu-map (kbd "M-n") #'corfu-popupinfo-scroll-up)  ;; corfu-previous
  (define-key corfu-map (kbd "M-d") #'corfu-popupinfo-documentation)
  (define-key corfu-map (kbd "M-D") #'corfu-popupinfo-location))

;; Add completion extensions
(use-package cape
  :elpaca t
  :bind (("M-p" . completion-at-point) ;; capf
         ("M-f p t" . complete-tag)        ;; etags
         ("M-f p d" . cape-dabbrev)        ;; or dabbrev-completion
         ("M-f p h" . cape-history)
         ("M-f p f" . cape-file)
         ("M-f p k" . cape-keyword)
         ("M-f p s" . cape-symbol)
         ("M-f p a" . cape-abbrev)
         ("M-f p i" . cape-ispell)
         ("M-f p l" . cape-line)
         ("M-f p w" . cape-dict)
         ("M-f p &" . cape-sgml))
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file))

;; Completions (core framework)
(use-package vertico
  :elpaca (:files (:defaults "extensions/*.el"))
  :config
  (setq vertico-count 13)
  (setq vertico-cycle t) ; useful for consult-imenu
  (define-key vertico-map (kbd "M-q") 'vertico-multiform-vertical)
  (define-key vertico-map (kbd "M-g") 'vertico-multiform-grid)
  (define-key vertico-map (kbd "<S-backspace>") 'vertico-directory-up)

  (define-key vertico-map (kbd "M-n") 'vertico-next-group)
  (define-key vertico-map (kbd "M-p") 'vertico-previous-group)

  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  (setq enable-recursive-minibuffers t)

  (vertico-multiform-mode)
  (setq vertico-multiform-commands
        '((consult-ripgrep buffer indexed)
          (consult-xref buffer indexed)
          (eglot-find-implementation indexed) ;; TODO: change to vertical
          (consult-imenu buffer)
          (xref-find-references buffer)
          (meain/imenu-or-eglot buffer)
          (tree-jump-search buffer)
          (consult-tree-jump-search buffer) ;; TODO: does not work
          (consult-buffer flat)
          (t flat)))
  (setq vertico-multiform-categories
        '((file grid)
          (consult-grep buffer)
          (t flat)))
  (vertico-mode))
(use-package savehist
  :init
  (savehist-mode))
(use-package orderless
  :elpaca t
  :config
  (setq completion-styles '(orderless basic))

  (defun flex-if-twiddle (pattern _index _total)
    (cond ((string-suffix-p "~" pattern)
           `(orderless-flex . ,(substring pattern 0 -1)))
          ((string-prefix-p "~" pattern)
           `(orderless-flex . ,(substring pattern 1)))))
  (defun initialism-if-comma (pattern index _total)
    (cond ((string-suffix-p "," pattern)
           `(orderless-initialism . ,(substring pattern 0 -1)))
          ((string-prefix-p "," pattern)
           `(orderless-initialism . ,(substring pattern 1)))))
  (defun without-if-bang (pattern _index _total)
    (cond
     ((equal "!" pattern)
      '(orderless-literal . ""))
     ((string-suffix-p "!" pattern)
      `(orderless-without-literal . ,(substring pattern 0 -1)))
     ((string-prefix-p "!" pattern)
      `(orderless-without-literal . ,(substring pattern 1)))))

  (setq orderless-style-dispatchers '(initialism-if-comma
                                      flex-if-twiddle
                                      without-if-bang))
  (orderless-define-completion-style orderless+basic
    (orderless-matching-styles '(orderless-literal
                                 orderless-regexp)))
  (setq completion-category-overrides
        '((command (styles orderless+basic))
          (symbol (styles orderless+basic))
          (variable (styles orderless+basic))
          (file (styles basic partial-completion)))))
(use-package marginalia
  :elpaca t
  :defer 1
  :bind (:map minibuffer-local-map ("C-b" . marginalia-cycle))
  :config (marginalia-mode))

;; Show completions option even when there is a typo
;; (use-package typo
;;   :elpaca t
;;   :config (add-to-list 'completion-styles 'typo t))

;; Aggressive completions
(use-package aggressive-completion
  :elpaca t
  :defer 1
  :disabled t
  :config
  (setq aggressive-completion-delay 1.0)
  (setq aggressive-completion-auto-completion-help nil)
  (defun meain/vertico-complete ()
    (interactive)
    ;; (minibuffer-complete)
    (when vertico--count-ov ;; Only if vertico is active.
      (when vertico-flat-mode
        (vertico-multiform-vertical 'vertico-grid-mode)
        (vertico--exhibit))))
  (setq aggressive-completion-auto-complete-fn #'meain/vertico-complete)
  (aggressive-completion-mode t))

;; Consult without consultation fees
(use-package consult
  :elpaca t
  :after (xref)
  :defer 1
  :config
  (setq consult-ripgrep-args "rg --null --line-buffered --color=never --max-columns=1000 --path-separator /\
      --smart-case --no-heading --line-number --hidden --follow --glob \"!.git/*\"")
  (setq xref-show-xrefs-function #'consult-xref)
  (setq xref-show-definitions-function #'consult-xref)
  (evil-set-command-property 'consult-imenu :jump t)
  :init
  (define-key evil-normal-state-map (kbd "<SPC> /") 'consult-line))

(use-package consult-notmuch
  :after (notmuch)
  :elpaca t
  :commands (consult-notmuch consult-notmuch-tree consult-notmuch-address))

(use-package consult-git-log-grep
  :after (consult)
  :commands (consult-git-log-grep)
  :elpaca (consult-git-log-grep
           :host github
           :repo "ghosty141/consult-git-log-grep"))

;; Embark stuff
(use-package embark
  :defer 1
  :elpaca t
  :init (setq prefix-help-command #'embark-prefix-help-command)
  :config
  (global-set-key (kbd "C-'")  'embark-act)
  (global-set-key (kbd "C-.")  'embark-dwim)
  (global-set-key (kbd "C-h B")  'embark-bindings))
(use-package embark-consult
  :elpaca t
  :defer t
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode))

;; Helpful package
(use-package helpful
  :elpaca t
  :after evil-leader
  :commands (helpful-callable helpful-variable helpful-at-point helpful-key)
  :init
  (global-set-key (kbd "C-h f") #'helpful-callable)
  (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h k") #'helpful-key)
  (global-set-key (kbd "C-h x") #'helpful-command)
  (global-set-key (kbd "C-h o") #'helpful-symbol)
  (global-set-key (kbd "C-c C-d") #'helpful-at-point))

;; Map find-library along with helpful keybinds
(use-package emacs
  :init
  (global-set-key (kbd "C-h l") #'find-library))

;; ibuffer
(use-package ibuffer
  :commands (ibuffer ibuffer-other-window)
  :init
  (setq ibuffer-expert t)
  (global-set-key (kbd "M-c")
                  (meain/with-alternate (call-interactively 'switch-to-buffer)
                                        (ibuffer-other-window))))
(use-package ibuffer-project
  :elpaca t
  :after (ibuffer project)
  :config
  (add-to-list 'ibuffer-project-root-functions '(file-remote-p . "Remote"))
  (add-hook 'ibuffer-hook
            (lambda ()
              (setq ibuffer-filter-groups (ibuffer-project-generate-filter-groups))
              (unless (eq ibuffer-sorting-mode 'project-file-relative)
                (ibuffer-do-sort-by-project-file-relative))))
  (setq ibuffer-formats '((mark modified read-only " "
                                (name 18 18 :left :elide) " " (size 9 -1 :right) " "
                                (mode 16 16 :left :elide) " " project-file-relative)))
  :init (add-hook 'ibuffer-hook
                  (lambda ()
                    (setq ibuffer-filter-groups (ibuffer-project-generate-filter-groups)))))

;; rg.el
(use-package rg
  :elpaca t
  :commands rg
  :after evil-leader
  :init
  (evil-leader/set-key "f"
    (meain/with-alternate (consult-ripgrep) (call-interactively 'rg)))
  :config (setq rg-command-line-flags '("--hidden" "--follow")))

;; dumb-jump
(use-package dumb-jump
  :elpaca t
  :after evil-leader
  :commands dumb-jumb-go
  :init (evil-leader/set-key "J" 'dumb-jump-go)
  :config
  (advice-add 'dumb-jump-go :around #'meain/recenter-advice)
  (evil-set-command-property 'dumb-jumb-go :jump t))

;; Code formatting
(use-package apheleia
  :elpaca t
  :after evil
  :commands (apheleia-format-buffer meain/format-buffer)
  :config
  ;; json
  (setf (alist-get 'fixjson apheleia-formatters)
        '("fixjson"))
  (setf (alist-get 'json-mode apheleia-mode-alist)
        '(fixjson))

  ;; golang
  (setf (alist-get 'goimports apheleia-formatters)
        '("goimports"))
  (setf (alist-get 'gofumpt apheleia-formatters)
        '("gofumpt"))
  (setf (alist-get 'gci apheleia-formatters)
        '("gci" "/dev/stdin"))
  (setf (alist-get 'go-mode apheleia-mode-alist)
        '(goimports))
  (setf (alist-get 'go-ts-mode apheleia-mode-alist)
        '(goimports))

  ;; markdown
  (setf (alist-get 'markdown-mode apheleia-mode-alist)
        '(prettier-markdown))

  ;; clojure
  (setf (alist-get 'zprint apheleia-formatters)
        '("zprint"))
  (setf (alist-get 'clojure-mode apheleia-mode-alist)
        '(zprint))

  (setf (alist-get 'shell-script-mode apheleia-mode-alist)
        '(shfmt))
  (setf (alist-get 'sh-mode apheleia-mode-alist)
        '(shfmt))

  (setf (alist-get 'nixpkgsfmt apheleia-formatters)
        '("nixpkgs-fmt"))
  (setf (alist-get 'nix-mode apheleia-mode-alist)
        '(nixpkgsfmt))

  (defun meain/format-buffer ()
    "Format a buffer."
    (interactive)
    (cond
     ((eq major-mode 'emacs-lisp-mode)
      (indent-region (point-min) (point-max)))
     ((eq major-mode 'ledger-mode)
      (ledger-mode-clean-buffer))
     (t (call-interactively 'apheleia-format-buffer))))

  :init
  (add-hook 'go-mode-hook 'apheleia-mode)
  (add-hook 'go-ts-mode-hook 'apheleia-mode)
  (define-key evil-normal-state-map (kbd ",,") #'meain/format-buffer))

;; Xref customization
(use-package xref
  :after (evil)
  :config
  (define-key evil-normal-state-map (kbd "M-.") #'xref-find-definitions)
  (define-key evil-normal-state-map (kbd "M-?") #'xref-find-references)
  (define-key evil-normal-state-map (kbd "g d") 'xref-find-definitions)
  (define-key evil-normal-state-map (kbd "g D") 'xref-find-implementations)
  (define-key evil-normal-state-map (kbd "g r") 'xref-find-references)
  ;; (setq xref-show-definitions-function 'xref-show-definitions-completing-read)
  (setq xref-auto-jump-to-first-xref 'move) ;; Use 'show to open it
  (setq xref-auto-jump-to-first-definition 'move))


;; LSP using lspce
;; (use-package yasnippet :elpaca t)
;; (use-package lspce
;;   :load-path "/home/meain/dev/src/lspce"
;;   :after (yasnippet)
;;   :config (progn
;;             (setq lspce-send-changes-idle-time 1)
;;             (lspce-set-log-file "/tmp/lspce.log")
;;             (lspce-enable-logging)
;;             ;; (add-hook 'rust-mode-hook 'lspce-mode)
;;             (add-hook 'go-ts-mode-hook 'lspce-mode)
;;             (setq lspce-server-programs `(("rust"  "rust-analyzer" "" lspce-ra-initializationOptions)
;;                                           ("python" "pylsp" "" )
;;                                           ("go" "gopls" "")
;;                                           ("C" "clangd" "--all-scopes-completion --clang-tidy --enable-config --header-insertion-decorators=0")
;;                                           ("java" "java" lspce-jdtls-cmd-args lspce-jdtls-initializationOptions)))))

;; LSP
(use-package eglot
  :commands eglot-ensure
  ;; :elpaca t ;; use builtin version
  :after (project flymake)
  :config
  ;; Supposedly speed up eglot
  ;; https://www.reddit.com/r/emacs/comments/17jrsmv/comment/k74b3tg/
  (advice-add 'jsonrpc--log-event :override #'ignore)
  (setopt eglot-events-buffer-size 10)

  (setq eglot-autoshutdown t)
  (setq eglot-sync-connect nil)
  (setq eglot-extend-to-xref t) ;; extend eglot to files gone to with go-to-def
  ;; https://www.masteringemacs.org/article/seamlessly-merge-multiple-documentation-sources-eldoc
  (setq eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly)
  (add-to-list 'eglot-server-programs '(lua-mode . ("~/.luarocks/bin/lua-lsp")))
  ;; yaml-mode useful for github actions
  (add-to-list 'eglot-server-programs '(yaml-mode . ("yaml-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs '(json-mode . ("vscode-json-languageserver" "--stdio")))
  (add-to-list 'eglot-server-programs '(javascript-mode . ("typescript-language-server" "--stdio" "--tsserver-path" "/home/meain/.nix-profile/bin/tsserver")))
  (add-to-list 'eglot-server-programs '(web-mode . ("typescript-language-server" "--stdio" "--tsserver-path" "/home/meain/.nix-profile/bin/tsserver")))
  (add-to-list 'eglot-server-programs '(js-mode . ("typescript-language-server" "--stdio" "--tsserver-path" "/home/meain/.nix-profile/bin/tsserver")))
  (add-to-list 'eglot-server-programs '(typescipt-mode . ("typescript-language-server" "--stdio" "--tsserver-path" "/home/meain/.nix-profile/bin/tsserver")))
  (add-to-list 'eglot-server-programs '(gfm-mode . ("logseqlsp" "-t" "logseqlsp-token")))
  ;; Can be enabled on fiction like things
  ;; (add-to-list 'eglot-server-programs '(markdown-mode . ("unified-language-server" "--parser=remark-parse" "--stdio")))
  (add-to-list 'eglot-server-programs '(markdown-mode . ("prosemd-lsp" "--stdio"))) ;; to be used in combination with flyspell
  (add-to-list 'eglot-server-programs '(rust-mode . ("rust-analyzer")))
  (add-to-list 'eglot-server-programs '(python-mode . ("pylsp")))
  (add-to-list 'eglot-server-programs '(nix-mode . ("rnix-lsp")))
  (setq-default eglot-workspace-configuration
                ;; (:gopls . ((staticcheck . t))) ;; Huge mem usage penalty
                '((:json.schemas . [((:fileMatch . ["package.json"]) (:url . "https://json.schemastore.org/package.json"))])))
  (add-to-list 'display-buffer-alist
               '("\\*sqls\\*"
                 (display-buffer-reuse-window display-buffer-at-bottom)
                 (reusable-frames . visible)
                 (window-height . 0.3)))
  ;; add flymake backend separately so that I can add other things as well to flymake
  (add-to-list 'eglot-stay-out-of 'flymake)
  (add-hook 'flymake-diagnostic-functions 'eglot-flymake-backend)
  (defclass eglot-sqls (eglot-lsp-server) ()
    :documentation "SQL's Language Server")
  (add-to-list 'eglot-server-programs '(sql-mode . (eglot-sqls "sqls" "-config" "~/.config/sqls/config.yaml")))
  (cl-defmethod eglot-execute-command ((server eglot-sqls) (command (eql executeQuery)) arguments)
    "For executeQuery."
    (let* ((beg (eglot--pos-to-lsp-position (if (use-region-p) (region-beginning) (point-min))))
           (end (eglot--pos-to-lsp-position (if (use-region-p) (region-end) (point-max))))
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
  (cl-defmethod eglot-execute-command ((server eglot-sqls) (_cmd (eql switchDatabase)) arguments)
    "For switchDatabase."
    (let* ((res (jsonrpc-request server :workspace/executeCommand
                                 `(:command "showDatabases" :arguments ,arguments :timeout 0.5)))
           (menu-items (split-string res "\n"))
           (menu `("Eglot code actions:" ("dummy" ,@menu-items)))
           (db (if (listp last-nonmenu-event)
                   (x-popup-menu last-nonmenu-event menu)
                 (completing-read "[eglot] Pick an database: "
                                  menu-items nil t nil nil (car menu-items)))))
      (jsonrpc-request server
                       :workspace/executeCommand `(:command "switchDatabase"
                                                            :arguments [,db]:timeout
                                                            0.5))))
  (cl-defmethod eglot-execute-command ((server eglot-sqls) (_cmd (eql switchConnections)) arguments)
    "For switchConnections"
    (let* ((res (jsonrpc-request server :workspace/executeCommand
                                 `(:command "switchConnections" :arguments ,arguments :timeout 0.5)))
           (menu-items (split-string res "\n"))
           (menu `("Eglot code actions:" ("dummy" ,@menu-items)))
           (db (if (listp last-nonmenu-event)
                   (x-popup-menu last-nonmenu-event menu)
                 (completing-read "[eglot] Pick an connection "
                                  menu-items nil t nil nil (car menu-items)))))
      (jsonrpc-request server :workspace/executeCommand
                       `(:command "switchConnections" :arguments [,db]:timeout 0.5))))
  (evil-define-key 'normal eglot-mode-map (kbd "g D") 'eglot-find-implementation)
  (evil-define-key 'normal eglot-mode-map (kbd "g Y") 'eglot-find-typeDefinition)
  (evil-define-key 'normal eglot-mode-map (kbd "g R") 'eglot-rename)
  (evil-define-key 'normal eglot-mode-map (kbd "g ,") 'eglot-format-buffer)
  (evil-define-key 'normal eglot-mode-map (kbd "g a") 'eglot-code-actions)

  ;; evil collection in go-mode was remapping them
  (evil-define-key 'normal go-mode-map (kbd "K") 'eldoc-print-current-symbol-info)
  (evil-define-key 'normal go-mode-map (kbd "g d") 'xref-find-definitions))

;; Speed up eglot communication by translating to bycode externally
(use-package eglot-booster
  :elpaca (:host github :repo "jdtsmith/eglot-booster")
  :after eglot
  :config (eglot-booster-mode))

;; Get hierarchy
(use-package eglot-hierarchy
  :commands (eglot-hierarchy-call-hierarchy eglot-hierarchy-type-hierarchy)
  :elpaca (:host github :repo "dolmens/eglot-hierarchy"))

;; consult-eglot
(use-package consult-eglot
  :elpaca t
  :commands consult-eglot-symbols
  :after (imenu eglot)
  :config
  (advice-add 'consult-imenu :around #'meain/recenter-top-advice)
  :init
  (defun meain/imenu-or-eglot (&optional alternate)
    "Create a func to alternate between goto thingy stuff.
Giving it a name so that I can target it in vertico mode and make it use buffer."
    (interactive "P")
    (cond
     ((equal alternate nil) (consult-imenu))
     ((equal alternate '(4)) (consult-eglot-symbols))
     ((equal alternate '(16)) (tree-jump-search))))
  (global-set-key (kbd "M-i") #'meain/imenu-or-eglot))

;; TODO Try out and add support for go mode
;; (use-package dwim-coder-mode :elpaca t)

;; Peek into files/definitions without opening them
(use-package peek
  :elpaca (:host github :repo "Ziqi-Yang/peek")
  :after (evil)
  :commands (peek-overlay-dwim peek-xref-definition)
  :init
  (define-key evil-normal-state-map (kbd "g L") 'peek-xref-definition)
  (define-key evil-normal-state-map (kbd "g l") 'peek-overlay-dwim))

;; Hacky symbol search using tree-sitter
(use-package emacs
  ;; TODO: Lazy load tree-jump
  :after (consult)
  :commands (tree-jump-search consult-tree-jump-search tree-jump-xref-backend)
  :config
  (load-file "/home/meain/.config/emacs/tree-jump.el")
  :init
  (add-to-list 'xref-backend-functions 'tree-jump-xref-backend)
  (global-set-key (kbd "M-I")
                  (meain/with-alternate (consult-tree-jump-search "!mock !_test ")
                                        (tree-jump-search))))

;; LogSeq related things
(use-package emacs
  :after (evil-leader)
  :config
  (defvar logseq-directory "~/.local/share/logseq/"
    "The directory where logseq files are stored.")

  (defun logseq-journal-today ()
    "Open the journal for today."
    (interactive)
    (let ((date (format-time-string "%Y_%m_%d")))
      (find-file (concat logseq-directory "journals/" date ".md"))))

  (add-to-list 'display-buffer-alist '("\\*logseq-journal\\*"
                                       (display-buffer-reuse-window display-buffer-at-bottom)
                                       (reusable-frames . visible)
                                       (window-height . 0.3)))

  (defun logseq-journal-toggle ()
    "Open the journal for today."
    (interactive)
    (let* ((date (format-time-string "%Y_%m_%d"))
           (file (expand-file-name (concat logseq-directory "journals/" date ".md")))
           (buffer (get-buffer "*logseq-journal*")))
      (if (equal (buffer-file-name) file)
          (delete-window)
        (progn
          (if buffer
              (when (not (equal (buffer-file-name buffer) file))
                (kill-buffer buffer)
                (with-current-buffer (find-file-noselect file)
                  (rename-buffer "*logseq-journal*")))
            (with-current-buffer (find-file-noselect file)
              (rename-buffer "*logseq-journal*")))
          (pop-to-buffer "*logseq-journal*")))))

  (defun logseq-journal-previous (&optional count)
    "If we are already in a journal page, go to the previous journal
by getting the date from filename.  Does not do anything if we are not
in a journal page.

COUNT is the number of journal entries to go back by.  You can pass
negative values to go forward."
    (interactive)
    (let ((dir (file-name-nondirectory (directory-file-name (file-name-directory (buffer-file-name)))))
          (fn (file-name-nondirectory (buffer-file-name))))
      (if (equal dir "journals")
          ;; filename will be in the format %Y_%m_%d
          (when-let* ((time-segments (split-string (substring fn 0 10) "_"))
                      (year (string-to-number (car time-segments)))
                      (month (string-to-number (cadr time-segments)))
                      (day (string-to-number (caddr time-segments)))
                      (date (encode-time (list 0 0 0 day month year)))
                      (fname (format-time-string
                              "%Y_%m_%d"
                              (time-subtract date
                                             (days-to-time (if count count 1)))))
                      (path (concat logseq-directory "journals/" fname ".md")))
            (when (or
                   (file-exists-p path)
                   (yes-or-no-p "Journal page does not exist.  Create new?"))
              (find-file path)))
        (message "Not in a journal page."))))

  (defun logseq-journal-next ()
    "Go to next journal page.
Use `logseq-journal-previous' with a negative argument when using
interactively."
    (declare (interactive-only t))
    (interactive)
    (logseq-journal-previous -1))

  (defun logseq-journal-entry ()
    "Add an entry to today's journal.
The journal entry line will be prefixed by the current timestamp."
    (interactive)
    (when-let ((text (read-string "Entry: "))
               (time (format-time-string "**%H:%M**"))
               (date (format-time-string "%Y_%m_%d"))
               (file (concat logseq-directory "journals/" date ".md")))
      ;; Append an entry to today's journal
      (with-temp-buffer
        (when (file-exists-p file)
          (insert-file-contents file))
        (goto-char (point-max))
        (insert (concat "\n- " time " " text))
        (write-file file))))

  (defun logseq-journal-open (date)
    "Open a journal for a given date.
DATE is a string in the format YYYY_MM_DD.
In case it is called interactively, the date can be picked using the calendar."
    (interactive (list (let ((event (calendar-read-date)))
                         (format "%04d_%02d_%02d" (caddr event) (car event) (cadr event)))))
    (find-file (concat logseq-directory "journals/" date ".md")))

  (defun logseq-page-open (page)
    "Open a page in logseq.
In case it is called interactively, the page is autocompleted from the
list of available pages."
    (interactive (list (completing-read
                        "Page: "
                        (directory-files (concat logseq-directory "pages/") nil "\\.md$"))))
    (find-file (concat logseq-directory "pages/" page)))
  :init
  (evil-leader/set-key "le" 'logseq-journal-entry)
  (evil-leader/set-key "ll" 'logseq-journal-toggle)
  (evil-leader/set-key "lj" 'logseq-journal-today)
  (evil-leader/set-key "lJ" 'logseq-journal-open)
  (evil-leader/set-key "lp" 'logseq-journal-previous)
  (evil-leader/set-key "ln" 'logseq-journal-next)
  (evil-leader/set-key "lP" 'logseq-page-open))

;; Tagbar alternative
(use-package imenu
  :defer t
  :after (consult)
  :commands imenu
  :config
  (setq imenu-auto-rescan t)
  (setq imenu-max-item-length 300)
  (advice-add 'consult-imenu
              :before
              (lambda ()
                ;; We want the previous .rest buffer if http response buffer
                (if (equal (buffer-name) "*HTTP Response*")
                    (previous-window-any-frame))))
  (global-set-key (kbd "M-i") 'consult-imenu))
(use-package flimenu
  :elpaca t
  :defer t
  :after imenu
  :config (flimenu-global-mode 1))
(use-package imenu-list
  :elpaca t
  :defer t
  :after (imenu consult)
  :commands imenu-list-smart-toggle
  :config
  (setq imenu-list-focus-after-activation t)
  (setq imenu-list-after-jump-hook nil)
  (setq imenu-list-auto-resize t))

;; Symbol overlay
(use-package symbol-overlay
  :elpaca t
  :defer t
  :commands (symbol-overlay-mode symbol-overlay-put))

;; magit dependency
(use-package transient :elpaca t)

;; Magit
(use-package magit
  :elpaca t
  :after (evil-leader transient)
  :commands (magit-status magit-commit-create magit-ignored-files meain/git-how-was-it)
  :init
  (evil-leader/set-key "gg" 'magit-status)
  (evil-leader/set-key "gc" 'magit-commit-create)
  (evil-leader/set-key "gb" 'magit-blame)
  (evil-leader/set-key "gG" 'magit-show-commit)
  (evil-leader/set-key "gT" 'magit-log-trace-definition)
  :config
  (evil-define-key 'normal magit-status-mode-map (kbd ";") 'magit-stage)
  (evil-define-key 'visual magit-status-mode-map (kbd ";") 'magit-stage)

  ;; make <escape> quit(go back one level) in magit popups
  (define-key transient-map (kbd "<escape>") 'transient-quit-one)
  (setq magit-diff-refine-hunk (quote all))
  (define-key magit-mode-map (kbd "M-w") 'delete-window)
  (setq magit-completing-read-function #'completing-read)

  (defun meain/git-how-was-it ()
    (interactive)
    (let* ((filepath (magit-file-relative-name))
           (filename (file-name-nondirectory (magit-file-relative-name)))
           (line-no (line-number-at-pos))
           (mm major-mode)
           (branch (completing-read "Branch: " (magit-list-local-branch-names)))
           (buffer (get-buffer-create (format "*git-how-was-it %s:%s*" branch filename))))
      (message (concat "git show " branch ":" filepath))
      (other-window 1)
      (switch-to-buffer buffer)
      (erase-buffer)
      (insert (shell-command-to-string (concat "git show " branch ":" filepath)))
      (goto-line line-no) ;; will be different, but just a start
      (funcall mm))))

;; Structural diff using difftastic
(use-package difftastic
  :disabled t
  :elpaca (:host github :repo "pkryger/difftastic.el")
  :config
  (eval-after-load 'magit-diff
    '(transient-append-suffix 'magit-diff '(-1 -1)
       [("D" "Difftastic diff (dwim)" difftastic-magit-diff)
        ("S" "Difftastic show" difftastic-magit-show)])))

;; Magit forge
(use-package forge
  :elpaca t
  :defer t
  :after (magit evil-leader)
  :config (evil-leader/set-key "gF" 'forge-browse-dwim))

;; Github review
(use-package github-review
  :elpaca t
  :defer t
  :after forge
  :commands (github-review-start github-review-forge-pr-at-point))

(use-package smerge-mode
  :after (evil evil-leader)
  :config
  (evil-leader/set-key "gmr" 'smerge-refine)
  (evil-leader/set-key "gmn" 'smerge-next)
  (evil-leader/set-key "gmp" 'smerge-prev)
  (evil-leader/set-key "gmu" 'smerge-keep-upper)
  (evil-leader/set-key "gml" 'smerge-keep-lower)
  (evil-leader/set-key "gma" 'smerge-keep-all))

;; Diff hl
(use-package diff-hl
  :elpaca t
  :defer 1
  :after evil-leader
  :config
  (diff-hl-flydiff-mode)
  (global-diff-hl-mode)
  (let* ((height (frame-char-height)) (width 2) (bits (make-vector height 0)))
    (define-fringe-bitmap 'my-diff-hl-bitmap bits height width))
  (setq diff-hl-fringe-bmp-function (lambda (type pos) 'my-diff-hl-bitmap))
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode-unless-remote)
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  (evil-set-command-property 'diff-hl-revert-hunk :jump t)
  (evil-set-command-property 'diff-hl-next-hunk :jump t)
  (evil-set-command-property 'diff-hl-previous-hunk :jump t)
  (evil-leader/set-key "gs" 'diff-hl-show-hunk)
  (evil-leader/set-key "gr" 'diff-hl-revert-hunk)
  (evil-leader/set-key "gj" 'diff-hl-next-hunk)
  (evil-leader/set-key "gk" 'diff-hl-previous-hunk)
  (evil-leader/set-key "gn" 'diff-hl-next-hunk)
  (evil-leader/set-key "gp" 'diff-hl-previous-hunk))

;; Git blame info
(use-package blamer
  :elpaca t
  :after evil-leader
  :commands (blamer-show-commit-info blamer-mode global-blamer-mode)
  :config
  (setq blamer-idle-time 0.1)
  (setq blamer-min-offset 30)
  (setq blamer-commit-formatter ":: %s")
  (set-face-attribute 'blamer-face nil :height 0.9)
  (setq blamer-max-commit-message-length 90)
  (setq blamer-border-lines '(?+ ?- ?+ ?| ?+ ?+ )) ;; default one creates issues with spacing
  :init (evil-leader/set-key "G" 'blamer-show-commit-info))

;; Magit todo
(use-package magit-todos
  :elpaca t
  :disabled t
  :defer 1
  :config
  (magit-todos-mode))

;; Matchit
(use-package evil-matchit
  :elpaca t
  :defer 1
  :config (global-evil-matchit-mode 1))

;; Highlight color codes
(use-package rainbow-mode
  :elpaca t
  :commands (rainbow-mode)
  :init (add-hook 'css-mode-hook 'rainbow-mode))

;; Code folding
(use-package origami
  :elpaca t
  :after (evil evil-leader)
  :defer 1
  :config (global-origami-mode)
  :init
  (define-key evil-normal-state-map (kbd "<SPC> TAB") 'evil-toggle-fold)
  (evil-leader/set-key "o" 'evil-toggle-fold))

;; drag-stuff
(use-package drag-stuff
  :elpaca t
  :after evil
  :diminish
  :commands (drag-stuff-up drag-stuff-down drag-stuff-left drag-stuff-right)
  :init
  (define-key evil-visual-state-map (kbd "<up>") 'drag-stuff-up)
  (define-key evil-visual-state-map (kbd "<down>") 'drag-stuff-down)
  (define-key evil-visual-state-map (kbd "<left>") 'drag-stuff-left)
  (define-key evil-visual-state-map (kbd "<right>") 'drag-stuff-right)
  :config
  (drag-stuff-mode t)
  (drag-stuff-global-mode 1))

;; Saveplace
(use-package saveplace
  :init
  (save-place-mode t)
  (setq save-place-file "~/.local/share/emacs/saveplace"))

;; Persistent undo using undo-tree
(use-package undo-tree
  :elpaca t
  :diminish
  :config
  (global-undo-tree-mode t)
  (setq undo-limit 80000000)
  (setq evil-want-fine-undo nil)
  (setq undo-tree-auto-save-history t)
  (setq undo-tree-history-directory-alist '(("." . "~/.local/share/emacs/undo"))))

;; Fancier tab management
(use-package tab-bar
  :after evil-leader
  :defer 3
  :commands (tab-close tab-new tab-next tab-bar-rename-tab
                       meain/switch-tab-dwim meain/create-or-delete-tab
                       tab-bar-switch-to-tab)
  :init
  (global-set-key (kbd "M-f ,") 'tab-bar-rename-tab)
  (global-set-key (kbd "M-o") 'meain/switch-tab-dwim)
  (evil-leader/set-key "t" 'meain/switch-tab-dwim)
  (evil-leader/set-key "T" 'meain/create-or-delete-tab)
  (evil-leader/set-key "C"
    (lambda ()
      (interactive)
      ;; TODO: make notmuch and elfeed automatically open up in scratch tab
      (tab-bar-switch-to-tab "scratch")))
  (global-set-key (kbd "M-f s") 'meain/switch-tab-dwim)
  :config
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
    "Switch between available tabs.
Pass `CHOOSER' as t to not automatically select the previous tab."
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
         (t (tab-bar-switch-to-tab (car tabs))))))))

;; which-key mode
(use-package which-key
  :elpaca t
  :defer 1
  :diminish
  :config
  (which-key-mode))

;; Expand region
(use-package expand-region
  :elpaca t
  :commands (er/expand-region)
  :config
  ;; make evil jump list work with expand-region
  (evil-set-command-property 'er/expand-region :jump t)
  :init
  (global-set-key (kbd "M--") 'er/expand-region))

;; dtrt (atuo find indend setting)
(use-package dtrt-indent
  :elpaca t
  :defer 1
  :diminish
  :config (dtrt-indent-global-mode))

(use-package indent-guide
  :elpaca t
  :after (evil-leader)
  :commands (indent-guide-global-mode indent-guide-mode)
  :init
  (setq indent-guide-delay nil)
  (setq indent-guide-char "¦") ; Other chars │
  (setq indent-guide-recursive t)
  (evil-leader/set-key "b I" 'indent-guide-global-mode)
  :config
  (set-face-attribute 'indent-guide-face nil :foreground "#DDD"))

;; vterm setup
(use-package vterm
  :elpaca t
  :defer t
  :after evil
  :commands (vterm meain/shell-toggle)
  ;; :init (global-set-key (kbd "M-;") 'meain/shell-toggle)
  :config
  (evil-set-initial-state 'vterm-mode 'insert)
  (setq vterm-max-scrollback 100000)
  (setq vterm-kill-buffer-on-exit t)
  (add-hook 'vterm-mode-hook (lambda ()
                               (setq imenu-generic-expression '((nil "^![a-zA-Z0-9_-]+ .+" 0)))))
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
  (add-to-list 'display-buffer-alist '("\\*popup-shell-.*"
                                       (display-buffer-reuse-window display-buffer-at-bottom)
                                       (reusable-frames . visible)
                                       (window-height . 0.3)))
  
  (defun meain/shell-name ()
    "Get the name of the shell based on project info."
    (format "*popup-shell-%s*"
            (if (project-current)
                (meain/project-name)
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
    (if (or always-create
            (s-starts-with-p "*popup-shell" (buffer-name)))
        (progn
          (if (s-starts-with-p "*popup-shell" (buffer-name))
              (delete-window))
          (vterm (meain/shell-name)))
      (call-interactively 'switch-to-buffer)))
  (defun meain/shell-other (&optional alternate)
    "Switch to previous shell in current project. Use ALTERNATE to get a list of shell in current project."
    (interactive "P")
    (let ((shell-buffers (remove-if-not (lambda (x)
                                          (s-starts-with-p (meain/shell-name) (buffer-name x)))
                                        (buffer-list))))
      (cond
       ((equal (length shell-buffers) 0) (message "No shells bruh!"))
       ((equal (length shell-buffers) 1) (message "Only one shell"))
       (alternate (switch-to-buffer
                   (completing-read "Choose shell: "
                                    (mapcar (lambda (x) (buffer-name x)) shell-buffers))))
       (t (switch-to-buffer (car (cdr shell-buffers)))))))
  (defun meain/run-in-vterm-kill (process event)
    "A process sentinel. Kills PROCESS's buffer if it is live."
    (let ((b (process-buffer process)))
      (and (buffer-live-p b) (kill-buffer b) (delete-window))))
  (defun meain/run-in-vterm (command)
    "Execute string COMMAND in a new vterm and kill the shell once done.  Useful for interactive items."
    (interactive (list (let* ((f (cond (buffer-file-name)
                                       ((eq major-mode 'dired-mode) (dired-get-filename nil t))))
                              (filename (concat " " (shell-quote-argument (and f (file-relative-name f))))))
                         (read-shell-command "Terminal command: "
                                             (cons filename 0)
                                             (cons 'shell-command-history 1)
                                             (list filename)))))
    (with-current-buffer (vterm (concat "*popup-shell-" command "*"))
      (set-process-sentinel vterm--process #'meain/run-in-vterm-kill)
      (vterm-send-string (concat command ";exit 0"))
      (vterm-send-return)))
  (defun meain/clear-and-exec ()
    (interactive)
    (vterm-clear)
    (vterm-clear-scrollback)
    (vterm-send-return))
  (define-key vterm-mode-map [(S-return)] 'meain/clear-and-exec)
  (defun meain/vterm--kill-vterm-buffer-and-window (process event)
    "Kill buffer and window on vterm PROCESS termination.  EVENT is the close event."
    (when (not (process-live-p process))
      (let ((buf (process-buffer process))
            (bufname (buffer-name (current-buffer))))
        (when (buffer-live-p buf)
          (with-current-buffer buf
            (kill-buffer)
            (ignore-errors (delete-window))
            (when (s-starts-with-p "terminal" bufname) ; wm opened term
              (delete-frame)))))))
  (add-hook 'vterm-mode-hook
            (lambda ()
              (set-process-sentinel (get-buffer-process (buffer-name))
                                    #'meain/vterm--kill-vterm-buffer-and-window))))

;; ranger in emacs
(use-package ranger
  :elpaca t
  :commands ranger
  :config
  (use-package image-dired+
    :elpaca t
    :config (image-diredx-async-mode)))

;; editorconfig
(use-package editorconfig
  :defer 1
  :elpaca t
  :config (editorconfig-mode 1))

;; eros for eval
(use-package eros
  :elpaca t
  :commands (eros-eval-last-sexp meain/eval-last-sexp)
  :after evil-leader
  :init
  (evil-leader/set-key ";" 'meain/eval-last-sexp)
  :config
  (eros-mode)
  (defun meain/eval-last-sexp (&optional alternate)
    "Do `eval-last-sexp'.  Pass ALTERNATE to go to end of line and do the same."
    (interactive "P")
    (if alternate
        (save-excursion
          (end-of-line)
          (eros-eval-last-sexp nil))
      (save-excursion
        (search-forward ")")
        (eros-eval-last-sexp nil)))))

;; Quick calculations
(use-package emacs
  :commands (meain/calc-eval)
  :after evil-leader
  :init
  (evil-leader/set-key ":" 'meain/calc-eval)
  :config
  (defun meain/calc-eval (start end)
    (interactive "r")
    (let ((thing (if (use-region-p)
                     (buffer-substring start end)
                   (thing-at-point 'line))))
      (if current-prefix-arg ; replace in that case
          (progn
            (if (use-region-p) (goto-char end) (end-of-line))
            (insert " = " (calc-eval thing)))
        (message "%s" (calc-eval thing))))))

;; Virtualenv
(use-package virtualenvwrapper
  :elpaca t
  :commands venv-workon
  :init (setq venv-location "~/.local/share/virtual_envs"))

;; Quick run current test
(use-package emacs
  :after (compile evil-leader)
  :commands (meain/toffee-run-test meain/toffee--get-test-command)
  :config
  ;; if available in another frame, don't recreate in current frame
  (push '("\\*compilation\\*" . (nil (reusable-frames . t))) display-buffer-alist)
  (defvar meain/toffee--previous-command nil)
  (defvar meain/toffee-run-previous-if-empty t)
  (defun meain/toffee--get-test-command (mode)
    (let ((default-directory
           (expand-file-name
            ;; custom-src-directory is supposed to come from .dir-locals.el
            (if (boundp 'custom-src-directory)
                custom-src-directory
              default-directory)))
          (command
           (shell-command-to-string
            (cond
             ((eq mode 'function) (format "toffee --verbose '%s' '%s'" (buffer-file-name) (line-number-at-pos)))
             ((eq mode 'suite) (format "toffee --verbose '%s'" (buffer-file-name)))
             ((eq mode 'project) (format "toffee --verbose --full '%s'" (buffer-file-name)))
             (t (error "Unknown mode for meain/toffee--get-test-command"))))))
      (if (s-starts-with-p "Unable to find any test" command)
          (cons default-directory nil)
        (cons default-directory (s-trim command)))))
  (defun meain/toffee-run-previous-test ()
    "Run previous test."
    (interactive)
    (let* ((dir-cmd (meain/toffee--get-test-command  'function))
           (default-directory (car dir-cmd))
           (command meain/toffee--previous-command))
      (if command
          (progn (compile (concat "nice " command)))
        (message "Unable to find any tests"))))
  (defun meain/toffee-run-test (&optional _)
    "Run test based on `MODE'. By default runs current function.
Pass universal args to run suite or project level tests."
    (interactive "P")
    (let* ((mode (cond
                  ((equal current-prefix-arg nil) 'function)
                  ((equal current-prefix-arg '(4)) 'suite)
                  ((equal current-prefix-arg '(16)) 'project)))
           (dir-cmd (meain/toffee--get-test-command  mode))
           (default-directory (car dir-cmd))
           (command (cdr dir-cmd)))
      (if command
          (progn
            (setq meain/toffee--previous-command command)
            (compile (concat "nice " command)))
        (if (and meain/toffee-run-previous-if-empty meain/toffee--previous-command)
            (progn
              (message "Could not find any tests, running previous test...")
              (compile (concat "nice " meain/toffee--previous-command)))
          (message "Unable to find any tests")))))
  :init
  (evil-leader/set-key "d" 'meain/toffee-run-test)
  (evil-leader/set-key "D" 'meain/toffee-run-previous-test))

;; Neotree
(use-package neotree
  :elpaca t
  :commands neotree
  :config
  (setq neo-window-fixed-size nil)
  (setq neo-theme 'classic)
  (add-hook 'neo-after-create-hook 'hl-line-mode))

;; Evil keybindings for a lot of things
(use-package evil-collection
  :defer 1
  :elpaca t
  :after evil
  :config
  (setq evil-collection-magit-want-horizontal-movement t)
  (setq evil-collection-magit-use-y-for-yank t)
  (evil-collection-init))

;; Highlight TODO items
(use-package hl-todo
  :elpaca t
  :defer 1
  :config
  (setq hl-todo-keyword-faces '(("TODO" . "#FF0000")
                                ("FIXME" . "#FF0000")
                                ("GOTCHA" . "#FF4500")
                                ("STUB" . "#1E90FF")
                                ("NOTE" . "#0090FF")
                                ("XXX" . "#AF0494")))
  (global-hl-todo-mode))

;; Emmet for html stuff (c-j to activate)
(use-package emmet-mode
  :elpaca t
  :defer t
  :commands (emmet-mode))

;; Direnv support
(use-package envrc
  :elpaca t
  :defer 1
  :config (envrc-global-mode))

(use-package emacs
  :config
  :commands (meain/cwd-fn meain/use-custom-src-directory)
  :config
  (defun meain/cwd-fn ()
    (expand-file-name
     ;; custom-src-directory is supposed to come from .dir-locals.el
     (if (boundp 'custom-src-directory)
         custom-src-directory
       (or (when-let ((project (project-current)))
             (project-root project))
           default-directory))))

  (defun meain/use-custom-src-directory (orig-fn &rest args)
    "Use custom src directory as default directory.
Instead of `default-directory' when calling `ORIG-FN' with `ARGS'."
    (let ((default-directory
           (expand-file-name
            ;; custom-src-directory is supposed to come from .dir-locals.el
            (if (boundp 'custom-src-directory)
                custom-src-directory
              default-directory))))
      (apply orig-fn args))))

;;; [FILETYPE PUGINS] ===============================================

(use-package rust-mode :elpaca t :defer t)
(use-package clojure-mode :elpaca t :defer t)
(use-package zig-mode :elpaca t :defer t)
(use-package go-mode
  :elpaca t
  :defer t
  :config
  (evil-set-command-property 'godef-jump :jump t))
(use-package go-fill-struct
  :elpaca t
  :commands (go-fill-struct))
(use-package go-tag
  :elpaca t
  :commands (go-tag-add go-tag-remove go-tag-refresh)
  :config (setq go-tag-args (list "-transform" "camelcase")))
(use-package go-impl
  :elpaca t
  :commands (go-impl)
  :config (advice-add 'go-impl :around #'meain/use-custom-src-directory))
(use-package go-stacktracer :elpaca t :commands (go-stacktracer-region))
(use-package go-guru :defer t :elpaca t)
(use-package go-dlv
  :elpaca t
  :defer t
  :config
  (defun meain/dlv-current-func ()
    (interactive)
    (let ((default-directory (if (boundp 'custom-src-directory)
                                 custom-src-directory
                               default-directory)))
      (call-interactively 'dlv-current-func)))
  (defun meain/dlv-replay ()
    (interactive)
    (let* ((default-default-directory (if (boundp 'custom-src-directory)
                                          custom-src-directory
                                        default-directory))
           (default-directory (completing-read
                               "Directory: "
                               (remove-if (lambda (x) (equalp x ""))
                                          (mapcar (lambda (x) (concat default-directory x))
                                                  (string-split (shell-command-to-string "fd -t d") "\n")))
                               nil t default-default-directory)))
      (dlv "dlv replay /home/meain/.local/share/rr/latest-trace")))
  (defun meain/dlv (&optional test)
    (interactive "P")
    (let* ((default-default-directory (if (boundp 'custom-src-directory)
                                          custom-src-directory
                                        default-directory))
           (default-directory (completing-read
                               "Directory: "
                               (remove-if (lambda (x) (equalp x ""))
                                          (mapcar (lambda (x) (concat default-directory x))
                                                  (string-split (shell-command-to-string "fd -t d") "\n")))
                               nil t default-default-directory)))
      (if test
          (let* ((dir-cmd (meain/toffee--get-test-command 'function))
                 (default-directory (car dir-cmd))
                 (command (cdr dir-cmd))
                 (test-dir (progn
                             (if (eq nil command)
                                 (error "No tests available"))
                             (car (reverse (string-split command " ")))))
                 (command-without-dir (string-join (reverse (cdr (reverse (string-split command " ")))) " "))
                 (dlv-command (s-replace-regexp
                               "^go test -v -run"
                               (format "dlv --backend rr test %s -- -test.v -test.run" test-dir)
                               command-without-dir)))
            (message dlv-command)
            (dlv dlv-command))
        (call-interactively 'dlv))))
  :commands (dlv dlv-current-func meain/dlv meain/dlv-replay meain/dlv-current-func))
(use-package lua-mode :elpaca t :defer t)
(use-package web-mode :elpaca t :defer t)
(use-package jinja2-mode :elpaca t :defer t)
(use-package config-general-mode :elpaca t :defer t :mode "/\\.env")
(use-package vimrc-mode :elpaca t :defer t)
(use-package sxhkdrc-mode :elpaca t :defer t)
(use-package edit-indirect :elpaca t)
(use-package markdown-mode
  :elpaca t
  :defer t
  :after (edit-indirect)
  :mode ("\\.md\\'" . gfm-mode)
  :config
  (setq markdown-url-compose-char '(8230 8943 35 9733 9875))
  (setq markdown-enable-html -1)
  (evil-define-key 'normal gfm-mode-map (kbd "<RET>") 'project-find-file)
  (evil-define-key 'normal gfm-mode-map (kbd "g d") 'markdown-do)
  (evil-define-key 'normal markdown-mode-map (kbd "<RET>") 'project-find-file)
  (evil-define-key 'normal markdown-mode-map (kbd "g d") 'markdown-do)
  (setq markdown-command "pandoc -t html5")
  (setq markdown-fontify-code-blocks-natively t))
(use-package reformatter :elpaca t :defer t) ;; needed by nix-mode
(use-package nix-mode
  :elpaca t
  :defer t
  :mode "\\.nix\\'"
  :config
  (add-hook 'nix-mode-hook (lambda ()
                             (setq imenu-create-index-function #'meain/imenu-config-nesting-path))))
;; builtin package for scheme (for tree-sitter grammar)
(use-package scheme-mode :defer t :mode "\\.scm\\'")
(use-package csv-mode
  :elpaca t
  :defer t
  :config
  (setq csv-align-mode t)
  (set-face-attribute 'csv-separator-face nil
                      :background "gray100"
                      :foreground "#000000"))
(use-package json-mode
  :elpaca t
  :defer t
  :config
  (add-hook 'json-mode-hook (lambda ()
                              (setq imenu-create-index-function #'meain/imenu-config-nesting-path))))
(use-package yaml-mode
  :elpaca t
  :defer t
  :config
  (remove-hook 'yaml-mode-hook 'yaml-set-imenu-generic-expression) ;; don't use default one
  (add-hook 'yaml-mode-hook (lambda ()
                              (setq imenu-create-index-function #'meain/imenu-config-nesting-path))))
(use-package ini-mode :elpaca t :defer t)
(use-package dockerfile-mode :elpaca t :defer t :mode "/Dockerfile")
(use-package docker-compose-mode :elpaca t :defer t)
(use-package protobuf-mode :elpaca t :defer t)
(use-package org
  :commands (org-mode org-timer org-timer-set-timer)
  :mode "/\\.org\\'"
  :config
  (use-package org-timer
    :config
    (setq org-clock-sound "~/.config/datafiles/sounds/timer.mp3"))
  (setq org-agenda-files (list "~/.local/share/org/master.org"))
  (setq org-log-done 'time)
  (setq org-todo-keywords '((sequence "TODO" "|" "DONE" "CANCELLED")))
  (global-set-key (kbd "M-f j") 'org-agenda-list)
  (evil-define-key 'normal org-mode-map (kbd "M-l") 'meain/move-swap-right)
  (evil-define-key 'normal org-mode-map (kbd "M-h") 'meain/move-swap-left)
  (evil-define-key 'normal org-mode-map (kbd "M-k") 'meain/move-swap-up)
  (evil-define-key 'normal org-mode-map (kbd "M-j") 'meain/move-swap-down)
  (evil-define-key 'normal org-mode-map (kbd "gk") 'org-backward-heading-same-level)
  (evil-define-key 'normal org-mode-map (kbd "gj") 'org-forward-heading-same-level)
  (evil-define-key 'normal org-mode-map (kbd "gK") 'org-move-subtree-up)
  (evil-define-key 'normal org-mode-map (kbd "gJ") 'org-move-subtree-down)
  (evil-define-key 'normal org-mode-map (kbd "gH") 'org-promote-subtree)
  (evil-define-key 'normal org-mode-map (kbd "gL") 'org-demote-subtree)
  (evil-define-key 'normal org-mode-map (kbd "gt") 'org-todo)
  (evil-define-key 'normal org-mode-map (kbd "gr") 'org-ctrl-c-ctrl-c))
(use-package org-modern
  :elpaca t
  :disabled t
  :after org
  :commands (org-modern-mode org-modern-agenda)
  :init
  (add-hook 'org-mode-hook #'org-modern-mode)
  (add-hook 'org-agenda-finalize-hook #'org-modern-agenda))

;; for kmonad files
(use-package kbd-mode
  :defer t
  :mode "\\.kbd\\'"
  :elpaca (kbd-mode :host github
                    :repo "kmonad/kbd-mode"))

;; Show metadata for binary files instead of opening them
(use-package eff
  :elpaca (:host github :repo "oxidase/eff-mode"))

;; mtodo-mode
(use-package emacs
  :after evil
  :config
  (load (expand-file-name "~/.config/emacs/mtodo-mode.el"))
  (add-hook 'mtodo-mode-hook (lambda ()
                               (setq imenu-generic-expression '((nil "^#+\s+.+" 0)))))
  (evil-define-key 'normal mtodo-mode-map (kbd "g d") 'mtodo-mark-done)
  (evil-define-key 'normal mtodo-mode-map (kbd "g m") 'mtodo-mark-undone)
  (evil-define-key 'normal mtodo-mode-map (kbd "g s") 'mtodo-mark-important))

;;; [EXTRA PLUGINS] =================================================

;; DAP client for Emacs
(use-package dape
  :elpaca (dape :type git :host github :repo "svaante/dape")
  :config
  (setq dape-inline-variables t) ;; Add inline variable hints, this feature is highly experimental
  (setq dape-repl-use-shorthand t) ;; Use n for next etc. in REPL
  (setq dape-cwd-fn 'meain/cwd-fn)
  ;; (remove-hook 'dape-on-start-hooks 'dape-info) ;; To remove info buffer on startup
  ;; (remove-hook 'dape-on-start-hooks 'dape-repl) ;; To remove repl buffer on startup
  ;; (add-hook 'dape-compile-compile-hooks 'kill-buffer) ;; Kill compile buffer on build success

  ;; Golang config
  (add-to-list 'dape-configs
               `(delve
                 modes (go-mode go-ts-mode)
                 command "dlv"
                 command-args ("dap" "--listen" "127.0.0.1:55878")
                 command-cwd meain/cwd-fn
                 host "127.0.0.1"
                 port 55878
                 :type "debug"
                 :request "launch"
                 :cwd meain/cwd-fn
                 :program meain/cwd-fn)))

(use-package gud
  :after (evil)
  :init
  (define-key evil-normal-state-map (kbd "<SPC> d r") 'gud-reset)
  (define-key evil-normal-state-map (kbd "<SPC> d b") 'gud-break)
  (define-key evil-normal-state-map (kbd "<SPC> d c") 'gud-cont)
  (define-key evil-normal-state-map (kbd "<SPC> d n") 'gud-next)
  (define-key evil-normal-state-map (kbd "<SPC> d s") 'gud-step)
  (define-key evil-normal-state-map (kbd "<SPC> d u") 'gud-up)
  (define-key evil-normal-state-map (kbd "<SPC> d g") 'gud-until))

(use-package hydra
  :elpaca t
  :commands (defhydra))

(use-package emacs
  :after (evil hydra gud)
  :commands (hydra-gud/body)
  :init
  (defhydra hydra-gud ()
    "gud"
    ("n" gud-next "next")
    ("c" gud-cont "continue")
    ("r" 'gud-reset "reset")
    ("b" 'gud-break "break")
    ("s" 'gud-step "step")
    ("u" 'gud-up "up")
    ("g" 'gud-until "go till"))

  (define-key evil-normal-state-map (kbd "<SPC> d d") 'hydra-gud/body))

;; Dashboard
(use-package dashboard
  :disabled t
  :elpaca t
  :config
  (setq dashboard-banner-logo-title nil)
  (setq dashboard-center-content t)
  (setq dashboard-show-shortcuts t)
  (setq dashboard-startup-banner 'official)
  (setq dashboard-items '((recents . 3) (projects . 7)))
  (setq dashboard-projects-backend 'project)
  (setq dashboard-set-navigator t)
  (setq dashboard-set-footer nil)
  (dashboard-setup-startup-hook))

;; Winner mode
(use-package winner
  :defer 1
  :config
  (global-set-key (kbd "M-f <left>") 'winner-undo)
  (global-set-key (kbd "M-f <right>") 'winner-redo)
  (winner-mode))

;; Process management
(use-package proced
  :commands proced
  :config
  (setq proced-enable-color-flag t))

;; notmuch
(use-package notmuch
  :elpaca t
  :commands notmuch
  :after evil-leader
  :init
  (evil-leader/set-key "a n" 'notmuch)
  :config
  (evil-define-key 'normal notmuch-search-mode-map (kbd "u") 'evil-collection-notmuch-search-toggle-unread)
  (evil-define-key 'normal notmuch-show-mode-map (kbd "U") 'notmuch-show-browse-urls)
  (evil-define-key 'normal notmuch-show-mode-map (kbd "u") 'meain/notmuch-show-close-all-but-unread)
  (evil-define-key 'normal notmuch-show-mode-map (kbd "M-k") 'meain/move-swap-up)
  (evil-define-key 'normal notmuch-show-mode-map (kbd "M-j") 'meain/move-swap-down)
  (evil-define-key 'normal notmuch-show-mode-map (kbd "M-s m f") 'meain/find-emails-from-same-sender)
  ;; Advice to reset unread mail counter on exit
  (advice-add 'notmuch-bury-or-kill-this-buffer
              :after (lambda (&rest r) (meain/update-scratch-message)))
  (defun meain/find-emails-from-same-sender ()
    (interactive)
    (let* ((props (notmuch-show-get-message-properties))
           (from (plist-get (plist-get props :headers) :From)))
      (message "Searching for %s" from)
      (notmuch-search (concat "from:" from))))
  (defun meain/notmuch-show-close-all-but-unread ()
    "Close all messages until the first unread item."
    (interactive)
    (goto-char (point-min))
    (cl-loop do
             (notmuch-show-message-visible (notmuch-show-get-message-properties) nil)
             until
             (or (not (notmuch-show-goto-message-next))
                 (member "unread" (plist-get (notmuch-show-get-message-properties) :tags))))
    ;; make sure last message is open
    (notmuch-show-message-visible (notmuch-show-get-message-properties) t)
    (force-window-update))
  (setq notmuch-hello-logo nil)
  (setq notmuch-search-oldest-first nil)
  (setq notmuch-message-headers-visible nil)
  (setq message-auto-save-directory "~/.local/share/mail/meain")
  (setq notmuch-saved-searches
        '(
          (:name "Important" :query "tag:important AND tag:inbox" :key "i" :sort-order newest-first)
          (:name "Low priority" :query "tag:lowpri AND tag:inbox" :key "l" :sort-order newest-first)
          (:name "Flagged" :query "tag:flagged" :key "f")
          (:name "Todo" :query "tag:todo" :key "t")
          (:name "Github" :query "tag:github AND tag:inbox" :key "h")

          (:name "Alerts" :query "tag:alert AND tag:inbox" :key "a")
          (:name "Updates" :query "tag:update AND tag:inbox" :key "u")

          (:name "Newsletter" :query "tag:newsletter AND tag:inbox" :key "n")
          (:name "Unnecessary" :query "tag:unnecessary AND tag:inbox" :key "N" :sort-order newest-first)

          (:name "Untagged" :query "tag:untagged AND tag:inbox AND -tag:work" :key "U" :sort-order newest-first)
          (:name "Work untagged" :query "tag:untagged AND tag:inbox AND tag:work" :key "W" :sort-order newest-first)

          (:name "All Inbox" :query "tag:inbox" :key "B" :sort-order newest-first)
          (:name "All mail" :query "path:meain/** OR path:mail/**" :key "A" :sort-order newest-first)
          (:name "All work mail" :query "path:ic/**" :key "Z" :sort-order newest-first)))

  ;; Updating mailtag scripts
  (defun meain/update-mailtag-entry ()
    "Update the mailtag entry in current line to the tags from previous line."
    (interactive)

    ;; Get prev line format
    (next-line -1)
    (beginning-of-line)
    (kill-line)
    (yank)

    ;; Get current line
    (next-line)
    (beginning-of-line)
    (kill-line)

    ;; Update line with prev format
    (yank 2)

    (end-of-line)
    (search-backward ":")
    (forward-char)
    (kill-line)
    (yank 2)
    (insert "'"))

  (defun meain/add-to-mailtag ()
    "Add current email to mailtag file."
    (interactive)
    (let* (;; email sender
           (props (notmuch-show-get-message-properties))
           (sender (plist-get (plist-get props :headers) :From))
           (email (if (s-contains-p "<" sender)
                      (string-trim (car (string-split (cadr (string-split sender "<")) ">")))
                    sender))

           ;; location to paste
           (mailtag-file (car (string-split (shell-command-to-string "where mailtag") "\n")))
           (contents (with-temp-buffer
                       (insert-file-contents mailtag-file)
                       (buffer-string)))
           (lines (split-string contents "\n"))
           (headers (seq-filter (lambda (x) (string-prefix-p "##" x)) lines))
           (header (completing-read "Header: " headers)))
      (find-file mailtag-file) ;; (with-temp-buffer) was causing a lot of trouble somehow
      (goto-char (point-min))
      (re-search-forward (concat "^" header "$"))
      (forward-paragraph)
      (insert (concat email "\n"))
      (message "Post insert: %s %s %s" (point) (line-number-at-pos) (thing-at-point 'line))
      (goto-char (1- (point))) ;; (previous-line) does not seem to work
      (meain/update-mailtag-entry)
      (save-buffer mailtag-file)
      (previous-buffer)
      (message "Setup %s under %s" email header)
      (shell-command "mailtag")))


  ;; sending emails
  (setq mail-signature t)
  (setq mail-signature-file "~/.config/datafiles/mailsignature")
  (setq message-kill-buffer-on-exit t) ; kill buffer after sending mail
  (setq mail-specify-envelope-from t) ; Settings to work with msmtp
  (setq message-sendmail-envelope-from 'header)
  (setq mail-envelope-from 'header)
  (setq notmuch-fcc-dirs "Sent +sent -unread") ; stores sent mail to the specified directory
  (setq message-directory "~/.local/share/mail") ; stores postponed messages to the specified directory
  (setq sendmail-program "msmtp")
  (setq send-mail-function 'smtpmail-send-it)
  (setq message-sendmail-f-is-evil t)
  (setq message-default-mail-headers "Cc: \nBcc: \n")
  (setq message-sendmail-extra-arguments '("--read-envelope-from"))
  (setq message-send-mail-function 'message-send-mail-with-sendmail))

;; gnus
(use-package gnus
  :commands gnus
  :config
  (evil-define-key 'normal gnus-article-mode-map (kbd "M-n") 'gnus-summary-next-article) ;; <space> is always available
  (evil-define-key 'normal gnus-summary-mode-map (kbd "M-n") 'gnus-summary-next-article)
  (evil-define-key 'normal gnus-article-mode-map (kbd "p") 'gnus-summary-refer-parent-article)
  (evil-define-key 'normal gnus-summary-mode-map (kbd "p") 'gnus-summary-refer-parent-article)
  (evil-define-key 'normal gnus-summary-mode-map (kbd "P") 'gnus-summary-refer-thread) ; fetch all the messages in thread (useful for bookmarked)
  (evil-define-key 'normal gnus-summary-mode-map (kbd "a") 'gnus-summary-kill-thread)
  (evil-define-key 'normal gnus-group-mode-map (kbd "a") 'gnus-group-catchup-current-all)
  (setq gnus-select-method '(nnnil ""))
  (setq gnus-directory "~/.config/emacs/news")
  (setq gnus-secondary-select-methods
        '((nntp "news.gmane.io")))
  (add-hook 'gnus-group-mode-hook 'hl-line-mode)
  (add-hook 'gnus-summary-mode-hook 'hl-line-mode))

;; elfeed
(use-package elfeed
  :elpaca t
  :disabled t
  :commands (elfeed elfeed-update)
  :after (avl-tree evil-leader)
  :init
  ;; first run after 1 hour
  (use-package avl-tree)
  (run-at-time "1 hour" (* 6 60 60) (lambda () (elfeed-update) (elfeed-db-save)))
  (evil-leader/set-key "a e" 'elfeed)
  :config
  (setq elfeed-sort-order 'ascending)
  (setq elfeed-curl-timeout 60)
  (setq browse-url-browser-function 'browse-url-default-browser)
  (setq browse-url-generic-program "open")
  (setq browse-url-generic-args nil)
  (evil-define-key 'normal elfeed-search-mode-map (kbd "o") (meain/inlambda elfeed-search-browse-url t))
  (evil-define-key 'visual elfeed-search-mode-map (kbd "o") 'elfeed-search-browse-url)
  (evil-define-key 'normal elfeed-search-mode-map (kbd "d") 'meain/elfeed-search-filter)
  (evil-define-key 'normal elfeed-search-mode-map (kbd "f") 'meain/elfeed-search-filter-by-name)
  (evil-define-key 'normal elfeed-search-mode-map (kbd "D") (lambda ()
                                                              (interactive)
                                                              (setq elfeed-search-filter "@2-months-ago -nah +unread")
                                                              (elfeed-search-update :force)))
  (evil-define-key 'normal elfeed-search-mode-map (kbd "q") 'elfeed-db-unload)
  (defun meain/elfeed-open-all ()
    (interactive)
    (with-current-buffer "*elfeed-search*"
      (cl-loop for entry in elfeed-search-entries
               collect (browse-url (elfeed-entry-link entry))))
    (elfeed-untag elfeed-search-entries 'unread)
    (mapc #'elfeed-search-update-entry elfeed-search-entries))
  (evil-define-key 'normal elfeed-search-mode-map (kbd "O") 'meain/elfeed-open-all)
  (defun meain/elfeed-search-filter ()
    (interactive)
    (setq elfeed-search-filter "@2-months-ago -nah +unread")
    (elfeed-search-update :force)
    (let ((tag
           (completing-read
            "Apply tag: "
            (remove-if (lambda (x) (equalp x 'unread))
                       (delete-dups (flatten-list
                                     (cl-list*
                                      (with-current-buffer "*elfeed-search*"
                                        (cl-loop for entry in elfeed-search-entries
                                                 collect (elfeed-entry-tags entry)))))))
            nil t "\\.")))
      (setq elfeed-search-filter (concatenate 'string "@2-months-ago -nah +unread +" tag))
      (elfeed-search-update :force)
      (evil-goto-first-line)))
  (defun meain/elfeed-search-filter-by-name ()
    (interactive)
    (setq elfeed-search-filter (mapconcat 'identity
                                          (remove-if-not (lambda (x)
                                                           (or (string-prefix-p "+" x)
                                                               (string-prefix-p "-" x)
                                                               (string-prefix-p "@" x)))
                                                         (split-string elfeed-search-filter))
                                          " "))
    (elfeed-search-update :force)
    (let ((site
           (completing-read
            "Look for: "
            (remove-if (lambda (x) (equalp x 'unread))
                       (delete-dups
                        (flatten-list
                         (cl-list* (with-current-buffer "*elfeed-search*"
                                     (cl-loop for entry in elfeed-search-entries
                                              collect (cl-struct-slot-value
                                                       (type-of (elfeed-entry-feed (car elfeed-search-entries)))
                                                       'title
                                                       (elfeed-entry-feed entry)))))))))))
      ;; Need \s- instead of just a simple space because elfeed has issues with space in title
      (setq elfeed-search-filter (concatenate 'string
                                              elfeed-search-filter
                                              " ="
                                              (mapconcat 'identity
                                                         (split-string site)
                                                         "\\s-")))
      (elfeed-search-update :force)
      (evil-goto-first-line)))
  (setq-default elfeed-search-filter "@2-months-ago -nah +unread ")
  (setq elfeed-use-curl t)
  (setq elfeed-curl-max-connections 10)
  (setq elfeed-db-directory "~/.config/emacs/elfeed/")
  (setq elfeed-enclosure-default-dir "~/Downloads/")
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
           (tags-width 50)
           (title (or (elfeed-meta entry :title) (elfeed-entry-title entry) ""))
           (title-faces (elfeed-search--faces (elfeed-entry-tags entry)))
           (feed (elfeed-entry-feed entry))
           (feed-title (when feed (or (elfeed-meta feed :title) (elfeed-feed-title feed))))
           (tags (mapcar #'symbol-name (elfeed-entry-tags entry)))
           (tags-str (concat " (" (mapconcat 'identity tags ",") ")"))
           (title-width (- (window-width) feed-width tags-width 4))
           (title-column (elfeed-format-column title
                                               (elfeed-clamp elfeed-search-title-min-width
                                                             title-width elfeed-search-title-max-width)
                                               :left))
           (tag-column (elfeed-format-column tags-str
                                             (elfeed-clamp (length tags-str) tags-width tags-width)
                                             :left))
           (feed-column (elfeed-format-column feed-title
                                              (elfeed-clamp feed-width feed-width feed-width)
                                              :left)))
      (insert (propertize feed-column 'face 'elfeed-search-feed-face) " ")
      (insert (propertize title 'face title-faces 'kbd-help title))
      (insert (propertize tag-column 'face 'elfeed-search-tag-face) " ")))
  (setq elfeed-search-print-entry-function 'meain/elfeed-search-print)
  (defun meain/elfeed-display-buffer (buf &optional act &rest _)
    (pop-to-buffer buf))
  (setq elfeed-show-entry-switch #'meain/elfeed-display-buffer)
  (defun meain/elfeed-show-next-prev (&optional prev)
    "Go to next elfeed entry.  Pass PREV to switch to prev entry."
    (interactive)
    (if (equal (buffer-name) "*elfeed-entry*")
        (delete-window))
    (switch-to-buffer "*elfeed-search*")
    (if prev
        (previous-line 2))
    (pulse-momentary-highlight-one-line (point))
    (call-interactively 'elfeed-search-show-entry))
  (evil-define-key 'normal elfeed-show-mode-map (kbd "M-n") 'meain/elfeed-show-next-prev)
  (evil-define-key 'normal elfeed-show-mode-map (kbd "M-p") (meain/inlambda meain/elfeed-show-next-prev t))
  (defun meain/elfeed-enclosure-download (base-dir extension)
    "Download podcast to `BASE-DIR' with proper heirary using feed and title using `EXTENSION'"
    (start-process "*elfeed-enclosure-download*"
                   "*elfeed-enclosure-download*"
                   "downloader"
                   (elt (car (elfeed-entry-enclosures elfeed-show-entry)) 0)
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
    (meain/elfeed-enclosure-download "Downloads/podcasts" ".mp3"))
  (load-file "~/.config/emacs/elfeed-feeds.el"))

;; erc
(use-package erc
  :config
  (setq erc-timestamp-format "[%I:%M %p]"))

;; command log
(use-package command-log-mode
  :commands global-command-log-mode
  :elpaca t
  :init
  (defun meain/command-log-start ()
    "Enable command-log-mode and open command-log buffer."
    (interactive)
    (global-command-log-mode)
    (clm/open-command-log-buffer)))

;; Beacon mode
(use-package beacon
  :elpaca t
  :defer 1
  :diminish
  :config
  (add-to-list 'beacon-dont-blink-major-modes 'notmuch-search-mode) ; makes the line move around horizontally
  (setq beacon-blink-when-window-scrolls t)
  (advice-add 'evil-forward-section-begin
              :after (lambda (&rest r) (beacon-blink) (recenter))
              '((name . "beacon-blink")))
  (advice-add 'evil-backward-section-begin
              :after (lambda (&rest r) (beacon-blink) (recenter))
              '((name . "beacon-blink")))
  (beacon-mode t))

(use-package hl-line
  :disabled t
  :config (global-hl-line-mode t))

;; Ligatures
(use-package ligature
  :defer 3
  :disabled t
  :elpaca (ligature :host github
                    :repo "mickeynp/ligature.el")
  :config
  (ligature-set-ligatures 't '("www"))
  (ligature-set-ligatures 'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
                                       ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                                       "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                                       "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                                       "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                                       "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                                       "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                                       "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                                       ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                                       "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                                       "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                                       "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                                       "\\\\" "://"))
  (global-ligature-mode 1))

;; Focus mode
(use-package focus :elpaca t :commands focus-mode)
;; Writing mode
(use-package writeroom-mode
  :elpaca t
  :commands writeroom-mode
  :config
  (setq writeroom-global-effects (remove 'writeroom-set-fullscreen writeroom-global-effects)))
;; Naive linter for English prose
(use-package writegood-mode
  :elpaca t
  :defer t
  :commands (writegood-mode))

(use-package emacs
  :after (writeroom-mode evil-leader)
  :commands (meain/toggle-writing-mode)
  :config
  ;; TODO: convert writing-mode to minor mode
  (defvar meain/writing-mode-enabled -1 "State to store if `writing-mode' is enabled.")
  (defun meain/toggle-writing-mode ()
    "Toggle `writing-mode'."
    (interactive)
    (toggle-truncate-lines meain/writing-mode-enabled)
    (setq meain/writing-mode-enabled (if (eq meain/writing-mode-enabled t) -1 t))
    (writeroom-mode meain/writing-mode-enabled)
    (focus-mode meain/writing-mode-enabled)
    (writegood-mode meain/writing-mode-enabled)
    (flyspell-mode meain/writing-mode-enabled))
  :init
  (evil-leader/set-key "b W" 'meain/toggle-writing-mode))

;; tramp dired
(use-package tramp
  :commands (meain/tramp-open)
  :config
  (put 'temporary-file-directory 'standard-value (list temporary-file-directory))
  (setq remote-file-name-inhibit-cache nil)
  (setq tramp-default-method "ssh")
  (setq vc-ignore-dir-regexp (format "%s\\|%s" vc-ignore-dir-regexp tramp-file-name-regexp))
  (setq tramp-verbose 3)
  (defun meain/tramp-open ()
    "Open dired in a server by selecting a host via autocomplete."
    (interactive)
    (dired (concatenate 'string "/ssh:" (meain/ssh-host-picker) ":"))))

;; tramp-term
(use-package tramp-term
  :after tramp
  :elpaca t
  :commands (tramp-term meain/tramp-shell)
  :config
  (defun meain/tramp-shell ()
    "SSH into a server by selecting a host via autocomplete."
    (interactive)
    (tramp-term (list (meain/ssh-host-picker)))))

;; timing stuff
(use-package activity-watch-mode
  :elpaca t
  :defer 1
  :diminish
  :config (global-activity-watch-mode))

;; Control bluetooth devices
(use-package bluetooth
  :elpaca t
  :commands (bluetooth-list-devices))

;; Markdown preview
(defun meain/kill-markdown-preview ()
  "Preview markdown.  Using pandoc under the hood."
  (interactive)
  (let ((kill-buffer-query-functions nil))
    (if (get-buffer "*markdown-preview*")
        (progn
          (message "Killing old markdown preview server...")
          (kill-buffer "*markdown-preview*")))))
(defun meain/markdown-preview ()
  "Preview markdown.  Using pandoc under the hood."
  ;; TODO: handle local embedded images
  (interactive)
  (meain/kill-markdown-preview)
  (start-process "*markdown-preview*" "*markdown-preview*"
                 ",markdown-preview" buffer-file-name))

;; Restclient
(use-package restclient
  :elpaca t
  :defer t
  :mode ("\\.rest\\'". restclient-mode)
  :config (add-hook 'restclient-mode-hook (lambda ()
                                            (setq imenu-generic-expression '((nil "^#+\s+.+" 0))))))

;; Restclient jq integration
(use-package restclient-jq
  :elpaca t
  :after restclient
  :defer
  :init
  (add-hook 'restclient-mode-hook (lambda () (require 'restclient-jq))))

;; Link opening
(use-package ace-link
  :elpaca t
  :commands ace-link
  :init (global-set-key (kbd "M-f l") 'ace-link))

;; Docker
(use-package docker
  :elpaca t
  :defer t
  :commands (docker))

;; Kubernetes
(use-package kubernetes
  :elpaca t
  :disabled t
  :defer t
  :commands (meain/kube)
  :config
  (defun meain/kube ()
    "Hacky function to load `kubernetes-evil' as it was not loading otherwise."
    (interactive)
    (use-package kubernetes-evil :elpaca t)
    (kubernetes-overview)))

;; Window layout changer
(use-package rotate
  :elpaca t
  :after evil
  :commands (rotate-layout rotate-window)
  :init
  (define-key evil-normal-state-map (kbd "M-f <SPC>") 'rotate-layout))

;; Remember
(use-package remember
  :commands remember
  :config
  (setq remember-data-file "~/.config/emacs/remember-notes"
        remember-notes-initial-major-mode 'org-mode
        remember-notes-auto-save-visited-file-name t))

;; Automatically install treesit grammars
(use-package treesit-auto
  :elpaca t
  :config
  (global-treesit-auto-mode))

;; Tree sitter
(use-package tree-sitter
  :defer 1
  :disabled t
  :elpaca t
  :config
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)
  ;; debugging stuff
  (setq tree-sitter-debug-jump-buttons t)
  (setq tree-sitter-debug-highlight-jump-region nil)
  ;; Override the tree sitter debug button does
  (defun tree-sitter-debug--button-node-lookup (button)
    "The function to call when a `tree-sitter-debug' BUTTON is clicked."
    (unless tree-sitter-debug--source-code-buffer
      (error "No source code buffer set"))
    (unless (buffer-live-p tree-sitter-debug--source-code-buffer)
      (user-error "Source code buffer has been killed"))
    (unless button
      (user-error "This function must be called on a button"))
    (with-current-buffer tree-sitter-debug--source-code-buffer
      (pulse-momentary-highlight-region (car (tsc-node-byte-range (button-get button 'points-to)))
                                        (cdr (tsc-node-byte-range (button-get button 'points-to)))
                                        'mode-line)))
  (defvar meain/tree-sitter-config-nesting--queries '((json-mode . "(object (pair (string (string_content) @key) (_)) @item)")
                                                      (yaml-mode . "(block_mapping_pair (flow_node) @key (_)) @item")
                                                      (nix-mode . "(binding (attrpath (identifier) @key)) @item")))
  (defun meain/tree-sitter-config-nesting ()
    (when-let* ((query-s (cdr (assq major-mode meain/tree-sitter-config-nesting--queries)))
                (query (tsc-make-query tree-sitter-language query-s))
                (root-node (tsc-root-node tree-sitter-tree))
                (matches (tsc-query-matches query root-node #'tsc--buffer-substring-no-properties)))
      (string-join
       (remove-if #'null
                  (seq-map (lambda (x)
                             (let* ((item (seq-elt (cdr x) 0))
                                    (key (seq-elt (cdr x) 1))
                                    (pos (tsc-node-byte-range (cdr item))))
                               (when (> (byte-to-position (cdr pos))
                                        (point)
                                        (byte-to-position (car pos)))
                                 (format "%s" (tsc-node-text (cdr key))))))
                           matches))
       ".")))
  (defun meain/get-config-nesting-paths ()
    "Get out all the nested paths in a config file."
    (when-let* ((query-s (cdr (assq major-mode meain/tree-sitter-config-nesting--queries)))
                (root-node (tsc-root-node tree-sitter-tree))
                (query (tsc-make-query tree-sitter-language query-s))
                (matches (tsc-query-matches query root-node #'tsc--buffer-substring-no-properties))
                (item-ranges (seq-map (lambda (x)
                                        (let ((item (seq-elt (cdr x) 0))
                                              (key (seq-elt (cdr x) 1)))
                                          (list (tsc-node-text (cdr key))
                                                (tsc-node-range (cdr key))
                                                (tsc-node-range (cdr item)))))
                                      matches))
                (parent-nodes '(("#" 0))))
      (mapcar (lambda (x)
                (let* ((current-end (seq-elt (cadr (cdr x)) 1))
                       (parent-end (cadar parent-nodes))
                       (current-key (car x)))
                  (progn
                    (if (> current-end parent-end)
                        (setq parent-nodes
                              (-filter (lambda (y) (< current-end (cadr y)))
                                       parent-nodes)))
                    (setq parent-nodes (cons (list current-key current-end) parent-nodes))
                    (list (reverse (mapcar #'car parent-nodes))
                          (seq-elt (cadr x) 0)))))
              item-ranges)))
  (defun meain/imenu-config-nesting-path ()
    "Return config-nesting paths for use in imenu"
    (mapcar (lambda (x)
              (cons (string-join (car x) ".") (cadr x)))
            (meain/get-config-nesting-paths)))
  (setq meain/tree-sitter-class-like '((rust-mode . (impl_item))
                                       (python-mode . (class_definition))))
  (setq meain/tree-sitter-function-like '((rust-mode . (function_item))
                                          (go-mode . (function_declaration method_declaration))
                                          (sh-mode . (function_definition))
                                          (python-mode . (function_definition))))
  (defun meain/tree-sitter-thing-name (kind)
    "Get name of tree-sitter KIND thing."
    (when-let (tree-sitter-mode
               (node-types (pcase kind
                             ('class-like meain/tree-sitter-class-like)
                             ('function-like meain/tree-sitter-function-like)))
               (node-at-point (cl-some #'tree-sitter-node-at-point
                                       (alist-get major-mode node-types)))
               (node-name (tsc-get-child-by-field node-at-point :name)))
      (tsc-node-text node-name)))
  ;; Connect to which-function for magit-log-trace-definition
  (setq which-func-functions
        (list
         (lambda () (meain/tree-sitter-thing-name 'function-like))
         (lambda () (meain/tree-sitter-thing-name 'class-like)))))

(use-package tree-sitter-langs
  :load-path "/home/meain/dev/src/tree-sitter-langs"
  :defer 1
  :disabled t
  :after tree-sitter
  :config
  (push '(markdown-mode . markdown) tree-sitter-major-mode-language-alist)
  (push '(gfm-mode . markdown) tree-sitter-major-mode-language-alist)
  (setq tree-sitter-load-path '("/home/meain/dev/src/tree-sitter-langs/bin/"))

  ;; Don't highlight constructors in rust with hima
  (add-function :before-until tree-sitter-hl-face-mapping-function
                (lambda (capture-name)
                  (pcase capture-name
                    ("rust.constructor" 'tree-sitter-hl-face:function.call))))
  (add-hook 'rust-mode-hook
            (lambda ()
              (tree-sitter-hl-add-patterns nil
                [((identifier) @rust.constructor
                  (.match? @rust.constructor "^[A-Z]"))]))))

;; Some custom text objects based on treesitter
(use-package evil-textobj-tree-sitter
  :defer 1
  :load-path "/home/meain/dev/src/evil-textobj-tree-sitter/"
  :after (evil)
  :config
  (define-key evil-outer-text-objects-map "m" (evil-textobj-tree-sitter-get-textobj "import"
                                                '((python-mode . [(import_statement) @import])
                                                  (go-mode . [(import_spec) @import])
                                                  (rust-mode . [(use_declaration) @import]))))
  (define-key evil-outer-text-objects-map "f" (cons "evil-outer-function" (evil-textobj-tree-sitter-get-textobj "function.outer")))
  (define-key evil-inner-text-objects-map "f" (cons "evil-inner-function" (evil-textobj-tree-sitter-get-textobj "function.inner")))
  (define-key evil-outer-text-objects-map "c" (cons "evil-outer-class" (evil-textobj-tree-sitter-get-textobj "class.outer")))
  (define-key evil-inner-text-objects-map "c" (cons "evil-inner-class" (evil-textobj-tree-sitter-get-textobj "class.inner")))
  (define-key evil-outer-text-objects-map "n" (cons "evil-outer-comment" (evil-textobj-tree-sitter-get-textobj "comment.outer")))
  (define-key evil-inner-text-objects-map "n" (cons "evil-outer-comment" (evil-textobj-tree-sitter-get-textobj "comment.outer")))
  (define-key evil-outer-text-objects-map "v" (cons "evil-outer-conditional-loop" (evil-textobj-tree-sitter-get-textobj ("conditional.outer" "loop.outer"))))
  (define-key evil-inner-text-objects-map "v" (cons "evil-inner-conditional-loop" (evil-textobj-tree-sitter-get-textobj ("conditional.inner" "loop.inner"))))
  (define-key evil-inner-text-objects-map "a" (cons "evil-inner-parameter" (evil-textobj-tree-sitter-get-textobj "parameter.inner")))
  (define-key evil-outer-text-objects-map "a" (cons "evil-outer-parameter" (evil-textobj-tree-sitter-get-textobj "parameter.outer")))

  (define-key evil-normal-state-map (kbd "]a") (cons "goto-parameter-start" (meain/ilambda evil-textobj-tree-sitter-goto-textobj "parameter.inner")))
  (define-key evil-normal-state-map (kbd "[a") (cons "goto-parameter-start" (meain/ilambda evil-textobj-tree-sitter-goto-textobj "parameter.inner" t)))
  (define-key evil-normal-state-map (kbd "]A") (cons "goto-parameter-end" (meain/ilambda evil-textobj-tree-sitter-goto-textobj "parameter.inner" nil t)))
  (define-key evil-normal-state-map (kbd "[A") (cons "goto-parameter-end" (meain/ilambda evil-textobj-tree-sitter-goto-textobj "parameter.inner" t t)))
  (define-key evil-normal-state-map (kbd "]v") (cons "goto-conditional-start" (meain/ilambda evil-textobj-tree-sitter-goto-textobj ("conditional.outer" "loop.outer"))))
  (define-key evil-normal-state-map (kbd "[v") (cons "goto-conditional-start" (meain/ilambda evil-textobj-tree-sitter-goto-textobj ("conditional.outer" "loop.outer") t)))
  (define-key evil-normal-state-map (kbd "]V") (cons "goto-conditional-end" (meain/ilambda evil-textobj-tree-sitter-goto-textobj ("conditional.outer" "loop.outer") nil t)))
  (define-key evil-normal-state-map (kbd "[V") (cons "goto-conditional-end" (meain/ilambda evil-textobj-tree-sitter-goto-textobj ("conditional.outer" "loop.outer") t t)))
  (define-key evil-normal-state-map (kbd "]c") (cons "goto-class-start" (meain/ilambda evil-textobj-tree-sitter-goto-textobj "class.outer")))
  (define-key evil-normal-state-map (kbd "[c") (cons "goto-class-start" (meain/ilambda evil-textobj-tree-sitter-goto-textobj "class.outer" t)))
  (define-key evil-normal-state-map (kbd "]C") (cons "goto-class-end" (meain/ilambda evil-textobj-tree-sitter-goto-textobj "class.outer" nil t)))
  (define-key evil-normal-state-map (kbd "[C") (cons "goto-class-end" (meain/ilambda evil-textobj-tree-sitter-goto-textobj "class.outer" t t)))
  (define-key evil-normal-state-map (kbd "]n") (cons "goto-comment-start" (meain/ilambda evil-textobj-tree-sitter-goto-textobj "comment.outer")))
  (define-key evil-normal-state-map (kbd "[n") (cons "goto-comment-start" (meain/ilambda evil-textobj-tree-sitter-goto-textobj "comment.outer" t)))
  (define-key evil-normal-state-map (kbd "]N") (cons "goto-comment-end" (meain/ilambda evil-textobj-tree-sitter-goto-textobj "comment.outer" nil t)))
  (define-key evil-normal-state-map (kbd "[N") (cons "goto-comment-end" (meain/ilambda evil-textobj-tree-sitter-goto-textobj "comment.outer" t t)))
  (define-key evil-normal-state-map (kbd "]f") (cons "goto-function-start" (lambda () (interactive) (progn (evil-textobj-tree-sitter-goto-textobj "function.outer") (reposition-window)))))
  (define-key evil-normal-state-map (kbd "[f") (cons "goto-function-start" (lambda () (interactive) (progn (evil-textobj-tree-sitter-goto-textobj "function.outer" t) (reposition-window)))))
  (define-key evil-normal-state-map (kbd "]F") (cons "goto-function-end" (lambda () (interactive) (progn (evil-textobj-tree-sitter-goto-textobj "function.outer" nil t) (reposition-window)))))
  (define-key evil-normal-state-map (kbd "[F") (cons "goto-function-end" (lambda () (interactive) (progn (evil-textobj-tree-sitter-goto-textobj "function.outer" t t) (reposition-window))))))

;; Show context using tree-sitter
(use-package posframe-plus
  :elpaca (:host github :type git :repo "zbelial/posframe-plus" ))
(use-package treesitter-context
  :after (tree-sitter posframe-plus)
  :elpaca (:type git :host github :repo "zbelial/treesitter-context.el")
  :commands (treesitter-context-toggle-show)
  :config
  (setq treesitter-context-idle-time 0.5)
  (setq treesitter-context-show-context-always t)
  (setq treesitter-context-frame-autohide-timeout 15)
  (setq meain/treesitter-context-shown nil)

  (require 'treesitter-context-utils) ;; for `treesitter-context-toggle-show'
  :init
  (global-set-key (kbd "M-r") #'treesitter-context-toggle-show))

(use-package combobulate
  :commands (combobulate)
  :elpaca (:repo "mickeynp/combobulate" :host github))

(use-package ts-fold
  :defer t
  :after (tree-sitter evil-leader)
  :commands (ts-fold-mode)
  :elpaca (ts-fold :host github
                   :repo "jcs090218/ts-fold")
  :config
  (defun meain/toggle-fold ()
    (interactive)
    (if (equal tree-sitter-mode nil)
        (call-interactively 'evil-toggle-fold)
      (call-interactively 'ts-fold-toggle)))
  :init
  (add-hook 'tree-sitter-after-on-hook
            (lambda ()
              (origami-mode -1)
              (ts-fold-mode 1)
              (define-key evil-normal-state-map (kbd "<SPC> TAB") 'meain/toggle-fold)
              (evil-leader/set-key "o" 'meain/toggle-fold))))

;; Show scope info of block
;; (remove-overlays (point-min) (point-max))
(use-package scopeline
  :commands (scopeline-mode)
  :load-path "/home/meain/dev/src/scopeline.el"
  :config (setq scopeline-overlay-prefix " ~")
  :init (add-hook 'prog-mode-hook #'scopeline-mode))

;; Show definition beyond top of buffer in header
;; Similar: breadcrumb
(use-package topsy
  :defer t
  :elpaca t
  :init
  (add-hook 'find-file-hook #'topsy-mode))

;; Does not use imenu populated by eglot
;; (use-package breadcrumb
;;   :elpaca (:repo "joaotavora/breadcrumb" :host github))

;; Quick lookup in a dictionary
(use-package dictionary
  :elpaca t
  :commands (dictionary-search)
  :init
  (global-set-key (kbd "C-c d") #'dictionary-search)
  :config (setq dictionary-server "dict.org"))

;; Highlight enclosing parenthesis
(use-package highlight-parentheses
  :defer t
  :elpaca t
  :init (add-hook 'prog-mode-hook #'highlight-parentheses-mode)
  :config (setq highlight-parentheses-colors '("coral1")))

;; Auto recompile on save (useful for running tests)
(use-package recompile-on-save
  :elpaca t
  :commands (recompile-on-save-mode))

;; RFC reader
(use-package rfc-mode
  :elpaca t
  :commands (rfc-mode-browse rfc-mode-read)
  :config
  (setq rfc-mode-directory (expand-file-name "~/.local/share/rfc/"))
  (add-hook 'rfc-mode-hook 'writeroom-mode))

(use-package ledger-mode
  :elpaca t
  :defer t
  :commands (meain/ledger-add-entry)
  :mode "\\.ledger\\'"
  :config
  (defun meain/ledger-add-entry (&optional no-switch)
    (interactive "P")
    (if (not no-switch)
        (find-file "~/.local/share/ledger/master.ledger"))
    (let* ((accounts (mapcar 'list
                             (ledger-accounts-list)))
           (title (concat (format-time-string "%Y/%m/%d "
                                              (org-read-date nil 'to-time nil "Date:  "))
                          (completing-read "Description: "
                                           (split-string
                                            (shell-command-to-string
                                             "ledger -f ~/.local/share/ledger/master.ledger payees")
                                            "\n"))))
           (in (completing-read "What did you pay for? " accounts))
           (amount (concat (read-string "How much did you pay? ") " INR"))
           (out (completing-read "Where did the money come from? " accounts)))
      (goto-char (point-max))
      (newline)
      (insert title)
      (newline)
      (indent-to 4)
      (insert in "  " amount)
      (newline)
      (indent-to 4)
      (insert out)
      (ledger-mode-clean-buffer))))

(use-package scroll-on-drag
  :elpaca t
  :defer 3
  :config
  (setq scroll-on-drag-motion-scale 0.1)
  (global-set-key [down-mouse-2]
                  (lambda ()
                    (interactive)
                    (unless (scroll-on-drag)
                      (mouse-yank-primary t)))))

(use-package 0x0
  :elpaca t
  :defer t
  :disabled t
  :after evil-leader
  :commands (0x0-dwim 0x0-popup 0x0-upload-file 0x0-upload-text)
  :init (evil-leader/set-key "a 0" '0x0-dwim))

(use-package redacted
  :elpaca t
  :commands (redacted-mode)
  :config (add-hook 'redacted-mode-hook (lambda () (read-only-mode (if redacted-mode 1 -1)))))

(use-package avy
  :elpaca t
  :defer 3
  :after evil-leader
  :config
  (setq avy-timeout-seconds 0.3)
  (evil-leader/set-key "h" 'avy-goto-char-timer))

(use-package harpoon
  :elpaca t
  :after evil-leader
  :commands (harpoon-toggle-file
             harpoon-toggle-quick-menu
             harpoon-clear harpoon-add-file
             harpoon-go-to-1 harpoon-go-to-2
             harpoon-go-to-3 harpoon-go-to-4
             harpoon-go-to-5 harpoon-go-to-6
             harpoon-go-to-7 harpoon-go-to-8
             harpoon-go-to-9)
  :config
  (setq harpoon-cache-file (concat user-emacs-directory "harpoon/"))
  (setq harpoon-separate-by-branch nil)
  (set harpoon-project-package 'project)
  :init
  (evil-leader/set-key "F F" 'harpoon-toggle-file)
  (evil-leader/set-key "F r" 'harpoon-toggle-quick-menu)
  (evil-leader/set-key "F c" 'harpoon-clear)
  (evil-leader/set-key "F f" 'harpoon-add-file)
  (evil-leader/set-key "F j" 'harpoon-go-to-1)
  (evil-leader/set-key "F k" 'harpoon-go-to-2)
  (evil-leader/set-key "F l" 'harpoon-go-to-3)
  (evil-leader/set-key "F ;" 'harpoon-go-to-4)
  (evil-leader/set-key "F h j" 'harpoon-go-to-5)
  (evil-leader/set-key "F h k" 'harpoon-go-to-6)
  (evil-leader/set-key "F h l" 'harpoon-go-to-7)
  (evil-leader/set-key "F h ;" 'harpoon-go-to-8)
  (evil-leader/set-key "F h f" 'harpoon-go-to-9))

;; Kinda like screensavers
(use-package zone
  :defer t
  :config
  (setq zone-programs [zone-pgm-rotate-LR-lockstep])
  (zone-when-idle (* 5 60)))

;; Mermaid mode
(use-package mermaid-mode :defer t :elpaca t)

;; Edit any textfield in Emacs
(use-package emacs-everywhere
  :defer t
  :elpaca t
  :config
  (remove-hook 'emacs-everywhere-init-hooks #'emacs-everywhere-major-mode-org-or-markdown)
  (remove-hook 'emacs-everywhere-init-hooks #'emacs-everywhere-apply-major-mode)
  (add-hook 'emacs-everywhere-init-hooks #'gfm-mode))

;; Fontify face (useful to debug themes)
(use-package fontify-face :elpaca t :defer t)

;; Keycast mode for demos
(use-package keycast
  :elpaca t
  :defer t
  :commands (keycast-mode keycast-background-mode keycast-log-mode keycast-tab-bar-mode)
  :config
  (add-hook 'keycast-mode-hook (lambda () (setq header-line-format nil)))
  (defvar keycast-background-mode)
  ;; TODO: fix not clearing the last thing on exit
  (define-minor-mode keycast-background-mode
    "Activate keycast mode in the background.  Enables variable to be used in header-line."
    :global t
    (cond
     (keycast-background-mode
      (add-hook 'post-command-hook #'keycast--update t)
      (add-hook 'minibuffer-exit-hook #'keycast--minibuffer-exit t))
     ((not (keycast--mode-active-p))
      (remove-hook 'post-command-hook #'keycast--update)
      (remove-hook 'minibuffer-exit-hook #'keycast--minibuffer-exit)))))

;;; [CUSTOM FUNCTIONS] ==============================================

;; Automatic chmod +x when you save a file that starts with a #! shebang:
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; Font size changes
(global-set-key (kbd "s-=") 'text-scale-increase)
(global-set-key (kbd "s--") 'text-scale-decrease)
(global-set-key (kbd "s-_") (meain/inlambda text-scale-set 0)) ; s-0 is used by wm

;; host picker
(defun meain/ssh-host-picker ()
  "Interactively pick ssh host."
  (with-temp-buffer
    (insert-file-contents "~/.ssh/config")
    (format "%s"
            (completing-read
             "Choose host: "
             (mapcar (lambda (x) (replace-regexp-in-string (regexp-quote "Host ") "" x))
                     (remove-if-not #'(lambda (x) (s-starts-with-p "Host" x))
                                    (split-string (buffer-string) "\n")))))))

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

;; Add keybindings to access important files.
(use-package emacs
  :after (evil-leader)
  :init
  (defun meain/qa--get-entries (filename)
    "Helper function to parse qa files.  `FILENAME' is the name of the file to parse."
    (let* ((contents (with-temp-buffer
                       (insert-file-contents filename)
                       (buffer-string)))
           (qa-entries (mapcar (lambda (x)
                                 (string-split x " "))
                               (string-split contents "\n"))))
      qa-entries))

  (mapcar (lambda (e)
            (when-let ((key (car e))
                       (name (cadr e))
                       (file (caddr e)))
              (evil-leader/set-key (concat "e " key)
                (cons name (lambda (&optional create)
                             (interactive "P")
                             (if (file-exists-p file)
                                 (if (file-directory-p file)
                                     (find-file
                                      (concat file "/"
                                              (completing-read
                                               "Choose file:"
                                               (directory-files file nil
                                                                directory-files-no-dot-files-regexp))))
                                   (find-file file))
                               (if create
                                   (find-file file)
                                 (message "Unable to find %s" file))))))))
          (meain/qa--get-entries "~/.config/datafiles/qa-files"))

  ;; Add keybinding to access common projects quickly.
  ;; qa-projects (quick-access-projects) file contains the list of
  ;; projects that will be added here.
  (mapcar (lambda (e)
            (when-let ((key (car e))
                       (name (cadr e))
                       (folder (caddr e)))
              (evil-leader/set-key (concat "s e " key)
                (cons name (lambda ()
                             (interactive)
                             (project-switch-project folder))))))
          (meain/qa--get-entries "~/.config/datafiles/qa-projects")))

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
(global-set-key (kbd "M-f f") 'meain/monacle-mode)

;; Revert buffer quickly (fix for dealing with eglot loosing it)
(global-set-key (kbd "M-f r") (lambda ()
                                (interactive)
                                (save-buffer)
                                (revert-buffer-quick)))

;; Quick open scratch buffers
(use-package emacs
  :after evil-leader
  :commands (meain/scratchy)
  :config
  (defun meain/scratchy (beg end)
    "Open scratch buffer in a specific mode."
    (interactive "r")
    (let* ((scratch-major-mode
            (completing-read
             "Choose mode: "
             (cons 'artist-mode
                   (cons 'mermaid-mode
                         (append
                          (cl-loop for sym the symbols of obarray
                                   when (and (functionp sym)
                                             (provided-mode-derived-p sym 'text-mode))
                                   collect sym)
                          (cl-loop for sym the symbols of obarray
                                   when (and (functionp sym)
                                             (provided-mode-derived-p sym 'prog-mode))
                                   collect sym))))
             nil nil nil nil "text-mode"))
           (scratch-file-name (concatenate 'string
                                           "~/.local/share/scratch/"
                                           (format "%s" scratch-major-mode) "-"
                                           (substring (uuid-string) 0 4)))
           (text (if (use-region-p) (buffer-substring beg end))))
      (find-file scratch-file-name)
      (if text (insert text))
      (funcall (intern scratch-major-mode))
      (if (eq (intern scratch-major-mode) 'artist-mode)
          (evil-local-mode -1))))
  :init
  (evil-leader/set-key "c"
    (meain/with-alternate (meain/create-or-switch-to-scratch)
                          (call-interactively 'meain/scratchy))))

;; vime functionality within emacs
(use-package uuid :elpaca t :commands uuid-string)
(use-package emacs
  :commands (meain/vime)
  :after (marginalia)
  :config
  (defun meain/marginalia-annotate-vime (cand)
    (when-let (root "~/.local/share/vime/")
      (marginalia-annotate-file (expand-file-name (car (split-string cand " ")) root))))
  (add-to-list 'marginalia-annotator-registry '(vime meain/marginalia-annotate-vime builtin none))
  (add-to-list 'marginalia-prompt-categories '("Choose vime:" . vime))
  (defun meain/vime-name-append (filename directory)
    "Util function used to parse :name block for vime entries.  FILENAME is the name of the vime file."
    (with-temp-buffer
      (insert-file-contents (concatenate 'string directory "/" filename))
      (concatenate 'string
                   filename
                   (if (s-starts-with-p ":name" (car (split-string (buffer-string) "\n")))
                       (replace-regexp-in-string
                        (regexp-quote ":name")
                        ""
                        (car (split-string (buffer-string) "\n")))
                     ""))))
  (defun meain/vime--get-new-filename (directory)
    "Get a nonexistent file in `DIRECTORY'.
After a while you start repeating filenames as we have just 4 so checking for exists."
    (let ((filename (concat directory "/_"
                            (substring (uuid-string) 0 4))))
      (if (file-exists-p filename)
          (meain/vime--get-new-filename)
        filename)))
  (defun meain/vime (name directory &optional createnew)
    "Load a random file inside vime dir.  Used as a temp notes dir.
Pass in `LISTITEMS to decide if you wanna create a new item or search for existing items."
    (interactive "P")
    (if (not createnew)
        (let ((vertico-sort-function nil))
          (find-file
           (concat
            directory "/"
            (car (split-string
                  (completing-read
                   (concat "Choose " name ": ")
                   (mapcar (lambda (x) (meain/vime-name-append (car x) directory))
                           (sort (remove-if-not #'(lambda (x)
                                                    (eq (nth 1 x) nil))
                                                (directory-files-and-attributes directory))
                                 #'(lambda (x y) (time-less-p (nth 6 y) (nth 6 x))))))))))
          (mtodo-mode))
      (progn
        (find-file (meain/vime--get-new-filename directory))
        (insert ":name ")
        (mtodo-mode)
        (evil-insert 1))))
  :init
  (evil-leader/set-key "v" '(lambda (createnew)
                              (interactive "P")
                              (meain/vime "vime" "~/.local/share/vime" createnew))))

(defun meain/drop-current-file ()
  "Use ,drop to drop files on to personal server."
  (interactive)
  (let ((drop-url (car (split-string (shell-command-to-string (concat "NO_COPY=1 ,drop " (buffer-file-name))) "\n"))))
    (message drop-url)
    (meain/copy-to-clipboard drop-url)))

;; Notes
(use-package emacs
  :commands (meain/open-note)
  :after (marginalia)
  :config
  ;; Open note
  (defun meain/nested-list-dir (directory)
    "List items two level deep in DIRECTORY."
    (apply 'concatenate
           'list
           (mapcar (lambda (x)
                     (mapcar (lambda (y) (concatenate 'string (car x) "/" (car y)))
                             (remove-if #'(lambda (x) (or (eq (nth 1 x) t)
                                                          (equal (substring (nth 0 x) 0 1) ".")))
                                        (directory-files-and-attributes
                                         (concatenate 'string directory "/" (car x))))))
                   (remove-if #'(lambda (x)
                                  (or (eq (nth 1 x) nil)
                                      (equal (substring (nth 0 x) 0 1) ".")
                                      (equal (nth 0 x) "archive")
                                      (equal (nth 0 x) "temp")))
                              (directory-files-and-attributes directory)))))
  (defun meain/marginalia-annotate-note (cand)
    (when-let (root "~/.local/share/notes/")
      (marginalia-annotate-file (expand-file-name cand root))))
  (add-to-list 'marginalia-annotator-registry '(note meain/marginalia-annotate-note builtin none))
  (add-to-list 'marginalia-prompt-categories '("Choose note:" . note))
  (defun meain/open-note ()
    "Quick open a note from `.notes` directory."
    (interactive)
    (find-file
     (concatenate 'string
                  "~/.local/share/notes/"
                  (completing-read "Choose note: "
                                   (meain/nested-list-dir "~/.local/share/notes")))))
  :init (evil-leader/set-key "a N" 'meain/open-note))

;; devdocs
(use-package devdocs
  :elpaca t
  :commands (devdocs-search devdocs-lookup devdocs-install))

;; cheat.sh
(use-package cheat-sh
  :elpaca t
  :commands (cheat-sh cheat-sh-maybe-region)
  :init
  (evil-leader/set-key "a d"
    (meain/with-alternate (call-interactively 'devdocs-lookup)
                          (call-interactively 'cheat-sh-search-topic))))

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
  (let ((qed-buffer-name (concatenate 'string "qed-" (substring (uuid-string) 0 4))))
    (generate-new-buffer qed-buffer-name)
    (switch-to-buffer qed-buffer-name)
    (evil-paste-after 1)
    (gfm-mode)))

;; vim-printer remake in elisp
(use-package emacs
  :after evil
  :commands (meain/quick-print)
  :config
  (defun meain/quick-print (beg end)
    "Quickly print the variable your cursor is under.  `BEG' and `END' is used in visual mode."
    (interactive "r")
    (let* ((thing-to-print (if (use-region-p)
                               (buffer-substring beg end)
                             (symbol-name (symbol-at-point))))
           (escaped-thing-to-print (string-replace "\"" "\\\"" thing-to-print)))
      (if current-prefix-arg
          (evil-open-above 1)
        (evil-open-below 1))
      (let* (
             (filename (car (reverse (string-split (buffer-file-name) "/"))))
             (prefix (format "%s:%s" filename (line-number-at-pos))))
        (insert (pcase major-mode
                  ('emacs-lisp-mode (format "(message \"%s %s: %%s\" %s)" prefix escaped-thing-to-print thing-to-print thing-to-print))
                  ('lisp-interaction-mode (format "(message \"%s %s: %%s\" %s)" prefix escaped-thing-to-print thing-to-print thing-to-print))
                  ('rust-mode (format "println!(\"%s %s: {:?}\", %s);" prefix escaped-thing-to-print thing-to-print))
                  ('rust-ts-mode (format "println!(\"%s %s: {:?}\", %s);" prefix escaped-thing-to-print thing-to-print))
                  ('go-mode (format "fmt.Println(\"%s %s:\", %s)" prefix escaped-thing-to-print thing-to-print))
                  ('go-ts-mode (format "fmt.Println(\"%s %s:\", %s)" prefix escaped-thing-to-print thing-to-print))
                  ('lua-mode (format "print(\"%s %s:\", %s)" prefix escaped-thing-to-print thing-to-print))
                  ('js-mode (format "console.log(\"%s %s:\", %s)" prefix escaped-thing-to-print thing-to-print))
                  ('typescript-ts-mode (format "console.log(\"%s %s:\", %s)" prefix escaped-thing-to-print thing-to-print))
                  ('web-mode (format "console.log(\"%s %s:\", %s)" prefix escaped-thing-to-print thing-to-print))
                  ('shell-script-mode (format "echo \"%s %s:\" %s" prefix escaped-thing-to-print thing-to-print))
                  ('python-ts-mode (format "print(\"%s %s:\", %s)" prefix escaped-thing-to-print thing-to-print))
                  ('python-mode (format "print(\"%s %s:\", %s)" prefix escaped-thing-to-print thing-to-print))))))
    (evil-force-normal-state))
  :init
  (define-key evil-normal-state-map (kbd "g p") 'meain/quick-print))

;; Journal entry
(use-package emacs
  :after evil-leader
  :init
  (add-hook 'find-file-hook
            (lambda ()
              (if (string-prefix-p (expand-file-name "~/.local/share/journal")
                                   default-directory)
                  (progn
                    (copilot-mode -1) ; noooope
                    (auto-fill-mode)))))
  (evil-leader/set-key "a J"
    (lambda ()
      "Start writing journal entry.  `journal' invokes emacsclient and gives control back over to Emacs."
      (interactive)
      (start-process-shell-command "journal" "*journal*"
                                   "EDITOR='emacsclient' ,journal"))))


;; Narrow region
(use-package fancy-narrow
  :elpaca t
  :after evil
  :commands (fancy-narrow-to-region fancy-widen evil-fancy-narrow)
  :config
  ;; TODO: remove extra args
  (evil-define-operator evil-fancy-narrow (beg end type register _handler)
    "Narrow to region"
    :move-point nil
    :repeat nil
    (interactive "<R><x><y>")
    (fancy-narrow-to-region beg end))
  (defun meain/narrow-region-dwim (&optional basic)
    "Narrow or widen the region (dwim)."
    (interactive)
    (if (eq evil-state 'visual)
        (if basic
            (call-interactively 'narrow-to-region)
          (call-interactively 'fancy-narrow-to-region))
      (if basic
          (call-interactively 'widen)
        (call-interactively 'fancy-widen))))
  :init
  (define-key evil-normal-state-map (kbd "X") 'evil-fancy-narrow)
  (global-set-key (kbd "M-N") 'meain/narrow-region-dwim))

;; Copilot, I guess
(use-package copilot
  :defer 3
  :elpaca (:host github
                 :repo "zerolfx/copilot.el"
                 :files ("dist" "*.el"))
  :config
  (global-copilot-mode t)
  (setq copilot-idle-delay 0)
  (setq copilot-max-char -1)

  ;; Suppress indentation warning from copilot
  ;; https://github.com/zerolfx/copilot.el/pull/212#issuecomment-1862487382
  (add-to-list 'warning-suppress-types '(copilot copilot-no-mode-indent))

  (define-key copilot-mode-map (kbd "M-f M-f") #'copilot-complete)
  (define-key copilot-mode-map (kbd "M-f M-j") #'copilot-next-completion)
  (define-key copilot-mode-map (kbd "M-f M-k") #'copilot-previous-completion)
  (define-key copilot-mode-map (kbd "M-f M-l") #'copilot-accept-completion))

;; Better GPT-3 interaction
(use-package c3po
  :elpaca (:host github :repo "d1egoaz/c3po.el")
  :commands (c3po-assistant-new-chat
             c3po-assistant-new-chat-replace-region
             c3po-grammar-checker-new-chat c3po-grammar-checker-new-chat-replace-region
             c3po-developer-new-chat c3po-developer-new-chat-replace-region
             c3po-rewriter-new-chat c3po-rewriter-new-chat-replace-region)
  :config (setq c3po-api-key openai-api-key))

;; OpenAI GPT-3 interaction
(use-package gptel
  :elpaca t
  :commands (gptel gptel-send gptel-rewrite-menu)
  :config
  (setq gptel-model "gpt-4")
  (setq gptel-api-key openai-api-key)
  :init
  (global-set-key (kbd "M-f i i") (lambda () (interactive) (gptel-send t)))
  (global-set-key (kbd "M-f i r") (lambda () (interactive) (gptel-rewrite-menu))))

;; Chatgpt shell
(use-package chatgpt-shell
  :elpaca t
  :config
  (setq chatgpt-shell-model-version "gpt-4")
  (setq chatgpt-shell-openai-key openai-api-key))

;; Buffer/Frame/Window keybinds
(use-package emacs
  :after evil-leader
  :init
  (evil-leader/set-key "b k" 'kill-buffer)
  (evil-leader/set-key "b o" 'previous-buffer)
  (evil-leader/set-key "b f" 'find-file)
  (evil-leader/set-key "b d" 'delete-frame))

;; Server edit complete
(use-package emacs
  :after evil-leader
  :init
  (evil-leader/set-key "s s" 'server-edit))

;; Next and previous buffer
(use-package emacs
  :after evil
  :init
  (define-key evil-normal-state-map (kbd "C-S-o") 'previous-buffer)
  (define-key evil-normal-state-map (kbd "C-S-i") 'next-buffer))

;; Bookmarks
(setq bookmark-save-flag 1)
(setq bookmark-set-fringe-mark nil)
(advice-add 'bookmark-jump :around #'meain/recenter-advice)
(global-set-key (kbd "M-f m") 'bookmark-jump)
(global-set-key (kbd "M-f M") 'bookmark-set)

;; Quick file rename
(defun meain/rename-current-file ()
  "Rename the current visiting file and switch buffer focus to it."
  (interactive)
  (if (null (buffer-file-name))
      (user-error "Buffer does not have a filename: %s" (current-buffer)))
  (let ((new-filename (meain/expand-filename-prompt
                       (format "Rename %s to: " (file-name-nondirectory (buffer-file-name))))))
    (if (null (file-writable-p new-filename))
        (user-error "New file not writable: %s" new-filename))
    (rename-file (buffer-file-name) new-filename 1)
    (find-alternate-file new-filename)
    (message "Renamed to and now visiting: %s" (abbreviate-file-name new-filename))))
(defun meain/expand-filename-prompt (prompt)
  "Return expanded filename PROMPT."
  (expand-file-name (read-file-name prompt)))
(defalias 'rename 'meain/rename-current-file)

;; Delete current file
(defun meain/delete-current-file ()
  "Delete current file and close buffer."
  (interactive)
  (delete-file (buffer-file-name))
  (meain/kill-current-buffer-unless-scratch))

;; Copy filename to clipboard
(defun meain/copy-file-name-to-clipboard (&optional abs-path)
  "Copy the current filename into clipboard.  Pass `ABS-PATH' if you need absolute path."
  (interactive "P")
  (let ((file-path (if (equal major-mode 'dired-mode)
                       default-directory
                     (buffer-file-name))))
    (if file-path
        (let* ((project-path (if (and
                                  (project-current)
                                  (not (file-remote-p default-directory)))
                                 (expand-file-name (car (project-roots (project-current))))
                               ""))
               (trimmed-path (if (length> project-path 0)
                                 (string-replace project-path "" file-path)
                               file-path))
               (copy-path (if abs-path
                              file-path
                            trimmed-path)))
          (meain/copy-to-clipboard copy-path)
          (message "Copied '%s' to the clipboard" copy-path))
      (message "No file associated with buffer"))))
(defalias #'meain/copy-path-to-clipboard #'meain/copy-file-name-to-clipboard)

;; Quickly add a prog1 wrapper for logging
(defun meain/elisp-inspect-value (&optional before)
  "Quick prog1/progn dance to view variable value.
Default is after, but use BEFORE to print before."
  (interactive)
  (let* ((variable-name (read-string "Variable name: "))
         (subst-string (concat "(message \"" variable-name ": %s\" " variable-name ")")))
    (if before
        (execute-kbd-macro (kbd "vabS)aprogn"))
      (execute-kbd-macro (kbd "vabS)aprog1")))
    (evil-force-normal-state)
    (if before
        (progn
          (execute-kbd-macro (kbd "i"))
          (insert subst-string))
      (progn
        (execute-kbd-macro (kbd "vab"))
        (evil-force-normal-state)
        (execute-kbd-macro (kbd "i"))
        (insert subst-string)))
    (evil-force-normal-state)))

;; setting proper default-dir
(defun meain/set-proper-default-dir ()
  "Function to set the `default-directory' value as the project root if available."
  (interactive)
  (let ((go-base (locate-dominating-file default-directory "go.mod")))
    (if (not (file-remote-p default-directory))
        (setq default-directory (cond
                                 ((not (eq (project-current) nil))
                                  (car (project-roots (project-current))))
                                 ((not (eq go-base nil)) go-base)
                                 (t "~/"))))))
(add-hook 'find-file-hook 'meain/set-proper-default-dir)

;; Quikly add markdown links to document
(defun meain/markdown-linkify-thing (start end)
  "Function to search and add markdown links to document.  START and END for position."
  (interactive "r")
  (let* ((orig-thang (if (use-region-p)
                         (buffer-substring start end)
                       (thing-at-point 'symbol)))
         (thang (read-string "Search term: " orig-thang))
         (json-object-type 'plist)
         (json-array-type 'list)
         (lurl (car (split-string
                     (completing-read
                      (format "Choose URL (%s): " thang)
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
      (insert (format "[%s](%s)" orig-thang lurl)))))

;; Open current file in Github
(use-package emacs
  :after evil-leader
  :commands (meain/github-url)
  :config
  (defun meain/github-url (&optional use-branch)
    "Open the Github page for the current file.  Pass USE-BRANCH to use branch name instead of commit hash."
    (interactive "P")
    (save-restriction
      (widen)
      (let* ((git-url (replace-regexp-in-string
                       "\.git$"
                       ""
                       (s-replace "git@github\.com:"
                                  "https://github.com/"
                                  (car (split-string
                                        (shell-command-to-string
                                         "git config --get remote.origin.url") "\n")))))
             (git-branch (car (split-string
                               (shell-command-to-string
                                (if use-branch
                                    "git rev-parse --abbrev-ref HEAD"
                                  "git log --format='%H' -n 1"
                                  ))
                               "\n")))
             (web-url (format "%s/blob/%s/%s%s"
                              git-url
                              git-branch
                              (file-relative-name (if (equal major-mode 'dired-mode)
                                                      default-directory
                                                    buffer-file-name)
                                                  (car (project-roots (project-current))))
                              (if (equal major-mode 'dired-mode)
                                  ""
                                (format "#L%s" (line-number-at-pos))))))
        (progn
          (message "%s coped to clipboard." web-url)
          (meain/copy-to-clipboard web-url)))))
  :init
  (evil-leader/set-key "g l" 'meain/github-url))

;; Generate pdf from markdown document
(defun meain/markdown-pdf ()
  "Generate pdf from markdown document."
  (interactive)
  (message "Generating pdf of %s. Just give it a moment.." (buffer-file-name))
  (start-process-shell-command "*markdown-pdf*" "*markdown-pdf*"
                               (concatenate 'string ",markdown-to-pdf " (buffer-file-name))))

(defun meain/markdown-html ()
  "Generate pdf from markdown document."
  (interactive)
  (message "Generating markdown for %s. Just give it a moment.." (buffer-file-name))
  (start-process-shell-command "*markdown-html*" "*markdown-html*"
                               (concatenate 'string ",markdown-to-html " (buffer-file-name))))

;; Run markdown code blocks (forest.el)
(defun meain/run-markdown-code-block (&optional insert-to-buffer)
  "Run markdown code block under cursor.
Pass INSERT-TO-BUFFER to insert output to current buffer."
  (interactive "P")
  (let* ((start (nth 0 (markdown-get-enclosing-fenced-block-construct)))
         (end (nth 1 (markdown-get-enclosing-fenced-block-construct)))
         (snippet-with-markers (buffer-substring start end))
         (snippet (string-join (cdr (butlast (split-string snippet-with-markers "\n"))) "\n"))
         (snippet-runner (car (last (split-string (car (split-string snippet-with-markers "\n")) "[ `]+")))))
    (setq temp-source-file (make-temp-file "thing-to-run"))
    (pulse-momentary-highlight-region start end 'mode-line)
    (message "Code: %s" snippet)
    (message "Runner: %s" snippet-runner)
    (append-to-file snippet nil temp-source-file)
    (message "Running code...")
    (if insert-to-buffer
        (progn
          (goto-char end)
          (end-of-line)
          (newline)
          (insert "\n```output\n")
          (insert (shell-command-to-string (format "%s '%s'" snippet-runner temp-source-file)))
          (insert "```"))
      (with-current-buffer (get-buffer-create "*markdown-runner-output*")
        (erase-buffer)
        (insert (shell-command-to-string (format "%s '%s'" snippet-runner temp-source-file)))
        (switch-to-buffer (current-buffer))))
    (delete-file temp-source-file t)))

;; Fetch mail
(defun meain/fetchmail ()
  "Fetch email using mailsync."
  (interactive)
  (async-shell-command ",mail-sync"))

(defun meain/copy-to-clipboard (message)
  "Copy `MESSAGE' into clipboard."
  (with-temp-buffer
    (insert message)
    (let ((deactivate-mark t))
      (call-process-region (point-min) (point-max) "pbcopy"))))

;; Emoji picker
(defun meain/pick-emoji ()
  "Pick emoji using completions.
This contains a lot of hacks to get it working with H-q keybinding and a popup."
  (interactive)
  (let* ((filename (concat (getenv "HOME") "/.config/datafiles/emojis.txt"))
         (contents (with-temp-buffer (insert-file-contents filename) (buffer-string)))
         (emojis (split-string contents "\n"))
         (header-line-format " You are now going to pick an emoji, choose wisely"))
    (switch-to-buffer "*pick-emoji*")
    (run-with-timer 0.5 nil 'vertico-multiform-grid) ; FIXME: can't do it in a sane way
    (run-at-time "0.1 sec" nil #'vertico--exhibit)
    (meain/copy-to-clipboard (car (split-string
                                   (completing-read "Pick emoji: " emojis nil t)))))
  (if (equal "emacs-popup" (cdr (assq 'name (frame-parameters))))
      (delete-frame)))

;; Create new blog entry
(defun meain/blog-new-entry ()
  "Quick function to start a new blog entry from Emacs."
  (interactive)
  (start-process-shell-command "blog" "*blog*"
                               (concat "zsh -ic ',blog " (read-string "Blog slug: ") "'")))
;; Search from Emacs
(use-package emacs
  :after evil-leader
  :commands (meain/eww-search-ddg)
  :config
  (defun meain/eww-search-ddg (&optional open)
    "Search using eww on ddg.  Pass OPEN to open in browser instead."
    (interactive "P")
    (let* ((thing (if (use-region-p)
                      (buffer-substring start end)
                    (thing-at-point 'symbol)))
           (searchterm (replace-regexp-in-string " " "+" (read-from-minibuffer "Search query: "))))
      (if open
          (start-process-shell-command "browser-open-ddg" "*browser-open-ddg*"
                                       (concat "open 'https://duckduckgo.com/?q=" searchterm "'"))
        (eww (concat "http://lite.duckduckgo.com/lite/?q=" searchterm)))))
  :init
  (evil-leader/set-key "a s" 'meain/eww-search-ddg))

;; search from Emacs using xwidgets
(defun meain/search-xwidget ()
  "Search from Emacs using xwidgets."
  (interactive)
  (xwidget-webkit-browse-url (concat "https://duckduckgo.com/?q="
                                     (read-string "Search term: " (thing-at-point 'symbol)))))

;; Check available update for elpaca managed packages
(add-hook 'markdown-mode-hook
          (lambda ()
            (if (equal "/tmp/elpaca-available-updates.md" (buffer-file-name))
                (markdown-toggle-url-hiding 1))))
(defun meain/elpaca-available-updates ()
  "Check available update for elpaca managed packages."
  (interactive)
  (message "Fetching updates and calculating changes...")
  ;; (async-shell-command "zsh -ic 'elpaca-available-updates'"))
  (start-process-shell-command "elpaca-available-updates" "*elpaca-available-updates*"
                               "zsh -ic 'elpaca-available-updates'"))

;; popup frame thingy
(defun meain/emacs-popup-frame (thing-to-popup)
  "Popup and interactive frame thingy.  For use from hammerspoon.
Pass THING-TO-POPUP as the thing to popup."
  (interactive)
  (let ((frame (make-frame '((auto-raise . t)
                             (height . 40)
                             (internal-border-width . 20)
                             (name . "emacs-popup")
                             ;; (left . 0.33)
                             (left-fringe . 0)
                             (line-spacing . 3)
                             (menu-bar-lines . 0)
                             (right-fringe . 0)
                             (tool-bar-lines . 0)
                             ;; (top . 48)
                             (undecorated . t)
                             (unsplittable . t)
                             (vertical-scroll-bars . nil)
                             (width . 150)))))
    (select-frame frame))
  (funcall thing-to-popup))
(defun meain/emacs-mini-popup-frame (thing-to-popup)
  "Popup and interactive frame thingy.  For use from hammerspoon.
Pass THING-TO-POPUP as the thing to popup."
  (interactive)
  (let ((frame (make-frame '((auto-raise . t)
                             (height . 20)
                             (width . 100)
                             (internal-border-width . 20)
                             (name . "emacs-popup")
                             ;; (left . 0.33)
                             (left-fringe . 0)
                             (line-spacing . 3)
                             (menu-bar-lines . 0)
                             (right-fringe . 0)
                             (tool-bar-lines . 0)
                             ;; (top . 48)
                             (undecorated . t)
                             (unsplittable . t)
                             (background-color . "#E1F5FE")
                             (vertical-scroll-bars . nil)))))
    (select-frame frame))
  (funcall thing-to-popup))
(defun meain/emacs-popup-minibuffer-frame (thing-to-popup)
  "Popup and interactive frame thingy.  For use from hammerspoon.
Pass THING-TO-POPUP as the thing to popup."
  (interactive)
  (let ((frame (make-frame '((auto-raise . t)
                             (height . 20)
                             (internal-border-width . 20)
                             (name . "emacs-popup")
                             ;; (left . 0.33)
                             (left-fringe . 0)
                             (line-spacing . 3)
                             (menu-bar-lines . 0)
                             (right-fringe . 0)
                             (tool-bar-lines . 0)
                             (minibuffer . only)
                             ;; (top . 48)
                             (undecorated . t)
                             (unsplittable . t)
                             (vertical-scroll-bars . nil)
                             (background-color . "#E1F5FE")
                             (width . 150)))))
    (select-frame frame))
  (funcall thing-to-popup))

;; Patterns for replacing filenames with (builtin option: find-sibling-file)
;; Sticking with custom version as we have an option to create the file if it does not exist
;; Example for find-sibling-file:
;; (cl-pushnew '("\\([^/]+\\)\\.el\\'" "\\1-test.el") find-sibling-rules :test #'equal)
;; (cl-pushnew '("\\([^/]+\\)-test\\.el\\'" "\\1.el") find-sibling-rules :test #'equal)
(use-package emacs
  :after evil-leader
  :commands (meain/find-alternate-file)
  :config
  (defvar meain/find-alternate-file--patterns '(("thing-for-today-personal.mtodo" "thing-for-today.mtodo")
                                                ("early-init.el" "init.el")
                                                ("i3/config" "i3status/config")
                                                ("shell.nix" "default.nix")
                                                ("_test.go" ".go")
                                                ("-test.el" ".el")))
  (defun meain/find-alternate-file (&optional create)
    "Open alternate file.  Useful for opening test of currently active file.
Pass `CREATE' to create the alternate file if it does not exits."
    (interactive "P")
    (if (buffer-file-name)
        (let* ((file-patterns
                (apply #'append
                       (seq-map (lambda (x)
                                  (if (> (length (car x)) (length (cadr x)))
                                      (list (list (car x) (cadr x))
                                            (list (cadr x) (car x)))
                                    (list (list (cadr x) (car x))
                                          (list (car x) (cadr x)))))
                                meain/find-alternate-file--patterns)))
               (alt-file
                (car (remove-if (lambda (x) (equal x nil))
                                (seq-map (lambda (f)
                                           (if (string-match (car f) (buffer-file-name))
                                               (s-replace-regexp (car f) (nth 1 f) (buffer-file-name))))
                                         file-patterns)))))
          (message "Switching to %s" (file-name-nondirectory alt-file))
          (if alt-file
              (if (file-exists-p alt-file)
                  (find-file alt-file)
                (if create
                    (find-file alt-file)
                  (message "Alternate file '%s' is not available on disk" alt-file)))
            (message "Unable to determine alternate file")))
      (message "Not in a file")))
  :init
  (evil-leader/set-key "e e" 'meain/find-alternate-file))

;; Splitting and joining list (https://github.com/AckslD/nvim-trevJ.lua)
(use-package tree-surgeon
  :load-path "/home/meain/dev/src/tree-surgeon"
  :after (evil-leader)
  :config (evil-leader/set-key "H j" 'tree-surgeon-split-join))

;; Screenshot Emacs frame
(defvar meain/frameshot-directory "~/docs/Pictures/Screenshots/"
  "Default directory for frame shots.")
(defvar meain/frameshot-format 'svg
  "Default frame shot format.")
(defun meain/frameshot ()
  "Save Emacs frame as frame shot.
Directory is determined by variable `frameshot-directory' and if
not defined, it will be saved in the `$HOME' directory."
  (interactive)
  (let* ((image (x-export-frames nil (or meain/frameshot-format 'png)))
	     (directory (or meain/frameshot-directory (getenv "HOME")))
	     (file (concat directory (format-time-string "EMACS-Screenshot-%Y-%m-%d-%T.")
		               (symbol-name meain/frameshot-format))))
    (make-directory directory t)
    (with-temp-file file (insert image))
    (meain/copy-to-clipboard (expand-file-name file))
    (message "Frame shot saved as `%s'" file)))

;; Just some hima testing code
(defun meain/reload-current-theme ()
  "Util to reload hima theme for debugging."
  (interactive)
  (message "%s" custom-enabled-themes)
  (let ((current-theme (car custom-enabled-themes)))
    (disable-theme current-theme)
    (load-theme current-theme t)))

(use-package which-func :commands (which-function))

;; Better modeline
;; TODO: `mode-line-format-right-align' can be used to right align items in modeline
(use-package mode-line-idle
  :elpaca t
  :commands (mode-line-idle))
(setq-default mode-line-format
              (list
               '(:eval
                 (mode-line-idle 0.3
                                 '(:eval (propertize
                                          "█"
                                          'font-lock-face
                                          (list :foreground (concat "#"
                                                                    (substring
                                                                     (md5 (if (project-current)
                                                                              ;; TODO: encode worktree information?
                                                                              (meain/project-name)
                                                                            "")) 0 6)))))
                                 "░"))
               '(:eval (if (eq 'emacs evil-state) "[E] " " ")) ;; vim or emacs mode
               '(:eval (list (if (eq buffer-file-name nil)
                                 ""
                               (concatenate 'string
                                            (car (cdr (reverse (split-string (buffer-file-name) "/"))))
                                            "/"))
                             (propertize "%b"
                                         'face
                                         (if (buffer-modified-p)
                                             'font-lock-string-face
                                           'font-lock-builtin-face)
                                         'help-echo
                                         (buffer-file-name))))
               (propertize ":%l:%c")
               ;; '(:eval (mode-line-idle 0.3
               ;;                         '(:propertize (:eval
               ;;                                        (if (boundp 'tree-sitter-mode)
               ;;                                            (let ((thing-name (meain/tree-sitter-thing-name 'class-like)))
               ;;                                              (if thing-name (format ":%s" thing-name)))))
               ;;                                       face
               ;;                                       hima-simple-gray)
               ;;                         ""))
               ;; '(:eval (mode-line-idle 0.3
               ;;                         '(:propertize (:eval
               ;;                                        (if (boundp 'tree-sitter-mode)
               ;;                                            (let ((thing-name (meain/tree-sitter-thing-name 'function-like))
               ;;                                                  (config-nesting (meain/tree-sitter-config-nesting)))
               ;;                                              (if thing-name
               ;;                                                  (format ":%s" thing-name)
               ;;                                                (if config-nesting
               ;;                                                    (format ":%s" config-nesting))))
               ;;                                          (when-let (func-name (which-function))
               ;;                                            (format ":%s" func-name))))
               ;;                                       face
               ;;                                       hima-simple-gray)
               ;;                         ""))
               '(:eval (mode-line-idle 1.0
                                       '(:propertize (:eval (if (and (project-current)
                                                                     (not (file-remote-p default-directory)))
                                                                (list " "
                                                                      (let* ((explicit (cdr (car (cdr (cdr (tab-bar--current-tab))))))
                                                                             (name (cdr (car (cdr (tab-bar--current-tab)))))
                                                                             (out-name (if explicit
                                                                                           (concatenate 'string ":" name)
                                                                                         (if (project-current)
                                                                                             (concat ";"
                                                                                                     (meain/project-name))
                                                                                           ""))))
                                                                        (format "%s" out-name)))))
                                                     face
                                                     hima-simple-gray)
                                       ""))
               '(:eval (mode-line-idle 1.0
                                       '(:propertize (:eval (when-let (vc vc-mode)
                                                              (list " @" (substring vc 5))))
                                                     face
                                                     hima-simple-gray)
                                       ""))
               '(:eval (if (boundp 'keycast-mode-line) keycast-mode-line))
               'mode-line-format-right-align
               '(:eval (if (boundp 'org-timer-mode-line-string) (concat org-timer-mode-line-string " ")))
               (propertize "%p") ;; position in file
               (propertize " %m ")))

;; Print emacs startup time
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds" (float-time (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;;scratch message
(setq initial-scratch-message (let ((package-count 0)
                                    (time (emacs-init-time)))
                                (when (bound-and-true-p package-alist)
                                  (setq package-count (length package-activated-list)))
                                ;; TODO: figure out how to get elpaca package count
                                ;; (when (boundp 'straight--profile-cache)
                                ;;   (setq package-count (+ (hash-table-size straight--profile-cache)
                                ;;                          package-count)))
                                (if (zerop package-count)
                                    (format ";; Emacs started in %s" time)
                                  (format ";; %d packages loaded in %s" package-count time))))

;; Auto updating scratch message
(run-at-time "3 minutes" (* 5 60) 'meain/update-scratch-message)

;; Start server once we have emacs running
(require 'server)
(unless (server-running-p)
  (progn
    (server-start)
    (start-process-shell-command "server-start-notify" "*server-start-notify*" "notify --pri 'Emacs server started'")))

(provide 'init)

;;; init.el ends here
