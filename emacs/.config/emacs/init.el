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

(defun get-api-key (key)
  "Retrieve the API key for the specified KEY from 'pass'."
  (string-trim (shell-command-to-string (format "pass show %s 2>/dev/null" key)) "\n" "\n"))
(defvar groq-api-key (get-api-key "groq/apikey"))
(defvar openrouter-api-key (get-api-key "openrouter/apikey"))
(defvar openai-api-key (get-api-key "openai/apikey"))
(defvar anthropic-api-key (get-api-key "anthropic/apikey"))
(defvar github-models-api-key (get-api-key "github-models/apikey"))

;; Setup elpaca
(defvar elpaca-installer-version 0.11)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
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
    (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable :ensure use-package keyword.
  (elpaca-use-package-mode))

;; Block until current queue processed.
(elpaca-wait)

;; Use package config
(setq use-package-verbose t)
(setq use-package-enable-imenu-support t)
(setq use-package-always-demand (getenv "LOAD_FULL_EMACS"))
(setq use-package-compute-statistics t) ;; Run `use-package-statistics' to get load timings

;; Benchmark emacs startup (enable when necessary)
(use-package benchmark-init
  :ensure t
  :disabled t
  :config
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

;; Get proper PATH (not used as we are launching from shell)
;; TODO: Convert to async: https://br0g.0brg.net/2024/emacs-async-exec-path-from-shell.html
(use-package exec-path-from-shell
  :ensure t
  :disabled t
  :config
  ;; https://github.com/purcell/exec-path-from-shell#making-exec-path-from-shell-faster
  ;; (setq exec-path-from-shell-arguments '("-l")) ;; removing -i
  (exec-path-from-shell-initialize))

;;; [BASE EVIL] =================================================

;; Evil mode (set this up first)
(use-package evil
  :ensure t

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
  (define-key evil-normal-state-map (kbd "Y") 'meain/yank-till-line-end)

  ;; Buffer navigation
  (define-key evil-normal-state-map (kbd "C-S-o") 'previous-buffer)
  (define-key evil-normal-state-map (kbd "C-S-i") 'next-buffer)

  (define-key evil-normal-state-map "Q" 'evil-record-macro)
  (define-key evil-visual-state-map
              (kbd ";")
              (lambda ()
                (interactive)
                (call-interactively 'eval-region)
                (evil-force-normal-state)))

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

  (defun meain/yank-till-line-end ()
    "Yank till end of line."
    (interactive)
    (evil-yank (point) ;; subtracting 1 for newline
               (- (save-excursion (forward-line) (point)) 1))))

;; Evil leader
(use-package evil-leader
  :ensure t
  :after evil
  :config
  (global-evil-leader-mode)
  (evil-leader/set-leader "s")

  ;; server edit
  (evil-leader/set-key "s s" 'server-edit)

  ;; buffer commands
  (evil-leader/set-key "b k" 'kill-buffer)
  (evil-leader/set-key "b o" 'previous-buffer)
  (evil-leader/set-key "b f" 'find-file)
  (evil-leader/set-key "b d" 'delete-frame))

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

;; Highlight trailing whitespace
(setq-default show-trailing-whitespace t)
;; (set-face-background 'trailing-whitespace "red")

;;; [VISUAL CONFIG] ==============================================

;; Set up fonts
(use-package emacs
  :commands (meain/select-font meain/what-font-am-i-using)
  :config
  (defun meain/select-font ()
    "Select and set a font."
    (interactive)
    (let ((font-name (completing-read "Choose font: " (cl-remove-duplicates (font-family-list)))))
      (let ((family (meain/get-font-prop font-name 'family))
            (weight (meain/get-font-prop font-name 'weight)))
        (set-frame-font family)
        (set-face-attribute 'default nil :font family :weight weight)
        (set-face-attribute 'fixed-pitch nil :font family :weight weight)
        (set-face-attribute 'variable-pitch nil :font family :weight weight)
        (setq-default line-spacing (meain/get-font-prop font-name 'line-spacing)))))
  (defun meain/what-font-am-i-using ()
    "Show the name/details for the current font in use."
    (interactive)
    (message "%s" (face-attribute 'default :font))))

;; Bell: audio -> visual
(use-package emacs
  :config
  (setq visible-bell nil)
  (setq ring-bell-function (lambda ()
                             (unless (memq this-command
                                           '(isearch-abort abort-recursive-edit exit-minibuffer keyboard-quit))
                               (invert-face 'mode-line)
                               (invert-face 'header-line)
                               (run-with-timer 0.1 nil 'invert-face 'mode-line)
                               (run-with-timer 0.1 nil 'invert-face 'header-line)))))

;; Theme
(setq hima-theme-load-path (concat (getenv "HOME") "/dev/src/hima-theme"))
(use-package hima-theme
  :load-path hima-theme-load-path
  :config
  (load-theme 'hima t))

;;; [BASIC BUILTINS] ===========================================

;; Use mouse to do some stuff when you are lazy
;; TODO: Causes Emacs to freeze when open
(context-menu-mode nil)

;; Show open and closing brackets
(use-package show-paren
  :defer t
  :config
  (setq show-paren-delay 0)
  (setq show-paren-context-when-offscreen t)
  (setq show-paren-style 'parenthesis)
  (show-paren-mode t))

;; Keep files in sync with filesystem
;; Tweak these settings carefully. This makes things quite slow if not
;; configured correctly.
(use-package autorevert
  :config
  (setq auto-revert-interval 5)
  (setq auto-revert-check-vc-info nil)
  (setq global-auto-revert-non-file-buffers nil)
  (setq auto-revert-verbose nil)
  (global-auto-revert-mode t))

;; Disable line wrapping
(use-package emacs
  :after evil-leader
  :defer t
  :config
  (setq-default truncate-lines 1)
  (evil-leader/set-key "b w" 'toggle-truncate-lines))

;; auto-fill
(use-package emacs
  :after evil-leader
  :defer t
  :config
  (evil-leader/set-key "b F" 'auto-fill-mode))

;; Cursor blink
(blink-cursor-mode -1)

;; Follow symlinks for vc
(setq vc-follow-symlinks t)

;; auto-pair
(use-package electric
  :defer t
  :config
  ;; https://www.reddit.com/r/emacs/comments/1hwf46n/comment/m63mddk
  ;; This ensures multiple quotes are not added at the beginning or end of a word
  (defun meain/electric-pair-conservative-inhibit (char)
    (or
     ;; I find it more often preferable not to pair when the
     ;; same char is next.
     (eq char (char-after))
     ;; Don't pair up when we insert the second of "" or of ((.
     (and (eq char (char-before))
          (eq char (char-before (1- (point)))))
     ;; I also find it often preferable not to pair next to a word.
     (eq (char-syntax (following-char)) ?w)
     ;; Don't pair at the end of a word, unless parens.
     (and
      (eq (char-syntax (char-before (1- (point)))) ?w)
      (eq (preceding-char) char)
      (not (eq (char-syntax (preceding-char)) ?\()))))
  (setq electric-pair-inhibit-predicate 'meain/electric-pair-conservative-inhibit)
  (electric-pair-mode t))

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
(defmacro alambda (original alternate)
  "Macro for easily creating commands with alternate on `universal-argument'.
Pass ORIGINAL and ALTERNATE options."
  `(lambda (&optional use-alternate)
     (interactive "P")
     (if use-alternate ,alternate ,original)))

;;; [EVIL CONFIG] ================================================

;; Evil commentary
(use-package evil-commentary
  :ensure t
  :defer nil
  :config (evil-commentary-mode))

;; Evil surround
(use-package evil-surround
  :ensure t
  :defer nil
  :config (global-evil-surround-mode 1))

;; Evil text objects
(use-package evil-textobj-line :ensure t)
(use-package evil-textobj-syntax :ensure t)
(use-package evil-indent-plus
  :ensure t
  :defer nil
  :config
  (define-key evil-inner-text-objects-map "i" 'evil-indent-plus-i-indent)
  (define-key evil-outer-text-objects-map "i" 'evil-indent-plus-a-indent)
  (define-key evil-inner-text-objects-map "I" 'evil-indent-plus-i-indent-up-down)
  (define-key evil-outer-text-objects-map "I" 'evil-indent-plus-a-indent-up-down))

;; Evil number increment
(use-package evil-numbers
  :ensure t
  :defer t
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
  (define-key evil-normal-state-map
              (kbd "<SPC> <SPC>")
              (lambda ()
                (interactive)
                (if (equal (buffer-name) "*scratch*")
                    (with-temp-file
                        (concat
                         "/tmp/emacs-scratch-"
                         (format-time-string "%Y-%m-%d-%H-%M-%S"))
                      (message "Saved scratch to a temporary location")
                      (insert (with-current-buffer "*scratch*" (buffer-string))))
                  (call-interactively 'evil-write)))))

;; Hit universal arg without ctrl
(use-package emacs
  :after evil-leader
  :defer t
  :config
  (evil-leader/set-key "u" 'universal-argument)
  (global-set-key (kbd "M-u") 'universal-argument)
  (define-key universal-argument-map (kbd "M-u") 'universal-argument-more))

;; Auto resize windows (useful in go buffer, folks don't stop at 80)
(use-package golden-ratio
  :ensure t
  ;; Enable minor-mode manually when required
  :commands (golden-ratio golden-ratio-mode))

;; A silly little package to encourage on save
(use-package emacs
  :defer t
  :config
  (load (concat user-emacs-directory "encourage")))

;; Window commands
(use-package emacs
  :config
  (defun meain/move-or-swap (direction &optional swap)
    "Move to or swap window in DIRECTION (up/down/left/right) based on SWAP."
    (funcall (if swap
                 (intern (format "windmove-swap-states-%s" direction))
               (intern (format "windmove-%s" direction)))))

  ;; These are used a lot and so we need to defined these as functions
  (defun meain/move-swap-right () (interactive) (meain/move-or-swap "right"))
  (defun meain/move-swap-left () (interactive) (meain/move-or-swap "left"))
  (defun meain/move-swap-up () (interactive) (meain/move-or-swap "up"))
  (defun meain/move-swap-down () (interactive) (meain/move-or-swap "down"))

  (global-set-key (kbd "M-l") 'meain/move-swap-right)
  (global-set-key (kbd "M-h") 'meain/move-swap-left)
  (global-set-key (kbd "M-k") 'meain/move-swap-up)
  (global-set-key (kbd "M-j") 'meain/move-swap-down)

  (defun meain/split-window (direction &optional open-term)
    "Split window in DIRECTION and optionally OPEN-TERM."
    (funcall (intern (format "split-window-%s" direction)))
    (funcall (intern (format "windmove-%s" (if (equal direction "below") "down" "right"))))
    (when open-term (meain/eshell-toggle)))

  (global-set-key (kbd "M-b") (lambda (&optional term) (interactive "P") (meain/split-window "below" term)))
  (global-set-key (kbd "M-v") (lambda (&optional term) (interactive "P") (meain/split-window "right" term)))
  (global-set-key (kbd "M-w") 'delete-window))

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
            (if (project-current) (meain/project-name) "-")))
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
           " ")))
  (add-hook 'eshell-mode-hook (lambda ()
                                (setenv "TERM" "xterm-256color")
                                (define-key eshell-mode-map (kbd "M-l") 'meain/move-swap-right)
                                (define-key eshell-mode-map (kbd "M-h") 'meain/move-swap-left)
                                (define-key eshell-mode-map (kbd "M-k") 'meain/move-swap-up)
                                (define-key eshell-mode-map (kbd "M-j") 'meain/move-swap-down))))

;; Show the last command in the shell at top
(use-package sticky-shell
  :ensure (sticky-shell :host github
                        :repo "andyjda/sticky-shell")
  :after eshell
  :commands (sticky-shell-mode)
  :init
  (add-hook 'eshell-mode-hook 'sticky-shell-mode))

;; Midnight: Kill unused buffers at midnight
(use-package midnight
  :defer t
  :config
  (setq clean-buffer-list-delay-general 1)
  (midnight-mode t))

;; Shrink and enlarge windows (not contextual as of now)
;; https://www.emacswiki.org/emacs/WindowResize
(defmacro ilambda (functionname &rest args)
  "Create an interactive lambda of existing function `FUNCTIONNAME' with `ARGS'."
  (let ((funsymbol (concat "i/" (symbol-name functionname))))
    `(cons ,funsymbol (lambda () (interactive) (apply #',functionname ',args)))))
(global-set-key (kbd "M-H") (ilambda shrink-window-horizontally 5))
(global-set-key (kbd "M-L") (ilambda enlarge-window-horizontally 5))
(global-set-key (kbd "M-K") (ilambda shrink-window 5))
(global-set-key (kbd "M-J") (ilambda enlarge-window 5))

;; Switch to other frame
(use-package frame
  :after evil-leader
  :defer t
  :config
  (global-set-key (kbd "M-o") 'other-frame))

(use-package emacs
  :after evil
  :init
  (evil-leader/set-key "c" 'meain/create-or-switch-to-scratch)
  (define-key evil-normal-state-map (kbd "q") 'meain/kill-current-buffer-unless-scratch)
  :commands (meain/update-scratch-message meain/create-or-switch-to-scratch meain/kill-current-buffer-unless-scratch)
  :config
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
          (insert (format ";; The LLMs have been going through your %s buffers\n;; FYI, I have been up for the last %s"
                          (cl-count-if (lambda (b)
                                         (or (buffer-file-name b)
                                             (not (string-match "^ " (buffer-name b)))))
                                       (buffer-list))
                          (emacs-uptime "%D, %H")))))))

  (defun meain/create-or-switch-to-scratch ()
    "Switch to scratch buffer if exists, else create a scratch buffer with our config."
    (interactive)
    (cond
     ((get-buffer "*scratch*")
      (switch-to-buffer "*scratch*"))
     (t (progn
          (switch-to-buffer "*scratch*")
          (setq default-directory "~/")
          (lisp-interaction-mode)
          (meain/update-scratch-message)))))

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
       (t (previous-buffer))))))

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
  :defer t
  :init
  (define-key evil-normal-state-map (kbd "<SPC> ;") (cons "replace in buffer" (ilambda evil-ex "%s/")))
  (define-key evil-visual-state-map (kbd "<SPC> ;") (cons "replace in buffer"(ilambda evil-ex "'<,'>s/"))))

;; Highlight yanked region
(defun meain/evil-yank-advice (orig-fn beg end &rest args)
  "Advice to be added to `evil-yank' to highlight yanked region.
Pass ORIG-FN, BEG, END, TYPE, ARGS."
  (pulse-momentary-highlight-region beg end 'mode-line)
  (apply orig-fn beg end args))
(advice-add 'evil-yank :around 'meain/evil-yank-advice)

(use-package comint
  :defer t
  :config
  (add-hook 'comint-mode-hook (lambda () (setq-local show-trailing-whitespace nil))))

;; Recompile binding
(use-package compile
  :commands (compile recompile)
  :after (evil)
  :config
  (setq compilation-always-kill t)
  (setq compilation-ask-about-save nil)
  (setq compilation-scroll-output nil)

  ;; Ensure ansi escape sequences are rendered properly
  (setq ansi-color-for-compilation-mode t)
  (add-hook 'compilation-filter-hook #'ansi-color-compilation-filter)

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
    (alambda
     (call-interactively 'recompile)
     (call-interactively 'compile))))

;; Code coverage in buffer
;; To get coverage, run `go test -coverprofile=coverage.out ./...`
;; and then convert this to lcov using
;; gcov2lcov -infile coverage.out -outfile coverage.lcov -use-absolute-source-paths
;; Now you can load this into coverlay
(use-package coverlay
  :ensure t
  :commands (coverlay-load-file)
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
    (if (project-current)
        (file-name-nondirectory (directory-file-name
                                 (project-root (project-current))))))

  (defun meain/find-file-git-changed ()
    "Fuzzy find git changed files."
    (interactive)
    (let* ((untracked-files (shell-command-to-string "git ls-files --others --exclude-standard"))
           (changed-files (shell-command-to-string "git diff --name-only"))
           (files (split-string (concat untracked-files "\n" changed-files) "\n" t)))
      (find-file (completing-read "Pick file: " files))))

  (defun meain/find-file-semantic ()
    (interactive)
    (let* ((user-query (read-string "Search for: "))
           (default-directory (project-root (project-current)))
           (refer-output (shell-command-to-string (concat "refer search '"user-query "'")))
           (files (seq-map (lambda (x) (cadr (split-string x " " t)))
                           (split-string refer-output "\n" t))))
      (find-file (completing-read "Pick file: " files))))

  (defun meain/refresh-semantic-search-index ()
    (interactive)
    (let ((default-directory (project-root (project-current))))
      (async-shell-command "refer add . --ignore && refer reindex" "*semantic-index-refresh*")))

  :init
  (evil-leader/set-key "p p"
    (alambda (call-interactively 'project-switch-project)
             (project-find-file)))

  (define-key evil-normal-state-map (kbd "<SPC> <RET>") 'meain/find-file-git-changed)
  (define-key evil-normal-state-map (kbd "<M-RET>") (alambda
                                                     (meain/find-file-semantic)
                                                     (meain/refresh-semantic-search-index)))
  (define-key evil-normal-state-map (kbd "<RET>") 'project-find-file))

;; eldoc load
(use-package eldoc
  :defer t
  :after (evil)
  :config
  (setq eldoc-echo-area-use-multiline-p nil)
  (define-key evil-normal-state-map (kbd "K") 'eldoc-print-current-symbol-info)
  (global-eldoc-mode nil))

;; Show eldoc messages in a popup at point
(use-package eldoc-box
  :ensure t
  :commands (eldoc-box-help-at-point eldoc-box-hover-mode eldoc-box-hover-at-point-mode)
  :init
  (global-set-key (kbd "M-d")
                  (lambda ()
                    (interactive)
                    (let ((eldoc-echo-area-use-multiline-p t))
                      (call-interactively #'eldoc-box-help-at-point)))))

;; dired
(use-package dired
  :defer 1
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
  :after dired
  :defer t
  :config
  (setq dired-omit-files "\\.DS_Store$\\|__pycache__$\\|.pytest_cache$\\|\\.mypy_cache$\\|\\.egg-info$"))

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
  :ensure t
  :after (tree-surgeon)
  ;; can't defer loading of this as we need it in every single spawned
  ;; buffer including scratch
  :init (add-hook 'find-file-hook #'aas-activate-for-major-mode)
  :config
  (aas-global-mode)
  (aas-set-snippets 'global
                    ";date" '(tempel (format-time-string "%a %b %d %Y"))
                    ";time" '(tempel (format-time-string "%H:%M"))
                    ";file" '(tempel (file-name-nondirectory (buffer-file-name)))
                    ";path" '(tempel (string-remove-prefix
                                      (expand-file-name (project-root (project-current)))
                                      (buffer-file-name))))
  (aas-set-snippets 'emacs-lisp-mode
                    ";auto" ";;;###autoload"
                    ";la" '(tempel "(lambda (" p ") " r ")")
                    ";li" '(tempel "(lambda () (interactive) " r ")")
                    ";j" '(tempel "(message \"" r "\")"))
  (aas-set-snippets 'sql-mode
                    ";base" "SELECT * FROM information_schema.tables;")
  (aas-set-snippets 'js-mode
                    ";j" '(tempel "console.log(\"" r "\")"))
  (aas-set-snippets 'go-ts-mode
                    "!+" "!="
                    ";;" ":="
                    ";j" '(tempel "fmt.Println(\"" r "\")")
                    ";ap" '(tempel (s slice) " = append(" (s slice) ", " r ")")
                    ";rr" '(tempel "for _, " p " := range " p "{" n> r> n> "}")
                    ";ri" '(tempel "for i, " p " := range " p "{" n> r> n> "}")
                    ";er" '(tempel "if err != nil {" n> r> n> "}")))

;; Templates
(use-package tempel
  :ensure t
  :commands (tempel-complete tempel-expand tempel-insert)
  :init
  (global-set-key (kbd "M-*") 'tempel-complete)
  (global-set-key (kbd "M-)") 'tempel-next)
  (global-set-key (kbd "M-(") 'tempel-previous))

(use-package ispell
  :defer t
  :config
  ;; Was having some trouble with aspell not detecting dicts
  (setq ispell-program-name "ispell"))

;; flyspell
(use-package flyspell
  :defer t
  :after ispell
  :commands (flyspell-prog-mode flyspell-mode flyspell-goto-next-error)
  :init
  (add-hook 'prog-mode-hook 'flyspell-prog-mode)
  (add-hook 'text-mode-hook 'flyspell-mode)
  :bind (:map flyspell-mode-map ("C-;" . flyspell-auto-correct-word))
  :config
  (setq flyspell-delay-use-timer t) ;; use timer instead of sit-for
  ;; Make flyspell work with tree-sitter
  (setq-default flyspell-prog-text-faces
                '(tree-sitter-hl-face:comment
                  tree-sitter-hl-face:doc
                  tree-sitter-hl-face:string
                  font-lock-comment-face
                  font-lock-doc-face
                  font-lock-string-face)))
(use-package flyspell-correct
  :ensure t
  :after flyspell
  :commands (flyspell-correct-wrapper flyspell-goto-next-error)
  :bind (:map flyspell-mode-map ("C-:" . flyspell-correct-wrapper)))

;; flymake
(use-package flymake
  :defer 1
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
  :ensure t
  :after (flymake evil-leader)
  :config
  (setq flymake-diagnostic-at-point-error-prefix "! ")
  (setq flymake-diagnostic-at-point-display-diagnostic-function 'flymake-diagnostic-at-point-display-minibuffer)
  (evil-leader/set-key "j" 'flymake-goto-next-error)
  (evil-leader/set-key "k" 'flymake-goto-prev-error)
  (add-hook 'flymake-mode-hook #'flymake-diagnostic-at-point-mode))
(use-package flymake-quickdef
  :ensure t
  :after flymake
  :config
  ;; tint lints
  (flymake-quickdef-backend flymake-check-tint
                            :pre-let ((tint-exec (executable-find "tint")))
                            :pre-check (unless tint-exec (error "Cannot find tint executable"))
                            :write-type 'file
                            :proc-form (list tint-exec "lint" fmqd-temp-file)
                            :search-regexp "^\\([^:]+\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\): \\(.*\\)$"
                            :prep-diagnostic (let* ((lnum (string-to-number (match-string 2)))
                                                    (col (string-to-number (match-string 3)))
                                                    (pos (flymake-diag-region fmqd-source lnum col))
                                                    (beg (car pos))
                                                    (end (cdr pos))
                                                    (msg (format "tint> %s" (match-string 6))))
                                               (list fmqd-source beg end :warning msg)))
  (add-hook 'go-ts-mode-hook
            (lambda ()
              (add-hook 'flymake-diagnostic-functions 'flymake-check-tint nil t)))

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
              (add-hook 'flymake-diagnostic-functions 'flymake-hadolint nil t))))

(use-package flymake-collection
  :ensure t
  :defer t
  :hook (after-init . flymake-collection-hook-setup))

(use-package corfu
  :ensure (:files (:defaults "extensions/*"))
  :defer 1
  :config
  (setq completion-cycle-threshold 3)
  (setq corfu-auto t)
  (setq corfu-cycle t)
  (setq corfu-auto-delay 0.2) ;; chill for a moment (perf related, unverified)
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
  :ensure t
  :defer t
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
  :ensure (:files (:defaults "extensions/*.el"))
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

  (defun common-path-length (path1 path2)
    "Return the number of common directories in PATH1 and PATH2."
    (let ((split-path1 (split-string path1 "/"))
          (split-path2 (split-string path2 "/")))
      (cl-loop for dir1 in split-path1
               for dir2 in split-path2
               while (string= dir1 dir2)
               count 1)))

  (defun sort-by-proximity (files target-file)
    "Sort FILES by how close they are in structure to TARGET-FILE."
    (sort files
          (lambda (file1 file2)
            (> (common-path-length file1 target-file)
               (common-path-length file2 target-file)))))

  (defun meain/sort-files-list (files)
    (if (project-current)
        (let* ((prev-buffer (window-buffer (minibuffer-selected-window)))
               (current-file (if (buffer-file-name prev-buffer)
                                 (buffer-file-name prev-buffer)
                               "."))
               (project-path (expand-file-name (project-root (project-current))))
               (current-file-sans-project (string-remove-prefix project-path current-file)))
          (sort (sort-by-proximity files current-file-sans-project)
                ;; Also give less priority to "/mock/" files
                (lambda (file1 file2)
                  (if (string-match-p "/mock/" file1)
                      nil
                    (if (string-match-p "/mock/" file2)
                        t
                      nil)))))
      files))

  (vertico-multiform-mode)
  (setq vertico-multiform-commands
        '((consult-ripgrep buffer indexed)
          (consult-xref buffer indexed)
          (eglot-find-implementation indexed) ;; TODO: change to vertical
          (consult-imenu buffer)
          (project-find-file flat (vertico-sort-function . meain/sort-files-list))
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
(use-package savehist :config (savehist-mode t) :defer t)
(use-package orderless
  :ensure t
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
  :ensure t
  :defer nil
  :bind (:map minibuffer-local-map ("C-b" . marginalia-cycle))
  :config (marginalia-mode))

;; Show completions option even when there is a typo
;; (use-package typo
;;   :ensure t
;;   :config (add-to-list 'completion-styles 'typo t))

;; Consult without consultation fees
(use-package consult
  :ensure t
  :after (xref evil)
  :defer t
  :config
  (setq consult-ripgrep-args "rg --null --line-buffered --color=never --max-columns=1000 --path-separator /\
      --smart-case --no-heading --line-number --hidden --follow --glob \"!.git/*\"")
  (setq xref-show-xrefs-function #'consult-xref)
  (setq xref-show-definitions-function #'consult-xref)
  (evil-set-command-property 'consult-imenu :jump t)
  :init
  (define-key evil-normal-state-map (kbd "<SPC> /") 'consult-line))

;; Embark stuff
(use-package embark
  :defer t
  :ensure t
  :config
  (setq prefix-help-command #'embark-prefix-help-command)
  :init
  (global-set-key (kbd "C-'")  'embark-act)
  (global-set-key (kbd "C-.")  'embark-export)
  (global-set-key (kbd "C-h B")  'embark-bindings))
(use-package embark-consult
  :ensure t
  :defer t
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode))

;; Helpful package
(use-package helpful
  :ensure t
  :after evil-leader
  :commands (helpful-callable helpful-variable helpful-at-point helpful-key)
  :init
  (global-set-key (kbd "C-h f") #'helpful-callable)
  (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h k") #'helpful-key)
  (global-set-key (kbd "C-h x") #'helpful-command)
  (global-set-key (kbd "C-h o") #'helpful-symbol)
  (global-set-key (kbd "C-c C-d") #'helpful-at-point)

  ;; find-library is not helpful related, but good to map in the same
  ;; block along with the helpful keys as they have similar keybinds
  (global-set-key (kbd "C-h l") #'find-library))

;; ibuffer
(use-package ibuffer
  :commands (ibuffer ibuffer-other-window)
  :init
  (setq ibuffer-expert t)
  (global-set-key (kbd "M-c")
                  (alambda (call-interactively 'switch-to-buffer)
                           (ibuffer-other-window))))
(use-package ibuffer-project
  :ensure t
  :defer t
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
  :ensure t
  :commands rg
  :after evil-leader
  :init
  (evil-leader/set-key "f"
    (alambda (consult-ripgrep) (call-interactively 'rg)))
  :config (setq rg-command-line-flags '("--hidden" "--follow")))

;; dumber-jump
(use-package dumber-jump
  :ensure t
  :defer t
  :after evil-leader
  :config
  (add-hook 'xref-backend-functions #'dumber-jump-xref-activate))

;; Code formatting
(use-package apheleia
  :ensure t
  :after evil
  :commands (apheleia-format-buffer meain/format-buffer)
  :config
  ;; json
  (setf (alist-get 'fixjson apheleia-formatters) '("fixjson"))
  (setf (alist-get 'json-mode apheleia-mode-alist) '(fixjson))

  ;; golang
  (setf (alist-get 'goimports apheleia-formatters) '("goimports"))
  (setf (alist-get 'gofumpt apheleia-formatters) '("gofumpt"))
  (setf (alist-get 'gci apheleia-formatters) '("gci" "/dev/stdin"))
  (setf (alist-get 'go-mode apheleia-mode-alist) '(goimports))
  (setf (alist-get 'go-ts-mode apheleia-mode-alist) '(goimports))

  ;; cedar
  (setf (alist-get 'cedar-format apheleia-formatters) '("cedar" "format"))
  (setf (alist-get 'cedar-mode apheleia-mode-alist) '(cedar-format))

  ;; markdown
  (setf (alist-get 'markdown-mode apheleia-mode-alist) '(prettier-markdown))

  ;; clojure
  (setf (alist-get 'zprint apheleia-formatters) '("zprint"))
  (setf (alist-get 'clojure-mode apheleia-mode-alist) '(zprint))

  ;; shell
  (setf (alist-get 'shell-script-mode apheleia-mode-alist) '(shfmt))
  (setf (alist-get 'sh-mode apheleia-mode-alist) '(shfmt))

  ;; nix
  (setf (alist-get 'nixpkgsfmt apheleia-formatters) '("nixpkgs-fmt"))
  (setf (alist-get 'nixfmt apheleia-formatters) '("nixfmt"))
  (setf (alist-get 'nix-mode apheleia-mode-alist) '(nixfmt))

  (defun meain/format-buffer ()
    "Format a buffer."
    (interactive)
    (cond
     ((eq major-mode 'emacs-lisp-mode) (indent-region (point-min) (point-max)))
     ((eq major-mode 'ledger-mode) (ledger-mode-clean-buffer))
     (t (call-interactively 'apheleia-format-buffer))))

  :init
  (add-hook 'go-mode-hook 'apheleia-mode)
  (add-hook 'go-ts-mode-hook 'apheleia-mode)
  (define-key evil-normal-state-map (kbd ",,") #'meain/format-buffer))

;; Xref customization
(use-package xref
  :after (evil)
  :defer t
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
;; (use-package yasnippet :ensure t)
;; (use-package lspce
;;   :load-path "/Users/meain/dev/src/lspce"
;;   :after (yasnippet)
;;   :config (progn
;;             ;; (setq lspce-send-changes-idle-time 1)
;;             ;; (lspce-set-log-file "/tmp/lspce.log")
;;             ;; (lspce-enable-logging)
;;             ;; (add-hook 'rust-mode-hook 'lspce-mode)
;;             ;; (add-hook 'go-ts-mode-hook 'lspce-mode)
;;             (setq lspce-server-programs `(("rust"  "rust-analyzer" "" lspce-ra-initializationOptions)
;;                                           ("python" "pylsp" "" )
;;                                           ("go" "gopls" "")
;;                                           ("C" "clangd" "--all-scopes-completion --clang-tidy --enable-config --header-insertion-decorators=0")
;;                                           ("java" "java" lspce-jdtls-cmd-args lspce-jdtls-initializationOptions)))))

(use-package el-patch
  :ensure t
  :after eglot
  :defer t
  :config
  ;; Make eglot play nicely with auto-revert mode
  ;; https://github.com/joaotavora/eglot/issues/1449#issuecomment-2378670111
  (with-eval-after-load 'eglot
    (el-patch-defun eglot--signal-textDocument/didOpen ()
      "Send textDocument/didOpen to server."
      (el-patch-add (eglot--track-changes-fetch eglot--track-changes))
      (setq eglot--recent-changes nil
            eglot--versioned-identifier 0
            eglot--TextDocumentIdentifier-cache nil)
      (jsonrpc-notify
       (eglot--current-server-or-lose)
       :textDocument/didOpen `(:textDocument ,(eglot--TextDocumentItem))))))

;; LSP
(use-package eglot
  :commands eglot-ensure
  ;; :ensure t ;; use builtin version
  :after (project flymake jsonrpc)
  :config
  ;; Supposedly speed up eglot
  ;; https://www.reddit.com/r/emacs/comments/17jrsmv/comment/k74b3tg/
  (advice-add 'jsonrpc--log-event :override #'ignore)
  (setopt eglot-events-buffer-size 10)

  (setq jsonrpc-event-hook nil)
  (setq eglot-autoshutdown t)
  (setq eglot-sync-connect nil)
  (setq eglot-extend-to-xref t) ;; extend eglot to files gone to with go-to-def
  ;; https://www.masteringemacs.org/article/seamlessly-merge-multiple-documentation-sources-eldoc
  (setq eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly)

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
  (add-to-list 'eglot-server-programs '(markdown-mode . ("markdown-oxide" "--stdio"))) ;; (also: prosemd-lsp)
  (add-to-list 'eglot-server-programs '(rust-mode . ("rust-analyzer")))
  (add-to-list 'eglot-server-programs '(python-mode . ("pylsp")))
  (add-to-list 'eglot-server-programs '(nix-mode . ("rnix-lsp")))
  (setq-default eglot-workspace-configuration
                ;; https://cs.opensource.google/go/x/tools/+/master:gopls/doc/emacs.md
                ;; https://cs.opensource.google/go/x/tools/+/master:gopls/doc/settings.md
                ;; (:gopls . ((staticcheck . t))) ;; Huge mem usage penalty
                '((:json.schemas . [((:fileMatch . ["package.json"]) (:url . "https://json.schemastore.org/package.json"))])))

  ;; add flymake backend separately so that I can add other things as well to flymake
  (add-to-list 'eglot-stay-out-of 'flymake)
  (add-hook 'flymake-diagnostic-functions 'eglot-flymake-backend)

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
  :ensure (:host github :repo "jdtsmith/eglot-booster")
  :after eglot
  :config
  ;; https://www.reddit.com/r/emacs/comments/1jsxamc/the_new_json_parser_is_fast/
  (setq eglot-booster-io-only t)
  (eglot-booster-mode t))

;; Get hierarchy
(use-package eglot-hierarchy
  :commands (eglot-hierarchy-call-hierarchy eglot-hierarchy-type-hierarchy)
  :ensure (:host github :repo "dolmens/eglot-hierarchy"))

;; consult-eglot
(use-package consult-eglot
  :ensure t
  :commands (consult-eglot-symbols meain/imenu-or-eglot)
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
;; (use-package dwim-coder-mode :ensure t)

;; Peek into files/definitions without opening them
(use-package peek
  :ensure (:host github :repo "Ziqi-Yang/peek")
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
  (load-file (concat (getenv "HOME") "/.config/emacs/tree-jump.el"))
  :init
  (add-to-list 'xref-backend-functions 'tree-jump-xref-backend)
  (global-set-key (kbd "M-I")
                  (alambda (if (string-suffix-p "_test.go" (buffer-file-name))
                               (consult-tree-jump-search)
                             (consult-tree-jump-search "!mock !_test "))
                           (tree-jump-search))))

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
  :ensure t
  :defer t
  :after imenu
  :config (flimenu-global-mode 1))
(use-package imenu-list
  :ensure t
  :defer t
  :after (imenu consult)
  :commands imenu-list-smart-toggle
  :config
  (setq imenu-list-focus-after-activation t)
  (setq imenu-list-after-jump-hook nil)
  (setq imenu-list-auto-resize t))

;; Symbol overlay
(use-package symbol-overlay
  :ensure t
  :defer t
  :commands (symbol-overlay-mode symbol-overlay-put))

;; magit dependency
(use-package transient :ensure t :defer t)

;; Magit
(use-package magit
  :ensure t
  :after (evil-leader transient)
  :commands (magit-status magit-commit-create magit-ignored-files meain/git-how-was-it)
  :init
  (evil-leader/set-key "gg" 'magit-status)
  (evil-leader/set-key "gc" 'magit-commit-create)
  (evil-leader/set-key "gB" 'magit-blame)
  (evil-leader/set-key "gb" 'magit-branch)
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
  :ensure (:host github :repo "pkryger/difftastic.el")
  :config
  (eval-after-load 'magit-diff
    '(transient-append-suffix 'magit-diff '(-1 -1)
       [("D" "Difftastic diff (dwim)" difftastic-magit-diff)
        ("S" "Difftastic show" difftastic-magit-show)])))

(use-package ediff
  :after (evil-leader)
  :defer t
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq ediff-split-window-function 'split-window-horizontally))

(use-package smerge-mode
  :after (evil evil-leader)
  :config
  ;; Builtin smerge mode function has some issues (override it)
  ;; TODO: Submit bug to bug-gnu-emacs once verified
  (defun smerge-keep-n (n)
    (let* ((match-begin-0 (match-beginning 0))
           (match-begin-n (match-beginning n))
           (match-end-0 (match-end 0))
           (match-end-n (match-end n)))
      (smerge-remove-props match-begin-0 match-end-0)
      (delete-region match-end-n match-end-0)
      (delete-region match-begin-0 match-begin-n)))
  :init
  (evil-leader/set-key "gmm" 'smerge-mode)
  (evil-leader/set-key "gme" 'smerge-ediff)
  (evil-leader/set-key "gmr" 'smerge-refine)
  (evil-leader/set-key "gmn" 'smerge-next)
  (evil-leader/set-key "gmp" 'smerge-prev)
  (evil-leader/set-key "gmu" 'smerge-keep-upper)
  (evil-leader/set-key "gml" 'smerge-keep-lower)
  (evil-leader/set-key "gma" 'smerge-keep-all))

;; Diff hl
(use-package diff-hl
  :ensure t
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
  :ensure t
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

;; Matchit
(use-package evil-matchit
  :ensure t
  :defer t
  :config (global-evil-matchit-mode 1))

;; Highlight color codes
;; Alternative: https://github.com/DevelopmentCool2449/colorful-mode
(use-package rainbow-mode
  :ensure t
  :commands (rainbow-mode)
  :init (add-hook 'css-mode-hook 'rainbow-mode))

;; Code folding
(use-package origami
  :ensure t
  :after (evil evil-leader)
  :defer t
  :config (global-origami-mode)
  :commands (evil-toggle-fold)
  :init
  (define-key evil-normal-state-map (kbd "<SPC> TAB") 'evil-toggle-fold)
  (evil-leader/set-key "o" 'evil-toggle-fold))

;; drag-stuff
(use-package drag-stuff
  :ensure t
  :after evil
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
  :defer t
  :init
  (save-place-mode t)
  (setq save-place-file "~/.local/share/emacs/saveplace"))

;; Persistent undo using undo-tree
(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode t)
  (setq undo-limit 80000000)
  (setq evil-want-fine-undo nil)
  (setq undo-tree-auto-save-history t)
  (setq undo-tree-history-directory-alist '(("." . "~/.local/share/emacs/undo"))))

;; Fancier tab management
(use-package tab-bar
  :after evil-leader
  :defer t
  :commands (tab-close tab-new tab-next tab-bar-rename-tab
                       meain/switch-tab-dwim meain/create-or-delete-tab
                       tab-bar-switch-to-tab)
  :config
  (setq tab-bar-history-limit 100)
  ;; (tab-bar-history-mode t)
  ;; (global-set-key (kbd "M-f <left>") 'tab-bar-history-back)
  ;; (global-set-key (kbd "M-f <right>") 'tab-bar-history-forward)

  (global-set-key (kbd "M-f ,") 'tab-bar-rename-tab)
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
  :ensure t
  :defer nil
  :config
  (which-key-mode))

;; Expand region
(use-package expand-region
  :ensure t
  :commands (er/expand-region)
  :config
  ;; make evil jump list work with expand-region
  (evil-set-command-property 'er/expand-region :jump t)
  :init
  (global-set-key (kbd "M--") 'er/expand-region))

;; dtrt (atuo find indend setting)
(use-package dtrt-indent
  :ensure t
  :config (dtrt-indent-global-mode))

(use-package indent-guide
  :ensure t
  :after (evil-leader)
  :commands (indent-guide-global-mode indent-guide-mode)
  :init
  (setq indent-guide-delay nil)
  (setq indent-guide-char "¦") ; Other chars │
  (setq indent-guide-recursive t)
  (evil-leader/set-key "b I" 'indent-guide-global-mode)
  :config
  (set-face-attribute 'indent-guide-face nil :foreground "#DDD"))

;; editorconfig
(use-package editorconfig
  :defer t
  :ensure t
  :config (editorconfig-mode 1))

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
  :ensure t
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
  (defun meain/toffee--get-cwd ()
    "Get the current working directory to run the test.
For example if it is go-mod file, look up the go.mod file and use that directory."
    (pcase major-mode
      ('go-mode (locate-dominating-file (buffer-file-name) "go.mod"))
      ('go-ts-mode (locate-dominating-file (buffer-file-name) "go.mod"))
      (_ default-directory)))
  (defun meain/toffee--get-test-command (mode)
    (let ((default-directory
           (expand-file-name
            ;; custom-src-directory is supposed to come from .dir-locals.el
            (if (boundp 'custom-src-directory)
                custom-src-directory
              (meain/toffee--get-cwd))))
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
          (progn (compile command))
        (message "Unable to find any tests"))))
  (defun meain/toffee-run-test (&optional _)
    "Run test based on `MODE'. By default runs current function.
Pass universal args to run suite or project level tests."
    (interactive "P")
    (let* ((mode (cond
                  ((equal current-prefix-arg nil) 'function)
                  ((equal current-prefix-arg '(4)) 'suite)
                  ((equal current-prefix-arg '(16)) 'project)))
           (dir-cmd (meain/toffee--get-test-command mode))
           (default-directory (car dir-cmd))
           (command (cdr dir-cmd)))
      (if command
          (progn
            (setq meain/toffee--previous-command command)
            (compile command))
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
  :ensure t
  :commands neotree
  :config
  (setq neo-window-fixed-size nil)
  (setq neo-theme 'classic)
  (add-hook 'neo-after-create-hook 'hl-line-mode))

;; Evil keybindings for a lot of things
(use-package evil-collection
  :defer nil
  :ensure t
  :after evil
  :config
  (setq evil-collection-magit-want-horizontal-movement t)
  (setq evil-collection-magit-use-y-for-yank t)
  (evil-collection-init))

;; Highlight TODO items
(use-package hl-todo
  :ensure t
  :defer nil
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
  :ensure t
  :defer t
  :commands (emmet-mode))

;; Direnv support
(use-package envrc
  :ensure t
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

(use-package rust-mode :ensure t :defer t)
(use-package clojure-mode :ensure t :defer t)
(use-package zig-mode :ensure t :defer t)
(use-package go-mode
  :ensure t
  :defer t
  :config
  (evil-set-command-property 'godef-jump :jump t))
(use-package go-fill-struct :ensure t :commands (go-fill-struct))
(use-package go-tag
  :ensure t
  :commands (go-tag-add go-tag-remove go-tag-refresh)
  :config (setq go-tag-args (list "-transform" "camelcase")))
(use-package go-impl
  :ensure t
  :commands (go-impl)
  :config (advice-add 'go-impl :around #'meain/use-custom-src-directory))
(use-package go-stacktracer :ensure t :commands (go-stacktracer-region))
(use-package lua-mode :ensure t :defer t)
(use-package web-mode :ensure t :defer t)
(use-package jinja2-mode :ensure t :defer t)
(use-package config-general-mode :ensure t :defer t :mode "/\\.env")
(use-package vimrc-mode :ensure t :defer t)
(use-package sxhkdrc-mode :ensure t :defer t)
(use-package edit-indirect :ensure t)
(use-package reformatter :ensure t :defer t) ;; needed by nix-mode
(use-package nix-mode :ensure t :defer t :mode "\\.nix\\'")
;; builtin package for scheme (for tree-sitter grammar)
(use-package scheme-mode :defer t :mode "\\.scm\\'")

(use-package markdown-mode
  :ensure t
  :defer t
  :after (edit-indirect evil)
  :mode ("\\.md\\'" . gfm-mode)
  :config
  (setq markdown-url-compose-char '(8230 8943 35 9733 9875))
  (setq markdown-enable-html -1)
  (setq markdown-gfm-use-electric-backquote nil) ; don't ask me to pick lang in ```
  (evil-define-key 'normal gfm-mode-map (kbd "<RET>") 'project-find-file)
  (evil-define-key 'normal gfm-mode-map (kbd "g d") 'markdown-do)
  (evil-define-key 'normal markdown-mode-map (kbd "<RET>") 'project-find-file)
  (evil-define-key 'normal markdown-mode-map (kbd "g d") 'markdown-do)
  (setq markdown-command "pandoc -t html5")
  (setq markdown-fontify-code-blocks-natively t)

  ;; When a link is pasted with an active selection, convert to a markdown link
  (defun meain/paste-after-or-create-link (from to)
    (interactive "r")
    (let ((clipboard-text (substring-no-properties (current-kill 0 t))))
      (if (ffap-url-p clipboard-text)
          (save-excursion
            (goto-char from)
            (insert "[")
            (goto-char (1+ to))
            (insert "](")
            (yank)
            (insert ")"))
        (delete-region from to)
        (yank))))
  (evil-define-key 'visual markdown-mode-map "p" 'meain/paste-after-or-create-link)

  ;; Quickly add markdown links to document
  (defun meain/markdown-linkify-thing (start end)
    "Function to search and add markdown links to document.
START and END for position."
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

  ;; Generate pdf from markdown document
  (defun meain/markdown-pdf ()
    "Generate pdf from markdown document."
    (interactive)
    (message "Generating pdf of %s. Just give it a moment.." (buffer-file-name))
    (start-process-shell-command "*markdown-pdf*" "*markdown-pdf*"
                                 (concat ",markdown-to-pdf " (buffer-file-name))))

  (defun meain/markdown-html ()
    "Generate pdf from markdown document."
    (interactive)
    (message "Generating markdown for %s. Just give it a moment.." (buffer-file-name))
    (start-process-shell-command "*markdown-html*" "*markdown-html*"
                                 (concat ",markdown-to-html " (buffer-file-name))))

  ;; Run markdown code blocks
  ;; Possible alternative https://github.com/md-babel/md-babel.el
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
            (insert "\n```\n")
            (insert (shell-command-to-string (format "%s '%s'" snippet-runner temp-source-file)))
            (insert "```"))
        (with-current-buffer (get-buffer-create "*markdown-runner-output*")
          (erase-buffer)
          (insert (shell-command-to-string (format "%s '%s'" snippet-runner temp-source-file)))
          (switch-to-buffer (current-buffer))))
      (delete-file temp-source-file t))))

(use-package go-dlv
  :ensure t
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
            (meain/copy-to-clipboard dlv-command)
            (dlv dlv-command))
        (call-interactively 'dlv))))
  :commands (dlv dlv-current-func meain/dlv meain/dlv-replay meain/dlv-current-func))

(use-package csv-mode
  :ensure t
  :defer t
  :config
  ;; https://www.emacswiki.org/emacs/CsvMode
  (require 'cl)
  (require 'color)
  (defun meain/csv-highlight (&optional separator)
    (interactive (list (when current-prefix-arg (read-char "Separator: "))))
    (font-lock-mode 1)
    (let* ((separator (or separator ?\,))
           (n (count-matches (string separator) (point-at-bol) (point-at-eol)))
           (colors (loop for i from 0 to 1.0 by (/ 2.0 n)
                         collect (apply #'color-rgb-to-hex
                                        (color-hsl-to-rgb i 0.3 0.5)))))
      (loop for i from 2 to n by 2
            for c in colors
            for r = (format "^\\([^%c\n]+%c\\)\\{%d\\}" separator separator i)
            do (font-lock-add-keywords nil `((,r (1 '(face (:foreground ,c)))))))))

  (add-hook 'csv-mode-hook 'meain/csv-highlight)
  (add-hook 'csv-mode-hook 'csv-align-mode)
  (add-hook 'csv-mode-hook '(lambda () (interactive) (toggle-truncate-lines nil)))

  (set-face-attribute 'csv-separator-face nil
                      :background "gray100"
                      :foreground "#000000"))

;; Just the syntax files for cedar mode
(use-package cedar-mode :load-path "/Users/meain/dev/src/cedar-mode")

(use-package emacs
  :config
  (add-hook 'nxml-mode-hook
            (lambda ()
              (define-key nxml-mode-map (kbd "M-l") 'meain/move-swap-right)
              (define-key nxml-mode-map (kbd "M-h") 'meain/move-swap-left)
              (define-key nxml-mode-map (kbd "M-k") 'meain/move-swap-up)
              (define-key nxml-mode-map (kbd "M-j") 'meain/move-swap-down))))
(use-package json-mode :ensure t :defer t)
(use-package yaml-mode :ensure t :defer t)
(use-package yaml-ts-mode
  :after (tree-surgeon)
  :config
  (add-hook 'yaml-ts-mode-hook
            (lambda ()
              (setq imenu-create-index-function
                    (tree-surgeon-kv-imenu-index-function 'yaml
                                                          "(block_mapping_pair key: (flow_node)) @body"
                                                          "key: (flow_node) @key")))))
(use-package json-ts-mode
  :after (tree-surgeon)
  :config
  (add-hook 'json-ts-mode-hook
            (lambda ()
              (setq imenu-create-index-function
                    (tree-surgeon-kv-imenu-index-function 'json
                                                          "(pair key: (string)) @body"
                                                          "key: (string (string_content) @key)")))))
(use-package ini-mode :ensure t :defer t)
(use-package dockerfile-mode :ensure t :defer t :mode "/Dockerfile")
(use-package docker-compose-mode :ensure t :defer t)
(use-package protobuf-mode :ensure t :defer t :disabled t)
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

;; for kmonad files
(use-package kbd-mode
  :defer t
  :mode "\\.kbd\\'"
  :ensure (kbd-mode :host github
                    :repo "kmonad/kbd-mode"))

;; Show metadata for binary files instead of opening them
(use-package eff
  :defer t
  :ensure (:host github :repo "oxidase/eff-mode"))

;; mtodo-mode
(use-package emacs
  :after evil
  :disabled t
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
  :ensure (dape :type git :host github :repo "svaante/dape")
  :disabled t
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
  :commands (gud-break gud-cont)
  :init
  (define-key evil-normal-state-map (kbd "<SPC> d r") 'gud-reset)
  (define-key evil-normal-state-map (kbd "<SPC> d b") 'gud-break)
  (define-key evil-normal-state-map (kbd "<SPC> d c") 'gud-cont)
  (define-key evil-normal-state-map (kbd "<SPC> d n") 'gud-next)
  (define-key evil-normal-state-map (kbd "<SPC> d s") 'gud-step)
  (define-key evil-normal-state-map (kbd "<SPC> d u") 'gud-up)
  (define-key evil-normal-state-map (kbd "<SPC> d g") 'gud-until))

;; Winner mode
(use-package winner
  :defer nil
  :config
  (global-set-key (kbd "M-f <left>") 'winner-undo)
  (global-set-key (kbd "M-f <right>") 'winner-redo)
  (winner-mode))

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
  (setq gnus-secondary-select-methods '((nntp "news.gmane.io")))
  (add-hook 'gnus-group-mode-hook 'hl-line-mode)
  (add-hook 'gnus-summary-mode-hook 'hl-line-mode))

;; erc
(use-package erc
  :commands (erc)
  :config
  (setq erc-timestamp-format "[%I:%M %p]"))

;; command log
(use-package command-log-mode
  :commands global-command-log-mode
  :ensure t
  :init
  (defun meain/command-log-start ()
    "Enable command-log-mode and open command-log buffer."
    (interactive)
    (global-command-log-mode)
    (clm/open-command-log-buffer)))

;; Beacon mode
(use-package beacon
  :ensure t
  :defer t
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
  :ensure (ligature :host github
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
(use-package focus :ensure t :commands focus-mode)
;; Writing mode
(use-package writeroom-mode
  :ensure t
  :commands writeroom-mode
  :config
  (setq writeroom-global-effects (remove 'writeroom-set-fullscreen writeroom-global-effects)))
;; Naive linter for English prose
(use-package writegood-mode
  :ensure t
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
    (dired (concat "/ssh:" (meain/ssh-host-picker) ":"))))

;; tramp-term
(use-package tramp-term
  :after tramp
  :ensure t
  :commands (tramp-term meain/tramp-shell)
  :config
  (defun meain/tramp-shell ()
    "SSH into a server by selecting a host via autocomplete."
    (interactive)
    (tramp-term (list (meain/ssh-host-picker)))))

;; timing stuff
(use-package activity-watch-mode
  :ensure t
  :disabled t
  :defer t
  :config (global-activity-watch-mode))

;; Control bluetooth devices
(use-package bluetooth
  :ensure t
  :commands (bluetooth-list-devices))

(use-package emacs
  :commands (meain/kill-markdown-preview meain/markdown-preview)
  :config
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
                   ",markdown-preview" buffer-file-name)))

;; Restclient
;; Alternative: https://github.com/federicotdn/verb
(use-package restclient
  :ensure t
  :defer t
  :mode ("\\.rest\\'". restclient-mode)
  :config (add-hook 'restclient-mode-hook (lambda ()
                                            (setq imenu-generic-expression '((nil "^#+\s+.+" 0))))))

;; Restclient jq integration
(use-package restclient-jq
  :ensure t
  :after restclient
  :defer
  :init
  (add-hook 'restclient-mode-hook (lambda () (require 'restclient-jq))))

;; Link opening
(use-package ace-link
  :ensure t
  :commands ace-link
  :init (global-set-key (kbd "M-f l") 'ace-link))

;; Docker
(use-package docker
  :ensure t
  :defer t
  :commands (docker))

;; Kubernetes
(use-package kubernetes
  :ensure t
  :disabled t
  :defer t
  :commands (meain/kube)
  :config
  (defun meain/kube ()
    "Hacky function to load `kubernetes-evil' as it was not loading otherwise."
    (interactive)
    (use-package kubernetes-evil :ensure t)
    (kubernetes-overview)))

;; Window layout changer
(use-package rotate
  :ensure t
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

(use-package treesit
  :defer t
  :config
  (setq treesit-language-source-alist
        '((typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src"))
          (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))
          (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "master"))
          (jsdoc . ("https://github.com/tree-sitter/tree-sitter-jsdoc" "master"))
          (yaml . ("https://github.com/ikatyang/tree-sitter-yaml" "master"))
          (json . ("https://github.com/tree-sitter/tree-sitter-json" "master"))
          ;; (nix . ("https://github.com/nix-community/tree-sitter-nix" "master")) ;; no nix-ts-mode
          ;; method_spec was removed from upstream go grammar, but emacs treesit depends on it
          (go . ("https://github.com/meain/tree-sitter-go" "e395081"))
          (go-mod . ("https://github.com/camdencheek/tree-sitter-go-mod" "main"))
          (python . ("https://github.com/tree-sitter/tree-sitter-python"))))

  (defun meain/install-treesit-grammars ()
    (interactive)
    (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist)))

  ;; You can find out the function by running "strings" in the so or dylib
  (setq treesit-load-name-override-list '((go-mod "libtree-sitter-go-mod"  "tree_sitter_gomod")))

  (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode))

  (add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-ts-modmodee))
  (add-to-list 'auto-mode-alist '("\\.json\\'" . json-ts-mode))
  ;; (add-to-list 'auto-mode-alist '("\\.nix\\'" . nix-mode))

  (add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))
  (add-to-list 'major-mode-remap-alist '(go-mode . go-ts-mode)))

;; Some custom text objects based on treesitter
(use-package evil-textobj-tree-sitter
  :defer 1
  :load-path "/Users/meain/dev/src/evil-textobj-tree-sitter/"
  :after (evil)
  :config
  (defvar meain/tree-sitter-mappings '(("a" "parameter" ("parameter.inner") ("parameter.outer"))
                                       ("v" "conditional" ("conditional.inner" "loop.inner") ("conditional.outer" "loop.outer"))
                                       ("c" "class" ("class.inner") ("class.outer"))
                                       ("f" "function" ("function.inner") ("function.outer"))
                                       ("n" "comment" ("comment.outer") ("comment.outer"))))
  (dolist (mapping meain/tree-sitter-mappings)
    (let ((key (car mapping))
          (name (cadr mapping))
          (inner (caddr mapping))
          (outer (cadddr mapping)))
      ;; Need this weird `eval' here as `evil-textobj-tree-sitter-get-textobj' is a macro
      (eval `(define-key evil-inner-text-objects-map ,key (cons ,(concat "evil-inner-" name) (evil-textobj-tree-sitter-get-textobj ,inner))))
      (eval `(define-key evil-outer-text-objects-map ,key (cons ,(concat "evil-outer-" name) (evil-textobj-tree-sitter-get-textobj ,outer))))
      (define-key evil-normal-state-map (kbd (concat "]" key)) (cons (concat "goto-" name "-start") (lambda () (interactive) (evil-textobj-tree-sitter-goto-textobj outer))))
      (define-key evil-normal-state-map (kbd (concat "[" key)) (cons (concat "goto-" name "-start") (lambda () (interactive) (evil-textobj-tree-sitter-goto-textobj outer t))))
      (define-key evil-normal-state-map (kbd (concat "]" (upcase key))) (cons (concat "goto-" name "-end") (lambda () (interactive) (evil-textobj-tree-sitter-goto-textobj outer nil t))))
      (define-key evil-normal-state-map (kbd (concat "[" (upcase key))) (cons (concat "goto-" name "-end") (lambda () (interactive) (evil-textobj-tree-sitter-goto-textobj outer t t))))))

  (define-key evil-outer-text-objects-map "m"
              (evil-textobj-tree-sitter-get-textobj
                "import"
                '((python-mode . ((import_statement) @import))
                  (python-ts-mode . ((import_statement) @import))
                  (go-mode . ((import_spec) @import))
                  (go-ts-mode . ((import_spec) @import))
                  (rust-mode . ((use_declaration) @import))))))

;; Show context using tree-sitter
(use-package posframe-plus
  :defer t
  :ensure (:host github :type git :repo "zbelial/posframe-plus" ))
(use-package treesitter-context
  :after (tree-sitter posframe-plus)
  :ensure (:type git :host github :repo "zbelial/treesitter-context.el")
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
  :ensure (:repo "mickeynp/combobulate" :host github))

(use-package treesit-fold
  :defer t
  :after (evil-leader)
  :commands (treesit-fold-mode meain/toggle-fold)
  :ensure (treesit-fold :host github
                        :repo "emacs-tree-sitter/treesit-fold")
  :config
  (setq treesit-fold-line-count-show t)
  (setq treesit-fold-line-count-format "- %d lines -")

  (defun meain/toggle-fold ()
    (interactive)
    (if (equal treesit-primary-parser  nil)
        (call-interactively 'evil-toggle-fold)
      (call-interactively 'treesit-fold-toggle)))
  :init
  (define-key evil-normal-state-map (kbd "<SPC> TAB") 'meain/toggle-fold)
  (evil-leader/set-key "o" 'meain/toggle-fold))

;; Show scope info of block
;; (remove-overlays (point-min) (point-max))
(setq scopeline-load-path (concat (getenv "HOME") "/dev/src/scopeline.el"))
(use-package scopeline
  :commands (scopeline-mode)
  :load-path scopeline-load-path
  :config (setq scopeline-overlay-prefix " ~")
  :init (add-hook 'prog-mode-hook #'scopeline-mode))

;; Breadcrumb of current file/func
(use-package breadcrumb
  :ensure (:repo "joaotavora/breadcrumb" :host github)
  :config (breadcrumb-mode))

;; Quick lookup in a dictionary
(use-package dictionary
  :ensure t
  :commands (dictionary-search)
  :init
  (global-set-key (kbd "C-c d") #'dictionary-search)
  :config (setq dictionary-server "dict.org"))

;; Highlight enclosing parenthesis
(use-package highlight-parentheses
  :defer t
  :ensure t
  :init (add-hook 'prog-mode-hook #'highlight-parentheses-mode)
  :config (setq highlight-parentheses-colors '("coral1")))

;; Auto recompile on save (useful for running tests)
(use-package recompile-on-save
  :ensure t
  :commands (recompile-on-save-mode))

;; RFC reader
(use-package rfc-mode
  :ensure t
  :commands (rfc-mode-browse rfc-mode-read)
  :config
  (setq rfc-mode-directory (expand-file-name "~/.local/share/rfc/"))
  (add-hook 'rfc-mode-hook 'writeroom-mode))

(use-package scroll-on-drag
  :ensure t
  :disabled t
  :defer nil
  :config
  (setq scroll-on-drag-motion-scale 0.1)
  (global-set-key [down-mouse-2]
                  (lambda ()
                    (interactive)
                    (unless (scroll-on-drag)
                      (mouse-yank-primary t)))))

(use-package redacted
  :ensure t
  :commands (redacted-mode)
  :config (add-hook 'redacted-mode-hook (lambda () (read-only-mode (if redacted-mode 1 -1)))))

;; Mermaid mode
(use-package mermaid-mode :defer t :ensure t)

;; Edit any textfield in Emacs
(use-package emacs-everywhere
  :defer t
  :disabled t
  :ensure t
  :config
  (remove-hook 'emacs-everywhere-init-hooks #'emacs-everywhere-major-mode-org-or-markdown)
  (remove-hook 'emacs-everywhere-init-hooks #'emacs-everywhere-apply-major-mode)
  (add-hook 'emacs-everywhere-init-hooks #'gfm-mode))

;; Fontify face (useful to debug themes)
(use-package fontify-face :ensure t :defer t)

;; Keycast mode for demos
(use-package keycast
  :ensure t
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
(global-set-key (kbd "s-_") (ilambda text-scale-set 0)) ; s-0 is used by wm

;; Add keybindings to access important files.
(use-package emacs
  :after (evil-leader)
  :defer nil
  :config
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
(use-package emacs
  :commands meain/monacle-mode
  :config
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
  :init
  (global-set-key (kbd "M-f f") 'meain/monacle-mode))

;; Revert buffer quickly (fix for dealing with eglot loosing it)
(global-set-key (kbd "M-f r") (lambda ()
                                (interactive)
                                (save-buffer)
                                (revert-buffer-quick)))

;; Quick notes
(use-package emacs
  :after (evil-leader)
  :commands (meain/create-quick-note)
  :config
  (setq meain/quick-notes-templates-directory "~/dev/src/templates")
  (defun meain/create-quick-note ()
    (interactive)
    (let* ((templates (directory-files meain/quick-notes-templates-directory nil ".+\\..+"))
           (name-input (completing-read "Title: " templates nil nil))
           (is-template (member name-input templates))
           (extension (if is-template "" ; template would have the extension
                        (concat "." (completing-read "Extension: " '("md") nil nil))))
           (filename (concat meain/quick-notes-directory "/"
                             (format-time-string "%Y-%m/%d %H.%M " (current-time))
                             name-input extension)))
      (find-file filename)
      (when is-template
        (insert-file-contents (concat meain/quick-notes-templates-directory "/" name-input))
        (goto-char (point-min)))))
  :init
  (setq meain/quick-notes-directory "~/.local/share/vime/")
  (evil-leader/set-key "v"
    (alambda
     (project-switch-project meain/quick-notes-directory) ; TODO: sort files by timestamp
     (meain/create-quick-note))))

;; vim-printer remake in elisp
(use-package emacs
  :after evil
  :commands (meain/quick-print)
  :config
  (defun meain/quick-print ()
    "Quickly print the variable your cursor is under or the active region."
    (interactive)
    (let* ((thing-to-print (if (use-region-p)
                               (buffer-substring (region-beginning) (region-end))
                             (symbol-name (symbol-at-point))))
           (escaped-thing-to-print (string-replace "\"" "\\\"" thing-to-print)))
      (let* ((filename (car (reverse (string-split (buffer-file-name) "/"))))
             (prefix (format "%s:%s" filename (line-number-at-pos)))
             (template
              (pcase major-mode
                ((or 'emacs-lisp-mode 'lisp-interaction-mode) "(message \"{pfx} {esc}: %s\" {thing})")
                ((or 'rust-mode 'rust-ts-mode) "println!(\"{pfx} {esc}: {{:?}}\", {thing});")
                ((or 'go-mode 'go-ts-mode) "fmt.Println(\"{pfx} {esc}:\", {thing})")
                ((or 'lua-mode) "print(\"{pfx} {esc}:\", {thing})")
                ((or 'js-mode 'typescript-ts-mode 'web-mode) "console.log(\"{pfx} {esc}:\", {thing})")
                ((or 'shell-script-mode) "echo \"{pfx} {esc}:\" {thing}")
                ((or 'python-ts-mode 'python-mode) "print(\"{pfx} {esc}:\", {thing})")
                (_ (error "Unknown mode for quick-prin")))))
        (if current-prefix-arg
            (evil-open-above 1)
          (evil-open-below 1))
        (insert (string-replace
                 "{thing}" thing-to-print
                 (string-replace "{esc}" escaped-thing-to-print
                                 (string-replace "{pfx}" prefix template))))))
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

(use-package browse-url
  :config
  ;; Convert anything which looks like CP-<digits> to
  ;; https://veeam-vdc.atlassian.net/browse/CP-<digits>
  (defun meain/browse-jira (url &rest _)
    (let ((jira-id (replace-regexp-in-string "^http://\\(\\S+\\)" "\\1" url)))
      (browse-url (concat "https://veeam-vdc.atlassian.net/browse/" jira-id))))
  (setq browse-url-handlers
        '(("^http://CP-[0-9]+" . meain/browse-jira))))

(use-package auto-highlight-symbol
  :ensure t
  :commands (auto-highlight-symbol-mode)
  :init
  (add-hook 'prog-mode-hook #'auto-highlight-symbol-mode))

(use-package aidermacs
  :ensure (:host github :repo "MatthewZMD/aidermacs")
  :commands (aidermacs-transient-menu)
  :after (evil)
  :config
  (setq aidermacs-auto-commits nil)
  (setq aidermacs-use-architect-mode nil)
  (setq aidermacs-backend 'comint)
  :init
  (define-key evil-normal-state-map (kbd "<SPC> a") 'aidermacs-transient-menu))

;; Copilot, I guess
(use-package copilot
  :defer t
  :after jsonrpc
  :ensure (:host github
                 :repo "zerolfx/copilot.el"
                 :files ("dist" "*.el"))
  :config
  ;; https://github.com/copilot-emacs/copilot.el/issues/226
  (defun copilot-turn-on-unless-buffer-read-only ()
    "Turn on `copilot-mode' if the buffer is writable."
    (unless buffer-read-only (copilot-mode 1)))
  (add-hook 'text-mode-hook #'copilot-turn-on-unless-buffer-read-only)
  (add-hook 'prog-mode-hook #'copilot-turn-on-unless-buffer-read-only)

  (setq copilot-idle-delay 0)
  (setq copilot-max-char -1)

  ;; Suppress indentation warning from copilot
  ;; https://github.com/zerolfx/copilot.el/pull/212#issuecomment-1862487382
  (add-to-list 'warning-suppress-types '(copilot copilot-no-mode-indent))

  (define-key copilot-mode-map (kbd "M-f M-f") #'copilot-complete)
  (define-key copilot-mode-map (kbd "M-f M-j") #'copilot-next-completion)
  (define-key copilot-mode-map (kbd "M-f M-k") #'copilot-previous-completion)
  (define-key copilot-mode-map (kbd "M-f M-;") #'copilot-accept-completion-by-word)
  (define-key copilot-mode-map (kbd "M-f M-l") #'copilot-accept-completion))

;; Copilot chat
(use-package copilot-chat
  :defer t
  :after (request)
  :ensure (:host github :repo "chep/copilot-chat.el" :files ("*.el"))
  :config
  ;; From https://github.com/chep/copilot-chat.el/issues/24
  (defun meain/copilot-chat-display (prefix)
    "Opens the Copilot chat window, adding the current buffer to the context.

Called with a PREFIX, resets the context buffer list before opening"
    (interactive "P")

    (require 'copilot-chat)
    (let ((buf (current-buffer)))

      ;; Explicit reset before doing anything, avoid it resetting later on
      ;; target-fn and ignoring the added buffers
      (unless (copilot-chat--ready-p)
        (copilot-chat-reset))

      (when prefix (copilot-chat--clear-buffers))

      (copilot-chat--add-buffer buf)
      (copilot-chat-display))))

;; LLM chat interface
(use-package gptel
  :ensure t
  ;; :ensure (:host github :repo "karthink/gptel")
  :commands (gptel gptel-send gptel-rewrite-menu)
  :config
  (setq gptel-model 'gpt-4o-mini)
  (setq gptel-api-key openai-api-key)
  (setq gptel-expert-commands t)
  (setq gptel-use-tools t)

  (gptel-make-openai "Github Models"
    :host "models.inference.ai.azure.com"
    :endpoint "/chat/completions?api-version=2024-05-01-preview"
    :stream t
    :key github-models-api-key
    :models '(gpt-4o gpt-4o-mini))

  (gptel-make-openai "Groq"
    :host "api.groq.com"
    :endpoint "/openai/v1/chat/completions"
    :stream t
    :key groq-api-key
    :models '(llama-3.1-70b-versatile
              llama-3.1-8b-instant
              llama3-70b-8192
              llama3-8b-8192
              mixtral-8x7b-32768
              gemma-7b-it))

  (gptel-make-anthropic "Claude"
    :stream t
    :key anthropic-api-key)

  :init
  (defun gptel-context-clear-all ()
    (interactive)
    (gptel-add -1))
  (defun gptel-context-add-website (url &optional no-cache)
    "Add content from a website to the GPTel context.
URL is the website address to fetch content from.
When NO-CACHE is non-nil, force fetching fresh content even if cached."
    (interactive "sURL: ")
    (let ((buffer-name (format "*gptel-context-website:%s*" url)))
      (with-current-buffer (get-buffer-create buffer-name)
        (if (and (not no-cache) (> (buffer-size) 0))
            (gptel-add)
          (let* ((url-buffer (url-retrieve-synchronously url))
                 (content-without-header (with-current-buffer url-buffer
                                           (buffer-substring-no-properties
                                            (search-forward "\n\n")
                                            (point-max)))))
            (erase-buffer)
            (insert content-without-header)
            (gptel-add))))))
  (defun gptel-context-add-shell-command (command &optional cache)
    "Add context to gptel from the output of a shell command.
If CACHE is non-nil, the output is cached."
    (interactive "sCommand: ")
    (let ((buffer (get-buffer-create (format "*gptel-context-shell:%s*" command))))
      (with-current-buffer buffer
        (if cache
            (when (> (buffer-size) 0)
              (gptel-add))
          (erase-buffer)
          (insert (shell-command-to-string command))
          (gptel-add)))))
  (defun gptel-context-add-website-content (url &optional no-cache)
    "Add context to gptel from the readable content of a website URL.
For optional NO-CACHE, use caching by default."
    (interactive "sEnter website URL: ")
    (gptel-context-add-shell-command (format "readable %s" url) (not no-cache)))

  ;; https://github.com/karthink/gptel/wiki/Defining-custom-gptel-commands
  (defvar gptel-lookup--history nil)
  (defun gptel-lookup (prompt)
    "Ask ChatGPT for a response to PROMPT."
    (interactive (list (read-string "Ask ChatGPT: " nil gptel-lookup--history)))
    (when (string= prompt "") (user-error "A prompt is required"))
    (gptel-request
     prompt
     :callback
     (lambda (response info)
       (if (not response)
           (message "gptel-lookup failed with message: %s" (plist-get info :status))
         (with-current-buffer (get-buffer-create "*gptel-lookup*")
           (let ((inhibit-read-only t))
             (erase-buffer)
             (insert response))
           (special-mode)
           (display-buffer (current-buffer)
                           `((display-buffer-in-side-window)
                             (side . bottom)
                             (window-height . ,#'fit-window-to-buffer))))))))

  (global-set-key (kbd "M-f i m") 'gptel)
  (global-set-key (kbd "M-f i s") 'gptel-send)
  (global-set-key (kbd "M-f i p") 'gptel-lookup)
  (global-set-key (kbd "M-f i c") 'gptel-context-clear-all)
  (global-set-key (kbd "M-f i a") 'gptel-add)
  (global-set-key (kbd "M-f i i") 'gptel-menu)
  (global-set-key (kbd "M-f i r") 'gptel-rewrite))

(use-package gptel-quick
  :ensure (:host github :repo "karthink/gptel-quick")
  :commands (gptel-quick)
  :config
  (setq gptel-quick-timeout 100)
  :init
  (global-set-key (kbd "M-f i j") 'gptel-quick))

(use-package llm :ensure t)
(use-package yap
  :load-path "/Users/meain/dev/src/yap"
  :after (llm)
  :config
  (setq yap-service "github")
  (setq yap-model "gpt-4o-mini") ; start with something cheap

  (setq yap-api-key:groq groq-api-key)
  (setq yap-api-key:openrouter openrouter-api-key)
  (setq yap-api-key:github github-models-api-key)
  (setq yap-api-key:openai openai-api-key)
  (setq yap-api-key:anthropic anthropic-api-key)
  (setq yap-log-requests "/Users/meain/.cache/yap")

  ;; Add window rules for *yap-response* buffer so that it shows up at
  ;; top of the frame
  (add-to-list 'display-buffer-alist
               `(,(rx bos "*yap-response*" eos)
                 (display-buffer-reuse-window
                  display-buffer-in-side-window)
                 (reusable-frames . visible)
                 (side            . top)
                 (window-height   . 0.3)))

  (defun meain/yap-pick-model ()
    "Pick a model from a list of preferred models."
    (interactive)
    (let* ((models '(("github:4o-mini" . ("github" "gpt-4o-mini"))
                     ("github:4o" . ("github" "gpt-4o"))
                     ("github:o3-mini" . ("github" "o3-mini"))
                     ;; ("github:deepseekV3" . ("github" "DeepSeek-V3"))
                     ;; ("github:o1-mini" . ("github" "o1-mini"))
                     ;; ("github:o1-preview" . ("github" "o1-preview"))
                     ("groq:llama4-scout" . ("groq" "meta-llama/llama-4-scout-17b-16e-instruct"))
                     ("groq:llama-deepseek-r1" . ("groq" "deepseek-r1-distill-llama-70b"))
                     ("openrouter:qwen2.5-coder-32b" . ("openrouter" "qwen/qwen-2.5-coder-32b-instruct"))
                     ("openrouter:deepseek-v3" . ("openrouter" "deepseek/deepseek-chat-v3-0324:free"))
                     ("openrouter:deepseek-r1" . ("openrouter" "deepseek/deepseek-r1:free"))
                     ("openrouter:flash" . ("openrouter" "google/gemini-2.0-flash-exp:free"))
                     ("openrouter:gemini-2. 5" . ("openrouter" "google/gemini-2.5-pro-exp-03-25:free"))
                     ;; ("ollama:llama3.2" . ("ollama" "llama3.2:3b-instruct-q8_0"))
                     ;; ("ollama:qwen2.5-coder-3b" . ("ollama" "qwen2.5-coder:3b-instruct-q8_0"))
                     ;; ("ollama:gemma" . ("ollama" "gemma:2b-instruct-q8_0"))
                     ;; ("ollama:macro-o1" . ("ollama" "marco-o1:7b-q8_0"))
                     ("anthropic:3.5sonnet" . ("anthropic" "claude-3-5-sonnet-latest"))
                     ("anthropic:3.7sonnet" . ("anthropic" "claude-3-7-sonnet-latest"))
                     ("anthropic:3.5haiku" . ("anthropic" "claude-3-5-haiku-latest"))))
           (name (completing-read "Model: " models nil t))
           (vals (cdr (assoc name models))))
      (when vals
        (setq yap-llm-provider-override nil)
        (setq yap-service (car vals))
        (setq yap-model (cadr vals)))))

  (defun meain/yap-use-openrouter-free ()
    (interactive)
    (let ((models (seq-filter (lambda (x) (string-suffix-p ":free" x))
                              (yap--get-models:openrouter))))
      (setq yap-llm-provider-override nil
            yap-service "openrouter"
            yap-model (completing-read "Model: " models))))

  (defun meain/yap-use-vscode-llm ()
    (interactive)
    (let ((vscode-llm (make-llm-openai-compatible
                       :chat-model "claude-3.5-sonnet"
                       :url "http://localhost:3838/v1")))
      (setq yap-service "vscode-llm"
            yap-model "v:claude-3.5-sonnet"
            yap-llm-provider-override vscode-llm)))

  (defun meain/yap-template-with-refer (prompt-type)
    "Enhance YAP templates with refer integration.
PROMPT-TYPE specifies the type of prompt to use ('rewrite or 'prompt)."
    (let ((prompt (read-string "Prompt: "))
          (buffer-content (or (yap--get-selected-text) "")))
      (yap-template-external-context
       (if (eq prompt-type 'rewrite)
           yap--default-system-prompt-for-rewrite
         yap--default-system-prompt-for-prompt)
       prompt
       (current-buffer)
       (with-temp-buffer
         (insert (concat prompt "\n" buffer-content))
         (shell-command-on-region
          (point-min) (point-max)
          "refer search --threshold 25 --format llm")
         (buffer-string)))))

  (add-to-list 'yap-templates '(yap-rewrite-with-refer . (lambda () (meain/yap-template-with-refer 'rewrite))))
  (add-to-list 'yap-templates '(yap-prompt-with-refer . (lambda () (meain/yap-template-with-refer 'prompt))))

  (defun meain/get-llm-prompt (name)
    "Get the prompt for NAME."
    (with-temp-buffer
      (insert-file-contents
       (concat user-emacs-directory "prompts/" name ".md"))
      (buffer-string)))

  (add-to-list
   'yap-templates
   '(identify-actionable-change . (lambda ()
                                    (yap-template-prompt (meain/get-llm-prompt "identify-actionable-change")))))

  (add-to-list
   'yap-templates
   '(perform-proposed-change . (lambda ()
                                 (yap-template-buffer-context
                                  (meain/get-llm-prompt "perform-proposed-change")
                                  (let ((proposal (read-string "Proposal (default: prev yap response): ")))
                                    (if (string= proposal "")
                                        (with-current-buffer "*yap-response*"
                                          (buffer-substring-no-properties (point-min) (point-max)))
                                      proposal))
                                  (current-buffer)))))

  (defun meain/select-enclosing-defun ()
    (unless (use-region-p)
      (let ((bounds (bounds-of-thing-at-point 'defun)))
        (when bounds
          (goto-char (car bounds))
          (push-mark (cdr bounds) t t)))))

  :init
  (global-unset-key (kbd "M-m"))
  (global-set-key (kbd "M-m M-c") 'yap-buffer-toggle)
  (global-set-key (kbd "M-m M-m") 'yap-prompt)
  (global-set-key (kbd "M-m M-r") 'yap-rewrite)
  (global-set-key (kbd "M-m M-w") 'yap-write)
  (global-set-key (kbd "M-m M-x M-m") (lambda () (interactive) (yap-prompt 'yap-prompt-with-refer)))
  (global-set-key (kbd "M-m M-x M-r") (lambda () (interactive) (yap-rewrite 'yap-rewrite-with-refer)))
  (global-set-key (kbd "M-m M-o") (lambda () (interactive) (yap-rewrite 'optimize-code)))
  (global-set-key (kbd "M-m M-i")
                  (lambda ()
                    (interactive)
                    (meain/select-enclosing-defun)
                    (yap-rewrite 'identify-actionable-change)))
  (global-set-key (kbd "M-m M-f") (lambda () (interactive) (yap-rewrite 'fix-diagnostic-error)))
  (global-set-key (kbd "M-m M-e") (lambda () (interactive) (yap-prompt 'explain-code))))

;; Text to speech stuff
;; Useful for reading out llm explanations
(use-package read-aloud
  :after evil
  :ensure (:host github :repo "gromnitsky/read-aloud.el")
  :commands (read-aloud-buf read-aloud-this)
  :init
  (define-key evil-normal-state-map (kbd "<SPC> r b") 'read-aloud-buf)
  (define-key evil-normal-state-map (kbd "<SPC> r r") 'read-aloud-this)
  (define-key evil-normal-state-map (kbd "<SPC> r s") 'read-aloud-stop)

  (define-key evil-visual-state-map (kbd "<SPC> r b") 'read-aloud-buf)
  (define-key evil-visual-state-map (kbd "<SPC> r r") 'read-aloud-this)
  (define-key evil-visual-state-map (kbd "<SPC> r s") 'read-aloud-stop)
  :config
  (setq read-aloud-engine "macos")
  (setq read-aloud-engines
        '("custom" (cmd ",speak" args nil)
          "macos" (cmd "/usr/bin/say" args ("-r" "250")))))

;; Bookmarks
(use-package bookmark
  :commands (bookmark-jump bookmark-set)
  :config
  (setq bookmark-save-flag 1)
  (setq bookmark-set-fringe-mark nil)
  (advice-add 'bookmark-jump :around #'meain/recenter-advice)
  (global-set-key (kbd "M-f m") 'bookmark-jump)
  (global-set-key (kbd "M-f M") 'bookmark-set))

;; File manipulations
(use-package emacs
  :commands (meain/delete-current-file meain/rename-current-file meain/move-current-file)
  :config
  ;; Delete current file
  (defun meain/delete-current-file ()
    "Delete current file and close buffer."
    (interactive)
    (delete-file (buffer-file-name))
    (meain/kill-current-buffer-unless-scratch))

  ;; Quick file rename
  (defun meain/rename-current-file ()
    "Rename current file in the same directory."
    (interactive)
    (let ((newname (read-string "New name: " (file-name-nondirectory (buffer-file-name)))))
      (rename-file (buffer-file-name) (concat (file-name-directory (buffer-file-name)) newname))
      (find-alternate-file (concat (file-name-directory (buffer-file-name)) newname))))

  (defun meain/move-current-file ()
    "Rename the current visiting file and switch buffer focus to it."
    (interactive)
    (if (null (buffer-file-name))
        (user-error "Buffer does not have a filename: %s" (current-buffer)))
    (let* ((prompt (format "Rename %s to: "
                           (file-name-nondirectory (buffer-file-name))))
           (new-filename (expand-file-name (read-file-name prompt))))
      (if (null (file-writable-p new-filename))
          (user-error "New file not writable: %s" new-filename))
      (rename-file (buffer-file-name) new-filename 1)
      (find-alternate-file new-filename)
      (message "Renamed to and now visiting: %s" (abbreviate-file-name new-filename)))))

;; Fix any escaped escape code in selection
(use-package emacs
  :commands (meain/fix-escapes)
  :config
  (defun meain/fix-escapes ()
    "Replace \\n to \n, \\t to \t and \\r to empty on selection."
    (interactive)
    (save-excursion
      (let ((start (region-beginning))
            (end (region-end)))
        (dolist (pair '(("\\n" . "\n") ("\\t" . "\t") ("\\r" . "")))
          (goto-char start)
          (while (search-forward (car pair) end t)
            (replace-match (cdr pair) nil t)))))))

(use-package emacs
  :config
  (defun meain/set-read-only-if-do-not-edit ()
    "Set the buffer to read-only if buffer contents has 'DO NOT EDIT' in it.
We limit the search to just top 10 lines so as to only check the header."
    (save-excursion
      (goto-char (point-min))
      (let ((content (buffer-substring (point) (line-end-position 10))))
        (when (and (not buffer-read-only)
                   (string-match "DO NOT EDIT" content))
          (read-only-mode 1)
          (message "Buffer seems to be generated. Set to read-only mode.")))))
  (add-hook 'find-file-hook 'meain/set-read-only-if-do-not-edit))

;; Copy stuff
(use-package emacs
  :commands (meain/copy-to-clipboard meain/copy-debugger-break-statement meain/copy-file-name-to-clipboard meain/copy-path-to-clipboard)
  :config
  (defun meain/copy-to-clipboard (message)
    "Copy `MESSAGE' into clipboard."
    (with-temp-buffer
      (insert message)
      (let ((deactivate-mark t))
        (call-process-region (point-min) (point-max) "pbcopy"))))

  (defun meain/copy-debugger-break-statement ()
    (interactive)
    (let ((file-name (buffer-file-name))
          (line-number (line-number-at-pos)))
      (meain/copy-to-clipboard (format "b %s:%s" file-name line-number))))

  (defun meain/copy-file-name-to-clipboard (&optional abs-path)
    "Copy the current filename into clipboard. Pass `ABS-PATH' if you need absolute path."
    (interactive "P")
    (let ((file-path (or (buffer-file-name) default-directory)))
      (if file-path
          (let ((copy-path (if abs-path file-path
                             (string-replace (car (project-roots (project-current))) "" file-path))))
            (meain/copy-to-clipboard copy-path)
            (message "Copied '%s' to the clipboard" copy-path))
        (message "No file associated with buffer"))))
  (defalias #'meain/copy-path-to-clipboard #'meain/copy-file-name-to-clipboard))

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

;; Open current file in Github
(use-package emacs
  :after evil-leader
  :commands (meain/github-url meain/github-pr-url)
  :config
  (defun meain/github-pr-url ()
    "Open the Github PR page for the current file and line."
    (interactive)
    (let* ((project-root (locate-dominating-file default-directory ".git")) ; Find the project root
           (relative-path (if project-root
                              (file-relative-name (buffer-file-name) project-root) ; Get relative path
                            (buffer-file-name)))) ; Fallback to absolute path
      (message "%s"
               (shell-command-to-string
                (format ",git-pr-for-line %s %s"
                        relative-path
                        (line-number-at-pos))))))

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

(use-package emacs
  :commands (meain/emacs-revert-all-project-buffers)
  :config
  (defun meain/emacs-revert-all-project-buffers ()
    "Revert all editable buffers belonging to the current project."
    (interactive)
    (seq-do
     (lambda (b)
       (when (buffer-local-value 'buffer-read-only b)
         (revert-buffer-quick b)))
     (project-buffers (project-current)))))

;; NOTE: Switch to find-sibling-file
;; Patterns for replacing filenames with (builtin option: find-sibling-file)
;; Sticking with custom version as we have an option to create the file if it does not exist
;; Example for find-sibling-file:
;; (cl-pushnew '("\\([^/]+\\)\\.el\\'" "\\1-test.el") find-sibling-rules :test #'equal)
;; (cl-pushnew '("\\([^/]+\\)-test\\.el\\'" "\\1.el") find-sibling-rules :test #'equal)
(use-package emacs
  :after (evil-leader)
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
                (car (cl-remove-if (lambda (x) (equal x nil))
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
(setq tree-surgeon-load-path (concat (getenv "HOME") "/dev/src/tree-surgeon"))
(use-package tree-surgeon
  :load-path tree-surgeon-load-path
  :after (evil-leader)
  :init (evil-leader/set-key "H j" 'tree-surgeon-split-join))

;; Just some hima testing code
(use-package emacs
  :commands (meain/reload-current-theme)
  :config
  (defun meain/reload-current-theme ()
    "Util to reload hima theme for debugging."
    (interactive)
    (message "%s" custom-enabled-themes)
    (let ((current-theme (car custom-enabled-themes)))
      (disable-theme current-theme)
      (load-theme current-theme t))))

(use-package which-func :commands (which-function))

;; Better modeline
(use-package mode-line-idle :ensure t :commands (mode-line-idle))
(defvar meain/modeline-project-color
  '(:eval
    (let* ((project-name (or (meain/project-name) ""))
           (hex-color (concat "#" (substring (md5 project-name) 0 6)))
           (rgb (color-name-to-rgb hex-color))
           (hsl (apply #'color-rgb-to-hsl rgb))
           (new-sat (min 0.3 (/ (nth 1 hsl) 2))) ; deccrease saturation
           (new-light (min 0.8 (* (nth 2 hsl) 2))) ; increase lightness
           (new-rgb (apply #'color-hsl-to-rgb (list (nth 0 hsl) new-sat new-light)))
           (new-hex (apply #'color-rgb-to-hex new-rgb)))
      ;; TODO: encode vcs worktree information
      (propertize (format " %s " project-name) ;; Alt: use █ with foreground color
                  'font-lock-face (list :background new-hex)))))
(defvar meain/modeline-filename
  '(:eval (list (if (eq buffer-file-name nil) ""
                  (concat (file-name-nondirectory
                           (directory-file-name
                            (file-name-directory (buffer-file-name)))) "/"))
                (propertize "%b"
                            'face (if (buffer-modified-p)
                                      'font-lock-string-face
                                    'font-lock-builtin-face)
                            'help-echo (buffer-file-name)))))

(defun meain/modeline-segment (expr)
  `(:eval (let ((value ,expr))
            (if value (propertize value 'face 'hima-simple-gray) ""))))
(defvar meain/modeline-vcs
  (meain/modeline-segment
   `(when-let (vc vc-mode) (concat " @" (substring vc 5)))))
(defvar meain/modeline-yap
  (meain/modeline-segment
   `(when (boundp 'yap-model) (concat " [" yap-model "]"))))

(setq-default
 mode-line-format
 (list
  '(:eval (mode-line-idle 0.3 meain/modeline-project-color "░"))
  '(:eval (if (eq 'emacs evil-state) "[E] " " ")) ;; vim or emacs mode
  meain/modeline-filename
  (propertize ":%l:%c")
  '(:eval (mode-line-idle 1.0 meain/modeline-vcs ""))
  '(:eval (mode-line-idle 1.0 meain/modeline-yap ""))
  '(:eval (if (boundp 'keycast-mode-line) keycast-mode-line))
  'mode-line-format-right-align
  '(:eval (if (boundp 'org-timer-mode-line-string) (concat org-timer-mode-line-string " ")))
  (propertize "%p") ;; position in file
  (propertize " %m ")
  " "))

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
