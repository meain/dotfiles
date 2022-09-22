;;; init -- meain's Emacs config -*- lexical-binding: t -*-

;;; Commentary:
;; Well, this is a vimmer's Emacs config.  Nothing fancy though.

;;; Code:

;;; [PACKAGE SETUP] =============================================

;; Basic setup
(setq user-mail-address "mail@meain.io" user-full-name "Abin Simon")

;; Setup straight.el
(setq straight-repository-branch "develop")
(defvar bootstrap-version)
(let ((bootstrap-file (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
                                    'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(setq use-package-verbose t)
(straight-use-package 'use-package)
(setq use-package-always-demand (getenv "LOAD_FULL_EMACS"))

;; Benchmark emacs startup (enable when necessary)
(use-package benchmark-init
  :straight t
  :disabled :config
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

;; Get proper PATH (not used as we are launching from shell)
(use-package exec-path-from-shell
  :straight t
  :config (exec-path-from-shell-initialize))

;;; [BASE EVIL] =================================================

;; Evil mode (set this up first)
(use-package evil
  :straight t

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

  (evil-set-command-property 'evil-visual-char :jump t)
  (evil-set-command-property 'evil-visual-line :jump t)
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

  (advice-add 'evil-jump-forward :around #'meain/recenter-advice)
  (advice-add 'evil-jump-backward :around #'meain/recenter-advice)
  (advice-add 'evil-search-next :around #'meain/recenter-top-advice)
  (advice-add 'evil-search-previous :around #'meain/recenter-top-advice))

;; Evil leader
(use-package evil-leader
  :straight t
  :config
  (global-evil-leader-mode)
  (evil-leader/set-leader "s"))

;; Some keybindings
(evil-leader/set-key "h l" 'find-library)

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

;; Tab settings
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

;; Delete visual selection when I start typing
(delete-selection-mode 1)

;; Meta-f keybinds (tmux memories)
(global-unset-key (kbd "M-f")) ; have to unset first

;; Disable visual line mode (this causes issues with $ and a few other things in evil)
(global-visual-line-mode -1)

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
    (set-frame-font (meain/get-font-prop font-name 'family))
    (setq-default line-spacing (meain/get-font-prop font-name 'line-spacing))))

;; Bell: audio -> visual
(setq visible-bell nil)
(setq ring-bell-function (lambda ()
                           (unless (memq this-command
                                         '(isearch-abort abort-recursive-edit exit-minibuffer keyboard-quit))
                             (invert-face 'mode-line)
                             (invert-face 'header-line)
                             (run-with-timer 0.1 nil 'invert-face 'mode-line)
                             (run-with-timer 0.1 nil 'invert-face 'header-line))))

;; Theme
(load-theme 'hima t)

;; Diminish
(use-package diminish
  :straight t
  :defer t
  :init
  (diminish 'eldoc-mode)
  (diminish 'auto-revert-mode))

;;; [BASIC BUILTINS] ===========================================

;; Use mouse to do some stuff when you are lazy
(context-menu-mode t)

;; Show open and closing brackets
(show-paren-mode t)
(setq show-paren-delay 0)
(setq show-paren-style 'parenthesis)

;; Keep files in sync with filesystem
(global-auto-revert-mode t)
(setq auto-revert-interval 2)
(setq auto-revert-check-vc-info t)
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose t)

;; Disable line wrapping
(setq-default truncate-lines 1)
(evil-leader/set-key "b w" 'toggle-truncate-lines)

;; auto-fill
(evil-leader/set-key "b F" 'auto-fill-mode)

;; Cursor blink
(blink-cursor-mode -1)

;; Follow symlinks for vc
(setq vc-follow-symlinks t)

;; auto-pair
(electric-pair-mode t)

;; Enable recentf
(use-package recentf
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
  :straight t
  :defer 1
  :diminish
  :config (evil-commentary-mode))

;; Evil surround
(use-package evil-surround
  :defer 1
  :straight t
  :config (global-evil-surround-mode 1))

;; Evil text objects
(use-package evil-textobj-line :straight t :defer 1)
(use-package evil-textobj-syntax :straight t :defer 1)
(use-package evil-indent-plus
  :straight t
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
  :straight t
  :commands (evil-numbers/inc-at-pt-incremental evil-numbers/dec-at-pt-incremental)
  :init
  ;; cannot directly use C-x (in use by emacs)
  (define-key evil-normal-state-map (kbd "g C-a") 'evil-numbers/inc-at-pt-incremental)
  (define-key evil-normal-state-map (kbd "g C-x") 'evil-numbers/dec-at-pt-incremental))

;; Save buffer
(define-key evil-normal-state-map (kbd "<SPC> <SPC>") 'evil-write)

;; Hit universal arg without ctrl
(evil-leader/set-key "u" 'universal-argument)
(global-set-key (kbd "M-u") 'universal-argument)

;; Auto resize windows (useful in go buffer, folks don't stop at 80)
(use-package golden-ratio
  :straight t
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
  :config
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
  (add-hook 'eshell-mode-hook (lambda ()
                                (setenv "TERM" "xterm-256color")
                                (define-key eshell-mode-map (kbd "M-l") 'meain/move-swap-right)
                                (define-key eshell-mode-map (kbd "M-h") 'meain/move-swap-left)
                                (define-key eshell-mode-map (kbd "M-k") 'meain/move-swap-up)
                                (define-key eshell-mode-map (kbd "M-j") 'meain/move-swap-down))))


;; ansi-term config
(use-package term
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
(evil-leader/set-key "a f" 'other-frame)

;; Easier C-c C-c
(evil-leader/set-key "i"
  '(lambda ()
     (interactive)
     (execute-kbd-macro (kbd "C-c C-c"))))

;; Remap macro recoring key
(define-key evil-normal-state-map "Q" 'evil-record-macro)

;; Eval region
(define-key evil-visual-state-map (kbd ";") (lambda ()
                                              (interactive)
                                              (call-interactively 'eval-region)
                                              (evil-force-normal-state)))

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
                        (car (split-string (shell-command-to-string ",mail-unread|wc -l") "\n"))
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
(define-key evil-normal-state-map (kbd "q") 'meain/kill-current-buffer-unless-scratch)

;; Y to y$
(defun meain/yank-till-line-end ()
  "Yank till end of line."
  (interactive)
  (evil-yank (point)
             ;; subtracting 1 for newline
             (- (save-excursion (forward-line) (point)) 1)))
(define-key evil-normal-state-map (kbd "Y") 'meain/yank-till-line-end)

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
(define-key evil-normal-state-map (kbd "<SPC> ;") (cons "replace in buffer" (meain/ilambda evil-ex "%s/")))
(define-key evil-visual-state-map (kbd "<SPC> ;") (cons "replace in buffer"(meain/ilambda evil-ex "'<,'>s/")))

;; Highlight yanked region
(defun meain/evil-yank-advice (orig-fn beg end &rest args)
  "Advice to be added to `evil-yank' to highlight yanked region.  Pass ORIG-FN, BEG, END, TYPE, ARGS."
  (pulse-momentary-highlight-region beg end 'mode-line)
  (apply orig-fn beg end args))
(advice-add 'evil-yank :around 'meain/evil-yank-advice)

;; Recompile binding
(use-package compile
  :commands (compile recompile)
  :config
  (setq compilation-scroll-output t)
  (evil-set-initial-state 'comint-mode 'normal)
  (defun meain/compilation-colorcode (_buffer string)
    "Change background color of compilation `_BUFFER' to red on failure."
    (unless (string-prefix-p "finished" string) ; Having color for success was distracting
      (face-remap-add-relative 'default 'diff-hl-delete)))
  (add-to-list 'compilation-finish-functions 'meain/compilation-colorcode)
  (add-to-list 'compilation-finish-functions (lambda (&rest _) (toggle-truncate-lines t)))
  (add-to-list 'compilation-finish-functions (lambda (&rest _) (highlight-regexp "FAIL: .*" 'diff-refine-removed)))
  (defun meain/recompile-or-compile (&optional arg)
    "Compile or recompile based on universal `ARG'."
    (interactive "P")
    (if arg
        (call-interactively 'compile)
      (compile compile-command t)))
  :init
  (evil-leader/set-key "r" 'meain/recompile-or-compile))

;; Simplify how Async Shell Command buffers get displayed
;; (add-to-list 'display-buffer-alist
;;   '("\\*Async Shell Command\\*.*" display-buffer-no-window))
(add-to-list 'display-buffer-alist
             '("\\*Async Shell Command\\*.*"
               (display-buffer-reuse-window display-buffer-at-bottom)
               (reusable-frames . visible)
               (window-height . 0.1)))

;;; [OTHER PACKAGES] =============================================

;; project
(use-package project
  :defer t
  :commands (project-switch-project project-find-file project-roots project-current)
  :config
  (setq project-switch-commands 'project-find-file)
  (defun meain/project-name ()
    (file-name-nondirectory (directory-file-name (car (project-roots (project-current))))))
  :init
  (evil-leader/set-key "p p"
    (meain/with-alternate (call-interactively 'project-switch-project)
                          (project-find-file)))
  (define-key evil-normal-state-map (kbd "<RET>") 'project-find-file))

;; eldoc load
(use-package eldoc
  :defer t
  :config
  (setq eldoc-echo-area-use-multiline-p nil)
  (global-eldoc-mode nil))

;; dired
(use-package dired
  :defer t
  :config
  (require 'dired-x) ;; for dired-omit-files
  (setq delete-by-moving-to-trash t)
  (setq trash-directory "~/.Trash")
  (setq dired-listing-switches "-AGFhlgo")
  (setq dired-dwim-target t)
  ;; (setq dired-kill-when-opening-new-dired-buffer t)
  (define-key dired-mode-map (kbd "-") 'dired-up-directory)
  (setq dired-omit-files "\\.DS_Store$\\|__pycache__$\\|.pytest_cache$\\|\\.mypy_cache$\\|\\.egg-info$")
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
    (when-let ((_ (require 'vc))
               (ignores (magit-ignored-files))
               (exts (make-local-variable 'completion-ignored-extensions)))
      (dolist (item ignores) (add-to-list exts item))))
  (add-hook 'dired-mode-hook #'dired-dim-git-ignores))

;; Github like git info in dired
(use-package dired-git-info
  :straight t
  :after dired
  :commands (dired-git-info-mode))

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
  :straight t
  ;; can't defer loading of this as we need it in every single spawned
  ;; buffer including scratch
  :init (add-hook 'find-file-hook #'aas-activate-for-major-mode)
  :config
  (aas-global-mode)
  (defun meain/go-default-returns (type errformat)
    "Making it a function instead of an alist so that we can handle unknown TYPE."
    (pcase type
      ("error" errformat)
      ("bool" "false")
      ("string" "\"\"")
      ("byte" "0") ("rune" "0")
      ("int" "0") ("int32" "0") ("int64" "0")
      ("float32" "0.0") ("float64" "0.0")
      ("chan" "nil")
      ("interface" "nil")
      ("map" "nil")
      ("func" "nil")
      ((pred (string-prefix-p "<-")) "nil")
      ((pred (string-prefix-p "[")) "nil")
      ((pred (string-match " ")) nil) ; for situations with return name
      ((pred (string-prefix-p "*")) (concat (replace-regexp-in-string "\*" "&" type) "{}"))
      (_ (concat type "{}"))))
  (defun meain/go-return-string (errformat)
    "Get return string for go by looking up the return type of current func."
    (let* ((f-declaration (tree-sitter-node-at-pos 'function_declaration))
           (m-declaration (tree-sitter-node-at-pos 'method_declaration))
           (func-node (if (eq f-declaration nil) m-declaration f-declaration))
           (return-node (tsc-get-child-by-field func-node ':result)))
      ;; remove extra whitespace if nothing at end
      (replace-regexp-in-string " $"
                                ""
                                (concat "return "
                                        (if return-node
                                            (let ((return-node-type (tsc-node-type return-node))
                                                  (return-node-text (tsc-node-text return-node)))
                                              (pcase return-node-type
                                                ('parameter_list
                                                 (string-join (remove-if #'null
                                                                         (mapcar (lambda (x) (meain/go-default-returns x errformat))
                                                                                 (mapcar 'string-trim
                                                                                         ;; TODO: maybe use ts to find actual type nodes
                                                                                         (split-string (string-trim return-node-text "(" ")")
                                                                                                       ","))))
                                                              ", "))
                                                (_ (meain/go-default-returns return-node-text errformat)))))))))
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
    ";time" (lambda () (interactive) (insert (format-time-string "%T")))
    ";filename" (lambda () (interactive) (insert (file-name-nondirectory (buffer-file-name)))))
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
  (aas-set-snippets 'go-mode
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
    ";ie"
    (lambda ()
      (interactive)
      (insert (concat "if err != nil { fmt.Println(\"" (read-string "Error message: ") "\", err) }")))
    ";er"
    (lambda ()
      (interactive)
      (insert (concat "if err != nil { " (meain/go-return-string "err") " }")))
    ";tr"
    (lambda ()
      (interactive)
      (let ((left (read-string "Left: "))
            (right (read-string "Right: "))
            (thing (read-string "Incorrect thing: ")))
        (insert (concat "if " left " != " right "{ t.Errorf(\"incorrect " thing "; expected '%v', got '%v'\", " right " , " left ")}"))))
    ";ec"
    (lambda ()
      (interactive)
      (insert (concat "if err != nil { "
                      (meain/go-return-string
                       (concat "fmt.Errorf(\""
                               (read-string "Error message: ")
                               "; %v\", err)"))
                      " }")))
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
  :straight t
  :disabled t
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
  :straight t
  :after flyspell
  :commands (flyspell-correct-wrapper flyspell-goto-next-error)
  :bind (:map flyspell-mode-map ("C-:" . flyspell-correct-wrapper)))

;; Advanced spell checking
(use-package wucuo
  :straight t
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
  :defer 1
  :config
  (add-hook 'find-file-hook 'flymake-find-file-hook)
  (evil-set-command-property 'flymake-goto-next-error :jump t)
  (evil-set-command-property 'flymake-goto-prev-error :jump t))
(use-package flymake-diagnostic-at-point
  :straight t
  :after flymake
  :config
  (setq flymake-diagnostic-at-point-error-prefix "! ")
  (setq flymake-diagnostic-at-point-display-diagnostic-function 'flymake-diagnostic-at-point-display-minibuffer)
  (evil-leader/set-key "j" 'flymake-goto-next-error)
  (evil-leader/set-key "k" 'flymake-goto-prev-error)
  (add-hook 'flymake-mode-hook #'flymake-diagnostic-at-point-mode))
(use-package flymake-quickdef
  :straight t
  :after flymake
  :config
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
  :straight t
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
  :straight t
  :disabled t
  :defer t
  :after (consult company)
  :commands (consult-company))

;; Company quickhelp
(use-package company-quickhelp ; Show help in tooltip
  :straight t
  :disabled t
  :after company
  :config
  (company-quickhelp-mode)
  (setq pos-tip-foreground-color "#000000"
        pos-tip-background-color "#ffffff"))

(use-package corfu
  :straight t
  :config
  (setq completion-cycle-threshold 3)
  (setq corfu-auto t)
  (setq corfu-cycle t)
  (setq corfu-auto-delay .2)
  (setq corfu-auto-prefix 2)
  (setq corfu-history-mode t)
  (setq corfu-count 5)
  (define-key corfu-map (kbd "RET") 'newline-and-indent) ; default: corfu-insert

  (defun corfu-move-to-minibuffer ()
    "Move completion to minibuffer instead of corfu."
    (interactive)
    (let ((completion-extra-properties corfu--extra)
          completion-cycle-threshold completion-cycling)
      (apply #'consult-completion-in-region completion-in-region--data)))
  (define-key corfu-map "\M-m" #'corfu-move-to-minibuffer)

  (global-corfu-mode))

(use-package corfu-doc
  :straight t
  :after (corfu)
  :config
  ;; (add-hook 'corfu-mode-hook #'corfu-doc-mode)
  (define-key corfu-map (kbd "M-p") #'corfu-doc-scroll-down) ;; corfu-next
  (define-key corfu-map (kbd "M-n") #'corfu-doc-scroll-up)  ;; corfu-previous
  (define-key corfu-map (kbd "M-d") #'corfu-doc-toggle))

;; Add completion extensions
(use-package cape
  :straight t
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
  :straight (vertico
             :host github :repo "minad/vertico"
             :files (:defaults "extensions/*.el"))
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
          (consult-imenu buffer)
          (xref-find-references buffer)
          (meain/imenu-or-eglot buffer)
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
  :straight t
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
  :straight t
  :defer 1
  :bind (:map minibuffer-local-map ("C-b" . marginalia-cycle))
  :config (marginalia-mode))

;; Consult without consultation fees
(use-package consult
  :straight t
  :after (xref)
  :defer 1
  :config
  (setq consult-ripgrep-args "rg --null --line-buffered --color=never --max-columns=1000 --path-separator /\
      --smart-case --no-heading --line-number --hidden --follow --glob \"!.git/*\" .")
  (setq xref-show-xrefs-function #'consult-xref)
  (setq xref-show-definitions-function #'consult-xref)
  (evil-set-command-property 'consult-imenu :jump t))

;; Embark stuff
(use-package embark
  :defer 1
  :straight t
  :init (setq prefix-help-command #'embark-prefix-help-command)
  :config
  (global-set-key (kbd "C-'")  'embark-act)
  (global-set-key (kbd "C-.")  'embark-dwim)
  (global-set-key (kbd "C-h B")  'embark-bindings))
(use-package embark-consult
  :straight t
  :defer t
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode))

;; Helpful package
(use-package helpful
  :straight t
  :commands (helpful-callable helpful-variable helpful-at-point helpful-key)
  :init
  (evil-leader/set-key "h p" 'helpful-at-point)
  (evil-leader/set-key "h k" 'helpful-key)
  (evil-leader/set-key "h f" 'helpful-function)
  (evil-leader/set-key "h v" 'helpful-variable)
  (evil-leader/set-key "h o" 'helpful-symbol))

;; ibuffer
(use-package ibuffer
  :commands (ibuffer ibuffer-other-window)
  :init
  (setq ibuffer-expert t)
  (global-set-key (kbd "M-c")
                  (meain/with-alternate (call-interactively 'switch-to-buffer)
                                        (ibuffer-other-window))))
(use-package ibuffer-project
  :straight t
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
  :straight t
  :commands rg
  :init
  (evil-leader/set-key "F"
    (meain/with-alternate (consult-ripgrep) (call-interactively 'rg)))
  :config (setq rg-command-line-flags '("--hidden" "--follow")))

;; dumb-jump
(use-package dumb-jump
  :straight t
  :commands dumb-jumb-go
  :init (evil-leader/set-key "J" 'dumb-jump-go)
  :config
  (advice-add 'dumb-jump-go :around #'meain/recenter-advice)
  (evil-set-command-property 'dumb-jumb-go :jump t))


;; Code formatting
(use-package format-all
  :straight t
  :commands (format-all-buffer format-all-ensure-formatter)
  :config
  (define-format-all-formatter
   fixjson ; Use fixjson for formatting json files
   (:executable "fixjson")
   (:install "npm i -g fixjson")
   (:languages "JSON")
   (:features)
   (:format (format-all--buffer-easy executable)))
  (setq-default format-all-formatters '(("HTML" prettier) ("Go" goimports) ("JSON" fixjson) ("Nix" nixpkgs-fmt) ("Shell" shfmt)))
  :init
  (define-key evil-normal-state-map (kbd ",,") '(lambda () (interactive)
                                                  (format-all-ensure-formatter)
                                                  (if tree-sitter-mode
                                                      (tree-sitter-save-excursion
                                                        (format-all-buffer))
                                                    (format-all-buffer)))))

;; Xref customization
(use-package xref
  :config
  ;; (setq xref-show-definitions-function 'xref-show-definitions-completing-read)
  (setq xref-auto-jump-to-first-xref 'move) ;; Use 'show to open it
  (setq xref-auto-jump-to-first-definition 'move))

;; LSP
(use-package eglot
  :commands eglot-ensure
  :straight t
  :after (project flymake)
  :config
  (add-to-list 'eglot-server-programs '(lua-mode . ("~/.luarocks/bin/lua-lsp")))
  ;; yaml-mode useful for github actions
  (add-to-list 'eglot-server-programs '(yaml-mode . ("yaml-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs '(json-mode . ("vscode-json-languageserver" "--stdio")))
  (add-to-list 'eglot-server-programs '(javascript-mode . ("typescript-language-server" "--stdio" "--tsserver-path" "/home/meain/.nix-profile/bin/tsserver")))
  (add-to-list 'eglot-server-programs '(web-mode . ("typescript-language-server" "--stdio" "--tsserver-path" "/home/meain/.nix-profile/bin/tsserver")))
  (add-to-list 'eglot-server-programs '(js-mode . ("typescript-language-server" "--stdio" "--tsserver-path" "/home/meain/.nix-profile/bin/tsserver")))
  (add-to-list 'eglot-server-programs '(typescipt-mode . ("typescript-language-server" "--stdio" "--tsserver-path" "/home/meain/.nix-profile/bin/tsserver")))
  ;; Can be enabled on fiction like things
  ;; (add-to-list 'eglot-server-programs '(markdown-mode . ("unified-language-server" "--parser=remark-parse" "--stdio")))
  (add-to-list 'eglot-server-programs '(markdown-mode . ("prosemd-lsp" "--stdio"))) ;; to be used in combination with flyspell
  (add-to-list 'eglot-server-programs '(rust-mode . ("rust-analyzer")))
  (add-to-list 'eglot-server-programs '(python-mode . ("pylsp")))
  (add-to-list 'eglot-server-programs '(nix-mode . ("rnix-lsp")))
  (setq-default eglot-workspace-configuration
                '((:json.schemas . [((:fileMatch . ["package.json"]) (:url . "https://json.schemastore.org/package.json"))])
                  (:gopls . ((staticcheck . t) (experimentalWorkspaceModule . t)))))
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
  (evil-define-key 'normal eglot-mode-map (kbd "K") 'eldoc-print-current-symbol-info)
  (evil-define-key 'normal eglot-mode-map (kbd "g d") 'xref-find-definitions)
  (evil-define-key 'normal eglot-mode-map (kbd "g D") 'eglot-find-implementation)
  (evil-define-key 'normal eglot-mode-map (kbd "g r") 'xref-find-references)
  (evil-define-key 'normal eglot-mode-map (kbd "g R") 'eglot-rename)
  (evil-define-key 'normal eglot-mode-map (kbd "g ,") 'eglot-format-buffer)
  (evil-define-key 'normal eglot-mode-map (kbd "g a") 'eglot-code-actions))

;; consult-eglot
(use-package consult-eglot
  :straight t
  :commands consult-eglot-symbols
  :after (imenu eglot)
  :config
  (advice-add 'consult-imenu :around #'meain/recenter-top-advice)
  :init
  (defun meain/imenu-or-eglot (&optional alternate)
    "Create a func to alternate between goto thingy stuff.
Giving it a name so that I can target it in vertico mode and make it use buffer."
    (interactive "P")
    (if alternate
        (consult-eglot-symbols)
      (consult-imenu)))
  (global-set-key (kbd "M-i") #'meain/imenu-or-eglot))

;; Tagbar alternative
(use-package imenu
  :straight t
  :defer t
  :after (consult)
  :commands imenu
  :config
  (setq imenu-auto-rescan t)
  (setq imenu-max-item-length 300)
  (global-set-key (kbd "M-i") 'consult-imenu))
(use-package flimenu
  :straight t
  :defer t
  :after imenu
  :config (flimenu-global-mode 1))
(use-package imenu-list
  :straight t
  :defer t
  :after (imenu consult)
  :commands imenu-list-smart-toggle
  :config
  (setq imenu-list-focus-after-activation t)
  (setq imenu-list-after-jump-hook nil)
  (setq imenu-list-auto-resize t))

;; Magit
(use-package magit
  :straight t
  :commands (magit-status magit-commit-create magit-ignored-files)
  :init
  (evil-leader/set-key "gg" 'magit-status)
  (evil-leader/set-key "gc" 'magit-commit-create)
  (evil-leader/set-key "gG" 'magit-show-commit)
  (evil-leader/set-key "gT" 'magit-log-trace-definition)
  :config
  (evil-define-key 'normal magit-status-mode-map (kbd ";") 'magit-stage)
  (evil-define-key 'visual magit-status-mode-map (kbd ";") 'magit-stage)

  ;; make <escape> quit(go back one level) in magit popups
  (define-key transient-map (kbd "<escape>") 'transient-quit-one)
  (setq magit-diff-refine-hunk (quote all))
  (define-key magit-mode-map (kbd "M-w") 'delete-window)
  (setq magit-completing-read-function #'completing-read))

;; Magit forge
(use-package forge :straight t :defer t :after magit)

;; Github review
(use-package github-review
  :straight t
  :defer t
  :after forge
  :commands (github-review-start github-review-forge-pr-at-point))

;; Diff hl
(use-package diff-hl
  :straight t
  :defer 1
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
  :straight t
  :commands (blamer-show-commit-info blamer-mode global-blamer-mode)
  :custom
  (blamer-idle-time 0.1)
  (blamer-min-offset 30)
  :config
  (set-face-attribute 'blamer-face nil :height 0.9)
  (setq blamer-max-commit-message-length 90)
  (setq blamer-border-lines '(?+ ?- ?+ ?| ?+ ?+ )) ;; default one creates issues with spacing
  :init (evil-leader/set-key "G" 'blamer-show-commit-info))

;; Magit todo
(use-package magit-todos
  :straight t
  :defer 1
  :after (magit)
  :config
  (magit-todos-mode))

;; Matchit
(use-package evil-matchit
  :straight t
  :defer 1
  :config (global-evil-matchit-mode 1))

;; Highlight color codes
(use-package rainbow-mode
  :straight t
  :commands (rainbow-mode)
  :init (add-hook 'css-mode-hook 'rainbow-mode))

;; Code folding
(use-package origami
  :straight t
  :after evil
  :defer 1
  :config (global-origami-mode)
  :init
  (define-key evil-normal-state-map (kbd "<SPC> TAB") 'evil-toggle-fold)
  (evil-leader/set-key "o" 'evil-toggle-fold))

;; drag-stuff
(use-package drag-stuff
  :straight t
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
  :straight t
  :init
  (save-place-mode t)
  (setq save-place-file "~/.local/share/emacs/saveplace"))

;; Persistent undo using undo-tree
(use-package undo-tree
  :straight t
  :diminish
  :config
  (global-undo-tree-mode t)
  (setq undo-limit 80000000)
  (setq evil-want-fine-undo nil)
  (setq undo-tree-auto-save-history t)
  (setq undo-tree-history-directory-alist '(("." . "~/.local/share/emacs/undo"))))

;; Fancier tab managerment
(use-package tab-bar
  :straight t
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
         (t (tab-bar-switch-to-tab (car tabs))))))))

;; which-key mode
(use-package which-key
  :straight t
  :defer 1
  :diminish
  :config
  (which-key-mode))

;; Expand region
(use-package expand-region
  :straight t
  :commands (er/expand-region)
  :config
  ;; make evil jump list work with expand-region
  (evil-set-command-property 'er/expand-region :jump t)
  :init
  (global-set-key (kbd "M--") 'er/expand-region))

;; dtrt (atuo find indend setting)
(use-package dtrt-indent
  :straight t
  :defer 1
  :diminish
  :config (dtrt-indent-global-mode))

(use-package indent-guide
  :straight t
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
  :straight t
  :defer t
  :commands (vterm meain/shell-toggle)
  :init (global-set-key (kbd "M-;") 'meain/shell-toggle)
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
  (add-to-list 'display-buffer-alist
               '((lambda (bufname _)
                   (s-starts-with-p "*popup-shell" bufname))
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
      (vterm-send-string (concatenate 'string command ";exit 0"))
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
  :straight t
  :commands ranger
  :config
  (use-package image-dired+
    :straight t
    :config (image-diredx-async-mode)))

;; editorconfig
(use-package editorconfig
  :defer 1
  :straight t
  :config (editorconfig-mode 1))

;; eros for eval
(use-package eros
  :straight t
  :commands (eros-eval-last-sexp meain/eval-last-sexp)
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
  :straight t
  :commands venv-workon
  :init (setq venv-location "~/.local/share/virtual_envs"))

;; Quick run current test
(use-package emacs
  :after compile
  :commands (meain/test-runner meain/test-runner-full)
  :config
  ;; if available in another frame, don't recreate in current frame
  (push '("\\*compilation\\*" . (nil (reusable-frames . t))) display-buffer-alist)
  (defvar meain/test-runner-previous-command nil)
  (defvar meain/test-runner-run-previous-if-empty t)
  (defun meain/test-runner-full ()
    "Run the full test suite using toffee."
    (interactive)
    (compile (shell-command-to-string (format "toffee --full '%s'"
                                              (buffer-file-name)))))
  (defun meain/test-runner (&optional full-file)
    "Run the nearest test using toffee.  Pass `FULL-FILE' to run all test in file."
    (interactive "P")
    (let ((command (shell-command-to-string (if full-file
                                                (format "toffee '%s'" (buffer-file-name))
                                              (format "toffee '%s' '%s'"
                                                      (buffer-file-name) (line-number-at-pos))))))
      (if (not (s-starts-with-p "Unable to find any tests" command))
          (progn
            (setq meain/test-runner-previous-command command)
            (compile (concat "nice " command)))
        (if (and meain/test-runner-run-previous-if-empty meain/test-runner-previous-command)
            (progn
              (message "Could not find any tests, running previous test...")
              (compile (concat "nice " meain/test-runner-previous-command)))
          (message "Unable to find any tests")))))
  :init
  (evil-leader/set-key "d" 'meain/test-runner)
  (evil-leader/set-key "D" 'meain/test-runner-full))

;; Neotree
(use-package neotree
  :straight t
  :commands neotree
  :config
  (setq neo-window-fixed-size nil)
  (setq neo-theme 'classic)
  (add-hook 'neo-after-create-hook 'hl-line-mode))

;; Evil keybindings for a lot of things
(use-package evil-collection
  :defer 1
  :straight t
  :after evil
  :config
  (setq evil-collection-magit-want-horizontal-movement t)
  (setq evil-collection-magit-use-y-for-yank t)
  (evil-collection-init))

;; Highlight TODO items
(use-package hl-todo
  :straight t
  :defer 1
  :config
  (setq hl-todo-keyword-faces '(("TODO" . "#FF0000")
                                ("FIXME" . "#FF0000")
                                ("GOTCHA" . "#FF4500")
                                ("STUB" . "#1E90FF")
                                ("XXX" . "#AF0494")))
  (global-hl-todo-mode))

;; Emmet for html stuff (c-j to activate)
(use-package emmet-mode
  :straight t
  :defer t
  :commands (emmet-mode))

;; Direnv support
(use-package envrc
  :straight t
  :defer 1
  :config (envrc-global-mode))

;;; [FILETYPE PUGINS] ===============================================

(use-package rust-mode :straight t :defer t)
(use-package go-mode
  :straight t
  :defer t
  :config
  (evil-set-command-property 'godef-jump :jump t))
(use-package go-fill-struct
  :straight t
  :commands (go-fill-struct))
(use-package go-tag
  :straight t
  :commands (go-tag-add go-tag-remove go-tag-refresh)
  :config (setq go-tag-args (list "-transform" "camelcase")))
(use-package go-impl
  :straight t
  :commands (go-impl))
(use-package lua-mode :straight t :defer t)
(use-package web-mode :straight t :defer t)
(use-package jinja2-mode :straight t :defer t)
(use-package config-general-mode :straight t :defer t :mode "/\\.env")
(use-package vimrc-mode :straight t :defer t)
(use-package markdown-mode
  :straight t
  :defer t
  :mode ("\\.md\\'" . gfm-mode)
  :config
  (setq markdown-url-compose-char '(8230 8943 35 9733 9875))
  (use-package edit-indirect :straight t)
  (setq markdown-enable-html -1)
  (evil-define-key 'normal gfm-mode-map (kbd "<RET>") 'project-find-file)
  (evil-define-key 'normal gfm-mode-map (kbd "g d") 'markdown-do)
  (evil-define-key 'normal markdown-mode-map (kbd "<RET>") 'project-find-file)
  (evil-define-key 'normal markdown-mode-map (kbd "g d") 'markdown-do)
  (setq markdown-command "pandoc -t html5")
  (setq markdown-fontify-code-blocks-natively t))
(use-package nix-mode
  :straight t
  :defer t
  :mode "\\.nix\\'"
  :config
  (add-hook 'nix-mode-hook (lambda ()
                             (setq imenu-create-index-function #'meain/imenu-config-nesting-path))))
;; builtin package for scheme (for tree-sitter grammar)
(use-package scheme-mode :defer t :mode "\\.scm\\'")
(use-package csv-mode
  :straight t
  :defer t
  :config
  (setq csv-align-mode t)
  (set-face-attribute 'csv-separator-face nil
                      :background "gray100"
                      :foreground "#000000"))
(use-package json-mode
  :straight t
  :defer t
  :config
  (add-hook 'json-mode-hook (lambda ()
                              (setq imenu-create-index-function #'meain/imenu-config-nesting-path))))
(use-package yaml-mode
  :straight t
  :defer t
  :config
  (remove-hook 'yaml-mode-hook 'yaml-set-imenu-generic-expression) ;; don't use default one
  (add-hook 'yaml-mode-hook (lambda ()
                              (setq imenu-create-index-function #'meain/imenu-config-nesting-path))))
(use-package ini-mode :straight t :defer t)
(use-package dockerfile-mode :straight t :defer t :mode "/Dockerfile")
(use-package docker-compose-mode :straight t :defer t)
(use-package protobuf-mode :straight t :defer t)
(use-package org
  :commands (org-mode)
  :mode "/\\.org\\'"
  :config
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
  :straight t
  :after org
  :commands (org-modern-mode org-modern-agenda)
  :init
  (add-hook 'org-mode-hook #'org-modern-mode)
  (add-hook 'org-agenda-finalize-hook #'org-modern-agenda))

;; for kmonad files
(use-package kbd-mode
  :defer t
  :mode "\\.kbd\\'"
  :straight (kbd-mode :host github
                      :repo "kmonad/kbd-mode"))

;; mtodo-mode
(use-package emacs
  :config
  (load (expand-file-name "~/.config/emacs/mtodo-mode.el"))
  (add-hook 'mtodo-mode-hook (lambda ()
                               (setq imenu-generic-expression '((nil "^#+\s+.+" 0)))))
  (evil-define-key 'normal mtodo-mode-map (kbd "g d") 'mtodo-mark-done)
  (evil-define-key 'normal mtodo-mode-map (kbd "g m") 'mtodo-mark-undone)
  (evil-define-key 'normal mtodo-mode-map (kbd "g s") 'mtodo-mark-important))

;;; [EXTRA PLUGINS] =================================================

;; Dashboard
(use-package dashboard
  :disabled t
  :straight t
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

;; notmuch
(use-package notmuch
  :straight t
  :commands notmuch
  :init
  (evil-leader/set-key "a n" 'notmuch)
  :config
  (evil-define-key 'normal notmuch-search-mode-map (kbd "u") 'evil-collection-notmuch-search-toggle-unread)
  (evil-define-key 'normal notmuch-show-mode-map (kbd "U") 'notmuch-show-browse-urls)
  (evil-define-key 'normal notmuch-show-mode-map (kbd "u") 'meain/notmuch-show-close-all-but-unread)
  (evil-define-key 'normal notmuch-show-mode-map (kbd "M-k") 'meain/move-swap-up)
  (evil-define-key 'normal notmuch-show-mode-map (kbd "M-j") 'meain/move-swap-down)
  (evil-define-key 'normal notmuch-show-mode-map (kbd "M-s m f") 'meain/find-emails-from-same-sender)
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
        '((:name "Mails" :query "((tag:inbox AND tag:imbox) OR (tag:inbox AND tag:github) OR (tag:inbox AND tag:work)) AND -tag:flagged" :key "J" :sort-order newest-first)
          (:name "Imbox" :query "tag:imbox AND tag:inbox AND -tag:github AND -tag:flagged" :key "i" :sort-order newest-first)
          (:name "WInbox" :query "tag:work and tag:inbox" :key "I" :sort-order newest-first)
          (:name "Github" :query "tag:github AND tag:inbox" :key "h" :sort-order newest-first)
          (:name "Meain" :query "query=to:mail@meain.io AND tag:inbox" :key "m" :sort-order newest-first)
          (:name "Unread" :query "tag:unread AND tag:inbox AND -tag:python AND -tag:git AND -tag:newsletter AND -tag:jobhunt AND -tag:imbox AND -tag:work" :key "u" :sort-order newest-first)
          (:name "Python" :query "tag:python AND tag:inbox AND -tag:work" :key "p" :sort-order newest-first)
          (:name "Git" :query "tag:git AND tag:inbox AND -tag:work" :key "g" :sort-order newest-first)
          (:name "Newsletter" :query "tag:newsletter AND tag:inbox AND -tag:work" :key "n" :sort-order newest-first)
          (:name "Jobhunt" :query "tag:jobhunt AND tag:inbox AND -tag:work" :sort-order newest-first)
          (:name "Known" :query "tag:known AND tag:inbox AND -tag:todo AND -tag:flagged AND -tag:work" :key "k" :sort-order newest-first)
          (:name "Archiveable" :query "tag:bullshit AND tag:known AND tag:nonimportant AND tag:inbox AND -tag:work" :key "a" :sort-order newest-first)
          (:name "Todo" :query "tag:inbox AND tag:todo AND -tag:work" :key "t" :sort-order oldest-first)
          (:name "WTodo" :query "tag:inbox AND tag:todo AND tag:work" :key "T" :sort-order oldest-first)
          (:name "Flagged" :query "tag:flagged AND -tag:work" :key "f")
          (:name "WFlagged" :query "tag:flagged AND tag:work" :key "F")
          (:name "Watching" :query "tag:inbox AND tag:watch AND -tag:work" :key "w" :sort-order oldest-first)
          (:name "Read" :query "tag:inbox AND -tag:imbox AND -tag:newsletter AND -tag:python and -tag:unread AND -tag:jobhunt AND -tag:git AND -tag:todo AND -tag:flagged AND -tag:work" :key "r" :sort-order oldest-first)
          (:name "WRead" :query "tag:inbox AND -tag:imbox AND -tag:newsletter AND -tag:python and -tag:unread AND -tag:jobhunt AND -tag:git AND -tag:todo AND -tag:flagged AND tag:work" :key "R" :sort-order oldest-first)
          (:name "Sent" :query "tag:sent" :key "s" :sort-order newest-first)
          (:name "Drafts" :query "tag:draft AND tag:inbox" :key "d")
          (:name "All mail" :query "path:meain/**" :key "A" :sort-order newest-first)
          (:name "All work mail" :query "path:ic/**" :key "Z" :sort-order newest-first)))

  ;; sending emails
  (setq mail-signature t)
  (setq mail-signature-file "~/.config/datafiles/mailsignature")
  (setq message-kill-buffer-on-exit t) ; kill buffer after sending mail
  (setq mail-specify-envelope-from t) ; Settings to work with msmtp
  (setq message-sendmail-envelope-from 'header)
  (setq mail-envelope-from 'header)
  (setq notmuch-fcc-dirs "\"[Gmail].Sent Mail\" +sent -unread") ; stores sent mail to the specified directory
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
  (evil-define-key 'normal gnus-article-mode-map (kbd "P") 'gnus-summary-refer-parent-article)
  (evil-define-key 'normal gnus-summary-mode-map (kbd "P") 'gnus-summary-refer-parent-article)
  (setq gnus-select-method '(nnnil ""))
  (setq gnus-secondary-select-methods
        '((nntp "news.gmane.io")))
  (add-hook 'gnus-group-mode-hook 'hl-line-mode)
  (add-hook 'gnus-summary-mode-hook 'hl-line-mode))

;; elfeed
(use-package elfeed
  :straight t
  :commands (elfeed elfeed-update)
  :init
  ;; first run after 1 hour
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

;; command log
(use-package command-log-mode
  :commands global-command-log-mode
  :straight t
  :init
  (defun meain/command-log-start ()
    "Enable command-log-mode and open command-log buffer."
    (interactive)
    (global-command-log-mode)
    (clm/open-command-log-buffer)))

;; Beacon mode
(use-package beacon
  :straight t
  :defer 1
  :diminish
  :config
  (setq beacon-blink-when-window-scrolls t)
  (advice-add 'evil-forward-section-begin
              :after (lambda (&rest r) (beacon-blink) (recenter))
              '((name . "beacon-blink")))
  (advice-add 'evil-backward-section-begin
              :after (lambda (&rest r) (beacon-blink) (recenter))
              '((name . "beacon-blink")))
  (beacon-mode t))

;; Ligatures
(use-package ligature
  :defer 3
  :disabled t
  :straight (ligature :host github
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
(use-package focus :straight t :commands focus-mode)
;; Writing mode
(use-package writeroom-mode
  :straight t
  :commands writeroom-mode
  :config
  (setq writeroom-global-effects (remove 'writeroom-set-fullscreen writeroom-global-effects)))
;; Naive linter for English prose
(use-package writegood-mode
  :straight t
  :defer t
  :commands (writegood-mode))
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
(evil-leader/set-key "b W" 'meain/toggle-writing-mode)

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
    "Opern dired in a server by selecting a host via autocomplete."
    (interactive)
    (dired (concatenate 'string "/ssh:" (meain/ssh-host-picker) ":"))))

;; tramp-term
(use-package tramp-term
  :after tramp
  :straight t
  :commands (tramp-term meain/tramp-shell)
  :config
  (defun meain/tramp-shell ()
    "SSH into a server by selecting a host via autocomplete."
    (interactive)
    (tramp-term (list (meain/ssh-host-picker)))))

;; connect to docker via tramp
(use-package docker-tramp
  :straight t
  :defer t
  :after tramp)

;; timing stuff
(use-package activity-watch-mode
  :straight t
  :defer 1
  :diminish
  :config (global-activity-watch-mode))

;; Control bluetooth devices
(use-package bluetooth
  :straight t
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
  :straight t
  :defer t
  :mode ("\\.rest\\'". restclient-mode)
  :config (add-hook 'restclient-mode-hook (lambda ()
                                            (setq imenu-generic-expression '((nil "^#+\s+.+" 0))))))

;; Link opening
(use-package ace-link
  :straight t
  :commands ace-link
  :init (global-set-key (kbd "M-f l") 'ace-link))

;; Docker
(use-package docker
  :straight t
  :defer t
  :commands (docker))

;; Kubernetes
(use-package kubernetes
  :straight t
  :defer t
  :commands (meain/kube)
  :config
  (defun meain/kube ()
    "Hacky function to load `kubernetes-evil' as it was not loading otherwise."
    (interactive)
    (use-package kubernetes-evil :straight t)
    (kubernetes-overview)))

;; Window layout changer
(use-package rotate
  :straight t
  :commands (rotate-layout rotate-window)
  :config
  (define-key evil-normal-state-map (kbd "M-f <SPC>") 'rotate-layout))

;; Remember
(use-package remember
  :commands remember
  :config
  (setq remember-data-file "~/.config/emacs/remember-notes"
        remember-notes-initial-major-mode 'org-mode
        remember-notes-auto-save-visited-file-name t))

;; Tree sitter
(use-package tree-sitter
  :defer 1
  :straight t
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
                                                      (nix-mode . "(bind (attrpath (attr_identifier) @key)) @item")))
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
  :after tree-sitter
  :config
  (add-function :before-until tree-sitter-hl-face-mapping-function
                (lambda (capture-name)
                  (pcase capture-name
                    ("python.class.name" 'font-lock-function-name-face))))
  (add-hook 'python-mode-hook
            (lambda ()
              (tree-sitter-hl-add-patterns nil
                [(class_definition (identifier)
                                   @python.class.name)])))
  (add-function :before-until tree-sitter-hl-face-mapping-function
                (lambda (capture-name)
                  (pcase capture-name
                    ("python.docstring" 'tree-sitter-hl-face:doc))))
  (add-hook 'python-mode-hook
            (lambda ()
              (tree-sitter-hl-add-patterns nil
                [(function_definition (block (expression_statement (string)
                                                                   @python.docstring)))]))))

;; Some custom text objects based on treesitter
(use-package evil-textobj-tree-sitter
  :defer 1
  :load-path "/home/meain/dev/src/evil-textobj-tree-sitter/"
  :after tree-sitter
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
  (define-key evil-outer-text-objects-map "o" (cons "evil-outer-loop" (evil-textobj-tree-sitter-get-textobj "loop.outer")))
  (define-key evil-inner-text-objects-map "o" (cons "evil-inner-loop" (evil-textobj-tree-sitter-get-textobj "loop.inner")))
  (define-key evil-outer-text-objects-map "v" (cons "evil-outer-conditional" (evil-textobj-tree-sitter-get-textobj "conditional.outer")))
  (define-key evil-inner-text-objects-map "v" (cons "evil-inner-conditional" (evil-textobj-tree-sitter-get-textobj "conditional.inner")))
  (define-key evil-inner-text-objects-map "a" (cons "evil-inner-parameter" (evil-textobj-tree-sitter-get-textobj "parameter.inner")))
  (define-key evil-outer-text-objects-map "a" (cons "evil-outer-parameter" (evil-textobj-tree-sitter-get-textobj "parameter.outer")))

  (define-key evil-normal-state-map (kbd "]a") (cons "goto-parameter-start" (meain/ilambda evil-textobj-tree-sitter-goto-textobj "parameter.inner")))
  (define-key evil-normal-state-map (kbd "[a") (cons "goto-parameter-start" (meain/ilambda evil-textobj-tree-sitter-goto-textobj "parameter.inner" t)))
  (define-key evil-normal-state-map (kbd "]A") (cons "goto-parameter-end" (meain/ilambda evil-textobj-tree-sitter-goto-textobj "parameter.inner" nil t)))
  (define-key evil-normal-state-map (kbd "[A") (cons "goto-parameter-end" (meain/ilambda evil-textobj-tree-sitter-goto-textobj "parameter.inner" t t)))
  (define-key evil-normal-state-map (kbd "]v") (cons "goto-conditional-start" (meain/ilambda evil-textobj-tree-sitter-goto-textobj "conditional.outer")))
  (define-key evil-normal-state-map (kbd "[v") (cons "goto-conditional-start" (meain/ilambda evil-textobj-tree-sitter-goto-textobj "conditional.outer" t)))
  (define-key evil-normal-state-map (kbd "]V") (cons "goto-conditional-end" (meain/ilambda evil-textobj-tree-sitter-goto-textobj "conditional.outer" nil t)))
  (define-key evil-normal-state-map (kbd "[V") (cons "goto-conditional-end" (meain/ilambda evil-textobj-tree-sitter-goto-textobj "conditional.outer" t t)))
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

(use-package ts-fold
  :defer t
  :after (tree-sitter)
  :commands (ts-fold-mode)
  :straight (ts-fold :host github
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


;; Quick lookup in a dictionary
(use-package dictionary
  :straight t
  :commands (dictionary-search)
  :init
  (global-set-key (kbd "C-c d") #'dictionary-search)
  :config (setq dictionary-server "dict.org"))

;; Highlight enclosing parenthesis
(use-package highlight-parentheses
  :defer t
  :straight t
  :init (add-hook 'prog-mode-hook #'highlight-parentheses-mode)
  :config (setq highlight-parentheses-colors '("coral1")))

;; Auto recompile on save (useful for running tests)
(use-package recompile-on-save
  :straight t
  :commands (recompile-on-save-mode))

;; RFC reader
(use-package rfc-mode
  :straight t
  :commands (rfc-mode-browse rfc-mode-read)
  :config
  (setq rfc-mode-directory (expand-file-name "~/.local/share/rfc/"))
  (add-hook 'rfc-mode-hook 'writeroom-mode))

(use-package ledger-mode
  :straight t
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
  :straight t
  :defer 3
  :config
  (setq scroll-on-drag-motion-scale 0.1)
  (global-set-key [down-mouse-2]
                  (lambda ()
                    (interactive)
                    (unless (scroll-on-drag)
                      (mouse-yank-primary t)))))

(use-package 0x0
  :straight t
  :defer t
  :commands (0x0-dwim 0x0-popup 0x0-upload-file 0x0-upload-text)
  :init (evil-leader/set-key "a 0" '0x0-dwim))

(use-package redacted
  :straight t
  :commands (redacted-mode)
  :config (add-hook 'redacted-mode-hook (lambda () (read-only-mode (if redacted-mode 1 -1)))))

(use-package harpoon
  :straight t
  :config
  (setq harpoon-cache-file (concat user-emacs-directory "harpoon/"))
  (setq harpoon-separate-by-branch nil)
  (evil-leader/set-key "f H" 'harpoon-toggle-file)
  (evil-leader/set-key "f r" 'harpoon-toggle-quick-menu)
  (evil-leader/set-key "f c" 'harpoon-clear)
  (evil-leader/set-key "f h" 'harpoon-add-file)
  (evil-leader/set-key "f j" 'harpoon-go-to-1)
  (evil-leader/set-key "f k" 'harpoon-go-to-2)
  (evil-leader/set-key "f l" 'harpoon-go-to-3)
  (evil-leader/set-key "f ;" 'harpoon-go-to-4)
  (evil-leader/set-key "f f j" 'harpoon-go-to-5)
  (evil-leader/set-key "f f k" 'harpoon-go-to-6)
  (evil-leader/set-key "f f l" 'harpoon-go-to-7)
  (evil-leader/set-key "f f ;" 'harpoon-go-to-8)
  (evil-leader/set-key "f f f" 'harpoon-go-to-9))

(use-package denote
  :straight (denote :host github
                    :repo "protesilaos/denote")
  :defer t
  :commands (denote
             denote-dired-rename-file
             denote-link-buttonize-buffer
             denote-dired-mode-in-directories
             meain/personal-notes meain/work-notes
             meain/new-interview-note meain/new-meeting-note)
  :init
  (add-hook 'dired-mode-hook #'denote-dired-mode)
  ;; (add-hook 'find-file-hook #'denote-link-buttonize-buffer)
  (evil-leader/set-key "N p" 'meain/personal-notes)
  (evil-leader/set-key "N w" 'meain/work-notes)
  :config
  (setq denote-file-type 'markdown-yaml)
  (defun meain/denote--create-or-open (dir create)
    (let ((denote-directory (expand-file-name dir)))
      (if create
          (call-interactively #'denote)
        (find-file denote-directory))))
  (defun meain/work-notes (&optional create)
    "Create or view work notes."
    (interactive "P")
    (meain/denote--create-or-open  "~/.local/share/work-notes/" create))
  (defun meain/personal-notes (&optional create)
    "Create or view personal notes."
    (interactive "P")
    (meain/denote--create-or-open  "~/.local/share/til/" create))
  (defun meain/new-meeting-note ()
    "Create a new note to take meeting notes with info fetched from ,cal."
    (interactive)
    (let* ((denote-directory "~/.local/share/work-notes/")
           (current-meeting (with-temp-buffer
                              (insert-file-contents "/tmp/events-next")
                              (buffer-substring-no-properties (point-min)
                                                              (progn
                                                                (goto-char (point-min))
                                                                (end-of-line)
                                                                (point)))))
           (meeting-name (string-join (cdr (split-string current-meeting " ")) " "))
           (meeting-time (car (split-string current-meeting " ")))
           (name (read-string "Meeting name: " meeting-name))
           (meeting-time (read-string "Meeting time: " meeting-time))
           (context (read-string "Context tag: ")))
      (denote (concat "Meeting notes for " name) (list "meeting" "calendar" context))
      (insert "Name: " name
              "\nContext: " context
              "\nTime: " meeting-time
              "\nDate: " (format-time-string "%a %b %d %Y") "\n\n")))
  (defun meain/new-interview-note ()
    "Create a new note for taking notes on an interview candidate."
    (interactive)
    (let ((denote-directory "~/.local/share/work-notes/")
          (name (read-string "Name: "))
          (github (read-string "Github: ")))
      (denote (concat "Interview notes for " name) '("interview")) ;; more tags?
      (insert "Name: " name
              "\nGithub: " github
              "\nDate: " (format-time-string "%a %b %d %Y") "\n\n")
      (insert-file-contents "~/.config/datafiles/ptemplates/interview-tasklist")
      (message (concat "Now run ,evaluate-assignment " github))
      (meain/copy-to-clipboard (concat ",evaluate-assignment " github)))))

;; Kinda like screensavers
(use-package zone
  :defer t
  :config
  (setq zone-programs [zone-pgm-rotate-LR-lockstep])
  (zone-when-idle (* 5 60)))

;; Mermaid mode
(use-package mermaid-mode :defer t :straight t)

;; Edit any textfield in Emacs
(use-package emacs-everywhere
  :defer t
  :straight t
  :config
  (defun emacs-everywhere-markdown-p () t)
  (defun emacs-everywhere-major-mode-org-or-markdown () (gfm-mode))
  (defun emacs-everywhere-major-mode-function () (gfm-mode)))

;; Fontify face (useful to debug themes)
(use-package fontify-face :straight t :defer t)

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

;; Quick open important files
;; Doing this using a macro so that which key will show proper func name
(defmacro meain/quick-file-open-builder (shortcut name file)
  "Macro to create quick-file-open functions.
SHORTCUT is the keybinding to use.  NAME if the func suffix and FILE is the filename."
  (declare (debug t) (indent defun))
  (let ((funsymbol (intern (concat "open-" name)))
        (fullshortcut (concat "e " shortcut)))
    `(evil-leader/set-key ,fullshortcut
       (defun ,funsymbol (&optional create)
         (interactive "P")
         (if (file-exists-p ,file)
             (if (file-directory-p ,file)
                 (find-file (concat ,file "/"
                                    (completing-read "Choose file:"
                                                     (directory-files ,file nil directory-files-no-dot-files-regexp))))
               (find-file ,file))
           (if create
               (find-file ,file)
             (message "Unable to find %s" ,file)))))))
(meain/quick-file-open-builder "i" "init-el" "~/.dotfiles/emacs/.config/emacs/init.el")
(meain/quick-file-open-builder "3" "i3-config" "~/.dotfiles/i3/.config/i3/config")
(meain/quick-file-open-builder "s" "shell-nix" "shell.nix")
(meain/quick-file-open-builder "c" "mscripts" ".mscripts")
(meain/quick-file-open-builder "f" "elfeed-feeds" "~/.config/emacs/elfeed-feeds.el")
(meain/quick-file-open-builder "a" "early-init" "~/.dotfiles/emacs/.config/emacs/early-init.el")
(meain/quick-file-open-builder "h" "home-manager" "~/.dotfiles/nix/.config/nixpkgs/home.nix")
(meain/quick-file-open-builder "H" "hima-theme" "~/.dotfiles/emacs/.config/emacs/hima-theme.el")
(meain/quick-file-open-builder "t" "evil-textobj-tree-sitter" "~/.config/emacs/straight/repos/evil-textobj-tree-sitter/evil-textobj-tree-sitter.el")
(meain/quick-file-open-builder "l" "ledger" "~/.local/share/ledger/master.ledger")
(meain/quick-file-open-builder "I" "interesting" "~/.local/share/notes/note/interesting-links.md")
(meain/quick-file-open-builder "u" "useful-someday" "~/.local/share/notes/note/useful-someday.md")
(meain/quick-file-open-builder "r" "frequent-web-references" "~/.local/share/notes/note/web-references.md")
(meain/quick-file-open-builder "m" "thing-for-today" "~/.local/share/vime/thing-for-today.mtodo")
(meain/quick-file-open-builder "M" "thing-for-today-personal" "~/.local/share/vime/thing-for-today-personal.mtodo")

(evil-leader/set-key "E e" (cons "eaas-playground" (meain/ilambda project-switch-project "~/dev/rafay/eaas/eaas-playground/")))
(evil-leader/set-key "E p" (cons "paralus" (meain/ilambda project-switch-project "~/dev/rafay/paralus/paralus/")))
(evil-leader/set-key "E n" (cons "nur-packages" (meain/ilambda project-switch-project "~/dev/src/nur-packages/")))

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

;; Quick open scratch buffers
(defun meain/scratchy ()
  "Open scratch buffer in a specific mode."
  (interactive "P")
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
                                         (substring (uuid-string) 0 4))))
    (find-file scratch-file-name)
    (funcall (intern scratch-major-mode))
    (if (eq (intern scratch-major-mode) 'artist-mode)
        (evil-local-mode -1))))
(evil-leader/set-key "c"
  (meain/with-alternate (meain/create-or-switch-to-scratch)
                        (meain/scratchy)))

;; vime functionality within emacs
(use-package uuid :straight t :commands uuid-string)
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

;; TODO: merge this with previous function
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
          (xwidget-webkit-browse-url
           (concatenate 'string
                        "http://127.0.0.1:54321/?query="
                        lookup-term
                        "&docsets="
                        (completing-read "Docset: "
                                         (split-string (shell-command-to-string "dasht-docsets"))))))))))

;; cheat.sh
(use-package cheat-sh
  :straight t
  :commands (cheat-sh cheat-sh-maybe-region)
  :init
  (evil-leader/set-key "a d"
    (meain/with-alternate (call-interactively 'meain/dasht-docs)
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
    (insert (pcase major-mode
              ('emacs-lisp-mode (format "(message \"%s: %%s\" %s)" escaped-thing-to-print thing-to-print thing-to-print))
              ('lisp-interaction-mode (format "(message \"%s: %%s\" %s)" escaped-thing-to-print thing-to-print thing-to-print))
              ('rust-mode (format "println!(\"%s: {:?}\", %s);" escaped-thing-to-print thing-to-print))
              ('go-mode (format "fmt.Println(\"%s:\", %s)" escaped-thing-to-print thing-to-print))
              ('lua-mode (format "print(\"%s:\", %s)" escaped-thing-to-print thing-to-print))
              ('js-mode (format "console.log(\"%s:\", %s)" escaped-thing-to-print thing-to-print))
              ('web-mode (format "console.log(\"%s:\", %s)" escaped-thing-to-print thing-to-print))
              ('shell-script-mode (format "echo \"%s:\" %s" escaped-thing-to-print thing-to-print))
              ('python-mode (format "print(\"%s:\", %s)" escaped-thing-to-print thing-to-print)))))
  (evil-force-normal-state))
(define-key evil-normal-state-map (kbd "g p") 'meain/quick-print)

;; Journal entry
(add-hook 'find-file-hook
          (lambda ()
            (if (string-prefix-p (expand-file-name "~/.local/share/journal")
                                 default-directory)
                (progn
                  (company-mode -1)
                  (auto-fill-mode)))))
(evil-leader/set-key "a J"
  (lambda ()
    "Start writing journal entry.  `journal' invokes emacsclient and gives control back over to Emacs."
    (interactive)
    (start-process-shell-command "journal" "*journal*"
                                 "EDITOR='emacsclient' ,journal")))


;; Narrow region
(use-package fancy-narrow
  :straight t
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

;; Buffer/Frame/Window keybinds
(evil-leader/set-key "b k" 'kill-buffer)
(evil-leader/set-key "b o" 'previous-buffer)
(evil-leader/set-key "b f" 'find-file)
(evil-leader/set-key "b d" 'delete-frame)

;; Server edit complete
(evil-leader/set-key "s" 'server-edit)

;; Next and previous buffer
(define-key evil-normal-state-map (kbd "C-S-o") 'previous-buffer)
(define-key evil-normal-state-map (kbd "C-S-i") 'next-buffer)

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
  (if (not (file-remote-p default-directory))
      (setq default-directory (cond
                               ((not (eq (project-current) nil))
                                (car (project-roots (project-current))))
                               (t "~/")))))
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
(defun meain/github-url (&optional no-linenumber)
  "Open the Github page for the current file.  Pass NO-LINENUMBER to not add a line number."
  (interactive "P")
  (let* ((git-url (replace-regexp-in-string "\.git$"
                                            ""
                                            (s-replace "git@github\.com:"
                                                       "https://github.com/"
                                                       (car (split-string
                                                             (shell-command-to-string "git config --get remote.origin.url") "\n")))))
         ;; would love to remove abbrev-ref once we have some logic to check if we have pushed the latest version
         (git-branch (car (split-string (shell-command-to-string "git rev-parse --abbrev-ref HEAD") "\n")))
         (web-url (format "%s/blob/%s/%s%s"
                          git-url
                          git-branch
                          (file-relative-name (if (equal major-mode 'dired-mode)
                                                  default-directory
                                                buffer-file-name)
                                              (car (project-roots (project-current))))
                          (if (or no-linenumber (equal major-mode 'dired-mode))
                              ""
                            (format "#L%s" (line-number-at-pos))))))
    (progn
      (message "%s coped to clipboard." web-url)
      (meain/copy-to-clipboard web-url))))
(evil-leader/set-key "b G" 'meain/github-url)

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
(evil-leader/set-key "a s" 'meain/eww-search-ddg)

;; search from Emacs using xwidgets
(defun meain/search-xwidget ()
  "Search from Emacs using xwidgets."
  (interactive)
  (xwidget-webkit-browse-url (concat "https://duckduckgo.com/?q="
                                     (read-string "Search term: " (thing-at-point 'symbol)))))

;; Check available update for straight managed packages
(add-hook 'markdown-mode-hook
          (lambda ()
            (if (equal "/tmp/straight-available-updates.md" (buffer-file-name))
                (markdown-toggle-url-hiding 1))))
(defun meain/straight-available-updates ()
  "Check available update for straight managed packages."
  (interactive)
  (message "Fetching updates and calculating changes...")
  ;; (async-shell-command "zsh -ic 'straight-available-updates'"))
  (start-process-shell-command "straight-available-updates" "*straight-available-updates*"
                               "zsh -ic 'straight-available-updates'"))

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

:; Patterns can be regex if we want to match more precisely eg: ("/early-init.el$" "/init.el")
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
(evil-leader/set-key "e e" 'meain/find-alternate-file)

;; Splitting lists (https://github.com/AckslD/nvim-trevJ.lua)
(defun meain/split-at-commas (start end)
  "Split a selection at commas.  `START' and `END' are range start and end."
  (interactive "r")
  (if (use-region-p)
      (let ((regionp (buffer-substring start end)))
        (delete-active-region)
        (insert (concat "\n" (string-replace "," ",\n" regionp)))
        (if (equal major-mode 'go-mode)
            (insert ",\n")
          (insert "\n")))
    (message "Split only works on selections")))


;; Screenshot Emacs frame
(defvar meain/frameshot-directory "~/Pictures/Screenshots/"
  "Default directory for frame shots.")
(defvar meain/frameshot-format 'png
  "Default frame shot format.")
(defun meain/frameshot ()
  "Save Emacs frame as frame shot.
Directory is determined by variable `frameshot-directory' and if
not defined, it will be saved in the `$HOME' directory."
  (interactive)
  (let* ((image (x-export-frames nil (or frameshot-format 'png)))
	     (directory (or frameshot-directory (getenv "HOME")))
	     (file (concat directory (format-time-string "EMACS-Screenshot-%Y-%m-%d-%T.")
		               (symbol-name frameshot-format))))
    (make-directory directory t)
    (with-temp-file file
      (insert image))
    (dired directory)
    (revert-buffer)
    (dired-goto-file (expand-file-name file))
    (message "Frame shot saved as `%s'" file)))

;; Just some hima testing code
(defun meain/reload-current-theme ()
  "Util to reload hima theme for debugging."
  (interactive)
  (message "%s" custom-enabled-themes)
  (let ((current-theme (car custom-enabled-themes)))
    (disable-theme current-theme)
    (load-theme current-theme t)))

;; Better modeline
(use-package which-func
  :commands (which-function))
(use-package mode-line-idle
  :straight t
  :commands (mode-line-idle))
(setq-default mode-line-format nil)
(setq-default header-line-format
              (list '(:eval (if (eq 'emacs evil-state) "[E] " " ")) ;; vim or emacs mode
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
                    '(:eval (mode-line-idle 0.3
                                            '(:propertize (:eval (let ((thing-name (meain/tree-sitter-thing-name 'class-like)))
                                                                   (if thing-name
                                                                       (format ":%s" thing-name))))
                                                          face
                                                          hima-simple-gray)
                                            ""))
                    '(:eval (mode-line-idle 0.3
                                            '(:propertize (:eval (let ((thing-name (meain/tree-sitter-thing-name 'function-like))
                                                                       (config-nesting (meain/tree-sitter-config-nesting))
                                                                       (func-name (which-function)))
                                                                   (if thing-name
                                                                       (format ":%s" thing-name)
                                                                     (if config-nesting
                                                                         (format ":%s" config-nesting)
                                                                       (if func-name
                                                                           (format ":%s" func-name))
                                                                       )
                                                                     )))
                                                          face
                                                          hima-simple-gray)
                                            ""))
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
                    '(:eval (propertize " "
                                        'display
                                        `((space :align-to (- (+ right right-fringe right-margin)
                                                              ,(+ 2
                                                                  (+ (string-width (format-mode-line "%l:%c %p"))
                                                                     (string-width (format-mode-line "%m"))))))))) ;; spacer
                    (propertize "%l:%c %p") ;; position in file
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
                                (when (boundp 'straight--profile-cache)
                                  (setq package-count (+ (hash-table-size straight--profile-cache)
                                                         package-count)))
                                (if (zerop package-count)
                                    (format "Emacs started in %s" time)
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
