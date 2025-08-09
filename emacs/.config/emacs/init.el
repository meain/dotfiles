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

;; TODO: Use builtin util
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

;; Add  additional load path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'evil-m) ; setup this first
(require 'utils)
(requrie 'settings)
(require 'visual)

;;; [BASIC BUILTINS] ===========================================

(require 'editing)

;; Hit universal arg without ctrl
(use-package emacs
  :after evil-leader
  :defer t
  :config
  (evil-leader/set-key "u" 'universal-argument)
  (global-set-key (kbd "M-u") 'universal-argument)
  (define-key universal-argument-map (kbd "M-u") 'universal-argument-more))

;; A silly little package to encourage on save
(use-package emacs
  :defer t
  :config
  (load (concat user-emacs-directory "encourage")))

(require 'window-management)
(require 'terminals)

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

(requrie 'filesystem)
(require 'project-management)
(require 'checkers)
(require 'completions)
(require 'documentation)
(requrie 'scratcher)

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
  (defun meain/recenter-top-advice (orig-fn &rest args)
    "Used to recenter the buffer after `ORIG-FN' passing down `ARGS' down to it."
    (apply orig-fn args)
    (recenter 13))

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
  :init
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
;; https://www.matem.unam.mx/~omar/apropos-emacs.html#the-case-against-which-key-a-polemic
(use-package which-key
  :ensure t
  :defer nil
  :config
  ;; Only show up if explicitly requested
  (setq which-key-show-early-on-C-h t
        which-key-idle-delay 1e6 ; 11 days
        which-key-idle-secondary-delay 0.05)
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
  (setq-default markdown-hide-urls t) ;; Or call markdown-toggle-url-hiding

  (add-hook 'gfm-mode-hook #'toggle-truncate-lines)

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
(use-package just-ts-mode :ensure t :defer t)
(use-package kql-mode :ensure t :defer t)
(use-package yaml-mode :ensure t :defer t)
(use-package k8s-mode :ensure t :defer t) ; syntax highlighting for templated yaml files (helm)
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

(require 'tools)

;; Window layout changer
(use-package rotate
  :ensure t
  :after evil
  :commands (rotate-layout rotate-window)
  :init
  (define-key evil-normal-state-map (kbd "M-f <SPC>") 'rotate-layout))

(require 'tree-sitter-m)

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

;; Revert buffer quickly (fix for dealing with eglot loosing it)
(global-set-key (kbd "M-f r") (lambda ()
                                (interactive)
                                (save-buffer)
                                (revert-buffer-quick)))

(require 'bookmarking)
(require 'notes)

(use-package auto-highlight-symbol
  :ensure t
  :commands (auto-highlight-symbol-mode)
  :init
  (add-hook 'prog-mode-hook #'auto-highlight-symbol-mode))

(require 'ai)
(require 'gptel-m)
(require 'yap-m)
(require 'templating)
(require 'read-write)
(require 'file-manip)
(require 'automatic)
(require 'web)

(use-package which-func :commands (which-function))
(require 'random)

;; Better modeline
(use-package mode-line-idle :ensure t :commands (mode-line-idle))
(defvar meain/modeline-project-color
  '(:eval
    (let* ((project-name (or (meain/project-name) ""))
           (hex-color (concat "#" (substring (md5 project-name) 0 6)))
           (rgb (color-name-to-rgb hex-color))
           (hsl (apply #'color-rgb-to-hsl rgb))
           (new-rgb (apply #'color-hsl-to-rgb (list (nth 0 hsl) 0.13 0.7)))
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
  "Create a modeline segment with `EXPR' expression."
  `(:eval (let ((value ,expr))
            (if value (propertize value 'face 'hima-simple-gray) ""))))
(defvar meain/modeline-vcs
  (meain/modeline-segment
   `(when-let (vc vc-mode) (concat " @" (substring vc 5)))))
(defvar meain/modeline-yap
  (meain/modeline-segment
   `(when (or (boundp 'gptel-model) (boundp 'yap-model))
      (concat " ["
              (when (boundp 'yap-model) yap-model)
              "/"
              (when (boundp 'gptel-model) (format "%s" gptel-model))
              "]"))))

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
