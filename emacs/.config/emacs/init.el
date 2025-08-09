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
(require 'editing)
(require 'window-management)
(require 'terminals)

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

;;; [OTHER PACKAGES] =============================================

(requrie 'filesystem)
(require 'project-management)
(require 'checkers)
(require 'completions)
(require 'documentation)
(requrie 'scratcher)
(require 'searching)
(require 'lsp)
(require 'vcs)
(require 'navigation)
(require 'looks)

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

;; Direnv support
(use-package envrc
  :ensure t
  :config (envrc-global-mode))

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
