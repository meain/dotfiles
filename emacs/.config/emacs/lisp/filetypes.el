;;; filetypes.el --- Configs for different lang modes -*- lexical-binding: t; -*-

;;; Commentary:
;; Adapting vim naming, contains different modes related configs.

;;; Code:
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

(use-package mermaid-mode :defer t :ensure t)

(provide 'filetypes)
;;; filetypes.el ends here
