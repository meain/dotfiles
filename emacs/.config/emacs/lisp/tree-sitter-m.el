;;; tree-sitter-m.el --- Tree sitter related packages -*- lexical-binding: t; -*-

;;; Commentary:
;; Everything that has to do with tree sitter.

;;; Code:
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
          (just . ("https://github.com/IndianBoy42/tree-sitter-just" "main"))
          ;; (nix . ("https://github.com/nix-community/tree-sitter-nix" "master")) ;; no nix-ts-mode
          (go . ("https://github.com/tree-sitter/tree-sitter-go" "master"))
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

;; Splitting and joining list (https://github.com/AckslD/nvim-trevJ.lua)
(setq tree-surgeon-load-path (concat (getenv "HOME") "/dev/src/tree-surgeon"))
(use-package tree-surgeon
  :load-path tree-surgeon-load-path
  :after (evil-leader)
  :init (evil-leader/set-key "H j" 'tree-surgeon-split-join))

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

(provide 'tree-sitter-m)
;;; tree-sitter-m.el ends here
