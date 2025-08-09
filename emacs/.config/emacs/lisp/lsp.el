;;; lsp.el --- LSP related things -*- lexical-binding: t; -*-

;;; Commentary:
;; LSP related things

;;; Code:
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


(provide 'lsp)
;;; lsp.el ends here
