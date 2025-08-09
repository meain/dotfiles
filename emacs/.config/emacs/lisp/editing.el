;;; editing.el --- Editing related -*- lexical-binding: t; -*-

;;; Commentary:
;; Everything that makes text editing faster.

;;; Code:
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

;; Quick replace
(use-package emacs
  :after evil
  :defer t
  :init
  (define-key evil-normal-state-map (kbd "<SPC> ;") (cons "replace in buffer" (ilambda evil-ex "%s/")))
  (define-key evil-visual-state-map (kbd "<SPC> ;") (cons "replace in buffer"(ilambda evil-ex "'<,'>s/"))))

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
    ";ee" 'tree-surgeon-go-error
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

;; Persistent undo using undo-tree
(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode t)
  (setq undo-limit 80000000)
  (setq evil-want-fine-undo nil)
  (setq undo-tree-auto-save-history t)
  (setq undo-tree-history-directory-alist '(("." . "~/.local/share/emacs/undo"))))

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

;; editorconfig
(use-package editorconfig
  :defer t
  :ensure t
  :config (editorconfig-mode 1))

;; Emmet for html stuff (c-j to activate)
(use-package emmet-mode
  :ensure t
  :defer t
  :commands (emmet-mode))

(provide 'editing)
;;; editing.el ends here
