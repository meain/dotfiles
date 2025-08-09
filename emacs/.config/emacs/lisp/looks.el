;;; looks.el --- Things that improve the looks -*- lexical-binding: t; -*-

;;; Commentary:
;; Things that improve the looks or viaual aids.

;;; Code:
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

;; Symbol overlay
(use-package symbol-overlay
  :ensure t
  :defer t
  :commands (symbol-overlay-mode symbol-overlay-put))

;; Highlight color codes
;; Alternative: https://github.com/DevelopmentCool2449/colorful-mode
(use-package rainbow-mode
  :ensure t
  :commands (rainbow-mode)
  :init (add-hook 'css-mode-hook 'rainbow-mode))

;; A silly little package to encourage on save
(use-package emacs
  :defer t
  :config
  (load (concat user-emacs-directory "encourage")))

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

;; Highlight enclosing parenthesis
(use-package highlight-parentheses
  :defer t
  :ensure t
  :init (add-hook 'prog-mode-hook #'highlight-parentheses-mode)
  :config (setq highlight-parentheses-colors '("coral1")))

;; Fontify face (useful to debug themes)
(use-package fontify-face :ensure t :defer t)

(use-package auto-highlight-symbol
  :ensure t
  :commands (auto-highlight-symbol-mode)
  :init
  (add-hook 'prog-mode-hook #'auto-highlight-symbol-mode))

(provide 'looks)
;;; looks.el ends here
