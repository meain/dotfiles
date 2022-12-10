;;; hima-theme.el --- Custom face theme for Emacs

;;; Code:
(defgroup hima nil "Custom faces for more stuff." :prefix "hima-" :group nil)
(defvar hima-use-italics nil "Decides weather to use italics.")

(deftheme hima "A minimal theme with simple colors.")

(let* ((white "#FFFFFF")
       (light-white "#F5F5F5")
       (black "#000000")
       (light-black "#1C1C1C")

       (lighter-gray "#EEEEEE")
       (light-gray "#DADADA")
       (medium-gray "#A0A0A0")
       (dark-gray "#424242")

       (barely-orange "#EDE5CB")
       (lighter-orange "#F7C862")
       (light-orange "#FFAF00")
       (dark-orange "#E75600")

       (lighter-blue "#C1E7F4")
       (light-blue "#B7C9E2")
       (blue "#0087AF")
       (dark-blue "#4271AE")
       (green-blue "#C3E9DB")

       (light-green "#DDFEDD")
       (almost-light-green "#C9F3D2")
       (dark-green "#008861")
       (light-red "#FFDDDD")
       (almost-light-red "#F8CED3")
       (dark-red "#9E0047")

       (pink "#E41D91")

       (default-layer `((t (:foreground ,black))))
       (commentary-layer `((t (:foreground ,medium-gray :italic ,hima-use-italics))))
       (string-layer `((t (:foreground ,blue))))
       (definition-layer `((t (:foreground ,dark-orange))))
       (bold-layer `((t (:foreground ,black :weight bold))))
       (underline-layer `((t (:underline t))))
       (almost-invisible-layer `((t (:foreground ,light-gray))))
       (disabled-layer `((t (:foreground ,medium-gray))))
       (tooltip-layer `((t (:background ,lighter-gray :foreground ,black))))
       (tooltip-selected-layer `((t (:background ,light-blue :foreground ,black))))
       (all-black-layer `((t (:background ,black :foreground ,black))))
       (all-white-layer `((t (:background ,white :foreground ,white))))
       (green-light-green-layer `((t (:foreground ,dark-green :background ,light-green))))
       (greener-light-green-layer `((t (:foreground ,dark-green :background ,almost-light-green))))
       (red-light-red-layer `((t (:foreground ,dark-red :background ,light-red))))
       (reder-light-red-layer `((t (:foreground ,dark-red :background ,almost-light-red)))))

  (defface hima-simple-gray `((t (:foreground ,medium-gray)))
    "Simple gray color for use outside comments"
    :group 'hima)
  (defface hima-all-white `,all-white-layer
    "Simple gray color for use outside comments"
    :group 'hima)

  (custom-theme-set-faces 'hima
    ;; Basic items
    `(default ,default-layer)
    `(nobreak-space ,default-layer)
    `(underline ,underline-layer)
    `(cursor ((t (:background ,black))))
    `(fringe ((t (:background ,white :foreground ,light-gray))))
    `(highlight ((t (:background ,light-blue))))
    `(region ((t (:background ,lighter-blue :foreground ,black))))
    `(tooltip ((t (:background ,light-black :foreground ,medium-gray))))
    `(vertical-border ((t (:background ,lighter-gray :foreground ,lighter-gray))))
    `(widget-button ((t (:foreground ,dark-blue))))

    ;; Gui items
    `(header-line ((t (:foreground ,black :box (:line-width 1 :color ,lighter-gray :style nil))))) ; Works best for topsy-mode
    `(mode-line-inactive ((t (:background ,light-gray))))
    `(mode-line-buffer-id ,bold-layer)
    `(mode-line ((t (:background ,green-blue))))
    `(minibuffer-prompt ((t (:foreground ,blue))))

    ;; Basic font lock entries
    `(font-lock-keyword-face ,default-layer)
    `(font-lock-warning-face ,default-layer)
    `(font-lock-function-name-face ,definition-layer)
    `(font-lock-type-face ,default-layer)
    `(font-lock-preprocessor-face ,default-layer)
    `(font-lock-builtin-face ,default-layer)
    `(font-lock-variable-name-face ,default-layer)
    `(font-lock-string-face ,string-layer)
    `(font-lock-constant-face ,default-layer)
    `(font-lock-doc-face ,commentary-layer)
    `(font-lock-comment-face ,commentary-layer)

    ;; show-paren
    `(show-paren-mismatch ,red-light-red-layer)
    `(show-paren-match ,tooltip-selected-layer)

    ;; hl-line
    `(hl-line ((t (:background ,lighter-gray))))

    ;; linum
    `(linum ((t (:foreground ,black))))

    ;; company
    `(company-tooltip ,tooltip-layer)
    `(company-tooltip-annotation ,tooltip-layer)
    `(company-tooltip-common ((t (:foreground ,black))))
    `(company-tooltip-selection ,tooltip-selected-layer)
    `(company-tooltip-annotation-selection ,tooltip-selected-layer)
    `(company-tooltip-common-selection ,tooltip-selected-layer)
    `(company-preview ((t (:foreground ,dark-gray :background ,(face-attribute 'default :background)))))
    `(company-preview-common ((t (:foreground ,dark-gray :background ,(face-attribute 'default :background)))))
    `(company-scrollbar-bg ,all-black-layer)
    `(company-scrollbar-fg ,all-white-layer)

    ;; flyspell
    `(flyspell-duplicate ((t (:underline (:style wave :color ,dark-green)))))
    `(flyspell-incorrect ((t (:underline (:style wave :color ,dark-orange)))))

    ;; isearch
    `(isearch ((t (:background ,light-blue :foreground ,dark-gray))))

    ;; vterm (need a lot more colors)
    `(vterm-color-black ((t (:foreground "#444444" :background "#969694"))))
    `(vterm-color-red ((t (:foreground "#9E0047" :background "#FF0072"))))
    `(vterm-color-green ((t (:foreground "#718C00" :background "#8EB200"))))
    `(vterm-color-yellow ((t (:foreground "#B25000" :background "#FF7200"))))
    `(vterm-color-blue ((t (:foreground "#4271AE" :background "#5EA2F9"))))
    `(vterm-color-magenta ((t (:foreground "#8959A8" :background "#A56CCC"))))
    `(vterm-color-cyan ((t (:foreground "#3E999F" :background "#51C8D1"))))
    `(vterm-color-white ((t (:foreground "#F5F5F5" :background "#FFFFFF"))))
    `(vterm-color-underline ,underline-layer)

    `(term-color-black ((t (:foreground "#444444" :background "#969694"))))
    `(term-color-red ((t (:foreground "#9E0047" :background "#FF0072"))))
    `(term-color-green ((t (:foreground "#718C00" :background "#8EB200"))))
    `(term-color-yellow ((t (:foreground "#B25000" :background "#FF7200"))))
    `(term-color-blue ((t (:foreground "#4271AE" :background "#5EA2F9"))))
    `(term-color-magenta ((t (:foreground "#8959A8" :background "#A56CCC"))))
    `(term-color-cyan ((t (:foreground "#3E999F" :background "#51C8D1"))))
    `(term-color-white ((t (:foreground "#F5F5F5" :background "#FFFFFF"))))
    `(term-color-underline ,underline-layer)

    ;; magit
    `(diff-refine-removed ,reder-light-red-layer)
    `(diff-refine-added ,greener-light-green-layer)
    `(magit-diff-context ((t (:background ,white))))
    `(magit-diff-context-highlight ((t (:background ,light-white))))
    `(magit-diff-hunk-heading ((t (:background ,light-gray))))
    `(magit-diff-hunk-heading-highlight ((t (:background ,medium-gray))))
    `(magit-diff-removed ,red-light-red-layer)
    `(magit-diff-added ,green-light-green-layer)
    `(magit-diff-removed-highlight ,red-light-red-layer)
    `(magit-diff-added-highlight ,green-light-green-layer)
    `(magit-section-heading ((t (:foreground ,dark-blue))))
    `(magit-section-highlight ((t (:foreground ,pink))))

    ;; tree-sitter
    `(tree-sitter-hl-face:comment ,commentary-layer)
    `(tree-sitter-hl-face:doc ((t (:foreground ,dark-green))))
    `(tree-sitter-hl-face:string ,string-layer)
    `(tree-sitter-hl-face:punctuation.delimiter ,default-layer)
    `(tree-sitter-hl-face:keyword ,default-layer)
    `(tree-sitter-hl-face:property ,default-layer)
    `(tree-sitter-hl-face:method.call ,default-layer)
    `(tree-sitter-hl-face:function.call ,default-layer)
    `(tree-sitter-hl-face:function.method ,definition-layer)
    `(tree-sitter-hl-face:constructor ,definition-layer)

    ;; fancy-narrow
    `(fancy-narrow-blocked-face ,almost-invisible-layer)

    ;; dired
    `(dired-directory ((t (:weight bold :foreground ,black))))
    `(dired-ignored ,disabled-layer)

    ;; shr
    `(shr-link ,underline-layer)

    ;; variable-pitch
    `(variable-pitch ((t nil)))

    ;; strips-mode
    `(stripe-highlight ((t (:background ,lighter-blue))))
    `(stripe-hl-line ((t (:background ,barely-orange))))

    ;; eldoc
    `(eldoc-highlight-function-argument ((t (:foreground ,dark-blue))))

    ;; denote
    `(denote-faces-date ((t :foreground ,medium-gray)))
    `(denote-faces-time ((t :foreground ,medium-gray)))
    `(denote-faces-title ,default-layer)
    `(denote-faces-extension ,commentary-layer)
    `(denote-faces-keywords ((t :foreground ,medium-gray)))

    ;; diff-hl
    `(diff-hl-change ((t (:background ,barely-orange))))
    `(diff-hl-insert ((t (:background ,light-green))))
    `(diff-hl-delete ((t (:background ,light-red))))

    ;; keycast
    `(keycast-key ((t (:background ,light-blue))))
    ))

(provide-theme 'hima)
;;; hima-theme.el ends here
