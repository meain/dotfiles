;;; hima-theme.el --- Custom face theme for Emacs

;;; Code:
(defun meain/reload-hima ()
  (interactive)
  (disable-theme 'hima)
  (load-theme 'hima t))

(defgroup hima-faces nil "Custom faces for more stuff"
  :prefix "hima-faces-")

(deftheme hima "A minimal theme with simple colors")

(let* ((white "#FFFFFF")
       (light-white "#F5F5F5")
       (black "#000000")
       (light-black "#1C1C1C")

       (lighter-gray "#EEEEEE")
       (light-gray "#DADADA")
       (medium-gray "#A0A0A0")
       (dark-gray "#424242")

       (lighter-orange "#EDE5CB")
       (light-orange "#FFAF00")
       (dark-orange "#E75600")

       (lighter-blue "#C1E7F4")
       (light-blue "#B7C9E2")
       (blue "#0087AF")
       (dark-blue "#4271AE")
       (green-blue "#C3E9DB")

       (light-green "#DDFEDD")
       (dark-green "#008861")
       (light-red "#FFDDDD")
       (dark-red "#9E0047")

       (pink "#E41D91")

       (default-layer `((t (:foreground ,black :background ,(face-attribute 'default :background)))))
       (commentary-layer `((t (:foreground ,medium-gray :background ,(face-attribute 'default :background) :italic t))))
       (string-layer `((t (:foreground ,blue :background ,(face-attribute 'default :background)))))
       (bold-layer `((t (:foreground ,black :weight bold))))
       (underline-layer `((t (:underline t))))
       (tooltip-layer `((t (:background ,lighter-gray :foreground ,black))))
       (tooltip-selected-layer `((t (:background ,light-blue :foreground ,black))))
       (all-black-layer `((t (:background ,black :foreground ,black))))
       (all-white-layer `((t (:background ,white :foreground ,white))))
       (green-light-green-layer `((t (:foreground ,dark-green :background ,light-green))))
       (red-light-red-layer `((t (:foreground ,dark-red :background ,light-red)))))

  (defface hima-simple-gray `((t (:foreground ,medium-gray)))
    "Simple gray color for use outside comments"
    :group 'hima-faces)
  (defface hima-all-white `,all-white-layer
    "Simple gray color for use outside comments"
    :group 'hima-faces)

  (custom-theme-set-faces 'hima
    ;; Basic items
    `(default ,default-layer)
    `(underline ,underline-layer)
    `(cursor ((t (:background ,black))))
    `(fringe ((t (:background ,white :foreground ,light-gray))))
    `(highlight ((t (:background ,light-blue))))
    `(region ((t (:background ,lighter-blue :foreground ,black))))
    `(tooltip ((t (:background ,light-black :foreground ,medium-gray))))
    `(vertical-border ((t (:background ,lighter-gray :foreground ,lighter-gray))))
    `(widget-button ((t (:foreground ,dark-blue))))

    ;; Gui items
    `(header-line ((t (:background ,light-gray))))
    `(mode-line-inactive ((t (:background ,light-gray))))
    `(mode-line-buffer-id ,bold-layer)
    `(mode-line ((t (:background ,green-blue))))
    `(minibuffer-prompt ((t (:foreground ,blue :background ,white))))

    ;; Basic font lock entries
    `(font-lock-keyword-face ((t (:foreground ,black))))
    `(font-lock-warning-face ((t (:foreground ,black))))
    `(font-lock-function-name-face ((t (:foreground ,dark-orange))))
    `(font-lock-type-face ((t (:foreground ,black))))
    `(font-lock-preprocessor-face ((t (:foreground ,black))))
    `(font-lock-builtin-face ((t (:foreground ,black))))
    `(font-lock-variable-name-face ((t (:foreground ,black))))
    `(font-lock-string-face ((t (:foreground ,blue))))
    `(font-lock-constant-face ((t (:foreground ,black))))
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
    `(flyspell-duplicate ((t (:underline (:style wave)))))
    `(flyspell-incorrect ((t (:underline t :foreground ,light-orange))))

    ;; isearch
    `(isearch ((t (:background ,light-blue :foreground ,dark-gray))))

    ;; vterm (need a lot more colors)
    `(vterm-color-black ((t (:foreground "#444444" :background "#002B36"))))
    `(vterm-color-red ((t (:foreground "#9E0047" :background "#CB4B16"))))
    `(vterm-color-green ((t (:foreground "#718C00" :background "#586E75"))))
    `(vterm-color-yellow ((t (:foreground "#B25000" :background "#657B83"))))
    `(vterm-color-blue ((t (:foreground "#4271AE" :background "#839496"))))
    `(vterm-color-magenta ((t (:foreground "#8959A8" :background "#6C71C4"))))
    `(vterm-color-cyan ((t (:foreground "#3E999F" :background "#93A1A1"))))
    `(vterm-color-white ((t (:foreground "#F5F5F5" :background "#FDF6E3"))))
    `(vterm-color-underline ,underline-layer)

    ;; magit
    `(magit-diff-context ((t (:background ,white))))
    `(magit-diff-context-highlight ((t (:background ,light-white))))
    `(magit-diff-hunk-heading ((t (:background ,medium-gray))))
    `(magit-diff-hunk-heading-highlight ,tooltip-layer)
    `(magit-diff-removed ,red-light-red-layer)
    `(magit-diff-added ,green-light-green-layer)
    `(magit-diff-removed-highlight ,red-light-red-layer)
    `(magit-diff-added-highlight ,green-light-green-layer)
    `(magit-section-heading ((t (:foreground ,dark-blue))))
    `(magit-section-highlight ((t (:foreground ,pink))))

    ;; tree-sitter
    `(tree-sitter-hl-face:doc ((t (:foreground ,dark-green))))
    `(tree-sitter-hl-face:string ,string-layer)
    `(tree-sitter-hl-face:keyword ,default-layer)
    `(tree-sitter-hl-face:property ,default-layer)
    `(tree-sitter-hl-face:method.call ,default-layer)
    `(tree-sitter-hl-face:function.call ,default-layer)
    `(tree-sitter-hl-face:constructor ((t (:italic nil :foreground ,dark-orange))))

    ;; dired
    `(dired-directory ((t (:weight bold :foreground ,black))))

    ;; diff-hl
    `(diff-hl-change ((t (:background ,lighter-orange))))
    `(diff-hl-insert ((t (:background ,light-green))))
    `(diff-hl-delete ((t (:background ,light-red))))))

(provide-theme 'hima)

;; Local Variables:
;; no-byte-compile: t
;; End:
