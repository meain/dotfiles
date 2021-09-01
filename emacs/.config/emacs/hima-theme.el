;;; hima-theme.el --- Custom face theme for Emacs

;;; Code:
(defun meain/reload-hima ()
  (interactive)
  (disable-theme 'hima)
  (load-theme 'hima t))

(deftheme hima)


(defgroup hima-faces nil
  "Custom faces for more stuff"
  :prefix "hima-faces-")
(defface hima-simple-grey `((t (:foreground "#999999")))
  "Simple grey color for use outside comments"
  :group  'hima-faces)

(custom-theme-set-faces 'hima

'(underline ((((class color) (min-colors 89)) (:underline t :foreground "#949494"))))
'(font-lock-warning-face ((((class color) (min-colors 89)) (:foreground "#000000"))))
'(font-lock-keyword-face ((((class color) (min-colors 89)) (:foreground "#000000"))))
'(font-lock-function-name-face ((((class color) (min-colors 89)) (:foreground "#E75600"))))
'(font-lock-type-face ((((class color) (min-colors 89)) (:foreground "#000000"))))
'(font-lock-preprocessor-face ((((class color) (min-colors 89)) (:foreground "#000000"))))
'(font-lock-builtin-face ((((class color) (min-colors 89)) (:foreground "#000000"))))
'(font-lock-variable-name-face ((((class color) (min-colors 89)) (:foreground "#000000"))))
'(font-lock-string-face ((((class color) (min-colors 89)) (:foreground "#0087AF"))))
'(font-lock-constant-face ((((class color) (min-colors 89)) (:foreground "#000000"))))
'(font-lock-doc-face ((((class color) (min-colors 89)) (:foreground "#999999" :italic t))))
'(font-lock-comment-face ((((class color) (min-colors 89)) (:foreground "#999999" :italic t))))
'(show-paren-match-face ((((class color) (min-colors 89)) (:weight bold :background "#DADADA" :foreground "#1C1C1C"))))
'(default ((((class color) (min-colors 89)) (:background "#ffffff" :foreground "#000000"))))
'(highline-face ((((class color) (min-colors 89)) (:underline t :background "#f5f5f5"))))
'(region ((((class color) (min-colors 89)) (:background "#C1E7F4" :foreground "#424242"))))
'(fringe ((((class color) (min-colors 89)) (:underline t :foreground "#000000"))))
'(linum ((((class color) (min-colors 89)) (:underline t :foreground "#000000"))))

'(widget-button   ((((class color) (min-colors 89)) (:foreground "#4271AE"))))

'(ac-selection-face ((((class color) (min-colors 89)) (:background "#1C1C1C" :foreground "#EEEEEE"))))
'(ac-candidate-face ((((class color) (min-colors 89)) (:background "#D0D0D0" :foreground "#424242"))))

'(flyspell-duplicate ((((class color) (min-colors 89)) (:underline (:style wave)))))
'(flyspell-incorrect ((((class color) (min-colors 89)) (:underline t :foreground "#FFAF00"))))

'(mode-line-inactive ((((class color) (min-colors 89)) (:background "#EEEEEE"))))
'(mode-line-buffer-id ((((class color) (min-colors 89)) (:weight bold))))
'(mode-line ((((class color) (min-colors 89)) (:background "#C3E9DB"))))
'(minibuffer-prompt ((((class color) (min-colors 89)) (:foreground "#0087AF" :background "#ffffff"))))

'(isearch ((((class color) (min-colors 89)) (:weight bold :background "#C3E9DB" :foreground "#424242"))))
'(isearch-lazy-highlight-face ((((class color) (min-colors 89)) (:weight bold :background "#B7C9E2" :foreground "#424242"))))

'(vterm-color-black   ((((class color) (min-colors 89)) (:foreground "#444444" :background "#002B36"))))
'(vterm-color-red     ((((class color) (min-colors 89)) (:foreground "#9e0047" :background "#CB4B16"))))
'(vterm-color-green   ((((class color) (min-colors 89)) (:foreground "#718C00" :background "#586E75"))))
'(vterm-color-yellow  ((((class color) (min-colors 89)) (:foreground "#b25000" :background "#657B83"))))
'(vterm-color-blue    ((((class color) (min-colors 89)) (:foreground "#4271AE" :background "#839496"))))
'(vterm-color-magenta ((((class color) (min-colors 89)) (:foreground "#8959A8" :background "#6C71C4"))))
'(vterm-color-cyan    ((((class color) (min-colors 89)) (:foreground "#3E999F" :background "#93A1A1"))))
'(vterm-color-white   ((((class color) (min-colors 89)) (:foreground "#f5f5f5" :background "#FDF6E3"))))

'(magit-diff-context    ((((class color) (min-colors 89)) (:background "#FFFFFF"))))
'(magit-diff-context-highlight    ((((class color) (min-colors 89)) (:background "#E8E8E8"))))
'(magit-diff-hunk-heading    ((((class color) (min-colors 89)) (:background "#CCCCCC"))))
'(magit-diff-hunk-heading-highlight    ((((class color) (min-colors 89)) (:background "#424242" :foreground "#FFFFFF"))))
'(magit-diff-removed     ((((class color) (min-colors 89)) (:foreground "#9e0047" :background "#FFDDDD"))))
'(magit-diff-added   ((((class color) (min-colors 89)) (:foreground "#008861" :background "#DDFEDD"))))
'(magit-diff-removed-highlight     ((((class color) (min-colors 89)) (:foreground "#9e0047" :background "#FFDDDD"))))
'(magit-diff-added-highlight   ((((class color) (min-colors 89)) (:foreground "#008861" :background "#DDFEDD"))))
'(magit-section-heading   ((((class color) (min-colors 89)) (:foreground "#4271AE"))))
'(magit-section-highlight   ((((class color) (min-colors 89)) (:foreground "#E41D91"))))

'(tree-sitter-hl-face:doc ((((class color) (min-colors 89)) (:foreground "#0087AF"))))
'(tree-sitter-hl-face:string ((((class color) (min-colors 89)) (:foreground "#0087AF"))))
'(tree-sitter-hl-face:keyword ((((class color) (min-colors 89)) (:foreground "#000000"))))
'(tree-sitter-hl-face:property ((((class color) (min-colors 89)) (:foreground "#000000"))))
'(tree-sitter-hl-face:method.call ((((class color) (min-colors 89)) (:italic nil :foreground "#000000"))))
'(tree-sitter-hl-face:function.call ((((class color) (min-colors 89)) (:italic nil :foreground "#000000"))))
'(tree-sitter-hl-face:constructor ((((class color) (min-colors 89)) (:italic nil :foreground "#E75600"))))

'(dired-directory ((((class color) (min-colors 89)) (:weight bold :foreground "#000000")))))

(provide-theme 'hima)

;; Local Variables:
;; no-byte-compile: t
;; End:
