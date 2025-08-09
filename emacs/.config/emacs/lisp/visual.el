;;; visual.el --- Visual configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Anything that is related to how Emacs looks

;;; Code:
;; Set up fonts
(use-package emacs
  :commands (meain/select-font meain/what-font-am-i-using)
  :config
  (defun meain/select-font ()
    "Select and set a font."
    (interactive)
    (let ((font-name (completing-read "Choose font: " (cl-remove-duplicates (font-family-list)))))
      (let ((family (meain/get-font-prop font-name 'family))
            (weight (meain/get-font-prop font-name 'weight)))
        (set-frame-font family)
        (set-face-attribute 'default nil :font family :weight weight)
        (set-face-attribute 'fixed-pitch nil :font family :weight weight)
        (set-face-attribute 'variable-pitch nil :font family :weight weight)
        (setq-default line-spacing (meain/get-font-prop font-name 'line-spacing)))))
  (defun meain/what-font-am-i-using ()
    "Show the name/details for the current font in use."
    (interactive)
    (message "%s" (face-attribute 'default :font))))

;; Bell: audio -> visual
(use-package emacs
  :config
  (setq visible-bell nil)
  (setq ring-bell-function (lambda ()
                             (unless (memq this-command
                                           '(isearch-abort abort-recursive-edit exit-minibuffer keyboard-quit))
                               (invert-face 'mode-line)
                               (invert-face 'header-line)
                               (run-with-timer 0.1 nil 'invert-face 'mode-line)
                               (run-with-timer 0.1 nil 'invert-face 'header-line)))))

;; Theme
(setq hima-theme-load-path (concat (getenv "HOME") "/dev/src/hima-theme"))
(use-package hima-theme
  :load-path hima-theme-load-path
  :config
  (load-theme 'hima t))

;; Auto resize windows (useful in go buffer, folks don't stop at 80)
(use-package golden-ratio
  :ensure t
  ;; Enable minor-mode manually when required
  :commands (golden-ratio golden-ratio-mode))

;; Show open and closing brackets
(use-package show-paren
  :defer t
  :config
  (setq show-paren-delay 0)
  (setq show-paren-context-when-offscreen t)
  (setq show-paren-style 'parenthesis)
  (show-paren-mode t))

(use-package which-func :commands (which-function))

(provide 'visual)
;;; visual.el ends here
