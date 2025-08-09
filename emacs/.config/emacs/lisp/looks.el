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

(provide 'looks)
;;; looks.el ends here
