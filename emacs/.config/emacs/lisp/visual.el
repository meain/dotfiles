;;; visual.el --- Visual configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Anything that is related to how Emacs looks

;;; Code:
;; Set up fonts
(use-package emacs
  :commands (meain/set-font
             meain/current-font
             meain/reduce-font-size
             meain/increse-font-size
             meain/reset-font)
  :config
  (defun meain/set-font (&optional name)
    "Select and set a font."
    (interactive)
    (let ((font-name (or name
                         (completing-read "Choose font: " (cl-remove-duplicates (font-family-list))))))
      (let ((height (meain/get-font-prop font-name 'height))
            (weight (meain/get-font-prop font-name 'weight)))
        (message "Setting font to %s, height: %s, weight: %s" font-name height weight)
        (set-frame-font font-name)
        (set-face-attribute 'default nil :font font-name :weight weight :height height)
        (set-face-attribute 'fixed-pitch nil :inherit 'default)
        (set-face-attribute 'variable-pitch nil :inherit 'default)
        (setq-default line-spacing (meain/get-font-prop font-name 'line-spacing)))))
  (defun meain/reduce-font-size (&optional reverse)
    "Reduce the font size by 1 point. If REVERSE is non-nil, increase it instead."
    (interactive "P")
    (let* ((current-size (face-attribute 'default :height))
           (new-size (if reverse
                         (+ current-size 10)
                       (- current-size 10))))
      (set-face-attribute 'default nil :height new-size)
      (set-face-attribute 'fixed-pitch nil :height new-size)
      (set-face-attribute 'variable-pitch nil :height new-size)))
  (defun meain/increse-font-size ()
    "Increase the font size by 1 point."
    (interactive)
    (meain/reduce-font-size t))
  (defun meain/change-font-weight ()
    "Toggle between available font weights for the current font."
    (interactive)
    (let* ((weights (cl-remove-duplicates
                     (mapcar (lambda (f)
                               (font-get (font-spec :name f) :weight))
                             (x-list-fonts (face-attribute 'default :family)))
                     :test #'equal))
           (current-weight (face-attribute 'default :weight))
           (other-weights (cl-remove-if (lambda (w) (equal w current-weight)) weights))
           (new-weight (intern (completing-read
                                (format "New weight[%s]: " current-weight)
                                other-weights nil t))))
      (set-face-attribute 'default nil :weight new-weight)
      (set-face-attribute 'fixed-pitch nil :weight new-weight)
      (set-face-attribute 'variable-pitch nil :weight new-weight)))
  (defun meain/reset-font ()
    "Reset the font size to default size."
    (interactive)
    (let ((font-name (face-attribute 'default :family)))
      (meain/set-font font-name)))
  (defun meain/current-font ()
    "Show the name/details for the current font in use."
    (interactive)
    (message "%s | height: %s | weight: %s"
             (face-attribute 'default :family)
             (face-attribute 'default :height)
             (face-attribute 'default :weight))))

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
