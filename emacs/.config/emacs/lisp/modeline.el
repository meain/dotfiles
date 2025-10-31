;;; modeline.el --- Modline setup -*- lexical-binding: t; -*-

;;; Commentary:
;; Setup up a custom modeline.

;;; Code:
;; Better modeline
(use-package mode-line-idle :ensure t :commands (mode-line-idle))
(defvar meain/modeline-project-color
  '(:eval
    (let* ((project-name (or (meain/project-name) ""))
           (hex-color (concat "#" (substring (md5 project-name) 0 6)))
           (rgb (color-name-to-rgb hex-color))
           (hsl (apply #'color-rgb-to-hsl rgb))
           (new-rgb (apply #'color-hsl-to-rgb (list (nth 0 hsl) 0.13 0.7)))
           (new-hex (apply #'color-rgb-to-hex new-rgb)))
      ;; TODO: encode vcs worktree information
      (propertize (format " %s " project-name) ;; Alt: use █ with foreground color
                  'font-lock-face (list :background new-hex)))))
(defvar meain/modeline-filename
  '(:eval (list (if (eq buffer-file-name nil) ""
                  (concat (file-name-nondirectory
                           (directory-file-name
                            (file-name-directory (buffer-file-name)))) "/"))
                (propertize "%b"
                            'face (if (buffer-modified-p)
                                      'font-lock-string-face
                                    'font-lock-builtin-face)
                            'help-echo (buffer-file-name)))))

(defun meain/modeline-segment (expr)
  "Create a modeline segment with `EXPR' expression."
  `(:eval (let ((value ,expr))
            (if value (propertize value 'face 'hima-simple-gray) ""))))
(defvar meain/modeline-vcs
  (meain/modeline-segment
   `(when-let (vc vc-mode)
      (or
       (meain/jj-current-workspace)
       (concat " @" (substring vc 5))))))
(defvar meain/modeline-yap
  (meain/modeline-segment
   `(when (or (boundp 'gptel-model) (boundp 'yap-model))
      (concat " ["
              (when (boundp 'yap-model) yap-model)
              "/"
              (when (boundp 'gptel-model) (format "%s" gptel-model))
              "]"))))

(setq-default
 mode-line-format
 (list
  '(:eval (mode-line-idle 0.3 meain/modeline-project-color "░"))
  '(:eval (if (eq 'emacs evil-state) "[E] " " ")) ;; vim or emacs mode
  meain/modeline-filename
  (propertize ":%l:%c")
  '(:eval (mode-line-idle 1.0 meain/modeline-vcs ""))
  '(:eval (mode-line-idle 1.0 meain/modeline-yap ""))
  '(:eval (if (boundp 'keycast-mode-line) keycast-mode-line))
  'mode-line-format-right-align
  '(:eval (if (boundp 'org-timer-mode-line-string) (concat org-timer-mode-line-string " ")))
  (propertize "%p") ;; position in file
  (propertize " %m ")
  " "))

(provide 'modeline)
;;; modeline.el ends here
