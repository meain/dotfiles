;;; terminals.el --- Ternaimals related stuff -*- lexical-binding: t; -*-

;;; Commentary:
;; Ternaimals related stuff

;;; Code:
;; Eat terminal
;; TODO: Configure M-hjkl for split navigation
(use-package eat
  :ensure (:type git
                 :host codeberg
                 :repo "akib/emacs-eat"
                 :files ("*.el" ("term" "term/*.el") "*.texi"
                         "*.ti" ("terminfo/e" "terminfo/e/*")
                         ("terminfo/65" "terminfo/65/*")
                         ("integration" "integration/*")
                         (:exclude ".dir-locals.el" "*-tests.el")))
  :config
  (add-hook 'eat-mode-hook (lambda () (setq-local show-trailing-whitespace nil))))

;; Eshell config
(use-package eshell
  :init (global-set-key (kbd "M-t") 'meain/eshell-toggle)
  :after (vc copilot)
  :commands (meain/eshell-toggle eshell)
  :config
  (add-hook 'eshell-mode-hook (lambda () (copilot-mode -1)))
  (defun meain/eshell-name ()
    "Get the name of the eshell based on project info."
    (format "*popup-eshell-%s*"
            (if (project-current) (meain/project-name) "-")))
  (defun meain/eshell-toggle ()
    (interactive)
    (if (s-starts-with-p "*popup-eshell" (buffer-name))
        (quit-window)
      (let ((existing-eshell (get-buffer (meain/eshell-name))))
        (if existing-eshell (pop-to-buffer (meain/eshell-name))
          (progn
            (eshell t)
            (rename-buffer (meain/eshell-name)))))))
  (setq eshell-prompt-function
        (lambda ()
          (concat
           (propertize (string-join
                        (reverse
                         (ntake 2 (reverse
                                   (split-string
                                    (eshell/pwd) "/")))) "/")
                       'face `(:foreground "#93a1a1"))
           (propertize (if (car (vc-git-branches))
                           (concat "[" (car (vc-git-branches)) "]")
                         "") 'face `(:foreground "#93a1a1"))
           " ")))
  (add-hook 'eshell-mode-hook (lambda ()
                                (setenv "TERM" "xterm-256color")
                                (define-key eshell-mode-map (kbd "M-l") 'meain/move-swap-right)
                                (define-key eshell-mode-map (kbd "M-h") 'meain/move-swap-left)
                                (define-key eshell-mode-map (kbd "M-k") 'meain/move-swap-up)
                                (define-key eshell-mode-map (kbd "M-j") 'meain/move-swap-down))))


(provide 'terminals)
;;; terminals.el ends here
