;;; terminals.el --- Ternaimals related stuff -*- lexical-binding: t; -*-

;;; Commentary:
;; Ternaimals related stuff

;;; Code:
;; Compile and recompile
(use-package compile
  :commands (compile recompile)
  :after (evil)
  :config
  (setq compilation-always-kill t)
  (setq compilation-ask-about-save nil)
  (setq compilation-scroll-output nil)

  (setq compilation-scroll-output 'first-error) ; or nil
  ;; (setq-default compilation-auto-jump-to-first-error nil)

  ;; Ensure ansi escape sequences are rendered properly
  (setq ansi-color-for-compilation-mode t)
  (add-hook 'compilation-filter-hook #'ansi-color-compilation-filter)

  (defun meain/toggle-compilation-scroll-output ()
    "Toggle `compilation-scroll-output'."
    (interactive)
    (setq compilation-scroll-output (not compilation-scroll-output))
    (message "Compilation scroll set to %s" compilation-scroll-output))
  (evil-set-initial-state 'comint-mode 'normal)
  (defun meain/prettify-compilation (&rest _)
    "Few thing to prettify compilation buffer."
    (with-current-buffer "*compilation*"
      (toggle-truncate-lines -1)
      (highlight-regexp "FAIL: .*" 'diff-refine-removed)
      (highlight-regexp "=== RUN .*" 'ffap)))
  (advice-add 'compile :after 'meain/prettify-compilation)
  (defun meain/compilation-colorcode (_buffer string)
    "Change background color of compilation `_BUFFER' to red on failure."
    (if (string-prefix-p "finished" string)
        (face-remap-add-relative 'default 'diff-hl-insert)
      (face-remap-add-relative 'default 'diff-hl-delete)))
  (add-to-list 'compilation-finish-functions 'meain/compilation-colorcode)
  (defun meain/run-default ()
    (interactive)
    (compile (concat ".mscripts/default "
                     (read-string ".mscripts/default "))))
  :init
  (evil-leader/set-key "r"
    (alambda
     (call-interactively 'recompile)
     (call-interactively 'compile))))

(use-package comint
  :defer t
  :config
  (add-hook 'comint-mode-hook (lambda () (setq-local show-trailing-whitespace nil))))

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

;; Show the last command in the shell at top
(use-package sticky-shell
  :ensure (sticky-shell :host github
                        :repo "andyjda/sticky-shell")
  :after eshell
  :commands (sticky-shell-mode)
  :init
  (add-hook 'eshell-mode-hook 'sticky-shell-mode))

;; tramp-term
(use-package tramp-term
  :after tramp
  :ensure t
  :commands (tramp-term meain/tramp-shell)
  :config
  (defun meain/tramp-shell ()
    "SSH into a server by selecting a host via autocomplete."
    (interactive)
    (tramp-term (list (meain/ssh-host-picker)))))

(provide 'terminals)
;;; terminals.el ends here
