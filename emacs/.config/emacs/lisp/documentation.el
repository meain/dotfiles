;;; documentation.el --- Lookup documentation -*- lexical-binding: t; -*-

;;; Commentary:
;; Packages/Functions related to looking up documentation

;;; Code:
;; eldoc load
(use-package eldoc
  :defer t
  :after (evil)
  :config
  (setq eldoc-echo-area-use-multiline-p nil)
  (define-key evil-normal-state-map (kbd "K") 'eldoc-print-current-symbol-info)
  (global-eldoc-mode nil))

;; Show eldoc messages in a popup at point
(use-package eldoc-box
  :ensure t
  :commands (eldoc-box-help-at-point eldoc-box-hover-mode eldoc-box-hover-at-point-mode)
  :init
  (global-set-key (kbd "M-d")
                  (lambda ()
                    (interactive)
                    (let ((eldoc-echo-area-use-multiline-p t))
                      (call-interactively #'eldoc-box-help-at-point)))))

;; Helpful package
(use-package helpful
  :ensure t
  :after evil-leader
  :commands (helpful-callable helpful-variable helpful-at-point helpful-key)
  :init
  (global-set-key (kbd "C-h f") #'helpful-callable)
  (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h k") #'helpful-key)
  (global-set-key (kbd "C-h x") #'helpful-command)
  (global-set-key (kbd "C-h o") #'helpful-symbol)
  (global-set-key (kbd "C-c C-d") #'helpful-at-point)

  ;; find-library is not helpful related, but good to map in the same
  ;; block along with the helpful keys as they have similar keybinds
  (global-set-key (kbd "C-h l") #'find-library))

;; which-key mode
;; https://www.matem.unam.mx/~omar/apropos-emacs.html#the-case-against-which-key-a-polemic
(use-package which-key
  :ensure t
  :defer nil
  :config
  ;; Only show up if explicitly requested
  (setq which-key-show-early-on-C-h t
        which-key-idle-delay 1e6 ; 11 days
        which-key-idle-secondary-delay 0.05)
  (which-key-mode))

(provide 'documentation)
;;; documentation.el ends here
