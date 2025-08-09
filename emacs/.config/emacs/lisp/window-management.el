;;; window-management.el --- Window management related functinos -*- lexical-binding: t; -*-

;;; Commentary:
;; Anything related to managing windows or buffers.

;;; Code:
;; Shrink and enlarge windows (not contextual as of now)
;; https://www.emacswiki.org/emacs/WindowResize
(global-set-key (kbd "M-H") (ilambda shrink-window-horizontally 5))
(global-set-key (kbd "M-L") (ilambda enlarge-window-horizontally 5))
(global-set-key (kbd "M-K") (ilambda shrink-window 5))
(global-set-key (kbd "M-J") (ilambda enlarge-window 5))

;; Switch to other frame
(use-package frame
  :after evil-leader
  :defer t
  :config
  (global-set-key (kbd "M-o") 'other-frame))

(use-package emacs
  :commands (meain/move-or-swap
             meain/move-swap-right
             meain/move-swap-left
             meain/move-swap-up
             meain/move-swap-down
             meain/split-window)
  :config
  (defun meain/move-or-swap (direction &optional swap)
    "Move to or swap window in DIRECTION (up/down/left/right) based on SWAP."
    (funcall (if swap
                 (intern (format "windmove-swap-states-%s" direction))
               (intern (format "windmove-%s" direction)))))

  ;; These are used a lot and so we need to defined these as functions
  (defun meain/move-swap-right () (interactive) (meain/move-or-swap "right"))
  (defun meain/move-swap-left () (interactive) (meain/move-or-swap "left"))
  (defun meain/move-swap-up () (interactive) (meain/move-or-swap "up"))
  (defun meain/move-swap-down () (interactive) (meain/move-or-swap "down"))

  (global-set-key (kbd "M-l") 'meain/move-swap-right)
  (global-set-key (kbd "M-h") 'meain/move-swap-left)
  (global-set-key (kbd "M-k") 'meain/move-swap-up)
  (global-set-key (kbd "M-j") 'meain/move-swap-down)

  (defun meain/split-window (direction &optional open-term)
    "Split window in DIRECTION and optionally OPEN-TERM."
    (funcall (intern (format "split-window-%s" direction)))
    (funcall (intern (format "windmove-%s" (if (equal direction "below") "down" "right"))))
    (when open-term (meain/eshell-toggle)))

  :init
  (global-set-key (kbd "M-b") (lambda (&optional term)
                                (interactive "P")
                                (meain/split-window "below" term)))
  (global-set-key (kbd "M-v") (lambda (&optional term)
                                (interactive "P")
                                (meain/split-window "right" term)))
  (global-set-key (kbd "M-w") 'delete-window))

;; Midnight: Kill unused buffers at midnight
(use-package midnight
  :defer t
  :config
  (setq clean-buffer-list-delay-general 1)
  (midnight-mode t))

;; ibuffer
(use-package ibuffer
  :commands (ibuffer ibuffer-other-window)
  :init
  (setq ibuffer-expert t)
  (global-set-key (kbd "M-c")
                  (alambda (call-interactively 'switch-to-buffer)
                           (ibuffer-other-window))))

;; Winner mode
(use-package winner
  :defer nil
  :config
  (global-set-key (kbd "M-f <left>") 'winner-undo)
  (global-set-key (kbd "M-f <right>") 'winner-redo)
  (winner-mode))

;; Window layout changer
(use-package rotate
  :ensure t
  :after evil
  :commands (rotate-layout rotate-window)
  :init
  (define-key evil-normal-state-map (kbd "M-f <SPC>") 'rotate-layout))

(provide 'window-management)
;;; window-management.el ends here
