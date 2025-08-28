;;; vcs.el --- Version control related -*- lexical-binding: t; -*-

;;; Commentary:
;; Version control related

;;; Code:
;; magit dependency
(use-package cond-let :ensure (:host github :repo "tarsius/cond-let"))
(use-package transient :ensure t :defer t)

;; Magit
(use-package magit
  :ensure t
  :after (evil-leader transient cond-let)
  :commands (magit-status magit-commit-create magit-ignored-files meain/git-how-was-it)
  :init
  (evil-leader/set-key "gg" 'magit-status)
  (evil-leader/set-key "gc" 'magit-commit-create)
  (evil-leader/set-key "gB" 'magit-blame)
  (evil-leader/set-key "gb" 'magit-branch)
  (evil-leader/set-key "gG" 'magit-show-commit)
  (evil-leader/set-key "gT" 'magit-log-trace-definition)
  :config
  (evil-define-key 'normal magit-status-mode-map (kbd ";") 'magit-stage)
  (evil-define-key 'visual magit-status-mode-map (kbd ";") 'magit-stage)

  ;; make <escape> quit(go back one level) in magit popups
  (define-key transient-map (kbd "<escape>") 'transient-quit-one)
  (setq magit-diff-refine-hunk (quote all))
  (define-key magit-mode-map (kbd "M-w") 'delete-window)
  (setq magit-completing-read-function #'completing-read)

  (defun meain/git-how-was-it ()
    (interactive)
    (let* ((filepath (magit-file-relative-name))
           (filename (file-name-nondirectory (magit-file-relative-name)))
           (line-no (line-number-at-pos))
           (mm major-mode)
           (branch (completing-read "Branch: " (magit-list-local-branch-names)))
           (buffer (get-buffer-create (format "*git-how-was-it %s:%s*" branch filename))))
      (message (concat "git show " branch ":" filepath))
      (other-window 1)
      (switch-to-buffer buffer)
      (erase-buffer)
      (insert (shell-command-to-string (concat "git show " branch ":" filepath)))
      (goto-line line-no) ;; will be different, but just a start
      (funcall mm))))

;; Structural diff using difftastic
(use-package difftastic
  :disabled t
  :ensure (:host github :repo "pkryger/difftastic.el")
  :config
  (eval-after-load 'magit-diff
    '(transient-append-suffix 'magit-diff '(-1 -1)
       [("D" "Difftastic diff (dwim)" difftastic-magit-diff)
        ("S" "Difftastic show" difftastic-magit-show)])))

(use-package ediff
  :after (evil-leader)
  :defer t
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq ediff-split-window-function 'split-window-horizontally))

(use-package smerge-mode
  :after (evil evil-leader)
  :config
  ;; Builtin smerge mode function has some issues (override it)
  ;; TODO: Submit bug to bug-gnu-emacs once verified
  (defun smerge-keep-n (n)
    (let* ((match-begin-0 (match-beginning 0))
           (match-begin-n (match-beginning n))
           (match-end-0 (match-end 0))
           (match-end-n (match-end n)))
      (smerge-remove-props match-begin-0 match-end-0)
      (delete-region match-end-n match-end-0)
      (delete-region match-begin-0 match-begin-n)))
  :init
  (evil-leader/set-key "gmm" 'smerge-mode)
  (evil-leader/set-key "gme" 'smerge-ediff)
  (evil-leader/set-key "gmr" 'smerge-refine)
  (evil-leader/set-key "gmn" 'smerge-next)
  (evil-leader/set-key "gmp" 'smerge-prev)
  (evil-leader/set-key "gmu" 'smerge-keep-upper)
  (evil-leader/set-key "gml" 'smerge-keep-lower)
  (evil-leader/set-key "gma" 'smerge-keep-all))

;; Diff hl
(use-package diff-hl
  :ensure t
  :defer 1
  :after evil-leader
  :config
  (diff-hl-flydiff-mode)
  (global-diff-hl-mode)
  (let* ((height (frame-char-height)) (width 2) (bits (make-vector height 0)))
    (define-fringe-bitmap 'my-diff-hl-bitmap bits height width))
  (setq diff-hl-fringe-bmp-function (lambda (type pos) 'my-diff-hl-bitmap))
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode-unless-remote)
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  (evil-set-command-property 'diff-hl-revert-hunk :jump t)
  (evil-set-command-property 'diff-hl-next-hunk :jump t)
  (evil-set-command-property 'diff-hl-previous-hunk :jump t)
  (evil-leader/set-key "gs" 'diff-hl-show-hunk)
  (evil-leader/set-key "gr" 'diff-hl-revert-hunk)
  (evil-leader/set-key "gj" 'diff-hl-next-hunk)
  (evil-leader/set-key "gk" 'diff-hl-previous-hunk)
  (evil-leader/set-key "gn" 'diff-hl-next-hunk)
  (evil-leader/set-key "gp" 'diff-hl-previous-hunk))

;; Git blame info
(use-package blamer
  :ensure t
  :after evil-leader
  :commands (blamer-show-commit-info blamer-mode global-blamer-mode)
  :config
  (setq blamer-idle-time 0.1)
  (setq blamer-min-offset 30)
  (setq blamer-commit-formatter ":: %s")
  (set-face-attribute 'blamer-face nil :height 0.9)
  (setq blamer-max-commit-message-length 90)
  (setq blamer-border-lines '(?+ ?- ?+ ?| ?+ ?+ )) ;; default one creates issues with spacing
  :init (evil-leader/set-key "G" 'blamer-show-commit-info))


(provide 'vcs)
;;; vcs.el ends here
