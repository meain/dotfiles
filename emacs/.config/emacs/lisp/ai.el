;;; ai.el --- LLM related packages -*- lexical-binding: t; -*-

;;; Commentary:
;; Includes all the llm related packages except for gptel and yap
;; related things.  They both have relatively big configs that it
;; makes sense to separate them into individual config files

;;; Code:
;; Copilot
(use-package copilot
  :defer t
  :after jsonrpc
  :ensure (:host github
                 :repo "zerolfx/copilot.el"
                 :files ("dist" "*.el"))
  :config
  ;; https://github.com/copilot-emacs/copilot.el/issues/226
  (defun copilot-turn-on-unless-buffer-read-only ()
    "Turn on `copilot-mode' if the buffer is writable."
    (unless buffer-read-only (copilot-mode 1)))
  (add-hook 'text-mode-hook #'copilot-turn-on-unless-buffer-read-only)
  (add-hook 'prog-mode-hook #'copilot-turn-on-unless-buffer-read-only)

  (setq copilot-idle-delay 0)
  (setq copilot-max-char -1)

  ;; Suppress indentation warning from copilot
  ;; https://github.com/zerolfx/copilot.el/pull/212#issuecomment-1862487382
  (add-to-list 'warning-suppress-types '(copilot copilot-no-mode-indent))

  (define-key copilot-mode-map (kbd "M-f M-f") #'copilot-complete)
  (define-key copilot-mode-map (kbd "M-f M-j") #'copilot-next-completion)
  (define-key copilot-mode-map (kbd "M-f M-k") #'copilot-previous-completion)
  (define-key copilot-mode-map (kbd "M-f M-;") #'copilot-accept-completion-by-line)
  (define-key copilot-mode-map (kbd "M-f M-l") #'copilot-accept-completion))

;; Copilot chat (just use gptel)
(use-package copilot-chat
  :defer t
  :after (request)
  :ensure (:host github :repo "chep/copilot-chat.el" :files ("*.el"))
  :disabled t
  :config
  ;; From https://github.com/chep/copilot-chat.el/issues/24
  (defun meain/copilot-chat-display (prefix)
    "Opens the Copilot chat window, adding the current buffer to the context.

Called with a PREFIX, resets the context buffer list before opening"
    (interactive "P")

    (require 'copilot-chat)
    (let ((buf (current-buffer)))

      ;; Explicit reset before doing anything, avoid it resetting later on
      ;; target-fn and ignoring the added buffers
      (unless (copilot-chat--ready-p)
        (copilot-chat-reset))

      (when prefix (copilot-chat--clear-buffers))

      (copilot-chat--add-buffer buf)
      (copilot-chat-display))))

;; Aider from Emacs (termainl version is better)
(use-package aidermacs
  :ensure (:host github :repo "MatthewZMD/aidermacs")
  :disabled t
  :commands (aidermacs-transient-menu)
  :after (evil)
  :config
  (setq aidermacs-auto-commits nil)
  (setq aidermacs-use-architect-mode nil)
  (setq aidermacs-backend 'comint)
  :init
  (define-key evil-normal-state-map (kbd "<SPC> a") 'aidermacs-transient-menu))

;; Claude Code IDE
(use-package claude-code-ide
  :ensure (:type git :host github :repo "manzaltu/claude-code-ide.el")
  :disabled t
  :bind ("C-c C-'" . claude-code-ide-menu) ; Set your favorite keybinding
  :config
  (setq claude-code-ide-cli-path ",claude-code")
  (setq claude-code-ide-terminal-backend 'eat)
  (claude-code-ide-emacs-tools-setup)) ; Optionally enable Emacs MCP tools

(provide 'ai)
;;; ai.el ends here
