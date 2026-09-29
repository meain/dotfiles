;;; yap-m.el --- Personal config for yap -*- lexical-binding: t; -*-

;;; Commentary:
;; This file contains personal configuration for yap, a package for
;; interacting with dem llms.

;;; Code:
(use-package llm :ensure t)
(use-package yap
  :load-path "/Users/meain/dev/src/yap"
  :after (llm)
  :config
  (setq yap-service "anthropic")
  (setq yap-model "claude-sonnet-5-5")

  (setq yap-api-key:groq groq-api-key)
  (setq yap-api-key:openrouter openrouter-api-key)
  (setq yap-api-key:openai openai-api-key)
  (setq yap-api-key:anthropic anthropic-api-key)
  (setq yap-log-requests "/Users/meain/.cache/yap")

  ;; Add window rules for *yap-response* buffer so that it shows up at
  ;; top of the frame
  (add-to-list 'display-buffer-alist
               `(,(rx bos "*yap-response*" eos)
                 (display-buffer-reuse-window
                  display-buffer-in-side-window)
                 (reusable-frames . visible)
                 (side            . top)
                 (window-height   . 0.3)))

  (defun meain/yap-pick-model ()
    "Pick a model from a list of preferred models."
    (interactive)
    (let* ((models '(("groq:llama4-scout" . ("groq" "meta-llama/llama-4-scout-17b-16e-instruct"))
                     ("openrouter:deepseek-v4-flash" . ("openrouter" "~deepseek/deepseek-v4-flash-latest"))
                     ("openrouter:flash" . ("openrouter" "google/gemini-2.0-flash-exp:free"))
                     ("openrouter:gpt-5.6-luna" . ("openrouter" "openai/gpt-5.6-luna"))
                     ("anthropic:sonnet-5-5" . ("anthropic" "claude-sonnet-5-5"))
                     ("anthropic:opus-5" . ("anthropic" "claude-opus-5"))
                     ("anthropic:haiku-4-5" . ("anthropic" "claude-haiku-4-5"))))
           (name (completing-read "Model: " models nil t))
           (vals (cdr (assoc name models))))
      (when vals
        (setq yap-llm-provider-override nil)
        (setq yap-service (car vals))
        (setq yap-model (cadr vals)))))

  (defun meain/yap-use-openrouter-free ()
    (interactive)
    (let ((models (seq-filter (lambda (x) (string-suffix-p ":free" x))
                              (yap--get-models:openrouter))))
      (setq yap-llm-provider-override nil
            yap-service "openrouter"
            yap-model (completing-read "Model: " models))))

  (defun meain/yap-use-copilot-llm ()
    (interactive)
    (setq yap-service "copilot"
          yap-model "c:gpt-4.1"
          yap-llm-provider-override
          (make-llm-openai-compatible
           :chat-model "gpt-4.1"
           :url "http://localhost:4141/v1")))

  (defun meain/get-llm-prompt (name)
    "Get the prompt for NAME."
    (with-temp-buffer
      (insert-file-contents
       (concat (getenv "HOME") "/.config/datafiles/prompts/user/" name ".md"))
      (buffer-string)))

  (cl-loop for name in '(identify-actionable-change message-response review-diff)
           do (let ((n name))
                (add-to-list
                 'yap-templates
                 (cons n (lambda ()
                           (yap-template-prompt (meain/get-llm-prompt (symbol-name n))))))))

  (defun meain/select-enclosing-defun ()
    (unless (use-region-p)
      (let ((bounds (bounds-of-thing-at-point 'defun)))
        (when bounds
          (goto-char (car bounds))
          (push-mark (cdr bounds) t t)))))

  :init
  (global-unset-key (kbd "M-m"))
  (global-set-key (kbd "M-m M-c") 'yap-buffer-toggle)
  (global-set-key (kbd "M-m M-m") 'yap-prompt)
  (global-set-key (kbd "M-m M-r") 'yap-rewrite)
  (global-set-key (kbd "M-m M-w") 'yap-write)
  (global-set-key (kbd "M-m M-o") (lambda () (interactive) (yap-rewrite 'optimize-code)))
  (global-set-key (kbd "M-m M-i")
                  (lambda ()
                    (interactive)
                    (meain/select-enclosing-defun)
                    (yap-rewrite 'identify-actionable-change)))
  (global-set-key (kbd "M-m M-f") (lambda () (interactive) (yap-rewrite 'fix-diagnostic-error)))
  (global-set-key (kbd "M-m M-e") (lambda () (interactive) (yap-prompt 'explain-code))))


(provide 'yap-m)
;;; yap-m.el ends here
