;;; gptel-m.el --- Configuratino for gptel -*- lexical-binding: t; -*-

;;; Commentary:
;; Primarily module for gptel and related package.  We do however load
;; in things like tools and presents from other files.

;;; Code:
(use-package gptel
  :ensure t
  ;; :load-path "/Users/meain/dev/src/gptel"
  :commands (gptel gptel-send gptel-rewrite-menu)
  :config
  ;; (setq gptel-model 'claude-3.7-sonnet)
  (setq gptel-model 'gpt-4.1)
  (setq gptel-backend (gptel-make-gh-copilot "Copilot"))

  ;; Some configuration
  (setq gptel-api-key openai-api-key)
  (setq gptel-expert-commands t)
  (setq gptel-use-tools t)
  (setq gptel-include-tool-results t)
  (setq gptel-default-mode 'org-mode) ;; tool calls don't fold in markdown mode
  (setq gptel-prompt-prefix-alist '((markdown-mode . "🗣️ YOU\n") (org-mode . "* 🗣️ YOU\n") (text-mode . "# YOU\n")))
  (setq gptel-response-prefix-alist '((markdown-mode . "🤖 BOT\n") (org-mode . "* 🤖 BOT\n") (text-mode . "$ BOT\n")))

  ;; Add gptel mode keybinds
  (define-key gptel-mode-map (kbd "C-<return>") 'gptel-send)
  (define-key gptel-mode-map (kbd "C-c C-k") 'gptel-abort)

  ;; Hooks to update behaviour
  ;; (add-hook 'gptel-post-response-functions 'gptel-end-of-response) ;; jump to end of generated text
  (add-hook 'gptel-mode-hook (lambda ()
                               (toggle-truncate-lines nil)
                               (setq-local show-trailing-whitespace nil)))

  (gptel-make-openai "Github Models"
    :host "models.inference.ai.azure.com"
    :endpoint "/chat/completions?api-version=2024-05-01-preview"
    :stream t
    :key github-models-api-key
    :models '(gpt-4o gpt-4o-mini))

  (gptel-make-openai "Groq"
    :host "api.groq.com"
    :endpoint "/openai/v1/chat/completions"
    :stream t
    :key groq-api-key
    :models '(llama-3.1-70b-versatile
              openai/gpt-oss-20b
              openai/gpt-oss-120b
              llama-3.1-8b-instant
              llama3-70b-8192
              llama3-8b-8192
              mixtral-8x7b-32768
              gemma-7b-it))

  (gptel-make-anthropic "Claude"
    :stream t
    :key anthropic-api-key)

  (defun gptel-context-clear-all ()
    (interactive)
    (gptel-add -1))

  (defun gptel-context-add-website (url &optional no-cache)
    "Add content from a website to the GPTel context.
URL is the website address to fetch content from.
When NO-CACHE is non-nil, force fetching fresh content even if cached."
    (interactive "sURL: ")
    (let ((buffer-name (format "*gptel-context-website:%s*" url)))
      (with-current-buffer (get-buffer-create buffer-name)
        (if (and (not no-cache) (> (buffer-size) 0))
            (gptel-add)
          (let* ((url-buffer (url-retrieve-synchronously url))
                 (content-without-header (with-current-buffer url-buffer
                                           (buffer-substring-no-properties
                                            (search-forward "\n\n")
                                            (point-max)))))
            (erase-buffer)
            (insert content-without-header)
            (gptel-add))))))

  (defun gptel-context-add-shell-command (command &optional cache)
    "Add context to gptel from the output of a shell command.
If CACHE is non-nil, the output is cached."
    (interactive "sCommand: ")
    (let ((buffer (get-buffer-create (format "*gptel-context-shell:%s*" command))))
      (with-current-buffer buffer
        (if cache
            (when (> (buffer-size) 0)
              (gptel-add))
          (erase-buffer)
          (insert (shell-command-to-string command))
          (gptel-add)))))

  (defun gptel-context-add-website-content (url &optional no-cache)
    "Add context to gptel from the readable content of a website URL.
For optional NO-CACHE, use caching by default."
    (interactive "sEnter website URL: ")
    (gptel-context-add-shell-command (format "readable %s" url) (not no-cache)))

  ;; Tools & Presets
  (require 'gptel-tools)
  (require 'gptel-presets)

  (defvar gptel-lookup--history nil)
  (defun gptel-lookup (prompt)
    "Quick lookup for `PROMPT' using gptel."
    (interactive (list (read-string "Q: " nil gptel-lookup--history)))
    (when (string= prompt "") (user-error "A prompt is required"))
    (gptel-request
     (concat (and (use-region-p)
                  (buffer-substring-no-properties (region-beginning) (region-end)))
             "\n\n"
             prompt)
     :callback
     (lambda (response info)
       (if (not response)
           (message "gptel-lookup failed with message: %s" (plist-get info :status))
         (with-current-buffer (get-buffer-create "*gptel-lookup*")
           (let ((inhibit-read-only t))
             (erase-buffer)
             (insert response))
           (gfm-mode)
           (goto-char (point-min))
           (display-buffer (current-buffer)
                           `((display-buffer-in-side-window)
                             (reusable-frames . visible)
                             (side            . top)
                             (window-height . 0.2))))))))

  (defun gptel-get-user-queries ()
    "Return a list of user queries (prompts) from the current gptel buffer.
Skips regions marked as LLM responses (with 'gptel property set to 'response)
and tool calls (with 'gptel property whose car is 'tool)."
    (let ((queries '())
          (pos (point-min)))
      (while (< pos (point-max))
        (let* ((next-change (next-single-property-change pos 'gptel nil (point-max)))
               (prop (get-text-property pos 'gptel)))
          (cond
           ((or (eq prop 'response)
                (and (consp prop) (eq (car prop) 'tool)))
            ;; Skip response and tool call regions
            (setq pos next-change))
           (t
            ;; Collect user query region
            (let ((end (or next-change (point-max))))
              (push (string-trim (buffer-substring-no-properties pos end)) queries)
              (setq pos end))))))
      (nreverse (cl-remove-if #'string-empty-p queries))))

  (defun meain/gptel-rename-chat-buffer (&optional callback)
    "Extract user queries from current buffer and send them to an LLM using `gptel-request`.
The LLM will suggest filenames based on the themes discussed.
If CALLBACK is non-nil, it will be called with the result.

Just sending user messages for two reasons:
- It will be much smaller
- And more importantly, sending the full buffer produced bad filenames"
    (interactive)
    (message "Figuring out a good filename for this discussion...")
    (let* ((user-messages (gptel-get-user-queries))
           (prompt (format "Suggest concise filenames that reflect the main themes from the questions given in the context below.
Return a list of filenames only, one per line without extension.

<context>
%s
</context>
"
                           (mapconcat #'identity user-messages "\n"))))
      (gptel-request
       prompt
       :callback (or callback
                     (lambda (response _info)
                       (let ((filenames (cl-remove-if #'string-empty-p
                                                      (mapcar #'string-trim
                                                              (split-string response "\n" t)))))
                         (rename-visited-file
                          (concat (expand-file-name "~/.local/share/llm-discussions/")
                                  (completing-read "Pick a filename: " filenames nil t)
                                  "."
                                  (cond
                                   ((derived-mode-p 'org-mode) "org")
                                   ((derived-mode-p 'markdown-mode) "md")
                                   (t "txt"))))
                         filenames))))))

  :init
  (global-unset-key (kbd "M-;"))
  (global-set-key (kbd "M-; m") 'gptel)
  (global-set-key (kbd "M-; M-m") 'gptel)
  (global-set-key (kbd "M-; s") 'gptel-send)
  (global-set-key (kbd "M-; p") 'gptel-lookup)
  (global-set-key (kbd "M-; c") 'gptel-context-clear-all)
  (global-set-key (kbd "M-; a") 'gptel-add)
  (global-set-key (kbd "M-; i") 'gptel-menu)
  (global-set-key (kbd "M-; M-i") 'gptel-menu)
  (global-set-key (kbd "M-; r") 'gptel-rewrite)
  (global-set-key (kbd "M-; M-r") 'gptel-rewrite))

(use-package gptel-prompts
  :ensure (:host github :repo "jwiegley/gptel-prompts")
  :after (gptel)
  :demand t
  :config
  (setq gptel-prompts-directory
        (concat (getenv "HOME") "/.config/datafiles/prompts/system"))
  ;; Ensure prompts are updated if prompt files change
  ;; (gptel-prompts-add-update-watchers)
  (gptel-prompts-update))

(use-package gptel-quick
  :ensure (:host github :repo "karthink/gptel-quick")
  :after (gptel)
  :commands (gptel-quick)
  :config
  (setq gptel-quick-timeout 100)
  :init
  (global-set-key (kbd "M-; j") 'gptel-quick))

(provide 'gptel-m)
;;; gptel-m.el ends here