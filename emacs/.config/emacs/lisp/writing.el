;;; writing.el --- Writing related packages -*- lexical-binding: t; -*-

;;; Commentary:
;; Things that help me write prose in emacs.

;;; Code:
;; Markdown package configuration
(use-package markdown-mode
  :ensure t
  :defer t
  :after (edit-indirect evil)
  :mode ("\\.md\\'" . gfm-mode)
  :config
  (setq markdown-url-compose-char '(8230 8943 35 9733 9875))
  (setq markdown-enable-html -1)
  (setq markdown-gfm-use-electric-backquote nil) ; don't ask me to pick lang in ```
  (evil-define-key 'normal gfm-mode-map (kbd "<RET>") 'project-find-file)
  (evil-define-key 'normal gfm-mode-map (kbd "g d") 'markdown-do)
  (evil-define-key 'normal markdown-mode-map (kbd "<RET>") 'project-find-file)
  (evil-define-key 'normal markdown-mode-map (kbd "g d") 'markdown-do)
  (setq markdown-command "pandoc -t html5")
  (setq markdown-fontify-code-blocks-natively t)
  (setq-default markdown-hide-urls t) ;; Or call markdown-toggle-url-hiding

  (add-hook 'gfm-mode-hook #'toggle-truncate-lines)

  ;; When a link is pasted with an active selection, convert to a markdown link
  (defun meain/paste-after-or-create-link (from to)
    (interactive "r")
    (let ((clipboard-text (substring-no-properties (current-kill 0 t))))
      (if (ffap-url-p clipboard-text)
          (save-excursion
            (goto-char from)
            (insert "[")
            (goto-char (1+ to))
            (insert "](")
            (yank)
            (insert ")"))
        (delete-region from to)
        (yank))))
  (evil-define-key 'visual markdown-mode-map "p" 'meain/paste-after-or-create-link)

  ;; Quickly add markdown links to document
  (defun meain/markdown-linkify-thing (start end)
    "Function to search and add markdown links to document.
START and END for position."
    (interactive "r")
    (let* ((orig-thang (if (use-region-p)
                           (buffer-substring start end)
                         (thing-at-point 'symbol)))
           (thang (read-string "Search term: " orig-thang))
           (json-object-type 'plist)
           (json-array-type 'list)
           (lurl (car (split-string
                       (completing-read
                        (format "Choose URL (%s): " thang)
                        (mapcar (lambda (entry)
                                  (string-join (list (plist-get entry :url)
                                                     " :: "
                                                     (plist-get entry :title))))
                                (json-read-from-string (shell-command-to-string (string-join (list "ddgr --json '" thang "'"))))))
                       " "))))
      (save-excursion
        (if (use-region-p)
            (kill-region start end)
          (kill-region (beginning-of-thing 'symbol) (end-of-thing 'symbol)))
        (insert (format "[%s](%s)" orig-thang lurl)))))

  ;; Generate pdf from markdown document
  (defun meain/markdown-pdf ()
    "Generate pdf from markdown document."
    (interactive)
    (message "Generating pdf of %s. Just give it a moment.." (buffer-file-name))
    (start-process-shell-command "*markdown-pdf*" "*markdown-pdf*"
                                 (concat ",markdown-to-pdf " (buffer-file-name))))

  (defun meain/markdown-html ()
    "Generate pdf from markdown document."
    (interactive)
    (message "Generating markdown for %s. Just give it a moment.." (buffer-file-name))
    (start-process-shell-command "*markdown-html*" "*markdown-html*"
                                 (concat ",markdown-to-html " (buffer-file-name)))))

(use-package emacs
  :config
  ;; Run markdown code blocks
  ;; Possible alternative https://github.com/md-babel/md-babel.el
  (setq meain/run-markdown-code-block-marker "<!-- code block execution result -->")
  (defun meain/run-markdown-code-block (&optional insert-to-buffer)
    "Run markdown code block at point.  Insert output after block if INSERT-TO-BUFFER is non-nil."
    (interactive "P")
    (let* ((bounds (markdown-code-block-at-point))) ; returns (start . end) as integers
      (unless bounds (user-error "Not inside a markdown code block"))
      (let* ((start (car bounds))
             (end (cadr bounds))
             (raw-block (buffer-substring-no-properties start end))
             (lines (split-string raw-block "\n"))
             (fence-line (car lines))
             (lang (car (last (split-string fence-line "[ `]+"))))
             (code (string-join (butlast (cdr lines)) "\n"))
             (interpreter (alist-get (intern lang)
                                     '((python . "python3") (py . "python3")
                                       (ruby . "ruby") (rb . "ruby")
                                       (sh . "bash") (shell . "bash") (bash . "bash") (shell-script . "bash")
                                       (js . "node") (javascript . "node") (typescript . "ts-node") (ts . "ts-node")
                                       (emacs-lisp . "emacs --batch -l"))
                                     nil nil #'string=)))
        (unless (and lang (not (string= lang "")) interpreter)
          (user-error "Unknown or unsupported language: %s" lang))
        (let ((tmpfile (make-temp-file "md-run-block-")))
          (with-temp-file tmpfile (insert code))
          (unwind-protect
              (let ((output (shell-command-to-string (format "%s '%s'" interpreter tmpfile))))
                (if insert-to-buffer
                    (save-excursion
                      (goto-char end)
                      (insert (format "\n\n%s\n```\n%s\n```\n\n"
                                      meain/run-markdown-code-block-marker
                                      (string-trim-right output))))
                  (pop-to-buffer "*markdown-block-output*")
                  (erase-buffer)
                  (insert output)))
            (delete-file tmpfile)))))))

;; Writing mode
(use-package writeroom-mode
  :ensure t
  :commands writeroom-mode
  :config
  (setq writeroom-global-effects (remove 'writeroom-set-fullscreen writeroom-global-effects)))

;; Naive linter for English prose
(use-package writegood-mode
  :ensure t
  :defer t
  :commands (writegood-mode))

(use-package emacs
  :after (writeroom-mode evil-leader)
  :commands (meain/toggle-writing-mode)
  :config
  ;; TODO: convert writing-mode to minor mode
  (defvar meain/writing-mode-enabled -1 "State to store if `writing-mode' is enabled.")
  (defun meain/toggle-writing-mode ()
    "Toggle `writing-mode'."
    (interactive)
    (toggle-truncate-lines meain/writing-mode-enabled)
    (setq meain/writing-mode-enabled (if (eq meain/writing-mode-enabled t) -1 t))
    (writeroom-mode meain/writing-mode-enabled)
    (focus-mode meain/writing-mode-enabled)
    (writegood-mode meain/writing-mode-enabled)
    (flyspell-mode meain/writing-mode-enabled))
  :init
  (evil-leader/set-key "b W" 'meain/toggle-writing-mode))

(use-package emacs
  :commands (meain/kill-markdown-preview meain/markdown-preview)
  :config
  ;; Markdown preview
  (defun meain/kill-markdown-preview ()
    "Preview markdown.  Using pandoc under the hood."
    (interactive)
    (let ((kill-buffer-query-functions nil))
      (if (get-buffer "*markdown-preview*")
          (progn
            (message "Killing old markdown preview server...")
            (kill-buffer "*markdown-preview*")))))
  (defun meain/markdown-preview ()
    "Preview markdown.  Using pandoc under the hood."
    ;; TODO: handle local embedded images
    (interactive)
    (meain/kill-markdown-preview)
    (start-process "*markdown-preview*" "*markdown-preview*"
                   ",markdown-preview" buffer-file-name)))

(provide 'writing)
;;; writing.el ends here
