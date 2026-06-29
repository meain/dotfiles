;;; notes.el --- Note taking related packages -*- lexical-binding: t; -*-

;;; Commentary:
;; Note taking related packages. All configuration for note taking as
;; well as markdown specific configurations.

;;; Code:
;; Quick notes
(use-package emacs
  :after (evil-leader)
  :commands (meain/create-quick-note)
  :config
  (setq meain/quick-notes-templates-directory "~/dev/src/templates")
  (defun meain/create-quick-note ()
    (interactive)
    (require 's)
    (let* ((templates (seq-filter (lambda (x) (not (s-starts-with-p "_" x)))
                                  (directory-files meain/quick-notes-templates-directory nil ".+\\..+")))
           (name-input (completing-read "Name: " templates nil nil))
           (is-template (member name-input templates))
           (extension (if is-template "" ; template would have the extension
                        (if (s-contains-p "." name-input) "" ".md")))
           (filename (concat meain/quick-notes-directory "/"
                             (format-time-string "%Y-%m/%d %H.%M " (current-time))
                             name-input extension)))
      (find-file filename)
      (when is-template
        (insert-file-contents (concat meain/quick-notes-templates-directory "/" name-input))
        (goto-char (point-min)))))
  :init
  (setq meain/quick-notes-directory "~/.local/share/vime/")
  (evil-leader/set-key "v"
    (alambda
     (meain/create-quick-note)
     ;; TODO: sort files by timestamp when displaying
     (dired (concat meain/quick-notes-directory "/" (format-time-string "%Y-%m" (current-time)))))))

;; Obsidian handling
(use-package emacs
  :after (evil-leader)
  :config
  (defun meain/notes-goto-today-journal ()
    (interactive)
    (find-file (concat (getenv "NOTES_PATH")
                       "/Journal/Day/"
                       (format-time-string "%Y/%m/%Y-%m-%d" (current-time)) ".md")))
  :init
  (evil-leader/set-key "e n" 'meain/notes-goto-today-journal))

;; Markdown package configuration
;; Common markdown functions for both markdown-mode and markdown-ts-mode
(use-package emacs
  :commands (meain/paste-after-or-create-link meain/markdown-linkify-thing
             meain/markdown-pdf meain/markdown-html)
  :config
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

  (defun meain/markdown-linkify-thing (start end)
    "Search for and insert a markdown link at START to END or at point.
Uses 'ddgr' web search to look up a URL and insert a formatted markdown link."
    (interactive "r")
    (let* ((region-active (use-region-p))
           (thing-bounds (if region-active
                             (cons start end)
                           (bounds-of-thing-at-point 'symbol)))
           (thing-text (if region-active
                           (buffer-substring-no-properties start end)
                         (buffer-substring-no-properties (car thing-bounds) (cdr thing-bounds))))
           (search-term (read-string "Search term: " thing-text))
           (json-object-type 'plist)
           (json-array-type 'list)
           (search-cmd (format "ddgr --noua --json \"%s\"" search-term))
           (json-result (ignore-errors (shell-command-to-string search-cmd)))
           (results (condition-case nil
                        (json-read-from-string json-result)
                      (error nil))))
      (if (and results (listp results) (> (length results) 0))
          (let* ((candidates (mapcar (lambda (entry)
                                       (cons (format "%s :: %s"
                                                     (plist-get entry :url)
                                                     (plist-get entry :title))
                                             (plist-get entry :url)))
                                     results))
                 (choose (completing-read
                          (format "Choose URL for [%s]: " thing-text)
                          candidates nil t))
                 (chosen-url (cdr (assoc choose candidates))))
            (save-excursion
              (delete-region (car thing-bounds) (cdr thing-bounds))
              (insert (format "[%s](%s)" thing-text chosen-url))))
        (user-error "No search results or API error for %s" search-term))))

  (defun meain/markdown-pdf ()
    "Generate pdf from markdown document."
    (interactive)
    (if (not (buffer-file-name))
        (user-error "No file associated with current buffer")
      (message "Generating pdf of %s. Just give it a moment.." (buffer-file-name))
      (start-process-shell-command "*markdown-pdf*" "*markdown-pdf*"
                                   (concat ",markdown-to-pdf '" (buffer-file-name) "'"))))

  (defun meain/markdown-html ()
    "Generate html from markdown document."
    (interactive)
    (if (not (buffer-file-name))
        (user-error "No file associated with current buffer")
      (message "Generating markdown for %s. Just give it a moment.." (buffer-file-name))
      (start-process-shell-command "*markdown-html*" "*markdown-html*"
                                   (concat ",markdown-to-html '" (buffer-file-name) "'")))))

(use-package markdown-mode
  :ensure t
  :defer t
  :after (edit-indirect evil)
  :mode ("\\.md\\'" . gfm-mode)
  :config
  (setq markdown-url-compose-char '(8230 8943 35 9733 9875))
  (setq markdown-enable-html -1)
  (setq markdown-enable-wiki-links nil) ; makes markdown mode super slow
  (setq markdown-gfm-use-electric-backquote nil) ; don't ask me to pick lang in ```
  (evil-define-key 'insert gfm-mode-map (kbd "C-c e") 'meain/insert-emoji)
  (evil-define-key 'insert markdown-mode-map (kbd "C-c e") 'meain/insert-emoji)
  (evil-define-key 'normal gfm-mode-map (kbd "<RET>") 'project-find-file)
  (evil-define-key 'normal gfm-mode-map (kbd "g d") 'markdown-do)
  (evil-define-key 'normal markdown-mode-map (kbd "<RET>") 'project-find-file)
  (evil-define-key 'normal markdown-mode-map (kbd "g d") 'markdown-do)
  (evil-define-key 'insert gfm-mode-map (kbd "C-<return>") 'markdown-insert-list-item)
  (evil-define-key 'insert markdown-mode-map (kbd "C-<return>") 'markdown-insert-list-item)
  (evil-define-key 'visual markdown-mode-map "p" 'meain/paste-after-or-create-link)
  (setq markdown-command "pandoc -t html5 -f gfm")

  ;; Make it look like GitHub markdown
  (setq markdown-xhtml-body-preamble "<div class=\"markdown-body\">")
  (setq markdown-xhtml-body-epilogue "</div>")
  (setq markdown-xhtml-header-content
        "<style>.markdown-body { max-width: 800px; margin: 0 auto; padding: 2rem; }</style>")
  (setq markdown-css-paths (list (concat
                                  (getenv "HOME")
                                  "/.dotfiles/datafiles/.config/datafiles/github-markdown.css")))

  (setq-default markdown-fontify-code-blocks-natively t)
  (setq-default markdown-hide-urls t) ;; Or call markdown-toggle-url-hiding

  ;; Mark completed markdown list items with a grey and strikethrough
  (defface meain/markdown-done-item
    '((t :foreground "#A0A0A0" :strike-through "#D0D0D0"))
    "Face for completed GFM checklist items.")
  (font-lock-add-keywords
   'gfm-mode
   '(("^[[:space:]]*-[[:space:]]\\[x\\][[:space:]]\\(.*\\)$"
      1 'meain/markdown-done-item prepend))
   'append)

  (add-hook 'gfm-mode-hook (lambda () (toggle-truncate-lines nil))))

(use-package markdown-ts-mode
  :defer t
  :after evil
  :config
  (setq-default markdown-ts-hide-markup t)
  (setq-default markdown-ts-fontify-code-blocks-natively t)

  (evil-define-key 'normal org-mode-map (kbd "M-l") 'meain/move-swap-right)
  (evil-define-key 'normal org-mode-map (kbd "M-h") 'meain/move-swap-left)
  (evil-define-key 'normal org-mode-map (kbd "M-k") 'meain/move-swap-up)
  (evil-define-key 'normal org-mode-map (kbd "M-j") 'meain/move-swap-down)

  ;; (evil-define-key 'normal markdown-ts-mode-map (kbd "g d") 'markdown-ts-toggle-checkbox)
  (evil-define-key 'insert markdown-ts-mode-map (kbd "C-c e") 'meain/insert-emoji)
  (evil-define-key 'normal markdown-ts-mode-map (kbd "<RET>") 'project-find-file)
  (evil-define-key 'insert markdown-ts-mode-map (kbd "C-<return>") 'markdown-ts-insert-list-item)
  (evil-define-key 'visual markdown-ts-mode-map "p" 'meain/paste-after-or-create-link))

;; Run markdown code blocks
;; Possible alternative https://github.com/md-babel/md-babel.el
(use-package emacs
  :config
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

(use-package real-auto-save
  :ensure t
  :hook ((markdown-mode . real-auto-save-mode)
         (gfm-mode . real-auto-save-mode))
  :config
  (setq real-auto-save-interval 5))

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
  :commands (meain/kill-markdown-preview meain/markdown-preview meain/insert-emoji)
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
                   ",markdown-preview" buffer-file-name))

  ;; Emoji picker using built-in emoji.el
  (defun meain/insert-emoji ()
    "Insert an emoji via completing-read using Emacs built-in emoji data."
    (interactive)
    (require 'emoji)
    (emoji--init)
    (let* ((emojis nil))
      (maphash (lambda (glyph name)
                 (push (cons (format "%s %s" name glyph) glyph) emojis))
               emoji--names)
      (let* ((choice (completing-read "Emoji: " emojis nil t))
             (emoji (cdr (assoc choice emojis))))
        (when emoji
          (insert emoji))))))

(provide 'notes)
;;; notes.el ends here
