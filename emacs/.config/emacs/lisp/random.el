;;; random.el --- Random utility functions -*- lexical-binding: t; -*-

;;; Commentary:
;; My personal collection of random utility functions.

;;; Code:
(use-package emacs
  :commands (meain/emacs-revert-all-project-buffers)
  :config
  (defun meain/emacs-revert-all-project-buffers ()
    "Revert all editable buffers belonging to the current project."
    (interactive)
    (seq-do
     (lambda (b)
       (when (buffer-local-value 'buffer-read-only b)
         (revert-buffer-quick b)))
     (project-buffers (project-current)))))

(use-package emacs
  :commands (meain/delete-all-buffers)
  :config
  (defun meain/delete-all-buffers ()
    "Delete all but some buffers.  Rest everything."
    (interactive)
    (dolist (buf (buffer-list))
      (let ((name (buffer-name buf)))
        (unless (member name
                        '("*scratch*" "*Messages*" "*Warnings*"
                          "*copilot events*" "*copilot-language-server-log*"))
          (kill-buffer buf))))
    (switch-to-buffer "*scratch*")))

;; NOTE: Switch to find-sibling-file
;; Patterns for replacing filenames with (builtin option: find-sibling-file)
;; Sticking with custom version as we have an option to create the file if it does not exist
;; Example for find-sibling-file:
;; (cl-pushnew '("\\([^/]+\\)\\.el\\'" "\\1-test.el") find-sibling-rules :test #'equal)
;; (cl-pushnew '("\\([^/]+\\)-test\\.el\\'" "\\1.el") find-sibling-rules :test #'equal)
(use-package emacs
  :after (evil-leader)
  :commands (meain/find-alternate-file)
  :config
  (defvar meain/find-alternate-file--patterns '(("thing-for-today-personal.mtodo" "thing-for-today.mtodo")
                                                ("early-init.el" "init.el")
                                                ("i3/config" "i3status/config")
                                                ("shell.nix" "default.nix")
                                                ("_test.go" ".go")
                                                ("-test.el" ".el")))
  (defun meain/find-alternate-file (&optional create)
    "Open alternate file.  Useful for opening test of currently active file.
Pass `CREATE' to create the alternate file if it does not exits."
    (interactive "P")
    (if (buffer-file-name)
        (let* ((file-patterns
                (apply #'append
                       (seq-map (lambda (x)
                                  (if (> (length (car x)) (length (cadr x)))
                                      (list (list (car x) (cadr x))
                                            (list (cadr x) (car x)))
                                    (list (list (cadr x) (car x))
                                          (list (car x) (cadr x)))))
                                meain/find-alternate-file--patterns)))
               (alt-file
                (car (cl-remove-if (lambda (x) (equal x nil))
                                   (seq-map (lambda (f)
                                              (if (string-match (car f) (buffer-file-name))
                                                  (s-replace-regexp (car f) (nth 1 f) (buffer-file-name))))
                                            file-patterns)))))
          (message "Switching to %s" (file-name-nondirectory alt-file))
          (if alt-file
              (if (file-exists-p alt-file)
                  (find-file alt-file)
                (if create
                    (find-file alt-file)
                  (message "Alternate file '%s' is not available on disk" alt-file)))
            (message "Unable to determine alternate file")))
      (message "Not in a file")))
  :init
  (evil-leader/set-key "e e" 'meain/find-alternate-file))

;; Copy stuff
(use-package emacs
  :commands (meain/copy-to-clipboard meain/copy-debugger-break-statement meain/copy-file-name-to-clipboard meain/copy-path-to-clipboard)
  :config
  (defun meain/copy-to-clipboard (message)
    "Copy `MESSAGE' into clipboard."
    (with-temp-buffer
      (insert message)
      (let ((deactivate-mark t))
        (call-process-region (point-min) (point-max) "pbcopy"))))

  (defun meain/copy-debugger-break-statement ()
    (interactive)
    (let ((file-name (buffer-file-name))
          (line-number (line-number-at-pos)))
      (meain/copy-to-clipboard (format "b %s:%s" file-name line-number))))

  (defun meain/copy-file-name-to-clipboard (&optional abs-path)
    "Copy the current filename into clipboard. Pass `ABS-PATH' if you need absolute path."
    (interactive "P")
    (let ((file-path (or (buffer-file-name) default-directory)))
      (if file-path
          (let ((copy-path (if abs-path file-path
                             (string-replace (car (project-roots (project-current))) "" file-path))))
            (meain/copy-to-clipboard copy-path)
            (message "Copied '%s' to the clipboard" copy-path))
        (message "No file associated with buffer"))))
  (defalias #'meain/copy-path-to-clipboard #'meain/copy-file-name-to-clipboard))

;; Just some hima testing code
(use-package emacs
  :commands (meain/reload-current-theme)
  :config
  (defun meain/reload-current-theme ()
    "Util to reload hima theme for debugging."
    (interactive)
    (message "%s" custom-enabled-themes)
    (let ((current-theme (car custom-enabled-themes)))
      (disable-theme current-theme)
      (load-theme current-theme t))))

;; Fix any escaped escape code in selection
(use-package emacs
  :commands (meain/fix-escapes)
  :config
  (defun meain/fix-escapes ()
    "Replace \\n to \n, \\t to \t and \\r to empty on selection."
    (interactive)
    (save-excursion
      (let ((start (region-beginning))
            (end (region-end)))
        (dolist (pair '(("\\n" . "\n") ("\\t" . "\t") ("\\r" . "") ("\\\"". "\"")))
          (goto-char start)
          (while (search-forward (car pair) end t)
            (replace-match (cdr pair) nil t)))))))

;; Fullscreen current buffer
(use-package emacs
  :commands (meain/monacle-mode)
  :config
  (defvar meain/window-configuration nil)
  (define-minor-mode meain/monacle-mode
    "Zoom in and out of single window."
    :lighter " [M]"
    :global nil
    (if (one-window-p)
        (when meain/window-configuration
          (set-window-configuration meain/window-configuration))
      (setq meain/window-configuration (current-window-configuration))
      (delete-other-windows)))
  :init
  (global-set-key (kbd "M-f f") 'meain/monacle-mode))

;; Hit universal arg without ctrl
(use-package emacs
  :after evil-leader
  :defer t
  :config
  (evil-leader/set-key "u" 'universal-argument)
  (global-set-key (kbd "M-u") 'universal-argument)
  (define-key universal-argument-map (kbd "M-u") 'universal-argument-more))

(provide 'random)
;;; random.el ends here
