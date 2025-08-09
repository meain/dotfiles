;;; automatic.el --- Automatically fix up things within Emacs -*- lexical-binding: t; -*-

;;; Commentary:
;; Functions that run in the background to fix up things automatically.

;;; Code:
;; setting proper default-dir
(defun meain/set-proper-default-dir ()
  "Function to set the `default-directory' value as the project root if available."
  (interactive)
  (let* ((file-directory (file-name-directory (or buffer-file-name default-directory)))
         (go-base (locate-dominating-file file-directory "go.mod")) ;; when we navigate to installed go libs
         (jj-base (locate-dominating-file file-directory ".jj"))) ;; when we use jj worktrees
    (if (not (file-remote-p default-directory))
        (setq default-directory (cond
                                 ((not (eq (project-current) nil))
                                  (car (project-roots (project-current))))
                                 ((not (eq go-base nil)) go-base)
                                 ((not (eq jj-base nil)) jj-base)
                                 (t "~/"))))))
(add-hook 'find-file-hook 'meain/set-proper-default-dir)

(use-package emacs
  :config
  (defun meain/set-read-only-if-do-not-edit ()
    "Set the buffer to read-only if buffer contents has 'DO NOT EDIT' in it.
We limit the search to just top 10 lines so as to only check the header."
    (save-excursion
      (goto-char (point-min))
      (let ((content (buffer-substring (point) (line-end-position 10))))
        (when (and (not buffer-read-only)
                   (string-match "DO NOT EDIT" content))
          (read-only-mode 1)
          (message "Buffer seems to be generated. Set to read-only mode.")))))
  (add-hook 'find-file-hook 'meain/set-read-only-if-do-not-edit))

;; Keep files in sync with filesystem
;; Tweak these settings carefully. This makes things quite slow if not
;; configured correctly.
(use-package autorevert
  :config
  (setq auto-revert-interval 5)
  (setq auto-revert-check-vc-info nil)
  (setq global-auto-revert-non-file-buffers nil)
  (setq auto-revert-verbose nil)
  (global-auto-revert-mode t))

(provide 'automatic)
;;; automatic.el ends here
