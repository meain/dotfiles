;;; notes.el --- Note taking realted packages -*- lexical-binding: t; -*-

;;; Commentary:
;; Note taking realted packages

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

(provide 'notes)
;;; notes.el ends here
