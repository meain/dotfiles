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
    (let* ((templates (directory-files meain/quick-notes-templates-directory nil ".+\\..+"))
           (name-input (completing-read "Title: " templates nil nil))
           (is-template (member name-input templates))
           (extension (if is-template "" ; template would have the extension
                        (if (s-contains-p "." name-input) ""
                          (concat "." (completing-read "Extension: " '("md") nil nil)))))
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

;; Journal entry
(use-package emacs
  :after evil-leader
  :init
  (add-hook 'find-file-hook
            (lambda ()
              (if (string-prefix-p (expand-file-name "~/.local/share/journal")
                                   default-directory)
                  (progn
                    (copilot-mode -1) ; noooope
                    (auto-fill-mode)))))
  (evil-leader/set-key "a J"
    (lambda ()
      "Start writing journal entry.  `journal' invokes emacsclient and gives control back over to Emacs."
      (interactive)
      (start-process-shell-command "journal" "*journal*"
                                   "EDITOR='emacsclient' ,journal"))))


(provide 'notes)
;;; notes.el ends here
