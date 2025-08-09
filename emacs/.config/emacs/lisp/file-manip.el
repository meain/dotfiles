;;; file-manip.el --- File manipulation functions -*- lexical-binding: t; -*-

;;; Commentary:
;; Functions to manipulate files, such as opening in external editors,
;; moving, deleting renaming etc.

;;; Code:
(use-package emacs
  :commands (meain/delete-current-file meain/rename-current-file meain/move-current-file)
  :config
  ;; Delete current file
  (defun meain/delete-current-file ()
    "Delete current file and close buffer."
    (interactive)
    (delete-file (buffer-file-name))
    (meain/kill-current-buffer-unless-scratch))

  ;; Quick file rename
  (defun meain/rename-current-file ()
    "Rename current file in the same directory."
    (interactive)
    (let ((newname (read-string "New name: " (file-name-nondirectory (buffer-file-name)))))
      (rename-file (buffer-file-name) (concat (file-name-directory (buffer-file-name)) newname))
      (find-alternate-file (concat (file-name-directory (buffer-file-name)) newname))))

  (defun meain/move-current-file ()
    "Rename the current visiting file and switch buffer focus to it."
    (interactive)
    (if (null (buffer-file-name))
        (user-error "Buffer does not have a filename: %s" (current-buffer)))
    (let* ((prompt (format "Rename %s to: "
                           (file-name-nondirectory (buffer-file-name))))
           (new-filename (expand-file-name (read-file-name prompt))))
      (if (null (file-writable-p new-filename))
          (user-error "New file not writable: %s" new-filename))
      (rename-file (buffer-file-name) new-filename 1)
      (find-alternate-file new-filename)
      (message "Renamed to and now visiting: %s" (abbreviate-file-name new-filename)))))


(provide 'file-manip)
;;; file-manip.el ends here
