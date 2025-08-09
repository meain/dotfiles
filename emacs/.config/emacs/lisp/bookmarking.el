;;; bookmarking.el --- Packages related to jumping around or bookmarking -*- lexical-binding: t; -*-

;;; Commentary:
;; Packages related to jumping around or bookmarking

;;; Code:
;; Add keybindings to access important files.
(use-package emacs
  :after (evil-leader)
  :defer nil
  :config
  (defun meain/qa--get-entries (filename)
    "Helper function to parse qa files.  `FILENAME' is the name of the file to parse."
    (let* ((contents (with-temp-buffer
                       (insert-file-contents filename)
                       (buffer-string)))
           (qa-entries (mapcar (lambda (x)
                                 (string-split x " "))
                               (string-split contents "\n"))))
      qa-entries))

  (mapcar (lambda (e)
            (when-let ((key (car e))
                       (name (cadr e))
                       (file (caddr e)))
              (evil-leader/set-key (concat "e " key)
                (cons name (lambda (&optional create)
                             (interactive "P")
                             (if (file-exists-p file)
                                 (if (file-directory-p file)
                                     (find-file
                                      (concat file "/"
                                              (completing-read
                                               "Choose file:"
                                               (directory-files file nil
                                                                directory-files-no-dot-files-regexp))))
                                   (find-file file))
                               (if create
                                   (find-file file)
                                 (message "Unable to find %s" file))))))))
          (meain/qa--get-entries "~/.config/datafiles/qa-files"))

  ;; Add keybinding to access common projects quickly.
  ;; qa-projects (quick-access-projects) file contains the list of
  ;; projects that will be added here.
  (mapcar (lambda (e)
            (when-let ((key (car e))
                       (name (cadr e))
                       (folder (caddr e)))
              (evil-leader/set-key (concat "s e " key)
                (cons name (lambda ()
                             (interactive)
                             (project-switch-project folder))))))
          (meain/qa--get-entries "~/.config/datafiles/qa-projects")))

;; Bookmarks
(use-package bookmark
  :commands (bookmark-jump bookmark-set)
  :config
  (setq bookmark-save-flag 1)
  (setq bookmark-set-fringe-mark nil)
  (global-set-key (kbd "M-f m") 'bookmark-jump)
  (global-set-key (kbd "M-f M") 'bookmark-set)

  (defun meain/recenter-advice (orig-fn &rest args)
    "Used to recenter the buffer after `ORIG-FN' passing down `ARGS' down to it."
    (apply orig-fn args)
    (recenter))
  (advice-add 'bookmark-jump :around #'meain/recenter-advice))

;; Remember
(use-package remember
  :commands remember
  :config
  (setq remember-data-file "~/.config/emacs/remember-notes"
        remember-notes-initial-major-mode 'org-mode
        remember-notes-auto-save-visited-file-name t))

(provide 'bookmarking)
;;; bookmarking.el ends here
