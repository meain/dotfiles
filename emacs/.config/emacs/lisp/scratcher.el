;;; scratcher.el --- Scratch buffer related things -*- lexical-binding: t; -*-

;;; Commentary:
;; All thing realted to working with scratch buffers

;;; Code:
(use-package emacs
  :after evil
  :init
  (evil-leader/set-key "c" 'meain/create-or-switch-to-scratch)
  (define-key evil-normal-state-map (kbd "q") 'meain/kill-current-buffer-unless-scratch)
  :commands (meain/update-scratch-message meain/create-or-switch-to-scratch meain/kill-current-buffer-unless-scratch)
  :config
  (defun meain/update-scratch-message ()
    "Update scratch buffer contents to reflect open buffers and unread emails."
    (interactive)
    (with-current-buffer "*scratch*"
      (save-restriction
        (widen)
        (save-excursion
          (goto-char 1)
          ;; kill-line without copying to clipboard
          (delete-region (point) (save-excursion (end-of-line 2) (point)))
          (insert (format ";; The LLMs have been going through your %s buffers\n;; FYI, I have been up for the last %s"
                          (cl-count-if (lambda (b)
                                         (or (buffer-file-name b)
                                             (not (string-match "^ " (buffer-name b)))))
                                       (buffer-list))
                          (emacs-uptime "%D, %H")))))))

  (defun meain/create-or-switch-to-scratch ()
    "Switch to scratch buffer if exists, else create a scratch buffer with our config."
    (interactive)
    (cond
     ((get-buffer "*scratch*")
      (switch-to-buffer "*scratch*"))
     (t (progn
          (switch-to-buffer "*scratch*")
          (setq default-directory "~/")
          (lisp-interaction-mode)
          (meain/update-scratch-message)))))

  (defun meain/kill-current-buffer-unless-scratch ()
    "Kill current buffer if it is not scratch."
    (interactive)
    (if (= (length (mapcar #'window-buffer
                           (window-list))) 1)
        ;; TODO: optional delete frame advice on things that close (notmuch, elfeed)
        (if (equal "emacs-popup" (cdr (assq 'name (frame-parameters))))
            (delete-frame)
          (meain/create-or-switch-to-scratch))
      (cond
       ((derived-mode-p 'prog-mode)
        (evil-quit))
       ((member major-mode '(imenu-list-major-mode magit-mode))
        (evil-quit))
       ((equal major-mode 'vterm-mode)
        (progn
          (evil-insert 1)
          (vterm-reset-cursor-point)))
       (t (previous-buffer))))))

;; Enable recentf
(use-package recentf
  :defer t
  :init
  (recentf-mode t)
  (add-hook 'after-init-hook
            (lambda ()
              (with-current-buffer "*scratch*"
                (goto-char (point-max))
                (insert initial-scratch-message)
                (newline 2)
                (mapcar (lambda (x)
                          (insert "\n")
                          (insert-button
                           (string-join (reverse (cl-subseq (reverse (split-string x "/")) 0 2)) "/")
                           'action (lambda (_button) (find-file x))
                           'follow-link t))
                        (cl-subseq recentf-list 0 (min 3 (length recentf-list))))))))

;; Save buffer
(use-package emacs
  :after evil
  :init
  (define-key evil-normal-state-map
              (kbd "<SPC> <SPC>")
              (lambda ()
                (interactive)
                (cond
                 ((equal (buffer-name) "*scratch*")
                  (with-temp-file
                      (concat
                       "/tmp/emacs-scratch-"
                       (format-time-string "%Y-%m-%d-%H-%M-%S"))
                    (message "Saved scratch to a temporary location")
                    (insert (with-current-buffer "*scratch*" (buffer-string)))))
                 ((bound-and-true-p gptel-mode)
                  ;; See if the buffer already has a filename, if so
                  ;; use else, else call the gptel save buffer
                  ;; function.
                  (if (buffer-file-name)
                      (save-buffer)
                    (call-interactively 'meain/gptel-rename-chat-buffer)))
                 (t (call-interactively 'evil-write))))))

(provide 'scratcher)
;;; scratcher.el ends here
