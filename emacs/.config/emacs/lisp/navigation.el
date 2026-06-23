;;; navigation.el --- Navigation related packages -*- lexical-binding: t; -*-

;;; Commentary:
;; Navigate to places (not much searching).  Also include visual aids in languages.

;;; Code:
;; Scratch buffer navigation
(use-package emacs
  :after evil
  :init
  (evil-leader/set-key "c" 'meain/create-or-switch-to-scratch)
  (define-key evil-normal-state-map (kbd "q") 'meain/kill-current-buffer-unless-scratch)
  :commands (meain/update-scratch-message
             meain/create-or-switch-to-scratch
             meain/kill-current-buffer-unless-scratch)
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
       (t (previous-buffer)))))

  ;; Auto updating scratch message
  (run-at-time "3 minutes" (* 5 60) 'meain/update-scratch-message))

;; Matchit
(use-package evil-matchit
  :ensure t
  :defer t
  :config (global-evil-matchit-mode 1))

;; Code folding
(use-package origami
  :ensure t
  :after (evil evil-leader)
  :defer t
  :config (global-origami-mode)
  :commands (evil-toggle-fold)
  :init
  (define-key evil-normal-state-map (kbd "<SPC> TAB") 'evil-toggle-fold)
  (evil-leader/set-key "o" 'evil-toggle-fold))

;; Saveplace
(use-package saveplace
  :defer t
  :init
  (save-place-mode t)
  (setq save-place-file "~/.local/share/emacs/saveplace")
  ;; Recenter view after save-place restores cursor so it's not at screen edge
  (advice-add 'save-place-find-file-hook :after
              (lambda (&rest _)
                (when buffer-file-name (ignore-errors (recenter))))))

;; Fancier tab management
(use-package tab-bar
  :after evil-leader
  :defer t
  :commands (tab-close tab-new tab-next tab-bar-rename-tab
                       meain/switch-tab-dwim meain/create-or-delete-tab
                       meain/toggle-scratch-tab tab-bar-switch-to-tab)
  :config
  (setq tab-bar-history-limit 100)
  ;; (tab-bar-history-mode t)
  ;; (global-set-key (kbd "M-f <left>") 'tab-bar-history-back)
  ;; (global-set-key (kbd "M-f <right>") 'tab-bar-history-forward)

  (global-set-key (kbd "M-f ,") 'tab-bar-rename-tab)
  (evil-leader/set-key "t" 'meain/switch-tab-dwim)
  (evil-leader/set-key "T" 'meain/create-or-delete-tab)
  (evil-leader/set-key "C" (lambda () (interactive) (tab-bar-switch-to-tab "-scratch")))
  (evil-leader/set-key "s c" (lambda () (interactive) (tab-bar-switch-to-tab "-scratch")))
  (global-set-key (kbd "M-f s") 'meain/switch-tab-dwim)
  (global-unset-key (kbd "M-;"))
  (global-set-key (kbd "M-; M-;") 'meain/toggle-scratch-tab)

  (setq tab-bar-close-button-show nil)
  (setq tab-bar-close-last-tab-choice 'tab-bar-mode-disable)
  (setq tab-bar-close-tab-select 'recent)
  (setq tab-bar-new-tab-choice t)
  (setq tab-bar-new-tab-to 'right)
  (setq tab-bar-position nil)
  (setq tab-bar-show t)
  (setq tab-bar-tab-hints nil)
  (setq tab-bar-tab-name-function 'tab-bar-tab-name-all)
  (tab-bar-mode 1)
  (tab-bar-history-mode -1)
  (set-frame-parameter nil 'tab-bar-lines 1)

  ;; Custom tab bar display
  (defun meain/tab-bar-current-tab ()
    "Display all tabs with current tab highlighted, -scratch always first."
    (let* ((tabs (tab-bar-tabs))
           (current-tab (assq 'current-tab tabs))
           (sorted-tabs (sort (copy-sequence tabs)
                             (lambda (a b)
                               (let ((name-a (alist-get 'name a))
                                     (name-b (alist-get 'name b)))
                                 (cond
                                  ((string= name-a "-scratch") t)
                                  ((string= name-b "-scratch") nil)
                                  (t nil))))))
           (shrug " ¯\\_(ツ)_/¯ "))
      (concat
       (propertize shrug 'face 'shadow)
       (mapconcat
        (lambda (tab)
          (let ((name (alist-get 'name tab))
                (is-current (eq (car tab) 'current-tab)))
            (if is-current
                (propertize (format " [%s] " name) 'face 'tab-bar)
              (propertize (format " |%s| " name) 'face 'shadow))))
        sorted-tabs
        ""))))

  (defun meain/tab-bar-model-info ()
    "Display yap/gptel model information in faded text."
    (let ((yap (when (boundp 'yap-model) yap-model))
          (gptel (when (boundp 'gptel-model) (format "%s" gptel-model))))
      (when (or yap gptel)
        (propertize (format " [%s/%s] "
                            (or yap "") (or gptel ""))
                    'face 'shadow))))

  (setq tab-bar-format '(meain/tab-bar-current-tab
                         tab-bar-format-align-right
                         meain/tab-bar-model-info))

  (custom-set-faces
   '(tab-bar ((t (:box nil))))
   '(tab-bar-tab ((t (:box nil)))))

  (defun meain/create-or-delete-tab (&optional close)
    "Create or close tab"
    (interactive "P")
    (let ((tabs (cl-remove-if (lambda (x)
                                (equal x "-scratch"))
                              (mapcar (lambda (tab)
                                        (alist-get 'name tab))
                                      (tab-bar--tabs-recent)))))
      (if close
          (if (eq tabs nil)
              (message "Not closing last tab")
            (tab-close))
        (tab-new))))
  (defun meain/toggle-scratch-tab ()
    "Toggle to/from the -scratch tab.
If currently on -scratch, switch back to the previous tab.
Otherwise, switch to -scratch."
    (interactive)
    (if (equal (alist-get 'name (tab-bar--current-tab)) "-scratch")
        (tab-bar-switch-to-recent-tab)
      (tab-bar-switch-to-tab "-scratch")))

  (defun meain/switch-tab-dwim (&optional show-hidden)
    "Switch between available tabs.
If only two tabs, we switch to the other tab, and if we have more than
two tabs, prompt for which tab to switch to.

You can mark a tab as hidden by prefixing it with a -. These won't be
shown unless the `SHOW-HIDDEN' arg is provided."
    (interactive "P")
    (let ((tabs (cl-remove-if (lambda (x)
                                (and (not show-hidden)
                                     (string-prefix-p "-" x)))
                              (mapcar (lambda (tab)
                                        (alist-get 'name tab))
                                      (tab-bar--tabs-recent)))))
      (if (> (length tabs) 1)
          (tab-bar-switch-to-tab (completing-read "Select tab: " tabs nil t nil 'meain/tab-switch-history))
        (cond
         ((eq tabs nil)
          (message (concat "Only one tab present. Use `"
                           (substitute-command-keys "\\[meain/create-or-delete-tab]")
                           "` to create another tab.")))
         ;; NOTE: tab-bar-switch-to-tab will not work reliably if
         ;; multiple tabs have the same name. This can happen if we
         ;; open the same file in multiple tabs.
         (t (tab-bar-switch-to-tab (car tabs))))))))

;; Neotree
(use-package neotree
  :ensure t
  :commands neotree
  :config
  (setq neo-window-fixed-size nil)
  (setq neo-theme 'classic)
  (add-hook 'neo-after-create-hook 'hl-line-mode))

;; rg.el
(use-package rg
  :ensure t
  :commands rg
  :after evil-leader
  :init
  (evil-leader/set-key "f"
    (alambda (if (use-region-p)
                 (let ((text (buffer-substring-no-properties
                              (region-beginning)
                              (region-end))))
                   (evil-normal-state)
                   (consult-ripgrep default-directory text))
               (consult-ripgrep))
             (call-interactively 'rg)))
  :config (setq rg-command-line-flags '("--hidden" "--follow")))

;; dumb-jump (doing defer on this seems to not work reliably)
(use-package dumb-jump
  :ensure t
  :after evil-leader
  :custom
  (dumb-jump-prefer-searcher 'rg)
  (dumb-jump-quiet t)
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

;; Xref customization
(use-package xref
  :after (evil)
  :defer t
  :config
  (define-key evil-normal-state-map (kbd "M-.") #'xref-find-definitions)
  (define-key evil-normal-state-map (kbd "M-?") #'xref-find-references)
  (define-key evil-normal-state-map (kbd "g d") 'xref-find-definitions)
  (define-key evil-normal-state-map (kbd "g D") 'xref-find-implementations)
  (define-key evil-normal-state-map (kbd "g r") 'xref-find-references)
  ;; (setq xref-show-definitions-function 'xref-show-definitions-completing-read)
  (setq xref-auto-jump-to-first-xref 'move) ;; Use 'show to open it
  (setq xref-auto-jump-to-first-definition 'move))

;; Tagbar alternative
(use-package imenu
  :defer t
  :after (consult)
  :commands imenu
  :config
  (setq imenu-auto-rescan t)
  (setq imenu-max-item-length 300)
  (advice-add 'consult-imenu
              :before
              (lambda ()
                ;; We want the previous .rest buffer if http response buffer
                (if (equal (buffer-name) "*HTTP Response*")
                    (previous-window-any-frame))))
  :init
  (global-set-key (kbd "M-i") 'consult-imenu))
(use-package flimenu
  :ensure t
  :defer t
  :after imenu
  :config (flimenu-global-mode 1))
(use-package imenu-list
  :ensure t
  :defer t
  :after (imenu consult)
  :commands imenu-list-smart-toggle
  :config
  (setq imenu-list-focus-after-activation t)
  (setq imenu-list-after-jump-hook nil)
  (setq imenu-list-auto-resize t))

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
                                               (mapcar (lambda (f)
                                                         (file-relative-name f file))
                                                       (sort (directory-files-recursively file ".*" nil)
                                                             (lambda (a b)
                                                               (time-less-p
                                                                (file-attribute-modification-time (file-attributes b))
                                                                (file-attribute-modification-time (file-attributes a)))))))))
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
  :commands (remember remember-notes)
  :config
  (setq remember-data-file "~/.config/emacs/remember-notes"
        remember-notes-initial-major-mode 'org-mode
        remember-notes-auto-save-visited-file-name t))

(provide 'navigation)
;;; navigation.el ends here
