;;; completions.el --- Dealing with completions -*- lexical-binding: t; -*-

;;; Commentary:
;; Complete things for me.

;;; Code:
(use-package corfu
  :ensure (:files (:defaults "extensions/*"))
  :defer 1
  :config
  (setq completion-cycle-threshold 3)
  (setq corfu-auto t)
  (setq corfu-cycle t)
  (setq corfu-auto-delay 0.2) ;; chill for a moment (perf related, unverified)
  (setq corfu-auto-prefix 2)
  (setq corfu-history-mode t)
  (setq corfu-count 5)
  (define-key corfu-map (kbd "RET") 'newline-and-indent) ; default: corfu-insert

  ;; Plugin in case we need in buffer overlay for completions
  ;; https://code.bsdgeek.org/adam/corfu-candidate-overlay

  (defun corfu-move-to-minibuffer ()
    "Move completion to minibuffer instead of corfu."
    (interactive)
    (let ((completion-extra-properties corfu--extra)
          completion-cycle-threshold completion-cycling)
      (apply #'consult-completion-in-region completion-in-region--data)))
  (define-key corfu-map "\M-m" #'corfu-move-to-minibuffer)

  (global-corfu-mode)
  (corfu-popupinfo-mode)

  (define-key corfu-map (kbd "M-p") #'corfu-popupinfo-scroll-down) ;; corfu-next
  (define-key corfu-map (kbd "M-n") #'corfu-popupinfo-scroll-up)  ;; corfu-previous
  (define-key corfu-map (kbd "M-d") #'corfu-popupinfo-documentation)
  (define-key corfu-map (kbd "M-D") #'corfu-popupinfo-location))

;; Add completion extensions
(use-package cape
  :ensure t
  :defer t
  :bind (("M-p" . completion-at-point) ;; capf
         ("M-f p t" . complete-tag)        ;; etags
         ("M-f p d" . cape-dabbrev)        ;; or dabbrev-completion
         ("M-f p h" . cape-history)
         ("M-f p f" . cape-file)
         ("M-f p k" . cape-keyword)
         ("M-f p s" . cape-symbol)
         ("M-f p a" . cape-abbrev)
         ("M-f p i" . cape-ispell)
         ("M-f p l" . cape-line)
         ("M-f p w" . cape-dict)
         ("M-f p &" . cape-sgml))
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file))

;; Completions (core framework)
(use-package vertico
  :ensure (:files (:defaults "extensions/*.el"))
  :config
  (setq vertico-count 13)
  (setq vertico-cycle t) ; useful for consult-imenu
  (define-key vertico-map (kbd "M-q") 'vertico-multiform-vertical)
  (define-key vertico-map (kbd "M-g") 'vertico-multiform-grid)
  (define-key vertico-map (kbd "<S-backspace>") 'vertico-directory-up)

  (define-key vertico-map (kbd "M-n") 'vertico-next-group)
  (define-key vertico-map (kbd "M-p") 'vertico-previous-group)

  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  (setq enable-recursive-minibuffers t)

  (defun common-path-length (path1 path2)
    "Return the number of common directories in PATH1 and PATH2."
    (let ((split-path1 (split-string path1 "/"))
          (split-path2 (split-string path2 "/")))
      (cl-loop for dir1 in split-path1
               for dir2 in split-path2
               while (string= dir1 dir2)
               count 1)))

  (defun sort-by-proximity (files target-file)
    "Sort FILES by how close they are in structure to TARGET-FILE."
    (sort files
          (lambda (file1 file2)
            (> (common-path-length file1 target-file)
               (common-path-length file2 target-file)))))

  (defun meain/sort-files-list (files)
    (if (project-current)
        (let* ((prev-buffer (window-buffer (minibuffer-selected-window)))
               (current-file (if (buffer-file-name prev-buffer)
                                 (buffer-file-name prev-buffer)
                               "."))
               (project-path (expand-file-name (project-root (project-current))))
               (current-file-sans-project (string-remove-prefix project-path current-file)))
          (sort (sort-by-proximity files current-file-sans-project)
                ;; Also give less priority to "/mock/" files
                (lambda (file1 file2)
                  (if (string-match-p "/mock/" file1)
                      nil
                    (if (string-match-p "/mock/" file2)
                        t
                      nil)))))
      files))

  (vertico-multiform-mode)
  (setq vertico-multiform-commands
        '((consult-ripgrep buffer indexed)
          (consult-xref buffer indexed)
          (eglot-find-implementation indexed) ;; TODO: change to vertical
          (consult-imenu buffer)
          (project-find-file flat (vertico-sort-function . meain/sort-files-list))
          (xref-find-references buffer)
          (meain/imenu-or-eglot buffer)
          (tree-jump-search buffer)
          (consult-tree-jump-search buffer) ;; TODO: does not work
          (consult-buffer flat)
          (t flat)))
  (setq vertico-multiform-categories
        '((file grid)
          (consult-grep buffer)
          (t flat)))
  (vertico-mode))
(use-package savehist :config (savehist-mode t) :defer t)
(use-package orderless
  :ensure t
  :config
  (setq completion-styles '(orderless basic))

  (defun flex-if-twiddle (pattern _index _total)
    (cond ((string-suffix-p "~" pattern)
           `(orderless-flex . ,(substring pattern 0 -1)))
          ((string-prefix-p "~" pattern)
           `(orderless-flex . ,(substring pattern 1)))))
  (defun initialism-if-comma (pattern index _total)
    (cond ((string-suffix-p "," pattern)
           `(orderless-initialism . ,(substring pattern 0 -1)))
          ((string-prefix-p "," pattern)
           `(orderless-initialism . ,(substring pattern 1)))))
  (defun without-if-bang (pattern _index _total)
    (cond
     ((equal "!" pattern)
      '(orderless-literal . ""))
     ((string-suffix-p "!" pattern)
      `(orderless-without-literal . ,(substring pattern 0 -1)))
     ((string-prefix-p "!" pattern)
      `(orderless-without-literal . ,(substring pattern 1)))))

  (setq orderless-style-dispatchers '(initialism-if-comma
                                      flex-if-twiddle
                                      without-if-bang))
  (orderless-define-completion-style orderless+basic
    (orderless-matching-styles '(orderless-literal
                                 orderless-regexp)))
  (setq completion-category-overrides
        '((command (styles orderless+basic))
          (symbol (styles orderless+basic))
          (variable (styles orderless+basic))
          (file (styles basic partial-completion)))))
(use-package marginalia
  :ensure t
  :defer nil
  :bind (:map minibuffer-local-map ("C-b" . marginalia-cycle))
  :config (marginalia-mode))

;; Show completions option even when there is a typo
;; (use-package typo
;;   :ensure t
;;   :config (add-to-list 'completion-styles 'typo t))

;; Consult without consultation fees
(use-package consult
  :ensure t
  :after (xref evil)
  :defer t
  :config
  (setq consult-ripgrep-args "rg --null --line-buffered --color=never --max-columns=1000 --path-separator /\
      --smart-case --no-heading --line-number --hidden --follow --glob \"!.git/*\"")
  (setq xref-show-xrefs-function #'consult-xref)
  (setq xref-show-definitions-function #'consult-xref)
  (evil-set-command-property 'consult-imenu :jump t)
  :init
  (define-key evil-normal-state-map (kbd "<SPC> /") 'consult-line))

;; Embark stuff
(use-package embark
  :defer t
  :ensure t
  :config
  (setq prefix-help-command #'embark-prefix-help-command)
  :init
  (global-set-key (kbd "C-'")  'embark-act)
  (global-set-key (kbd "C-.")  'embark-export)
  (global-set-key (kbd "C-h B")  'embark-bindings))
(use-package embark-consult
  :ensure t
  :defer t
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(provide 'completions)
;;; completions.el ends here
