;;; checkers.el --- Checking for issues -*- lexical-binding: t; -*-

;;; Commentary:
;; Linters, formatters etc.

;;; Code:
;; Spell checking
(use-package flyspell
  :defer t
  :after ispell
  :commands (flyspell-prog-mode flyspell-mode flyspell-goto-next-error)
  :init
  (add-hook 'prog-mode-hook 'flyspell-prog-mode)
  (add-hook 'text-mode-hook 'flyspell-mode)
  :bind ("C-;" . flyspell-auto-correct-word)
  :config
  (setq flyspell-delay-use-timer t) ;; use timer instead of sit-for
  ;; Make flyspell work with tree-sitter
  (setq-default flyspell-prog-text-faces
                '(tree-sitter-hl-face:comment
                  tree-sitter-hl-face:doc
                  tree-sitter-hl-face:string
                  font-lock-comment-face
                  font-lock-doc-face
                  font-lock-string-face)))

(use-package flyspell-correct
  :ensure t
  :after flyspell
  :commands (flyspell-correct-wrapper flyspell-goto-next-error)
  :bind (:map flyspell-mode-map ("C-:" . flyspell-correct-wrapper)))

;; Code checking
(use-package flymake
  :defer 1
  :after evil
  :commands (flymake flymake-find-file-hook
                     flymake-goto-next-error
                     flymake-goto-prev-error)
  :init
  (add-hook 'find-file-hook 'flymake-find-file-hook)
  :config
  (setq flymake-show-diagnostics-at-end-of-line nil)
  (evil-set-command-property 'flymake-goto-next-error :jump t)
  (evil-set-command-property 'flymake-goto-prev-error :jump t))

(use-package flymake-diagnostic-at-point
  :ensure t
  :after (flymake evil-leader)
  :config
  (setq flymake-diagnostic-at-point-error-prefix "! ")
  (setq flymake-diagnostic-at-point-display-diagnostic-function 'flymake-diagnostic-at-point-display-minibuffer)
  (evil-leader/set-key "j" 'flymake-goto-next-error)
  (evil-leader/set-key "k" 'flymake-goto-prev-error)
  (add-hook 'flymake-mode-hook #'flymake-diagnostic-at-point-mode))

(use-package flymake-quickdef
  :ensure t
  :after flymake
  :config
  ;; tint lints
  (flymake-quickdef-backend flymake-check-tint
    :pre-let ((tint-exec (executable-find "tint")))
    :pre-check (unless tint-exec (error "Cannot find tint executable"))
    :write-type 'file
    :proc-form (list tint-exec "lint" fmqd-temp-file)
    :search-regexp "^\\([^:]+\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\): \\(.*\\)$"
    :prep-diagnostic (let* ((lnum (string-to-number (match-string 2)))
                            (col (string-to-number (match-string 3)))
                            (pos (flymake-diag-region fmqd-source lnum col))
                            (beg (car pos))
                            (end (cdr pos))
                            (msg (format "tint> %s" (match-string 6))))
                       (list fmqd-source beg end :warning msg)))
  (add-hook 'go-ts-mode-hook
            (lambda ()
              (add-hook 'flymake-diagnostic-functions 'flymake-check-tint nil t)))

  ;; https://github.com/crate-ci/typos
  (flymake-quickdef-backend flymake-check-typos
    :pre-let ((typos-exec (executable-find "typos")))
    :pre-check (unless typos-exec (error "Cannot find typos executable"))
    :write-type 'file
    :proc-form (list typos-exec "--hidden" "--format" "brief" fmqd-temp-file)
    :search-regexp "^\\([^:]+\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\): \\(.*\\)$"
    :prep-diagnostic (let* ((lnum (string-to-number (match-string 2)))
                            (col (string-to-number (match-string 3)))
                            (text (match-string 4))
                            (pos (flymake-diag-region fmqd-source lnum col))
                            (beg (car pos))
                            (end (cdr pos))
                            (msg (format "typos> %s" text)))
                       (list fmqd-source beg end :warning msg)))
  (add-hook 'flymake-diagnostic-functions 'flymake-check-typos)

  ;; https://github.com/rhysd/actionlint
  (flymake-quickdef-backend flymake-check-actionlint
    :pre-let ((actionlint-exec (executable-find "actionlint")))
    :pre-check (unless actionlint-exec (error "Cannot find actionlint executable"))
    :write-type 'file
    :proc-form (list actionlint-exec "-format" "{{range $err := .}}{{$err.Filepath}}:{{$err.Line}}:{{$err.Column}}:{{$err.Message}}\n{{end}}" fmqd-temp-file)
    :search-regexp "^\\([^:]+\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\):\\(.*\\)$"
    :prep-diagnostic (let* ((lnum (string-to-number (match-string 2)))
                            (col (string-to-number (match-string 3)))
                            (text (match-string 4))
                            (pos (flymake-diag-region fmqd-source lnum col))
                            (beg (car pos))
                            (end (cdr pos))
                            (msg (format "actionlint> %s" text)))
                       (list fmqd-source beg end :warning msg)))
  (add-hook 'yaml-mode-hook
            (lambda ()
              (if (string-match-p ".*\\.github/workflows/.*\\.ya?ml" (buffer-file-name))
                  (add-hook 'flymake-diagnostic-functions 'flymake-check-actionlint nil t))))

  ;; https://github.com/hadolint/hadolint
  (flymake-quickdef-backend flymake-hadolint
    :pre-let ((hadolint-exec (executable-find "hadolint")))
    :pre-check (unless hadolint-exec (error "Cannot find hadolint executable"))
    :write-type 'file
    :proc-form (list hadolint-exec "--no-color" fmqd-temp-file)
    :search-regexp "^\\([^:]+\\):\\([[:digit:]]+\\) \\(.*\\)$"
    :prep-diagnostic (let* ((lnum (string-to-number (match-string 2)))
                            (col 0)
                            (text (match-string 3))
                            (pos (flymake-diag-region fmqd-source lnum col))
                            (beg (car pos))
                            (end (cdr pos))
                            (msg (format "hadolint> %s" text)))
                       (list fmqd-source beg end :warning msg)))
  (add-hook 'dockerfile-mode-hook
            (lambda ()
              (add-hook 'flymake-diagnostic-functions 'flymake-hadolint nil t))))

(use-package flymake-collection
  :ensure t
  :defer t
  :hook (after-init . flymake-collection-hook-setup))

;; Code formatting
(use-package apheleia
  :ensure t
  :after evil
  :commands (apheleia-format-buffer meain/format-buffer)
  :config
  ;; json
  (setf (alist-get 'fixjson apheleia-formatters) '("fixjson"))
  (setf (alist-get 'json-mode apheleia-mode-alist) '(fixjson))

  ;; golang
  (setf (alist-get 'goimports apheleia-formatters) '("goimports"))
  (setf (alist-get 'gofumpt apheleia-formatters) '("gofumpt"))
  (setf (alist-get 'gci apheleia-formatters) '("gci" "/dev/stdin"))
  (setf (alist-get 'go-mode apheleia-mode-alist) '(goimports))
  (setf (alist-get 'go-ts-mode apheleia-mode-alist) '(goimports))

  ;; cedar
  (setf (alist-get 'cedar-format apheleia-formatters) '("cedar" "format"))
  (setf (alist-get 'cedar-mode apheleia-mode-alist) '(cedar-format))

  ;; markdown
  (setf (alist-get 'markdown-mode apheleia-mode-alist) '(prettier-markdown))

  ;; clojure
  (setf (alist-get 'zprint apheleia-formatters) '("zprint"))
  (setf (alist-get 'clojure-mode apheleia-mode-alist) '(zprint))

  ;; shell
  (setf (alist-get 'shell-script-mode apheleia-mode-alist) '(shfmt))
  (setf (alist-get 'sh-mode apheleia-mode-alist) '(shfmt))

  ;; nix
  (setf (alist-get 'nixpkgsfmt apheleia-formatters) '("nixpkgs-fmt"))
  (setf (alist-get 'nixfmt apheleia-formatters) '("nixfmt"))
  (setf (alist-get 'nix-mode apheleia-mode-alist) '(nixfmt))

  (defun meain/format-buffer ()
    "Format a buffer."
    (interactive)
    (cond
     ((eq major-mode 'emacs-lisp-mode) (indent-region (point-min) (point-max)))
     ((eq major-mode 'ledger-mode) (ledger-mode-clean-buffer))
     (t (call-interactively 'apheleia-format-buffer))))

  :init
  (add-hook 'go-mode-hook 'apheleia-mode)
  (add-hook 'go-ts-mode-hook 'apheleia-mode)
  (define-key evil-normal-state-map (kbd ",,") #'meain/format-buffer))

;; Code coverage in buffer
;; To get coverage, run `go test -coverprofile=coverage.out ./...`
;; and then convert this to lcov using
;; gcov2lcov -infile coverage.out -outfile coverage.lcov -use-absolute-source-paths
;; Now you can load this into coverlay
(use-package coverlay
  :ensure t
  :commands (coverlay-load-file)
  :config
  (setq coverlay:tested-line-background-color "#C9F3D2")
  (setq coverlay:untested-line-background-color "#F8CED3")
  (setq coverlay:mark-tested-lines nil))

;; Quick run current test
(use-package emacs
  :after (compile evil-leader)
  :commands (meain/toffee-run-test meain/toffee--get-test-command)
  :config
  ;; if available in another frame, don't recreate in current frame
  (push '("\\*compilation\\*" . (nil (reusable-frames . t))) display-buffer-alist)
  (defvar meain/toffee--previous-command nil)
  (defvar meain/toffee-run-previous-if-empty t)
  (defun meain/toffee--get-cwd ()
    "Get the current working directory to run the test.
For example if it is go-mod file, look up the go.mod file and use that directory."
    (pcase major-mode
      ('go-mode (locate-dominating-file (buffer-file-name) "go.mod"))
      ('go-ts-mode (locate-dominating-file (buffer-file-name) "go.mod"))
      (_ default-directory)))
  (defun meain/toffee--get-test-command (mode)
    (let ((default-directory
           (expand-file-name
            ;; custom-src-directory is supposed to come from .dir-locals.el
            (if (boundp 'custom-src-directory)
                custom-src-directory
              (meain/toffee--get-cwd))))
          (command
           (shell-command-to-string
            (cond
             ((eq mode 'function) (format "toffee --verbose '%s' '%s'" (buffer-file-name) (line-number-at-pos)))
             ((eq mode 'suite) (format "toffee --verbose '%s'" (buffer-file-name)))
             ((eq mode 'project) (format "toffee --verbose --full '%s'" (buffer-file-name)))
             (t (error "Unknown mode for meain/toffee--get-test-command"))))))
      (if (s-starts-with-p "Unable to find any test" command)
          (cons default-directory nil)
        (cons default-directory (s-trim command)))))
  (defun meain/toffee-run-previous-test ()
    "Run previous test."
    (interactive)
    (let* ((dir-cmd (meain/toffee--get-test-command  'function))
           (default-directory (car dir-cmd))
           (command meain/toffee--previous-command))
      (if command
          (progn (compile command))
        (message "Unable to find any tests"))))
  (defun meain/toffee-run-test (&optional _)
    "Run test based on `MODE'. By default runs current function.
Pass universal args to run suite or project level tests."
    (interactive "P")
    (let* ((mode (cond
                  ((equal current-prefix-arg nil) 'function)
                  ((equal current-prefix-arg '(4)) 'suite)
                  ((equal current-prefix-arg '(16)) 'project)))
           (dir-cmd (meain/toffee--get-test-command mode))
           (default-directory (car dir-cmd))
           (command (cdr dir-cmd)))
      (if command
          (progn
            (setq meain/toffee--previous-command command)
            (compile command))
        (if (and meain/toffee-run-previous-if-empty meain/toffee--previous-command)
            (progn
              (message "Could not find any tests, running previous test...")
              (compile (concat "nice " meain/toffee--previous-command)))
          (message "Unable to find any tests")))))
  :init
  (evil-leader/set-key "d" 'meain/toffee-run-test)
  (evil-leader/set-key "D" 'meain/toffee-run-previous-test))

(provide 'checkers)
;;; checkers.el ends here
