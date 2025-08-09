;;; debuggers.el --- Debugging related packages -*- lexical-binding: t; -*-

;;; Commentary:
;; Debugging related packages

;;; Code:
(use-package go-dlv
  :ensure t
  :defer t
  :config
  (defun meain/dlv-current-func ()
    (interactive)
    (let ((default-directory (if (boundp 'custom-src-directory)
                                 custom-src-directory
                               default-directory)))
      (call-interactively 'dlv-current-func)))
  (defun meain/dlv-replay ()
    (interactive)
    (let* ((default-default-directory (if (boundp 'custom-src-directory)
                                          custom-src-directory
                                        default-directory))
           (default-directory (completing-read
                               "Directory: "
                               (remove-if (lambda (x) (equalp x ""))
                                          (mapcar (lambda (x) (concat default-directory x))
                                                  (string-split (shell-command-to-string "fd -t d") "\n")))
                               nil t default-default-directory)))
      (dlv "dlv replay /home/meain/.local/share/rr/latest-trace")))
  (defun meain/dlv (&optional test)
    (interactive "P")
    (let* ((default-default-directory (if (boundp 'custom-src-directory)
                                          custom-src-directory
                                        default-directory))
           (default-directory (completing-read
                               "Directory: "
                               (remove-if (lambda (x) (equalp x ""))
                                          (mapcar (lambda (x) (concat default-directory x))
                                                  (string-split (shell-command-to-string "fd -t d") "\n")))
                               nil t default-default-directory)))
      (if test
          (let* ((dir-cmd (meain/toffee--get-test-command 'function))
                 (default-directory (car dir-cmd))
                 (command (cdr dir-cmd))
                 (test-dir (progn
                             (if (eq nil command)
                                 (error "No tests available"))
                             (car (reverse (string-split command " ")))))
                 (command-without-dir (string-join (reverse (cdr (reverse (string-split command " ")))) " "))
                 (dlv-command (s-replace-regexp
                               "^go test -v -run"
                               (format "dlv --backend rr test %s -- -test.v -test.run" test-dir)
                               command-without-dir)))
            (message dlv-command)
            (meain/copy-to-clipboard dlv-command)
            (dlv dlv-command))
        (call-interactively 'dlv))))
  :commands (dlv dlv-current-func meain/dlv meain/dlv-replay meain/dlv-current-func))

;; DAP client for Emacs
(use-package dape
  :ensure (dape :type git :host github :repo "svaante/dape")
  :disabled t
  :config
  (setq dape-inline-variables t) ;; Add inline variable hints, this feature is highly experimental
  (setq dape-repl-use-shorthand t) ;; Use n for next etc. in REPL
  (setq dape-cwd-fn 'meain/cwd-fn)
  ;; (remove-hook 'dape-on-start-hooks 'dape-info) ;; To remove info buffer on startup
  ;; (remove-hook 'dape-on-start-hooks 'dape-repl) ;; To remove repl buffer on startup
  ;; (add-hook 'dape-compile-compile-hooks 'kill-buffer) ;; Kill compile buffer on build success

  ;; Golang config
  (add-to-list 'dape-configs
               `(delve
                 modes (go-mode go-ts-mode)
                 command "dlv"
                 command-args ("dap" "--listen" "127.0.0.1:55878")
                 command-cwd meain/cwd-fn
                 host "127.0.0.1"
                 port 55878
                 :type "debug"
                 :request "launch"
                 :cwd meain/cwd-fn
                 :program meain/cwd-fn)))

(use-package gud
  :after (evil)
  :commands (gud-break gud-cont)
  :init
  (define-key evil-normal-state-map (kbd "<SPC> d r") 'gud-reset)
  (define-key evil-normal-state-map (kbd "<SPC> d b") 'gud-break)
  (define-key evil-normal-state-map (kbd "<SPC> d c") 'gud-cont)
  (define-key evil-normal-state-map (kbd "<SPC> d n") 'gud-next)
  (define-key evil-normal-state-map (kbd "<SPC> d s") 'gud-step)
  (define-key evil-normal-state-map (kbd "<SPC> d u") 'gud-up)
  (define-key evil-normal-state-map (kbd "<SPC> d g") 'gud-until))

(provide 'debuggers)
;;; debuggers.el ends here
