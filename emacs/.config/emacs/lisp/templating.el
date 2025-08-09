;;; templating.el --- Templating related package -*- lexical-binding: t; -*-

;;; Commentary:
;; This file contains personal configuration for templating related
;; packages.

;;; Code:
;; vim-printer remake in elisp
(use-package emacs
  :after evil
  :commands (meain/quick-print)
  :config
  (defun meain/quick-print ()
    "Quickly print the variable your cursor is under or the active region."
    (interactive)
    (let* ((thing-to-print (if (use-region-p)
                               (buffer-substring (region-beginning) (region-end))
                             (symbol-name (symbol-at-point))))
           (escaped-thing-to-print (string-replace "\"" "\\\"" thing-to-print)))
      (let* ((filename (car (reverse (string-split (buffer-file-name) "/"))))
             (prefix (format "%s:%s" filename (line-number-at-pos)))
             (template
              (pcase major-mode
                ((or 'emacs-lisp-mode 'lisp-interaction-mode) "(message \"{pfx} {esc}: %s\" {thing})")
                ((or 'rust-mode 'rust-ts-mode) "println!(\"{pfx} {esc}: {{:?}}\", {thing});")
                ((or 'go-mode 'go-ts-mode) "fmt.Println(\"{pfx} {esc}:\", {thing})")
                ((or 'lua-mode) "print(\"{pfx} {esc}:\", {thing})")
                ((or 'js-mode 'js-ts-mode 'typescript-ts-mode 'web-mode) "console.log(\"{pfx} {esc}:\", {thing})")
                ((or 'shell-script-mode 'sh-mode) "echo \"{pfx} {esc}:\" {thing}")
                ((or 'python-ts-mode 'python-mode) "print(\"{pfx} {esc}:\", {thing})")
                (_ (error "Unknown mode for quick-print")))))
        (if current-prefix-arg
            (evil-open-above 1)
          (evil-open-below 1))
        (insert (string-replace
                 "{thing}" thing-to-print
                 (string-replace "{esc}" escaped-thing-to-print
                                 (string-replace "{pfx}" prefix template))))))
    (evil-force-normal-state))
  :init
  (define-key evil-normal-state-map (kbd "g p") 'meain/quick-print))

(use-package autoinsert
  :init
  (setq auto-insert-query nil) ;; Don't ask before inserting
  :config
  (auto-insert-mode 1)

  (define-auto-insert
    "\\.el\\'"
    '(
      "Emacs Lisp header"
      ";;; " (file-name-nondirectory (buffer-file-name)) " --- " _ " -*- lexical-binding: t; -*-\n\n"
      ";;; Commentary:\n"
      ";; \n\n"
      ";;; Code:\n\n\n"
      "(provide '" (file-name-base) ")\n"
      ";;; " (file-name-nondirectory (buffer-file-name)) " ends here\n"
      )))

(provide 'templating)
;;; templating.el ends here
