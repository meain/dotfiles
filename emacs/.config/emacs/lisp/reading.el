;;; reading.el --- Reading related packages -*- lexical-binding: t; -*-

;;; Commentary:
;; Packages that help me read stuff within emacs

;;; Code:
(use-package focus :ensure t :commands focus-mode)

;; Text to speech stuff
;; Useful for reading out llm explanations
(use-package read-aloud
  :after evil
  :ensure (:host github :repo "gromnitsky/read-aloud.el")
  :commands (read-aloud-buf read-aloud-this)
  :init
  (define-key evil-normal-state-map (kbd "<SPC> r b") 'read-aloud-buf)
  (define-key evil-normal-state-map (kbd "<SPC> r r") 'read-aloud-this)
  (define-key evil-normal-state-map (kbd "<SPC> r s") 'read-aloud-stop)

  (define-key evil-visual-state-map (kbd "<SPC> r b") 'read-aloud-buf)
  (define-key evil-visual-state-map (kbd "<SPC> r r") 'read-aloud-this)
  (define-key evil-visual-state-map (kbd "<SPC> r s") 'read-aloud-stop)
  :config
  (setq meain/read-aloud-speed "250")
  (defun meain/set-read-aloud-speed ()
    "Set the speed of read-aloud."
    (interactive)
    (let ((speed (completing-read "Speed: " '("0.5" "1" "1.5" "2" "2.5" "3"))))
      (setq meain/read-aloud-speed (number-to-string (floor (* (string-to-number speed) 100))))))

  (setq read-aloud-engine "macos")
  (setq read-aloud-engines
        '("custom" (cmd ",speak" args nil)
          "macos" (cmd "/usr/bin/say" args ("-r" meain/read-aloud-speed)))))


(provide 'reading)
;;; reading.el ends here
