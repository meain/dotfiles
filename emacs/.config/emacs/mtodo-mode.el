;;; mtodo-mode.el --- A simple major mode for todo files -*- lexical-binding: t -*-


;;; Commentary:

;; This provides a simple major mode for working with todo entries.
;; You have basically 4 symbols used to denote todo/heading
;; # -> marks headings
;; - -> marks todo items
;; * -> marks important todo items
;; x -> marks completed items
                                        ;
;; For the most part all that this is doing is just providing some
;; highlight groups and ability for the user to convert the "bullet"
;; type of the different items.  This combined with the drag-stuff
;; thing gives you a pretty good todo list.

;;; Code:

(defconst mtodo-mode-font-lock-keywords
  '(("^\\ *#.*" . font-lock-string-face)
    ("^\\ *\\*.*" . font-lock-function-name-face)
    ("^\\ *x.*" . font-lock-comment-face)))

(defun mtodo-mark--internal (source target)
  "Util function to facilitate cahnging mark from SOURCE to TARGET."
  (save-excursion
    (let ((end-of-line (progn
                         (end-of-line)
                         (point))))
      (beginning-of-line)
      (search-forward-regexp source end-of-line
                             t)
      (replace-match target))))

(defun mtodo-mark-done ()
  "Mark an item as done."
  (interactive)
  (mtodo-mark--internal "[-\\*]" "x"))

(defun mtodo-mark-undone ()
  "Mark an item as not done."
  (interactive)
  (mtodo-mark--internal "[x\\*]" "-"))

(defun mtodo-mark-important ()
  "Mark an item as important."
  (interactive)
  (mtodo-mark--internal "[x-]" "*"))

(defvar mtodo-mode-map nil "Keymap for `mtodo-mode'.")
(progn
  (setq mtodo-mode-map (make-sparse-keymap))
  (define-key mtodo-mode-map (kbd "C-c C-s") 'mtodo-mark-important)
  (define-key mtodo-mode-map (kbd "C-c C-c") 'mtodo-mark-done)
  (define-key mtodo-mode-map (kbd "C-c C-d") 'mtodo-mark-undone))

;;;###autoload
(define-derived-mode mtodo-mode
  fundamental-mode
  "mtodo"
  "Major mode for working with mtodo files."
  (setq font-lock-defaults '(mtodo-mode-font-lock-keywords))
  (use-local-map mtodo-mode-map))

;; Automatically use mtodo-mode for .mtodo files.
;;;###autoload
(add-to-list 'auto-mode-alist
             '("\\.mtodo\\'" . mtodo-mode))

(provide 'mtodo-mode)
;;; mtodo-mode.el ends here
