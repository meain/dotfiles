;;; vc-review.el --- Review VC diffs with inline comments -*- lexical-binding: t; -*-

;;; Commentary:
;; A minor package for annotating jj (or other VC) diffs with inline review
;; comments.  Review files are saved as .vcr files under
;; ~/.local/share/vc-review/<change-id>.vcr.
;;
;; Comments use the format:   # REVIEW: <text>
;;
;; Keybindings in vc-review-mode:
;;   C-c c   insert a review comment (works in all evil states)
;;   C-c C-c extract all reviews with diff context and copy to clipboard

;;; Code:

(require 'diff-mode)
(require 'vc-jj)

(defconst vc-review-dir (expand-file-name "~/.local/share/vc-review")
  "Directory where .vcr review files are stored.")

(defun vc-review-add-comment (beg end)
  "Insert a review comment after the current line or selection."
  (interactive "r")
  (let* ((use-region (and (fboundp 'evil-visual-state-p)
                          (evil-visual-state-p)
                          (use-region-p)))
         (end-pos (if use-region (max beg end) (point))))
    (when use-region (deactivate-mark))
    (goto-char end-pos)
    (end-of-line)
    (newline)
    (insert "# REVIEW: "))
  (when (fboundp 'evil-insert-state)
    (evil-insert-state)))

(defun vc-review-collect ()
  "Extract all REVIEW comments with diff context and copy to kill-ring.
For each # REVIEW: line, includes up to 10 lines of diff context above it,
stopping at the nearest @@ hunk header."
  (interactive)
  (let ((result ""))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^# REVIEW: " nil t)
        (let* ((comment-line (line-beginning-position))
               (comment-text (buffer-substring comment-line (line-end-position)))
               (context-start
                (save-excursion
                  (let ((hunk-pos nil)
                        (lines 0))
                    (goto-char comment-line)
                    (while (and (> (point) (point-min))
                                (< lines 10)
                                (not hunk-pos))
                      (forward-line -1)
                      (setq lines (1+ lines))
                      (when (looking-at "^@@")
                        (setq hunk-pos (point))))
                    (point))))
               (filename
                (save-excursion
                  (goto-char comment-line)
                  (and (re-search-backward "^+++ " nil t)
                       (buffer-substring (point) (line-end-position))))))
          (setq result
                (concat result
                        (when filename (concat filename "\n"))
                        (buffer-substring context-start comment-line)
                        comment-text "\n\n")))))
    (if (string-empty-p result)
        (message "No REVIEW comments found")
      (kill-new result)
      (message "Copied %d review(s) to clipboard"
               (length (split-string result "\n\n" t))))))

(defvar-keymap vc-review-mode-map
  :doc "Keymap for vc-review-mode."
  "C-c c"   #'vc-review-collect
  "C-c C-c" #'vc-review-add-comment)

;;;###autoload
(define-derived-mode vc-review-mode diff-mode "VC-Review"
  "Major mode for annotating VC diffs with inline review comments.
Derives from `diff-mode' for syntax highlighting.  The buffer is
writable so you can insert # REVIEW(...): comments inline."
  (setq buffer-read-only nil))

;;;###autoload
(defun vc-review-open (change-id)
  "Open (or reopen) a review buffer for CHANGE-ID.
Loads ~/.local/share/vc-review/<change-id>.vcr if it exists, otherwise
populates the buffer from `vc-jj-diff'.  The buffer is set up in
`vc-review-mode' and visits the .vcr file so C-c C-c saves it."
  (interactive "sChange ID: ")
  (unless (file-directory-p vc-review-dir)
    (make-directory vc-review-dir t))
  (let* ((review-file (expand-file-name (concat change-id ".vcr") vc-review-dir))
         (buf (get-buffer-create (format "*vc-review: %s*"
                                         (substring change-id 0 8)))))
    (if (file-exists-p review-file)
        (with-current-buffer buf
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert-file-contents review-file))
          (vc-review-mode)
          (set-visited-file-name review-file t)
          (goto-char (point-min)))
      ;; Fresh review: populate from vc-jj-diff async, then set mode
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)))
      (vc-jj-diff nil change-id change-id buf)
      (with-current-buffer buf
        (vc-run-delayed
          (vc-review-mode)
          (set-visited-file-name review-file t)
          (goto-char (point-min)))))
    (pop-to-buffer buf)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.vcr\\'" . vc-review-mode))

(provide 'vc-review)
;;; vc-review.el ends here
