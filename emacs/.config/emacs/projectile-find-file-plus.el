;;; Code:

(defvar +projectile-find-file-default-score 1
  "Default score given to all files in the search.")
(defvar +projectile-find-file-score-recent-file (* 100 +projectile-find-file-default-score)
  "Score to add if found in recentf.")
(defvar +projectile-find-file-score-open-file (* 10 +projectile-find-file-score-recent-file)
  "Score to consider for files already open as buffer.")
(defvar +projectile-find-file-score-filename-match +projectile-find-file-score-recent-file
  "Dynamic score if search query matches the filename.")
(defvar +projectile-find-file-score-filename-prefix-match (* 10 +projectile-find-file-score-filename-match)
  "Dynamic score if the start of the file name matches the search string.")
(defvar +projectile-find-file-score-file-basename-exact-match (* 10 +projectile-find-file-score-filename-prefix-match)
  "Dynamic score if search exactly matches the basename of the file.")
(defvar +projectile-find-file-score-filename-exact-match (* 10 +projectile-find-file-score-file-basename-exact-match)
  "Dynamic score if the filname exactly matches the search string.")
(defvar +projectile-find-file-score-length-factor -0.5
  "Multiplier factor for length.")


(defvar +projectile-find-file--open-file-hash)
(defvar +projectile-find-file--recent-file-hash)


(defun +projectile-find-file--initialize()
  "To be called before initiating `ivy-read' to set recentf and open files in hashes for a faster access later."
  (setq +projectile-find-file--open-file-hash (make-hash-table :size 200 :test 'equal))
  (let ((seq-score 100))
    (dolist (el (delete
                 (file-relative-name (or (buffer-file-name) "") (projectile-project-root))
                 (projectile-project-buffer-files)))
      (puthash el seq-score +projectile-find-file--open-file-hash)
      (if (> seq-score 1)
          (setq seq-score (1- seq-score)))))
  (setq +projectile-find-file--recent-file-hash (make-hash-table :size 200 :test 'equal))
    (dolist (el (projectile-recentf-files))
      (puthash el 100 +projectile-find-file--recent-file-hash)))

(defun +projectile-find-file--score-files(file-list &optional search)
  "Gives scores to all the files in the FILE-LIST.
    (\"file1\" \"file2\")
    by returning a assoc list with score
    ((\"file1\" . 100) (\"file2\" . 20)
   Takes SEARCH into account, if provided, to give search based scores.
   Assumes the FILE-LIST is already filtered with SEARCH."

  (mapcar (lambda(f)
            `(,f . ,(+projectile-find-file--score-file f search)))
          file-list))

(defun +projectile-find-file--score-file(file &optional search)
  (let ((score +projectile-find-file-default-score)
        (open-file-score (gethash file +projectile-find-file--open-file-hash))
        (recent-file-score (gethash file +projectile-find-file--recent-file-hash)))
    (if open-file-score
        (setq score (+ score +projectile-find-file-score-open-file open-file-score)))
    (if recent-file-score
        (setq score (+ score +projectile-find-file-score-recent-file recent-file-score)))
    (when (> (length search) 1)
      (setq score (+ score (* +projectile-find-file-score-length-factor (length file))))
        (setq score (+ score
                       (cond ((string= (file-name-nondirectory file) search) +projectile-find-file-score-filename-exact-match)
                             ((string= (file-name-base file) search) +projectile-find-file-score-file-basename-exact-match)
                             ((string-match-p
                               (concat "\\`"
                                       (funcall ivy--regex-function search))
                               (file-name-nondirectory file))
                              +projectile-find-file-score-filename-prefix-match)
                             ((string-match-p (regexp-quote search) (file-name-nondirectory file)) +projectile-find-file-score-filename-match)
                             (t 0)))))
    score))

(defun +projectile-find-file--score-rank(fileA fileB)
  (> (cdr fileA) (cdr fileB)))

(defun +projectile-find-file--sort-file-with-scores(candidates &optional regexp)
  (mapcar (lambda(f) (car f))
          (sort
           (+projectile-find-file--score-files candidates regexp)
           '+projectile-find-file--score-rank)))


(defun +projectile-find-file--matcher(regexp candidates)
  (+projectile-find-file--sort-file-with-scores
   (counsel--find-file-matcher regexp candidates) regexp))



(defun +projectile-find-file(&optional arg &rest _)
  (interactive "P")
  (if (and (eq projectile-require-project-root 'prompt)
           (not (projectile-project-p)))
      (counsel-projectile-find-file-action-switch-project)
    (projectile-maybe-invalidate-cache arg)
    (+projectile-find-file--initialize)
    (let* ((project-files (+projectile-find-file--sort-file-with-scores (projectile-current-project-files))))
      (ivy-read (projectile-prepend-project-name "Find file: ")
                project-files
                :matcher #'+projectile-find-file--matcher
                :require-match t
                :action (lambda (x) (interactive) (find-file x))
                :caller '+projectile-find-file))))

(provide '+projectile-find-file)
;;; +projectile-find-file.el ends here
