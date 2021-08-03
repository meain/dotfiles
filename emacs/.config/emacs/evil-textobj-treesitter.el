;;; evil-textobj-treesitter.el --- evil textobj treesitter

;;; Commentary:
;; Tree sitter queries are pulled from https://github.com/nvim-treesitter/nvim-treesitter-textobjects

;;; Code:

(require 'evil)
(require 'tree-sitter)

(defgroup evil-textobj-treesitter nil "Text objects based on treesitter for Evil"
  :group 'evil)


(defvar evil-textobj-treesitter-queries (make-hash-table)
  "Map between `major-mode' and their language bundles of tree sitter queries.")
(puthash 'rust-mode "rust" evil-textobj-treesitter-queries)
(puthash 'python-mode "python" evil-textobj-treesitter-queries)
(puthash 'go-mode "go" evil-textobj-treesitter-queries)

(defun evil-textobj-treesitter--get-capture-groups ()
  "Util function to list all the available capture groups for a specific language."
  (let* ((m-lang-file (gethash major-mode evil-textobj-treesitter-queries))
         (m-ts-query-filename (concat "~/.config/emacs/ts-queries/" m-lang-file
                                      "/textobjects.scm"))
         (m-ts-debugging-query (with-temp-buffer
                                 (insert-file-contents m-ts-query-filename)
                                 (buffer-string)))
         (m-ts-root-node (tsc-root-node tree-sitter-tree))
         (m-ts-query (tsc-make-query tree-sitter-language m-ts-debugging-query))
         (m-ts-captures (tsc-query-captures m-ts-query m-ts-root-node
                                            #'tsc--buffer-substring-no-properties))
         (m-previous nil))
    (delete-dups (save-match-data (let ((pos 0) matches)
                                    (while (string-match "@[a-z\.]+" m-ts-debugging-query
                                                         pos)
                                      (push (match-string 0 m-ts-debugging-query)
                                            matches)
                                      (setq pos (match-end 0)))
                                    matches)))))

(defun evil-textobj-treesitter-print-capture-groups ()
  "Util function to list all the available capture groups for a specific language."
  (interactive)
  (cl-loop for
           entry
           in
           (evil-textobj-treesitter--get-capture-groups)
           do
           (message "%s" entry)))

(defun evil-textobj-treesitter--nodes-within (nodes)
  "NODES which contain the current point insdie them ordered inside out."
  (sort (remove-if-not (lambda (x)
                         (and (< (car (tsc-node-byte-range x)) (point))
                              (> (cdr (tsc-node-byte-range x)) (point))))
                       nodes)
        (lambda (x y)
          (< (+ (abs (- (point)
                        (car (tsc-node-byte-range x))))
                (abs (- (point)
                        (cdr (tsc-node-byte-range x))))) (+ (abs (- (point)
                        (car (tsc-node-byte-range y))))
                (abs (- (point)
                        (cdr (tsc-node-byte-range y)))))))))

(defun evil-textobj-treesitter--nodes-after (nodes)
  "NODES which contain the current point before them ordered top to bottom."
  (remove-if-not (lambda (x)
                   (> (car (tsc-node-byte-range x)) (point)))
                 nodes))

(defun evil-textobj-treesitter--get-nodes (group count)
  "Get a list of viable nodes based on GROUP value.
They will be order with captures with point inside them first then the
ones that follow.  This will return n(COUNT) items."
  (let* ((m-lang-file (gethash major-mode evil-textobj-treesitter-queries))
         (m-ts-query-filename (concat "~/.config/emacs/ts-queries/" m-lang-file
                                      "/textobjects.scm"))
         (m-ts-debugging-query (with-temp-buffer
                                 (insert-file-contents m-ts-query-filename)
                                 (buffer-string)))
         (m-ts-root-node (tsc-root-node tree-sitter-tree))
         (m-ts-query (tsc-make-query tree-sitter-language m-ts-debugging-query))
         (m-ts-captures (tsc-query-captures m-ts-query m-ts-root-node
                                            #'tsc--buffer-substring-no-properties))
         (m-previous nil))
    (progn
      (setq filtered (seq-map #'cdr
                              (remove-if-not (lambda (x)
                                               (eq (car x) (intern group)))
                                             m-ts-captures)))
      (setq filtered (remove-duplicates filtered
                                        :test (lambda (x y)
                                                (and (= (car (tsc-node-byte-range x)) (car (tsc-node-byte-range y)))
                                                     (= (cdr (tsc-node-byte-range x)) (cdr (tsc-node-byte-range y)))))))
      (setq nodes-within (evil-textobj-treesitter--nodes-within filtered))
      (setq nodes-after (evil-textobj-treesitter--nodes-after filtered))
      (setq mappable (append nodes-within nodes-after))
      (subseq mappable 0 count))))


(defun evil-textobj-treesitter--range (count beg end type ts-group)
  "Get the range of the closeset item of type `TS-GROUP'.
Not processing `BEG', `END' as of now.  `COUNT' is supported even
thought it does not actually make sense in most cases as if we do
3-in-func the selections will not be continues, but we can only
provide the start and end as of now which is what we are doing.
`TYPE' can probably be used to append inner or outer."
  (let ((nodes (evil-textobj-treesitter--get-nodes ts-group
                                                   count))
        (min nil)
        (max nil))
    ;; Have to do this as we might have nested functions
    (cl-loop for
             node
             in
             nodes
             do
             (progn
               (if (or (equal min nil)
                       (< (car (tsc-node-byte-range node)) min))
                   (setq min (car (tsc-node-byte-range node))))
               (if (or (equal max nil)
                       (> (cdr (tsc-node-byte-range node)) max))
                   (setq max (cdr (tsc-node-byte-range node))))))
    (evil-range min max)))

(defmacro evil-textobj-treesitter-get-textobj (group)
  "Macro to create a textobj function from `GROUP'."
  (let (funsymbol (intern (concat "evil-textobj-treesitter-" group)))
    `(evil-define-text-object ,funsymbol
       (count &optional beg end type)
       (evil-textobj-treesitter--range count beg
                                       end type ,group))))

(provide 'evil-textobj-treesitter)
;;; evil-textobj-treesitter.el ends here
