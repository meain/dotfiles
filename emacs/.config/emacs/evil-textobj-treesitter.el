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

(defun evil-textobj-treesitter--get-node (group)
  "Get the closest node of type GROUP."
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
      (seq-do (lambda (x)
                (if (and (equal (car x) (intern group))
                         (> (point) (car (tsc-node-byte-range (cdr x)))))
                    (setq m-previous (cdr x))))
              m-ts-captures)
      m-previous)))

(defun evil-textobj-treesitter--range (count beg end type ts-group)
  "Get the range of the closeset item of type `TS-GROUP'.
Not processing `COUNT', `BEG', `END' as of now.
`TYPE' can probably be used to append inner or outer."
  (message "c:%s b:%s e:%s" count beg end)
  (let* ((node (evil-textobj-treesitter--get-node ts-group))
         (node-ranges (tsc-node-byte-range node)))
    (evil-range (car node-ranges)
                (cdr node-ranges))))

(defmacro evil-textobj-treesitter-get-textobj (group)
  "Macro to create a textobj function from `GROUP'."
  (let (funsymbol (intern (concat "evil-textobj-treesitter-" group)))
    `(evil-define-text-object ,funsymbol
       (count &optional beg end type)
       (evil-textobj-treesitter--range count beg
                                       end type ,group))))

(provide 'evil-textobj-treesitter)
;;; evil-textobj-treesitter.el ends here
