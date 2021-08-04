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
  (let* ((lang-file (gethash major-mode evil-textobj-treesitter-queries))
         (query-filename (concat "~/.config/emacs/ts-queries/" lang-file
                                 "/textobjects.scm"))
         (debugging-query (with-temp-buffer
                            (insert-file-contents query-filename)
                            (buffer-string)))
         (root-node (tsc-root-node tree-sitter-tree))
         (query (tsc-make-query tree-sitter-language debugging-query))
         (captures (tsc-query-captures query root-node #'tsc--buffer-substring-no-properties))
         (filtered-captures (remove-if-not (lambda (x)
                                             (member (car x) group))
                                           captures))
         (nodes (seq-map #'cdr filtered-captures))
         (nodes-nodupes (remove-duplicates nodes
                                           :test (lambda (x y)
                                                   (and (= (car (tsc-node-byte-range x)) (car (tsc-node-byte-range y)))
                                                        (= (cdr (tsc-node-byte-range x)) (cdr (tsc-node-byte-range y)))))))
         (nodes-within (evil-textobj-treesitter--nodes-within nodes-nodupes))
         (nodes-after (evil-textobj-treesitter--nodes-after nodes-nodupes)))
    (subseq (append nodes-within nodes-after)
            0
            count)))

(defun evil-textobj-treesitter--range (count beg end type ts-group)
  "Get the range of the closeset item of type `TS-GROUP'.
Not processing `BEG', `END' as of now.  `COUNT' is supported even
thought it does not actually make sense in most cases as if we do
3-in-func the selections will not be continues, but we can only
provide the start and end as of now which is what we are doing.
`TYPE' can probably be used to append inner or outer."
  (let* ((nodes (evil-textobj-treesitter--get-nodes ts-group
                                                    count))
         (range-min (apply 'min
                           (seq-map (lambda (x)
                                      (car (tsc-node-byte-range x)))
                                    nodes)))
         (range-max (apply 'max
                           (seq-map (lambda (x)
                                      (cdr (tsc-node-byte-range x)))
                                    nodes))))
    ;; Have to compute min and max like this as we might have nested functions
    (evil-range range-min range-max)))

(defmacro evil-textobj-treesitter-get-textobj (group)
  "Macro to create a textobj function from `GROUP'.
You can pass in multiple groups as a list and in that case as long as
any one of them is vaild, it will be picked."
  (let* ((groups (if (eq (type-of group) 'string)
                     (list group)
                   group))
         (funsymbol (intern (concat "evil-textobj-treesitter-function--"
                                    (mapconcat 'identity groups "-"))))
         (interned-groups (map 'identity 'intern groups)))
    `(evil-define-text-object ,funsymbol
       (count &optional beg end type)
       (evil-textobj-treesitter--range count beg
                                       end type ',interned-groups))))

(provide 'evil-textobj-treesitter)
;;; evil-textobj-treesitter.el ends here
