;;; Commentary:
;; Move to meain/tree-surgeon.el.  It will collect all the little
;; tree-sitter based thingies once we have a few.

;;; Code:

(defun tree-surgeon-split-join--closest-node (nodes)
  "Find node closes to point from `NODES'."
  (let ((distance 999999)
        (node nil)
        (point (point)))
    (progn
      (seq-do (lambda (x)
                (let* (
                       (range (tsc-node-byte-range (cdr x)))
                       (start (byte-to-position (car range)))
                       (end (byte-to-position (cdr range)))
                       (smallest (min (abs (- start point))
                                      (abs (- end point)))))
                  (unless (or (< distance smallest)
                              (> start point)
                              (< end point))
                    (setq node x)
                    (setq distance smallest))))
              nodes)
      node)))

(defvar tree-surgeon-split-join-settings
  '( ; (mode . ((node_types) . trailing_comma))
    (json-mode . ((object) . nil))
    (rust-mode . ((parameters arguments) . t))
    (go-mode . ((argument_list parameter_list) . t))))

(defun tree-surgeon-split-join--get-named-children (node)
  "Get list of direct children of NODE."
  (let (children)
    (dotimes (index (tsc-count-named-children node))
      (push (tsc-get-nth-named-child node index) children))
    (reverse children)))

(defun tree-surgeon-split-join ()
  "Split or join arguments."
  (interactive)
  (when-let* ((settings (alist-get major-mode tree-surgeon-split-join-settings))
              (root-node (tsc-root-node tree-sitter-tree))
              (node-selectors (car settings))
              (string-query-builder (lambda (x) (concat "(" (symbol-name x) ") @list")))
              (string-query (string-join (mapcar string-query-builder node-selectors) " "))
              (query (tsc-make-query tree-sitter-language string-query))
              (nodes (tsc-query-captures query root-node #'tsc--buffer-substring-no-properties))
              (node (cdr (tree-surgeon-split-join--closest-node nodes)))
              (children (tree-surgeon-split-join--get-named-children node))
              (range (tsc-node-byte-range node))
              (start (byte-to-position (car range)))
              (end (byte-to-position (cdr range)))
              (text-pieces (seq-map (lambda (x) (tsc-node-text x)) children))
              (joined (string-join text-pieces ", ")))
    (let* ((use-trailing (cdr settings))
           (join (s-contains-p "\n" (tsc-node-text node))))
      (unless (= (length children) 0)
        (goto-char (+ start 1))
        (delete-char (- end start 2))
        (if join
            (insert joined)
          (progn
            (mapc (lambda (x)
                    (newline-and-indent)
                    (insert (concat x ",")))
                  text-pieces)
            (unless use-trailing (delete-char -1)) ; delete trailing comma
            (forward-line)
            ;; We could also use `indent-region' on the whole thing
            (indent-for-tab-command)))
        (goto-char start)))))