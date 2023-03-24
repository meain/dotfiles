;;; tree-jump.el --- Provides xref bindings using tree-sitter to find targets -*- lexical-binding: t; -*-

;;; Commentary:
;; The package adds go-to-definition and go-to-references like
;; bindings for xref.  The locations are generated behind the scenes
;; using tree-grepper [https://github.com/BrianHicks/tree-grepper] and
;; looking for symbols.  One can enable this adding this to the
;; `xref-backend-functions' list like below:
;; (add-to-list 'xref-backend-functions 'tree-jump-xref-backend)
;; This package was inspired by dump-jump.el.

;; TODO: We could change it in the future to just use the tags.scm
;; from file from respective language grammar repositories
;; TODO: It currently replies on a personal script of mine which
;; relies on tree-grepper.  I'm hoping to package this up into a
;; single binary in the future which we can download and install when
;; users pull down the package.
;; We could also implement it like ctags where we parse and write to a
;; file on disk(or sqlite) the entries and updating only re parsing the items
;; that changed between runs.  Now that we have this file, we use this
;; file from within Emacs to determine locations.

;;; Code:
(require 'cl-seq)
(require 'cl-generic)
(require 'xref)

(defun tree-jump--get-definitions ()
  "Get a list of all the items available for tree-jump."
  (when-let* ((symbols (shell-command-to-string ",symbol-search"))
              (choices (butlast (string-split symbols "\n"))))
    choices))

(defun tree-jump--get-references ()
  "Get a list of all the items available for tree-jump."
  (progn
    (message "%s" default-directory)
    (when-let* ((symbols (shell-command-to-string ",symbol-search ref"))
                (choices (butlast (string-split symbols "\n"))))
      choices)))

(defun tree-jump-search ()
  "Search for a symbol in the entire project."
  (interactive)
  (when-let* ((choices (tree-jump--get-definitions))
              (choice (completing-read "Choose entry:" choices))
              (splits (string-split choice ":"))
              (file (car splits))
              (line (string-to-number (car (cdr splits))))
              (column (string-to-number (car (cdr (cdr splits))))))
    (find-file file)
    (goto-char 0)
    (forward-line (- line 1))
    (forward-char (- column 1))
    (reposition-window)))

(defun tree-jump--find-symbol (symbol mode)
  "Show symbols matching `SYMBOL'.
Pass `MODE' to switch between definitions and references."
  (when-let* ((choices (cond
                        ((equal mode 'definitions) (tree-jump--get-definitions))
                        ((equal mode 'references) (tree-jump--get-references))))
              (filtered-choices (cl-remove-if-not
                                 (lambda (x) (string-match-p (concat ":" symbol "$") x))
                                 choices)))
    (mapcar (lambda (x)
              (let* ((splits (string-split x ":"))
                     (file (car splits))
                     (line (string-to-number (cadr splits)))
                     (column (string-to-number (caddr splits)))
                     (entry (cadddr (cdr splits))))
                (xref-make entry (xref-make-file-location file line (1- column)))))
            filtered-choices)))

;;;###autoload
(defun tree-jump-xref-backend ()
  "Tree-jump backend for Xref."
  'tree-jump)

(cl-defmethod xref-backend-identifier-at-point ((_ (eql tree-jump)))
  "Get identifier at point for `tree-jump'."
  (let ((current-symbol (symbol-at-point)))
    (when current-symbol
      (symbol-name current-symbol))))

(cl-defmethod xref-backend-identifier-completion-table ((_ (eql tree-jump)))
  "Return list of symbols in the current buffer."
  (mapcar (lambda (x)
            (cadddr (cdr (string-split x ":"))))
          (tree-jump--get-definitions)))

(cl-defmethod xref-backend-definitions ((_ (eql tree-jump)) symbol)
  "Find definition at point for `tree-jump' for `SYMBOL'."
  (tree-jump--find-symbol symbol 'definitions))

(cl-defmethod xref-backend-references ((_backend (eql tree-jump)) symbol)
  "Find references at point for `tree-jump' for `SYMBOL'."
  (tree-jump--find-symbol symbol 'references))

;; (cl-defmethod xref-backend-apropos ((_backend (eql tree-jump)) symbol)
;;   (tree-jump--find-symbol symbol))

(provide 'tree-jump)
;;; tree-jump.el ends here