;;; evil-textobj-function.el --- evil textobj function

;;; Commentary:

;;; Code:

(require 'evil)
(require 'tree-sitter)

(defgroup evil-textobj-function nil "Text object function for Evil"
  :group 'evil)

(defcustom evil-textobj-function-i-key "f"
  "Keys for evil-inner-function" :type 'string
  :group 'evil-textobj-function)

(defcustom evil-textobj-function-a-key "f"
  "Keys for evil-a-function" :type 'string
  :group 'evil-textobj-function)

(defun evil-function-range (count beg end type &optional inclusive)
  (if inclusive
      (evil-range (progn
                    (goto-char (thread-first (meain/ts-get-func-like-thing)
                                 (tree-sitter-node-at-point)
                                 (tsc-node-byte-range)
                                 (car)))
                    (line-beginning-position))
                  (thread-first (meain/ts-get-func-like-thing)
                    (tree-sitter-node-at-point)
                    (tsc-node-byte-range)
                    (cdr)))
    (evil-range (thread-first (meain/ts-get-func-like-thing)
                  (tree-sitter-node-at-point)
                  (tsc-node-byte-range)
                  (car))
                (thread-first (meain/ts-get-func-like-thing)
                  (tree-sitter-node-at-point)
                  (tsc-node-byte-range)
                  (cdr)))))

(evil-define-text-object evil-a-function
  (count &optional beg end type)
  "Select range between a character by which the command is followed."
  (evil-function-range count beg end type t))
(evil-define-text-object evil-inner-function
  (count &optional beg end type)
  "Select inner range between a character by which the command is followed."
  (evil-function-range count beg end type))

(define-key evil-outer-text-objects-map evil-textobj-function-a-key
  'evil-a-function)
(define-key evil-inner-text-objects-map evil-textobj-function-i-key
  'evil-inner-function)

(provide 'evil-textobj-function)

;;; evil-textobj-function.el ends here
