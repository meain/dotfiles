;;; utils.el --- Utility packages -*- lexical-binding: t; -*-

;;; Commentary:
;; Useful utilities uses elesewhere in the config.

;;; Code:
(defmacro ilambda (functionname &rest args)
  "Create an interactive lambda of existing function `FUNCTIONNAME' with `ARGS'."
  (let ((funsymbol (concat "i/" (symbol-name functionname))))
    `(cons ,funsymbol (lambda () (interactive) (apply #',functionname ',args)))))

;; macro for alternate pattern
(defmacro alambda (original alternate)
  "Macro for easily creating commands with alternate on `universal-argument'.
Pass ORIGINAL and ALTERNATE options."
  `(lambda (&optional use-alternate)
     (interactive "P")
     (if use-alternate ,alternate ,original)))

(use-package emacs
  :config
  :commands (meain/cwd-fn meain/use-custom-src-directory)
  :config
  (defun meain/cwd-fn ()
    (expand-file-name
     ;; custom-src-directory is supposed to come from .dir-locals.el
     (if (boundp 'custom-src-directory)
         custom-src-directory
       (or (when-let ((project (project-current)))
             (project-root project))
           default-directory))))

  (defun meain/use-custom-src-directory (orig-fn &rest args)
    "Use custom src directory as default directory.
Instead of `default-directory' when calling `ORIG-FN' with `ARGS'."
    (let ((default-directory
           (expand-file-name
            ;; custom-src-directory is supposed to come from .dir-locals.el
            (if (boundp 'custom-src-directory)
                custom-src-directory
              default-directory))))
      (apply orig-fn args))))

(provide 'utils)
;;; utils.el ends here
