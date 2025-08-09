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

(provide 'utils)
;;; utils.el ends here
