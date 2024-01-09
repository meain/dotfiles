;;; eglot-booster.el --- boost eglot using emacs-lsp-booster -*- lexical-binding: t; -*-
;; Copyright (C) 2024  J.D. Smith
;; https://github.com/blahgeek/emacs-lsp-booster/issues/1

;;; Commentary:

;; Boost eglot with emacs-lsp-booster.
;; 1. Download emacs-lsp-booster from
;;    https://github.com/blahgeek/emacs-lsp-booster
;; 2. In the cloned directory, build with cargo (rust):
;;    cargo build --release
;; 3. place the target/release/emacs-lsp-booster program somewhere on
;;    `exec-path'.
;; 4. M-x eglot-booster
;; 5. Use eglot like normal
;;
;; Note: works only with local lsp servers via standard input/output,
;; not remote LSP servers.

;;; Code:
(eval-when-compile (require 'cl-lib))
(require 'eglot)
(require 'jsonrpc)

(defun eglot-booster-plain-command (com)
  "Test if command COM is a plain eglot server command."
  (and (consp com)
       (not (integerp (cadr com)))
       (not (seq-intersection '(:initializationOptions :autoport) com))))

(defun eglot-booster ()
  "Boost plain eglot server programs with emacs-lsp-booster.
The emacs-lsp-booster program must be compiled and available on
variable `exec-path'.  Only local stdin/out based lsp servers can
be boosted."
  (interactive)
  (unless (executable-find "emacs-lsp-booster")
    (user-error "The emacs-lsp-booster program is not installed"))
  (if (get 'eglot-server-programs 'lsp-booster-p)
      (message "eglot-server-programs already boosted.")
    (let ((cnt 0)
	  (orig-read (symbol-function 'jsonrpc--json-read)))
      (dolist (entry eglot-server-programs)
	(cond
	 ((functionp (cdr entry))
	  (cl-incf cnt)
	  (let ((fun (cdr entry)))
	    (setcdr entry (lambda (&rest r) ; wrap function
			    (let ((res (apply fun r)))
			      (if (eglot-booster-plain-command res)
				  (cons "emacs-lsp-booster" res)
				res))))))
	 ((eglot-booster-plain-command (cdr entry))
	  (cl-incf cnt)
	  (setcdr entry (cons "emacs-lsp-booster" (cdr entry))))))
      (defalias 'jsonrpc--json-read
	(lambda ()
	  (or (and (= (following-char) ?#)
		   (let ((bytecode (read (current-buffer))))
		     (when (byte-code-function-p bytecode)
		       (funcall bytecode))))
	      (funcall orig-read))))
      (message "Boosted %d eglot-server-programs" cnt))
    (put 'eglot-server-programs 'lsp-booster-p t)))

(provide 'eglot-booster)
;;; eglot-booster.el ends here
