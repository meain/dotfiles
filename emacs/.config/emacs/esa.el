;; esa.el --- Emacs client for meain/esa agent framework -*- lexical-binding: t; -*-

;;; Commentary:
;; This package provides an Emacs client for the meain/esa agent framework.
;; Custom mode which will also syntax highlight tool/mcp calls, ie things that start with $ and # (not sure if it will conflict - or we can force it off here)
;; TODO: Using an actual termainl might allow us to also return confirmation stuff (or should we parse and show?)
;; TODO: In case of an active selection, send that via stdin (should we send the filename as well?)

;;; Code:
(defun esa--base (message agent continue)
  "Send the `MESSAGE` to esa and stream the response in a markdown buffer, without appending process messages. Always create a new buffer."
  (let* ((buffer-name (generate-new-buffer-name
                       (format "*%s-%s-%s*" "esa" (or agent "default") (or continue "new"))))
         (buffer (generate-new-buffer buffer-name)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (gfm-mode)
        (erase-buffer)))
    (display-buffer buffer)
    (let* ((args (append (when (not (null continue)) (list "-c" "-C" (format "%d" continue)))
                         (list "--")
                         (when (not (null agent))
                           (list (format "+%s" agent)))
                         (list message)))
           (proc (apply #'start-process
                        (format "%s%s" "esa" (if (not (null agent))
                                                 (format "-%s" agent) ""))
                        buffer
                        "esa"
                        args)))
      (set-process-sentinel
       proc
       (lambda (_process _event)
         ;; Do nothing: suppress process messages in the buffer.
         )))))

(defun esa--list-agents ()
  "Return a list of available agents for esa."
  (seq-map (lambda (x)
             (cons
              (car (string-split (cadr (string-split x "(")) ")"))
              (s-trim x)))
           (seq-filter (lambda (x) (s-starts-with-p "  " x))
                       (string-split (shell-command-to-string "esa --list-agents") "\n"))))

(defun esa--list-conversations ()
  "Return a list of available conversations for esa."
  (seq-map (lambda (x)
             (cons
              (s-trim (cadr (string-split x ": ")))
              (s-trim
               (car (string-split x ": ")))))
           (seq-filter (lambda (x) (s-starts-with-p "  " x))
                       (string-split (shell-command-to-string "esa --list-history") "\n"))))

(defun esa (message &optional agent)
  "Send a `MESSAGE` to esa and stream the response in a markdown buffer.

If `AGENT` is a string, it will be used as the agent name.
If `AGENT` is nil, the default agent will be used.
If `AGENT' is a universal-argument prefix, it will show a picker for the agent."
  (interactive "sMessage: \nP")
  (let ((agent (if (equal agent '(4))
                   (completing-read "Select Agent: " (esa--list-agents)))))
    (esa--base message agent nil)))

(defun esa-continue (message &optional pick-conversation)
  "Continue a conversation with `MESSAGE` in esa.
If `PICK-CONVERSATION` is a universal-argument prefix, it will show a picker for the conversation."
  (interactive "sMessage: \nP")
  (let ((conversation (if (equal pick-conversation '(4))
                          (cdr (assoc
                                (completing-read "Select Conversation: " (esa--list-conversations))
                                (esa--list-conversations)))
                        1)))
    (esa--base message nil conversation)))

(defun esa-show-output (&optional pick-conversation)
  "Show the output of one of the previous esa converstaions in a new buffer.
If `PICK-CONVERSATION` is a universal-argument prefix, it will show a picker for the converstaion."
  (interactive "P")
  (let* ((converstaions (esa--list-conversations))
         (converstaion (if (equal pick-conversation '(4))
                     (cdr (assoc
                           (completing-read "Select Conversation: " (esa--list-conversations))
                           (esa--list-conversations)))
                   (cdr (car converstaions)))))
    (when converstaion
      (let* ((buffer-name (format "*esa-output-%s*" converstaion))
             (buffer (generate-new-buffer buffer-name)))
        (with-current-buffer buffer
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert (shell-command-to-string
                     (format "esa --show-output %s" converstaion)))
            (gfm-mode))
          (goto-char (point-min)))
        (display-buffer buffer)))))

;; Temporary
(global-unset-key (kbd "M-e"))
(global-set-key (kbd "M-e M-e") 'esa)
(global-set-key (kbd "M-e M-c") 'esa-continue)
(global-set-key (kbd "M-e M-o") 'esa-show-output)

(provide 'esa)
;;; esa.el ends here