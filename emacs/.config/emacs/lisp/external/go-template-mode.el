;;; go-template-mode.el --- Major mode for Go template language

;;; Commentary:

;; Pulled from https://gist.github.com/grafov/10985431
;; 1) Copy this file somewhere in your Emacs `load-path'.  To see what
;;    your `load-path' is, run inside emacs: C-h v load-path<RET>
;;
;; 2) Add the following to your .emacs file:
;;
;;    (require 'go-template-mode)

;;; Known Bugs:

;; 1) Highlights all strings in the source file, including HTML attributes,
;;    and does not properly highlight template actions inside these strings.

(defvar go-template-mode-syntax-table
  (let ((st (make-syntax-table)))
    ;; Add _ to :word: character class
    (modify-syntax-entry ?_  "w" st)

    ;; Operators (punctuation)
    (modify-syntax-entry ?:  "." st)
    (modify-syntax-entry ?=  "." st)
    (modify-syntax-entry ?|  "." st)

    ;; Strings and comments are font-locked separately.
    (modify-syntax-entry ?\" "." st)
    (modify-syntax-entry ?\' "." st)
    (modify-syntax-entry ?`  "." st)
    (modify-syntax-entry ?\\ "." st)

    st)
  "Syntax table for Go template mode.")

(defvar go-template-mode-keywords
  '("define" "else" "end" "if" "range" "template" "with")
  "All keywords in the Go template language.  Used for font locking.")

(defvar go-template-mode-builtins
  '("and" "html" "index" "js" "len" "not" "or" "print" "printf" "println" "urlquery")
  "All builtin functions in the Go template language.  Used for font locking.")


(defconst go-template-mode-pair-tag
	(regexp-opt
	 '("a" "abbr" "acronym" "address" "applet" "area" "b" "bdo"
		 "big" "blockquote" "body" "button" "caption" "center" "cite"
		 "code" "col" "colgroup" "dd" "del" "dfn" "dif" "div" "dl"
		 "dt" "em" "fieldset" "font" "form" "frame" "frameset" "h1"
		 "header" "nav" "footer" "section"
		 "h2" "h3" "h4" "h5" "h6" "head" "html" "i" "iframe" "ins"
		 "kbd" "label" "legend" "li" "link" "map" "menu" "noframes"
		 "noscript" "object" "ol" "optgroup" "option" "p" "pre" "q"
		 "s" "samp" "script" "select" "small" "span" "strike"
		 "strong" "style" "sub" "sup" "table" "tbody" "td" "textarea"
		 "tfoot" "th" "thead" "title" "tr" "tt" "u" "ul" "var")
	 t))
(defconst go-template-mode-standalone-tag
	(regexp-opt
	 '("base" "br" "hr" "img" "input" "meta" "param")
	 t))

(defconst go-template-mode-font-lock-keywords
  `((go-template-mode-font-lock-cs-comment 0 font-lock-comment-face t)
		(go-template-mode-font-lock-cs-string 0 font-lock-string-face t)
		(,(regexp-opt '("{{" "}}"))  (0 font-lock-preprocessor-face))
		("$[a-zA-Z0-9]*" (0 font-lock-variable-name-face))
    (,(regexp-opt go-template-mode-keywords 'words) . font-lock-keyword-face)
    (,(regexp-opt go-template-mode-builtins 'words) . font-lock-builtin-face)
		(,(concat "</?" go-template-mode-pair-tag ">?") (0 font-lock-function-name-face))
    (,(concat "<" go-template-mode-standalone-tag ">?") (0 font-lock-function-name-face))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parser
;;

(defvar go-template-mode-mark-cs-end 1
  "The point at which the comment/string cache ends.  The buffer
will be marked from the beginning up to this point (that is, up
to and including character (1- go-template-mode-mark-cs-end)).")
(make-variable-buffer-local 'go-template-mode-mark-cs-end)

(defvar go-template-mode-mark-nesting-end 1
  "The point at which the nesting cache ends.  The buffer will be
marked from the beginning up to this point.")
(make-variable-buffer-local 'go-template-mode-mark-nesting-end)

(defun go-template-mode-mark-clear-cache (b e)
  "A before-change-function that clears the comment/string and
nesting caches from the modified point on."

  (save-restriction
    (widen)
    (when (<= b go-template-mode-mark-cs-end)
      ;; Remove the property adjacent to the change position.
      ;; It may contain positions pointing beyond the new end mark.
      (let ((b (let ((cs (get-text-property (max 1 (1- b)) 'go-template-mode-cs)))
		 (if cs (car cs) b))))
	(remove-text-properties
	 b (min go-template-mode-mark-cs-end (point-max)) '(go-template-mode-cs nil))
	(setq go-template-mode-mark-cs-end b)))
    (when (< b go-template-mode-mark-nesting-end)
      (remove-text-properties b (min go-template-mode-mark-nesting-end (point-max)) '(go-template-mode-nesting nil))
      (setq go-template-mode-mark-nesting-end b))))

(defmacro go-template-mode-parser (&rest body)
  "Evaluate BODY in an environment set up for parsers that use
text properties to mark text.  This inhibits changes to the undo
list or the buffer's modification status and inhibits calls to
the modification hooks.  It also saves the excursion and
restriction and widens the buffer, since most parsers are
context-sensitive."

  (let ((modified-var (make-symbol "modified")))
    `(let ((buffer-undo-list t)
           (,modified-var (buffer-modified-p))
           (inhibit-modification-hooks t)
           (inhibit-read-only t))
       (save-excursion
         (save-restriction
           (widen)
           (unwind-protect
               (progn ,@body)
             (set-buffer-modified-p ,modified-var)))))))

(defun go-template-mode-cs (&optional pos)
  "Return the comment/string state at point POS.  If point is
inside a comment or string (including the delimiters), this
returns a pair (START . END) indicating the extents of the
comment or string."

  (unless pos
    (setq pos (point)))
  (when (> pos go-template-mode-mark-cs-end)
    (go-template-mode-mark-cs pos))
  (get-text-property pos 'go-template-mode-cs))

(defun go-template-mode-mark-cs (end)
  "Mark comments and strings up to point END.  Don't call this
directly; use `go-template-mode-cs'."
  (setq end (min end (point-max)))
  (go-template-mode-parser
   (save-match-data
     (let ((pos
	    ;; Back up to the last known state.
	    (let ((last-cs
		   (and (> go-template-mode-mark-cs-end 1)
			(get-text-property (1- go-template-mode-mark-cs-end) 
					   'go-template-mode-cs))))
	      (if last-cs
		  (car last-cs)
		(max 1 (1- go-template-mode-mark-cs-end))))))
       (while (< pos end)
	 (goto-char pos)
	 (let ((cs-end			; end of the text property
		(cond
		 ((looking-at "{{/\\*")
		  (goto-char (+ pos 4))
		  (if (search-forward "*/}}" (1+ end) t)
		      (point)
		    end))
		 ((looking-at "\"")
		  (goto-char (1+ pos))
		  (if (looking-at "[^\"\n\\\\]*\\(\\\\.[^\"\n\\\\]*\\)*\"")
		      (match-end 0)
		    (end-of-line)
		    (point)))
		 ((looking-at "'")
		  (goto-char (1+ pos))
		  (if (looking-at "[^'\n\\\\]*\\(\\\\.[^'\n\\\\]*\\)*'")
		      (match-end 0)
		    (end-of-line)
		    (point)))
		 ((looking-at "`")
		  (goto-char (1+ pos))
		  (while (if (search-forward "`" end t)
			     (if (eq (char-after) ?`)
				 (goto-char (1+ (point))))
			   (goto-char end)
			   nil))
		  (point)))))
	   (cond
	    (cs-end
	     (put-text-property pos cs-end 'go-template-mode-cs (cons pos cs-end))
	     (setq pos cs-end))
	    ((re-search-forward "[\"'`]\\|{{/\\*" end t)
	     (setq pos (match-beginning 0)))
	    (t
	     (setq pos end)))))
       (setq go-template-mode-mark-cs-end pos)))))



(defun go-template-mode-font-lock-cs (limit comment)
  "Helper function for highlighting comment/strings.  If COMMENT is t,
set match data to the next comment after point, and advance point
after it.  If COMMENT is nil, use the next string.  Returns nil
if no further tokens of the type exist."
  ;; Ensures that `next-single-property-change' below will work properly.
  (go-template-mode-cs limit)
  (let (cs next (result 'scan))
    (while (eq result 'scan)
      (if (or (>= (point) limit) (eobp))
	  (setq result nil)
	(setq cs (go-template-mode-cs))
	(if cs
	    (if (eq (= (char-after (car cs)) ?/) comment)
		;; If inside the expected comment/string, highlight it.
		(progn
		  ;; If the match includes a "\n", we have a
		  ;; multi-line construct.  Mark it as such.
		  (goto-char (car cs))
		  (when (search-forward "\n" (cdr cs) t)
		    (put-text-property
		     (car cs) (cdr cs) 'font-lock-multline t))
		  (set-match-data (list (car cs) (cdr cs) (current-buffer)))
		  (goto-char (cdr cs))
		  (setq result t))
	      ;; Wrong type.  Look for next comment/string after this one.
	      (goto-char (cdr cs)))
	  ;; Not inside comment/string.  Search for next comment/string.
	  (setq next (next-single-property-change
		      (point) 'go-template-mode-cs nil limit))
	  (if (and next (< next limit))
	      (goto-char next)
	    (setq result nil)))))
    result))

(defun go-template-mode-font-lock-cs-string (limit)
  "Font-lock iterator for strings."
  (go-template-mode-font-lock-cs limit nil))

(defun go-template-mode-font-lock-cs-comment (limit)
  "Font-lock iterator for comments."
  (go-template-mode-font-lock-cs limit t))

;;;###autoload
(define-derived-mode go-template-mode fundamental-mode "Go-Template"
	"Major mode for editing Go template text.

This provides basic syntax highlighting for keyword, built-ins, functions,
and some types. It does not provide indentation."

	;; Font lock
  (set (make-local-variable 'font-lock-defaults)
			 '(go-template-mode-font-lock-keywords nil nil nil nil))

  ;; Remove stale text properties
  (save-restriction
    (widen)
    (remove-text-properties 1 (point-max)
                            '(go-template-mode-cs nil go-template-mode-nesting nil)))

  ;; Reset the syntax mark caches
  (setq go-template-mode-mark-cs-end      1
        go-template-mode-mark-nesting-end 1)
  (add-hook 'before-change-functions #'go-template-mode-mark-clear-cache nil t)

	;; Use tabs (Go style)
  (setq indent-tabs-mode t))

(add-to-list 'auto-mode-alist '("\\.gotmpl$" . go-template-mode))
(add-to-list 'auto-mode-alist '("\\.tmpl$" . go-template-mode))

(provide 'go-template-mode)

;;; go-template-mode.el ends here