;;; gptel-tools.el --- Tools for GPTel -*- lexical-binding: t; -*-

;;; Commentary:
;; This file defines various tools for GPTel
;; (load-file (concat user-emacs-directory "gptel-tools.el"))

;;; Code:
;; Search Web with ddgr CLI
(defun meain/gptel-tool--ddgr-search (query)
  "Run a web search for QUERY using the ddgr CLI, returning results as plain text.
Requires ddgr to be installed and available in your PATH."
  (if (executable-find "ddgr")
      (condition-case err
          (shell-command-to-string (concat "ddgr --noua --json " (shell-quote-argument query)))
        (error (format "Error performing search for '%s': %s" query (error-message-string err))))
    "Error: Search provider not available"))

(gptel-make-tool
 :function #'meain/gptel-tool--ddgr-search
 :name "web_search"
 :description "Search the web using for a query"
 :args (list '(:name "query"
                     :type string
                     :description "The search query"))
 :category "web")

;; Read a webpage
;; TODO: Explore https://github.com/adbar/trafilatura
(defun meain/gptel-tool--fetch-webpage (callback url)
  "Fetch webpage at URL and return Markdown to CALLBACK.
Convert its HTML content to Markdown via html2markdown CLI, and return
Markdown to CALLBACK.  On error, callback is invoked with an
appropriate message."
  (let ((temp-file (make-temp-file "gptel-html" nil ".html"))
        (output-buffer (generate-new-buffer " *gptel-html2md*")))
    (url-retrieve
     url
     (lambda (_status)
       (unwind-protect
           (condition-case err
               (progn
                 ;; Skip headers and write HTML to temp file
                 (goto-char (point-min))
                 (if (re-search-forward "\n\n" nil t)
                     (write-region (point) (point-max) temp-file nil 'silent)
                   (error "Failed to parse HTTP body"))
                 ;; Run html2markdown on the temp file
                 (if (executable-find "html2markdown")
                     (progn
                       (let ((exit-code
                              (call-process "html2markdown" temp-file output-buffer)))
                         (if (zerop exit-code)
                             (funcall callback
                                      (with-current-buffer output-buffer
                                        (buffer-string)))
                           (funcall callback "html2markdown CLI returned a non-zero exit code."))))
                   (funcall callback "html2markdown CLI not found on system PATH.")))
             (error (funcall callback
                             (format "Error: %s" (error-message-string err)))))
         ;; always clean up buffers and temp files
         (when (file-exists-p temp-file) (delete-file temp-file))
         (when (buffer-live-p output-buffer) (kill-buffer output-buffer))
         (when (buffer-live-p (current-buffer)) (kill-buffer (current-buffer))))))))

(gptel-make-tool
 :function #'meain/gptel-tool--fetch-webpage
 :name "fetch_webpage"
 :description "Fetch the contents of a webpage"
 :args (list
        '(:name "url"
                :type string
                :description "The HTTP or HTTPS URL of the webpage to fetch."))
 :async t
 :category "web"
 :confirm nil
 :include t)

;; Read a file
(defun meain/gptel-tool--read-file-lines (filepath &optional start-line end-line)
  "Read FILEPATH and return lines START-LINE to END-LINE (inclusive) as a string.
If lines are omitted, returns the full file.
Errors reported as a string."
  (condition-case err
      (with-temp-buffer
        (insert-file-contents (expand-file-name filepath))
        (let ((start (if start-line (max 1 start-line) 1))
              (end (if end-line (max start-line end-line) nil)))
          (goto-char (point-min))
          (if (and start-line end-line)
              (progn
                (forward-line (1- (max 0 (1- start))))
                (let ((from (point)))
                  (forward-line (- end start +1))
                  (buffer-substring-no-properties from (point))))
            (buffer-string))))
    (error (format "Error reading file %s: %s" filepath (error-message-string err)))))

(gptel-make-tool
 :function #'meain/gptel-tool--read-file-lines
 :name "read_file"
 :description "Read and return lines from a file. You may specify a start and end line, otherwise the whole file is returned."
 :args (list
        '(:name "filepath"
                :type string
                :description "Path to the file to read")
        '(:name "start_line"
                :type integer
                :optional t
                :description "Line number to start reading from (inclusive)")
        '(:name "end_line"
                :type integer
                :optional t
                :description "Line number to end reading at (inclusive)"))
 :category "filesystem"
 :include t)

;; Search for a Pattern in Files using ripgrep (rg)
(defun meain/gptel-tool--rg-search (pattern &optional directory)
  "Search for PATTERN using ripgrep (rg) in DIRECTORY (defaults to current dir).
Returns results as string.  If rg is not found, returns an error string."
  (let ((dir (or directory default-directory)))
    (if (executable-find "rg")
        (condition-case err
            (shell-command-to-string
             (format "rg --no-heading --line-number --color never %s %s"
                     (shell-quote-argument pattern)
                     (shell-quote-argument dir)))
          (error (format "Error running rg: %s" (error-message-string err))))
      "Error: rg (ripgrep) not found on system PATH.")))

(gptel-make-tool
 :function #'meain/gptel-tool--rg-search
 :name "search_rg"
 :description "Run ripgrep (rg) to search for a pattern in a directory. Returns matching lines with line numbers."
 :args (list
        '(:name "pattern"
                :type string
                :description "The regex pattern to search for")
        '(:name "directory"
                :type string
                :optional t
                :description "Directory to search in (defaults to current directory)"))
 :category "search"
 :include t)

;; Search for Files using fd CLI
(defun meain/gptel-tool--fd-search (pattern &optional directory)
  "Search for files matching PATTERN in DIRECTORY using fd.
Returns results as a newline-delimited string.
If fd is not installed or an error occurs, returns an error message."
  (let ((dir (or directory default-directory)))
    (if (executable-find "fd")
        (condition-case err
            (shell-command-to-string
             (format "fd --type f %s %s"
                     (shell-quote-argument pattern)
                     (shell-quote-argument dir)))
          (error (format "Error running fd: %s" (error-message-string err))))
      "Error: fd CLI not found in PATH.")))

(gptel-make-tool
 :function #'meain/gptel-tool--fd-search
 :name "search_files"
 :description "Search for files by pattern using the fd CLI. Returns the matching file paths as a newline-delimited string."
 :args (list
        '(:name "pattern"
          :type string
          :description "Filename pattern to search for (e.g. '*.el', 'main').")
        '(:name "directory"
          :type string
          :optional t
          :description "Directory to search in (optional; defaults to current directory)"))
 :category "filesystem")

;; List Files/Folders in a Directory
(defun meain/gptel-tool--list-directory (directory)
  "List all files and folders in DIRECTORY, one per line."
  (condition-case err
      (mapconcat #'identity
                 (directory-files (expand-file-name directory) nil nil t)
                 "\n")
    (error (format "Error listing directory %s: %s" directory (error-message-string err)))))

(gptel-make-tool
 :function #'meain/gptel-tool--list-directory
 :name "list_directory"
 :description "List all files and folders in a given directory, one per line."
 :args (list '(:name "directory"
                     :type string
                     :description "The path to the directory to list"))
 :category "filesystem"
 :include t)

;; Write a File
(defun meain/gptel-tool--write-file (filepath content)
  "Write CONTENT to FILEPATH, overwriting the file if it exists."
  (condition-case err
      (progn
        (with-temp-buffer
          (insert content)
          (write-region (point-min) (point-max) (expand-file-name filepath)))
        (format "Successfully wrote to file: %s" filepath))
    (error (format "Error writing to file %s: %s" filepath (error-message-string err)))))

(gptel-make-tool
 :function #'meain/gptel-tool--write-file
 :name "write_file"
 :description "Write content to a file at the specified path, overwriting if it exists."
 :args (list
        '(:name "filepath"
                :type string
                :description "Path to the file to write to")
        '(:name "content"
                :type string
                :description "Content to write to the file"))
 :category "filesystem"
 :include t
 :confirm t)

;; Open a Buffer (Create/Get)
(defun meain/gptel-tool--open-buffer (buffer-name)
  "Open (create or get) a buffer named BUFFER-NAME and return a status string."
  (condition-case err
      (with-current-buffer (get-buffer-create buffer-name)
        (switch-to-buffer (current-buffer))
        (format "Buffer opened: %s" buffer-name))
    (error (format "Error opening buffer %s: %s" buffer-name (error-message-string err)))))

(gptel-make-tool
 :function #'meain/gptel-tool--open-buffer
 :name "open_buffer"
 :description "Open (create if needed) an Emacs buffer with the specified name."
 :args (list '(:name "buffer_name"
                     :type string
                     :description "Name of the buffer to open or create"))
 :category "emacs"
 :include t)

;; List Open Buffers
(defun meain/gptel-tool--list-buffers ()
  "List the names of all open buffers, one per line."
  (mapconcat #'buffer-name (buffer-list) "\n"))

(gptel-make-tool
 :function #'meain/gptel-tool--list-buffers
 :name "list_buffers"
 :description "List the names of all open Emacs buffers, one per line."
 :args nil
 :category "emacs"
 :include t)

;; Read Buffer
(defun meain/gptel-tool--read-buffer (buffer-name)
  "Return the contents of BUFFER-NAME as a string.
Returns an error message if buffer is not live."
  (condition-case err
      (if (buffer-live-p (get-buffer buffer-name))
          (with-current-buffer buffer-name
            (buffer-substring-no-properties (point-min) (point-max)))
        (format "Error: buffer %s is not live." buffer-name))
    (error (format "Error: %s" (error-message-string err)))))

(gptel-make-tool
 :function #'meain/gptel-tool--read-buffer
 :name "read_buffer"
 :description "Return the contents of the named Emacs buffer. Fails gracefully if the buffer is not open."
 :args (list '(:name "buffer_name"
                     :type string
                     :description "Name of the buffer to read"))
 :category "emacs"
 :include t)

;; Insert Text into Buffer
(defun meain/gptel-tool--insert-into-buffer (buffer-name text)
  "Insert TEXT at the end of BUFFER-NAME.
Returns success or error message."
  (condition-case err
      (if (buffer-live-p (get-buffer buffer-name))
          (with-current-buffer buffer-name
            (save-excursion
              (goto-char (point-max))
              (insert text))
            (format "Text inserted into buffer %s." buffer-name))
        (format "Error: Buffer %s does not exist." buffer-name))
    (error (format "Error inserting into buffer %s: %s" buffer-name (error-message-string err)))))

(gptel-make-tool
 :function #'meain/gptel-tool--insert-into-buffer
 :name "insert_into_buffer"
 :description "Insert text at the end of a named buffer. Returns an error if the buffer does not exist."
 :args (list
        '(:name "buffer_name"
                :type string
                :description "Name of the buffer to insert into (must exist)")
        '(:name "text"
                :type string
                :description "Text to insert at end of buffer"))
 :category "emacs")

;; Open a File in Emacs
(defun meain/gptel-tool--open-file (filepath)
  "Open FILEPATH in Emacs.
Returns buffer name or error message."
  (condition-case err
      (let ((buf (find-file-noselect (expand-file-name filepath))))
        (format "Opened file: %s (buffer: %s)" filepath (buffer-name buf)))
    (error (format "Error opening file %s: %s" filepath (error-message-string err)))))

(gptel-make-tool
 :function #'meain/gptel-tool--open-file
 :name "open_file"
 :description "Open a file by path in Emacs (makes it visible in buffer list). Returns buffer name or error."
 :args (list
        '(:name "filepath"
          :type string
          :description "Path to the file to open in Emacs"))
 :category "emacs")

;; Run Arbitrary Shell Command (with confirmation)
(defun meain/gptel-tool--run-command (command &optional working-dir)
  "Run an arbitrary shell COMMAND in WORKING-DIR  and return its stdout as string."
  (let ((default-directory (if (and working-dir (not (string= working-dir "")))
                               (expand-file-name working-dir)
                             default-directory)))
    (condition-case err
        (shell-command-to-string command)
      (error (format "Error running command `%s`: %s" command (error-message-string err))))))

(gptel-make-tool
 :function #'meain/gptel-tool--run-command
 :name "run_command"
 :description "Run an arbitrary shell command and return the output. Confirmation is required before execution."
 :args (list
        '(:name "command"
                :type string
                :description "The shell command to run (e.g., ls -l /tmp)")
        '(:name "working_dir"
                :type string
                :optional t
                :description "If provided, the directory in which to run the command"))
 :category "general"
 :confirm t
 :include t)

;; Math Tool
(defun meain/gptel-tool--do-math (expression)
  "Safely evaluate simple math EXPRESSION, e.g., 2+2 or (sqrt 16)."
  (condition-case err (calc-eval expression)
    (error (format "Error in math expression `%s`: %s" expression (error-message-string err)))))

(gptel-make-tool
 :function #'meain/gptel-tool--do-math
 :name "do_math"
 :description "Evaluate a simple math expression (e.g., '2+2' or '(sqrt 16)'). Result is returned as string."
 :args (list '(:name "expression"
                     :type string
                     :description "Math expression: infix (e.g. 2+2), or Lisp (e.g. (sqrt 16))."))
 :category "general")

;; Get Current Date/Time
(defun meain/gptel-tool--get-date (format-str)
  "Return the current date/time formatted using FORMAT-STR.
Or a default format if not provided."
  (condition-case err
      (format-time-string (if (and format-str (not (string= format-str "")))
                              format-str
                            "%Y-%m-%d %H:%M:%S"))
    (error (format "Error formatting date with '%s': %s" format-str (error-message-string err)))))

(gptel-make-tool
 :function #'meain/gptel-tool--get-date
 :name "get_date"
 :description "Return the current date/time using a format string (optional). If omitted, uses \"%Y-%m-%d %H:%M:%S\"."
 :args (list
        '(:name "format_str"
                :type string
                :optional t
                :description "Format string for date/time (optional, defaults to \"%Y-%m-%d %H:%M:%S\")"))
 :category "system")

;; Show Recent Commits
(defun meain/gptel-tool--git-log (&optional file num)
  "Show last NUM commits for FILE (or whole repo if omitted)."
  (let ((cmd (if file
                 (format "git log -n %d --oneline -- %s" (or num 10) (shell-quote-argument file))
               (format "git log -n %d --oneline" (or num 10)))))
    (condition-case err
        (shell-command-to-string cmd)
      (error (format "Error running git log: %s" (error-message-string err))))))

(gptel-make-tool
 :function #'meain/gptel-tool--git-log
 :name "git_log"
 :description "Show recent git commits for a file, or the repo if no file is given."
 :args (list
        '(:name "file"
                :type string
                :optional t
                :description "File to show history for (optional)")
        '(:name "num"
                :type integer
                :optional t
                :description "Number of commits to show (default: 10)"))
 :category "git"
 :include t)

;; Show Commit Details by Hash
(defun meain/gptel-tool--git-show-commit (hash)
  "Show detailed info for commit HASH."
  (condition-case err
      (shell-command-to-string
       (format "git show %s" (shell-quote-argument hash)))
    (error (format "Error running git show: %s" (error-message-string err)))))

(gptel-make-tool
 :function #'meain/gptel-tool--git-show-commit
 :name "git_show_commit"
 :description "Show details (diff, message, files) for a given git commit hash."
 :args (list
        '(:name "hash"
                :type string
                :description "Commit hash to show details for"))
 :category "git"
 :include t)

(provide 'gptel-tools)
;;; gptel-tools.el ends here