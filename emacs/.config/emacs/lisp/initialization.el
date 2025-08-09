;;; initialization.el --- Initialization steps -*- lexical-binding: t; -*-

;;; Commentary:
;; Setup the package manager and start out things

;;; Code:
(defvar elpaca-installer-version 0.11)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable :ensure use-package keyword.
  (elpaca-use-package-mode))

;; Block until current queue processed.
(elpaca-wait)

;; Use package config
(setq use-package-verbose t)
(setq use-package-enable-imenu-support t)
(setq use-package-always-demand (getenv "LOAD_FULL_EMACS"))
(setq use-package-compute-statistics t) ;; Run `use-package-statistics' to get load timings

;; Benchmark emacs startup (enable when necessary)
(use-package benchmark-init
  :ensure t
  :disabled t
  :config
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

;; Get proper PATH (not used as we are launching from shell)
;; TODO: Convert to async: https://br0g.0brg.net/2024/emacs-async-exec-path-from-shell.html
(use-package exec-path-from-shell
  :ensure t
  :disabled t
  :config
  ;; https://github.com/purcell/exec-path-from-shell#making-exec-path-from-shell-faster
  ;; (setq exec-path-from-shell-arguments '("-l")) ;; removing -i
  (exec-path-from-shell-initialize))

;; Print emacs startup time
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds" (float-time (time-subtract after-init-time before-init-time)))
                     gcs-done)

            ;; Start server once we have emacs running
            (require 'server)
            (unless (server-running-p)
              (progn
                (server-start)
                (start-process-shell-command "server-start-notify"
                                             "*server-start-notify*"
                                             "notify --pri 'Emacs server started'")))))

(provide 'initialization)
;;; initialization.el ends here
