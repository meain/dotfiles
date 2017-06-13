;;; .emacs --- my personal emacs configuration file

(setq gc-cons-threshold 400000000)

;;; Begin initialization
;; Turn off mouse interface early in startup to avoid momentary display
(when window-system
  (tool-bar-mode -1)
  ;; (tooltip-mode -1)
  (scroll-bar-mode -1))
(menu-bar-mode -1)

;; Don't show start message
(setq inhibit-startup-message t)

;; Load credentials
(load "~/.credentials.el")

;;; Set up package
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

;; Set up use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; From use-package README
(eval-when-compile
  (require 'use-package))

;; Other dependacies
(require 'diminish)
(require 'bind-key)

;; (setq use-package-verbose t)
;; (server-start)

;;; Load the config
(setq vc-follow-symlinks nil)
(org-babel-load-file "~/.elconf.org")
(setq vc-follow-symlinks "ask")

(setq gc-cons-threshold 800000)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (sanityinc-tomorrow-bright)))
 '(custom-safe-themes
   (quote
    ("1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" default)))
 '(package-selected-packages
   (quote
    (focus wakatime-mode which-key web-mode waher-theme use-package try spaceline smooth-scrolling smartparens shell-pop sane-term restclient rainbow-delimiters quickrun projectile persistent-scratch osx-clipboard org-bullets olivetti neotree monokai-theme markdown-mode magit hiwin helm flycheck fiplr expand-region evil-surround evil-search-highlight-persist evil-leader evil-commentary elpy drag-stuff diff-hl dashboard autopair auto-complete all-the-icons-dired ag)))
 '(wakatime-api-key credential-store/wakatime-api-key)
 '(wakatime-cli-path
   "/Library/Frameworks/Python.framework/Versions/2.7/bin/wakatime")
 '(wakatime-python-bin nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
