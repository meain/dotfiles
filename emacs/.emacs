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


;; Emacs stuff
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cursor-type (quote bar))
 '(custom-safe-themes
   (quote
    ("235dc2dd925f492667232ead701c450d5c6fce978d5676e54ef9ca6dd37f6ceb" "d5f17ae86464ef63c46ed4cb322703d91e8ed5e718bf5a7beb69dd63352b26b2" "43c1a8090ed19ab3c0b1490ce412f78f157d69a29828aa977dae941b994b4147" "a632c5ce9bd5bcdbb7e22bf278d802711074413fd5f681f39f21d340064ff292" "51e228ffd6c4fff9b5168b31d5927c27734e82ec61f414970fc6bcce23bc140d" "715fdcd387af7e963abca6765bd7c2b37e76154e65401cd8d86104f22dd88404" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" default)))
 '(package-selected-packages
   (quote
    (jedi focus wakatime-mode which-key web-mode waher-theme use-package try spaceline smooth-scrolling smartparens shell-pop sane-term restclient rainbow-delimiters quickrun projectile persistent-scratch osx-clipboard org-bullets olivetti neotree monokai-theme magit hiwin helm flycheck fiplr expand-region evil-surround evil-search-highlight-persist evil-leader evil-commentary elpy drag-stuff diff-hl dashboard autopair auto-complete all-the-icons-dired ag)))
 '(wakatime-api-key credential-store/wakatime-api-key)
 '(wakatime-cli-path
   "/Library/Frameworks/Python.framework/Versions/2.7/bin/wakatime")
 '(wakatime-python-bin nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fixed-pitch ((t (:family "Monaco"))))
 '(markdown-header-face ((t (:inherit font-lock-function-name-face :weight bold :foundry "variable" :family "Monaco"))))
 '(markdown-header-face-1 ((t (:inherit markdown-header-face :height 1.8))))
 '(markdown-header-face-2 ((t (:inherit markdown-header-face :height 1.4))))
 '(markdown-header-face-3 ((t (:inherit markdown-header-face :height 1.2))))
 '(org-level-1 ((t (:inherit outline-1 :height 1.7))))
 '(org-level-2 ((t (:inherit outline-2 :height 1.5))))
 '(org-level-3 ((t (:inherit outline-3 :height 1.3))))
 '(org-level-4 ((t (:inherit outline-4 :height 1.1))))
 '(org-level-5 ((t (:inherit outline-5 :height 1.0)))))
