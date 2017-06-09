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
(org-babel-load-file "~/.elconf.org")

(setq gc-cons-threshold 800000)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (monokai-theme which-key web-mode vimrc-mode use-package try spaceline-all-the-icons smooth-scrolling sane-term restclient rainbow-mode rainbow-delimiters projectile persistent-scratch osx-clipboard org-bullets neotree mode-icons minimap markdown-mode magit labburn-theme jazz-theme hemisu-theme helm gruvbox-theme go-mode git-gutter flycheck fiplr expand-region evil-surround evil-search-highlight-persist evil-leader evil-commentary elpy drag-stuff diff-hl dashboard cyberpunk-theme autopair auto-complete all-the-icons-dired ag))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
