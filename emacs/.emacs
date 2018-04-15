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
    ("36282815a2eaab9ba67d7653cf23b1a4e230e4907c7f110eebf3cdf1445d8370" "83db918b06f0b1df1153f21c0d47250556c7ffb5b5e6906d21749f41737babb7" "a24c5b3c12d147da6cef80938dca1223b7c7f70f2f382b26308eba014dc4833a" "e11569fd7e31321a33358ee4b232c2d3cf05caccd90f896e1df6cab228191109" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "7366916327c60fdf17b53b4ac7f565866c38e1b4a27345fe7facbf16b7a4e9e8" "7b4d9b8a6ada8e24ac9eecd057093b0572d7008dbd912328231d0cada776065a" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "39dd7106e6387e0c45dfce8ed44351078f6acd29a345d8b22e7b8e54ac25bac4" "c3d4af771cbe0501d5a865656802788a9a0ff9cf10a7df704ec8b8ef69017c68" "d6922c974e8a78378eacb01414183ce32bc8dbf2de78aabcc6ad8172547cb074" "ed0b4fc082715fc1d6a547650752cd8ec76c400ef72eb159543db1770a27caa7" default)))
 '(package-selected-packages
   (quote
    (vimrc-mode base16-theme arjen-grey-theme material-theme zenburn-theme color-theme-sanityinc-tomorrow monochrome-theme cyberpunk-theme gruvbox-theme jedi focus wakatime-mode which-key web-mode waher-theme use-package try spaceline smooth-scrolling smartparens shell-pop sane-term restclient rainbow-delimiters quickrun projectile persistent-scratch osx-clipboard org-bullets olivetti neotree monokai-theme magit hiwin helm flycheck fiplr expand-region evil-surround evil-search-highlight-persist evil-leader evil-commentary elpy drag-stuff diff-hl dashboard autopair auto-complete all-the-icons-dired ag)))
 '(wakatime-cli-path "/usr/local/bin/wakatime")
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
