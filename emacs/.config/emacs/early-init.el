;; Setup LIBRARY env variable (needed for gccemacs)
(setenv "LIBRARY_PATH" "/Applications/Emacs.app/Contents/MacOS/lib/gcc/10:/Applications/Emacs.app/Contents/MacOS/lib/gcc/10/gcc/x86_64-apple-darwin17/10.2.0")

;; Update some visual settings
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode 1)  ;; causes focus issue if disabled in railwaycat/homebrew-emacsmacport
(setq inhibit-startup-screen t)
;; (set-frame-parameter nil 'undecorated t)

;; Font setup
;; iA Writer Mono S 14 | Inconsolata 16 | Fantasque Sans Mono 16 | Fira Code 15 | Noto Sans Mono 15 | DankMono Nerd Font 15
;; Font (set-frame-font  "DankMono Nerd Font 15")
(setq font-family-default "DankMono Nerd Font 15")
(setq line-spacing nil) ;; .1 for Inconsolata
(add-to-list 'default-frame-alist
             `(font . ,font-family-default))

;; Window decoraations
(add-to-list 'default-frame-alist
             '(ns-transparent-titlebar . t))
;; (setq default-frame-alist '((undecorated . t)))
;; (add-to-list 'default-frame-alist '(drag-internal-border . 10))
;; (add-to-list 'default-frame-alist '(internal-border-width . 10))
;; (add-to-list 'default-frame-alist '(ns-appearance . dark))

;; Bump up gc threshold until emacs startup
(setq gc-cons-threshold most-positive-fixnum ; 2^61 bytes
      gc-cons-percentage 0.6)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold 16777216 gc-cons-percentage
                  0.1)))
