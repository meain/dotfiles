;;; early-init -- meain's Emacs config -*- lexical-binding: t -*-

;;; Commentary:
;; Well, this is a vimmer's Emacs config.  Nothing fancy though.

;;; Code:

;; Native compilation stuff
(setq comp-speed 2)
(setq package-native-compile t)
(setq native-comp-async-report-warnings-errors nil)
(setq native-comp-deferred-compilation-deny-list nil)

;; Disable package.el as we are using straight
(setq package-enable-at-startup nil)

;; Increase max specpdl size
(setq max-specpdl-size 32123)

;; MacOS specific keybinding changes
(setq mac-option-modifier 'meta)
(setq mac-command-modifier 'super)

;; Update some visual settings
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)  ;; causes focus issue if disabled in railwaycat/homebrew-emacsmacport
(setq inhibit-startup-screen t)
;; (set-frame-parameter nil 'undecorated t)

;; Font setup
;; iA Writer Mono S 14 | Inconsolata 16 | Fantasque Sans Mono 10 | JetBrains Mono 14
;; Fira Code 15 | Noto Sans Mono 15 | DankMono Nerd Font 15 | Roboto Mono 14 | Iosevka 15
;; Conta Mono 14 | IBM Plex Mono 13 | MonoLisa 13 | Kraft Mono 14 | SpaceMono Nerd Font Mono 14
;; Aglet Mono 15 | VictorMono Nerd Font 14 | NanumGothicCoding 15 | BPMono 14 | Fairfax HD 16
;; Liga Camingocode 15 | Oxygen Mono 14 | M+ 1m 14 | MonospaceTypewriter 14 | Luculent 14
;; Iosevka Slab 14 | Edlo 13 | Bront 15 | Whois 15 | Ligconsolata 16 | Operator Mono 14
;; PragmataPro 14 | agave 15 | Unifont 15 | Apercu Mono 14 | Binchotan_Sharp 15
;; CMU Typewriter Text 16 | Input 14 | SF Mono 14 | CQ Mono 11 | Izayoi Monospaced 10
;; Font: (set-frame-font  "Whois 14")
;; Line spacing: (setq-default line-spacing nil)

(defvar meain/font-list '(;; name size line-spacing weight
                          ("Adelle Mono" . '(10 .1 'regular))
                          ("Aglet Mono" . '(9 .2 'regular))
                          ("Anka/Coder" . '(10 nil 'regular))
                          ("BPMono" . '(10 .1 'regular))
                          ("Cartograph CF" . '(9 nil 'regular))
                          ("Consolas" . '(10 nil 'regular))
                          ("Conta" . '(10 nil 'regular))
                          ("CQ Mono" . '(11 .1 'regular))
                          ("Dank Mono" . '(10 nil 'regular))
                          ("DankMono Nerd Font" . '(10 nil 'regular))
                          ("DM Mono" . '(11 nil 'regular))
                          ("Envy Code R" . '(11 nil 'regular))
                          ("FairfaxHD" . '(13 nil 'regular))
                          ("Fantasque Sans Mono" . '(10 .2 'regular))
                          ("Fira Code" . '(10 .1 'thin))
                          ("fixed" . '(11 nil 'regular))
                          ("Hermit" . '(9 nil 'regular))
                          ("Inconsolata" . '(11 nil 'regular))
                          ("Iosevka" . '(11 nil 'regular))
                          ("Iosevka Comfy" . '(12 .2 'regular))
                          ("Iosevka Light" . '(12 .1 'regular))
                          ("Iosevka Thin" . '(11 nil 'regular))
                          ("Izayoi Monospaced" . '(10 .1 'regular))
                          ("JetBrains Mono" . '(10 nil 'regular))
                          ("Lab Mono" . '(9 .2 'regular))
                          ("League Mono" . '(10 .2 'light))
                          ("Lekton" . '(11 .1 'regular))
                          ("Liberation Mono" . '(10 .1 'regular))
                          ("Liga CamingoCode" . '(10 nil 'regular))
                          ("M+ 1m" . '(10 nil 'regular))
                          ("Mirage" . '(10 .1 'regular))
                          ("Monaco for Powerline" . '(9 .1 'regular))
                          ("Monofur" . '(12 nil 'regular))
                          ("Monoid" . '(8 .1 'light))
                          ("Monoid HalfLoose" . '(7 .1 'light))
                          ("Monoisome" . '(9 .3 'light))
                          ("MonoLisa" . '(10 .2 'regular))
                          ("NK57 Monospace" . '(9 .2 'regular))
                          ("Operator Mono" . '(10 .1 'light))
                          ("Panic Sans" . '(9 nil 'regular))
                          ("PT Mono" . '(10 nil 'regular))
                          ("Red Hat Mono" . '(9 nil 'regular))
                          ("Roboto Mono" . '(10 nil 'light))
                          ("Sarasa Mono SC Nerd". '(11 nil 'regular))
                          ("SpaceMono Nerd Font Mono" . '(11 nil 'regular))
                          ("Sudo" . '(14 nil 'regular))
                          ("TT2020Base" . '(13 .1 'regular))
                          ("TT2020 Style E" . '(13 .2 'regular))
                          ("Victor Mono" . '(10 .1 'regular))
                          ("Whois" . '(12 .1 'regular))
                        ))
(defvar meain/font-name "TT2020 Style E")

(defun meain/get-font-prop (font-name prop)
  "Get PROP (property) from FONT-NAME."
  (interactive)
  (let ((font-properties (assoc font-name meain/font-list)))
    (if font-properties
        (pcase prop
          ('family (format "%s %s" font-name (car (nth 2 font-properties))))
          ('weight (cadr (nth 2 (nth 2 font-properties))))
          ('line-spacing (nth 1 (nth 2 font-properties))))
      (pcase prop
        ('family font-name)
        ('weight 'regular)
        ('line-spacing nil)))))
(defvar meain/font-family-default (meain/get-font-prop meain/font-name 'family) "Default font family for everything in Emacs.")
(defvar meain/font-weight-default (meain/get-font-prop meain/font-name 'weight) "Default font weight for everything in Emacs.")
(setq-default line-spacing (meain/get-font-prop meain/font-name 'line-spacing))
(add-to-list 'default-frame-alist `(font . ,meain/font-family-default))
(custom-set-faces
 '(fixed-pitch ((t (:inherit default))))
 '(variable-pitch ((t (:inherit default)))))


;; Wider frames by default
(add-to-list 'default-frame-alist `(width . 150))

;; Window decoraations
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
;; (setq default-frame-alist '((undecorated . t)))
;; (add-to-list 'default-frame-alist '(drag-internal-border . 10))
;; (add-to-list 'default-frame-alist '(internal-border-width . 10))
;; (add-to-list 'default-frame-alist '(ns-appearance . dark))

;; Bump up gc threshold until emacs startup
(setq gc-cons-threshold most-positive-fixnum gc-cons-percentage 0.6)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold 16777216 gc-cons-percentage 0.1)))

;;; early-init.el ends here
