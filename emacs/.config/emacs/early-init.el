;;; early-init -- meain's Emacs config -*- lexical-binding: t -*-

;;; Commentary:
;; Well, this is a vimmer's Emacs config.  Nothing fancy though.

;;; Code:

;; Native compilation stuff
(setq comp-speed 2)
(setq package-native-compile t)
(setq native-comp-async-report-warnings-errors nil)
(setq native-comp-deferred-compilation-deny-list nil)

;; Disable package.el as we are using elpaca
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
(add-to-list 'default-frame-alist '(undecorated-round . t))

;; Font setup
;; iA Writer Mono S 14 | Inconsolata 16 | Fantasque Sans Mono 10 | JetBrains Mono 14
;; Fira Code 15 | Noto Sans Mono 15 | DankMono Nerd Font 15 | Roboto Mono 14 | Iosevka 15
;; Conta Mono 14 | IBM Plex Mono 13 | MonoLisa 13 | Kraft Mono 14 | SpaceMono Nerd Font Mono 14
;; Aglet Mono 15 | VictorMono Nerd Font 14 | NanumGothicCoding 15 | BPMono 14 | Fairfax HD 16
;; Liga Camingocode 15 | Oxygen Mono 14 | M+ 1m 14 | MonospaceTypewriter 14 | Luculent 14
;; Iosevka Slab 14 | Edlo 13 | Bront 15 | Whois 15 | Ligconsolata 16 | Operator Mono 14
;; PragmataPro 14 | agave 15 | Unifont 15 | Apercu Mono 14 | Binchotan_Sharp 15
;; CMU Typewriter Text 16 | Input 14 | SF Mono 14 | CQ Mono 11 | Izayoi Monospaced 10
;; Font: (set-frame-font  "Fantasque Sans Mono 11" nil t)
;; Line spacing: (setq-default line-spacing nil)
;; Fine grained size control (set-face-attribute 'default nil :height 110)

(defvar meain/font-list '(;; name height line-spacing weight
                          ("0xProto" . '(140 .3 'regular))
                          ("Adelle Mono" . '(160 .1 'regular))
                          ("Agave" . '(160 .1 'regular))
                          ("Aglet Mono" . '(160 .2 'regular))
                          ("American Typewriter" . '(160 .2 'regular))
                          ("Anka/Coder" . '(160 .1 'regular))
                          ("Annotation Mono" . '(160 .2 'light))
                          ("Anonymous Pro" . '(160 .2 'regular))
                          ("Apercu Mono Pro" . '(150 .3 'light))
                          ("Apercu Mono" . '(150 .3 'light))
                          ("Aporetic Sans Mono" . '(160 .2 'regular))
                          ("Aporetic Serif Mono" . '(160 .1 'regular))
                          ("Berkeley Mono" . '(160 .2 'light))
                          ("BPMono" . '(160 .1 'regular))
                          ("Cartograph CF" . '(160 nil 'regular))
                          ("Cascadia Code" . '(160 .2 'light))
                          ("Cascadia Mono" . '(160 nil 'regular))
                          ("CMU Typewriter Text" . '(160 nil 'regular))
                          ("Codename Coder Free 4F" . '(160 nil 'regular))
                          ("Comic Mono" . '(160 .1 'regular))
                          ("CommitMono" . '(160 .1 'regular))
                          ("Consola Mono" . '(160 nil 'regular))
                          ("Consolas" . '(160 nil 'regular))
                          ("Conta" . '(160 nil 'regular))
                          ("Courier New" . '(160 .2 'regular))
                          ("CozetteVector" . '(160 nil 'regular))
                          ("CQ Mono" . '(160 .1 'regular))
                          ("DaddyTimeMono" . '(160 nil 'regular))
                          ("Dank Mono" . '(160 .1 'regular))
                          ("DankMono Nerd Font" . '(160 nil 'regular))
                          ("DejaVu Sans Mono" . '(160 nil 'regular))
                          ("Departure Mono" . '(160 .2 'regular))
                          ("DinaRemaster" . '(160 .2 'regular))
                          ("DM Mono" . '(160 .1 'light))
                          ("Edlo" . '(160 nil 'regular))
                          ("Ellograph CF Thin" . '(160 .1 'regular))
                          ("Ellograph CF" . '(160 .1 'regular))
                          ("Envy Code R" . '(160 nil 'regular))
                          ("FairfaxHD" . '(160 nil 'regular))
                          ("Fantasque Sans Mono" . '(160 .1 'regular))
                          ("Fira Code" . '(160 .2 'thin))
                          ("fixed" . '(160 nil 'regular))
                          ("Geist Mono Light" . '(160 .1 'regular))
                          ("Geist Mono" . '(160 .1 'light))
                          ("Gintronic" . '(160 nil 'regular))
                          ("Go Mono" . '(160 .2 'regular))
                          ("GT Pressura Mono" . '(160 .1 'light))
                          ("Hack" . '(160 nil 'light))
                          ("Hermit" . '(160 nil 'regular))
                          ("HyperFont" . '(160 .2 'regular))
                          ("iA Writer Duo S" . '(160 nil 'regular))
                          ("iA Writer Mono S" . '(160 nil 'regular))
                          ("IBM Plex Mono" . '(160 nil 'regular))
                          ("Inconsolata" . '(160 nil 'regular))
                          ("Indicate Mono" . '(160 .2 'regular))
                          ("Input Mono Compressed" . '(150 .2 'light))
                          ("Input Mono Condensed" . '(140 .2 'light))
                          ("Input Mono" . '(160 .2 'regular))
                          ("Input" . '(160 nil 'regular)) ; custom version
                          ("Intel One Mono" . '(160 nil 'thin))
                          ("Iosevka Comfy Motion" . '(160 .1 'light))
                          ("Iosevka Comfy Wide Motion" . '(160 nil 'regular))
                          ("Iosevka Comfy" . '(160 .2 'regular))
                          ("Iosevka Light" . '(160 .1 'regular))
                          ("Iosevka Slab" . '(160 nil 'regular))
                          ("Iosevka Thin" . '(160 nil 'regular))
                          ("Iosevka" . '(160 .1 'light))
                          ("Izayoi Monospaced" . '(160 .2 'regular))
                          ("JetBrains Mono Light" . '(160 nil 'light))
                          ("JetBrains Mono" . '(160 .1 'regular))
                          ("JuliaMono" . '(160 .2 'light))
                          ("Kraft Mono" . '(160 .2 'regular))
                          ("Lab Mono" . '(160 .1 'regular))
                          ("Latin Modern Mono Light Cond" . '(160 nil 'regular))
                          ("Latin Modern Mono" . '(160 nil 'regular))
                          ("League Mono" . '(160 .2 'light))
                          ("Lekton" . '(160 .2 'regular))
                          ("Letter Gothic Std" . '(160 nil 'bold))
                          ("Liberation Mono" . '(160 .1 'regular))
                          ("Liga CamingoCode" . '(160 nil 'regular))
                          ("Lotion" . '(160 nil 'regular))
                          ("Luculent" . '(160 .2 'regular))
                          ("M+ 1m" . '(160 nil 'regular))
                          ("Maple Mono" . '(160 nil 'light))
                          ("Martian Mono Condensed" . '(160 nil 'regular))
                          ("Martian Mono ExtraLight" . '(160 nil 'regular))
                          ("Martian Mono Light" . '(160 nil 'regular))
                          ("Martian Mono" . '(160 nil 'regular))
                          ("MD IO" . '(160 .2 'thin))
                          ("Menlo" . '(160 .1 'regular))
                          ("Monaco for Powerline" . '(160 .1 'regular))
                          ("Monaco" . '(160 .1 'regular))
                          ("Monaspace Argon" . '(160 .2 'regular))
                          ("Monaspace Krypton" . '(160 .2 'regular))
                          ("Monaspace Neon Light" . '(160 .2 'regular))
                          ("Monaspace Neon Var" . '(160 .1 'regular))
                          ("Monaspace Neon" . '(160 .2 'regular))
                          ("Monaspace Radon" . '(160 .2 'regular))
                          ("Monaspace Xenon" . '(160 .2 'regular))
                          ("Monoflow" . '(160 nil 'regular))
                          ("monofur" . '(160 .2 'regular))
                          ("Monoid HalfLoose" . '(160 .1 'light))
                          ("Monoid" . '(160 .2 'light))
                          ("Monoisome HalfLoose" . '(160 .2 'light))
                          ("Monoisome" . '(160 .3 'light))
                          ("MonoLisa" . '(160 .2 'regular))
                          ("mononoki" . '(160 .1 'regular))
                          ("monoOne" . '(160 nil 'regular))
                          ("MonospaceTypewriter" . '(160 .2 'regular))
                          ("NanumGothicCoding" . '(160 .3 'regular))
                          ("NK57 Monospace" . '(160 .2 'regular))
                          ("Noto Sans Mono ExtraCondensed" . '(160 nil 'regular))
                          ("Noto Sans Mono" . '(160 .1 'regular))
                          ("OCRA" . '(160 nil 'regular))
                          ("Old Timey Code" . '(160 .1 'regular))
                          ("Old Timey Mono" . '(160 .1 'regular))
                          ("Operator Mono Light" . '(160 .1 'regular))
                          ("Operator Mono" . '(160 .1 'thin))
                          ("Opus One" . '(160 nil 'regular))
                          ("Overpass Mono Light" . '(160 nil 'regular))
                          ("Overpass Mono" . '(160 nil 'regular))
                          ("Oxygen Mono" . '(160 .2 'regular))
                          ("Panic Sans" . '(160 nil 'regular))
                          ("PragmataPro" . '(160 .1 'regular))
                          ("ProFont for Powerline" . '(160 .1 'regular))
                          ("PT Mono" . '(160 .1 'regular))
                          ("Quinze" . '(160 nil 'regular))
                          ("Rec Mono Casual" . '(160 nil 'regular))
                          ("Rec Mono Linear" . '(160 nil 'regular))
                          ("Rec Mono Semicasual" . '(160 nil 'regular))
                          ("Recursive Monospace Casual" . '(160 nil 'regular))
                          ("Recursive Monospace" . '(160 nil 'regular))
                          ("Red Hat Mono" . '(160 nil 'regular))
                          ("Reno Mono" . '(160 nil 'regular))
                          ("Roboto Mono" . '(160 .2 'regular))
                          ("Sarasa Mono SC Nerd". '(160 nil 'regular))
                          ("saxMono" . '(160 nil 'regular))
                          ("SF Mono" . '(160 .1 'light))
                          ("Share Tech Mono" . '(160 nil 'regular))
                          ("Sometype Mono" . '(160 nil 'regular))
                          ("SpaceMono Nerd Font Mono" . '(160 nil 'regular))
                          ("Sudo" . '(160 .2 'thin))
                          ("SV Basic Manual" . '(160 .3 'regular))
                          ("Syne Mono" . '(160 nil 'regular))
                          ("Syne Tactile" . '(160 .1 'regular))
                          ("Terminess Nerd Font Mono" . '(160 .1 'regular))
                          ("Terminess Nerd Font" . '(160 nil 'regular))
                          ("TerminessTTF Nerd Font Mono" . '(160 nil 'regular))
                          ("TerminessTTF Nerd Font" . '(160 nil 'regular))
                          ("Terminus (TTF)" . '(160 .2 'regular))
                          ("Times New Roman" . '(160 .1 'regular))
                          ("Tlwg Typewriter" . '(160 nil 'regular))
                          ("TT2020 Style E" . '(160 .2 'regular))
                          ("TT2020" . '(160 .3 'regular))
                          ("TT2020Base" . '(160 .1 'regular))
                          ("Typori" . '(160 .2 'regular)) ;; not monospace
                          ("Ubuntu Mono" . '(160 .1 'regular))
                          ("Unifont" . '(160 .2 'regular))
                          ("Uroob" . '(160 nil 'regular))
                          ("Victor Mono" . '(160 nil 'regular))
                          ("Whois" . '(160 .2 'regular))
                          ("Wire Type Mono" . '(160 nil 'regular))
                          ("Xanh Mono" . '(160 .1 'regular))
                          ("Zed Mono" . '(160 .1 'light))
                          ))
(defvar meain/font-name "Input Mono Compressed")

(defvar meain/font-height-default 160 "Default font height for everything in Emacs.")
(defun meain/get-font-prop (font-name prop)
  "Get PROP (property) from FONT-NAME."
  (interactive)
  (let ((font-properties (assoc font-name meain/font-list)))
    (if font-properties
        (pcase prop
          ('family font-name)
          ('height (car (nth 2 font-properties)))
          ('weight (cadr (nth 2 (nth 2 font-properties))))
          ('line-spacing (nth 1 (nth 2 font-properties))))
      (pcase prop
        ('family font-name)
        ('height meain/font-height-default)
        ('weight 'regular)
        ('line-spacing nil)))))

(setq-default line-spacing (meain/get-font-prop meain/font-name 'line-spacing))
(add-to-list 'default-frame-alist `(font . ,(meain/get-font-prop meain/font-name 'family)))
(set-face-attribute 'default nil
                    :font (meain/get-font-prop meain/font-name 'family)
                    :height (meain/get-font-prop meain/font-name 'height)
                    :weight (meain/get-font-prop meain/font-name 'weight))
(set-face-attribute 'fixed-pitch nil :inherit 'default)
(set-face-attribute 'variable-pitch nil :inherit 'default)

;; Wider frames by default
(add-to-list 'default-frame-alist `(width . 150))

;; Window decoraations
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
;; (setq default-frame-alist '((undecorated . t)))
;; (add-to-list 'default-frame-alist '(drag-internal-border . 10))
;; (add-to-list 'default-frame-alist '(internal-border-width . 10))
;; (add-to-list 'default-frame-alist '(ns-appearance . dark))

;; Perf: fontify when idle, speed ups scroll
;; https://www.reddit.com/r/emacs/comments/14c4l8j/comment/joku4bh/
;; https://codeberg.org/ideasman42/emacs-jit-lock-stealth-progress
(setq jit-lock-stealth-time 1.25)
(setq jit-lock-stealth-nice 0.5) ;; Seconds between font locking.
(setq jit-lock-chunk-size 4096)

;; Bump up gc threshold until emacs startup
(setq gc-cons-threshold most-positive-fixnum gc-cons-percentage 0.6)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 1024 1024) gc-cons-percentage 0.1)
            (run-with-idle-timer 2 t (lambda () (garbage-collect)))))

;;; early-init.el ends here
