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

(defvar meain/font-list '(;; name size line-spacing weight
                          ("0xProto" . '(14 .2 'regular))
                          ("Adelle Mono" . '(11 .1 'regular))
                          ("Agave" . '(17 .1 'regular))
                          ("Aglet Mono" . '(11 .2 'regular))
                          ("American Typewriter" . '(16 .2 'regular))
                          ("Anka/Coder" . '(14 .1 'regular))
                          ("Anonymous Pro" . '(16 .2 'regular))
                          ("Apercu Mono Pro" . '(16 .2 'regular))
                          ("Apercu Mono" . '(11 nil 'regular))
                          ("Aporetic Serif Mono" . '(15 .1 'regular))
                          ("BPMono" . '(10 .1 'regular))
                          ("Berkeley Mono" . '(15 .2 'light))
                          ("CMU Typewriter Text" . '(13 nil 'regular))
                          ("CQ Mono" . '(13 .1 'regular))
                          ("Cartograph CF" . '(11 nil 'regular))
                          ("Cascadia Code Light" . '(11 nil 'regular))
                          ("Cascadia Code" . '(13 nil 'regular))
                          ("Cascadia Mono Light" . '(13 nil 'regular))
                          ("Cascadia Mono" . '(13 nil 'regular))
                          ("Codename Coder Free 4F" . '(13 nil 'regular))
                          ("Comic Mono" . '(15 .1 'regular))
                          ("CommitMono" . '(11 .1 'regular))
                          ("Consola Mono" . '(16 nil 'regular))
                          ("Consolas" . '(12 nil 'regular))
                          ("Conta" . '(11 nil 'regular))
                          ("Courier New" . '(17 .2 'regular))
                          ("CozetteVector" . '(12 nil 'regular))
                          ("DM Mono" . '(15 .1 'light))
                          ("DaddyTimeMono" . '(16 nil 'regular))
                          ("Dank Mono" . '(16 .1 'regular))
                          ("DankMono Nerd Font" . '(13 nil 'regular))
                          ("DejaVu Sans Mono" . '(11 nil 'regular))
                          ("Departure Mono" . '(14 .2 'regular))
                          ("Edlo" . '(11 nil 'regular))
                          ("Ellograph CF Thin" . '(11 .1 'regular))
                          ("Ellograph CF" . '(11 .1 'regular))
                          ("Envy Code R" . '(11 nil 'regular))
                          ("FairfaxHD" . '(13 nil 'regular))
                          ("Fantasque Sans Mono" . '(18 .1 'regular))
                          ("Fira Code" . '(14 .2 'thin))
                          ("GT Pressura Mono" . '(16 .1 'light))
                          ("Geist Mono Light" . '(14 .1 'regular))
                          ("Geist Mono" . '(15 .1 'light))
                          ("Gintronic" . '(11 nil 'regular))
                          ("Go Mono" . '(15 .2 'regular))
                          ("Hack" . '(13 nil 'light))
                          ("Hermit" . '(10 nil 'regular))
                          ("HyperFont" . '(16 .2 'regular))
                          ("IBM Plex Mono" . '(13 nil 'regular))
                          ("Inconsolata" . '(16 nil 'regular))
                          ("Indicate Mono" . '(16 .2 'regular))
                          ("Input Mono Compressed" . '(15 .2 'regular))
                          ("Input Mono" . '(14 .2 'regular))
                          ("Input" . '(12 nil 'regular)) ; custom version
                          ("IntelOne Mono" . '(11 nil 'regular))
                          ("Iosevka Comfy Motion" . '(16 .1 'light))
                          ("Iosevka Comfy Wide Motion" . '(11 nil 'regular))
                          ("Iosevka Comfy" . '(14 .2 'regular))
                          ("Iosevka Light" . '(12 .1 'regular))
                          ("Iosevka Slab" . '(12 nil 'regular))
                          ("Iosevka Thin" . '(11 nil 'regular))
                          ("Iosevka" . '(16 .1 'light))
                          ("Izayoi Monospaced" . '(12 .2 'regular))
                          ("JetBrains Mono Light" . '(12 nil 'light))
                          ("JetBrains Mono" . '(14 .1 'regular))
                          ("JuliaMono" . '(14 .2 'light))
                          ("Kraft Mono" . '(11 .2 'regular))
                          ("Lab Mono" . '(11 .1 'regular))
                          ("Latin Modern Mono Light Cond" . '(19 nil 'regular))
                          ("Latin Modern Mono" . '(17 nil 'regular))
                          ("League Mono" . '(10 .2 'light))
                          ("Lekton" . '(17 .2 'regular))
                          ("Letter Gothic Std" . '(11 nil 'bold))
                          ("Liberation Mono" . '(10 .1 'regular))
                          ("Liga CamingoCode" . '(11 nil 'regular))
                          ("Lotion" . '(17 nil 'regular))
                          ("Luculent" . '(11 .2 'regular))
                          ("M+ 1m" . '(11 nil 'regular))
                          ("MD IO" . '(15 .1 'light))
                          ("Maple Mono" . '(15 nil 'regular))
                          ("Martian Mono Condensed" . '(13 nil 'regular))
                          ("Martian Mono ExtraLight" . '(13 nil 'regular))
                          ("Martian Mono Light" . '(13 nil 'regular))
                          ("Martian Mono" . '(13 nil 'regular))
                          ("Monaco for Powerline" . '(10 .1 'regular))
                          ("Monaco" . '(14 .1 'regular))
                          ("Monaspace Argon" . '(15 .2 'regular))
                          ("Monaspace Krypton" . '(15 .2 'regular))
                          ("Monaspace Neon Light" . '(15 .2 'regular))
                          ("Monaspace Neon Var" . '(15 .1 'regular))
                          ("Monaspace Neon" . '(15 .2 'regular))
                          ("Monaspace Radon" . '(15 .2 'regular))
                          ("Monaspace Xenon" . '(15 .2 'regular))
                          ("MonoLisa" . '(11 .2 'regular))
                          ("Monoflow" . '(14 nil 'regular))
                          ("Monoid HalfLoose" . '(9 .1 'light))
                          ("Monoid" . '(13 .2 'light))
                          ("Monoisome HalfLoose" . '(11 .2 'light))
                          ("Monoisome" . '(12 .3 'light))
                          ("MonospaceTypewriter" . '(14 .2 'regular))
                          ("NK57 Monospace" . '(10 .2 'regular))
                          ("NanumGothicCoding" . '(16 .3 'regular))
                          ("Noto Sans Mono ExtraCondensed" . '(15 nil 'regular))
                          ("Noto Sans Mono" . '(15 .1 'regular))
                          ("OCRA" . '(12 nil 'regular))
                          ("Old Timey Code" . '(20 .1 'regular))
                          ("Old Timey Mono" . '(20 .1 'regular))
                          ("Operator Mono Light" . '(11 .1 'regular))
                          ("Operator Mono" . '(16 .1 'thin))
                          ("Opus One" . '(14 nil 'regular))
                          ("Overpass Mono Light" . '(11 nil 'regular))
                          ("Overpass Mono" . '(11 nil 'regular))
                          ("Oxygen Mono" . '(11 .2 'regular))
                          ("PT Mono" . '(15 .1 'regular))
                          ("Panic Sans" . '(11 nil 'regular))
                          ("PragmataPro" . '(12 .1 'regular))
                          ("ProFont for Powerline" . '(13 .1 'regular))
                          ("Quinze" . '(13 nil 'regular))
                          ("Rec Mono Casual" . '(11 nil 'regular))
                          ("Rec Mono Linear" . '(11 nil 'regular))
                          ("Rec Mono Semicasual" . '(11 nil 'regular))
                          ("Recursive Monospace Casual" . '(11 nil 'regular))
                          ("Recursive Monospace" . '(11 nil 'regular))
                          ("Red Hat Mono" . '(11 nil 'regular))
                          ("Reno Mono" . '(11 nil 'regular))
                          ("Roboto Mono" . '(15 .2 'regular))
                          ("SF Mono Light" . '(10 nil 'regular))
                          ("SF Mono" . '(11 nil 'regular))
                          ("SV Basic Manual" . '(18 .3 'regular))
                          ("Sarasa Mono SC Nerd". '(12 nil 'regular))
                          ("Share Tech Mono" . '(11 nil 'regular))
                          ("Sometype Mono" . '(12 nil 'regular))
                          ("SpaceMono Nerd Font Mono" . '(15 nil 'regular))
                          ("Sudo" . '(19 .2 'thin))
                          ("Syne Mono" . '(12 nil 'regular))
                          ("Syne Tactile" . '(14 .1 'regular))
                          ("TT2020 Style E" . '(13 .2 'regular))
                          ("TT2020" . '(16 .3 'regular))
                          ("TT2020Base" . '(13 .1 'regular))
                          ("Terminess Nerd Font Mono" . '(18 .1 'regular))
                          ("Terminess Nerd Font" . '(18 nil 'regular))
                          ("TerminessTTF Nerd Font Mono" . '(14 nil 'regular))
                          ("TerminessTTF Nerd Font" . '(16 nil 'regular))
                          ("Times New Roman" . '(17 .1 'regular))
                          ("Tlwg Typewriter" . '(12 nil 'regular))
                          ("Typori" . '(15 .2 'regular)) ;; not monospace
                          ("Ubuntu Mono" . '(12 .1 'regular))
                          ("Unifont" . '(17 .2 'regular))
                          ("Uroob" . '(14 nil 'regular))
                          ("Victor Mono" . '(15 nil 'regular))
                          ("Whois" . '(17 .2 'regular))
                          ("Wire Type Mono" . '(18 nil 'regular))
                          ("Xanh Mono" . '(16 .1 'regular))
                          ("Zed Mono" . '(16 .1 'light))
                          ("fixed" . '(11 nil 'regular))
                          ("iA Writer Duo S" . '(15 nil 'regular))
                          ("iA Writer Mono S" . '(15 nil 'regular))
                          ("monoOne" . '(11 nil 'regular))
                          ("monofur" . '(13 nil 'regular))
                          ("monofur" . '(17 .2 'regular))
                          ("mononoki" . '(14 .1 'regular))
                          ("saxMono" . '(13 nil 'regular))
                          ))
(defvar meain/font-name "Input Mono Compressed")

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
