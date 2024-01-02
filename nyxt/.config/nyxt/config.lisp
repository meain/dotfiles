(in-package #:nyxt-user)

;; (define-configuration browser
;;   (;; This is for Nyxt to never prompt me about restoring the previous session.
;;    (autofills (list (make-autofill :name "First Name" :fill "Abin")
;;                     (make-autofill :name "Last Name" :fill "Simon")
;;                     (make-autofill :name "Name" :fill "Abin Simon")
;;                     (make-autofill :name "Email" :fill "mail@meain.io")))))

;; (define-configuration browser
;;     ((theme
;;       (make-instance 'theme:theme
;;                      :font-family "Sarasa Mono SC Nerd"
;;                      :accent-color "#C1E7F4")
;;       :doc "The theme to use for the browser.")))

;; https://github.com/shaunsingh/nix-darwin-dotfiles/blob/main/configs/nyxt/style.lisp
;; increase size of message-buffer
(define-configuration (window)
    ((message-buffer-height 21)))

;; message buffer appearance
(define-configuration (window)
    ((message-buffer-style
      (theme:themed-css (theme *browser*)
                        `(* :font-family "Sarasa Mono SC Nerd" :font-size "11px")
                        `(body :padding 0 :padding-left "9px" :margin "3px")))))

;; prompt buffer appearance
(define-configuration prompt-buffer
    ((style (str:concat
             %slot-value%
             (theme:themed-css (theme *browser*)  
                               `(* :font-family "Sarasa Mono SC Nerd" :font-size "13px")
                               `(body :border "0px")
                               `("#prompt" :margin-left "9px" :margin-right "9px" :font-weight "bold")
                               `("#prompt-area" :border "0px")
                               `(".arrow-right, .arrow-left" :all "unset")
                               `("button[title=vi-normal-mode], button[title=vi-insert-mode]:hover" :margin "4.25px 0px 0px 12.5px")
                               `("button[title=vi-insert-mode], button[title=vi-normal-mode]:hover" :padding "4.25px 0px 0px 12.5px")
                               `("#close-button" :display "flex" :justify-content "center" :align-items "center")
                               `(".source" :margin "0px" :border "0px")
                               `(".source-name" :padding "3px")
                               `(".source-content" :padding "2px" :margin "0px")
                               )))))

;; status buffer appearance
(define-configuration status-buffer
    ((style (str:concat
             %slot-value%
             (theme:themed-css (theme *browser*)  
                               `(* :font-family "Sarasa Mono SC Nerd" :font-size "13px")
                               `("#controls" :display "flex" :justify-content "space-around")
                               `(".arrow-left" :clip-path "unset")
                               `(".arrow-right" :clip-path "unset")
                               `(".tab" :clip-path "unset"))))))

;; web buffer appearance
(define-configuration web-buffer
    ((smooth-scrolling t)
     (style (str:concat
             %slot-value%
             (theme:themed-css (theme *browser*)  
                               `(* :font-family "Sarasa Mono SC Nerd !important"))))
     (override-map (let ((map (make-keymap "override-map")))
		             (define-key map
		                 "M-x" 'execute-command
		                 "M-c" 'switch-buffer
		                 "M-m" 'switch-buffer-next
		                 "M-n" 'switch-buffer-previous
		                 "C-tab" 'switch-buffer-last
		                 "C-x b" (lambda-command my/switch-buffer ()
			                                     "Switch-buffer with expected buffer-order."
			                                     (switch-buffer :current-is-last-p t))
		                 "M-w" 'delete-current-buffer
		                 "C-x C-k" 'delete-buffer
                         "C-r" 'reload-page)))))

;; use vi-modes
(define-configuration (web-buffer panel-buffer)
    ((default-modes (pushnew 'nyxt/mode/vi:vi-normal-mode %slot-value%))))
(define-configuration (prompt-buffer nyxt/mode/editor:editor-buffer)
    ((default-modes (pushnew 'nyxt/mode/vi:vi-insert-mode %slot-value%))))

;; (define-configuration nyxt/style-mode:dark-mode
;;   ((style #.(cl-css:css
;;              '((*
;;                 :background-color "red !important"
;;                 :background-image "none !important"
;;                 :color "white")
;;                (a
;;                 :background-color "black !important"
;;                 :background-image "none !important"
;;                 :color "#7D8FA3 !important"))))))
;; (make-style-association
;;   :predicate (lambda (url) (> (length url) 10))
;;   :style (cl-css:css
;;     '((body
;;        :background-color "black"))))
