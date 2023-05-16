;; Add to your config.lisp
(define-configuration browser
  ((theme (make-instance 'theme:theme
                         :dark-p t
                         :background-color "black"
                         :on-background-color "white"
                         :primary-color "rgb(170, 170, 170)"
                         :on-primary-color "black"
                         :secondary-color "rgb(100, 100, 100)"
                         :on-secondary-color "white"
                         :accent-color "#37A8E4"
                         :on-accent-color "black"))))