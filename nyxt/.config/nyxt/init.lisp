(define-configuration status-buffer
    ((glyph-mode-presentation-p t)
     (style (str:concat 
             %slot-default%
             (cl-css:css
              '(("@font-face"
                 :font-family "myFont"
                 :src "local('Unifont')")
                ("*"
                 :font-family "myFont"
                 :background-color "rgb(0,0,0) !important")))))))