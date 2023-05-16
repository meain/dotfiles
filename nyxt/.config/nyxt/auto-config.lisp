(define-configuration buffer
    ((default-modes (append '(nyxt::vi-normal-mode) %slot-default%))))
(define-configuration prompt-buffer
    ((default-modes (append '(nyxt::vi-insert-mode) %slot-default%))))