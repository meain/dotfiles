;;; tools.el --- Packages to work with external tools -*- lexical-binding: t; -*-

;;; Commentary:
;; Packages to work with external tools, such as docker, kubernetes, jira etc.

;;; Code:
;; Docker
(use-package docker
  :ensure t
  :defer t
  :commands (docker))

;; Kubernetes
(use-package kubernetes
  :ensure t
  :disabled t
  :defer t
  :commands (meain/kube)
  :config
  (defun meain/kube ()
    "Hacky function to load `kubernetes-evil' as it was not loading otherwise."
    (interactive)
    (use-package kubernetes-evil :ensure t)
    (kubernetes-overview)))

;; Control bluetooth devices
(use-package bluetooth
  :ensure t
  :commands (bluetooth-list-devices))

;; timing stuff
(use-package activity-watch-mode
  :ensure t
  :disabled t
  :defer t
  :config (global-activity-watch-mode))

(provide 'tools)
;;; tools.el ends here
