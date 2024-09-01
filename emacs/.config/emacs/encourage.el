;;; encourage.el --- Add a message to minibuffer every time you save a document

;;; Commentary:
;; Add a message to minibuffer every time you save a document
;; A simple package which is inspired by a neovim plugin at
;; r-cha/encourage.nvim which itself was inspired by a VSCode plugin
;; at nicollasricas/vscode-encourage which was inspired by Visual
;; Studio plugin Haacked.Encourage.

;;; Code:
(defgroup encourage nil "Encourage customization group."
  :group 'tools)

(defcustom encourage-encouragements
  '("Great job! ✨"
    "You're doing great! 💪"
    "Keep up the good work! 🌟"
    "Well done! 🎉"
    "Onward and upward! 🚀"
    "You're on fire! 🔥"
    "You're a star! ⭐️"
    "You're amazing! 🌈"
    "That was awesome! 🎈"
    "Smart move. 🧠"
    "Bravo! 👏"
    "Nailed it. 🔨")
  "List of encouragements to display in the minibuffer."
  :type 'list
  :group 'encourage)

(defun save-message ()
  "Display a message in the minibuffer every time a document is saved."
  (let ((egmt (nth (random (length encourage-encouragements))
                   encourage-encouragements)))
    (if (and (featurep 'posframe) (fboundp 'posframe-show))
        ;; Shot at top right of frame
        (posframe-show "*encourage*"
                       :string (concat " " egmt " ")
                       :timeout 2
                       :poshandler 'posframe-poshandler-frame-top-right-corner
                       :background-color "skyblue"
                       :foreground-color "white"
                       :poshandler (lambda (_info) pos))
      (message egmt))))

(add-hook 'after-save-hook 'save-message)

(provide 'encourage)
;;; encourage.el ends here
