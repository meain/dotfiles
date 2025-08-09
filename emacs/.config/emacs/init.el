;;; init -- meain's Emacs config -*- lexical-binding: t -*-

;;; Commentary:
;;                    ____,
;;                   /.---|
;;                   `    |     ___
;;                       (=\.  /-. \
;;                        |\/\_|"|  |
;;                        |_\ |;-|  ;
;;                        | / \| |_/ \
;;                        | )/\/      \
;;                        | ( '|  \   |
;;                        |    \_ /   \
;;                        |    /  \_.--\
;;                        \    |    (|\`
;;                         |   |     \
;;                         |   |      '.
;;                         |  /         \
;;                         \  \.__.__.-._)
;;
;;
;; Well, hello there! How are you doing wanderer? Looking for some
;; lisp goodness?  You might find it here, you might not.  If you do
;; find what you are looking for here, feel free take them with you,
;; give them a new life, a new filesystem, a new home.  All I ask of
;; you is to treat them with love and care.  They have always been
;; with me, playing along with my musing, catching little typos and
;; finding little bugs.  They stuck strong to my side even when the
;; Rust borrow checker came for me.  I'm not gonna lie, there has been
;; many a times where I have doubted my skills, but they have always
;; believed in me.
;;
;; If these parenthesis could talk, they would have a lot of stories
;; to tell.  Some good, some bad, some really ugly.  But at the end of
;; the day, I'm sure they are all happy to be where they are.
;;
;; If they give you any trouble, my GitHub issues is always open
;; unlike the doors of heaven.  They probably won't, these are the
;; good ones, but God sometimes have different plans, and everyone
;; gets hit with hard times.
;;
;; Good luck!

;;; Code:
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'initialization) ; should be first
(require 'evil-m) ; should be second
(require 'utils)
(require 'settings)
(require 'visual)
(require 'editing)
(require 'window-management)
(require 'terminals)
(require 'filesystem)
(require 'project-management)
(require 'checkers)
(require 'completions)
(require 'documentation)
(require 'scratcher)
(require 'searching)
(require 'lsp)
(require 'vcs)
(require 'navigation)
(require 'looks)
(require 'filetypes)
(require 'debuggers)
(require 'extras)
(require 'tools)
(require 'tree-sitter-m)
(require 'bookmarking)
(require 'notes)
(require 'ai)
(require 'gptel-m)
(require 'yap-m)
(require 'templating)
(require 'reading)
(require 'writing)
(require 'file-manip)
(require 'automatic)
(require 'web)
(require 'random)
(require 'modeline)

(provide 'init)
;;; init.el ends here
