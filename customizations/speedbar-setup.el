;;; load and customize sr-speedbar
(add-to-list 'load-path (concat user-emacs-directory "customizations"))

(load "sr-speedbar.el")
(require 'sr-speedbar)
(setq sr-speedbar-right-side nil)
